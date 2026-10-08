#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::{Result, bail};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, LambdaId, Node, Refs, Rt, Scope,
    TagValue, UserEvent,
    effects::Effect,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    typ::{FnType, Type},
};
use graphix_package_core::{seam_tick, seam_value, sort_values};
use graphix_rt::GXRt;
use netidx::{publisher::Typ, subscriber::Value};
use netidx_core::pack::{Pack, PackError};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{collections::VecDeque, fmt::Debug};

/// An iterator that knows its length, so a result fills its array in
/// place.
struct Counted<I> {
    it: I,
    left: usize,
}

impl<I: Iterator<Item = Value>> Iterator for Counted<I> {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let v = self.it.next()?;
        self.left -= 1;
        Some(v)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

impl<I: Iterator<Item = Value>> ExactSizeIterator for Counted<I> {}

/// The array of `len` values `it` yields.
fn array_of(len: usize, it: impl Iterator<Item = Value>) -> Value {
    Value::Array(ValArray::from_iter_exact(Counted { it, left: len }))
}

/// `vs` with every array spread into its elements, and how many that is.
fn spread(vs: &[Value]) -> (usize, impl Iterator<Item = Value> + '_) {
    fn elems(v: &Value) -> &[Value] {
        match v {
            Value::Array(a) => &a[..],
            v => std::slice::from_ref(v),
        }
    }
    let len = vs.iter().map(|v| elems(v).len()).sum();
    (len, vs.iter().flat_map(move |v| elems(v).iter().cloned()))
}

fn fc_concat(args: &[Value]) -> Option<Value> {
    let (len, it) = spread(args);
    Some(array_of(len, it))
}

graphix_package_core::fast_builtin!(Concat, ConcatEv, "array_concat", fc_concat);

fn fc_push_back(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a), tl @ ..] => {
            Some(array_of(a.len() + tl.len(), a.iter().chain(tl).cloned()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    PushBack,
    PushBackEv,
    "array_push_back",
    fc_push_back
);

fn fc_push_front(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a), tl @ ..] => {
            Some(array_of(a.len() + tl.len(), tl.iter().chain(a.iter()).cloned()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    PushFront,
    PushFrontEv,
    "array_push_front",
    fc_push_front
);

/// The last `#n` of the array's elements and the pushed values; a
/// negative size keeps none.
fn fc_window(args: &[Value]) -> Option<Value> {
    match args {
        [Value::I64(n), Value::Array(a), tl @ ..] => {
            let n = usize::try_from(*n).unwrap_or(0);
            let total = a.len() + tl.len();
            let skip = total.saturating_sub(n);
            Some(array_of(total - skip, a.iter().chain(tl).skip(skip).cloned()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Window, WindowEv, "array_window", fc_window);

fn fc_flatten(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => {
            let (len, it) = spread(a);
            Some(array_of(len, it))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Flatten, FlattenEv, "array_flatten", fc_flatten);

fn fc_sort(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(dir), Value::Bool(numeric), Value::Array(a)] => {
            let mut sorted = sort_values(dir, *numeric, a.iter().cloned())?;
            Some(Value::Array(ValArray::from_iter_exact(sorted.drain(..))))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Sort, SortEv, "array_sort", fc_sort);

fn fc_dedup(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => {
            let mut seen: LPooled<AHashSet<Value>> = LPooled::take();
            let mut kept: LPooled<Vec<Value>> = LPooled::take();
            for v in a.iter() {
                if seen.insert(v.clone()) {
                    kept.push(v.clone());
                }
            }
            Some(Value::Array(ValArray::from_iter_exact(kept.drain(..))))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Dedup, DedupEv, "array_dedup", fc_dedup);

fn fc_enumerate(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => Some(Value::Array(ValArray::from_iter_exact(
            a.iter().enumerate().map(|(i, v)| (i as i64, v.clone()).into()),
        ))),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    Enumerate,
    EnumerateEv,
    "array_enumerate",
    fc_enumerate
);

fn fc_zip(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a0), Value::Array(a1)] => {
            Some(Value::Array(ValArray::from_iter_exact(
                a0.iter().cloned().zip(a1.iter().cloned()).map(|p| p.into()),
            )))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Zip, ZipEv, "array_zip", fc_zip);

fn fc_unzip(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a)] => {
            let mut t0: LPooled<Vec<Value>> = LPooled::take();
            let mut t1: LPooled<Vec<Value>> = LPooled::take();
            for v in a {
                if let Value::Array(a) = v {
                    match &a[..] {
                        [v0, v1] => {
                            t0.push(v0.clone());
                            t1.push(v1.clone());
                        }
                        _ => (),
                    }
                }
            }
            let v0 = Value::Array(ValArray::from_iter_exact(t0.drain(..)));
            let v1 = Value::Array(ValArray::from_iter_exact(t1.drain(..)));
            Some(Value::Array(ValArray::from_iter_exact([v0, v1].into_iter())))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Unzip, UnzipEv, "array_unzip", fc_unzip);

#[derive(Debug)]
struct Group<R: Rt, E: UserEvent> {
    queue: VecDeque<Value>,
    buf: SmallVec<[Value; 16]>,
    pred: Node<R, E>,
    ready: bool,
    pid: BindId,
    nid: BindId,
    xid: BindId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Group<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "array_group";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                let scope = scope.append_block("fn", LambdaId::new().inner());
                let n_typ = Type::Primitive(Typ::I64.into());
                let etyp = typ.args[0].typ.clone();
                let mftyp = match &typ.args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected function not {t}"),
                };
                let (nid, n) =
                    genn::bind(ctx, &scope.lexical, "n", n_typ.clone(), top_id);
                let (xid, x) = genn::bind(ctx, &scope.lexical, "x", etyp.clone(), top_id);
                let pid = BindId::new();
                let fnode = genn::reference(ctx, pid, Type::Fn(mftyp.clone()), top_id);
                let pred =
                    genn::apply(fnode, scope, smallvec::smallvec![n, x], &mftyp, top_id);
                Ok(Box::new(Self {
                    queue: VecDeque::new(),
                    buf: smallvec![],
                    pred,
                    ready: true,
                    pid,
                    nid,
                    xid,
                    out: TagValue::phantom(),
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let queue = Pack::decode(buf)?;
        let buf_ = Pack::decode(buf)?;
        let pred = image::decode_node(ctx, buf)?;
        let ready = bool::decode(buf)?;
        let pid = BindId::decode(buf)?;
        let nid = BindId::decode(buf)?;
        let xid = BindId::decode(buf)?;
        Ok(Box::new(Self {
            queue,
            buf: buf_,
            pred,
            ready,
            pid,
            nid,
            xid,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Group<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.queue.encode(buf)?;
        self.buf.encode(buf)?;
        self.pred.image_encode(buf)?;
        self.ready.encode(buf)?;
        self.pid.encode(buf)?;
        self.nid.encode(buf)?;
        self.xid.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        macro_rules! set {
            ($v:expr) => {{
                self.ready = false;
                self.buf.push($v.clone());
                let len = Value::I64(self.buf.len() as i64);
                ctx.rt.store_insert(self.nid, TagValue::fired(len.clone()));
                ctx.event.variables.insert(self.nid, TagValue::fired(len));
                ctx.rt.store_insert(self.xid, TagValue::fired($v.clone()));
                ctx.event.variables.insert(self.xid, TagValue::fired($v));
            }};
        }
        if let Some(tv) = seam_tick(from[0].update(ctx)) {
            self.queue.push_back(tv.value_cloned());
        }
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            ctx.rt.store_insert(self.pid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.pid, TagValue::tagged(v, tag));
        }
        if self.ready && self.queue.len() > 0 {
            let v = self.queue.pop_front().unwrap();
            set!(v);
        }
        let res = loop {
            // Cooperative interrupt: abort a wedged grouping loop.
            if ctx.interrupted() {
                break None;
            }
            // CR claude for eric: [bug] seam_tick reads a bottom answer from the
            // predicate as "not answered yet", so `ready` stays false. A bottom answer
            // is a FreshBottom: a `?` raise (which `throws 'e` allows), a div0, or a
            // `$` on a bad index. When that bottom depends only on n and x, nothing
            // re-fires the predicate. Group then never emits again, and every later v
            // is pushed onto `queue` and never popped. The raise is the clear case: the
            // error reaches the catch and the group is dead, whereas a seq machine
            // aborts the run and takes the next trigger. A gated predicate (`n >= th$`
            // with th null for a while) does rely on this wait and recovers today, so
            // the fix has to tell a raise apart from a pending gate. probe:
            // design/review-2026-10-05/repro/collections-str-04.gx (collections-str-04)
            // 2026-10-07 claude: open. A fresh bottom answer is either a raise (the group
            // should move on) or a gate still closed (it should wait), and the answer's tag
            // cannot tell them apart; the seq machine's abort is the model for the first.
            // 2026-10-08 claude: re-addressed, a rule: when array::group's predicate
            // raises, should the group drop that element and move on (as a seq abort
            // takes the next trigger), or keep waiting as for a closed gate? The answer's
            // tag cannot tell the two; the raise reaches the catch either way.
            // 2026-10-08 claude: re-addressed, a rule: when array::group's predicate
            // raises, should the group drop that element and move on (as a seq abort
            // takes the next trigger), or keep waiting as for a closed gate? The answer's
            // tag cannot tell the two; the raise reaches the catch either way.
            match seam_tick(self.pred.update(ctx)).map(|tv| tv.value_cloned()) {
                None => break None,
                Some(v) => {
                    self.ready = true;
                    match v {
                        Value::Bool(true) => {
                            break Some(Value::Array(ValArray::from_iter_exact(
                                self.buf.drain(..),
                            )));
                        }
                        _ => match self.queue.pop_front() {
                            None => break None,
                            Some(v) => set!(v),
                        },
                    }
                }
            }
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_remove(&self.nid);
        ctx.rt.store_remove(&self.pid);
        ctx.rt.store_remove(&self.xid);
        ctx.env.unbind_variable(self.nid);
        ctx.env.unbind_variable(self.xid);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.pred.sleep(ctx);
    }
}

/// An array's elements, in order.
#[derive(Debug)]
struct ArrayElems;

impl graphix_package_core::Elements for ArrayElems {
    const ITER: &str = "array_iter";
    const ITERQ: &str = "array_iterq";
    type Cursor = (usize, ValArray);

    fn cursor(v: Value) -> Option<Self::Cursor> {
        match v {
            Value::Array(a) if !a.is_empty() => Some((0, a)),
            _ => None,
        }
    }

    fn next((i, a): &mut Self::Cursor) -> Option<Value> {
        let v = a.get(*i)?.clone();
        *i += 1;
        Some(v)
    }
}

type Iter = graphix_package_core::Iter<ArrayElems>;
type IterQ = graphix_package_core::IterQ<ArrayElems>;

fn fc_rotate(args: &[Value]) -> Option<Value> {
    match args {
        [Value::I64(n), Value::Array(a)] => {
            let r = if a.is_empty() { 0 } else { n.rem_euclid(a.len() as i64) as usize };
            if r == 0 {
                return Some(Value::Array(a.clone()));
            }
            let p = a.len() - r;
            let mut tmp: LPooled<Vec<Value>> = LPooled::take();
            tmp.extend(a[p..].iter().chain(&a[..p]).cloned());
            Some(Value::Array(ValArray::from_iter_exact(tmp.drain(..))))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Rotate, RotateEv, "array_rotate", fc_rotate);

graphix_derive::defpackage! {
    builtins => [
        Concat,
        Dedup,
        Enumerate,
        Zip,
        Unzip,
        Flatten,
        Group as Group<GXRt<X>, X::UserEvent>,
        Iter,
        IterQ,
        PushBack,
        PushFront,
        Sort,
        Window,
        Rotate,
    ],
}
