#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use ahash::AHashSet;
use anyhow::{Result, bail};
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, LambdaId, Node, Refs, Rt, Scope, TagValue,
    effects::Effect,
    expr::ExprId,
    image::{self, ImageBuf},
    node::genn,
    rep,
    typ::{FnType, Type},
};
use graphix_package_core::{bottom_of, seam_tick, seam_value, sort_values};
use graphix_rt::GXRt;
use netidx::subscriber::Value;
use netidx_core::pack::PackError;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{collections::VecDeque, fmt::Debug};

/// The array of the `len` values `it` yields; `it` yields exactly `len`.
fn array_of(len: usize, it: impl Iterator<Item = Value>) -> Value {
    Value::Array(graphix_compiler::valarray_of(len, it))
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
        [dir, Value::Bool(numeric), Value::Array(a)] => {
            let mut sorted = sort_values(dir, *numeric, a.iter().cloned())?;
            Some(Value::Array(ValArray::from_iter_exact(sorted.drain(..))))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Sort, SortEv, "array_sort", fc_sort; cached; ordered(2..3));

fn fc_dedup(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => {
            // XCR claude for claude: [perf/medium] Under the hook loan, every abstract
            // value whose type implements Eq hashes to its id alone (GxAbstract::hash,
            // eq_applies), so this set compares each element with every kept one
            // through the Eq hook. That is quadratic: 4000 elements take 7 s in debug
            // where the impl-free run takes 0.1 s. Decide whether dedup keys by Ord
            // where the type implements it (as map keys do), or accept and document the
            // cost. probe:
            // design/review-2026-10-26/repro/c-module-traits-dedup-quadratic.sh Also
            // reported as (tests-fusion-traits-04). (c-module-traits-05)
            // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q58,
            // discussed): add a hash method to the core Eq trait that returns the part
            // of the value to be hashed (what the implementer's eq compares). The
            // runtime hashes that, so hashing and dedup over such values stop degrading
            // to the type id.
            // 2026-10-28 claude: Ruling built: core Eq has an optional hash method
            // (fn(self) -> Any), the part of a value its eq reads;
            // CoreTrait::runtime_method lets an impl leave it out and refuses a
            // program's call ("Eq::hash is called by hashing, never by a program").
            // GxAbstract's Hash, where an Eq impl decides equality, hashes the id, the
            // params and that part (ValueHookDispatch::hash,
            // coretraits::dispatch_hash); left out, the type alone, as before. dedup of
            // 4000 distinct values: 184 ms with hash, 3.6 s without. Pins:
            // abstract_value::test::eq_hashes_by_its_part,
            // lang::traits::{eq_hash_merges_what_eq_calls_equal,
            // eq_hash_is_not_called_by_a_program}.
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

graphix_package_core::fast_builtin!(Dedup, DedupEv, "array_dedup", fc_dedup; cached; ordered(0..1));

fn fc_enumerate(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => Some(Value::Array(ValArray::from_iter_exact(
            a.iter()
                .enumerate()
                .map(|(i, v)| rep::tuple([Value::I64(i as i64), v.clone()])),
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
                a0.iter()
                    .zip(a1.iter())
                    .map(|(v0, v1)| rep::tuple([v0.clone(), v1.clone()])),
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
                if let Some([v0, v1]) = rep::tuple_elems(v) {
                    t0.push(v0.clone());
                    t1.push(v1.clone());
                }
            }
            let v0 = Value::Array(ValArray::from_iter_exact(t0.drain(..)));
            let v1 = Value::Array(ValArray::from_iter_exact(t1.drain(..)));
            Some(rep::tuple([v0, v1]))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Unzip, UnzipEv, "array_unzip", fc_unzip);

#[derive(Debug)]
struct Group<R: Rt> {
    queue: VecDeque<Value>,
    buf: SmallVec<[Value; 16]>,
    pred: genn::Callback<R>,
    ready: bool,
    out: TagValue,
}

impl<R: Rt> Group<R> {
    fn with(pred: genn::Callback<R>) -> Self {
        Self {
            queue: VecDeque::new(),
            buf: smallvec![],
            pred,
            ready: true,
            out: TagValue::phantom(),
        }
    }
}

impl<R: Rt> BuiltIn<R> for Group<R> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "array_group";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        match from {
            [_, _] => {
                let scope = scope.append_block("fn", LambdaId::new().inner());
                let mftyp = match &resolved.unwrap_or(typ).args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected function not {t}"),
                };
                let pred = genn::Callback::new(ctx, &scope, &mftyp, &["n", "x"], top_id)?;
                Ok(Box::new(Self::with(pred)))
            }
            _ => bail!("expected two arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let pred = genn::Callback::image_decode(ctx, buf)?;
        Ok(Box::new(Self::with(pred)))
    }
}

impl<R: Rt> Apply<R> for Group<R> {
    /// The queue, the group being built and the wait for an answer
    /// exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.queue.is_empty() || !self.buf.is_empty() || !self.ready {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.pred.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let x = from[0].update(ctx);
        let x_tag = x.tag();
        if let Some(tv) = seam_tick(x) {
            self.queue.push_back(tv.value_cloned());
        }
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            self.pred.feed_fn(ctx, tv.clone());
        }
        if self.ready
            && let Some(v) = self.queue.pop_front()
        {
            self.ready = false;
            self.buf.push(v.clone());
            let len = Value::I64(self.buf.len() as i64);
            self.pred.feed_arg(ctx, 0, TagValue::fired(len));
            self.pred.feed_arg(ctx, 1, TagValue::fired(v));
        }
        let res = match seam_tick(self.pred.update(ctx)).map(|tv| tv.value_cloned()) {
            // only an element waiting is answered: f re-fired by what
            // else it reads answers nothing
            None => None,
            Some(_) if self.ready => None,
            Some(answer) => {
                self.ready = true;
                let group = match answer {
                    Value::Bool(true) => {
                        Some(Value::Array(ValArray::from_iter_exact(self.buf.drain(..))))
                    }
                    _ => None,
                };
                // what queued while f answered starts next cycle: what fired f
                // now would fire it again, and that is no answer for the next
                // element
                if let Some(v) = self.queue.pop_front() {
                    self.ready = false;
                    self.buf.push(v.clone());
                    ctx.rt
                        .set_var(self.pred.args()[0], Value::I64(self.buf.len() as i64));
                    ctx.rt.set_var(self.pred.args()[1], v);
                }
                group
            }
        };
        // a group of earlier elements delivers; a bottom input stands in
        // place of a ride
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match bottom_of([x_tag]) {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.pred.sleep(ctx);
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.queue.clear();
        self.buf.clear();
        self.ready = true;
        self.out = TagValue::phantom();
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
            Some(array_of(a.len(), a[p..].iter().chain(&a[..p]).cloned()))
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
        Group as Group<GXRt<X>>,
        Iter,
        IterQ,
        PushBack,
        PushFront,
        Sort,
        Window,
        Rotate,
    ],
}
