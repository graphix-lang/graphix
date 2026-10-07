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
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, seam_tick, seam_value, sort_values,
};
use graphix_rt::GXRt;
use netidx::{publisher::Typ, subscriber::Value};
use netidx_core::pack::{Pack, PackError};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{collections::VecDeque, fmt::Debug};

fn fc_concat(args: &[Value]) -> Option<Value> {
    let mut buf: SmallVec<[Value; 32]> = SmallVec::new();
    for v in args {
        match v {
            Value::Array(a) => buf.extend(a.iter().cloned()),
            v => buf.push(v.clone()),
        }
    }
    Some(Value::Array(ValArray::from_iter_exact(buf.drain(..))))
}

graphix_package_core::fast_builtin!(Concat, ConcatEv, "array_concat", fc_concat);

fn fc_push_back(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a), tl @ ..] => {
            // CR claude for claude: [perf] fc_push_back, like fc_push_front, fc_concat,
            // fc_flatten and fc_dedup, copies its result through an unpooled
            // SmallVec<[Value; 32]> and then again into ValArray::from_iter_exact. Past
            // 32 elements, each call mallocs and frees a buffer the size of the result
            // (160 KB for a 10k array) and moves every Value twice. These are
            // FastCalls, so a kernel that calls them pays this too. The length is known
            // up front (a.len() + tl.len(); flatten can sum its parts), so the array
            // can be filled in place from a counted chain, as the private `Counted` in
            // fusion/emit_helpers.rs:1737 does; dedup can collect into an LPooled<Vec>.
            // (x-alloc-09)
            let mut buf: SmallVec<[Value; 32]> = SmallVec::new();
            buf.extend(a.iter().cloned());
            buf.extend(tl.iter().cloned());
            Some(Value::Array(ValArray::from_iter_exact(buf.drain(..))))
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
            let mut buf: SmallVec<[Value; 32]> = SmallVec::new();
            buf.extend(tl.iter().cloned());
            buf.extend(a.iter().cloned());
            Some(Value::Array(ValArray::from_iter_exact(buf.drain(..))))
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

#[derive(Debug, Default, netidx_derive::Pack)]
struct WindowEv(SmallVec<[Value; 32]>);

graphix_package_core::pack_image_state!(WindowEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for WindowEv {
    // CR claude for claude: [bug] array::window is a pure function of its arguments (the
    // SmallVec is scratch that every eval drains or clears), but it is declared
    // Effect::Sync, which effects.rs reserves for cross-invocation state or a result
    // that depends on which arguments arrived. At a wake CachedArgs re-runs eval only
    // for a Stateless builtin (graphix-package-core/src/lib.rs:785) and otherwise
    // retags the old result. So a re-selected arm whose input fire was consumed by a
    // sibling arm shows the window from before the sleep: [0, 42] where array::push
    // over the same arguments shows [20, 42], in both engines alike. With no FastCall,
    // every kernel that reaches window de-fuses. Declare it
    // Stateless(Some(FastCall::Plain(..))) with a fast fn that does what eval does, and
    // drop it from the stateful list in design/recursive_activations.md. probe:
    // design/review-2026-10-05/repro/x-builtin-effects-07.gx (graphix-fuzz run; check
    // says AGREE). (x-builtin-effects-07)
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "array_window";

    fn eval(&mut self, _ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        // window requires ALL its args before producing anything.
        match &from.0[..] {
            [Some(Value::I64(window)), Some(Value::Array(a)), tl @ ..]
                if tl.iter().all(|v| v.is_some()) =>
            {
                // CR claude for claude: [bug] A negative #n casts to a huge usize, so
                // total <= window always holds and the window keeps every element ever
                // pushed. array::window(#n: -1, ..) is therefore an unbounded buffer,
                // though mod.gxi promises an array no larger than #n. Convert with
                // usize::try_from and treat a negative size as 0 (or log and bottom).
                // probe: design/review-2026-10-05/repro/x-engine-collections-09.gx
                // (x-engine-collections-09)
                let window = *window as usize;
                let total = a.len() + tl.len();
                let tl_vals = tl.iter().map(|v| v.clone().unwrap());
                if total <= window {
                    self.0.extend(a.iter().cloned());
                    self.0.extend(tl_vals);
                } else if a.len() >= (total - window) {
                    self.0.extend(a[(total - window)..].iter().cloned());
                    self.0.extend(tl_vals);
                } else {
                    self.0.extend(tl_vals.skip(tl.len() - window));
                }
                let a = ValArray::from_iter_exact(self.0.drain(..));
                Some(Value::Array(a))
            }
            _ => {
                self.0.clear();
                None
            }
        }
    }
}

type Window = CachedArgs<WindowEv>;

fn fc_flatten(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Array(a) => {
            let mut buf: SmallVec<[Value; 32]> = SmallVec::new();
            for v in a.iter() {
                match v {
                    Value::Array(a) => buf.extend(a.iter().cloned()),
                    v => buf.push(v.clone()),
                }
            }
            Some(Value::Array(ValArray::from_iter_exact(buf.drain(..))))
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
            // CR claude for claude: [bug] This set misses keys that == calls equal.
            // netidx-value's Hash for F32/F64 (../netidx/netidx-value/src/op.rs:59-72)
            // hashes -0.0 by its raw bits and keeps the sign bit in its NaN mask, while
            // its PartialEq says -0.0 == 0.0 and every NaN is equal. So dedup keeps
            // both zeros and both NaN signs (x86's 0.0 / 0.0 is a negative NaN), while
            // == and map keys treat each pair as one value. Fix it in that Hash (one
            // bit pattern for every NaN, -0.0 hashed as 0.0), since every hash
            // container of Value depends on it. probe:
            // design/review-2026-10-05/repro/x-engine-collections-06.gx
            // (x-engine-collections-06)
            let mut seen: LPooled<AHashSet<Value>> = LPooled::take();
            let mut buf: SmallVec<[Value; 32]> = SmallVec::new();
            for v in a.iter() {
                if !seen.contains(v) {
                    seen.insert(v.clone());
                    buf.push(v.clone());
                }
            }
            Some(Value::Array(ValArray::from_iter_exact(buf.drain(..))))
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
            // CR claude for claude: [bug] seam_tick reads a bottom answer from the
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

    // CR claude for claude: [bug] delete never unbinds the `n` and `x` bindings that init
    // made with genn::bind, so each deleted Group leaves two Binds in the global
    // env.by_id for the rest of the session, and both keep its fresh `#fn` scope path
    // alive. Collection-slot churn or a dynamic rebind over array::group therefore
    // grows memory without bound, about 530 bytes per deleted Group. Core filter, opt
    // and sys::net::publish unbind at delete and stay flat. Add
    // `ctx.env.unbind_variable(self.nid)` and `ctx.env.unbind_variable(self.xid)` here.
    // queuefn's WrapperApply::delete (stdlib/graphix-package-core/src/queuefn.rs:146)
    // has the same omission: no unbind_variable and no store_remove for its arg_bids.
    // probe: design/review-2026-10-05/repro/collections-str-05.gx (VmRSS +26 MB per
    // 1000 ticks; the same program with core filter stays flat). (collections-str-05)
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_remove(&self.nid);
        ctx.rt.store_remove(&self.pid);
        ctx.rt.store_remove(&self.xid);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.pred.sleep(ctx);
    }
}

#[derive(Debug)]
struct Iter(BindId, ExprId, TagValue);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Iter {
    const NAME: &str = "array_iter";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(Iter(id, top_id, TagValue::phantom())))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Iter(id, top_id, TagValue::phantom())))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Iter {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.0.encode(buf)?;
        self.1.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if let Some(Value::Array(a)) =
            seam_tick(from[0].update(ctx)).map(|tv| tv.value_cloned())
        {
            for v in a.iter() {
                // Cooperative interrupt: abort a wedged iter over a huge
                // array (partial emit is accepted for a deliberate kill).
                if ctx.interrupted() {
                    return self.2.ride();
                }
                ctx.rt.set_var(self.0, v.clone());
            }
        }
        let res = ctx.event.variables.get(&self.0).map(|tv| tv.value_cloned());
        match res {
            Some(v) => self.2.set(TagValue::fired(v)),
            None => self.2.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.0, self.1)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.0, self.1);
        self.0 = BindId::new();
        ctx.rt.ref_var(self.0, self.1);
        self.2 = TagValue::phantom();
    }
}

#[derive(Debug)]
struct IterQ {
    triggered: usize,
    queue: VecDeque<(usize, ValArray)>,
    id: BindId,
    top_id: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IterQ {
    const NAME: &str = "array_iterq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(IterQ {
            triggered: 0,
            queue: VecDeque::new(),
            id,
            top_id,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let triggered = usize::decode(buf)?;
        let queue = Pack::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(IterQ { triggered, queue, id, top_id, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IterQ {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.triggered.encode(buf)?;
        self.queue.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx)).is_some() {
            self.triggered += 1;
        }
        if let Some(Value::Array(a)) =
            seam_tick(from[1].update(ctx)).map(|tv| tv.value_cloned())
        {
            if a.len() > 0 {
                self.queue.push_back((0, a));
            }
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            let (i, a) = self.queue.front_mut().unwrap();
            while self.triggered > 0 && *i < a.len() {
                if ctx.interrupted() {
                    return self.out.ride();
                }
                ctx.rt.set_var(self.id, a[*i].clone());
                *i += 1;
                self.triggered -= 1;
            }
            if *i == a.len() {
                self.queue.pop_front();
            }
        }
        let res = ctx.event.variables.get(&self.id).map(|tv| tv.value_cloned());
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.queue.clear();
        self.triggered = 0;
        self.out = TagValue::phantom();
    }
}

fn fc_iota(args: &[Value]) -> Option<Value> {
    match args {
        [Value::I64(n)] => {
            if *n > graphix_compiler::node::MAX_ARRAY_INIT_LEN {
                log::error!(
                    "array::init: size {n} exceeds the {} element \
                     limit — producing no value",
                    graphix_compiler::node::MAX_ARRAY_INIT_LEN
                );
                return None;
            }
            let n = (*n).max(0) as usize;
            Some(Value::Array(ValArray::from_iter_exact(
                (0..n).map(|i| Value::I64(i as i64)),
            )))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Iota, IotaEv, "array_iota", fc_iota);

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
        Iota,
        Iter,
        IterQ,
        PushBack,
        PushFront,
        Sort,
        Window,
        Rotate,
    ],
}
