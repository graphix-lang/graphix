use crate::{
    deftype, CachedArgs, CachedVals, EvalCached, FoldFn, FoldQ, MapCollection, MapFn,
    MapQ, Slot,
};
use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use graphix_compiler::{
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, BuiltInInitFn, Event, ExecCtx, LambdaId, Node, Refs, Rt,
    UserEvent,
};
use netidx::{publisher::Typ, subscriber::Value, utils::Either};
use netidx_value::ValArray;
use smallvec::{smallvec, SmallVec};
use std::{collections::VecDeque, fmt::Debug, iter, sync::Arc};

impl MapCollection for ValArray {
    fn iter_values(&self) -> impl Iterator<Item = Value> {
        (**self).iter().cloned()
    }

    fn len(&self) -> usize {
        (**self).len()
    }

    fn select(v: Value) -> Option<Self> {
        match v {
            Value::Array(a) => Some(a.clone()),
            _ => None,
        }
    }

    fn project(self) -> Value {
        Value::Array(self)
    }

    fn etyp(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Array(et) => Ok((**et).clone()),
            _ => bail!("expected array"),
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct MapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_map";
    deftype!(
        "core::array",
        "fn(Array<'a>, fn('a) -> 'b throws 'e) -> Array<'b> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter_exact(
            slots.iter().map(|s| s.cur.clone().unwrap()),
        )))
    }
}

pub(super) type Map<R, E> = MapQ<R, E, MapImpl>;

#[derive(Debug, Default)]
pub(super) struct FilterImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FilterImpl {
    type Collection = ValArray;

    const NAME: &str = "array_filter";
    deftype!(
        "core::array",
        "fn(Array<'a>, fn('a) -> bool throws 'e) -> Array<'a> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], a: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().zip(a.iter()).filter_map(
            |(p, v)| match p.cur {
                Some(Value::Bool(true)) => Some(v.clone()),
                _ => None,
            },
        ))))
    }
}

pub(super) type Filter<R, E> = MapQ<R, E, FilterImpl>;

#[derive(Debug, Default)]
pub(super) struct FlatMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FlatMapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_flat_map";
    deftype!(
        "core::array",
        "fn(Array<'a>, fn('a) -> ['b, Array<'b>] throws 'e) -> Array<'b> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().flat_map(|s| {
            match s.cur.as_ref().unwrap() {
                Value::Array(a) => Either::Left(a.clone().into_iter()),
                v => Either::Right(iter::once(v.clone())),
            }
        }))))
    }
}

pub(super) type FlatMap<R, E> = MapQ<R, E, FlatMapImpl>;

#[derive(Debug, Default)]
pub(super) struct FilterMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FilterMapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_filter_map";
    deftype!(
        "core::array",
        "fn(Array<'a>, fn('a) -> Option<'b> throws 'e) -> Array<'b> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().filter_map(|s| {
            match s.cur.as_ref().unwrap() {
                Value::Null => None,
                v => Some(v.clone()),
            }
        }))))
    }
}

pub(super) type FilterMap<R, E> = MapQ<R, E, FilterMapImpl>;

#[derive(Debug, Default)]
pub(super) struct FindImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FindImpl {
    type Collection = ValArray;

    const NAME: &str = "array_find";
    deftype!(
        "core::array",
        "fn(Array<'a>, fn('a) -> bool throws 'e) -> Option<'a> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], a: &ValArray) -> Option<Value> {
        let r = slots
            .iter()
            .enumerate()
            .find(|(_, s)| match s.cur.as_ref() {
                Some(Value::Bool(true)) => true,
                _ => false,
            })
            .map(|(i, _)| a[i].clone())
            .unwrap_or(Value::Null);
        Some(r)
    }
}

pub(super) type Find<R, E> = MapQ<R, E, FindImpl>;

#[derive(Debug, Default)]
pub(super) struct FindMapImpl;

impl<R: Rt, E: UserEvent> MapFn<R, E> for FindMapImpl {
    type Collection = ValArray;

    const NAME: &str = "array_find_map";
    deftype!(
        "core::array",
        "fn(Array<'a>, fn('a) -> Option<'b> throws 'e) -> Option<'b> throws 'e"
    );

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        let r = slots
            .iter()
            .find_map(|s| match s.cur.as_ref().unwrap() {
                Value::Null => None,
                v => Some(v.clone()),
            })
            .unwrap_or(Value::Null);
        Some(r)
    }
}

pub(super) type FindMap<R, E> = MapQ<R, E, FindMapImpl>;

#[derive(Debug)]
struct FoldImpl;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for FoldImpl {
    type Collection = ValArray;

    const NAME: &str = "array_fold";
    deftype!(
        "core::array",
        "fn(Array<'a>, 'b, fn('b, 'a) -> 'b throws 'e) -> 'b throws 'e"
    );
}

type Fold<R, E> = FoldQ<R, E, FoldImpl>;

#[derive(Debug, Default)]
pub(super) struct ConcatEv(SmallVec<[Value; 32]>);

impl EvalCached for ConcatEv {
    const NAME: &str = "array_concat";
    deftype!("core::array", "fn(Array<'a>, @args: Array<'a>) -> Array<'a>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        for v in from.0.iter() {
            match v {
                Some(Value::Array(a)) => {
                    for v in a.iter() {
                        self.0.push(v.clone())
                    }
                }
                Some(v) => self.0.push(v.clone()),
                None => present = false,
            }
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

pub(super) type Concat = CachedArgs<ConcatEv>;

#[derive(Debug, Default)]
pub(super) struct PushBackEv(SmallVec<[Value; 32]>);

impl EvalCached for PushBackEv {
    const NAME: &str = "array_push_back";
    deftype!("core::array", "fn(Array<'a>, @args: 'a) -> Array<'a>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::Array(a)), tl @ ..] => {
                self.0.extend(a.iter().map(|v| v.clone()));
                for v in tl {
                    match v {
                        Some(v) => self.0.push(v.clone()),
                        None => present = false,
                    }
                }
            }
            [] | [None, ..] | [Some(_), ..] => present = false,
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

pub(super) type PushBack = CachedArgs<PushBackEv>;

#[derive(Debug, Default)]
pub(super) struct PushFrontEv(SmallVec<[Value; 32]>);

impl EvalCached for PushFrontEv {
    const NAME: &str = "array_push_front";
    deftype!("core::array", "fn(Array<'a>, @args: 'a) -> Array<'a>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::Array(a)), tl @ ..] => {
                for v in tl {
                    match v {
                        Some(v) => self.0.push(v.clone()),
                        None => present = false,
                    }
                }
                self.0.extend(a.iter().map(|v| v.clone()));
            }
            [] | [None, ..] | [Some(_), ..] => present = false,
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

pub(super) type PushFront = CachedArgs<PushFrontEv>;

#[derive(Debug, Default)]
pub(super) struct WindowEv(SmallVec<[Value; 32]>);

impl EvalCached for WindowEv {
    const NAME: &str = "array_window";
    deftype!("core::array", "fn(#n:i64, Array<'a>, @args: 'a) -> Array<'a>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut present = true;
        match &from.0[..] {
            [Some(Value::I64(window)), Some(Value::Array(a)), tl @ ..] => {
                let window = *window as usize;
                let total = a.len() + tl.len();
                if total <= window {
                    self.0.extend(a.iter().cloned());
                    for v in tl {
                        match v {
                            Some(v) => self.0.push(v.clone()),
                            None => present = false,
                        }
                    }
                } else if a.len() >= (total - window) {
                    self.0.extend(a[(total - window)..].iter().cloned());
                    for v in tl {
                        match v {
                            Some(v) => self.0.push(v.clone()),
                            None => present = false,
                        }
                    }
                } else {
                    for v in &tl[tl.len() - window..] {
                        match v {
                            Some(v) => self.0.push(v.clone()),
                            None => present = false,
                        }
                    }
                }
            }
            [] | [_] | [_, None, ..] | [None, _, ..] | [Some(_), Some(_), ..] => {
                present = false
            }
        }
        if present {
            let a = ValArray::from_iter_exact(self.0.drain(..));
            Some(Value::Array(a))
        } else {
            self.0.clear();
            None
        }
    }
}

pub(super) type Window = CachedArgs<WindowEv>;

#[derive(Debug, Default)]
pub(super) struct LenEv;

impl EvalCached for LenEv {
    const NAME: &str = "array_len";
    deftype!("core::array", "fn(Array<'a>) -> i64");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => Some(Value::I64(a.len() as i64)),
            Some(_) | None => None,
        }
    }
}

pub(super) type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
pub(super) struct FlattenEv(SmallVec<[Value; 32]>);

impl EvalCached for FlattenEv {
    const NAME: &str = "array_flatten";
    deftype!("core::array", "fn(Array<Array<'a>>) -> Array<'a>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::Array(a)) => {
                for v in a.iter() {
                    match v {
                        Value::Array(a) => self.0.extend(a.iter().map(|v| v.clone())),
                        v => self.0.push(v.clone()),
                    }
                }
                let a = ValArray::from_iter_exact(self.0.drain(..));
                Some(Value::Array(a))
            }
            Some(_) | None => None,
        }
    }
}

pub(super) type Flatten = CachedArgs<FlattenEv>;

#[derive(Debug, Default)]
pub(super) struct SortEv(SmallVec<[Value; 32]>);

impl EvalCached for SortEv {
    const NAME: &str = "array_sort";
    deftype!(
        "core::array",
        "fn(?#dir:Direction, ?#numeric:bool, Array<'a>) -> Array<'a>"
    );

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        fn cn(v: &Value) -> Value {
            v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone())
        }
        match &from.0[..] {
            [Some(Value::String(dir)), Some(Value::Bool(numeric)), Some(Value::Array(a))] => {
                match &**dir {
                    "Ascending" => {
                        self.0.extend(a.iter().cloned());
                        if *numeric {
                            self.0.sort_by(|v0, v1| cn(v0).cmp(&cn(v1)))
                        } else {
                            self.0.sort();
                        }
                        Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
                    }
                    "Descending" => {
                        self.0.extend(a.iter().cloned());
                        if *numeric {
                            self.0.sort_by(|a0, a1| cn(a1).cmp(&cn(a0)))
                        } else {
                            self.0.sort_by(|a0, a1| a1.cmp(a0));
                        }
                        Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

pub(super) type Sort = CachedArgs<SortEv>;

#[derive(Debug, Default)]
pub(super) struct EnumerateEv;

impl EvalCached for EnumerateEv {
    const NAME: &str = "array_enumerate";
    deftype!("core::array", "fn(Array<'a>) -> Array<(i64, 'a)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        if let Some(Value::Array(a)) = &from.0[0] {
            let a = ValArray::from_iter_exact(
                a.iter().enumerate().map(|(i, v)| (i, v.clone()).into()),
            );
            return Some(Value::Array(a));
        }
        None
    }
}

pub(super) type Enumerate = CachedArgs<EnumerateEv>;

#[derive(Debug, Default)]
pub(super) struct ZipEv;

impl EvalCached for ZipEv {
    const NAME: &str = "array_zip";
    deftype!("core::array", "fn(Array<'a>, Array<'b>) -> Array<('a, 'b)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::Array(a0)), Some(Value::Array(a1))] => {
                Some(Value::Array(ValArray::from_iter_exact(
                    a0.iter().cloned().zip(a1.iter().cloned()).map(|p| p.into()),
                )))
            }
            _ => None,
        }
    }
}

pub(super) type Zip = CachedArgs<ZipEv>;

#[derive(Debug, Default)]
pub(super) struct UnzipEv {
    t0: Vec<Value>,
    t1: Vec<Value>,
}

impl EvalCached for UnzipEv {
    const NAME: &str = "array_unzip";
    deftype!("core::array", "fn(Array<('a, 'b)>) -> (Array<'a>, Array<'b>)");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::Array(a))] => {
                for v in a {
                    if let Value::Array(a) = v {
                        match &a[..] {
                            [v0, v1] => {
                                self.t0.push(v0.clone());
                                self.t1.push(v1.clone());
                            }
                            _ => (),
                        }
                    }
                }
                let v0 = Value::Array(ValArray::from_iter_exact(self.t0.drain(..)));
                let v1 = Value::Array(ValArray::from_iter_exact(self.t1.drain(..)));
                Some(Value::Array(ValArray::from_iter_exact([v0, v1].into_iter())))
            }
            _ => None,
        }
    }
}

pub(super) type Unzip = CachedArgs<UnzipEv>;

#[derive(Debug)]
pub(super) struct Group<R: Rt, E: UserEvent> {
    queue: VecDeque<Value>,
    buf: SmallVec<[Value; 16]>,
    pred: Node<R, E>,
    ready: bool,
    pid: BindId,
    nid: BindId,
    xid: BindId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Group<R, E> {
    const NAME: &str = "array_group";
    deftype!(
        "core::array",
        "fn('a, fn(i64, 'a) -> bool throws 'e) -> Array<'a> throws 'e"
    );

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, typ, scope, from, top_id| match from {
            [_, _] => {
                let scope =
                    scope.append(&format_compact!("fn{}", LambdaId::new().inner()));
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
                let pred = genn::apply(fnode, scope, vec![n, x], &mftyp, top_id);
                Ok(Box::new(Self {
                    queue: VecDeque::new(),
                    buf: smallvec![],
                    pred,
                    ready: true,
                    pid,
                    nid,
                    xid,
                }))
            }
            _ => bail!("expected two arguments"),
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Group<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! set {
            ($v:expr) => {{
                self.ready = false;
                self.buf.push($v.clone());
                let len = Value::I64(self.buf.len() as i64);
                ctx.cached.insert(self.nid, len.clone());
                event.variables.insert(self.nid, len);
                ctx.cached.insert(self.xid, $v.clone());
                event.variables.insert(self.xid, $v);
            }};
        }
        if let Some(v) = from[0].update(ctx, event) {
            self.queue.push_back(v);
        }
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.pid, v.clone());
            event.variables.insert(self.pid, v);
        }
        if self.ready && self.queue.len() > 0 {
            let v = self.queue.pop_front().unwrap();
            set!(v);
        }
        loop {
            match self.pred.update(ctx, event) {
                None => break None,
                Some(v) => {
                    self.ready = true;
                    match v {
                        Value::Bool(true) => {
                            break Some(Value::Array(ValArray::from_iter_exact(
                                self.buf.drain(..),
                            )))
                        }
                        _ => match self.queue.pop_front() {
                            None => break None,
                            Some(v) => set!(v),
                        },
                    }
                }
            }
        }
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.nid);
        ctx.cached.remove(&self.pid);
        ctx.cached.remove(&self.xid);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.sleep(ctx);
    }
}

#[derive(Debug)]
pub(super) struct Iter(BindId, ExprId);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Iter {
    const NAME: &str = "array_iter";
    deftype!("core::array", "fn(Array<'a>) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, _, _, _, top_id| {
            let id = BindId::new();
            ctx.rt.ref_var(id, top_id);
            Ok(Box::new(Iter(id, top_id)))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Iter {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(Value::Array(a)) = from[0].update(ctx, event) {
            for v in a.iter() {
                ctx.rt.set_var(self.0, v.clone());
            }
        }
        event.variables.get(&self.0).map(|v| v.clone())
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.0, self.1)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        // there may be in flight updates which we now must ignore forever
        ctx.rt.unref_var(self.0, self.1);
        self.0 = BindId::new();
        ctx.rt.ref_var(self.0, self.1);
    }
}

#[derive(Debug)]
pub(super) struct IterQ {
    triggered: usize,
    queue: VecDeque<(usize, ValArray)>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IterQ {
    const NAME: &str = "array_iterq";
    deftype!("core::array", "fn(#clock:Any, Array<'a>) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, _, _, _, top_id| {
            let id = BindId::new();
            ctx.rt.ref_var(id, top_id);
            Ok(Box::new(IterQ { triggered: 0, queue: VecDeque::new(), id, top_id }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IterQ {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(Value::Array(a)) = from[1].update(ctx, event) {
            if a.len() > 0 {
                self.queue.push_back((0, a));
            }
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            let (i, a) = self.queue.front_mut().unwrap();
            while self.triggered > 0 && *i < a.len() {
                ctx.rt.set_var(self.id, a[*i].clone());
                *i += 1;
                self.triggered -= 1;
            }
            if *i == a.len() {
                self.queue.pop_front();
            }
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        self.queue.clear();
        self.triggered = 0;
    }
}

pub(super) fn register<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<R, E>) -> Result<(ArcStr, ArcStr)> {
    ctx.register_builtin::<Concat>()?;
    ctx.register_builtin::<Filter<R, E>>()?;
    ctx.register_builtin::<FilterMap<R, E>>()?;
    ctx.register_builtin::<Find<R, E>>()?;
    ctx.register_builtin::<FindMap<R, E>>()?;
    ctx.register_builtin::<FlatMap<R, E>>()?;
    ctx.register_builtin::<Enumerate>()?;
    ctx.register_builtin::<Zip>()?;
    ctx.register_builtin::<Unzip>()?;
    ctx.register_builtin::<Flatten>()?;
    ctx.register_builtin::<Fold<R, E>>()?;
    ctx.register_builtin::<Group<R, E>>()?;
    ctx.register_builtin::<Iter>()?;
    ctx.register_builtin::<IterQ>()?;
    ctx.register_builtin::<Len>()?;
    ctx.register_builtin::<Map<R, E>>()?;
    ctx.register_builtin::<PushBack>()?;
    ctx.register_builtin::<PushFront>()?;
    ctx.register_builtin::<Sort>()?;
    ctx.register_builtin::<Window>()?;
    Ok((literal!(include_str!("array.gx")), literal!(include_str!("array.gxi"))))
}
