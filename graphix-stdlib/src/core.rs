use crate::{deftype, CachedArgs, CachedVals, EvalCached};
use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use graphix_compiler::{
    err, errf,
    expr::{Expr, ExprId, ModPath},
    node::genn,
    typ::FnType,
    Apply, BindId, BuiltIn, BuiltInInitFn, Event, ExecCtx, Node, Refs, Rt, UserEvent,
};
use netidx::subscriber::Value;
use netidx_value::FromValue;
use std::{collections::VecDeque, sync::Arc, time::Duration};
use tokio::time::Instant;
use triomphe::Arc as TArc;

#[derive(Debug)]
struct IsErr;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IsErr {
    const NAME: &str = "is_err";
    deftype!("core", "fn(Any) -> bool");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(IsErr)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IsErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|v| match v {
            Value::Error(_) => Value::Bool(true),
            _ => Value::Bool(false),
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct FilterErr;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for FilterErr {
    const NAME: &str = "filter_err";
    deftype!("core", "fn(Any) -> error");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(FilterErr)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for FilterErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|v| match v {
            v @ Value::Error(_) => Some(v),
            _ => None,
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct ToError;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ToError {
    const NAME: &str = "error";
    deftype!("core", "fn(Any) -> error");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(ToError)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ToError {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|v| match v.cast_to::<ArcStr>() {
            Ok(s) => Value::Error(s),
            Err(e) => Value::Error(format_compact!("{e}").as_str().into()),
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Once {
    val: bool,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    const NAME: &str = "once";
    deftype!("core", "fn('a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(Once { val: false })))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Once {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        match from {
            [s] => s.update(ctx, event).and_then(|v| {
                if self.val {
                    None
                } else {
                    self.val = true;
                    Some(v)
                }
            }),
            _ => None,
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.val = false
    }
}

#[derive(Debug, Default)]
struct AllEv;

impl EvalCached for AllEv {
    const NAME: &str = "all";
    deftype!("core", "fn(@args: Any) -> Any");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &*from.0 {
            [] => None,
            [hd, tl @ ..] => match hd {
                None => None,
                v @ Some(_) => {
                    if tl.into_iter().all(|v1| v1 == v) {
                        v.clone()
                    } else {
                        None
                    }
                }
            },
        }
    }
}

type All = CachedArgs<AllEv>;

fn add_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l + r),
    }
}

#[derive(Debug, Default)]
struct SumEv;

impl EvalCached for SumEv {
    const NAME: &str = "sum";
    deftype!("core", "fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => add_vals(res, v.clone()),
        })
    }
}

type Sum = CachedArgs<SumEv>;

#[derive(Debug, Default)]
struct ProductEv;

fn prod_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l * r),
    }
}

impl EvalCached for ProductEv {
    const NAME: &str = "product";
    deftype!("core", "fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => prod_vals(res, v.clone()),
        })
    }
}

type Product = CachedArgs<ProductEv>;

#[derive(Debug, Default)]
struct DivideEv;

fn div_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l / r),
    }
}

impl EvalCached for DivideEv {
    const NAME: &str = "divide";
    deftype!("core", "fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => div_vals(res, v.clone()),
        })
    }
}

type Divide = CachedArgs<DivideEv>;

#[derive(Debug, Default)]
struct MinEv;

impl EvalCached for MinEv {
    const NAME: &str = "min";
    deftype!("core", "fn('a, @args:'a) -> 'a");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => {
                    res = Some(v);
                }
                (Some(v0), Some(v)) => {
                    res = if v < v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
    }
}

type Min = CachedArgs<MinEv>;

#[derive(Debug, Default)]
struct MaxEv;

impl EvalCached for MaxEv {
    const NAME: &str = "max";
    deftype!("core", "fn('a, @args: 'a) -> 'a");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => {
                    res = Some(v);
                }
                (Some(v0), Some(v)) => {
                    res = if v > v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
    }
}

type Max = CachedArgs<MaxEv>;

#[derive(Debug, Default)]
struct AndEv;

impl EvalCached for AndEv {
    const NAME: &str = "and";
    deftype!("core", "fn(@args: bool) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(true));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => (),
                Some(_) => {
                    res = Some(Value::Bool(false));
                }
            }
        }
        res
    }
}

type And = CachedArgs<AndEv>;

#[derive(Debug, Default)]
struct OrEv;

impl EvalCached for OrEv {
    const NAME: &str = "or";
    deftype!("core", "fn(@args: bool) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(false));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => {
                    res = Some(Value::Bool(true));
                }
                Some(_) => (),
            }
        }
        res
    }
}

type Or = CachedArgs<OrEv>;

#[derive(Debug)]
struct Filter<R: Rt, E: UserEvent> {
    ready: bool,
    queue: VecDeque<Value>,
    pred: Node<R, E>,
    typ: TArc<FnType>,
    top_id: ExprId,
    fid: BindId,
    x: BindId,
    out: BindId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Filter<R, E> {
    const NAME: &str = "filter";
    deftype!("core", "fn('a, fn('a) -> bool) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, typ, scope, from, top_id| match from {
            [arg, fnode] => {
                let (x, xn) = genn::bind(ctx, scope, "x", arg.typ().clone(), top_id);
                let fid = BindId::new();
                let fnode = genn::reference(ctx, fid, fnode.typ().clone(), top_id);
                let typ = TArc::new(typ.clone());
                let pred = genn::apply(fnode, vec![xn], typ.clone(), top_id);
                let queue = VecDeque::new();
                let out = BindId::new();
                ctx.rt.ref_var(out, top_id);
                Ok(Box::new(Self { ready: true, queue, pred, typ, fid, x, out, top_id }))
            }
            _ => bail!("expected two arguments"),
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Filter<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! set {
            ($v:expr) => {{
                self.ready = false;
                ctx.cached.insert(self.x, $v.clone());
                event.variables.insert(self.x, $v);
            }};
        }
        macro_rules! maybe_cont {
            () => {{
                if let Some(v) = self.queue.front().cloned() {
                    set!(v);
                    continue;
                }
                break;
            }};
        }
        if let Some(v) = from[0].update(ctx, event) {
            self.queue.push_back(v);
        }
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        if self.ready && self.queue.len() > 0 {
            let v = self.queue.front().unwrap().clone();
            set!(v);
        }
        loop {
            match self.pred.update(ctx, event) {
                None => break,
                Some(v) => {
                    self.ready = true;
                    match v {
                        Value::Bool(true) => {
                            ctx.rt.set_var(self.out, self.queue.pop_front().unwrap());
                            maybe_cont!();
                        }
                        _ => {
                            let _ = self.queue.pop_front();
                            maybe_cont!();
                        }
                    }
                }
            }
        }
        event.variables.get(&self.out).map(|v| v.clone())
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        for n in from.iter_mut() {
            n.typecheck(ctx)?;
        }
        self.typ.args[0].typ.check_contains(&ctx.env, &from[0].typ())?;
        self.typ.args[1].typ.check_contains(&ctx.env, &from[1].typ())?;
        self.pred.typecheck(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.fid);
        ctx.cached.remove(&self.out);
        ctx.cached.remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.pred.delete(ctx);
        ctx.rt.unref_var(self.out, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.out, self.top_id);
        self.out = BindId::new();
        ctx.rt.ref_var(self.out, self.top_id);
        self.queue.clear();
        self.pred.sleep(ctx);
    }
}

#[derive(Debug)]
struct Queue {
    triggered: usize,
    queue: VecDeque<Value>,
    id: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Queue {
    const NAME: &str = "queue";
    deftype!("core", "fn(#clock:Any, 'a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, _, _, from, top_id| match from {
            [_, _] => {
                let id = BindId::new();
                ctx.rt.ref_var(id, top_id);
                Ok(Box::new(Self { triggered: 0, queue: VecDeque::new(), id, top_id }))
            }
            _ => bail!("expected two arguments"),
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Queue {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(v) = from[1].update(ctx, event) {
            self.queue.push_back(v);
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            self.triggered -= 1;
            ctx.rt.set_var(self.id, self.queue.pop_front().unwrap());
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.triggered = 0;
        self.queue.clear();
    }
}

#[derive(Debug)]
struct Seq {
    id: BindId,
    top_id: ExprId,
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Seq {
    const NAME: &str = "seq";
    deftype!("core", "fn(i64, i64) -> i64");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|ctx, _, _, from, top_id| {
            let id = BindId::new();
            ctx.rt.ref_var(id, top_id);
            let args = CachedVals::new(from);
            Ok(Box::new(Self { id, top_id, args }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Seq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.args.update(ctx, from, event) {
            match &self.args.0[..] {
                [Some(Value::I64(i)), Some(Value::I64(j))] if i <= j => {
                    for v in *i..*j {
                        ctx.rt.set_var(self.id, Value::I64(v));
                    }
                }
                _ => return err!("invalid args i must be <= j"),
            }
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    last: Option<Instant>,
    tid: Option<BindId>,
    top_id: ExprId,
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Throttle {
    const NAME: &str = "throttle";
    deftype!("core", "fn(?#rate:duration, 'a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, from, top_id| {
            let args = CachedVals::new(from);
            Ok(Box::new(Self {
                wait: Duration::ZERO,
                last: None,
                tid: None,
                top_id,
                args,
            }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Throttle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! maybe_schedule {
            ($last:expr) => {{
                let now = Instant::now();
                if now - *$last >= self.wait {
                    *$last = now;
                    return self.args.0[1].clone();
                } else {
                    let id = BindId::new();
                    ctx.rt.ref_var(id, self.top_id);
                    ctx.rt.set_timer(id, self.wait - (now - *$last));
                    self.tid = Some(id);
                    return None;
                }
            }};
        }
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        if up[0]
            && let Some(Value::Duration(d)) = &self.args.0[0]
        {
            self.wait = *d;
            if let Some(id) = self.tid.take()
                && let Some(last) = &mut self.last
            {
                ctx.rt.unref_var(id, self.top_id);
                maybe_schedule!(last)
            }
        }
        if up[1] && self.tid.is_none() {
            match &mut self.last {
                Some(last) => maybe_schedule!(last),
                None => {
                    self.last = Some(Instant::now());
                    return self.args.0[1].clone();
                }
            }
        }
        if let Some(id) = self.tid
            && let Some(_) = event.variables.get(&id)
        {
            ctx.rt.unref_var(id, self.top_id);
            self.tid = None;
            self.last = Some(Instant::now());
            return self.args.0[1].clone();
        }
        None
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.tid.take() {
            ctx.rt.unref_var(id, self.top_id);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.delete(ctx);
        self.last = None;
        self.wait = Duration::ZERO;
        self.args.clear();
    }
}

#[derive(Debug)]
struct Count {
    count: i64,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Count {
    const NAME: &str = "count";
    deftype!("core", "fn(Any) -> i64");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(Count { count: 0 })))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Count {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from.into_iter().fold(false, |u, n| u || n.update(ctx, event).is_some()) {
            self.count += 1;
            Some(Value::I64(self.count))
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.count = 0
    }
}

#[derive(Debug, Default)]
struct MeanEv;

impl EvalCached for MeanEv {
    const NAME: &str = "mean";
    deftype!(
        "core",
        "fn([Number, Array<Number>], @args: [Number, Array<Number>]) -> f64"
    );

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        let mut total = 0.;
        let mut samples = 0;
        let mut error = None;
        for v in from.flat_iter() {
            if let Some(v) = v {
                match v.cast_to::<f64>() {
                    Err(e) => error = errf!("{e:?}"),
                    Ok(v) => {
                        total += v;
                        samples += 1;
                    }
                }
            }
        }
        if let Some(e) = error {
            Some(e)
        } else if samples == 0 {
            err!("mean requires at least one argument")
        } else {
            Some(Value::F64(total / samples as f64))
        }
    }
}

type Mean = CachedArgs<MeanEv>;

#[derive(Debug)]
struct Uniq(Option<Value>);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Uniq {
    const NAME: &str = "uniq";
    deftype!("core", "fn('a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(Uniq(None))))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Uniq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|v| {
            if Some(&v) != self.0.as_ref() {
                self.0 = Some(v.clone());
                Some(v)
            } else {
                None
            }
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.0 = None
    }
}

#[derive(Debug)]
struct Never;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Never {
    const NAME: &str = "never";
    deftype!("core", "fn(@args: Any) -> _");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(Never)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Never {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        for n in from {
            n.update(ctx, event);
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Clone, Copy)]
enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl FromValue for Level {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Trace" => Ok(Self::Trace),
            "Debug" => Ok(Self::Debug),
            "Info" => Ok(Self::Info),
            "Warn" => Ok(Self::Warn),
            "Error" => Ok(Self::Error),
            v => bail!("invalid log level {v}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum LogDest {
    Stdout,
    Stderr,
    Log(Level),
}

impl FromValue for LogDest {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.clone().cast_to::<ArcStr>()? {
            "Stdout" => Ok(Self::Stdout),
            "Stderr" => Ok(Self::Stderr),
            _ => Ok(Self::Log(v.cast_to()?)),
        }
    }
}

#[derive(Debug)]
struct Dbg {
    spec: Expr,
    dest: LogDest,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Dbg {
    const NAME: &str = "dbg";
    deftype!("core", "fn(?#dest:[`Stdout, `Stderr, Log], 'a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, from, _| {
            Ok(Box::new(Dbg { spec: from[1].spec().clone(), dest: LogDest::Stderr }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Dbg {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[0].update(ctx, event)
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        from[1].update(ctx, event).map(|v| {
            match self.dest {
                LogDest::Stderr => eprintln!("{} dbg({}): {v}", self.spec.pos, self.spec),
                LogDest::Stdout => println!("{} dbg({}): {v}", self.spec.pos, self.spec),
                LogDest::Log(level) => match level {
                    Level::Trace => {
                        log::trace!("{} dbg({}): {v}", self.spec.pos, self.spec)
                    }
                    Level::Debug => {
                        log::debug!("{} dbg({}): {v}", self.spec.pos, self.spec)
                    }
                    Level::Info => {
                        log::info!("{} dbg({}): {v}", self.spec.pos, self.spec)
                    }
                    Level::Warn => {
                        log::warn!("{} dbg({}): {v}", self.spec.pos, self.spec)
                    }
                    Level::Error => {
                        log::error!("{} dbg({}): {v}", self.spec.pos, self.spec)
                    }
                },
            };
            v
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Log {
    scope: ModPath,
    dest: Level,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Log {
    const NAME: &str = "log";
    deftype!("core", "fn(?#level:Log, 'a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, scope, _, _| {
            Ok(Box::new(Self { scope: scope.clone(), dest: Level::Info }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Log {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[0].update(ctx, event)
            && let Ok(d) = v.cast_to::<Level>()
        {
            self.dest = d;
        }
        if let Some(v) = from[1].update(ctx, event) {
            match self.dest {
                Level::Trace => log::trace!("{}: {v}", self.scope),
                Level::Debug => log::debug!("{}: {v}", self.scope),
                Level::Info => log::info!("{}: {v}", self.scope),
                Level::Warn => log::warn!("{}: {v}", self.scope),
                Level::Error => log::error!("{}: {v}", self.scope),
            }
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

pub(super) fn register<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<R, E>) -> Result<ArcStr> {
    ctx.register_builtin::<Queue>()?;
    ctx.register_builtin::<All>()?;
    ctx.register_builtin::<And>()?;
    ctx.register_builtin::<Count>()?;
    ctx.register_builtin::<Divide>()?;
    ctx.register_builtin::<Filter<R, E>>()?;
    ctx.register_builtin::<FilterErr>()?;
    ctx.register_builtin::<IsErr>()?;
    ctx.register_builtin::<Max>()?;
    ctx.register_builtin::<Mean>()?;
    ctx.register_builtin::<Min>()?;
    ctx.register_builtin::<Never>()?;
    ctx.register_builtin::<Once>()?;
    ctx.register_builtin::<Seq>()?;
    ctx.register_builtin::<Or>()?;
    ctx.register_builtin::<Product>()?;
    ctx.register_builtin::<Sum>()?;
    ctx.register_builtin::<Uniq>()?;
    ctx.register_builtin::<ToError>()?;
    ctx.register_builtin::<Dbg>()?;
    ctx.register_builtin::<Log>()?;
    ctx.register_builtin::<Throttle>()?;
    Ok(literal!(include_str!("core.gx")))
}
