use anyhow::Result;
use arcstr::literal;

use chrono::Utc;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, FastCall, Node, Rt, Scope, TagValue,
    UserEvent,
    effects::Effect,
    err,
    expr::ExprId,
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, fast_eval, seam_arg, seam_tick, seam_value,
};
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use std::time::Duration;

/// Drop a timer's private fire id: its timer, its reference and the value
/// the runtime stored for it, which no one else can read.
fn release<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<'_, R, E>, id: BindId, eid: ExprId) {
    ctx.rt.cancel_timer(id);
    ctx.unref_var(id, eid);
    ctx.rt.store_remove(&id);
}

#[derive(Debug)]
pub(crate) struct AfterIdle {
    /// The latest raw timeout value — re-cast when a delivery
    /// (re)arms the idle timer.
    timeout_v: Option<Value>,
    /// The latest value of the watched arg — the emission source when
    /// the timer fires, after the arg's delivery is gone.
    last_v: Option<Value>,
    id: Option<BindId>,
    /// Set by `sleep`, taken by the next update: the wait starts again
    /// over the present arguments.
    slept: bool,
    eid: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for AfterIdle {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_time_after_idle";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(AfterIdle {
            timeout_v: None,
            last_v: None,
            id: None,
            slept: false,
            eid: top_id,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let timeout_v = Pack::decode(buf)?;
        let last_v = Pack::decode(buf)?;
        let eid = ExprId::decode(buf)?;
        Ok(Box::new(AfterIdle {
            timeout_v,
            last_v,
            id: None,
            slept: false,
            eid,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for AfterIdle {
    /// `id` is an armed runtime timer.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.id.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.timeout_v.encode(buf)?;
        self.last_v.encode(buf)?;
        self.eid.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.slept);
        let mut timeout_up = false;
        if let Some(tv) = seam_value(from[0].update(ctx)) {
            timeout_up = tv.is_fired();
            self.timeout_v = Some(tv.value_cloned());
        }
        let mut val_up = false;
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            val_up = tv.is_fired();
            self.last_v = Some(tv.value_cloned());
        }
        if let Some(secs) = &self.timeout_v
            && (timeout_up || val_up || (woke && self.last_v.is_some()))
        {
            if let Some(old) = self.id.take() {
                release(ctx, old, self.eid);
            }
            match secs.clone().cast_to::<Duration>() {
                Ok(dur) => {
                    let id = BindId::new();
                    self.id = Some(id);
                    ctx.rt.ref_var(id, self.eid);
                    ctx.rt.set_timer(id, dur);
                    return self.out.ride();
                }
                Err(_) => {
                    self.id = None;
                    return self.out.ride();
                }
            }
        }
        let res = self.id.and_then(|id| {
            if ctx.event.variables.contains_key(&id) {
                self.id = None;
                release(ctx, id, self.eid);
                self.last_v.clone()
            } else {
                None
            }
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(id) = self.id.take() {
            release(ctx, id, self.eid)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
        if let Some(id) = self.id.take() {
            release(ctx, id, self.eid);
        }
        self.timeout_v = None;
        self.last_v = None;
        self.slept = true;
    }
}

/// How many fires a timer's `repeat` asks for: `None` forever.
fn fires(repeat: &Value) -> Option<Option<u64>> {
    match repeat {
        Value::Bool(true) => Some(None),
        Value::Bool(false) => Some(Some(1)),
        Value::I8(n) if *n < 0 => None,
        Value::I16(n) if *n < 0 => None,
        Value::I32(n) | Value::Z32(n) if *n < 0 => None,
        Value::I64(n) | Value::Z64(n) if *n < 0 => None,
        v => v.clone().cast_to::<u64>().ok().map(Some),
    }
}

/// `timer(timeout, repeat)`: a run of fires starts whenever either
/// argument fires, and again at a wake over the present arguments.
#[derive(Debug)]
pub(crate) struct Timer {
    timeout: Option<Duration>,
    /// Fires left in this run, `None` forever.
    left: Option<u64>,
    id: Option<BindId>,
    /// Set by `sleep`, taken by the next update.
    slept: bool,
    eid: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Timer {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_time_timer";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self::new(top_id)))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Self::new(ExprId::decode(buf)?)))
    }
}

impl Timer {
    fn new(eid: ExprId) -> Self {
        Self {
            timeout: None,
            left: None,
            id: None,
            slept: false,
            eid,
            out: TagValue::phantom(),
        }
    }

    fn stop<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(id) = self.id.take() {
            release(ctx, id, self.eid);
        }
    }

    /// Arm the next fire of the run, if it has one.
    fn arm<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.stop(ctx);
        if let Some(dur) = self.timeout
            && self.left != Some(0)
        {
            let id = BindId::new();
            self.id = Some(id);
            ctx.rt.ref_var(id, self.eid);
            ctx.rt.set_timer(id, dur);
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Timer {
    /// A run exists only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.timeout.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.eid.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.slept);
        let (timeout, timeout_fired) = seam_arg(ctx, &mut from[0]);
        let (repeat, repeat_fired) = seam_arg(ctx, &mut from[1]);
        if (woke || timeout_fired || repeat_fired)
            && let (Some(timeout), Some(repeat)) = (timeout, repeat)
        {
            match (timeout.cast_to::<Duration>(), fires(&repeat)) {
                (Ok(dur), Some(left)) => {
                    self.timeout = Some(dur);
                    self.left = left;
                    self.arm(ctx);
                }
                _ => {
                    self.stop(ctx);
                    self.timeout = None;
                    return self.out.set(TagValue::fired(err!(
                        literal!("TimerError"),
                        "timer(per, rep): expected duration, bool or number >= 0"
                    )));
                }
            }
        }
        let fired =
            self.id.and_then(|id| ctx.event.variables.get(&id)).map(|t| t.value_cloned());
        match fired {
            Some(now) => {
                self.left = self.left.map(|n| n.saturating_sub(1));
                self.arm(ctx);
                self.out.set(TagValue::fired(now))
            }
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.stop(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
        self.timeout = None;
        self.slept = true;
        self.stop(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct Now {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Now {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "sys_time_now";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Self { out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Now {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx)).is_some() {
            self.out.set(TagValue::fired(Value::from(Utc::now())))
        } else {
            self.out.ride()
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

macro_rules! time_fn {
    ($ev:ident, $ty:ident, $fc:ident, $name:literal, |$a:ident, $b:ident| $body:expr) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            let $a = args[0].clone().cast_to().ok()?;
            let $b = args[1].clone().cast_to().ok()?;
            Some($body)
        }

        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        graphix_package_core::unit_image_state!($ev);

        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $ev {
            const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain($fc)));
            const NAME: &str = $name;

            fn eval(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                fast_eval(ctx, $fc, from)
            }
        }

        pub(crate) type $ty = CachedArgs<$ev>;
    };
}

/// Variant tag for the catchable errors the duration functions return.
static DURATION_ERR_TAG: arcstr::ArcStr = literal!("DurationError");

// datetime ± duration saturates at the datetime range limits; duration −
// duration saturates at zero; duration + duration and scaling return
// catchable errors on overflow / negative / NaN.
time_fn!(TimeAddEv, TimeAdd, fc_time_add, "sys_time_add", |t, d| {
    let t: chrono::DateTime<Utc> = t;
    let d: Duration = d;
    match chrono::Duration::from_std(d).ok().and_then(|d| t.checked_add_signed(d)) {
        Some(t) => Value::from(t),
        None => Value::from(chrono::DateTime::<Utc>::MAX_UTC),
    }
});

time_fn!(TimeSubEv, TimeSub, fc_time_sub, "sys_time_sub", |t, d| {
    let t: chrono::DateTime<Utc> = t;
    let d: Duration = d;
    match chrono::Duration::from_std(d).ok().and_then(|d| t.checked_sub_signed(d)) {
        Some(t) => Value::from(t),
        None => Value::from(chrono::DateTime::<Utc>::MIN_UTC),
    }
});

time_fn!(TimeAddDurEv, TimeAddDur, fc_time_add_dur, "sys_time_add_dur", |a, b| {
    let a: Duration = a;
    let b: Duration = b;
    match a.checked_add(b) {
        Some(d) => Value::Duration(d.into()),
        None => graphix_compiler::err!(DURATION_ERR_TAG, "duration overflow"),
    }
});

time_fn!(TimeSubDurEv, TimeSubDur, fc_time_sub_dur, "sys_time_sub_dur", |a, b| {
    let a: Duration = a;
    let b: Duration = b;
    Value::Duration(a.saturating_sub(b).into())
});

time_fn!(TimeDiffEv, TimeDiff, fc_time_diff, "sys_time_diff", |later, earlier| {
    let later: chrono::DateTime<Utc> = later;
    let earlier: chrono::DateTime<Utc> = earlier;
    Value::Duration((later - earlier).to_std().unwrap_or(Duration::ZERO).into())
});

time_fn!(TimeScaleEv, TimeScale, fc_time_scale, "sys_time_scale", |d, by| {
    let d: Duration = d;
    let by: f64 = by;
    match Duration::try_from_secs_f64(d.as_secs_f64() * by) {
        Ok(d) => Value::Duration(d.into()),
        Err(_) => graphix_compiler::err!(
            DURATION_ERR_TAG,
            "invalid duration scale (negative, NaN, or overflow)"
        ),
    }
});
