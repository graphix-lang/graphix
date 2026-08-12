use anyhow::{Result, bail};
use arcstr::literal;
use chrono::Utc;
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::EffectKind, err, expr::ExprId, typ::FnType,
};
use graphix_package_core::{CachedVals, seam_tick, seam_value};
use netidx::{publisher::FromValue, subscriber::Value};
use std::{ops::SubAssign, time::Duration};

#[derive(Debug)]
pub(crate) struct AfterIdle {
    /// The latest raw timeout value — re-cast when a delivery
    /// (re)arms the idle timer.
    timeout_v: Option<Value>,
    /// The latest value of the watched arg — the emission source when
    /// the timer fires (async, after the arg's delivery is long
    /// gone). An explicit OWN field, not an arg-cache slot
    /// (design/dense_delivery.md, the throttle/timer P4 item).
    last_v: Option<Value>,
    id: Option<BindId>,
    eid: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for AfterIdle {
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "sys_time_after_idle";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
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
            eid: top_id,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for AfterIdle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let mut timeout_up = false;
        if let Some(tv) = seam_value(from[0].update(ctx, event)) {
            timeout_up = tv.is_fired();
            self.timeout_v = Some(tv.value_cloned());
        }
        let mut val_up = false;
        if let Some(tv) = seam_value(from[1].update(ctx, event)) {
            val_up = tv.is_fired();
            self.last_v = Some(tv.value_cloned());
        }
        if let Some(secs) = &self.timeout_v
            && (timeout_up || val_up)
        {
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
            if event.variables.contains_key(&id) {
                self.id = None;
                ctx.rt.unref_var(id, self.eid);
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.eid)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.eid);
        }
        self.timeout_v = None;
        self.last_v = None
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.timeout_v = None;
        self.last_v = None
    }
}

#[derive(Debug, Clone, Copy)]
enum Repeat {
    Yes,
    No,
    N(u64),
}

impl FromValue for Repeat {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::Bool(true) => Ok(Repeat::Yes),
            Value::Bool(false) => Ok(Repeat::No),
            v => match v.cast_to::<u64>() {
                Ok(n) => Ok(Repeat::N(n)),
                Err(_) => bail!("could not cast to repeat"),
            },
        }
    }
}

impl SubAssign<u64> for Repeat {
    fn sub_assign(&mut self, rhs: u64) {
        match self {
            Repeat::Yes | Repeat::No => (),
            Repeat::N(n) => *n -= rhs,
        }
    }
}

impl Repeat {
    fn will_repeat(&self) -> bool {
        match self {
            Repeat::No => false,
            Repeat::Yes => true,
            Repeat::N(n) => *n > 0,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Timer {
    /// The latest raw repeat value — re-cast when a later timeout
    /// delivery (re)schedules, the cross-cycle read the arg-cache
    /// slot used to serve.
    repeat_v: Option<Value>,
    timeout: Option<Duration>,
    repeat: Repeat,
    id: Option<BindId>,
    eid: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Timer {
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "sys_time_timer";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self {
            repeat_v: None,
            timeout: None,
            repeat: Repeat::No,
            id: None,
            eid: top_id,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Timer {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        macro_rules! error {
            () => {{
                self.id = None;
                self.timeout = None;
                self.repeat = Repeat::No;
                return self.out.set(TagValue::fired(err!(
                    literal!("TimerError"),
                    "timer(per, rep): expected duration, bool or number >= 0"
                )));
            }};
        }
        macro_rules! schedule {
            ($dur:expr) => {{
                let id = BindId::new();
                self.id = Some(id);
                ctx.rt.ref_var(id, self.eid);
                ctx.rt.set_timer(id, $dur);
            }};
        }
        let new_timeout = match seam_value(from[0].update(ctx, event)) {
            Some(tv) if tv.is_fired() => Some(tv.value_cloned()),
            _ => None,
        };
        let mut repeat_up = false;
        if let Some(tv) = seam_value(from[1].update(ctx, event)) {
            repeat_up = tv.is_fired();
            self.repeat_v = Some(tv.value_cloned());
        }
        match (new_timeout, &self.repeat_v, repeat_up) {
            (None, Some(r), true) => match r.clone().cast_to::<Repeat>() {
                Err(_) => error!(),
                Ok(repeat) => {
                    self.repeat = repeat;
                    if let Some(dur) = self.timeout {
                        if self.id.is_none() && repeat.will_repeat() {
                            schedule!(dur)
                        }
                    }
                }
            },
            (Some(s), None, _) => match s.cast_to::<Duration>() {
                Err(_) => error!(),
                Ok(dur) => self.timeout = Some(dur),
            },
            (Some(s), Some(r), _) => {
                match (s.cast_to::<Duration>(), r.clone().cast_to::<Repeat>()) {
                    (Err(_), _) | (_, Err(_)) => error!(),
                    (Ok(dur), Ok(repeat)) => {
                        self.timeout = Some(dur);
                        self.repeat = repeat;
                        schedule!(dur)
                    }
                }
            }
            (None, _, _) => (),
        }
        let res = self
            .id
            .and_then(|id| event.variables.get(&id).map(|now| (id, now)))
            .map(|(id, now)| {
                ctx.rt.unref_var(id, self.eid);
                self.id = None;
                self.repeat -= 1;
                if let Some(dur) = self.timeout {
                    if self.repeat.will_repeat() {
                        schedule!(dur)
                    }
                }
                now.value_cloned()
            });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.eid);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.repeat_v = None;
        self.timeout = None;
        self.repeat = Repeat::No;
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.eid);
        }
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.repeat_v = None
    }
}

#[derive(Debug)]
pub(crate) struct Now {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Now {
    // When trigger fires, samples the current time and emits same-cycle.
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "sys_time_now";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Now {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx, event), ctx.dense_seam).is_some() {
            self.out.set(TagValue::fired(Value::from(Utc::now())))
        } else {
            self.out.ride()
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

macro_rules! time_fn {
    ($ev:ident, $ty:ident, $name:literal, |$a:ident, $b:ident| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;
        impl<R: Rt, E: UserEvent> graphix_package_core::EvalCached<R, E> for $ev {
            const EFFECT: EffectKind = EffectKind::Sync;
            const STATELESS: bool = true;
            const NAME: &str = $name;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                let $a = from.get(0)?;
                let $b = from.get(1)?;
                Some($body)
            }
        }
        pub(crate) type $ty = graphix_package_core::CachedArgs<$ev>;
    };
}

/// Variant tag for the catchable errors the duration functions return.
static DURATION_ERR_TAG: arcstr::ArcStr = literal!("DurationError");

// The evicted datetime/duration OPERATOR semantics, verbatim (netidx
// op.rs): datetime ± duration SATURATES at the datetime range limits;
// duration − duration SATURATES at zero (durations are unsigned —
// graphix #176 C); duration + duration and scaling are CATCHABLE
// errors on overflow / negative / NaN (function-land gets the rare-
// stdlib-fn error discipline, where the operator logged and bottomed).
time_fn!(TimeAddEv, TimeAdd, "sys_time_add", |t, d| {
    let t: chrono::DateTime<Utc> = t;
    let d: Duration = d;
    match chrono::Duration::from_std(d).ok().and_then(|d| t.checked_add_signed(d)) {
        Some(t) => Value::from(t),
        None => Value::from(chrono::DateTime::<Utc>::MAX_UTC),
    }
});

time_fn!(TimeSubEv, TimeSub, "sys_time_sub", |t, d| {
    let t: chrono::DateTime<Utc> = t;
    let d: Duration = d;
    match chrono::Duration::from_std(d).ok().and_then(|d| t.checked_sub_signed(d)) {
        Some(t) => Value::from(t),
        None => Value::from(chrono::DateTime::<Utc>::MIN_UTC),
    }
});

time_fn!(TimeAddDurEv, TimeAddDur, "sys_time_add_dur", |a, b| {
    let a: Duration = a;
    let b: Duration = b;
    match a.checked_add(b) {
        Some(d) => Value::Duration(d.into()),
        None => graphix_compiler::err!(DURATION_ERR_TAG, "duration overflow"),
    }
});

time_fn!(TimeSubDurEv, TimeSubDur, "sys_time_sub_dur", |a, b| {
    let a: Duration = a;
    let b: Duration = b;
    Value::Duration(a.saturating_sub(b).into())
});

time_fn!(TimeScaleEv, TimeScale, "sys_time_scale", |d, by| {
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
