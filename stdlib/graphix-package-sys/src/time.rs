use anyhow::Result;
use arcstr::literal;

use chrono::Utc;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    err,
    expr::ExprId,
    image::{self, ImageBuf},
    node::WakeBit,
    typ::FnType,
};
use graphix_package_core::{seam_arg, seam_tick, seam_value};
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use std::time::Duration;

/// Emit the latest value after the inputs have been idle for the timeout.
#[derive(Debug)]
pub(crate) struct AfterIdle {
    /// The latest raw timeout value — re-cast when a delivery
    /// (re)arms the idle timer.
    timeout_v: Option<Value>,
    /// The latest value of the watched arg — the emission source when
    /// the timer fires, after the arg's delivery is gone.
    last_v: Option<Value>,
    id: Option<BindId>,
    /// A wait a sleep paused, which the next update resumes.
    slept: WakeBit,
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
            slept: WakeBit::default(),
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
            slept: WakeBit::default(),
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
        let resume = self.slept.take();
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
            && (timeout_up || val_up || resume)
        {
            match secs.clone().cast_to::<Duration>() {
                Ok(dur) => {
                    if let Some(old) = self.id.take() {
                        ctx.release_var(old, self.eid);
                    }
                    let id = BindId::new();
                    self.id = Some(id);
                    ctx.ref_var(id, self.eid);
                    ctx.rt.set_timer(id, dur);
                    return self.out.ride();
                }
                Err(e) => {
                    log::error!(
                        target: graphix_compiler::FAILURE_TARGET,
                        "after_idle: invalid timeout {secs}: {e:#}"
                    );
                }
            }
        }
        let res = self.id.and_then(|id| {
            if ctx.event.variables.contains_key(&id) {
                self.id = None;
                ctx.release_var(id, self.eid);
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
            ctx.release_var(id, self.eid)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
        if let Some(id) = self.id.take() {
            ctx.release_var(id, self.eid);
            self.slept.set();
        }
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = WakeBit::default();
        self.timeout_v = None;
        self.last_v = None;
        self.out = TagValue::phantom();
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
/// argument fires; a sleep pauses it and the wake resumes it.
#[derive(Debug)]
pub(crate) struct Timer {
    timeout: Option<Duration>,
    /// Fires left in this run, `None` forever.
    left: Option<u64>,
    id: Option<BindId>,
    /// A run a sleep paused, which the next update resumes.
    slept: WakeBit,
    eid: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Timer {
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "sys_time_timer";
    const RESUMES: bool = true;

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
            slept: WakeBit::default(),
            eid,
            out: TagValue::phantom(),
        }
    }

    fn stop<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(id) = self.id.take() {
            ctx.release_var(id, self.eid);
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
            ctx.ref_var(id, self.eid);
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
        if self.slept.take() {
            self.arm(ctx);
        }
        let (timeout, timeout_fired) = seam_arg(ctx, &mut from[0]);
        let (repeat, repeat_fired) = seam_arg(ctx, &mut from[1]);
        if (timeout_fired || repeat_fired)
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
        // CR claude for eric: [bug/medium] Clearing the output here, as every event
        // source's sleep does, makes the source bottom at the wake. A bottomed argument
        // bottoms its consumer, so state built on the event is hidden until the next
        // event. In the probe, count(timer) in a woken arm shows nothing for three
        // outer ticks though it kept its 2. count(x) over a variable shows its count at
        // a re-entry, as wake_resumes_quietly pins. A UI arm showing count(tick) or
        // hold(#clock: tick, v) blanks for up to a period at every re-entry.
        // wake_resumes_standing_timers relies on this: it counts count(timer)'s fires,
        // and a count shown at the re-entry would add one. Rule on what a woken event
        // source reads as to its consumers: absent without bottom (no fire), or bottom
        // as now, documented in design/async_sleep_outputs.md. Then pin count(timer) at
        // a re-entry. probe:
        // design/review-2026-10-19/repro/x-engine-firing-event-clear-bottoms-consumer.gx
        // (x-engine-firing-03)
        // 2026-10-24 claude: Eric ruled (2026-10-24): absent without bottom: a woken
        // event source does not fire and is not bottom, so its consumers keep showing
        // their state.
        // 2026-10-25 claude: Tried the ruling in its one form the tags can hold today:
        // each of the 15 event sources' sleep() kept its output and rode it stale
        // (out.ride() for out = phantom) instead of clearing it, so a woken source
        // neither fires nor is bottom and count(timer) keeps showing its count. Six pins
        // of the rule this replaces then fail in every mode:
        // async_restart::{iter_reentry_emits_no_old_element, line_reader_rewake,
        // wake_resumes_standing_timers},
        // db::db_subscription_accessors_do_not_replay_on_wake,
        // net::{net_subscribe_arm_rewake, net_subscribe_arm_rewake_moved_path}. A select
        // emits its arm's current value at a re-entry, so an arm whose body is the event
        // itself (`On => subscribe(p)`, a line reader, an iter) re-emits the old event
        // as new, which design/async_sleep_outputs.md names as the reason for the clear.
        // Reverted. The ruling as worded needs a third state beside value and bottom,
        // absent: never fires, does not bottom a consumer (count rides its resident over
        // it), and gives a select nothing to emit at the re-entry, in the TagValue tags
        // and the JIT's discs alike. Back to eric: build that state, or accept the
        // re-emission (and rewrite those six pins), or keep the clear.
        self.out = TagValue::phantom();
        self.slept.set();
        self.stop(ctx)
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.slept = WakeBit::default();
        self.timeout = None;
        self.out = TagValue::phantom();
    }
}

#[derive(Debug)]
pub(crate) struct Now {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Now {
    const EFFECT: Effect = Effect::Sync;
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

        graphix_package_core::fast_builtin!(pub(crate) $ty, $ev, $name, $fc);
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
