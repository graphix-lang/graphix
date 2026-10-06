use anyhow::{Result, bail};
use arcstr::literal;
use bytes::{Buf, BufMut};
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
    CachedArgs, CachedVals, EvalCached, fast_eval, seam_tick, seam_value,
};
use netidx::{publisher::FromValue, subscriber::Value};
use netidx_core::pack::{Pack, PackError};
use std::{ops::SubAssign, time::Duration};

/// Drop a timer's private fire id: its reference and the value the
/// runtime stored for it, which no one else can read.
fn release<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<'_, R, E>, id: BindId, eid: ExprId) {
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
            && (timeout_up || val_up)
        {
            // CR claude for claude: [bug] Both arms drop an armed `self.id` without
            // `release`. Its `by_ref` entry stays for good, and when its timer fires
            // the runtime stores the dead id and updates this statement (for a script,
            // the whole program) for nothing. That is about 200 bytes and one stray
            // update per re-arm, and a debounce re-arms on every input. Timer does the
            // same in `schedule!` from the `(Some(s), Some(r), _)` arm and in
            // `error!()`. Releasing first is not the whole fix: `push_var_event`
            // (graphix-rt/src/gx.rs:455) stores every timer fire, so a released id
            // whose timer fires later stays in the store, as it already does after
            // `sleep`/`delete` of an armed timer (about 70 bytes each). probe:
            // design/review-2026-10-05/repro/x-node-contract-05.py (800k re-arms: 226
            // MB peak against 62 MB). (x-node-contract-05)
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
            // CR claude for claude: [bug] This subtraction underflows on N(0).
            // Timer::update's (Some(timeout), Some(repeat)) arm (line 343) schedules
            // without checking will_repeat(), so timer(d, 0) arms a timer. A count that
            // drops to 0 while a fire is pending (line 325) also keeps its armed timer,
            // and in both cases the decrement at line 357 arrives here with 0. A debug
            // build panics and kills the runtime ("graphix runtime is dead"); a release
            // build wraps to u64::MAX and fires every period forever, where the gxi
            // promises n fires. A negative count also fires forever instead of raising
            // the TimerError its message promises, because from_value's
            // cast_to::<u64>() wraps -1 to u64::MAX. probe:
            // design/review-2026-10-05/repro/x-panics-11.gx (x-panics-11)
            Repeat::N(n) => *n -= rhs,
        }
    }
}

impl Pack for Repeat {
    fn encoded_len(&self) -> usize {
        match self {
            Repeat::Yes | Repeat::No => 1,
            Repeat::N(n) => 1 + n.encoded_len(),
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            Repeat::Yes => 0u8.encode(buf),
            Repeat::No => 1u8.encode(buf),
            Repeat::N(n) => {
                2u8.encode(buf)?;
                n.encode(buf)
            }
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        match u8::decode(buf)? {
            0 => Ok(Repeat::Yes),
            1 => Ok(Repeat::No),
            2 => Ok(Repeat::N(u64::decode(buf)?)),
            _ => Err(PackError::UnknownTag),
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
    /// delivery (re)schedules.
    repeat_v: Option<Value>,
    timeout: Option<Duration>,
    repeat: Repeat,
    id: Option<BindId>,
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
        Ok(Box::new(Self {
            repeat_v: None,
            timeout: None,
            repeat: Repeat::No,
            id: None,
            eid: top_id,
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let repeat_v = Pack::decode(buf)?;
        let timeout = Pack::decode(buf)?;
        let repeat = Repeat::decode(buf)?;
        let eid = ExprId::decode(buf)?;
        Ok(Box::new(Self {
            repeat_v,
            timeout,
            repeat,
            id: None,
            eid,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Timer {
    /// `id` is an armed runtime timer.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.id.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.repeat_v.encode(buf)?;
        self.timeout.encode(buf)?;
        self.repeat.encode(buf)?;
        self.eid.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
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
        let new_timeout = match seam_value(from[0].update(ctx)) {
            Some(tv) if tv.is_fired() => Some(tv.value_cloned()),
            _ => None,
        };
        let mut repeat_up = false;
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            repeat_up = tv.is_fired();
            self.repeat_v = Some(tv.value_cloned());
        }
        match (new_timeout, &self.repeat_v, repeat_up) {
            (None, Some(r), true) => match r.clone().cast_to::<Repeat>() {
                Err(_) => error!(),
                Ok(repeat) => {
                    self.repeat = repeat;
                    if let Some(dur) = self.timeout {
                        // CR claude for claude: [bug] A one-shot timer whose repeat arg
                        // arrives after its timeout never fires. The `(Some(s), None,
                        // _)` arm only stores the timeout, and this arm arms the timer
                        // only when `will_repeat()`, which is false for Repeat::No. In
                        // `timer(100ms, r)` with `r` written 50 ms after init, `false`
                        // fires 0 times but `1` fires once, though the doc treats them
                        // the same. With `r` present at init, `false` fires once. This
                        // arm cannot tell a timeout that was never armed from a
                        // one-shot already spent, so that needs its own state. probe:
                        // design/review-2026-10-05/repro/sys-io-12.gx (sys-io-12)
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
            .and_then(|id| {
                ctx.event.variables.get(&id).map(|now| (id, now.value_cloned()))
            })
            .map(|(id, now)| {
                release(ctx, id, self.eid);
                self.id = None;
                self.repeat -= 1;
                if let Some(dur) = self.timeout {
                    if self.repeat.will_repeat() {
                        schedule!(dur)
                    }
                }
                now
            });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(id) = self.id.take() {
            release(ctx, id, self.eid);
        }
    }

    // CR claude for claude: [bug] Sleep drops the timer's timeout and repeat, and update
    // rebuilds them only from a fired timeout. At an arm's wake, a binding or parameter
    // argument arrives stale, so `timer(interval, true)` in a re-selected arm never
    // fires again, while `timer(duration:3.ms, true)` restarts because constants fire
    // at the wake. AfterIdle::sleep (line 144) and CachedArgsAsync::sleep
    // (graphix-package-core/src/lib.rs:936) have the same hole: they reset their output
    // on the premise that the operation restarts on wake, nothing restarts it over
    // level arguments, and the arm stays bottom for good (json::read(doc),
    // sys::fs::read_all(p), after_idle(d, v)). Subscribe and Publish handle this with a
    // slept bit that makes the first update after sleep act on the present arguments;
    // these three need the same. Both engines agree, so the fuzzer cannot see it.
    // Probe: design/review-2026-10-05/repro/x-engine-firing-04.gx. (x-engine-firing-04)
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.out = TagValue::phantom();
        self.repeat_v = None;
        self.timeout = None;
        self.repeat = Repeat::No;
        if let Some(id) = self.id.take() {
            release(ctx, id, self.eid);
        }
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
