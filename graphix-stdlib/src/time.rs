use crate::{arity2, deftype, CachedVals};
use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use graphix_compiler::{
    err, errf, expr::ExprId, Apply, BindId, BuiltIn, BuiltInInitFn, Event, ExecCtx, Node,
    Rt, UserEvent,
};
use netidx::{publisher::FromValue, subscriber::Value};
use std::{ops::SubAssign, sync::Arc, time::Duration};

#[derive(Debug)]
struct AfterIdle {
    args: CachedVals,
    id: Option<BindId>,
    eid: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for AfterIdle {
    const NAME: &str = "after_idle";
    deftype!("time", "fn([duration, Number], 'a) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, from, eid| {
            Ok(Box::new(AfterIdle { args: CachedVals::new(from), id: None, eid }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for AfterIdle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        let ((timeout, val), (timeout_up, val_up)) = arity2!(self.args.0, &up);
        match ((timeout, val), (timeout_up, val_up)) {
            ((Some(secs), _), (true, _)) | ((Some(secs), _), (_, true)) => match secs
                .clone()
                .cast_to::<Duration>()
            {
                Err(e) => {
                    self.id = None;
                    return errf!("after_idle(timeout, cur): expected duration {e:?}");
                }
                Ok(dur) => {
                    let id = BindId::new();
                    self.id = Some(id);
                    ctx.rt.ref_var(id, self.eid);
                    ctx.rt.set_timer(id, dur);
                    return None;
                }
            },
            ((None, _), (_, _))
            | ((_, None), (_, _))
            | ((Some(_), Some(_)), (false, _)) => (),
        };
        self.id.and_then(|id| {
            if event.variables.contains_key(&id) {
                self.id = None;
                ctx.rt.unref_var(id, self.eid);
                self.args.0.get(1).and_then(|v| v.clone())
            } else {
                None
            }
        })
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
        self.args.clear()
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
struct Timer {
    args: CachedVals,
    timeout: Option<Duration>,
    repeat: Repeat,
    id: Option<BindId>,
    eid: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Timer {
    const NAME: &str = "timer";
    deftype!("time", "fn([duration, Number], [bool, Number]) -> datetime");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, from, eid| {
            Ok(Box::new(Self {
                args: CachedVals::new(from),
                timeout: None,
                repeat: Repeat::No,
                id: None,
                eid,
            }))
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Timer {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! error {
            () => {{
                self.id = None;
                self.timeout = None;
                self.repeat = Repeat::No;
                return err!("timer(per, rep): expected duration, bool or number >= 0");
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
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        let ((timeout, repeat), (timeout_up, repeat_up)) = arity2!(self.args.0, &up);
        match ((timeout, repeat), (timeout_up, repeat_up)) {
            ((None, Some(r)), (true, true)) | ((_, Some(r)), (false, true)) => {
                match r.clone().cast_to::<Repeat>() {
                    Err(_) => error!(),
                    Ok(repeat) => {
                        self.repeat = repeat;
                        if let Some(dur) = self.timeout {
                            if self.id.is_none() && repeat.will_repeat() {
                                schedule!(dur)
                            }
                        }
                    }
                }
            }
            ((Some(s), None), (true, _)) => match s.clone().cast_to::<Duration>() {
                Err(_) => error!(),
                Ok(dur) => self.timeout = Some(dur),
            },
            ((Some(s), Some(r)), (true, _)) => {
                match (s.clone().cast_to::<Duration>(), r.clone().cast_to::<Repeat>()) {
                    (Err(_), _) | (_, Err(_)) => error!(),
                    (Ok(dur), Ok(repeat)) => {
                        self.timeout = Some(dur);
                        self.repeat = repeat;
                        schedule!(dur)
                    }
                }
            }
            ((_, _), (false, false))
            | ((None, None), (_, _))
            | ((None, _), (true, false))
            | ((_, None), (false, true)) => (),
        }
        self.id.and_then(|id| event.variables.get(&id).map(|now| (id, now))).map(
            |(id, now)| {
                ctx.rt.unref_var(id, self.eid);
                self.id = None;
                self.repeat -= 1;
                if let Some(dur) = self.timeout {
                    if self.repeat.will_repeat() {
                        schedule!(dur)
                    }
                }
                now.clone()
            },
        )
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.eid);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.args.clear();
        self.timeout = None;
        self.repeat = Repeat::No;
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.eid);
        }
    }
}

pub(super) fn register<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<R, E>) -> Result<ArcStr> {
    ctx.register_builtin::<AfterIdle>()?;
    ctx.register_builtin::<Timer>()?;
    Ok(literal!(include_str!("time.gx")))
}
