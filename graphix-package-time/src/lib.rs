use anyhow::{bail, Result};
use arcstr::literal;
use chrono::Utc;
use graphix_compiler::{
    err, expr::ExprId, Apply, BindId, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
};
use graphix_package_core::{arity2, deftype, CachedVals};
use netidx::{publisher::FromValue, subscriber::Value};
use std::{ops::SubAssign, time::Duration};

#[derive(Debug)]
struct AfterIdle {
    args: CachedVals,
    id: Option<BindId>,
    eid: ExprId,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for AfterIdle {
    const NAME: &str = "time_after_idle";
    deftype!("fn([duration, Number], 'a) -> 'a");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(AfterIdle { args: CachedVals::new(from), id: None, eid: top_id }))
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
            ((Some(secs), _), (true, _)) | ((Some(secs), _), (_, true)) => {
                match secs.clone().cast_to::<Duration>() {
                    Ok(dur) => {
                        let id = BindId::new();
                        self.id = Some(id);
                        ctx.rt.ref_var(id, self.eid);
                        ctx.rt.set_timer(id, dur);
                        return None;
                    }
                    Err(_) => {
                        self.id = None;
                        return None;
                    }
                }
            }
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
    const NAME: &str = "time_timer";
    deftype!(
        "fn([duration, Number], [bool, Number]) -> Result<datetime, `TimerError(string)>"
    );

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self {
            args: CachedVals::new(from),
            timeout: None,
            repeat: Repeat::No,
            id: None,
            eid: top_id,
        }))
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
                return Some(err!(
                    literal!("TimerError"),
                    "timer(per, rep): expected duration, bool or number >= 0"
                ));
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

#[derive(Debug)]
struct Now;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Now {
    const NAME: &str = "time_now";
    deftype!("fn(Any) -> datetime");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Now {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            Some(Value::from(Utc::now()))
        } else {
            None
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}
    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

graphix_derive::defpackage! {
    builtins => [
        AfterIdle,
        Timer,
        Now,
    ],
}
