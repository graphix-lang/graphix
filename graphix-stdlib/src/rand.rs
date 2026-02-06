use crate::{deftype, CachedVals};
use anyhow::Result;
use arcstr::{literal, ArcStr};
use graphix_compiler::{
    Apply, BuiltIn, BuiltInInitFn, Event, ExecCtx, Node, Rt, UserEvent,
};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use rand::{rng, seq::SliceRandom, Rng};
use smallvec::{smallvec, SmallVec};
use std::sync::Arc;

#[derive(Debug)]
struct Rand {
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Rand {
    const NAME: &str = "rand";
    deftype!("fn<'a: [Int, Float]>(?#start:'a, ?#end:'a, #clock:Any) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, from, _| Ok(Box::new(Rand { args: CachedVals::new(from) })))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Rand {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! gen_cases {
            ($start:expr, $end:expr, $($typ:ident),+) => {
                match ($start, $end) {
                    $(
                        (Value::$typ(start), Value::$typ(end)) if start < end => {
                            Some(Value::$typ(rng().random_range(*start..*end)))
                        }
                    ),+
                    _ => None
                }
            };
        }
        let up = self.args.update(ctx, from, event);
        if up {
            match &self.args.0[..] {
                [Some(start), Some(end), Some(_)] => gen_cases!(
                    start, end, F32, F64, I32, I64, Z32, Z64, U32, U64, V32, V64
                ),
                _ => None,
            }
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.args.clear()
    }
}

#[derive(Debug)]
struct Pick;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Pick {
    const NAME: &str = "rand_pick";
    deftype!("fn(Array<'a>) -> 'a");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(Pick)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Pick {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|a| match a {
            Value::Array(a) if a.len() > 0 => {
                Some(a[rng().random_range(0..a.len())].clone())
            }
            _ => None,
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Shuffle(SmallVec<[Value; 32]>);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Shuffle {
    const NAME: &str = "rand_shuffle";
    deftype!("fn(Array<'a>) -> Array<'a>");

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(Shuffle(smallvec![]))))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Shuffle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|a| match a {
            Value::Array(a) => {
                self.0.extend(a.iter().cloned());
                self.0.shuffle(&mut rng());
                Some(Value::Array(ValArray::from_iter_exact(self.0.drain(..))))
            }
            _ => None,
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.0.clear()
    }
}

pub(super) fn register<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
) -> Result<(ArcStr, ArcStr)> {
    ctx.register_builtin::<Rand>()?;
    ctx.register_builtin::<Pick>()?;
    ctx.register_builtin::<Shuffle>()?;
    Ok((literal!(include_str!("rand.gx")), literal!(include_str!("rand.gxi"))))
}
