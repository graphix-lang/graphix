#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, effects::Effect,
    expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_package_core::{CachedVals, Invocation, seam_tick};
use netidx::subscriber::Value;
use netidx_core::pack::PackError;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use rand::{
    RngExt,
    distr::uniform::{Error as RangeError, SampleRange, SampleUniform},
    rng,
    seq::SliceRandom,
};
use std::{
    fmt::Display,
    ops::{Div, Mul},
};

/// A uniform sample of `[start, end)`.
fn sample<T: SampleUniform + PartialOrd + Copy>(
    start: T,
    end: T,
) -> Result<T, RangeError> {
    let range = start..end;
    if range.is_empty() {
        return Err(RangeError::EmptyRange);
    }
    // a float range's sample may round up to its end
    loop {
        let v = range.clone().sample_single(&mut rng())?;
        if v < end {
            break Ok(v);
        }
    }
}

/// [`sample`] over floats, where a range of finite bounds too wide for its
/// type is sampled at half scale: halving and doubling are exact.
fn sample_float<F>(start: F, end: F) -> Result<F, RangeError>
where
    F: SampleUniform + PartialOrd + Copy + Mul<Output = F> + Div<Output = F> + From<f32>,
{
    let two = F::from(2.);
    sample(start, end).or_else(|e| match e {
        RangeError::NonFinite => sample(start / two, end / two).map(|v| v * two),
        e => Err(e),
    })
}

/// `sampled` as a value, or nothing, logged.
fn logged<T: Display>(
    start: T,
    end: T,
    sampled: Result<T, RangeError>,
    value: fn(T) -> Value,
) -> Option<Value> {
    match sampled {
        Ok(v) => Some(value(v)),
        Err(_) => {
            log::error!(
                target: graphix_compiler::FAILURE_TARGET,
                "rand: no value in [{start}, {end})"
            );
            None
        }
    }
}

#[derive(Debug)]
struct Rand {
    args: CachedVals,
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Rand {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "rand";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Rand { args: CachedVals::new(from), out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let args = CachedVals::image_decode(buf)?;
        Ok(Box::new(Rand { args, out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for Rand {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.args.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        macro_rules! gen_cases {
            ($start:expr, $end:expr; $($ftyp:ident),+; $($typ:ident),+) => {
                match ($start, $end) {
                    $(
                        (Value::$ftyp(start), Value::$ftyp(end)) => {
                            logged(*start, *end, sample_float(*start, *end), Value::$ftyp)
                        }
                    )+
                    $(
                        (Value::$typ(start), Value::$typ(end)) => {
                            logged(*start, *end, sample(*start, *end), Value::$typ)
                        }
                    )+
                    _ => None
                }
            };
        }
        let res = match self.args.update(ctx, from) {
            Invocation::Bottom { fresh } => return self.out.set_bottom(fresh),
            Invocation::Quiet => None,
            Invocation::Fired => {
                match (self.args.value(0), self.args.value(1), self.args.value(2)) {
                    (Some(start), Some(end), Some(_)) => gen_cases!(
                        start, end; F32, F64; I8, I16, I32, I64, Z32, Z64, U8, U16, U32,
                        U64, V32, V64
                    ),
                    _ => None,
                }
            }
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.args.clear()
    }
}

#[derive(Debug)]
struct Pick {
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Pick {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "rand_pick";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Pick { out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Pick { out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for Pick {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        // a bottom input is a bottom output
        let tv = from[0].update(ctx);
        if tv.tag().is_bottom() {
            return self.out.set_bottom(tv.tag().triggers());
        }
        let res = seam_tick(tv).and_then(|a| match a.value_cloned() {
            Value::Array(a) if a.len() > 0 => {
                Some(a[rng().random_range(0..a.len())].clone())
            }
            _ => None,
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

#[derive(Debug)]
struct Shuffle {
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Shuffle {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "rand_shuffle";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Shuffle { out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Shuffle { out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for Shuffle {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        // a bottom input is a bottom output
        let tv = from[0].update(ctx);
        if tv.tag().is_bottom() {
            return self.out.set_bottom(tv.tag().triggers());
        }
        let res = seam_tick(tv).and_then(|a| match a.value_cloned() {
            Value::Array(a) => {
                let mut elems: LPooled<Vec<Value>> = a.iter().cloned().collect();
                elems.shuffle(&mut rng());
                Some(Value::Array(ValArray::from_iter_exact(elems.drain(..))))
            }
            _ => None,
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

#[cfg(test)]
mod test;

graphix_derive::defpackage! {
    builtins => [
        Rand,
        Pick,
        Shuffle,
    ],
}
