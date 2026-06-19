#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use arcstr::{literal, ArcStr};
use graphix_compiler::{effects::EffectKind, errf, ExecCtx, Rt, UserEvent};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};
use netidx::subscriber::Value;
use netidx_value::ValArray;
use regex::Regex;

fn maybe_compile(s: &str, re: &mut Option<Regex>) -> Result<()> {
    let compile = match re {
        None => true,
        Some(re) => re.as_str() != s,
    };
    if compile {
        *re = Some(Regex::new(s)?)
    }
    Ok(())
}

static TAG: ArcStr = literal!("ReError");

#[derive(Debug, Default)]
struct IsMatchEv {
    re: Option<Regex>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsMatchEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "re_is_match";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::String(s)) = &from.0[0] {
            if let Err(e) = maybe_compile(s, &mut self.re) {
                return Some(errf!(TAG, "{e:?}"));
            }
        }
        if let Some(Value::String(s)) = &from.0[1] {
            if let Some(re) = self.re.as_ref() {
                return Some(Value::Bool(re.is_match(s)));
            }
        }
        None
    }
}

type IsMatch = CachedArgs<IsMatchEv>;

#[derive(Debug, Default)]
struct FindEv {
    re: Option<Regex>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for FindEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "re_find";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::String(s)) = &from.0[0] {
            if let Err(e) = maybe_compile(s, &mut self.re) {
                return Some(errf!(TAG, "{e:?}"));
            }
        }
        if let Some(Value::String(s)) = &from.0[1] {
            if let Some(re) = self.re.as_ref() {
                let a = ValArray::from_iter(
                    re.find_iter(s).map(|s| Value::String(s.as_str().into())),
                );
                return Some(Value::Array(a));
            }
        }
        None
    }
}

type Find = CachedArgs<FindEv>;

#[derive(Debug, Default)]
struct CapturesEv {
    re: Option<Regex>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for CapturesEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "re_captures";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::String(s)) = &from.0[0] {
            if let Err(e) = maybe_compile(s, &mut self.re) {
                return Some(errf!(TAG, "{e:?}"));
            }
        }
        if let Some(Value::String(s)) = &from.0[1] {
            if let Some(re) = self.re.as_ref() {
                let a = ValArray::from_iter(re.captures_iter(s).map(|c| {
                    let a = ValArray::from_iter(c.iter().map(|m| match m {
                        None => Value::Null,
                        Some(m) => Value::String(m.as_str().into()),
                    }));
                    Value::Array(a)
                }));
                return Some(Value::Array(a));
            }
        }
        None
    }
}

type Captures = CachedArgs<CapturesEv>;

#[derive(Debug, Default)]
struct SplitEv {
    re: Option<Regex>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SplitEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "re_split";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::String(s)) = &from.0[0] {
            if let Err(e) = maybe_compile(s, &mut self.re) {
                return Some(errf!(TAG, "{e:?}"));
            }
        }
        if let Some(Value::String(s)) = &from.0[1] {
            if let Some(re) = self.re.as_ref() {
                let a = ValArray::from_iter(re.split(s).map(|s| Value::String(s.into())));
                return Some(Value::Array(a));
            }
        }
        None
    }
}

type Split = CachedArgs<SplitEv>;

#[derive(Debug, Default)]
struct SplitNEv {
    re: Option<Regex>,
    lim: Option<usize>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SplitNEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "re_splitn";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        if let Some(Value::String(s)) = &from.0[0] {
            if let Err(e) = maybe_compile(s, &mut self.re) {
                return Some(errf!(TAG, "{e:?}"));
            }
        }
        if let Some(Value::I64(lim)) = &from.0[1] {
            self.lim = Some(*lim as usize);
        }
        if let Some(Value::String(s)) = &from.0[2] {
            if let Some(lim) = self.lim {
                if let Some(re) = self.re.as_ref() {
                    let a = ValArray::from_iter(
                        re.splitn(s, lim).map(|s| Value::String(s.into())),
                    );
                    return Some(Value::Array(a));
                }
            }
        }
        None
    }
}

type SplitN = CachedArgs<SplitNEv>;

#[cfg(test)]
mod test;

graphix_derive::defpackage! {
    builtins => [
        IsMatch,
        Find,
        Captures,
        Split,
        SplitN,
    ],
}
