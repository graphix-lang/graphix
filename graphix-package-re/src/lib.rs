use anyhow::Result;
use arcstr::{literal, ArcStr};
use fxhash::FxHashMap;
use graphix_compiler::{errf, ExecCtx, Rt, UserEvent};
use graphix_package::{CustomDisplay, Package};
use graphix_package_core::{deftype, CachedArgs, CachedVals, EvalCached};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
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

impl EvalCached for IsMatchEv {
    const NAME: &str = "re_is_match";
    deftype!("fn(#pat:string, string) -> Result<bool, `ReError(string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
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

impl EvalCached for FindEv {
    const NAME: &str = "re_find";
    deftype!("fn(#pat:string, string) -> Result<Array<string>, `ReError(string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
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

impl EvalCached for CapturesEv {
    const NAME: &str = "re_captures";
    deftype!("fn(#pat:string, string) -> Result<Array<Array<Option<string>>>, `ReError(string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
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

impl EvalCached for SplitEv {
    const NAME: &str = "re_split";
    deftype!("fn(#pat:string, string) -> Result<Array<string>, `ReError(string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
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

impl EvalCached for SplitNEv {
    const NAME: &str = "re_splitn";
    deftype!(
        "fn(#pat:string, #limit:i64, string) -> Result<Array<string>, `ReError(string)>"
    );

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
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

graphix_derive::defpackage! {
    builtins => [
        IsMatch,
        Find,
        Captures,
        Split,
        SplitN,
    ],
}
