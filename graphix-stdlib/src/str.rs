use crate::{deftype, CachedArgs, CachedVals, EvalCached};
use anyhow::{bail, Context, Result};
use arcstr::{literal, ArcStr};
use escaping::Escape;
use graphix_compiler::{
    err, errf, Apply, BuiltIn, BuiltInInitFn, Event, ExecCtx, Node, Rt, UserEvent,
};
use netidx::{path::Path, subscriber::Value};
use netidx_value::ValArray;
use smallvec::SmallVec;
use std::{cell::RefCell, sync::Arc};

#[derive(Debug, Default)]
struct StartsWithEv;

impl EvalCached for StartsWithEv {
    const NAME: &str = "starts_with";
    deftype!("fn(#pfx:string, string) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::String(pfx)), Some(Value::String(val))) => {
                if val.starts_with(&**pfx) {
                    Some(Value::Bool(true))
                } else {
                    Some(Value::Bool(false))
                }
            }
            _ => None,
        }
    }
}

type StartsWith = CachedArgs<StartsWithEv>;

#[derive(Debug, Default)]
struct EndsWithEv;

impl EvalCached for EndsWithEv {
    const NAME: &str = "ends_with";
    deftype!("fn(#sfx:string, string) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::String(sfx)), Some(Value::String(val))) => {
                if val.ends_with(&**sfx) {
                    Some(Value::Bool(true))
                } else {
                    Some(Value::Bool(false))
                }
            }
            _ => None,
        }
    }
}

type EndsWith = CachedArgs<EndsWithEv>;

#[derive(Debug, Default)]
struct ContainsEv;

impl EvalCached for ContainsEv {
    const NAME: &str = "contains";
    deftype!("fn(#part:string, string) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::String(chs)), Some(Value::String(val))) => {
                if val.contains(&**chs) {
                    Some(Value::Bool(true))
                } else {
                    Some(Value::Bool(false))
                }
            }
            _ => None,
        }
    }
}

type Contains = CachedArgs<ContainsEv>;

#[derive(Debug, Default)]
struct StripPrefixEv;

impl EvalCached for StripPrefixEv {
    const NAME: &str = "strip_prefix";
    deftype!("fn(#pfx:string, string) -> Option<string>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::String(pfx)), Some(Value::String(val))) => val
                .strip_prefix(&**pfx)
                .map(|s| Value::String(s.into()))
                .or(Some(Value::Null)),
            _ => None,
        }
    }
}

type StripPrefix = CachedArgs<StripPrefixEv>;

#[derive(Debug, Default)]
struct StripSuffixEv;

impl EvalCached for StripSuffixEv {
    const NAME: &str = "strip_suffix";
    deftype!("fn(#sfx:string, string) -> Option<string>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::String(sfx)), Some(Value::String(val))) => val
                .strip_suffix(&**sfx)
                .map(|s| Value::String(s.into()))
                .or(Some(Value::Null)),
            _ => None,
        }
    }
}

type StripSuffix = CachedArgs<StripSuffixEv>;

#[derive(Debug, Default)]
struct TrimEv;

impl EvalCached for TrimEv {
    const NAME: &str = "trim";
    deftype!("fn(string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(val)) => Some(Value::String(val.trim().into())),
            _ => None,
        }
    }
}

type Trim = CachedArgs<TrimEv>;

#[derive(Debug, Default)]
struct TrimStartEv;

impl EvalCached for TrimStartEv {
    const NAME: &str = "trim_start";
    deftype!("fn(string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(val)) => Some(Value::String(val.trim_start().into())),
            _ => None,
        }
    }
}

type TrimStart = CachedArgs<TrimStartEv>;

#[derive(Debug, Default)]
struct TrimEndEv;

impl EvalCached for TrimEndEv {
    const NAME: &str = "trim_end";
    deftype!("fn(string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(val)) => Some(Value::String(val.trim_end().into())),
            _ => None,
        }
    }
}

type TrimEnd = CachedArgs<TrimEndEv>;

#[derive(Debug, Default)]
struct ReplaceEv;

impl EvalCached for ReplaceEv {
    const NAME: &str = "replace";
    deftype!("fn(#pat:string, #rep:string, string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1], &from.0[2]) {
            (
                Some(Value::String(pat)),
                Some(Value::String(rep)),
                Some(Value::String(val)),
            ) => Some(Value::String(val.replace(&**pat, &**rep).into())),
            _ => None,
        }
    }
}

type Replace = CachedArgs<ReplaceEv>;

#[derive(Debug, Default)]
struct DirnameEv;

impl EvalCached for DirnameEv {
    const NAME: &str = "dirname";
    deftype!("fn(string) -> Option<string>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(path)) => match Path::dirname(path) {
                None if path != "/" => Some(Value::String(literal!("/"))),
                None => Some(Value::Null),
                Some(dn) => Some(Value::String(dn.into())),
            },
            _ => None,
        }
    }
}

type Dirname = CachedArgs<DirnameEv>;

#[derive(Debug, Default)]
struct BasenameEv;

impl EvalCached for BasenameEv {
    const NAME: &str = "basename";
    deftype!("fn(string) -> Option<string>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(path)) => match Path::basename(path) {
                None => Some(Value::Null),
                Some(dn) => Some(Value::String(dn.into())),
            },
            _ => None,
        }
    }
}

type Basename = CachedArgs<BasenameEv>;

#[derive(Debug, Default)]
struct StringJoinEv;

impl EvalCached for StringJoinEv {
    const NAME: &str = "string_join";
    deftype!("fn(#sep:string, @args: [string, Array<string>]) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }
        match &from.0[..] {
            [_] | [] => None,
            [None, ..] => None,
            [Some(sep), parts @ ..] => {
                // this is fairly common, so we check it before doing any real work
                for p in parts {
                    if p.is_none() {
                        return None;
                    }
                }
                let sep = match sep {
                    Value::String(c) => c.clone(),
                    sep => match sep.clone().cast_to::<ArcStr>().ok() {
                        Some(c) => c,
                        None => return None,
                    },
                };
                BUF.with_borrow_mut(|buf| {
                    macro_rules! push {
                        ($c:expr) => {
                            if buf.is_empty() {
                                buf.push_str($c.as_str());
                            } else {
                                buf.push_str(sep.as_str());
                                buf.push_str($c.as_str());
                            }
                        };
                    }
                    buf.clear();
                    for p in parts {
                        match p.as_ref().unwrap() {
                            Value::String(c) => push!(c),
                            Value::Array(a) => {
                                for v in a.iter() {
                                    if let Value::String(c) = v {
                                        push!(c)
                                    }
                                }
                            }
                            _ => return None,
                        }
                    }
                    Some(Value::String(buf.as_str().into()))
                })
            }
        }
    }
}

type StringJoin = CachedArgs<StringJoinEv>;

#[derive(Debug, Default)]
struct StringConcatEv;

impl EvalCached for StringConcatEv {
    const NAME: &str = "string_concat";
    deftype!("fn(@args: [string, Array<string>]) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }
        let parts = &from.0[..];
        // this is a fairly common case, so we check it before doing any real work
        for p in parts {
            if p.is_none() {
                return None;
            }
        }
        BUF.with_borrow_mut(|buf| {
            buf.clear();
            for p in parts {
                match p.as_ref().unwrap() {
                    Value::String(c) => buf.push_str(c.as_ref()),
                    Value::Array(a) => {
                        for v in a.iter() {
                            if let Value::String(c) = v {
                                buf.push_str(c.as_ref())
                            }
                        }
                    }
                    _ => return None,
                }
            }
            Some(Value::String(buf.as_str().into()))
        })
    }
}

type StringConcat = CachedArgs<StringConcatEv>;

fn build_escape(esc: Value) -> Result<Escape> {
    fn escape_non_printing(c: char) -> bool {
        c.is_control()
    }
    let [(_, to_escape), (_, escape_char), (_, tr)] =
        esc.cast_to::<[(ArcStr, Value); 3]>().context("parse escape")?;
    let escape_char = {
        let s = escape_char.cast_to::<ArcStr>().context("escape char")?;
        if s.len() != 1 {
            bail!("expected a single escape char")
        }
        s.chars().next().unwrap()
    };
    let to_escape = match to_escape {
        Value::String(s) => s.chars().collect::<SmallVec<[char; 32]>>(),
        _ => bail!("escape: expected a string"),
    };
    let tr =
        tr.cast_to::<SmallVec<[(ArcStr, ArcStr); 8]>>().context("escape: parsing tr")?;
    for (k, _) in &tr {
        if k.len() != 1 {
            bail!("escape: tr key {k} is invalid, expected 1 character");
        }
    }
    let tr = tr
        .into_iter()
        .map(|(k, v)| (k.chars().next().unwrap(), v))
        .collect::<SmallVec<[_; 8]>>();
    let tr = tr.iter().map(|(c, s)| (*c, s.as_str())).collect::<SmallVec<[_; 8]>>();
    Escape::new(escape_char, &to_escape, &tr, Some(escape_non_printing))
}

macro_rules! escape_fn {
    ($name:ident, $builtin_name:literal, $escape:ident) => {
        #[derive(Debug)]
        struct $name {
            escape: Option<Escape>,
            args: CachedVals,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const NAME: &str = $builtin_name;
            deftype!(
                "fn(?#esc:Escape, string) -> Result<string, `StringError(string)>"
            );

            fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
                Arc::new(|_, _, _, from, _| {
                    Ok(Box::new(Self { escape: None, args: CachedVals::new(from) }))
                })
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                static TAG: ArcStr = literal!("StringError");
                let mut up = [false; 2];
                self.args.update_diff(&mut up, ctx, from, event);
                if up[0] {
                    match &self.args.0[0] {
                        Some(esc) => match build_escape(esc.clone()) {
                            Err(e) => {
                                return Some(errf!(TAG, "escape: invalid argument {e:?}"))
                            }
                            Ok(esc) => self.escape = Some(esc),
                        },
                        _ => return None,
                    };
                }
                match (up, &self.escape, &self.args.0[1]) {
                    ([_, true], Some(esc), Some(Value::String(s))) => {
                        Some(Value::String(ArcStr::from(esc.$escape(&s))))
                    }
                    (_, _, _) => None,
                }
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
                self.escape = None;
                self.args.clear();
            }
        }
    };
}

escape_fn!(StringEscape, "string_escape", escape);
escape_fn!(StringUnescape, "string_unescape", unescape);

macro_rules! string_split {
    ($name:ident, $final_name:ident, $builtin:literal, $fn:ident) => {
        #[derive(Debug, Default)]
        struct $name;

        impl EvalCached for $name {
            const NAME: &str = $builtin;
            deftype!("fn(#pat:string, string) -> Array<string>");

            fn eval(&mut self, from: &CachedVals) -> Option<Value> {
                // this is a fairly common case, so we check it before doing any real work
                for p in &from.0[..] {
                    if p.is_none() {
                        return None;
                    }
                }
                let pat = match &from.0[0] {
                    Some(Value::String(s)) => s,
                    _ => return None,
                };
                match &from.0[1] {
                    Some(Value::String(s)) => Some(Value::Array(ValArray::from_iter(
                        s.$fn(&**pat).map(|s| Value::String(ArcStr::from(s))),
                    ))),
                    _ => None,
                }
            }
        }

        type $final_name = CachedArgs<$name>;
    };
}

string_split!(StringSplitEv, StringSplit, "string_split", split);
string_split!(StringRSplitEv, StringRSplit, "string_rsplit", rsplit);

macro_rules! string_splitn {
    ($name:ident, $final_name:ident, $builtin:literal, $fn:ident) => {
        #[derive(Debug, Default)]
        struct $name;

        impl EvalCached for $name {
            const NAME: &str = $builtin;
            deftype!("fn(#pat:string, #n:i64, string) -> Result<Array<string>, `StringSplitError(string)>");

            fn eval(&mut self, from: &CachedVals) -> Option<Value> {
                static TAG: ArcStr = literal!("StringSplitError");
                // this is a fairly common case, so we check it before doing any real work
                for p in &from.0[..] {
                    if p.is_none() {
                        return None;
                    }
                }
                let pat = match &from.0[0] {
                    Some(Value::String(s)) => s,
                    _ => return None,
                };
                let n = match &from.0[1] {
                    Some(Value::I64(n)) if *n > 0 => *n as usize,
                    Some(v) => return Some(errf!(TAG, "splitn: {v} must be a number > 0")),
                    None => return None,
                };
                match &from.0[2] {
                    Some(Value::String(s)) => Some(Value::Array(ValArray::from_iter(
                        s.$fn(n, &**pat).map(|s| Value::String(ArcStr::from(s))),
                    ))),
                    _ => None,
                }
            }
        }

        type $final_name = CachedArgs<$name>;
    };
}

string_splitn!(StringSplitNEv, StringSplitN, "string_splitn", splitn);
string_splitn!(StringRSplitNEv, StringRSplitN, "string_rsplitn", rsplitn);

#[derive(Debug, Default)]
struct StringSplitEscapedEv;

impl EvalCached for StringSplitEscapedEv {
    const NAME: &str = "string_split_escaped";
    deftype!("fn(#esc:string, #sep:string, string) -> Result<Array<string>, `SplitEscError(string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        static TAG: ArcStr = literal!("SplitEscError");
        // this is a fairly common case, so we check it before doing any real work
        for p in &from.0[..] {
            if p.is_none() {
                return None;
            }
        }
        let esc = match &from.0[0] {
            Some(Value::String(s)) if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Some(err!(TAG, "split_escaped: invalid escape char")),
        };
        let sep = match &from.0[1] {
            Some(Value::String(s)) if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Some(err!(TAG, "split_escaped: invalid separator")),
        };
        match &from.0[2] {
            Some(Value::String(s)) => Some(Value::Array(ValArray::from_iter(
                escaping::split(s, esc, sep).map(|s| Value::String(ArcStr::from(s))),
            ))),
            _ => None,
        }
    }
}

type StringSplitEscaped = CachedArgs<StringSplitEscapedEv>;

#[derive(Debug, Default)]
struct StringSplitNEscapedEv;

impl EvalCached for StringSplitNEscapedEv {
    const NAME: &str = "string_splitn_escaped";
    deftype!(
        "fn(#n:i64, #esc:string, #sep:string, string) -> Result<Array<string>, `SplitNEscError(string)>"
    );

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        static TAG: ArcStr = literal!("SplitNEscError");
        // this is a fairly common case, so we check it before doing any real work
        for p in &from.0[..] {
            if p.is_none() {
                return None;
            }
        }
        let n = match &from.0[0] {
            Some(Value::I64(n)) if *n > 0 => *n as usize,
            Some(v) => return Some(errf!(TAG, "splitn_escaped: invalid n {v}")),
            None => return None,
        };
        let esc = match &from.0[1] {
            Some(Value::String(s)) if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Some(err!(TAG, "split_escaped: invalid escape char")),
        };
        let sep = match &from.0[2] {
            Some(Value::String(s)) if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Some(err!(TAG, "split_escaped: invalid separator")),
        };
        match &from.0[3] {
            Some(Value::String(s)) => Some(Value::Array(ValArray::from_iter(
                escaping::splitn(s, esc, n, sep).map(|s| Value::String(ArcStr::from(s))),
            ))),
            _ => None,
        }
    }
}

type StringSplitNEscaped = CachedArgs<StringSplitNEscapedEv>;

#[derive(Debug, Default)]
struct StringSplitOnceEv;

impl EvalCached for StringSplitOnceEv {
    const NAME: &str = "string_split_once";
    deftype!("fn(#pat:string, string) -> Option<(string, string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        // this is a fairly common case, so we check it before doing any real work
        for p in &from.0[..] {
            if p.is_none() {
                return None;
            }
        }
        let pat = match &from.0[0] {
            Some(Value::String(s)) => s,
            _ => return None,
        };
        match &from.0[1] {
            Some(Value::String(s)) => match s.split_once(&**pat) {
                None => Some(Value::Null),
                Some((s0, s1)) => Some(Value::Array(ValArray::from([
                    Value::String(s0.into()),
                    Value::String(s1.into()),
                ]))),
            },
            _ => None,
        }
    }
}

type StringSplitOnce = CachedArgs<StringSplitOnceEv>;

#[derive(Debug, Default)]
struct StringRSplitOnceEv;

impl EvalCached for StringRSplitOnceEv {
    const NAME: &str = "string_rsplit_once";
    deftype!("fn(#pat:string, string) -> Option<(string, string)>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        // this is a fairly common case, so we check it before doing any real work
        for p in &from.0[..] {
            if p.is_none() {
                return None;
            }
        }
        let pat = match &from.0[0] {
            Some(Value::String(s)) => s,
            _ => return None,
        };
        match &from.0[1] {
            Some(Value::String(s)) => match s.rsplit_once(&**pat) {
                None => Some(Value::Null),
                Some((s0, s1)) => Some(Value::Array(ValArray::from([
                    Value::String(s0.into()),
                    Value::String(s1.into()),
                ]))),
            },
            _ => None,
        }
    }
}

type StringRSplitOnce = CachedArgs<StringRSplitOnceEv>;

#[derive(Debug, Default)]
struct StringToLowerEv;

impl EvalCached for StringToLowerEv {
    const NAME: &str = "string_to_lower";
    deftype!("fn(string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(s)) => Some(Value::String(s.to_lowercase().into())),
            _ => None,
        }
    }
}

type StringToLower = CachedArgs<StringToLowerEv>;

#[derive(Debug, Default)]
struct StringToUpperEv;

impl EvalCached for StringToUpperEv {
    const NAME: &str = "string_to_upper";
    deftype!("fn(string) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(s)) => Some(Value::String(s.to_uppercase().into())),
            _ => None,
        }
    }
}

type StringToUpper = CachedArgs<StringToUpperEv>;

#[derive(Debug, Default)]
struct SprintfEv {
    buf: String,
    args: Vec<Value>,
}

impl EvalCached for SprintfEv {
    const NAME: &str = "string_sprintf";
    deftype!("fn(string, @args: Any) -> string");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::String(fmt)), args @ ..] => {
                self.buf.clear();
                self.args.clear();
                for v in args {
                    match v {
                        Some(v) => self.args.push(v.clone()),
                        None => return None,
                    }
                }
                match netidx_value::printf(&mut self.buf, fmt, &self.args) {
                    Ok(_) => Some(Value::String(ArcStr::from(&self.buf))),
                    Err(e) => Some(Value::error(ArcStr::from(e.to_string()))),
                }
            }
            _ => None,
        }
    }
}

type Sprintf = CachedArgs<SprintfEv>;

#[derive(Debug, Default)]
struct LenEv;

impl EvalCached for LenEv {
    const NAME: &str = "string_len";
    deftype!("fn(string) -> i64");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(s)) => Some(Value::I64(s.len() as i64)),
            _ => None,
        }
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct SubEv(String);

impl EvalCached for SubEv {
    const NAME: &str = "string_sub";
    deftype!(
        "fn(#start:i64, #len:i64, string) -> Result<string, `SubError(string)>"
    );

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[..] {
            [Some(Value::I64(start)), Some(Value::I64(len)), Some(Value::String(s))]
                if *start >= 0 && *len >= 0 =>
            {
                let start = *start as usize;
                let end = start + *len as usize;
                self.0.clear();
                for (i, c) in s.chars().enumerate() {
                    if i >= start && i < end {
                        self.0.push(c);
                    }
                }
                Some(Value::String(ArcStr::from(&self.0)))
            }
            v => Some(errf!(literal!("SubError"), "sub args must be non negative {v:?}")),
        }
    }
}

type Sub = CachedArgs<SubEv>;

#[derive(Debug, Default)]
struct ParseEv;

impl EvalCached for ParseEv {
    const NAME: &str = "string_parse";
    deftype!("fn(string) -> Result<PrimNoErr, Any>");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(s)) => match s.parse::<Value>() {
                Ok(v) => match v {
                    Value::Error(e) => Some(errf!(literal!("ParseError"), "{e}")),
                    v => Some(v),
                },
                Err(e) => Some(errf!(literal!("ParseError"), "{e:?}")),
            },
            _ => None,
        }
    }
}

type Parse = CachedArgs<ParseEv>;

pub(super) fn register<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
) -> Result<(ArcStr, ArcStr)> {
    ctx.register_builtin::<StartsWith>()?;
    ctx.register_builtin::<EndsWith>()?;
    ctx.register_builtin::<Contains>()?;
    ctx.register_builtin::<StripPrefix>()?;
    ctx.register_builtin::<StripSuffix>()?;
    ctx.register_builtin::<Trim>()?;
    ctx.register_builtin::<TrimStart>()?;
    ctx.register_builtin::<TrimEnd>()?;
    ctx.register_builtin::<Replace>()?;
    ctx.register_builtin::<Dirname>()?;
    ctx.register_builtin::<Basename>()?;
    ctx.register_builtin::<StringJoin>()?;
    ctx.register_builtin::<StringConcat>()?;
    ctx.register_builtin::<StringEscape>()?;
    ctx.register_builtin::<StringUnescape>()?;
    ctx.register_builtin::<StringSplit>()?;
    ctx.register_builtin::<StringRSplit>()?;
    ctx.register_builtin::<StringSplitN>()?;
    ctx.register_builtin::<StringRSplitN>()?;
    ctx.register_builtin::<StringSplitOnce>()?;
    ctx.register_builtin::<StringRSplitOnce>()?;
    ctx.register_builtin::<StringSplitEscaped>()?;
    ctx.register_builtin::<StringSplitNEscaped>()?;
    ctx.register_builtin::<StringToLower>()?;
    ctx.register_builtin::<StringToUpper>()?;
    ctx.register_builtin::<Sprintf>()?;
    ctx.register_builtin::<Len>()?;
    ctx.register_builtin::<Sub>()?;
    ctx.register_builtin::<Parse>()?;
    Ok((literal!(include_str!("str.gx")), literal!(include_str!("str.gxi"))))
}
