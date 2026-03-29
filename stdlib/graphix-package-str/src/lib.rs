#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Context, Result};
use arcstr::{literal, ArcStr};
use escaping::Escape;
use graphix_compiler::{
    err, errf,
    expr::ExprId,
    typ::{FnType, Type},
    Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, TypecheckPhase, UserEvent,
};
use graphix_package_core::{extract_cast_type, CachedArgs, CachedVals, EvalCached};
use netidx::{path::Path, subscriber::Value};
use netidx_value::ValArray;
use smallvec::SmallVec;
use std::cell::RefCell;

#[derive(Debug, Default)]
struct StartsWithEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StartsWithEv {
    const NAME: &str = "str_starts_with";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EndsWithEv {
    const NAME: &str = "str_ends_with";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ContainsEv {
    const NAME: &str = "str_contains";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StripPrefixEv {
    const NAME: &str = "str_strip_prefix";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StripSuffixEv {
    const NAME: &str = "str_strip_suffix";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TrimEv {
    const NAME: &str = "str_trim";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(val)) => Some(Value::String(val.trim().into())),
            _ => None,
        }
    }
}

type Trim = CachedArgs<TrimEv>;

#[derive(Debug, Default)]
struct TrimStartEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TrimStartEv {
    const NAME: &str = "str_trim_start";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(val)) => Some(Value::String(val.trim_start().into())),
            _ => None,
        }
    }
}

type TrimStart = CachedArgs<TrimStartEv>;

#[derive(Debug, Default)]
struct TrimEndEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TrimEndEv {
    const NAME: &str = "str_trim_end";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(val)) => Some(Value::String(val.trim_end().into())),
            _ => None,
        }
    }
}

type TrimEnd = CachedArgs<TrimEndEv>;

#[derive(Debug, Default)]
struct ReplaceEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ReplaceEv {
    const NAME: &str = "str_replace";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DirnameEv {
    const NAME: &str = "str_dirname";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BasenameEv {
    const NAME: &str = "str_basename";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringJoinEv {
    const NAME: &str = "str_join";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }
        match &from.0[..] {
            [_] | [] => None,
            [None, ..] => None,
            [Some(sep), parts @ ..] => {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringConcatEv {
    const NAME: &str = "str_concat";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }
        let parts = &from.0[..];
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
            const NEEDS_CALLSITE: bool = false;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new(Self { escape: None, args: CachedVals::new(from) }))
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

escape_fn!(StringEscape, "str_escape", escape);
escape_fn!(StringUnescape, "str_unescape", unescape);

macro_rules! string_split {
    ($name:ident, $final_name:ident, $builtin:literal, $fn:ident) => {
        #[derive(Debug, Default)]
        struct $name;

        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $name {
            const NAME: &str = $builtin;
            const NEEDS_CALLSITE: bool = false;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
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

string_split!(StringSplitEv, StringSplit, "str_split", split);
string_split!(StringRSplitEv, StringRSplit, "str_rsplit", rsplit);

macro_rules! string_splitn {
    ($name:ident, $final_name:ident, $builtin:literal, $fn:ident) => {
        #[derive(Debug, Default)]
        struct $name;

        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $name {
            const NAME: &str = $builtin;
            const NEEDS_CALLSITE: bool = false;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                static TAG: ArcStr = literal!("StringSplitError");
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
                    Some(v) => {
                        return Some(errf!(TAG, "splitn: {v} must be a number > 0"))
                    }
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

string_splitn!(StringSplitNEv, StringSplitN, "str_splitn", splitn);
string_splitn!(StringRSplitNEv, StringRSplitN, "str_rsplitn", rsplitn);

#[derive(Debug, Default)]
struct StringSplitEscapedEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringSplitEscapedEv {
    const NAME: &str = "str_split_escaped";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        static TAG: ArcStr = literal!("SplitEscError");
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringSplitNEscapedEv {
    const NAME: &str = "str_splitn_escaped";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        static TAG: ArcStr = literal!("SplitNEscError");
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringSplitOnceEv {
    const NAME: &str = "str_split_once";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringRSplitOnceEv {
    const NAME: &str = "str_rsplit_once";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringToLowerEv {
    const NAME: &str = "str_to_lower";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(s)) => Some(Value::String(s.to_lowercase().into())),
            _ => None,
        }
    }
}

type StringToLower = CachedArgs<StringToLowerEv>;

#[derive(Debug, Default)]
struct StringToUpperEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringToUpperEv {
    const NAME: &str = "str_to_upper";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SprintfEv {
    const NAME: &str = "str_sprintf";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const NAME: &str = "str_len";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::String(s)) => Some(Value::I64(s.len() as i64)),
            _ => None,
        }
    }
}

type Len = CachedArgs<LenEv>;

#[derive(Debug, Default)]
struct SubEv(String);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SubEv {
    const NAME: &str = "str_sub";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
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
struct ParseEv {
    cast_typ: Option<Type>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ParseEv {
    const NAME: &str = "str_parse";
    const NEEDS_CALLSITE: bool = true;

    fn init(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self { cast_typ: extract_cast_type(resolved) }
    }

    fn typecheck(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_cast_type(Some(resolved));
                if self.cast_typ.is_none() {
                    bail!("str::parse requires a concrete return type")
                }
                Ok(())
            }
        }
    }

    fn eval(&mut self, ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let raw = match &from.0[0] {
            Some(Value::String(s)) => match s.parse::<Value>() {
                Ok(v) => match v {
                    Value::Error(e) => return Some(errf!(literal!("ParseError"), "{e}")),
                    v => v,
                },
                Err(e) => return Some(errf!(literal!("ParseError"), "{e:?}")),
            },
            _ => return None,
        };
        Some(match &self.cast_typ {
            Some(typ) => typ.cast_value(&ctx.env, raw),
            None => errf!("TypeError", "parse requires a concrete type annotation"),
        })
    }
}

type Parse = CachedArgs<ParseEv>;

graphix_derive::defpackage! {
    builtins => [
        StartsWith,
        EndsWith,
        Contains,
        StripPrefix,
        StripSuffix,
        Trim,
        TrimStart,
        TrimEnd,
        Replace,
        Dirname,
        Basename,
        StringJoin,
        StringConcat,
        StringEscape,
        StringUnescape,
        StringSplit,
        StringRSplit,
        StringSplitN,
        StringRSplitN,
        StringSplitOnce,
        StringRSplitOnce,
        StringSplitEscaped,
        StringSplitNEscaped,
        StringToLower,
        StringToUpper,
        Sprintf,
        Len,
        Sub,
        Parse,
    ],
}
