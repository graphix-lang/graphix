#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use escaping::Escape;
use graphix_compiler::{
    Apply, BuiltIn, Event, ExecCtx, FastFn, Node, Rt, Scope, TagValue, UserEvent,
    effects::EffectKind,
    err, errf,
    expr::ExprId,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, extract_cast_type, fast_eval, seam_value,
};
use netidx::{path::Path, subscriber::Value};
use netidx_value::ValArray;
use smallvec::SmallVec;
use std::cell::RefCell;

fn fc_starts_with(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(pfx), Value::String(val)) => {
            Some(Value::Bool(val.starts_with(&**pfx)))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StartsWithEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StartsWithEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_starts_with";
    const FASTCALL: Option<FastFn> = Some(fc_starts_with);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_starts_with, from)
    }
}

type StartsWith = CachedArgs<StartsWithEv>;

fn fc_ends_with(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(sfx), Value::String(val)) => {
            Some(Value::Bool(val.ends_with(&**sfx)))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct EndsWithEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EndsWithEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_ends_with";
    const FASTCALL: Option<FastFn> = Some(fc_ends_with);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_ends_with, from)
    }
}

type EndsWith = CachedArgs<EndsWithEv>;

fn fc_contains(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(chs), Value::String(val)) => {
            Some(Value::Bool(val.contains(&**chs)))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct ContainsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ContainsEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_contains";
    const FASTCALL: Option<FastFn> = Some(fc_contains);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_contains, from)
    }
}

type Contains = CachedArgs<ContainsEv>;

fn fc_strip_prefix(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(pfx), Value::String(val)) => val
            .strip_prefix(&**pfx)
            .map(|s| Value::String(s.into()))
            .or(Some(Value::Null)),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StripPrefixEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StripPrefixEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_strip_prefix";
    const FASTCALL: Option<FastFn> = Some(fc_strip_prefix);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_strip_prefix, from)
    }
}

type StripPrefix = CachedArgs<StripPrefixEv>;

fn fc_strip_suffix(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(sfx), Value::String(val)) => val
            .strip_suffix(&**sfx)
            .map(|s| Value::String(s.into()))
            .or(Some(Value::Null)),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StripSuffixEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StripSuffixEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_strip_suffix";
    const FASTCALL: Option<FastFn> = Some(fc_strip_suffix);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_strip_suffix, from)
    }
}

type StripSuffix = CachedArgs<StripSuffixEv>;

fn fc_trim(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(val) => Some(Value::String(val.trim().into())),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct TrimEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TrimEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_trim";
    const FASTCALL: Option<FastFn> = Some(fc_trim);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_trim, from)
    }
}

type Trim = CachedArgs<TrimEv>;

fn fc_trim_start(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(val) => Some(Value::String(val.trim_start().into())),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct TrimStartEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TrimStartEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_trim_start";
    const FASTCALL: Option<FastFn> = Some(fc_trim_start);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_trim_start, from)
    }
}

type TrimStart = CachedArgs<TrimStartEv>;

fn fc_trim_end(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(val) => Some(Value::String(val.trim_end().into())),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct TrimEndEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for TrimEndEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_trim_end";
    const FASTCALL: Option<FastFn> = Some(fc_trim_end);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_trim_end, from)
    }
}

type TrimEnd = CachedArgs<TrimEndEv>;

fn fc_replace(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::String(pat), Value::String(rep), Value::String(val)) => {
            Some(Value::String(val.replace(&**pat, &**rep).into()))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct ReplaceEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ReplaceEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_replace";
    const FASTCALL: Option<FastFn> = Some(fc_replace);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_replace, from)
    }
}

type Replace = CachedArgs<ReplaceEv>;

fn fc_dirname(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(path) => match Path::dirname(path) {
            None if path != "/" => Some(Value::String(literal!("/"))),
            None => Some(Value::Null),
            Some(dn) => Some(Value::String(dn.into())),
        },
        _ => None,
    }
}

#[derive(Debug, Default)]
struct DirnameEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DirnameEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_dirname";
    const FASTCALL: Option<FastFn> = Some(fc_dirname);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_dirname, from)
    }
}

type Dirname = CachedArgs<DirnameEv>;

fn fc_basename(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(path) => match Path::basename(path) {
            None => Some(Value::Null),
            Some(dn) => Some(Value::String(dn.into())),
        },
        _ => None,
    }
}

#[derive(Debug, Default)]
struct BasenameEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BasenameEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_basename";
    const FASTCALL: Option<FastFn> = Some(fc_basename);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_basename, from)
    }
}

type Basename = CachedArgs<BasenameEv>;

fn fc_row_col(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(path) => {
            let col = match Path::basename(path) {
                Some(s) => s,
                None => return Some(Value::Null),
            };
            let parent = match Path::dirname(path) {
                Some(s) => s,
                None => return Some(Value::Null),
            };
            let row = match Path::basename(parent) {
                Some(s) => s,
                None => return Some(Value::Null),
            };
            Some(Value::Array(ValArray::from([
                Value::String(row.into()),
                Value::String(col.into()),
            ])))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct RowColEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for RowColEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_row_col";
    const FASTCALL: Option<FastFn> = Some(fc_row_col);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_row_col, from)
    }
}

type RowCol = CachedArgs<RowColEv>;

fn fc_join(args: &[Value]) -> Option<Value> {
    thread_local! {
        static BUF: RefCell<String> = RefCell::new(String::new());
    }
    match args {
        [_] | [] => None,
        [sep, parts @ ..] => {
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
                    match p {
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

#[derive(Debug, Default)]
struct StringJoinEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringJoinEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_join";
    const FASTCALL: Option<FastFn> = Some(fc_join);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_join, from)
    }
}

type StringJoin = CachedArgs<StringJoinEv>;

fn fc_concat(args: &[Value]) -> Option<Value> {
    thread_local! {
        static BUF: RefCell<String> = RefCell::new(String::new());
    }
    BUF.with_borrow_mut(|buf| {
        buf.clear();
        for p in args {
            match p {
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

#[derive(Debug, Default)]
struct StringConcatEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringConcatEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_concat";
    const FASTCALL: Option<FastFn> = Some(fc_concat);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_concat, from)
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
            out: TagValue,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const EFFECT: EffectKind = EffectKind::Sync;
            const STATELESS: bool = true;
            const NAME: &str = $builtin_name;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                _from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new(Self { escape: None, out: TagValue::phantom() }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> &TagValue {
                static TAG: ArcStr = literal!("StringError");
                // both args update up front: the config error path
                // below must not skip the data arg's update. Only a
                // FIRED delivery is an event on either channel.
                let esc = match seam_value(from[0].update(ctx, event)) {
                    Some(tv) if tv.is_fired() => Some(tv.value_cloned()),
                    _ => None,
                };
                let data = match seam_value(from[1].update(ctx, event)) {
                    Some(tv) if tv.is_fired() => Some(tv.value_cloned()),
                    _ => None,
                };
                if let Some(esc) = esc {
                    match build_escape(esc) {
                        Err(e) => {
                            let v = errf!(TAG, "escape: invalid argument {e:?}");
                            return self.out.set(TagValue::fired(v));
                        }
                        Ok(esc) => self.escape = Some(esc),
                    }
                }
                match (&self.escape, data) {
                    (Some(esc), Some(Value::String(s))) => self.out.set(TagValue::fired(
                        Value::String(ArcStr::from(esc.$escape(&s))),
                    )),
                    (_, _) => self.out.ride(),
                }
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
                // Sleep is PAUSE: the compiled escape is a pure memo
                // of the config arg and survives — clearing it here
                // required a fired config re-delivery no wake provides
                // (a default arg fires once, at the instance's birth),
                // so a re-woken arm's escape stayed unconfigured
                // forever (aug31f ryouko finding 01's second layer).
            }

            fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
                // The compiled escape is a pure memo of the config
                // arg and survives replay.
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
            const EFFECT: EffectKind = EffectKind::Sync;
            const NAME: &str = $builtin;
            const STATELESS: bool = true;

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
            const EFFECT: EffectKind = EffectKind::Sync;
            const NAME: &str = $builtin;
            const STATELESS: bool = true;

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
                        return Some(errf!(TAG, "splitn: {v} must be a number > 0"));
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

fn fc_split_escaped(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("SplitEscError");
    let esc = match &args[0] {
        Value::String(s) if s.len() == 1 => s.chars().next().unwrap(),
        _ => return Some(err!(TAG, "split_escaped: invalid escape char")),
    };
    let sep = match &args[1] {
        Value::String(s) if s.len() == 1 => s.chars().next().unwrap(),
        _ => return Some(err!(TAG, "split_escaped: invalid separator")),
    };
    match &args[2] {
        Value::String(s) => Some(Value::Array(ValArray::from_iter(
            escaping::split(s, esc, sep).map(|s| Value::String(ArcStr::from(s))),
        ))),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StringSplitEscapedEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringSplitEscapedEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_split_escaped";
    const FASTCALL: Option<FastFn> = Some(fc_split_escaped);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_split_escaped, from)
    }
}

type StringSplitEscaped = CachedArgs<StringSplitEscapedEv>;

fn fc_splitn_escaped(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("SplitNEscError");
    let n = match &args[0] {
        Value::I64(n) if *n > 0 => *n as usize,
        v => return Some(errf!(TAG, "splitn_escaped: invalid n {v}")),
    };
    let esc = match &args[1] {
        Value::String(s) if s.len() == 1 => s.chars().next().unwrap(),
        _ => return Some(err!(TAG, "split_escaped: invalid escape char")),
    };
    let sep = match &args[2] {
        Value::String(s) if s.len() == 1 => s.chars().next().unwrap(),
        _ => return Some(err!(TAG, "split_escaped: invalid separator")),
    };
    match &args[3] {
        Value::String(s) => Some(Value::Array(ValArray::from_iter(
            escaping::splitn(s, esc, n, sep).map(|s| Value::String(ArcStr::from(s))),
        ))),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StringSplitNEscapedEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringSplitNEscapedEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_splitn_escaped";
    const FASTCALL: Option<FastFn> = Some(fc_splitn_escaped);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_splitn_escaped, from)
    }
}

type StringSplitNEscaped = CachedArgs<StringSplitNEscapedEv>;

fn fc_split_once(args: &[Value]) -> Option<Value> {
    let pat = match &args[0] {
        Value::String(s) => s,
        _ => return None,
    };
    match &args[1] {
        Value::String(s) => match s.split_once(&**pat) {
            None => Some(Value::Null),
            Some((s0, s1)) => Some(Value::Array(ValArray::from([
                Value::String(s0.into()),
                Value::String(s1.into()),
            ]))),
        },
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StringSplitOnceEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringSplitOnceEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_split_once";
    const FASTCALL: Option<FastFn> = Some(fc_split_once);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_split_once, from)
    }
}

type StringSplitOnce = CachedArgs<StringSplitOnceEv>;

fn fc_rsplit_once(args: &[Value]) -> Option<Value> {
    let pat = match &args[0] {
        Value::String(s) => s,
        _ => return None,
    };
    match &args[1] {
        Value::String(s) => match s.rsplit_once(&**pat) {
            None => Some(Value::Null),
            Some((s0, s1)) => Some(Value::Array(ValArray::from([
                Value::String(s0.into()),
                Value::String(s1.into()),
            ]))),
        },
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StringRSplitOnceEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringRSplitOnceEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_rsplit_once";
    const FASTCALL: Option<FastFn> = Some(fc_rsplit_once);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_rsplit_once, from)
    }
}

type StringRSplitOnce = CachedArgs<StringRSplitOnceEv>;

fn fc_to_lower(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(s) => Some(Value::String(s.to_lowercase().into())),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StringToLowerEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringToLowerEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_to_lower";
    const FASTCALL: Option<FastFn> = Some(fc_to_lower);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_to_lower, from)
    }
}

type StringToLower = CachedArgs<StringToLowerEv>;

fn fc_to_upper(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(s) => Some(Value::String(s.to_uppercase().into())),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct StringToUpperEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for StringToUpperEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_to_upper";
    const FASTCALL: Option<FastFn> = Some(fc_to_upper);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_to_upper, from)
    }
}

type StringToUpper = CachedArgs<StringToUpperEv>;

fn fc_sprintf(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(fmt), rest @ ..] => {
            let mut buf = String::new();
            match netidx_value::printf(&mut buf, fmt, rest) {
                Ok(_) => Some(Value::String(ArcStr::from(&buf))),
                Err(e) => Some(errf!(literal!("FormatError"), "{e}")),
            }
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct SprintfEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SprintfEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_sprintf";
    const FASTCALL: Option<FastFn> = Some(fc_sprintf);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_sprintf, from)
    }
}

type Sprintf = CachedArgs<SprintfEv>;

#[derive(Debug, Default)]
struct LenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for LenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_len";
    const FASTCALL: Option<FastFn> = Some(str_len);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(str_len, from)
    }
}

fn str_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(s)] => Some(Value::I64(s.len() as i64)),
        _ => None,
    }
}

type Len = CachedArgs<LenEv>;

fn fc_sub(args: &[Value]) -> Option<Value> {
    match args {
        [Value::I64(start), Value::I64(len), Value::String(s)]
            if *start >= 0 && *len >= 0 =>
        {
            let start = *start as usize;
            let end = start + *len as usize;
            let mut buf = String::new();
            for (i, c) in s.chars().enumerate() {
                if i >= start && i < end {
                    buf.push(c);
                }
            }
            Some(Value::String(ArcStr::from(&buf)))
        }
        v @ [_, _, _] => {
            Some(errf!(literal!("SubError"), "sub args must be non negative {v:?}"))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct SubEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SubEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_sub";
    const FASTCALL: Option<FastFn> = Some(fc_sub);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_sub, from)
    }
}

type Sub = CachedArgs<SubEv>;

#[derive(Debug, Default)]
struct ParseEv {
    cast_typ: Option<Type>,
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ParseEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "str_parse";

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

    fn typecheck0(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.cast_typ = extract_cast_type(Some(resolved));
        if std::env::var_os("GXDBG_PARSE").is_some() {
            eprintln!("PARSE-TC1 cast_typ={:?} resolved={resolved:?}", self.cast_typ);
        }
        if self.cast_typ.is_none() {
            bail!("str::parse requires a concrete return type")
        }
        Ok(())
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
        if std::env::var_os("GXDBG_PARSE").is_some() {
            eprintln!("PARSE cast_typ={:?}", self.cast_typ);
        }
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
        RowCol,
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
