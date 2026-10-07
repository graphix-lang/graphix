#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use escaping::Escape;
use graphix_compiler::{
    CompileCtx, ExecCtx, FastCall, Node, Rt, Scope, UserEvent,
    effects::Effect,
    env::Env,
    err, errf,
    expr::ExprId,
    expr::split_escaped,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, FastMemo, cast_target, fast_eval, fast_eval_typed,
};
use netidx::{path::Path, subscriber::Value};
use netidx_derive::FromValue;
use netidx_value::ValArray;
use poolshark::local::LPooled;
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

graphix_package_core::fast_builtin!(
    StartsWith,
    StartsWithEv,
    "str_starts_with",
    fc_starts_with
);

fn fc_ends_with(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(sfx), Value::String(val)) => {
            Some(Value::Bool(val.ends_with(&**sfx)))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(EndsWith, EndsWithEv, "str_ends_with", fc_ends_with);

fn fc_contains(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(chs), Value::String(val)) => {
            Some(Value::Bool(val.contains(&**chs)))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Contains, ContainsEv, "str_contains", fc_contains);

fn fc_strip_prefix(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(pfx), Value::String(val)) => val
            .strip_prefix(&**pfx)
            .map(|s| Value::String(s.into()))
            .or(Some(Value::Null)),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StripPrefix,
    StripPrefixEv,
    "str_strip_prefix",
    fc_strip_prefix
);

fn fc_strip_suffix(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::String(sfx), Value::String(val)) => val
            .strip_suffix(&**sfx)
            .map(|s| Value::String(s.into()))
            .or(Some(Value::Null)),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StripSuffix,
    StripSuffixEv,
    "str_strip_suffix",
    fc_strip_suffix
);

fn fc_trim(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(val) => Some(Value::String(val.trim().into())),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Trim, TrimEv, "str_trim", fc_trim);

fn fc_trim_start(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(val) => Some(Value::String(val.trim_start().into())),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    TrimStart,
    TrimStartEv,
    "str_trim_start",
    fc_trim_start
);

fn fc_trim_end(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(val) => Some(Value::String(val.trim_end().into())),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(TrimEnd, TrimEndEv, "str_trim_end", fc_trim_end);

fn fc_replace(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1], &args[2]) {
        (Value::String(pat), Value::String(rep), Value::String(val)) => {
            Some(Value::String(val.replace(&**pat, &**rep).into()))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Replace, ReplaceEv, "str_replace", fc_replace);

fn fc_dirname(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(path) => match Path::dirname(path) {
            // only an absolute path one level below the root has it as parent
            None if path.starts_with('/') && path.len() > 1 => {
                Some(Value::String(literal!("/")))
            }
            None => Some(Value::Null),
            Some(dn) => Some(Value::String(dn.into())),
        },
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Dirname, DirnameEv, "str_dirname", fc_dirname);

fn fc_basename(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(path) => match Path::basename(path) {
            None => Some(Value::Null),
            Some(dn) => Some(Value::String(dn.into())),
        },
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Basename, BasenameEv, "str_basename", fc_basename);

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

graphix_package_core::fast_builtin!(RowCol, RowColEv, "str_row_col", fc_row_col);

fn fc_join(args: &[Value]) -> Option<Value> {
    let [sep, parts @ ..] = args else { return None };
    if parts.is_empty() {
        return None;
    }
    let sep = match sep {
        Value::String(c) => c.clone(),
        sep => sep.clone().cast_to::<ArcStr>().ok()?,
    };
    let mut buf: LPooled<String> = LPooled::take();
    let mut first = true;
    let mut push = |c: &str| {
        if !first {
            buf.push_str(&sep);
        }
        first = false;
        buf.push_str(c);
    };
    for p in parts {
        match p {
            Value::String(c) => push(c),
            Value::Array(a) => {
                for v in a.iter() {
                    if let Value::String(c) = v {
                        push(c)
                    }
                }
            }
            _ => return None,
        }
    }
    Some(Value::String(ArcStr::from(buf.as_str())))
}

graphix_package_core::fast_builtin!(StringJoin, StringJoinEv, "str_join", fc_join);

fn fc_concat(args: &[Value]) -> Option<Value> {
    let mut buf: LPooled<String> = LPooled::take();
    for p in args {
        match p {
            Value::String(c) => buf.push_str(c),
            Value::Array(a) => {
                for v in a.iter() {
                    if let Value::String(c) = v {
                        buf.push_str(c)
                    }
                }
            }
            _ => return None,
        }
    }
    Some(Value::String(ArcStr::from(buf.as_str())))
}

graphix_package_core::fast_builtin!(
    StringConcat,
    StringConcatEv,
    "str_concat",
    fc_concat
);

fn build_escape(esc: Value) -> Result<Escape> {
    fn escape_non_printing(c: char) -> bool {
        c.is_control()
    }
    #[derive(FromValue)]
    struct Fields {
        escape: ArcStr,
        escape_char: ArcStr,
        tr: SmallVec<[(ArcStr, ArcStr); 8]>,
    }
    let Fields { escape, escape_char, tr } = esc.cast_to().context("parse escape")?;
    let Some(escape_char) = one_char(&Value::String(escape_char)) else {
        bail!("expected a single escape char")
    };
    let to_escape = escape.chars().collect::<SmallVec<[char; 32]>>();
    let tr = tr
        .into_iter()
        .map(|(k, v)| match one_char(&Value::String(k.clone())) {
            Some(c) => Ok((c, v)),
            None => bail!("escape: tr key {k} is invalid, expected 1 character"),
        })
        .collect::<Result<SmallVec<[_; 8]>>>()?;
    let tr = tr.iter().map(|(c, s)| (*c, s.as_str())).collect::<SmallVec<[_; 8]>>();
    Escape::new(escape_char, &to_escape, &tr, Some(escape_non_printing))
}

thread_local! {
    static ESCAPES: RefCell<FastMemo<Value, Escape>> = RefCell::new(FastMemo::new(16));
}

/// Run `f` over the escape table `esc` configures; an invalid
/// configuration is the `StringError` value.
fn with_escape(esc: &Value, f: impl FnOnce(&Escape) -> Value) -> Value {
    static TAG: ArcStr = literal!("StringError");
    ESCAPES.with(|c| {
        c.borrow_mut()
            .with(esc, || build_escape(esc.clone()), f)
            .unwrap_or_else(|e| errf!(TAG, "escape: invalid argument {e:#}"))
    })
}

macro_rules! escape_fn {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident, $escape:ident) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            match args {
                [esc, Value::String(s)] => Some(with_escape(esc, |esc| {
                    Value::String(ArcStr::from(esc.$escape(s)))
                })),
                _ => None,
            }
        }

        #[derive(Debug, Default)]
        struct $ev;

        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $ev {
            const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain($fc)));
            const NAME: &str = $builtin;

            fn eval(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                fast_eval(ctx, $fc, from)
            }
        }

        type $name = CachedArgs<$ev>;

        graphix_package_core::unit_image_state!($ev);
    };
}

escape_fn!(StringEscapeEv, StringEscape, "str_escape", fc_escape, escape);
escape_fn!(StringUnescapeEv, StringUnescape, "str_unescape", fc_unescape, unescape);

macro_rules! split_fn {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident) => {
        graphix_package_core::fast_builtin!($name, $ev, $builtin, $fc);
    };
}

fn strings<'a>(it: impl Iterator<Item = &'a str>) -> Value {
    Value::Array(ValArray::from_iter(it.map(|s| Value::String(ArcStr::from(s)))))
}

macro_rules! string_split {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident, $fn:ident) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            match args {
                [Value::String(pat), Value::String(s)] => Some(strings(s.$fn(&**pat))),
                _ => None,
            }
        }

        split_fn!($ev, $name, $builtin, $fc);
    };
}

string_split!(StringSplitEv, StringSplit, "str_split", fc_split, split);
string_split!(StringRSplitEv, StringRSplit, "str_rsplit", fc_rsplit, rsplit);

macro_rules! string_splitn {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident, $fn:ident) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            static TAG: ArcStr = literal!("StringSplitError");
            match args {
                [Value::String(pat), Value::I64(n), Value::String(s)] if *n > 0 => {
                    Some(strings(s.$fn(*n as usize, &**pat)))
                }
                [_, n, _] => Some(errf!(TAG, "splitn: {n} must be a number > 0")),
                _ => None,
            }
        }

        split_fn!($ev, $name, $builtin, $fc);
    };
}

string_splitn!(StringSplitNEv, StringSplitN, "str_splitn", fc_splitn, splitn);
string_splitn!(StringRSplitNEv, StringRSplitN, "str_rsplitn", fc_rsplitn, rsplitn);

/// The one char a string holds.
fn one_char(v: &Value) -> Option<char> {
    let Value::String(s) = v else { return None };
    let mut cs = s.chars();
    match (cs.next(), cs.next()) {
        (Some(c), None) => Some(c),
        _ => None,
    }
}

fn fc_split_escaped(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("SplitEscError");
    let esc = match &args[0] {
        v => match one_char(v) {
            Some(c) => c,
            None => return Some(err!(TAG, "invalid escape char")),
        },
    };
    let sep = match &args[1] {
        v => match one_char(v) {
            Some(c) => c,
            None => return Some(err!(TAG, "invalid separator")),
        },
    };
    match &args[2] {
        Value::String(s) => Some(Value::Array(ValArray::from_iter(
            split_escaped(s, esc, sep, usize::MAX)
                .map(|s| Value::String(ArcStr::from(s))),
        ))),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StringSplitEscaped,
    StringSplitEscapedEv,
    "str_split_escaped",
    fc_split_escaped
);

fn fc_splitn_escaped(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("SplitNEscError");
    let n = match &args[0] {
        Value::I64(n) if *n > 0 => *n as usize,
        v => return Some(errf!(TAG, "splitn_escaped: invalid n {v}")),
    };
    let esc = match &args[1] {
        v => match one_char(v) {
            Some(c) => c,
            None => return Some(err!(TAG, "invalid escape char")),
        },
    };
    let sep = match &args[2] {
        v => match one_char(v) {
            Some(c) => c,
            None => return Some(err!(TAG, "invalid separator")),
        },
    };
    match &args[3] {
        Value::String(s) => Some(Value::Array(ValArray::from_iter(
            split_escaped(s, esc, sep, n).map(|s| Value::String(ArcStr::from(s))),
        ))),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StringSplitNEscaped,
    StringSplitNEscapedEv,
    "str_splitn_escaped",
    fc_splitn_escaped
);

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

graphix_package_core::fast_builtin!(
    StringSplitOnce,
    StringSplitOnceEv,
    "str_split_once",
    fc_split_once
);

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

graphix_package_core::fast_builtin!(
    StringRSplitOnce,
    StringRSplitOnceEv,
    "str_rsplit_once",
    fc_rsplit_once
);

fn fc_to_lower(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(s) => Some(Value::String(s.to_lowercase().into())),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StringToLower,
    StringToLowerEv,
    "str_to_lower",
    fc_to_lower
);

fn fc_to_upper(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::String(s) => Some(Value::String(s.to_uppercase().into())),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StringToUpper,
    StringToUpperEv,
    "str_to_upper",
    fc_to_upper
);

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

graphix_package_core::fast_builtin!(Sprintf, SprintfEv, "str_sprintf", fc_sprintf);

graphix_package_core::fast_builtin!(Len, LenEv, "str_len", str_len);

fn str_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(s)] => Some(Value::I64(s.len() as i64)),
        _ => None,
    }
}

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

graphix_package_core::fast_builtin!(Sub, SubEv, "str_sub", fc_sub);

fn fc_parse(env: &Env, rtype: &Type, args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("ParseError");
    let raw = match args {
        [Value::String(s)] => match s.parse::<Value>() {
            Ok(Value::Error(e)) => return Some(errf!(TAG, "{e}")),
            Ok(v) => v,
            Err(e) => return Some(errf!(TAG, "{e:#}")),
        },
        _ => return None,
    };
    // every failure is the declared ParseError
    Some(match cast_target(rtype) {
        Some(typ) => match typ.cast_value(env, raw) {
            Value::Error(e) => errf!(TAG, "{e}"),
            v => v,
        },
        None => errf!(TAG, "parse requires a concrete type annotation"),
    })
}

#[derive(Debug, Default, netidx_derive::Pack)]
struct ParseEv {
    rtype: Option<Type>,
}

graphix_package_core::pack_image_state!(ParseEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ParseEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Typed(fc_parse)));
    const NAME: &str = "str_parse";

    fn init(
        _ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self { rtype: resolved.map(|ft| ft.rtype.clone()) }
    }

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.rtype = Some(resolved.rtype.clone());
        Ok(())
    }

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval_typed(ctx, fc_parse, self.rtype.as_ref()?, from)
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
