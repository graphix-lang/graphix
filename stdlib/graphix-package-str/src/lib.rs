#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use escaping::Escape;
use graphix_compiler::{
    env::Env,
    err, errf,
    expr::{Expr, split_escaped},
    rep,
    typ::{FnType, TVal, Type},
};
use graphix_package_core::{ErrorText, FastMemo, cast_target};
use netidx::{path::Path, subscriber::Value};
use netidx_derive::FromValue;
use netidx_value::{Typ, ValArray};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::cell::RefCell;

/// A fast builtin over one string argument `$s`, its value `$e`.
macro_rules! str_fn {
    ($name:ident, $ev:ident, $builtin:literal, $fc:ident, |$s:ident| $e:expr) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            match args {
                [Value::String($s)] => Some($e),
                _ => None,
            }
        }

        graphix_package_core::fast_builtin!($name, $ev, $builtin, $fc);
    };
}

/// A fast builtin over a pattern `$p` and then a string `$s`, its value
/// `$e`.
macro_rules! str_pat_fn {
    ($name:ident, $ev:ident, $builtin:literal, $fc:ident, |$p:ident, $s:ident| $e:expr) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            match args {
                [Value::String($p), Value::String($s)] => Some($e),
                _ => None,
            }
        }

        graphix_package_core::fast_builtin!($name, $ev, $builtin, $fc);
    };
}

/// A string, or null for none.
fn some_str(s: Option<&str>) -> Value {
    s.map_or(Value::Null, |s| Value::String(s.into()))
}

/// The pair of strings as a tuple, or null for none.
fn some_pair(p: Option<(&str, &str)>) -> Value {
    match p {
        None => Value::Null,
        Some((a, b)) => rep::tuple([Value::String(a.into()), Value::String(b.into())]),
    }
}

str_pat_fn!(StartsWith, StartsWithEv, "str_starts_with", fc_starts_with, |p, s| {
    Value::Bool(s.starts_with(&**p))
});
str_pat_fn!(EndsWith, EndsWithEv, "str_ends_with", fc_ends_with, |p, s| {
    Value::Bool(s.ends_with(&**p))
});
str_pat_fn!(Contains, ContainsEv, "str_contains", fc_contains, |p, s| {
    Value::Bool(s.contains(&**p))
});
str_pat_fn!(StripPrefix, StripPrefixEv, "str_strip_prefix", fc_strip_prefix, |p, s| {
    some_str(s.strip_prefix(&**p))
});
str_pat_fn!(StripSuffix, StripSuffixEv, "str_strip_suffix", fc_strip_suffix, |p, s| {
    some_str(s.strip_suffix(&**p))
});
str_pat_fn!(
    StringSplitOnce,
    StringSplitOnceEv,
    "str_split_once",
    fc_split_once,
    |p, s| some_pair(s.split_once(&**p))
);
str_pat_fn!(
    StringRSplitOnce,
    StringRSplitOnceEv,
    "str_rsplit_once",
    fc_rsplit_once,
    |p, s| some_pair(s.rsplit_once(&**p))
);
str_pat_fn!(StringSplit, StringSplitEv, "str_split", fc_split, |p, s| strings(
    s.split(&**p)
));
str_pat_fn!(StringRSplit, StringRSplitEv, "str_rsplit", fc_rsplit, |p, s| {
    strings(s.rsplit(&**p))
});
str_fn!(Trim, TrimEv, "str_trim", fc_trim, |s| Value::String(s.trim().into()));
str_fn!(TrimStart, TrimStartEv, "str_trim_start", fc_trim_start, |s| {
    Value::String(s.trim_start().into())
});
str_fn!(TrimEnd, TrimEndEv, "str_trim_end", fc_trim_end, |s| {
    Value::String(s.trim_end().into())
});
str_fn!(StringToLower, StringToLowerEv, "str_to_lower", fc_to_lower, |s| {
    Value::String(s.to_lowercase().into())
});
str_fn!(StringToUpper, StringToUpperEv, "str_to_upper", fc_to_upper, |s| {
    Value::String(s.to_uppercase().into())
});

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
            Some(rep::tuple([Value::String(row.into()), Value::String(col.into())]))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(RowCol, RowColEv, "str_row_col", fc_row_col);

/// `f` over each string of `parts`, an array's strings in order; `None`
/// when a part is neither a string nor an array.
fn each_part(parts: &[Value], mut f: impl FnMut(&str)) -> Option<()> {
    for p in parts {
        match p {
            Value::String(c) => f(c),
            Value::Array(a) => a.iter().for_each(|v| {
                if let Value::String(c) = v {
                    f(c)
                }
            }),
            _ => return None,
        }
    }
    Some(())
}

fn fc_join(args: &[Value]) -> Option<Value> {
    let [Value::String(sep), parts @ ..] = args else { return None };
    if parts.is_empty() {
        return None;
    }
    let mut buf: LPooled<String> = LPooled::take();
    let mut first = true;
    each_part(parts, |c| {
        if !first {
            buf.push_str(sep);
        }
        first = false;
        buf.push_str(c);
    })?;
    Some(Value::String(ArcStr::from(buf.as_str())))
}

graphix_package_core::fast_builtin!(StringJoin, StringJoinEv, "str_join", fc_join);

fn fc_concat(args: &[Value]) -> Option<Value> {
    let mut buf: LPooled<String> = LPooled::take();
    each_part(args, |c| buf.push_str(c))?;
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
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
    struct Fields {
        escape: ArcStr,
        escape_char: ArcStr,
        tr: SmallVec<[Value; 8]>,
    }
    let Fields { escape, escape_char, tr } = esc.cast_to().context("parse escape")?;
    let Some(escape_char) = one_char(&Value::String(escape_char)) else {
        bail!("expected a single escape char")
    };
    let to_escape = escape.chars().collect::<SmallVec<[char; 32]>>();
    let tr = tr
        .iter()
        .map(|kv| match rep::read_tuple(kv).map(rep::Parts::elems) {
            Some([key @ Value::String(k), Value::String(v)]) => match one_char(key) {
                Some(c) => Ok((c, v.as_str())),
                None => bail!("escape: tr key {k} is invalid, expected 1 character"),
            },
            _ => bail!("escape: tr entry {kv} is not a pair of strings"),
        })
        .collect::<Result<SmallVec<[_; 8]>>>()?;
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

        graphix_package_core::fast_builtin!($name, $ev, $builtin, $fc);
    };
}

escape_fn!(StringEscapeEv, StringEscape, "str_escape", fc_escape, escape);
escape_fn!(StringUnescapeEv, StringUnescape, "str_unescape", fc_unescape, unescape);

fn strings<'a>(it: impl Iterator<Item = &'a str>) -> Value {
    Value::Array(ValArray::from_iter(it.map(|s| Value::String(ArcStr::from(s)))))
}

macro_rules! string_splitn {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident, $fn:ident) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            static TAG: ArcStr = literal!("StringSplitError");
            match args {
                [Value::String(pat), Value::I64(n), Value::String(s)] if *n > 0 => {
                    Some(strings(s.$fn(*n as usize, &**pat)))
                }
                [_, Value::I64(n), _] => Some(errf!(TAG, "#n must be > 0, got {n}")),
                _ => None,
            }
        }

        graphix_package_core::fast_builtin!($name, $ev, $builtin, $fc);
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

/// The escape and separator chars of a split, each one char and not the
/// other: an escape that is the separator would escape every separator.
fn esc_sep(tag: &ArcStr, esc: &Value, sep: &Value) -> Result<(char, char), Value> {
    let esc = one_char(esc).ok_or_else(|| err!(tag, "invalid escape char"))?;
    let sep = one_char(sep).ok_or_else(|| err!(tag, "invalid separator"))?;
    match esc == sep {
        true => Err(err!(tag, "escape and separator must differ")),
        false => Ok((esc, sep)),
    }
}

/// `s` split into at most `n` parts at `sep` where `esc` does not escape
/// it, or the error `tag` names a bad `esc` or `sep` by.
fn split_esc(
    tag: &ArcStr,
    n: usize,
    esc: &Value,
    sep: &Value,
    s: &Value,
) -> Option<Value> {
    let (esc, sep) = match esc_sep(tag, esc, sep) {
        Ok(es) => es,
        Err(e) => return Some(e),
    };
    match s {
        Value::String(s) => Some(strings(split_escaped(s, esc, sep, n))),
        _ => None,
    }
}

fn fc_split_escaped(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("SplitEscError");
    match args {
        [esc, sep, s] => split_esc(&TAG, usize::MAX, esc, sep, s),
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
    match args {
        [Value::I64(n), esc, sep, s] if *n > 0 => {
            split_esc(&TAG, *n as usize, esc, sep, s)
        }
        [Value::I64(n), ..] => Some(errf!(TAG, "#n must be > 0, got {n}")),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(
    StringSplitNEscaped,
    StringSplitNEscapedEv,
    "str_splitn_escaped",
    fc_splitn_escaped
);

/// The most text sprintf writes, as much as `buffer::encode` pads: a
/// width or precision from data allocates no further.
const MAX_SPRINTF: usize = 64 * 1024 * 1024;

/// A writer that refuses to grow past [`MAX_SPRINTF`].
struct Capped<'a> {
    buf: &'a mut String,
    over: bool,
}

impl std::fmt::Write for Capped<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if self.buf.len() + s.len() > MAX_SPRINTF {
            self.over = true;
            return Err(std::fmt::Error);
        }
        self.buf.push_str(s);
        Ok(())
    }
}

fn fc_sprintf(env: &Env, _ft: &FnType, args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("FormatError");
    let [Value::String(fmt), rest @ ..] = args else { return None };
    let composite = |v: &Value| {
        matches!(
            v,
            Value::Array(_) | Value::Map(_) | Value::Abstract(_) | Value::Error(_)
        )
    };
    let mut printed: LPooled<Vec<Value>> = LPooled::take();
    if rest.iter().any(composite) {
        printed.extend(rest.iter().map(|v| match v {
            v if composite(v) => {
                let v = TVal { env, typ: &Type::Any, v };
                Value::String(format_compact!("{v}").as_str().into())
            }
            v => v.clone(),
        }));
    }
    let rest = if printed.is_empty() { rest } else { &printed[..] };
    let mut buf: LPooled<String> = LPooled::take();
    let mut out = Capped { buf: &mut buf, over: false };
    match netidx_value::printf(&mut out, fmt, rest) {
        Ok(_) => Some(Value::String(ArcStr::from(buf.as_str()))),
        Err(_) if out.over => {
            Some(errf!(TAG, "the result exceeds the {MAX_SPRINTF} byte limit"))
        }
        Err(e) => Some(errf!(TAG, "{e}")),
    }
}

graphix_package_core::typed_fast_builtin!(Sprintf, SprintfEv, "str_sprintf", fc_sprintf);

graphix_package_core::fast_builtin!(Len, LenEv, "str_len", str_len);

fn str_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(s)] => Some(Value::I64(s.len() as i64)),
        _ => None,
    }
}

graphix_package_core::fast_builtin!(CharLen, CharLenEv, "str_char_len", str_char_len);

fn str_char_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(s)] => Some(Value::I64(s.chars().count() as i64)),
        _ => None,
    }
}

fn fc_sub(args: &[Value]) -> Option<Value> {
    match args {
        [Value::I64(start), Value::I64(len), Value::String(s)]
            if *start >= 0 && *len >= 0 =>
        {
            // the byte bounds of chars [start, start + len), cut at the end
            let a = s.char_indices().nth(*start as usize).map_or(s.len(), |(i, _)| i);
            let b =
                s[a..].char_indices().nth(*len as usize).map_or(s.len(), |(i, _)| a + i);
            Some(Value::String(match (a, b) {
                (0, b) if b == s.len() => s.clone(),
                (a, b) => ArcStr::from(&s[a..b]),
            }))
        }
        [Value::I64(start), Value::I64(len), _] => {
            let (arg, n) = if *start < 0 { ("#start", start) } else { ("#len", len) };
            Some(errf!(literal!("SubError"), "{arg} must be non negative, got {n}"))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Sub, SubEv, "str_sub", fc_sub);

fn fc_parse(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("ParseError");
    let [Value::String(s)] = args else { return None };
    // every failure is the declared ParseError
    let Some(typ) = cast_target(&ft.rtype) else {
        return Some(errf!(TAG, "parse requires a concrete type annotation"));
    };
    // a string prints as its text, and a number as text the cast reads
    let parsed = match &typ {
        Type::Primitive(p) if *p == Typ::String => return Some(Value::String(s.clone())),
        Type::Primitive(p) if !p.is_empty() && p.iter().all(|t| t.is_number()) => None,
        _ => s.parse::<Expr>().ok().and_then(|e| e.literal_value()),
    };
    Some(match parsed {
        Some(v) => match typ.cast_value(env, v) {
            Value::Error(e) => errf!(TAG, "{}", ErrorText(&e)),
            v => v,
        },
        None => match typ.cast_value(env, Value::String(s.clone())) {
            Value::Error(_) => errf!(TAG, "cannot parse {s:?} as {typ}"),
            v => v,
        },
    })
}

graphix_package_core::typed_fast_builtin!(Parse, ParseEv, "str_parse", fc_parse);

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
        CharLen,
        Sub,
        Parse,
    ],
}
