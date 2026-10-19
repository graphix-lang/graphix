#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use escaping::Escape;
use graphix_compiler::{env::Env, err, errf, expr::split_escaped, rep, typ::FnType};
use graphix_package_core::{FastMemo, cast_target};
use netidx::{path::Path, subscriber::Value};
use netidx_derive::FromValue;
use netidx_value::ValArray;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::cell::RefCell;

// CR claude for claude: [structure/low] starts_with/ends_with/contains,
// strip_prefix/strip_suffix, trim x3, to_lower/to_upper, split_once/rsplit_once and
// split_escaped/splitn_escaped are copies differing only in the str method called, and
// fc_join/fc_concat repeat the same parts walk. Fold them into macros like
// string_split! (one for str -> bool predicates, one for str -> str maps, one for the
// split_once pair) and one parts walker for join/concat. Found by reading; not run.
// (collections-str-10)
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
            Some(rep::tuple([Value::String(row.into()), Value::String(col.into())]))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(RowCol, RowColEv, "str_row_col", fc_row_col);

fn fc_join(args: &[Value]) -> Option<Value> {
    let [Value::String(sep), parts @ ..] = args else { return None };
    if parts.is_empty() {
        return None;
    }
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

macro_rules! string_split {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident, $fn:ident) => {
        fn $fc(args: &[Value]) -> Option<Value> {
            match args {
                [Value::String(pat), Value::String(s)] => Some(strings(s.$fn(&**pat))),
                _ => None,
            }
        }

        graphix_package_core::fast_builtin!($name, $ev, $builtin, $fc);
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

fn fc_split_escaped(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("SplitEscError");
    let (esc, sep) = match esc_sep(&TAG, &args[0], &args[1]) {
        Ok(es) => es,
        Err(e) => return Some(e),
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
    let (esc, sep) = match esc_sep(&TAG, &args[1], &args[2]) {
        Ok(es) => es,
        Err(e) => return Some(e),
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
            Some((s0, s1)) => {
                Some(rep::tuple([Value::String(s0.into()), Value::String(s1.into())]))
            }
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
            Some((s0, s1)) => {
                Some(rep::tuple([Value::String(s0.into()), Value::String(s1.into())]))
            }
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

// CR claude for claude: [bug/medium] printf pads to any width and precision, so a `*`
// width from data or a large width in a format string allocates without bound.
// `str::sprintf("<%*d>", 100000000000, 1)` OOM-kills the runtime, and `%.999999999f`
// builds a 1 GB string. buffer::encode caps Pad at 64 MiB; give sprintf the same cap.
// Write through a bounded fmt::Write that errors past the cap (fish-printf pads
// incrementally, so that stops it), and answer FormatError. probe:
// design/review-2026-10-19/repro/x-panics-sprintf-width-oom.gx (x-panics-04)
// CR claude for claude: [bug/medium] sprintf formats its `@args: Any` with Value's raw
// Display, so a tuple, struct, variant or list argument prints its kind-marker
// abstract: `str::sprintf("%s", (1, 2))` is `[abstract:EnwukQtE2EoTn2Uo4FoXw40A, i64:1,
// i64:2]`, where interpolation prints `(1, 2)`. Both engines agree on the wrong text.
// Fix: make it a typed_fast_builtin and render each non-scalar argument with the typed
// printer interpolation uses (TVal over the site's argument type), passing the result
// to printf as a string. probe:
// design/review-2026-10-19/repro/x-builtin-effects-sprintf-markers.gx Also reported as
// (x-engine-collections-02). (x-builtin-effects-01)
// CR claude for claude: [bug/medium] sprintf hands its `Any` args to netidx printf,
// which formats every array, map and abstract with netidx's own Display. So a tuple,
// struct, variant or list prints its kind markers (`[abstract:EnwukQtE2EoTn2Uo4FoXw40A,
// i64:1, i64:2]` for `(1, 2)`), and a user abstract type prints as base64 bytes with
// its Display impl ignored: a Plain fast fn runs without the display hooks. Make it a
// typed_fast_builtin running under with_display_hooks, and render each non-scalar
// argument with TVal under Type::Any (what interpolation prints) before calling printf.
// probe: design/review-2026-10-19/repro/collections-str-sprintf-markers.gx
// (collections-str-01)
fn fc_sprintf(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(fmt), rest @ ..] => {
            let mut buf: LPooled<String> = LPooled::take();
            // CR claude for claude: [bug/medium] netidx's printf formats a composite
            // argument with netidx's Display, which prints a tuple/struct/variant's
            // kind marker as `abstract:<base64>`. `str::sprintf("%s", (1, `A(2), {x:
            // 3}))` returns "[abstract:EnwukQ…A, i64:1, [abstract:…C, \"A\", i64:2],
            // …]". Render each Array/Map/Error/Abstract argument with typ::Naked into a
            // string before calling printf. fc_parse's two `errf!(TAG, "{e}")` arms and
            // core's read_back print a cast error the same way
            // (ParseError("[abstract:…, \"InvalidCast\", …]")); take the message out of
            // the InvalidCast variant there. probe:
            // design/review-2026-10-19/repro/x-panics-marker-leaks-into-text.gx
            // (x-panics-05)
            match netidx_value::printf(&mut *buf, fmt, rest) {
                Ok(_) => Some(Value::String(ArcStr::from(buf.as_str()))),
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
        v @ [_, _, _] => {
            // CR claude for claude: [readability/low] This message dumps the Rust Debug
            // of the argument slice, e.g. `[I64(-1), I64(2), String("héllo")]`. splitn
            // and splitn_escaped print `n` with netidx's typed Display ("i64:0"). Name
            // the argument and its plain value instead, e.g. "#start must be non
            // negative, got -1". probe: str::sub(#start: -1, #len: 2, "héllo") run by
            // hand. (collections-str-09)
            Some(errf!(literal!("SubError"), "sub args must be non negative {v:?}"))
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(Sub, SubEv, "str_sub", fc_sub);

// CR claude for claude: [bug/low] When the cast fails, the `Value::Error(e) =>
// errf!(TAG, "{e}")` arms format the InvalidCast variant with Value's raw Display, so
// the ParseError the program sees carries the kind marker:
// `[abstract:EnwukQtE2EoTn2Uo4FoXw40C, "InvalidCast", "can't cast 1.5 to (i64, string):
// not a tuple"]`. core's read_back (CachedArgsAsync DATA_RESULT, db subscriptions) has
// the same `errf!(tag, "{e}")`. Fix: one helper that takes an error value's message (a
// one-string variant's payload via rep::read_variant, else the typed printer), used
// here and in read_back. probe:
// design/review-2026-10-19/repro/x-builtin-effects-parse-error-markers.gx
// (x-builtin-effects-06)
fn fc_parse(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("ParseError");
    let [Value::String(s)] = args else { return None };
    // every failure is the declared ParseError
    let Some(typ) = cast_target(&ft.rtype) else {
        return Some(errf!(TAG, "parse requires a concrete type annotation"));
    };
    // CR claude for claude: [bug/medium] The value-literal parser takes a bare integer
    // only as i64. str::parse<u64> therefore refuses every u64 above i64::MAX,
    // including the string cast<string> writes for u64:18446744073709551615, while
    // cast<u64> of the same string reads it (typ/cast.rs `parse` goes through i128).
    // str::parse<f64>("9223372036854775808") fails too. For a scalar target, read the
    // string with the cast (cast_value of Value::String(s)), so there is one string
    // reader, and pin the u64::MAX round trip. probe:
    // design/review-2026-10-19/repro/x-engine-collections-parse-u64.gx
    // (x-engine-collections-03)
    // CR claude for eric: [bug/medium] parse reads the text as a netidx literal before
    // casting, so text that is not a literal fails even where the cast would succeed.
    // parse<string>("abc") is a ParseError, parse<string>("007") is "7" and
    // parse<string>("4.20") is "4.2", and parse<datetime>("2020-01-01T00:00:00Z") fails
    // while cast<datetime> of the same string works. This needs a rule. One option:
    // cast the String itself when the target holds string or the literal parse fails,
    // and keep the literal parse for collection syntax. probe:
    // design/review-2026-10-19/repro/collections-str-parse-not-cast.gx
    // (collections-str-06)
    Some(match s.parse::<Value>() {
        Ok(Value::Error(e)) => errf!(TAG, "{e}"),
        // CR claude for claude: [bug/medium] A failed cast is rewrapped by printing its
        // error payload with {e}. Since kind markers, that payload is the marked
        // `InvalidCast(msg) variant, so the ParseError a program sees reads
        // "[abstract:EnwukQtE2EoTn2Uo4FoXw40C, \"InvalidCast\", \"can't cast 300 to u8:
        // ...\"]" for an ordinary `str::parse("300")` into u8. core's read_back rewraps
        // the same way for typed async reads and db::subscribe. Fix: let Type return
        // the cast failure's message (cast_value_int has it before errf!) through a
        // Result-returning entry, and wrap that message here and in read_back, so the
        // error never round-trips through a Value. probe:
        // design/review-2026-10-19/repro/x-rerun-5-parse-error-prints-marker.gx
        // (x-rerun-5-03)
        Ok(v) => match typ.cast_value(env, v) {
            // CR claude for claude: [bug/medium] `e` is the marked variant
            // `InvalidCast(msg)`, and `{e}` is netidx's raw Value Display. The
            // ParseError text therefore carries the kind marker: str::parse<u8>("256")
            // gives ParseError("[abstract:EnwukQtE2EoTn2Uo4FoXw40C, \"InvalidCast\",
            // \"can't cast 256 to u8: ...\"]") in both engines. Take the message out of
            // the variant with rep::variant, or print it with tval::Naked, and pin the
            // text in a fixture. probe:
            // design/review-2026-10-19/repro/x-engine-collections-parse-marker-leak.gx
            // (x-engine-collections-01)
            // CR claude for claude: [bug/medium] `e` here is the cast's marked
            // `InvalidCast(msg)` variant, and `{e}` is netidx's Display. So
            // `str::parse` to u8 of "300" reports
            // ParseError("[abstract:EnwukQtE2EoTn2Uo4FoXw40C, \"InvalidCast\", \"can't
            // cast 300 to u8: ...\"]"). Use the variant's string payload (rep::variant)
            // as the message. graphix_package_core::read_back formats the same payload
            // the same way. probe:
            // design/review-2026-10-19/repro/collections-str-parse-error-marker.gx Also
            // reported as (gx-stdlib-02). (collections-str-02)
            Value::Error(e) => errf!(TAG, "{e}"),
            v => v,
        },
        Err(_) => errf!(TAG, "cannot parse {s:?} as {typ}"),
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
