#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use graphix_compiler::errf;
use graphix_package_core::FastMemo;
use netidx::subscriber::Value;
use netidx_value::ValArray;
use regex::Regex;
use std::cell::RefCell;

static TAG: ArcStr = literal!("ReError");

thread_local! {
    static PATTERNS: RefCell<FastMemo<ArcStr, Regex>> = RefCell::new(FastMemo::new(64));
}

/// Run `f` over the compiled `pat`; an invalid pattern is the `ReError`
/// value.
fn with_regex(pat: &ArcStr, f: impl FnOnce(&Regex) -> Value) -> Value {
    PATTERNS.with(|c| {
        c.borrow_mut()
            .with(pat, || Ok(Regex::new(pat)?), f)
            .unwrap_or_else(|e| errf!(TAG, "{e:#}"))
    })
}

fn strings<'a>(it: impl Iterator<Item = &'a str>) -> Value {
    Value::Array(ValArray::from_iter(it.map(|s| Value::String(s.into()))))
}

fn fc_is_match(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(pat), Value::String(s)] => {
            Some(with_regex(pat, |re| Value::Bool(re.is_match(s))))
        }
        _ => None,
    }
}

fn fc_find(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(pat), Value::String(s)] => {
            Some(with_regex(pat, |re| strings(re.find_iter(s).map(|m| m.as_str()))))
        }
        _ => None,
    }
}

fn fc_captures(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(pat), Value::String(s)] => Some(with_regex(pat, |re| {
            Value::Array(ValArray::from_iter(re.captures_iter(s).map(|c| {
                Value::Array(ValArray::from_iter(c.iter().map(|m| match m {
                    None => Value::Null,
                    Some(m) => Value::String(m.as_str().into()),
                })))
            })))
        })),
        _ => None,
    }
}

fn fc_split(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(pat), Value::String(s)] => {
            Some(with_regex(pat, |re| strings(re.split(s))))
        }
        _ => None,
    }
}

fn fc_splitn(args: &[Value]) -> Option<Value> {
    match args {
        [Value::String(pat), Value::I64(lim), Value::String(s)] => {
            match usize::try_from(*lim) {
                Ok(lim) if lim > 0 => {
                    Some(with_regex(pat, |re| strings(re.splitn(s, lim))))
                }
                _ => Some(errf!(TAG, "splitn: #limit must be positive, not {lim}")),
            }
        }
        _ => None,
    }
}

graphix_package_core::fast_builtin!(IsMatch, IsMatchEv, "re_is_match", fc_is_match);
graphix_package_core::fast_builtin!(Find, FindEv, "re_find", fc_find);
graphix_package_core::fast_builtin!(Captures, CapturesEv, "re_captures", fc_captures);
graphix_package_core::fast_builtin!(Split, SplitEv, "re_split", fc_split);
graphix_package_core::fast_builtin!(SplitN, SplitNEv, "re_splitn", fc_splitn);

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
