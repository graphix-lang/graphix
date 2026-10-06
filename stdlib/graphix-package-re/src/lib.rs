#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use graphix_compiler::{ExecCtx, FastCall, Rt, UserEvent, effects::Effect, errf};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached, FastMemo, fast_eval};
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
            // CR claude for claude: [bug] The ReError text is `{e:?}` of an
            // anyhow::Error. anyhow's Debug appends the backtrace it captured whenever
            // RUST_BACKTRACE or RUST_LIB_BACKTRACE is set, so the string a program
            // reads depends on the environment and on the engine: under
            // RUST_BACKTRACE=1 the error of `re::is_match(#pat: "(", "x")` is 973
            // characters in the JIT and 5957 in the node-walk, and graphix-fuzz check
            // reports a divergence. `{e:#}` gives the same message and its causes
            // without the trace; str::parse and str's escape functions build their
            // errors the same way (graphix-package-str/src/lib.rs:817, 451).
            // Separately, fc_splitn (line 77) casts #limit with `as usize`, so -1 means
            // no limit and 0 gives [] where str::splitn returns an error for n <= 0,
            // and the gxi's "split at most #limit times" is one off: #limit 2 gives two
            // parts. probe: design/review-2026-10-05/repro/small-pkgs-16.gx
            // (small-pkgs-16)
            .unwrap_or_else(|e| errf!(TAG, "{e:?}"))
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
            Some(with_regex(pat, |re| strings(re.splitn(s, *lim as usize))))
        }
        _ => None,
    }
}

macro_rules! re_fn {
    ($ev:ident, $name:ident, $builtin:literal, $fc:ident) => {
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

re_fn!(IsMatchEv, IsMatch, "re_is_match", fc_is_match);
re_fn!(FindEv, Find, "re_find", fc_find);
re_fn!(CapturesEv, Captures, "re_captures", fc_captures);
re_fn!(SplitEv, Split, "re_split", fc_split);
re_fn!(SplitNEv, SplitN, "re_splitn", fc_splitn);

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
