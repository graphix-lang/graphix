//! Embedder-callable dispatch schedules — the `callable-v1` contract
//! and the ONE place its text format lives.
//!
//! The contract this mode enforces: **an embedder callable dispatch
//! (`GXHandle::compile_callable` + `Callable::call`) is observationally
//! the in-language call with the same arguments arriving on its
//! argument bindings.** The two routes share one text artifact; the
//! RUNNER picks the route, so the program flows unchanged through
//! every protocol (check / minimize / regress / corpus / the
//! isolated-child stdin), exactly like the schedule header:
//!
//! ```text
//! // callable-v1: handler=m0::handler; cx0=i64:7; cx0=i64:9
//! { m0::observe }
//! // file-v1: m0.gx
//! let state = { v: 0 };
//! let inner = |st: &{v: i64}, x: i64| -> null select x {
//!   0 => null,
//!   n => { *st <- (n ~ { v: (*st).v + n }); null }
//! };
//! let handler = |x: i64| -> null inner(&state, x);
//! let observe = state
//! ```
//!
//! Sections are `;`-separated: the handler's module path first, then
//! one section per DISPATCH epoch — a space-separated `name=value` set
//! giving the handler's positional arguments in order (every epoch
//! must carry the same name sequence; the names are the driver-decl
//! bindings). Values use the schedule's literal vocabulary.
//!
//! The driver is SYNTHESIZED by the runner, not written in the body:
//! the argument declarations (the D4 contract, `let cx0: i64 = i64:0;
//! cx0 <- never(i64:0);`) and the in-language call
//! (`let cdrv = m0::handler(cx0);`) are composed into the compile
//! text's top level — identically in BOTH routes, so at init the
//! handler runs once with the canonical defaults either way. The
//! routes then differ only in how the dispatch epochs are delivered:
//!
//! - **In-language** (route A): each epoch is a `set_many` injection
//!   on the argument bindings — the driver's callsite dispatches, the
//!   existing schedule machinery verbatim.
//! - **Dispatch** (route B): the arguments are never injected; the
//!   runner resolves the handler (`compile_ref_by_name` → the lambda
//!   value → `compile_callable`) and `call`s each epoch's values —
//!   the path every GUI/TUI handler dispatch takes, where the callee
//!   instances are born lazily cycles after the enclosing body's
//!   reference values were delivered (the ConnectDeref silent-write
//!   class, 9f9e01d0 — invisible to the engine-differential oracle
//!   because both engines share the node).
//!
//! The handler lives in a `file-v1` module because the body's block
//! scopes under an anonymous `do<ExprId>` path — module bindings are
//! what `compile_ref_by_name` can reach from root.
//!
//! Route comparison strength is per-epoch FINAL values
//! ([`crate::trace::Trace::agrees_final`]): the routes' dispatch
//! machinery differs (injection echo vs callable arg tasks), so
//! cycle-exact pacing across routes is not contractual; the settled
//! value per epoch is.

use netidx::publisher::Value;

use crate::schedule::{canonical, parse_value, render_value, value_kind};

pub const HEADER_PREFIX: &str = "// callable-v1:";

/// Cheap detection for routing decisions (batching, check pairing)
/// without a full parse. Same leading-comment-block scan as parse: a
/// header below provenance comments still counts.
pub fn has_header(text: &str) -> bool {
    let mut cursor = text;
    loop {
        let t = cursor.trim_start_matches(['\n', ' ']);
        if t.starts_with(HEADER_PREFIX) {
            return true;
        }
        if t.starts_with("//") {
            match t.split_once('\n') {
                Some((_, r)) => cursor = r,
                None => return false,
            }
        } else {
            return false;
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallSpec {
    /// The handler binding's module path (e.g. `m0::handler`) — must
    /// be reachable by `compile_ref_by_name` from root.
    pub handler: String,
    /// One entry per dispatch epoch: the handler's positional
    /// arguments in order, as `(driver-decl name, value)`.
    pub epochs: Vec<Vec<(String, Value)>>,
}

impl CallSpec {
    /// The argument declarations in positional order: name, graphix
    /// type name, canonical default. Derived from the first epoch
    /// (parse enforces every epoch carries the same name sequence).
    pub fn args(&self) -> Vec<(String, &'static str, Value)> {
        match self.epochs.first() {
            None => Vec::new(),
            Some(ep) => ep
                .iter()
                .map(|(name, v)| {
                    let (t, d) = canonical(v);
                    (name.clone(), t, d)
                })
                .collect(),
        }
    }

    /// The driver-side top-level declarations (the D4 contract, same
    /// as `Schedule::decls`) plus the in-language driver call. Placed
    /// AFTER the file-module `mod` declarations (the driver references
    /// into the handler's module).
    pub fn decls(&self) -> String {
        let mut s = String::new();
        let mut params = String::new();
        for (name, t, d) in self.args() {
            let lit = match d {
                Value::I64(_) => "i64:0",
                Value::F64(_) => "f64:0.0",
                Value::Bool(_) => "false",
                other => panic!("unsupported callable value kind {other:?}"),
            };
            s.push_str(&format!("let {name}: {t} = {lit};\n{name} <- never({lit});\n"));
            if !params.is_empty() {
                params.push_str(", ");
            }
            // `skip(1, …)` absorbs the decl's initial default, so the
            // in-language callsite dispatches ONLY on injections: in
            // the dispatch route (nothing injected) it never fires at
            // all, and the callable's instances are the first the
            // handler ever gets — the embedder geometry (a handler
            // nobody calls in-language), which is where the lazy-
            // instance reference bugs live.
            params.push_str(&format!("skip(#n: 1, {name})"));
        }
        s.push_str(&format!("let cdrv = {}({params});\n", self.handler));
        s
    }

    /// The one-line header.
    pub fn header(&self) -> String {
        let mut s = format!("{HEADER_PREFIX} handler={}", self.handler);
        for ep in &self.epochs {
            s.push(';');
            for (name, v) in ep.iter() {
                s.push(' ');
                s.push_str(name);
                s.push('=');
                s.push_str(&render_value(v));
            }
        }
        s
    }

    /// Assemble the wrapper: header + body (the body carries its own
    /// schedule header and file sections as usual — the callable line
    /// goes first so both scans find their header in the leading
    /// comment block).
    pub fn render(&self, body: &str) -> String {
        format!("{}\n{body}", self.header())
    }

    /// Split a wrapper into its callable spec and body. No header →
    /// `None` and the whole text. Same leading-comment-block scan as
    /// `Schedule::parse` (the two headers may appear in either order,
    /// each scan skipping the other as a comment). A malformed header
    /// is an error in every protocol — a generator or minimizer bug,
    /// never silently a comment.
    pub fn parse(text: &str) -> Result<(Option<CallSpec>, String), String> {
        let mut cursor = text;
        let (pre, line, rest) = loop {
            let t = cursor.trim_start_matches(['\n', ' ']);
            if t.starts_with(HEADER_PREFIX) {
                let pre_len = text.len() - t.len();
                break match t.split_once('\n') {
                    Some((l, r)) => (&text[..pre_len], l, r),
                    None => (&text[..pre_len], t, ""),
                };
            }
            if t.starts_with("//") {
                match t.split_once('\n') {
                    Some((_, r)) => {
                        cursor = r;
                        continue;
                    }
                    None => return Ok((None, text.to_string())),
                }
            }
            return Ok((None, text.to_string()));
        };
        let spec = &line[HEADER_PREFIX.len()..];
        let mut sections = spec.split(';');
        let head = sections.next().ok_or("empty callable header")?;
        let mut handler = None;
        for kv in head.split_whitespace() {
            let (k, v) =
                kv.split_once('=').ok_or_else(|| format!("bad callable key `{kv}`"))?;
            match k {
                "handler" => {
                    if !v.split("::").all(|part| {
                        !part.is_empty()
                            && part.chars().all(|c| c.is_alphanumeric() || c == '_')
                    }) {
                        return Err(format!("bad handler path `{v}`"));
                    }
                    handler = Some(v.to_string());
                }
                _ => return Err(format!("unknown callable key `{k}`")),
            }
        }
        let handler = handler.ok_or("callable header missing handler=")?;
        let mut epochs = Vec::new();
        let mut names: Option<Vec<(String, u8)>> = None;
        for sec in sections {
            let mut ep = Vec::new();
            for kv in sec.split_whitespace() {
                let (name, lit) = kv
                    .split_once('=')
                    .ok_or_else(|| format!("bad dispatch arg `{kv}`"))?;
                let v = parse_value(lit)?;
                ep.push((name.to_string(), v));
            }
            if ep.is_empty() {
                return Err("empty dispatch epoch".into());
            }
            let sig: Vec<(String, u8)> =
                ep.iter().map(|(n, v)| (n.clone(), value_kind(v))).collect();
            match &names {
                None => names = Some(sig),
                Some(first) if *first == sig => (),
                Some(_) => {
                    return Err("dispatch epochs disagree on argument names/types".into());
                }
            }
            epochs.push(ep);
        }
        if epochs.is_empty() {
            return Err("callable header has no dispatch epochs".into());
        }
        // The body is everything around the header line (provenance
        // comments above it stay, like the schedule scan).
        let body = format!("{pre}{rest}");
        Ok((Some(CallSpec { handler, epochs }), body))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip() {
        let c = CallSpec {
            handler: "m0::handler".into(),
            epochs: vec![
                vec![("cx0".into(), Value::I64(7)), ("cx1".into(), Value::Bool(true))],
                vec![("cx0".into(), Value::I64(-9)), ("cx1".into(), Value::Bool(false))],
            ],
        };
        let body = "{ m0::observe }\n// file-v1: m0.gx\nlet observe = 0";
        let text = c.render(body);
        let (c2, body2) = CallSpec::parse(&text).expect("parse");
        assert_eq!(c2.as_ref(), Some(&c));
        assert_eq!(body2.trim(), body);
        // decls carry every arg plus the driver call
        let d = c.decls();
        assert!(d.contains("let cx0: i64 = i64:0;"));
        assert!(d.contains("let cx1: bool = false;"));
        assert!(
            d.contains("let cdrv = m0::handler(skip(#n: 1, cx0), skip(#n: 1, cx1));")
        );
    }

    #[test]
    fn no_header_passes_through() {
        let (c, body) = CallSpec::parse("{ 1 + 1 }").expect("parse");
        assert!(c.is_none());
        assert_eq!(body, "{ 1 + 1 }");
    }

    #[test]
    fn header_below_schedule_line_is_found() {
        let text = "// schedule-v1: cap=64 events=512; in0=i64:1\n\
                    // callable-v1: handler=m0::h; cx0=i64:2\n\
                    { m0::observe }";
        let (c, body) = CallSpec::parse(text).expect("parse");
        assert!(c.is_some());
        assert!(body.contains("schedule-v1"), "schedule line must survive: {body}");
        assert!(!body.contains("callable-v1"));
    }

    #[test]
    fn mismatched_epoch_args_refused() {
        let text = "// callable-v1: handler=m0::h; cx0=i64:1; cx1=i64:2\n{ 0 }";
        assert!(CallSpec::parse(text).is_err());
    }
}
