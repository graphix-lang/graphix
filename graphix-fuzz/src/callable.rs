//! The `callable-v1` header: embedder-callable dispatch schedules.
//!
//! An embedder dispatch (`GXHandle::compile_callable` + `Callable::call`)
//! must be observationally the in-language call with the same arguments
//! arriving on its argument bindings. One text serves both routes; the
//! runner picks the route.
//!
//! ```text
//! // callable-v1: handler=m0::handler; cx0=i64:7; cx0=i64:9
//! { m0::observe }
//! // file-v1: m0.gx
//! let handler = |x: i64| -> null ...;
//! let observe = ...
//! ```
//!
//! Sections are `;`-separated: the handler's module path, then one
//! space-separated `name=value` set per dispatch epoch (every epoch
//! carries the same names; values use the schedule literal vocabulary).
//! The runner synthesizes the argument declarations and the in-language
//! driver call ([`CallSpec::decls`]). The handler must live in a
//! `file-v1` module so `compile_ref_by_name` reaches it from root.
//! Routes are compared on per-epoch final values, not cycle pacing.

use crate::schedule::{Lit, decl, parse_epoch, render_epochs, split_header};

pub const HEADER_PREFIX: &str = "// callable-v1:";

/// Whether `text`'s leading comment block holds a `callable-v1` header.
pub fn has_header(text: &str) -> bool {
    split_header(text, HEADER_PREFIX).is_some()
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallSpec {
    /// The handler binding's module path (e.g. `m0::handler`) — must
    /// be reachable by `compile_ref_by_name` from root.
    pub handler: String,
    /// One entry per dispatch epoch: the handler's positional
    /// arguments in order, as `(driver-decl name, value)`.
    pub epochs: Vec<Vec<(String, Lit)>>,
}

impl CallSpec {
    /// The argument declarations in positional order, each with its kind
    /// (from the first epoch; parse enforces that every epoch carries the
    /// same names and kinds).
    pub fn args(&self) -> Vec<(String, Lit)> {
        self.epochs.first().map(|ep| ep.clone()).unwrap_or_default()
    }

    /// The driver-side argument declarations plus the in-language driver
    /// call. Placed after the file-module `mod` declarations.
    pub fn decls(&self) -> String {
        let mut s = String::new();
        let mut params = Vec::new();
        for (name, kind) in self.args() {
            s.push_str(&decl(&name, kind));
            // `skip(1, …)` absorbs the decl's default so the in-language
            // callsite dispatches only on injections; in the dispatch route
            // it never fires and the callable's instances are the handler's first.
            params.push(format!("skip(#n: 1, {name})"));
        }
        s.push_str(&format!("let cdrv = {}({});\n", self.handler, params.join(", ")));
        s
    }

    /// The one-line header.
    pub fn header(&self) -> String {
        let mut s = format!("{HEADER_PREFIX} handler={}", self.handler);
        render_epochs(&mut s, &self.epochs);
        s
    }

    /// Header + body; either header order parses.
    pub fn render(&self, body: &str) -> String {
        format!("{}\n{body}", self.header())
    }

    /// Split a wrapper into its spec and body; no header → `None` and the
    /// whole text. The two headers may appear in either order. A malformed
    /// header is an error, never silently a comment.
    pub fn parse(text: &str) -> Result<(Option<CallSpec>, String), String> {
        let Some((line, body)) = split_header(text, HEADER_PREFIX) else {
            return Ok((None, text.to_string()));
        };
        let mut sections = line[HEADER_PREFIX.len()..].split(';');
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
        let mut epochs: Vec<Vec<(String, Lit)>> = Vec::new();
        for sec in sections {
            let ep = parse_epoch(sec)?;
            if let Some(first) = epochs.first()
                && (first.len() != ep.len()
                    || first
                        .iter()
                        .zip(&ep)
                        .any(|((n0, v0), (n, v))| n0 != n || !v0.same_kind(*v)))
            {
                return Err("dispatch epochs disagree on argument names/types".into());
            }
            epochs.push(ep);
        }
        if epochs.is_empty() {
            return Err("callable header has no dispatch epochs".into());
        }
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
                vec![("cx0".into(), Lit::I64(7)), ("cx1".into(), Lit::Bool(true))],
                vec![("cx0".into(), Lit::I64(-9)), ("cx1".into(), Lit::Bool(false))],
            ],
        };
        let body = "{ m0::observe }\n// file-v1: m0.gx\nlet observe = 0";
        let text = c.render(body);
        let (c2, body2) = CallSpec::parse(&text).expect("parse");
        assert_eq!(c2.as_ref(), Some(&c));
        assert_eq!(body2.trim(), body);
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
