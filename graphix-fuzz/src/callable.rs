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

use netidx::publisher::Value;

use crate::schedule::{canonical, parse_value, render_value, value_kind};

pub const HEADER_PREFIX: &str = "// callable-v1:";

/// Detect a header without a full parse; the same leading-comment-block
/// scan as [`CallSpec::parse`].
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
    /// The argument declarations in positional order: name, graphix type
    /// name, canonical default (from the first epoch; parse enforces that
    /// every epoch carries the same names).
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

    /// The driver-side argument declarations plus the in-language driver
    /// call. Placed after the file-module `mod` declarations.
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
            // `skip(1, …)` absorbs the decl's default so the in-language
            // callsite dispatches only on injections; in the dispatch route
            // it never fires and the callable's instances are the handler's first.
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

    /// Header + body. The callable line goes first so both header scans
    /// find theirs in the leading comment block.
    pub fn render(&self, body: &str) -> String {
        format!("{}\n{body}", self.header())
    }

    /// Split a wrapper into its spec and body; no header → `None` and the
    /// whole text. The two headers may appear in either order. A malformed
    /// header is an error, never silently a comment.
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
