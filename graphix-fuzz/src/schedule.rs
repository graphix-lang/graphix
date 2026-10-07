//! Injection schedules: the `schedule-v1` header that drives a
//! reactive program through multiple input epochs.
//!
//! ```text
//! // schedule-v1: cap=64 events=512; in0=i64:3 in1=f64:1.5; in0=i64:4
//! { let acc = 0; acc <- in0 ~ (acc + in0); acc }
//! ```
//!
//! Sections are `;`-separated: the trace caps first (schedule data, so
//! every mode runs under identical budgets), then one section per epoch
//! of simultaneous `name=type:literal` injections. No header means a
//! single-burst program with default caps. The driver declares each
//! input at the compile-text top level as `let in0: i64 = 0; in0 <-
//! never(0);` (the `<-` keeps the binding unstable so fusion binds a
//! kernel input); epoch 0 observes the canonical default.

use netidx::publisher::Value;

use crate::trace;

pub const HEADER_PREFIX: &str = "// schedule-v1:";

/// An injectable literal: the scalar kinds a schedule or a dispatch
/// carries. Its header form round-trips exactly, `NaN`/`inf` included.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Lit {
    I64(i64),
    F64(f64),
    Bool(bool),
}

impl Lit {
    pub fn type_name(self) -> &'static str {
        match self {
            Lit::I64(_) => "i64",
            Lit::F64(_) => "f64",
            Lit::Bool(_) => "bool",
        }
    }

    /// The graphix source the driver declares an input of this kind with.
    pub fn default_src(self) -> &'static str {
        match self {
            Lit::I64(_) => "i64:0",
            Lit::F64(_) => "f64:0.0",
            Lit::Bool(_) => "false",
        }
    }

    /// The value the runtime injects.
    pub fn value(self) -> Value {
        match self {
            Lit::I64(n) => Value::I64(n),
            Lit::F64(f) => Value::F64(f),
            Lit::Bool(b) => Value::Bool(b),
        }
    }

    pub fn same_kind(self, other: Lit) -> bool {
        std::mem::discriminant(&self) == std::mem::discriminant(&other)
    }

    pub fn render(self) -> String {
        format!(
            "{}:{}",
            self.type_name(),
            match self {
                Lit::I64(n) => n.to_string(),
                Lit::F64(f) => f.to_string(),
                Lit::Bool(b) => b.to_string(),
            }
        )
    }

    pub fn parse(lit: &str) -> Result<Lit, String> {
        let (t, l) = lit.split_once(':').ok_or_else(|| format!("bad literal `{lit}`"))?;
        match t {
            "i64" => l.parse().map(Lit::I64).map_err(|e| format!("bad i64 `{l}`: {e}")),
            "f64" => l.parse().map(Lit::F64).map_err(|e| format!("bad f64 `{l}`: {e}")),
            "bool" => {
                l.parse().map(Lit::Bool).map_err(|e| format!("bad bool `{l}`: {e}"))
            }
            _ => Err(format!("unsupported schedule type `{t}`")),
        }
    }
}

/// A driver input's declaration: its default, and a `<-` that keeps the
/// binding unstable so fusion binds a kernel input.
pub(crate) fn decl(name: &str, kind: Lit) -> String {
    let (t, d) = (kind.type_name(), kind.default_src());
    format!("let {name}: {t} = {d};\n{name} <- never({d});\n")
}

/// One `; name=lit ..` section per epoch.
pub(crate) fn render_epochs(out: &mut String, epochs: &[Vec<(String, Lit)>]) {
    for ep in epochs {
        out.push(';');
        for (name, v) in ep {
            out.push(' ');
            out.push_str(name);
            out.push('=');
            out.push_str(&v.render());
        }
    }
}

/// One `name=lit ..` epoch section.
pub(crate) fn parse_epoch(sec: &str) -> Result<Vec<(String, Lit)>, String> {
    let mut ep = Vec::new();
    for kv in sec.split_whitespace() {
        let (name, lit) =
            kv.split_once('=').ok_or_else(|| format!("bad injection `{kv}`"))?;
        ep.push((name.to_string(), Lit::parse(lit)?));
    }
    if ep.is_empty() {
        return Err("empty epoch section".into());
    }
    Ok(ep)
}

/// Find the line starting with `prefix` in `text`'s leading comment block:
/// the header line and the text without it, every other line kept.
pub(crate) fn split_header<'a>(text: &'a str, prefix: &str) -> Option<(&'a str, String)> {
    let mut cursor = text;
    loop {
        let t = cursor.trim_start_matches(['\n', ' ']);
        if !t.starts_with("//") {
            return None;
        }
        let (line, rest) = t.split_once('\n').unwrap_or((t, ""));
        if t.starts_with(prefix) {
            let before = &text[..text.len() - t.len()];
            return Some((line, format!("{before}{rest}")));
        }
        if rest.is_empty() {
            return None;
        }
        cursor = rest;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Schedule {
    /// One entry per injection epoch (after the compile burst): the
    /// simultaneous `(input name, value)` set delivered before that
    /// epoch's quiescence wait.
    pub epochs: Vec<Vec<(String, Lit)>>,
    /// Per-segment active-cycle budget (see `GXHandle::trace_start`).
    pub max_cycles: u64,
    /// Total trace event budget.
    pub max_events: usize,
}

impl Default for Schedule {
    fn default() -> Self {
        Schedule {
            epochs: Vec::new(),
            max_cycles: trace::MAX_CYCLES,
            max_events: trace::MAX_EVENTS,
        }
    }
}

impl Schedule {
    pub fn is_empty(&self) -> bool {
        self.epochs.is_empty()
            && self.max_cycles == trace::MAX_CYCLES
            && self.max_events == trace::MAX_EVENTS
    }

    /// The unique injected input names in first-appearance order, each
    /// with the kind of its (consistent) literals.
    pub fn inputs(&self) -> Vec<(String, Lit)> {
        let mut out: Vec<(String, Lit)> = Vec::new();
        for ep in &self.epochs {
            for (name, v) in ep {
                if !out.iter().any(|(n, _)| n == name) {
                    out.push((name.clone(), *v));
                }
            }
        }
        out
    }

    /// The driver-side top-level input declarations.
    pub fn decls(&self) -> String {
        self.inputs().into_iter().map(|(name, kind)| decl(&name, kind)).collect()
    }

    /// The one-line header, or `None` when the schedule is empty with
    /// default caps (a single-burst wrapper needs no header).
    pub fn header(&self) -> Option<String> {
        if self.is_empty() {
            return None;
        }
        let mut s =
            format!("{HEADER_PREFIX} cap={} events={}", self.max_cycles, self.max_events);
        render_epochs(&mut s, &self.epochs);
        Some(s)
    }

    /// Assemble the wrapper artifact: header line (when any) + body.
    pub fn render(&self, body: &str) -> String {
        match self.header() {
            None => body.to_string(),
            Some(h) => format!("{h}\n{body}"),
        }
    }

    /// Split a wrapper into its schedule and the text without the
    /// header line. No header → the empty schedule and the whole text.
    /// The header may sit anywhere in the leading `//` block, whose other
    /// lines (a `callable-v1` header among them) are kept. A malformed
    /// header is an error, never silently a comment.
    pub fn parse(text: &str) -> Result<(Schedule, String), String> {
        let Some((line, body)) = split_header(text, HEADER_PREFIX) else {
            return Ok((Schedule::default(), text.to_string()));
        };
        let mut sections = line[HEADER_PREFIX.len()..].split(';');
        let caps = sections.next().ok_or("empty header")?;
        let mut max_cycles = None;
        let mut max_events = None;
        for kv in caps.split_whitespace() {
            let (k, v) = kv.split_once('=').ok_or_else(|| format!("bad cap `{kv}`"))?;
            match k {
                "cap" => {
                    max_cycles =
                        Some(v.parse().map_err(|e| format!("bad cap `{v}`: {e}"))?)
                }
                "events" => {
                    max_events =
                        Some(v.parse().map_err(|e| format!("bad events `{v}`: {e}"))?)
                }
                _ => return Err(format!("unknown cap key `{k}`")),
            }
        }
        let mut epochs = Vec::new();
        let mut kinds: Vec<(String, Lit)> = Vec::new();
        for sec in sections {
            let ep = parse_epoch(sec)?;
            for (name, v) in &ep {
                match kinds.iter().find(|(n, _)| n == name) {
                    None => kinds.push((name.clone(), *v)),
                    Some((_, k)) if k.same_kind(*v) => (),
                    Some(_) => {
                        return Err(format!("input `{name}` changes type across epochs"));
                    }
                }
            }
            epochs.push(ep);
        }
        let sched = Schedule {
            epochs,
            max_cycles: max_cycles.unwrap_or(trace::MAX_CYCLES),
            max_events: max_events.unwrap_or(trace::MAX_EVENTS),
        };
        Ok((sched, body))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip() {
        let s = Schedule {
            epochs: vec![
                vec![("in0".into(), Lit::I64(3)), ("in1".into(), Lit::F64(1.5))],
                vec![("in0".into(), Lit::I64(-4))],
                vec![("in1".into(), Lit::F64(f64::NAN)), ("in2".into(), Lit::Bool(true))],
            ],
            max_cycles: 32,
            max_events: 256,
        };
        let body = "{ let acc = 0; acc <- in0 ~ (acc + in0); acc }";
        let text = s.render(body);
        let (s2, body2) = Schedule::parse(&text).expect("parse");
        assert_eq!(body2.trim(), body);
        assert_eq!(s2.max_cycles, 32);
        assert_eq!(s2.max_events, 256);
        assert_eq!(s2.epochs.len(), 3);
        // NaN != NaN under IEEE; compare rendered forms instead.
        assert_eq!(s.render(body), s2.render(&body2));
    }

    #[test]
    fn other_header_lines_survive_either_order() {
        let callable = "// callable-v1: handler=m0::h; cx0=i64:2";
        let schedule = "// schedule-v1: cap=64 events=512; in0=i64:1";
        for text in [
            format!("{callable}\n{schedule}\n{{ m0::observe }}"),
            format!("{schedule}\n{callable}\n{{ m0::observe }}"),
        ] {
            let (s, body) = Schedule::parse(&text).expect("parse");
            assert_eq!(s.epochs.len(), 1);
            assert!(body.contains(callable), "the callable line is kept: {body}");
            let (c, body) = crate::callable::CallSpec::parse(&body).expect("parse");
            assert!(c.is_some(), "{text}");
            assert_eq!(body.trim(), "{ m0::observe }");
        }
    }

    #[test]
    fn headerless_is_empty() {
        let text = "{ let x = i64:5; x * i64:3 }";
        let (s, body) = Schedule::parse(text).expect("parse");
        assert!(s.is_empty());
        assert_eq!(body, text);
        assert_eq!(s.render(&body), text);
        // Leading ordinary comments are NOT headers.
        let with_comment = "// minimized:\n{ i64:1 }";
        let (s, body) = Schedule::parse(with_comment).expect("parse");
        assert!(s.is_empty());
        assert_eq!(body, with_comment);
    }

    #[test]
    fn decls_follow_first_appearance() {
        let (s, _) = Schedule::parse(
            "// schedule-v1: cap=8 events=64; b=f64:2.5 a=i64:1; a=i64:2\nbody",
        )
        .expect("parse");
        let d = s.decls();
        assert_eq!(
            d,
            "let b: f64 = f64:0.0;\nb <- never(f64:0.0);\n\
             let a: i64 = i64:0;\na <- never(i64:0);\n"
        );
    }

    #[test]
    fn rejects() {
        assert!(Schedule::parse("// schedule-v1: cap=8; \nbody").is_err());
        assert!(
            Schedule::parse("// schedule-v1: cap=8 events=1; in0=q:1\nbody").is_err()
        );
        assert!(
            Schedule::parse("// schedule-v1: cap=8 events=1; in0=i64:1; in0=f64:1\nbody")
                .is_err()
        );
        assert!(Schedule::parse("// schedule-v1: zap=8\nbody").is_err());
    }
}
