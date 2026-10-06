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

#[derive(Debug, Clone, PartialEq)]
pub struct Schedule {
    /// One entry per injection epoch (after the compile burst): the
    /// simultaneous `(input name, value)` set delivered before that
    /// epoch's quiescence wait.
    // CR claude for eric: [structure] Schedule and CallSpec hold injections as netidx
    // Values, although only i64, f64 and bool are legal. So other kinds are
    // representable and are answered by panics (render_value, canonical,
    // Schedule::decls, CallSpec::decls), and value_kind encodes the kind as magic u8s.
    // The decl text and the per-epoch header loop are copied verbatim between Schedule
    // and CallSpec, and the leading-comment header scan is written three times
    // (has_header, CallSpec::parse, Schedule::parse). An `enum Lit { I64(i64),
    // F64(f64), Bool(bool) }` with its type name, default literal, render and parse
    // would remove value_kind, canonical and the panics, and one decl helper plus one
    // section renderer would serve both specs. (fuzz-main-aux-16)
    pub epochs: Vec<Vec<(String, Value)>>,
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
    /// with the graphix type name of its (consistent) literal kind and
    /// the type-canonical default the driver declares it with.
    pub fn inputs(&self) -> Vec<(String, &'static str, Value)> {
        let mut out: Vec<(String, &'static str, Value)> = Vec::new();
        for ep in &self.epochs {
            for (name, v) in ep {
                if !out.iter().any(|(n, _, _)| n == name) {
                    let (t, d) = canonical(v);
                    out.push((name.clone(), t, d));
                }
            }
        }
        out
    }

    /// The driver-side top-level input declarations. The defaults are
    /// graphix source, unlike the header's `render_value` forms.
    pub fn decls(&self) -> String {
        let mut s = String::new();
        for (name, t, d) in self.inputs() {
            let lit = match d {
                Value::I64(_) => "i64:0",
                Value::F64(_) => "f64:0.0",
                Value::Bool(_) => "false",
                other => panic!("unsupported schedule value kind {other:?}"),
            };
            s.push_str(&format!("let {name}: {t} = {lit};\n{name} <- never({lit});\n"));
        }
        s
    }

    /// The one-line header, or `None` when the schedule is empty with
    /// default caps (a single-burst wrapper needs no header).
    pub fn header(&self) -> Option<String> {
        if self.is_empty() {
            return None;
        }
        let mut s =
            format!("{HEADER_PREFIX} cap={} events={}", self.max_cycles, self.max_events);
        for ep in &self.epochs {
            s.push(';');
            for (name, v) in ep.iter() {
                s.push(' ');
                s.push_str(name);
                s.push('=');
                s.push_str(&render_value(v));
            }
        }
        Some(s)
    }

    /// Assemble the wrapper artifact: header line (when any) + body.
    pub fn render(&self, body: &str) -> String {
        match self.header() {
            None => body.to_string(),
            Some(h) => format!("{h}\n{body}"),
        }
    }

    /// Split a wrapper into its schedule and body. No header → the
    /// empty schedule and the whole text. The header may sit below other
    /// leading `//` lines; the body returned starts after it. A malformed
    /// header is an error, never silently a comment.
    pub fn parse(text: &str) -> Result<(Schedule, &str), String> {
        let mut cursor = text;
        // CR claude for eric: [bug] This loop skips leading `//` lines to reach the
        // schedule header, and the parse returns only the text after that header, so a
        // `// callable-v1:` line above it is dropped. CallSpec::render writes exactly
        // that order, and so do mutate_wrapper, minimize's reattach and typemorph's
        // compose. Subject::parse runs this parse first, so the subject loses its
        // CallSpec while callable::has_header still selects the Dispatch route: no
        // dispatch runs and every pair agrees. The minimizer rewrites a schedule-first
        // program into this order, then deletes the handler's module because its
        // candidates no longer need it. Keep the skipped lines in the returned body as
        // CallSpec::parse keeps `pre`, or give the three scans one shared header
        // splitter. probe: design/review-2026-10-05/repro/fuzz-main-aux-08.gx
        // (graphix-fuzz check: AGREE; with its two header lines swapped: DIVERGENCE).
        // (fuzz-main-aux-08)
        let (line, rest) = loop {
            let t = cursor.trim_start_matches(['\n', ' ']);
            if t.starts_with(HEADER_PREFIX) {
                break match t.split_once('\n') {
                    Some((l, r)) => (l, r),
                    None => (t, ""),
                };
            }
            if t.starts_with("//") {
                match t.split_once('\n') {
                    Some((_, r)) => {
                        cursor = r;
                        continue;
                    }
                    None => return Ok((Schedule::default(), text)),
                }
            }
            return Ok((Schedule::default(), text));
        };
        let spec = &line[HEADER_PREFIX.len()..];
        let mut sections = spec.split(';');
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
        let mut types: Vec<(String, u8)> = Vec::new();
        for sec in sections {
            let mut ep = Vec::new();
            for kv in sec.split_whitespace() {
                let (name, lit) =
                    kv.split_once('=').ok_or_else(|| format!("bad injection `{kv}`"))?;
                let v = parse_value(lit)?;
                let kind = value_kind(&v);
                match types.iter().find(|(n, _)| n == name) {
                    None => types.push((name.to_string(), kind)),
                    Some((_, k)) if *k == kind => (),
                    Some(_) => {
                        return Err(format!("input `{name}` changes type across epochs"));
                    }
                }
                ep.push((name.to_string(), v));
            }
            if ep.is_empty() {
                return Err("empty epoch section".into());
            }
            epochs.push(ep);
        }
        Ok((
            Schedule {
                epochs,
                max_cycles: max_cycles.unwrap_or(trace::MAX_CYCLES),
                max_events: max_events.unwrap_or(trace::MAX_EVENTS),
            },
            // CR claude for eric: [bug] This returns only the text after the schedule
            // header, so every `//` line the scan skipped above it is dropped, a `//
            // callable-v1:` header included (CallSpec::parse keeps its `pre`, this does
            // not). CallSpec::render documents the callable line first and either order
            // as fine, and minimize's `reattach` (lib.rs:2458), mutate_wrapper
            // (mutate.rs:345) and typemorph_subject (lib.rs:2876) all emit callable
            // first. Subject::parse then finds no CallSpec, so nothing is dispatched on
            // either route, while `has_header` still sends the program through
            // check_callable, which agrees vacuously. A pin with both headers in that
            // order is silently vacuous, and minimizing one written schedule-first
            // returns a program that no longer diverges. Keep the skipped lines in the
            // returned body (or render the schedule first everywhere and fix the docs),
            // and test both orders through Subject::parse. probe:
            // design/review-2026-10-05/repro/fuzz-lib-b-06.sh (check reports DIVERGENCE
            // with the schedule line first and AGREE with the two lines swapped;
            // minimize with budget 1 prints "no divergence to minimize").
            // (fuzz-lib-b-06)
            rest,
        ))
    }
}

/// The injectable scalar set: i64, f64, bool. Rendering round-trips
/// exactly, `NaN`/`inf` included.
pub(crate) fn render_value(v: &Value) -> String {
    match v {
        Value::I64(n) => format!("i64:{n}"),
        Value::F64(f) => format!("f64:{f}"),
        Value::Bool(b) => format!("bool:{b}"),
        other => panic!("unsupported schedule value kind {other:?}"),
    }
}

pub(crate) fn parse_value(lit: &str) -> Result<Value, String> {
    let (t, l) = lit.split_once(':').ok_or_else(|| format!("bad literal `{lit}`"))?;
    match t {
        "i64" => l.parse().map(Value::I64).map_err(|e| format!("bad i64 `{l}`: {e}")),
        "f64" => l.parse().map(Value::F64).map_err(|e| format!("bad f64 `{l}`: {e}")),
        "bool" => l.parse().map(Value::Bool).map_err(|e| format!("bad bool `{l}`: {e}")),
        _ => Err(format!("unsupported schedule type `{t}`")),
    }
}

pub(crate) fn value_kind(v: &Value) -> u8 {
    match v {
        Value::I64(_) => 0,
        Value::F64(_) => 1,
        Value::Bool(_) => 2,
        _ => u8::MAX,
    }
}

pub(crate) fn canonical(v: &Value) -> (&'static str, Value) {
    match v {
        Value::I64(_) => ("i64", Value::I64(0)),
        Value::F64(_) => ("f64", Value::F64(0.0)),
        Value::Bool(_) => ("bool", Value::Bool(false)),
        other => panic!("unsupported schedule value kind {other:?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip() {
        let s = Schedule {
            epochs: vec![
                vec![("in0".into(), Value::I64(3)), ("in1".into(), Value::F64(1.5))],
                vec![("in0".into(), Value::I64(-4))],
                vec![
                    ("in1".into(), Value::F64(f64::NAN)),
                    ("in2".into(), Value::Bool(true)),
                ],
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
        assert_eq!(s.render(body), s2.render(body2));
    }

    #[test]
    fn headerless_is_empty() {
        let text = "{ let x = i64:5; x * i64:3 }";
        let (s, body) = Schedule::parse(text).expect("parse");
        assert!(s.is_empty());
        assert_eq!(body, text);
        assert_eq!(s.render(body), text);
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
