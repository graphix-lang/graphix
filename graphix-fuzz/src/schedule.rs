//! Injection schedules — the multi-epoch input plan a reactive program
//! is driven with, and the ONE place its text format lives.
//!
//! A scheduled program is a single text artifact — the wrapper — that
//! flows unchanged through every protocol (check / minimize / regress /
//! corpus / the isolated-child stdin): an optional one-line header
//! followed by the program body.
//!
//! ```text
//! // schedule-v1: cap=64 events=512; in0=i64:3 in1=f64:1.5; in0=i64:4
//! { let acc = 0; acc <- in0 ~ (acc + in0); acc }
//! ```
//!
//! Sections are `;`-separated: the caps first (the runtime trace
//! budgets — schedule DATA, so both evaluation modes run under
//! identical budgets and a cap mismatch is a real divergence), then one
//! section per epoch, each a space-separated set of simultaneous
//! `name=type:literal` injections. A wrapper with no header is a
//! single-burst program (empty schedule, default caps) — every
//! pre-schedule finding parses unchanged.
//!
//! The oracle owns parse AND render; the generator constructs
//! [`Schedule`] values and renders through [`Schedule::render`] — the
//! round-trip test pins the format so the two can't drift.
//!
//! Injected inputs are declared by the DRIVER at the top level of the
//! compile text (`let in0: i64 = 0; in0 <- never(0);` — the D4
//! contract: the `<-` marks the binding unstable so fusion binds a
//! kernel input instead of const-folding the default). Top level, NOT
//! inside the `{ mod test; … }` wrap: a Do block scopes under an
//! anonymous `do<ExprId>` path, so module-internal names aren't
//! reachable by `compile_ref_by_name` from root — root-level decls
//! are, and the module body sees them lexically. Defaults are
//! type-canonical (0 / 0.0 / false): epoch 0 observes the default,
//! injections start at epoch 1.

use netidx::publisher::Value;

use crate::trace;

pub const HEADER_PREFIX: &str = "// schedule-v1:";

#[derive(Debug, Clone, PartialEq)]
pub struct Schedule {
    /// One entry per injection epoch (after the compile burst): the
    /// simultaneous `(input name, value)` set delivered before that
    /// epoch's quiescence wait.
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

    /// The driver-side top-level input declarations (the D4 contract).
    /// The default literals are graphix SOURCE (unlike the header's
    /// `render_value` forms, which only schedule.rs ever parses).
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
    /// empty schedule and the whole text. A malformed header is an
    /// error (a generator or minimizer bug — never silently a comment).
    ///
    /// The header may sit BELOW other leading `//` comment lines (a
    /// pinned finding's provenance block) — the scan walks the leading
    /// comment block and stops at the first non-comment line. When the
    /// header is found mid-block, the returned body starts AFTER it
    /// (the provenance comments above it stay in the file, not the
    /// compiled body). Previously only line 1 was checked, so a
    /// finding file with its schedule below the `// bisect:` header ran
    /// with ZERO injections — a vacuous re-check (soak jul04 item 10).
    pub fn parse(text: &str) -> Result<(Schedule, &str), String> {
        let mut cursor = text;
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
            rest,
        ))
    }
}

/// The v1 injectable scalar set: i64, f64, bool. Rendering must
/// round-trip exactly (Rust's shortest-roundtrip float formatting;
/// `NaN`/`inf` parse back via `f64::from_str`).
fn render_value(v: &Value) -> String {
    match v {
        Value::I64(n) => format!("i64:{n}"),
        Value::F64(f) => format!("f64:{f}"),
        Value::Bool(b) => format!("bool:{b}"),
        other => panic!("unsupported schedule value kind {other:?}"),
    }
}

fn parse_value(lit: &str) -> Result<Value, String> {
    let (t, l) = lit.split_once(':').ok_or_else(|| format!("bad literal `{lit}`"))?;
    match t {
        "i64" => l.parse().map(Value::I64).map_err(|e| format!("bad i64 `{l}`: {e}")),
        "f64" => l.parse().map(Value::F64).map_err(|e| format!("bad f64 `{l}`: {e}")),
        "bool" => l.parse().map(Value::Bool).map_err(|e| format!("bad bool `{l}`: {e}")),
        _ => Err(format!("unsupported schedule type `{t}`")),
    }
}

fn value_kind(v: &Value) -> u8 {
    match v {
        Value::I64(_) => 0,
        Value::F64(_) => 1,
        Value::Bool(_) => 2,
        _ => u8::MAX,
    }
}

fn canonical(v: &Value) -> (&'static str, Value) {
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
