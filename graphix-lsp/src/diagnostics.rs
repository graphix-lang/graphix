//! Convert anyhow errors from the graphix compiler into LSP diagnostics.
//!
//! The compiler reports errors as anyhow chains, with position information
//! either embedded in the leaf parser-error message ("Parse error at line:
//! L, column: C") or in the displayed `ErrorContext` ("at: L:C, in: …").
//! We surface the deepest position info we can find.

use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

pub fn error_to_diagnostics(err: &anyhow::Error, text: &str) -> Vec<Diagnostic> {
    let chain: Vec<String> = err.chain().map(|c| c.to_string()).collect();
    // Innermost cause makes the best inline message — that's the actual
    // type/parse error rather than a "while compiling …" wrapper.
    let leaf = chain.last().cloned().unwrap_or_else(|| "error".into());
    // Pick the deepest position info we can find. ErrorContext wraps appear
    // closer to the leaf so iterating from leaf outward finds the most
    // specific span first.
    let pos = chain
        .iter()
        .rev()
        .find_map(|s| extract_position(s))
        .unwrap_or(Position { line: 0, character: 0 });
    let pos = clamp_position(pos, text);
    let range = Range { start: pos, end: pos };
    // Strip the position prefix from `at: line: L, column: C, in: …` so
    // the message itself doesn't repeat what the range already encodes.
    let message = strip_context_prefix(&leaf).to_string();
    vec![Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("graphix".to_string()),
        message,
        ..Default::default()
    }]
}

fn strip_context_prefix(s: &str) -> &str {
    if let Some(rest) = s.strip_prefix("at: ") {
        // "at: line: L, column: C, in: …" → "…" (the `in:` payload)
        if let Some(idx) = rest.find(", in: ") {
            return &rest[idx + ", in: ".len()..];
        }
    }
    s
}

/// Try to extract `line:column` (1-based) from a single chain entry.
fn extract_position(s: &str) -> Option<Position> {
    if let Some(p) = parse_combine_error_pos(s) {
        return Some(p);
    }
    if let Some(p) = parse_context_pos(s) {
        return Some(p);
    }
    None
}

/// Match combine's parse error: `Parse error at line: L, column: C`
/// or compile errors via `ErrorContext`: `at: line: L, column: C, in: …`.
/// SourcePosition formats as `line: L, column: C` so both forms share the
/// same suffix.
fn parse_combine_error_pos(s: &str) -> Option<Position> {
    let i = s.find("Parse error at ")?;
    parse_line_col_at(&s[i + "Parse error at ".len()..])
}

fn parse_context_pos(s: &str) -> Option<Position> {
    let i = s.find("at: ")?;
    parse_line_col_at(&s[i + "at: ".len()..])
}

fn parse_line_col_at(s: &str) -> Option<Position> {
    let s = s.strip_prefix("line: ")?;
    let (line_s, rest) = s.split_once(',')?;
    let line: u32 = line_s.trim().parse().ok()?;
    let col_marker = rest.find("column: ")?;
    let col_s = &rest[col_marker + "column: ".len()..];
    let end = col_s.find(|c: char| !c.is_ascii_digit()).unwrap_or(col_s.len());
    let col: u32 = col_s[..end].parse().ok()?;
    Some(Position { line: line.saturating_sub(1), character: col.saturating_sub(1) })
}

fn clamp_position(p: Position, text: &str) -> Position {
    let lines: Vec<&str> = text.lines().collect();
    if lines.is_empty() {
        return Position { line: 0, character: 0 };
    }
    let line = (p.line as usize).min(lines.len() - 1);
    let line_len = lines[line].chars().count() as u32;
    Position {
        line: line as u32,
        character: p.character.min(line_len),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_combine_error() {
        let p = parse_combine_error_pos("Parse error at line: 3, column: 7\nfoo").unwrap();
        assert_eq!(p, Position { line: 2, character: 6 });
    }

    #[test]
    fn parses_context_pos() {
        let p =
            parse_context_pos("at: line: 12, column: 4, in: let x = ...").unwrap();
        assert_eq!(p, Position { line: 11, character: 3 });
    }
}
