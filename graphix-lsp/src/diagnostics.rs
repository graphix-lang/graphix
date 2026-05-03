//! Convert anyhow errors from the graphix compiler into LSP diagnostics.
//!
//! The compiler reports errors as anyhow chains, with position information
//! either embedded in the leaf parser-error message ("Parse error at line:
//! L, column: C") or in the displayed `ErrorContext`
//! ("at: line: L, column: C in file <path>, in: …"). We surface the deepest
//! position and source-file info we can find.

use crate::position::PositionEncoding;
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use std::path::PathBuf;

/// What the chain told us about the failure: a position (line/col) and
/// optionally the source file the error originated in.
#[derive(Debug, Clone, Default)]
pub struct ErrorLocation {
    pub position: Option<Position>,
    pub file: Option<PathBuf>,
}

/// Build a single LSP `Diagnostic` from an anyhow error. `text` is
/// the source text the error pertains to and is used to clamp
/// out-of-range positions back inside the document. `encoding`
/// controls how the (line, char-col) pair the error reports is
/// translated into the LSP `Position`'s `character` units.
pub fn error_to_diagnostics(
    err: &anyhow::Error,
    text: &str,
    encoding: PositionEncoding,
) -> Vec<Diagnostic> {
    let loc = error_location(err);
    let char_pos = loc.position.unwrap_or_default();
    let pos = clamp_position(char_pos, text);
    let pos = crate::position::char_col_to_position_in_text(
        text,
        pos.line,
        pos.character as usize,
        encoding,
    );
    vec![Diagnostic {
        range: Range { start: pos, end: pos },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("graphix".to_string()),
        message: error_leaf_message(err),
        ..Default::default()
    }]
}

/// Walk the error chain and pick the most specific position +
/// source file we can find. Inner ErrorContext wraps appear closer
/// to the leaf, so iterating from leaf outward gets the deepest
/// span first.
pub fn error_location(err: &anyhow::Error) -> ErrorLocation {
    let chain: Vec<String> = err.chain().map(|c| c.to_string()).collect();
    let mut loc = ErrorLocation::default();
    for s in chain.iter().rev() {
        if loc.file.is_none() {
            if let Some(p) = extract_file(s) {
                loc.file = Some(p);
            }
        }
        if loc.position.is_none() {
            if let Some(p) = extract_position(s) {
                loc.position = Some(p);
            }
        }
        if loc.file.is_some() && loc.position.is_some() {
            break;
        }
    }
    loc
}

/// Trim the chain's leaf entry to a clean inline message —
/// strip both the `at: …` position prefix and the trailing
/// `, in: <snippet>` for legibility.
pub fn error_leaf_message(err: &anyhow::Error) -> String {
    let chain: Vec<String> = err.chain().map(|c| c.to_string()).collect();
    let leaf = chain.last().cloned().unwrap_or_else(|| "error".into());
    strip_context_prefix(&leaf).to_string()
}

/// Extract `in file <path>` from an `at: … in file PATH, in: …`
/// chain entry, if present.
fn extract_file(s: &str) -> Option<PathBuf> {
    let i = s.find(" in file ")?;
    let rest = &s[i + " in file ".len()..];
    let end = rest.find(", in: ").unwrap_or(rest.len());
    let path = rest[..end].trim();
    if path.is_empty() {
        None
    } else {
        Some(PathBuf::from(path))
    }
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

    #[test]
    fn parses_context_pos_with_file() {
        let p = parse_context_pos(
            "at: line: 12, column: 4 in file /tmp/foo.gx, in: let x = ...",
        )
        .unwrap();
        assert_eq!(p, Position { line: 11, character: 3 });
    }

    #[test]
    fn extracts_file_from_chain_entry() {
        let s = "at: line: 12, column: 4 in file /tmp/foo.gx, in: let x = ...";
        assert_eq!(extract_file(s), Some(PathBuf::from("/tmp/foo.gx")));
    }
}
