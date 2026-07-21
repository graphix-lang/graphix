//! LSP `Position` ↔ char-column conversion under different position
//! encodings.
//!
//! LSP positions are per-line; the `character` field's units depend on
//! the encoding the client and server negotiated:
//!
//! - **UTF-16** (the default): one unit per UTF-16 code unit. BMP
//!   characters are 1 unit; supplementary-plane characters (most emoji,
//!   `𝒜`, etc.) are 2 units.
//! - **UTF-32**: one unit per Unicode scalar (Rust `char`). Equivalent
//!   to counting `chars()`.
//! - **UTF-8**: one unit per UTF-8 byte. Equivalent to byte offsets.
//!
//! The graphix compiler (and our cursor helpers) want char-column
//! offsets — `combine`'s `SourcePosition::column` advances per char.
//! These helpers translate at the LSP boundary so the rest of the code
//! can speak chars uniformly.
//!
//! For ASCII-only lines (the common case in graphix source) all three
//! encodings agree, so this is a no-op fast path most of the time.

use lsp_types::{Position, PositionEncodingKind};

/// Position encoding negotiated with the client. We narrow
/// `lsp_types::PositionEncodingKind` to the three variants the spec
/// defines, so we can match exhaustively.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionEncoding {
    Utf8,
    Utf16,
    Utf32,
}

impl PositionEncoding {
    /// Map the negotiated `PositionEncodingKind` (or its absence,
    /// meaning the LSP default of UTF-16) into our enum. Anything we
    /// don't recognize falls back to UTF-16 — that's the spec's
    /// default and what every conforming client supports.
    pub fn from_kind(kind: Option<&PositionEncodingKind>) -> Self {
        match kind {
            Some(k) if *k == PositionEncodingKind::UTF8 => Self::Utf8,
            Some(k) if *k == PositionEncodingKind::UTF32 => Self::Utf32,
            _ => Self::Utf16,
        }
    }
}

/// Convert an LSP `Position` to a char-column offset within its line.
/// `line_text` is the line at `position.line`, with the line terminator
/// already stripped (i.e. the same string `str::lines()` yields).
///
/// Out-of-range character offsets clamp to the end of the line — the
/// LSP spec mandates this, otherwise an editor sending a position past
/// EOL (e.g. while the user is typing) would surface as a hard error.
pub fn position_to_char_col(
    line_text: &str,
    position: Position,
    encoding: PositionEncoding,
) -> usize {
    let target = position.character as u32;
    if target == 0 {
        return 0;
    }
    match encoding {
        PositionEncoding::Utf32 => line_text.chars().take(target as usize).count(),
        PositionEncoding::Utf16 => {
            let mut units = 0u32;
            let mut chars = 0usize;
            for c in line_text.chars() {
                let next = units + c.len_utf16() as u32;
                if next > target {
                    break;
                }
                units = next;
                chars += 1;
                if units == target {
                    break;
                }
            }
            chars
        }
        PositionEncoding::Utf8 => {
            let mut bytes = 0u32;
            let mut chars = 0usize;
            for c in line_text.chars() {
                let next = bytes + c.len_utf8() as u32;
                if next > target {
                    break;
                }
                bytes = next;
                chars += 1;
                if bytes == target {
                    break;
                }
            }
            chars
        }
    }
}

/// Build an LSP `Position` from a (line, char-column) pair. Inverse of
/// `position_to_char_col`. `char_col` is clamped to the line length so
/// callers don't have to worry about whether the compiler reported a
/// column past EOL.
pub fn char_col_to_position(
    line_text: &str,
    line: u32,
    char_col: usize,
    encoding: PositionEncoding,
) -> Position {
    let character = match encoding {
        PositionEncoding::Utf32 => char_col.min(line_text.chars().count()) as u32,
        PositionEncoding::Utf16 => {
            line_text.chars().take(char_col).map(|c| c.len_utf16() as u32).sum()
        }
        PositionEncoding::Utf8 => {
            line_text.chars().take(char_col).map(|c| c.len_utf8() as u32).sum()
        }
    };
    Position { line, character }
}

/// Helper for the common case: get the line at `position.line` from
/// the full document text and convert the position to a char-column.
/// Returns `None` if the line index is out of range.
pub fn position_to_char_col_in_text(
    text: &str,
    position: Position,
    encoding: PositionEncoding,
) -> Option<usize> {
    let line = text.lines().nth(position.line as usize)?;
    Some(position_to_char_col(line, position, encoding))
}

/// Helper for `char_col_to_position` taking the full document text.
/// Returns a position with `character: 0` if the line index is out of
/// range — keeps the LSP response well-formed in degenerate cases.
pub fn char_col_to_position_in_text(
    text: &str,
    line: u32,
    char_col: usize,
    encoding: PositionEncoding,
) -> Position {
    let line_text = text.lines().nth(line as usize).unwrap_or("");
    char_col_to_position(line_text, line, char_col, encoding)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn ascii_all_encodings_equivalent() {
        let line = "let x = 1";
        for enc in
            [PositionEncoding::Utf8, PositionEncoding::Utf16, PositionEncoding::Utf32]
        {
            for col in 0..=line.len() {
                assert_eq!(
                    position_to_char_col(line, at(0, col as u32), enc),
                    col,
                    "encoding {:?} col {}",
                    enc,
                    col
                );
                assert_eq!(
                    char_col_to_position(line, 0, col, enc),
                    at(0, col as u32),
                    "reverse encoding {:?} col {}",
                    enc,
                    col
                );
            }
        }
    }

    #[test]
    fn utf16_supplementary_plane() {
        // 𝒜 (MATHEMATICAL SCRIPT CAPITAL A) is U+1D49C — outside the
        // BMP, so 2 UTF-16 code units, 4 UTF-8 bytes, 1 char.
        let line = "a𝒜b";
        // After "a" (1 unit/byte/char), then 𝒜 (+2 units, +4 bytes, +1 char), then "b".
        // Cursor right after 𝒜:
        // - utf16 character = 3 → char col = 2
        // - utf8 character = 5 → char col = 2
        // - utf32 character = 2 → char col = 2
        assert_eq!(position_to_char_col(line, at(0, 3), PositionEncoding::Utf16), 2);
        assert_eq!(position_to_char_col(line, at(0, 5), PositionEncoding::Utf8), 2);
        assert_eq!(position_to_char_col(line, at(0, 2), PositionEncoding::Utf32), 2);

        // Reverse direction.
        assert_eq!(char_col_to_position(line, 0, 2, PositionEncoding::Utf16), at(0, 3));
        assert_eq!(char_col_to_position(line, 0, 2, PositionEncoding::Utf8), at(0, 5));
        assert_eq!(char_col_to_position(line, 0, 2, PositionEncoding::Utf32), at(0, 2));
    }

    #[test]
    fn utf16_position_inside_surrogate_clamps_down() {
        // 𝒜 occupies utf16 units [1..3]. character=2 is in the middle
        // of its surrogate pair; we clamp to the char boundary before
        // — col=1 (after "a").
        let line = "a𝒜b";
        assert_eq!(position_to_char_col(line, at(0, 2), PositionEncoding::Utf16), 1);
    }

    #[test]
    fn utf16_bmp_non_ascii() {
        // ñ (U+00F1) is BMP — 1 utf16 unit, 2 utf8 bytes, 1 char.
        let line = "señor";
        // utf16 col 4 → after "seño" → char col 4
        // utf8 col 5 → after "seño" → char col 4
        // utf32 col 4 → char col 4
        assert_eq!(position_to_char_col(line, at(0, 4), PositionEncoding::Utf16), 4);
        assert_eq!(position_to_char_col(line, at(0, 5), PositionEncoding::Utf8), 4);
        assert_eq!(position_to_char_col(line, at(0, 4), PositionEncoding::Utf32), 4);
    }

    #[test]
    fn out_of_range_clamps_to_end() {
        let line = "abc";
        assert_eq!(position_to_char_col(line, at(0, 99), PositionEncoding::Utf16), 3);
        assert_eq!(position_to_char_col(line, at(0, 99), PositionEncoding::Utf32), 3);
    }

    #[test]
    fn empty_line() {
        assert_eq!(position_to_char_col("", at(0, 0), PositionEncoding::Utf16), 0);
        assert_eq!(position_to_char_col("", at(0, 5), PositionEncoding::Utf16), 0);
    }

    #[test]
    fn from_kind_default_is_utf16() {
        assert_eq!(PositionEncoding::from_kind(None), PositionEncoding::Utf16);
        assert_eq!(
            PositionEncoding::from_kind(Some(&PositionEncodingKind::UTF8)),
            PositionEncoding::Utf8
        );
        assert_eq!(
            PositionEncoding::from_kind(Some(&PositionEncodingKind::UTF32)),
            PositionEncoding::Utf32
        );
    }
}
