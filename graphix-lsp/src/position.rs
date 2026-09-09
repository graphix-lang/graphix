//! LSP `Position` ↔ char-column conversion under the negotiated
//! position encoding (UTF-16 code units, UTF-32 scalars, or UTF-8
//! bytes). The compiler and the cursor helpers speak char columns, so
//! these translate at the LSP boundary; ASCII lines are a no-op.

use lsp_types::{Position, PositionEncodingKind};

/// Position encoding negotiated with the client, narrowed to the three
/// variants the spec defines.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionEncoding {
    Utf8,
    Utf16,
    Utf32,
}

impl PositionEncoding {
    /// Map the negotiated `PositionEncodingKind` (absent or unrecognized
    /// means the UTF-16 default) into our enum.
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
/// Out-of-range character offsets clamp to the end of the line, as the
/// LSP spec mandates.
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

/// Build an LSP `Position` from a (line, char-column) pair; inverse of
/// `position_to_char_col`. `char_col` is clamped to the line length.
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

/// `position_to_char_col` over the full document text; `None` if the
/// line index is out of range.
pub fn position_to_char_col_in_text(
    text: &str,
    position: Position,
    encoding: PositionEncoding,
) -> Option<usize> {
    let line = text.lines().nth(position.line as usize)?;
    Some(position_to_char_col(line, position, encoding))
}

/// `char_col_to_position` over the full document text; `character: 0`
/// if the line index is out of range.
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
        // 𝒜 (U+1D49C) is 2 UTF-16 units, 4 UTF-8 bytes, 1 char.
        let line = "a𝒜b";
        // Cursor right after 𝒜: utf16 3, utf8 5, utf32 2 → char col 2.
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
        // character=2 is mid surrogate pair; clamp to the boundary before.
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
