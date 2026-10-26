//! LSP `Position` ↔ char-column conversion under the negotiated
//! position encoding (UTF-16 code units or UTF-32 scalars). The compiler and the cursor helpers speak char columns, so
//! these translate at the LSP boundary; ASCII lines are a no-op.

use ahash::AHashMap;
use arcstr::ArcStr;
use lsp_types::{Position, PositionEncodingKind};
use poolshark::local::LPooled;

/// Position encoding negotiated with the client: the UTF-16 default, or
/// UTF-32 when the client offers it (the server never picks UTF-8).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionEncoding {
    Utf16,
    Utf32,
}

impl PositionEncoding {
    /// Map the negotiated `PositionEncodingKind` (absent or unrecognized
    /// means the UTF-16 default) into our enum.
    pub fn from_kind(kind: Option<&PositionEncodingKind>) -> Self {
        match kind {
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
    };
    Position { line, character }
}

/// A document's lines as LSP numbers them: a line ends at "\r\n", "\r"
/// or "\n", and what follows the last break, empty or not, is a line.
// CR claude for claude: [bug/low] This counts a lone \r as a line break, but the parser
// does not, and diagnostics and query positions come from the parser. In a lone-CR
// document, `let x = 1;\rlet y = "a" + 1;\ry` publishes its error at line 0, character
// 10, instead of line 1, characters 8..15. Have the parser count \r as a line break, or
// translate parser positions through this function before they reach the client. probe:
// design/review-2026-10-26/repro/x-rerun-5-lsp-lone-cr-diag.py (x-rerun-5-08)
// CR claude for claude: [bug/low] These lines break at a lone \r, but the compiler's
// SourcePosition starts a line only at \n. In a lone-CR document every compiler
// position is read against the wrong line: "nope not defined" on the third line is
// published at (0,10), and hover on line 1 answers nothing. Also text::text_between
// (str::lines), text::use_path (split_inclusive) and text::call_context (counts \n)
// each split lines their own way. Fix: make the parser's positioner count a lone \r as
// a newline, so compiler lines match the client's, and route those three helpers
// through this function. probe: design/review-2026-10-26/repro/lsp-lone-cr-positions.py
// Also reported as (x-lsp-probe-06). (lsp-03)
pub fn lines(text: &str) -> impl Iterator<Item = &str> {
    let mut rest = Some(text);
    std::iter::from_fn(move || {
        let t = rest?;
        Some(match t.find(['\r', '\n']) {
            None => {
                rest = None;
                t
            }
            Some(i) => {
                let brk = if t[i..].starts_with("\r\n") { 2 } else { 1 };
                rest = Some(&t[i + brk..]);
                &t[..i]
            }
        })
    })
}

/// A text's lines, found once: each conversion indexes them where a
/// scan would read the text from its top.
pub struct LineIndex {
    text: ArcStr,
    /// Each line's byte range, its break left out.
    lines: LPooled<Vec<(usize, usize)>>,
}

impl LineIndex {
    pub fn new(text: &ArcStr) -> Self {
        let base = text.as_ptr() as usize;
        let mut index: LPooled<Vec<(usize, usize)>> = LPooled::take();
        index.extend(lines(text).map(|l| {
            let start = l.as_ptr() as usize - base;
            (start, start + l.len())
        }));
        Self { text: text.clone(), lines: index }
    }

    fn line(&self, line: u32) -> Option<&str> {
        self.lines.get(line as usize).map(|&(start, end)| &self.text[start..end])
    }

    /// `position_to_char_col` at `position`'s line; `None` if there is
    /// no such line.
    pub fn char_col(
        &self,
        position: Position,
        encoding: PositionEncoding,
    ) -> Option<usize> {
        Some(position_to_char_col(self.line(position.line)?, position, encoding))
    }

    /// `char_col_to_position` at `line`; `character: 0` if there is no
    /// such line.
    pub fn position(
        &self,
        line: u32,
        char_col: usize,
        encoding: PositionEncoding,
    ) -> Position {
        char_col_to_position(self.line(line).unwrap_or(""), line, char_col, encoding)
    }
}

/// The [`LineIndex`] of each text a request converts positions in,
/// built at its first conversion.
#[derive(Default)]
pub struct LineIndexes(AHashMap<usize, LineIndex>);

impl LineIndexes {
    pub fn of(&mut self, text: &ArcStr) -> &LineIndex {
        self.0.entry(text.as_ptr() as usize).or_insert_with(|| LineIndex::new(text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn lines_break_as_lsp_does() {
        let got: Vec<&str> = lines("a\r\nb\rc\n\nd\n").collect();
        assert_eq!(got, ["a", "b", "c", "", "d", ""]);
        assert_eq!(lines("").collect::<Vec<_>>(), [""]);
        let index = LineIndex::new(&arcstr::literal!("x\n"));
        assert_eq!(index.char_col(at(1, 0), PositionEncoding::Utf16), Some(0));
        assert_eq!(index.char_col(at(2, 0), PositionEncoding::Utf16), None);
        let index = LineIndex::new(&ArcStr::from("a\r\nb𝒜c\rd"));
        assert_eq!(index.position(1, 2, PositionEncoding::Utf16), at(1, 3));
        assert_eq!(index.char_col(at(2, 1), PositionEncoding::Utf32), Some(1));
        assert_eq!(index.position(5, 2, PositionEncoding::Utf16), at(5, 0));
    }

    #[test]
    fn ascii_all_encodings_equivalent() {
        let line = "let x = 1";
        for enc in [PositionEncoding::Utf16, PositionEncoding::Utf32] {
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
        // 𝒜 (U+1D49C) is 2 UTF-16 units, 1 char.
        let line = "a𝒜b";
        // Cursor right after 𝒜: utf16 3, utf32 2 → char col 2.
        assert_eq!(position_to_char_col(line, at(0, 3), PositionEncoding::Utf16), 2);
        assert_eq!(position_to_char_col(line, at(0, 2), PositionEncoding::Utf32), 2);

        // Reverse direction.
        assert_eq!(char_col_to_position(line, 0, 2, PositionEncoding::Utf16), at(0, 3));
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
        // ñ (U+00F1) is BMP — 1 utf16 unit, 1 char.
        let line = "señor";
        // utf16 col 4 → after "seño" → char col 4
        // utf32 col 4 → char col 4
        assert_eq!(position_to_char_col(line, at(0, 4), PositionEncoding::Utf16), 4);
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
            PositionEncoding::Utf16
        );
        assert_eq!(
            PositionEncoding::from_kind(Some(&PositionEncodingKind::UTF32)),
            PositionEncoding::Utf32
        );
    }
}
