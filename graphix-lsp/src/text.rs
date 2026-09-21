//! Source text around a position. Columns are char counts here; the
//! LSP encoding is translated at the boundary (`crate::position`).

use graphix_compiler::SourcePosition;
use lsp_types::Position;
use poolshark::local::LPooled;

pub(crate) fn is_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// The 0-indexed (line, char column) of a compiler position.
pub(crate) fn zero_based(pos: SourcePosition) -> Position {
    Position {
        line: pos.line.saturating_sub(1).max(0) as u32,
        character: pos.column.saturating_sub(1).max(0) as u32,
    }
}

/// True when `cursor` is on the `len` chars at `start`, either end
/// included.
pub(crate) fn covers(start: Position, len: usize, cursor: Position) -> bool {
    cursor.line == start.line
        && cursor.character >= start.character
        && cursor.character <= start.character + len as u32
}

fn line_chars(text: &str, line: u32) -> Option<LPooled<Vec<char>>> {
    let line = text.lines().nth(line as usize)?;
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(line.chars());
    Some(chars)
}

fn is_pathsep(chars: &[char], i: usize) -> bool {
    chars.get(i).copied() == Some(':') && chars.get(i + 1).copied() == Some(':')
}

fn path_start(chars: &[char], mut start: usize) -> usize {
    while start > 0 {
        if is_id_char(chars[start - 1]) {
            start -= 1;
        } else if start >= 2 && is_pathsep(chars, start - 2) {
            start -= 2;
        } else {
            break;
        }
    }
    start
}

/// The identifier the cursor is on (`map` in `array::map`).
pub(crate) fn ident_at(text: &str, cursor: Position) -> Option<String> {
    let chars = line_chars(text, cursor.line)?;
    let col = (cursor.character as usize).min(chars.len());
    let (mut start, mut end) = (col, col);
    while start > 0 && is_id_char(chars[start - 1]) {
        start -= 1;
    }
    while end < chars.len() && is_id_char(chars[end]) {
        end += 1;
    }
    (start != end).then(|| chars[start..end].iter().collect())
}

/// Where the thing starting at `start` ends, for want of end positions
/// in the AST: a path ends with itself, anything else with its line.
pub(crate) fn extent(text: &str, start: Position) -> Position {
    let Some(chars) = line_chars(text, start.line) else { return start };
    let from = (start.character as usize).min(chars.len());
    let mut end = from;
    while end < chars.len() {
        if is_id_char(chars[end]) {
            end += 1;
        } else if is_pathsep(&chars, end) {
            end += 2;
        } else {
            break;
        }
    }
    if end == from {
        end = chars.len();
        while end > from && chars[end - 1].is_whitespace() {
            end -= 1;
        }
    }
    Position { line: start.line, character: end as u32 }
}

/// What is being typed at the cursor.
pub(crate) struct Typed {
    /// The path left of the cursor (`array::ma`, `array::`, or empty).
    pub path: String,
    /// The field chain the path hangs off (`["p", "inner"]` for
    /// `p.inner.na`), empty when it hangs off nothing.
    pub receiver: Vec<String>,
}

pub(crate) fn typed_before(text: &str, cursor: Position) -> Option<Typed> {
    let chars = line_chars(text, cursor.line)?;
    let col = (cursor.character as usize).min(chars.len());
    let start = path_start(&chars, col);
    let path: String = chars[start..col].iter().collect();
    let mut receiver = Vec::new();
    let mut at = start;
    while at > 0 && chars[at - 1] == '.' {
        let end = at - 1;
        let mut begin = end;
        while begin > 0 && is_id_char(chars[begin - 1]) {
            begin -= 1;
        }
        if begin == end {
            break;
        }
        receiver.insert(0, chars[begin..end].iter().collect());
        at = begin;
    }
    Some(Typed { path, receiver })
}

/// The callee path (`foo`, `array::map`) if the cursor is inside an open
/// `(`'s argument list; `None` inside `[`/`{` or past a statement
/// boundary. String literals are not parsed.
pub(crate) fn call_context(text: &str, cursor: Position) -> Option<String> {
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(text.chars());
    let (mut offset, mut line, mut col) = (0usize, 0u32, 0u32);
    while offset < chars.len() && (line, col) != (cursor.line, cursor.character) {
        if chars[offset] == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        offset += 1;
    }
    let mut depth = 0i32;
    let mut i = offset;
    while i > 0 {
        i -= 1;
        match chars[i] {
            ')' | ']' | '}' => depth += 1,
            '(' if depth == 0 => {
                let mut end = i;
                while end > 0 && chars[end - 1].is_whitespace() {
                    end -= 1;
                }
                let start = path_start(&chars, end);
                return (start != end).then(|| chars[start..end].iter().collect());
            }
            '[' | '{' if depth == 0 => return None,
            '(' | '[' | '{' => depth -= 1,
            ';' if depth == 0 => return None,
            _ => {}
        }
    }
    None
}

/// The start of the `#label` being typed at the cursor. `?#label` is a
/// fn type's syntax, not a call's.
pub(crate) fn label_start(text: &str, cursor: Position) -> Option<Position> {
    let chars = line_chars(text, cursor.line)?;
    let col = (cursor.character as usize).min(chars.len());
    let mut start = col;
    while start > 0 && is_id_char(chars[start - 1]) {
        start -= 1;
    }
    if start == 0 || chars[start - 1] != '#' {
        return None;
    }
    if start >= 2 && chars[start - 2] == '?' {
        return None;
    }
    Some(Position { line: cursor.line, character: (start - 1) as u32 })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn paths_and_receivers() {
        let text = "let q = p.inner.na + array::ma";
        assert_eq!(ident_at(text, at(0, 22)).as_deref(), Some("array"));
        let t = typed_before(text, at(0, 18)).unwrap();
        assert_eq!(
            (t.path.as_str(), t.receiver),
            ("na", vec!["p".into(), "inner".into()])
        );
        let t = typed_before(text, at(0, 28)).unwrap();
        assert_eq!((t.path.as_str(), t.receiver.len()), ("array::", 0));
    }

    #[test]
    fn extents() {
        let text = "let z = f(util::bump, \"no\")  ";
        assert_eq!(extent(text, at(0, 10)), at(0, 20));
        assert_eq!(extent(text, at(0, 22)), at(0, 27));
        assert_eq!(extent(text, at(3, 0)), at(3, 0));
    }

    #[test]
    fn call_contexts() {
        let text = "let q = array::map(xs, |x| f(x, [1, ";
        assert_eq!(call_context(text, at(0, 22)).as_deref(), Some("array::map"));
        assert_eq!(call_context(text, at(0, 31)).as_deref(), Some("f"));
        assert_eq!(call_context(text, at(0, 36)), None);
    }
}
