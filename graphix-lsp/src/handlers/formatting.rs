use crate::{
    position::{PositionEncoding, char_col_to_position},
    state::ServerState,
    uri::uri_to_path,
};
use anyhow::Result;
use graphix_compiler::expr::format::{FormatConfig, Refused, SourceKind, format_source};
use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};

/// The edit that turns `text` into its formatted form: none when it is
/// formatted already, else one that replaces the whole document.
fn edits(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
    encoding: PositionEncoding,
) -> Result<Vec<TextEdit>> {
    let formatted = format_source(kind, text, cfg)?;
    if *formatted == text {
        return Ok(vec![]);
    }
    let last_line = text.rsplit('\n').next().unwrap_or("");
    let line = text.matches('\n').count() as u32;
    let end = char_col_to_position(last_line, line, last_line.chars().count(), encoding);
    let range = Range { start: Position { line: 0, character: 0 }, end };
    Ok(vec![TextEdit { range, new_text: formatted.to_string() }])
}

/// `Err` when the formatter refused its own output or `graphixfmt.json`
/// is malformed. A document that does not parse has nothing to format,
/// and its diagnostics say why.
pub fn handle(
    state: &ServerState,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = &params.text_document.uri;
    let Some(doc) = state.documents.get(uri) else {
        return Ok(None);
    };
    let path = uri_to_path(uri);
    let kind = path.as_deref().map_or(SourceKind::Program, SourceKind::of_path);
    let cfg = match path.as_deref().and_then(|p| p.parent()) {
        Some(dir) => FormatConfig::discover(dir)?,
        None => FormatConfig::default(),
    };
    match edits(kind, &doc.text, &cfg, state.position_encoding) {
        Ok(edits) => Ok(Some(edits)),
        Err(e) if e.is::<Refused>() => Err(e),
        Err(_) => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn replaces_the_whole_document() {
        let text = "let  x=1;\nlet s = \"é\"";
        let got = edits(
            SourceKind::Program,
            text,
            &FormatConfig::default(),
            PositionEncoding::Utf16,
        )
        .unwrap();
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].new_text, "let x = 1;\nlet s = \"é\"\n");
        assert_eq!(got[0].range.start, Position { line: 0, character: 0 });
        assert_eq!(got[0].range.end, Position { line: 1, character: 11 });
        let utf8 = edits(
            SourceKind::Program,
            text,
            &FormatConfig::default(),
            PositionEncoding::Utf8,
        )
        .unwrap();
        assert_eq!(utf8[0].range.end, Position { line: 1, character: 12 });
    }

    #[test]
    fn a_trailing_newline_ends_the_range_on_the_next_line() {
        let got = edits(
            SourceKind::Program,
            "let  x=1\n",
            &FormatConfig::default(),
            PositionEncoding::Utf16,
        );
        assert_eq!(got.unwrap()[0].range.end, Position { line: 1, character: 0 });
    }

    #[test]
    fn formatted_text_needs_no_edit() {
        let got = edits(
            SourceKind::Interface,
            "val v: i64\n",
            &FormatConfig::default(),
            PositionEncoding::Utf16,
        );
        assert!(got.unwrap().is_empty());
    }

    #[test]
    fn unparseable_text_is_an_error_the_handler_swallows() {
        let got = edits(
            SourceKind::Program,
            "let x = ",
            &FormatConfig::default(),
            PositionEncoding::Utf16,
        );
        assert!(got.unwrap_err().downcast_ref::<Refused>().is_none());
    }
}
