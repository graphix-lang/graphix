//! Where a failed check says its error is. A parse error carries a
//! `ParserContext`; a compile error an `ErrorSite` (the expression it
//! arose in) under any number of `ErrorContext`s.

use graphix_compiler::expr::{ErrorContext, ErrorSite, ParserContext, Source};
use lsp_types::Position;
use std::path::PathBuf;

#[derive(Debug, Clone, Default)]
pub struct ErrorLocation {
    pub position: Option<Position>,
    pub file: Option<PathBuf>,
}

/// The most specific position in the chain. `downcast_ref` finds the
/// outermost context of a type, so an error wrapped without `At::at`
/// falls back to the outermost `ErrorContext`.
pub fn error_location(err: &anyhow::Error) -> ErrorLocation {
    if let Some(pc) = err.downcast_ref::<ParserContext>() {
        return location_from_origin_pos(&pc.ori.source, pc.pos);
    }
    let site = err.downcast_ref::<ErrorSite>().map(|s| &s.0);
    match site.or_else(|| err.downcast_ref::<ErrorContext>()) {
        Some(ec) => location_from_origin_pos(&ec.0.ori.source, ec.0.pos),
        None => ErrorLocation::default(),
    }
}

/// Compose an `ErrorLocation` from the compiler's 1-based (line, column)
/// and the originating `Source`; LSP positions are 0-based.
fn location_from_origin_pos(
    source: &Source,
    pos: graphix_compiler::SourcePosition,
) -> ErrorLocation {
    let line = (pos.line.saturating_sub(1).max(0)) as u32;
    let character = (pos.column.saturating_sub(1).max(0)) as u32;
    let file = match source {
        Source::File(p) => Some(p.clone()),
        _ => None,
    };
    ErrorLocation { position: Some(Position { line, character }), file }
}

/// The chain's leaf is the human-readable failure text.
pub fn error_leaf_message(err: &anyhow::Error) -> String {
    err.chain().last().map(|c| c.to_string()).unwrap_or_else(|| "error".into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::anyhow;
    use arcstr::literal;
    use graphix_compiler::{
        SourcePosition,
        expr::{At, Expr, ExprKind, Origin},
    };
    use std::str::FromStr;
    use triomphe::Arc;

    fn ori(path: &str) -> Arc<Origin> {
        Arc::new(Origin {
            parent: None,
            source: Source::File(PathBuf::from_str(path).unwrap()),
            text: literal!(""),
        })
    }

    fn expr_at(line: i32, column: i32, ori: Arc<Origin>) -> Expr {
        // Only pos and ori matter for ErrorContext.
        let pos = SourcePosition { line, column };
        let mut e = ExprKind::NoOp.to_expr(pos);
        e.ori = ori;
        e
    }

    #[test]
    fn a_compile_error_is_located_where_it_arose() {
        let o = ori("/tmp/foo.gx");
        let err = anyhow!("raw not defined")
            .at(&expr_at(20, 26, o.clone()))
            .at(&expr_at(12, 4, o.clone()))
            .at(&expr_at(1, 1, o.clone()));
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 19, character: 25 }));
        assert_eq!(loc.file, Some(PathBuf::from("/tmp/foo.gx")));
    }

    /// Parser failures wrap their error in `ParserContext`. The LSP
    /// recovers position + file the same way it does for compile errors.
    #[test]
    fn error_location_from_parser_context() {
        let o = ori("/tmp/bar.gx");
        let pc =
            ParserContext { ori: o.clone(), pos: SourcePosition { line: 3, column: 7 } };
        let err = anyhow!("unexpected token").context(pc);
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 2, character: 6 }));
        assert_eq!(loc.file, Some(PathBuf::from("/tmp/bar.gx")));
    }
}
