//! Convert anyhow errors from the graphix compiler into LSP diagnostics.
//! Compile errors carry an `ErrorContext(Expr)` and parser errors a
//! `ParserContext`; both are recovered by `downcast_ref`.

use graphix_compiler::expr::{ErrorContext, ParserContext, Source};
use lsp_types::Position;
use std::path::PathBuf;

/// What the chain told us about the failure: a position (line/col) and
/// optionally the source file the error originated in.
#[derive(Debug, Clone, Default)]
pub struct ErrorLocation {
    pub position: Option<Position>,
    pub file: Option<PathBuf>,
}

/// Walk the error chain for the most specific position and source file.
/// `anyhow::Error::downcast_ref` returns the outermost matching context,
/// which is the right one for the compile path.
pub fn error_location(err: &anyhow::Error) -> ErrorLocation {
    if let Some(ec) = err.downcast_ref::<ErrorContext>() {
        return location_from_origin_pos(&ec.0.ori.source, ec.0.pos);
    }
    if let Some(pc) = err.downcast_ref::<ParserContext>() {
        return location_from_origin_pos(&pc.ori.source, pc.pos);
    }
    ErrorLocation::default()
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
        expr::{Expr, ExprKind, Origin},
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

    /// A compile bail carries an outer `ErrorContext`; `error_location`
    /// pulls position and file from it.
    #[test]
    fn error_location_from_compile_error_context() {
        let o = ori("/tmp/foo.gx");
        let e = expr_at(12, 4, o.clone());
        let err = anyhow!("name not defined").context(ErrorContext(e));
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 11, character: 3 }));
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

    /// With stacked `ErrorContext` wraps the outermost (most recently
    /// attached) wins: a containing expression's position, still inside
    /// the user's code.
    #[test]
    fn error_location_picks_outermost_context() {
        let o = ori("/tmp/foo.gx");
        let inner = expr_at(20, 26, o.clone());
        let outer = expr_at(1, 1, o.clone());
        let err = anyhow!("raw not defined")
            .context(ErrorContext(inner))
            .context(ErrorContext(outer));
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 0, character: 0 }));
    }
}
