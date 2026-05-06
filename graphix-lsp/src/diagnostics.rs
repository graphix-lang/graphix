//! Convert anyhow errors from the graphix compiler into LSP diagnostics.
//!
//! Compile-time errors are wrapped in `ErrorContext(Expr)` and parser
//! errors in `ParserContext`; both carry the originating `Origin` and
//! `SourcePosition` directly. We walk the anyhow chain and `downcast_ref`
//! to recover them — no message-string scraping.

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

/// Walk the error chain and pick the most specific position +
/// source file we can find.
///
/// `ErrorContext` (compile-time) and `ParserContext` (parser) carry
/// `Origin` + `SourcePosition` structurally. anyhow stores them inside
/// `ContextError<C, E>` wrappers, so `<&dyn Error>::downcast_ref` (which
/// matches the actual chain-entry type) doesn't see them — instead we
/// use `anyhow::Error::downcast_ref`, which walks the context chain via
/// anyhow's vtable and returns the outermost matching `C`. For our
/// migration the relevant wraps live at the top of the chain, so the
/// outermost match is the right one.
pub fn error_location(err: &anyhow::Error) -> ErrorLocation {
    if let Some(ec) = err.downcast_ref::<ErrorContext>() {
        return location_from_origin_pos(&ec.0.ori.source, ec.0.pos);
    }
    if let Some(pc) = err.downcast_ref::<ParserContext>() {
        return location_from_origin_pos(&pc.ori.source, pc.pos);
    }
    ErrorLocation::default()
}

/// Compose an `ErrorLocation` from the compiler's 1-based
/// (line, column) and the originating `Source`. LSP wants 0-based
/// positions, so we subtract 1.
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

/// Use the chain's leaf as the diagnostic message. With structured
/// `ErrorContext` / `ParserContext` carrying position info, the leaf
/// is just the human-readable failure text (e.g. `"raw not defined"`).
pub fn error_leaf_message(err: &anyhow::Error) -> String {
    err.chain().last().map(|c| c.to_string()).unwrap_or_else(|| "error".into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::anyhow;
    use arcstr::literal;
    use graphix_compiler::{
        expr::{Expr, ExprKind, Origin},
        SourcePosition,
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
        // ExprKind::NoOp is the smallest concrete expr available — we
        // only care about pos and ori for ErrorContext.
        let pos = SourcePosition { line, column };
        let mut e = ExprKind::NoOp.to_expr(pos);
        e.ori = ori;
        e
    }

    /// A compile bail wrapped via the new `bailat!` macro produces an
    /// anyhow chain whose outer entry is an `ErrorContext` carrying the
    /// originating expr. `error_location` should pull the position and
    /// file straight out of it.
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
        let pc = ParserContext { ori: o.clone(), pos: SourcePosition { line: 3, column: 7 } };
        let err = anyhow!("unexpected token").context(pc);
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 2, character: 6 }));
        assert_eq!(loc.file, Some(PathBuf::from("/tmp/bar.gx")));
    }

    /// When multiple `ErrorContext` wraps stack up, anyhow's vtable
    /// returns the outermost — i.e. the most recently attached context.
    /// For compile-time wraps that's the parent expression's position
    /// rather than the failing leaf, which is acceptable: the user is
    /// taken to a containing expression rather than a missing token, but
    /// the diagnostic still lands inside the user's code.
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
