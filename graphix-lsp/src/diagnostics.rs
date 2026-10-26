//! Where a failed check says its error is. A parse error carries a
//! `ParserContext`; a compile error an `ErrorSite` (the expression it
//! arose in) under any number of `ErrorContext`s.

use crate::text::zero_based;
use graphix_compiler::{
    SourcePosition,
    expr::{ErrorContext, ErrorSite, ParserContext, Source, WrittenAt, parser},
};
use lsp_types::Position;
use std::path::PathBuf;

#[derive(Debug, Clone, Default)]
pub struct ErrorLocation {
    pub position: Option<Position>,
    /// Where the erring expression ends; a parse error is a point.
    pub end: Option<Position>,
    pub file: Option<PathBuf>,
}

/// The most specific position in the chain. `downcast_ref` finds the
/// outermost context of a type, so an error wrapped without `At::at`
/// falls back to the outermost `ErrorContext`.
pub fn error_location(err: &anyhow::Error) -> ErrorLocation {
    // XCR claude for claude: [bug/medium] A ParserContext wins here even when it sits
    // outside the error's ErrorSite. Module's interface checks wrap
    // check_hidden_typedef's `.at(e)` (the implementation's typedef) in a ParserContext
    // at the sig item, so "T is defined by its interface; delete this definition" is
    // published on the .gxi's `type T`. That tells the user to delete the interface's
    // declaration, while `--check` names util.gx. Make ParserContext a site: add it at
    // those module.rs sites only when the error has no ErrorSite (as `At::sited` does),
    // and let `At::at`/`sited` treat a ParserContext as the site. probe:
    // design/review-2026-10-26/repro/x-lsp-probe-parser-context-wins.py
    // (x-lsp-probe-03)
    // 2026-10-26 claude: A ParserContext is now a site: At::placed adds one only to an
    // error with no site (module.rs's interface checks use it), At::at/sited treat it
    // as one, and error_location takes the ErrorSite first, then a ParserContext, then
    // the outermost ErrorContext. Pins: graphix-shell tests/lsp
    // an_implementation_error_stands_on_the_implementation (the probe's case, util.gx
    // line 2), graphix-lsp
    // diagnostics::tests::an_interface_item_places_only_an_unsited_error.
    if let Some(site) = err.downcast_ref::<ErrorSite>() {
        let e = site.expr();
        return location(&e.ori.source, e.pos, e.end);
    }
    if let Some(pc) = err.downcast_ref::<ParserContext>() {
        return location(&pc.ori.source, pc.pos, WrittenAt::NOWHERE);
    }
    match err.downcast_ref::<ErrorContext>() {
        Some(c) => location(&c.expr().ori.source, c.expr().pos, c.expr().end),
        None => ErrorLocation::default(),
    }
}

fn location(source: &Source, pos: SourcePosition, end: WrittenAt) -> ErrorLocation {
    let file = match source {
        Source::File(p) => Some(p.clone()),
        _ => None,
    };
    let end = end.get().map(zero_based);
    ErrorLocation { position: Some(zero_based(pos)), end, file }
}

/// The chain's leaf is the human-readable failure text; a parse
/// failure's without the position and source line its squiggle shows.
pub fn error_leaf_message(err: &anyhow::Error) -> String {
    if let Some(p) = err.downcast_ref::<parser::ParseFailure>() {
        return p.summary.clone();
    }
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

    /// An interface item places only an error that has no site: one
    /// raised at an implementation's expression stays there, and one it
    /// places keeps its place through the frames above it.
    #[test]
    fn an_interface_item_places_only_an_unsited_error() {
        let imp = ori("/tmp/util.gx");
        let sig = ori("/tmp/util.gxi");
        let pos = SourcePosition { line: 1, column: 1 };
        let err = anyhow!("delete this definition")
            .at(&expr_at(4, 1, imp.clone()))
            .placed(&sig, pos)
            .at(&expr_at(1, 1, imp.clone()));
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 3, character: 0 }));
        assert_eq!(loc.file, Some(PathBuf::from("/tmp/util.gx")));
        let err = anyhow!("missing").placed(&sig, pos).at(&expr_at(9, 2, imp));
        let loc = error_location(&err);
        assert_eq!(loc.position, Some(Position { line: 0, character: 0 }));
        assert_eq!(loc.file, Some(PathBuf::from("/tmp/util.gxi")));
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
