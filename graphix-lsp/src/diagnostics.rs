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
    // CR claude for claude: [bug/medium] A ParserContext wins here even when it sits
    // outside the error's ErrorSite. Module's interface checks wrap
    // check_hidden_typedef's `.at(e)` (the implementation's typedef) in a ParserContext
    // at the sig item, so "T is defined by its interface; delete this definition" is
    // published on the .gxi's `type T`. That tells the user to delete the interface's
    // declaration, while `--check` names util.gx. Make ParserContext a site: add it at
    // those module.rs sites only when the error has no ErrorSite (as `At::sited` does),
    // and let `At::at`/`sited` treat a ParserContext as the site. probe:
    // design/review-2026-10-26/repro/x-lsp-probe-parser-context-wins.py
    // (x-lsp-probe-03)
    if let Some(pc) = err.downcast_ref::<ParserContext>() {
        return location(&pc.ori.source, pc.pos, WrittenAt::NOWHERE);
    }
    let site = err.downcast_ref::<ErrorSite>().map(|s| s.expr());
    match site.or_else(|| err.downcast_ref::<ErrorContext>().map(|c| c.expr())) {
        Some(e) => location(&e.ori.source, e.pos, e.end),
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
