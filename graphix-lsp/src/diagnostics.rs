//! Where a failed check says its error is. A parse error carries a
//! `ParserContext`; a compile error an `ErrorSite` (the expression it
//! arose in) under any number of `ErrorContext`s.

use crate::text::zero_based;
use graphix_compiler::{
    SourcePosition,
    expr::{ErrorContext, ErrorSite, ParserContext, Source, Told, WrittenAt, parser},
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

// XCR claude for claude: [readability/low] A refusal the check tells first as
// a context above the error it explains reaches the editor without it: the
// diagnostic is the chain's leaf alone. A module task's decision about a
// cell the code around it left open (`Module::typecheck0`) and a top-level
// input's decision about an earlier input's (`Input::refused`) both wrap
// the check's own error, so with main.gx `let fs = array::map([1], |r| |x|
// x + x); mod m; m::go`, m.gxi `val go: (i64, f64)`, m.gx `let g =
// super::fs[0]$; let go = (g(1), g(1.5))` `--check` says "the check decides
// a type the code around the module left open: annotate `fs`" above the
// val's mismatch, and the server puts only "val go is declared (i64, f64)
// but implemented ('_8356: .., '_8357: ..): type variable usage mismatch"
// on m.gx, naming no binding to annotate. One way: a typed context for a
// refusal told first, which the leaf message leads with. probe:
// ~/ptmp/cr29/scratch/r23/23h/module-open-instance-msg-01/mq2/main.gx
// (lsp-leaf-drops-told-first-01)
// 2026-11-14 claude: A refusal's reason or remedy told above the error it
// explains is now an expr::Told (Told::over), and error_leaf_message leads the
// leaf with every Told in the chain, outermost first, a repeat once; --check
// prints as before. Told at: Module::typecheck0, Input::refused, the settle
// note, #[native] cannot hold, met_default_note and the join_default note, the
// narrowing note, typecheck_arg's pinned/decided note, arith_rule, traits.rs's
// implements note and its three method/default refusals, sig_matches's
// signature/implementation sides. Pins: graphix-lsp
// diagnostics::tests::what_was_told_leads_the_leaf, graphix-shell tests/lsp
// what_the_check_tells_first_leads_the_diagnostic (this probe, string + string,
// a met default told at two frames); both fail with the lead undone, and the
// arithmetic and met-default cases fail with op.rs's and callsite.rs's notes
// back to .context.
/// The chain's leaf is the human-readable failure text, a parse failure's
/// without the position and source line its squiggle shows, led by what
/// the check told above it.
pub fn error_leaf_message(err: &anyhow::Error) -> String {
    let leaf = match err.downcast_ref::<parser::ParseFailure>() {
        Some(p) => p.summary.clone(),
        None => {
            err.chain().last().map(|c| c.to_string()).unwrap_or_else(|| "error".into())
        }
    };
    match err.downcast_ref::<Told>() {
        None => leaf,
        Some(told) => {
            let mut message = String::new();
            for line in told.lines() {
                message.push_str(line);
                message.push('\n');
            }
            message.push_str(&leaf);
            message
        }
    }
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
        Arc::new(Origin::new(
            None,
            Source::File(PathBuf::from_str(path).unwrap()),
            literal!(""),
        ))
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

    /// What the check told above an error leads its leaf, outermost
    /// first, through the frames between, a line told twice running once;
    /// the frames themselves are not told.
    #[test]
    fn what_was_told_leads_the_leaf() {
        let o = ori("/tmp/foo.gx");
        let err = anyhow!("type mismatch i64 does not contain string").at(&expr_at(
            3,
            2,
            o.clone(),
        ));
        let err = Told::over(err, "annotate `g`");
        let err = Told::over(err.at(&expr_at(2, 1, o.clone())), "annotate `fs`");
        let err = Told::over(err, "annotate `fs`").context("compiling module m");
        assert_eq!(
            error_leaf_message(&err),
            "annotate `fs`\nannotate `g`\ntype mismatch i64 does not contain string"
        );
        let plain = anyhow!("missing not defined").at(&expr_at(1, 1, o));
        assert_eq!(error_leaf_message(&plain), "missing not defined");
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
