use super::{
    GRAPHIX_ESC, GRAPHIX_MUST_ESC, expr,
    grow::{grow, note_reason, refuse},
    holds_attr, holds_comment, sptoken,
};
use crate::expr::{Expr, ExprKind, StrForm};
use arcstr::ArcStr;
use combine::{
    RangeStream, attempt, between, choice, many, many1, not_followed_by, optional,
    parser::char::string,
    position, satisfy,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use compact_str::CompactString;
use netidx_value::{Value, parser::escaped_string};
use poolshark::local::LPooled;
use triomphe::Arc;

parser! {
    pub(super) fn interpolated[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        #[derive(Debug, Clone)]
        enum Intp {
            // a run of text: where it starts and ends
            Lit(SourcePosition, SourcePosition, CompactString),
            Expr(Expr),
        }
        fn text(s: &str) -> ExprKind {
            ExprKind::Constant(Value::String(ArcStr::from(s)))
        }
        impl Intp {
            fn to_expr(self) -> Expr {
                match self {
                    Intp::Lit(pos, end, s) => {
                        text(&s).to_expr(pos).ending(end).written_as(StrForm::Text)
                    }
                    Intp::Expr(s) => s,
                }
            }
        }
        // Adjacent literal parts merge so the AST is canonical whichever
        // delimiter the author wrote.
        fn finish(pos: SourcePosition, mut toks: LPooled<Vec<Intp>>) -> Expr {
            let mut merged: LPooled<Vec<Intp>> = LPooled::take();
            for t in toks.drain(..) {
                match (merged.last_mut(), t) {
                    (Some(Intp::Lit(_, prev_end, prev)), Intp::Lit(_, end, s)) => {
                        prev.push_str(&s);
                        *prev_end = end;
                    }
                    (_, t) => merged.push(t),
                }
            }
            // a string with no splice is one constant, quotes and all
            let whole = match &merged[..] {
                [] => Some(""),
                [Intp::Lit(_, _, s)] => Some(s.as_str()),
                _ => None,
            };
            match whole {
                Some(s) => text(s).to_expr(pos),
                None => ExprKind::StringInterpolate {
                    args: Arc::from_iter(merged.drain(..).map(Intp::to_expr)),
                }
                .to_expr(pos),
            }
        }
        // a splice prints on its string's line, where no comment can stand
        let spliced = |(e, end): (Expr, SourcePosition)| {
            // CR claude for claude: [bug/medium] This checks only `dec.comments`, so a
            // comment ending a line inside a splice (`"[{ let x = 1; // hi\n x }]"`) or
            // standing below a list in one parses. The formatter then refuses the file
            // as its own bug. An attribute in a splice also gets through, and the
            // printer breaks the splice across lines. Refuse any decoration with no
            // single-line form here: comments, eol, below and attributes, or ask Eric
            // whether attributes stay legal in splices. probe:
            // design/review-2026-10-26/repro/t-parser-a-splice-eol-comment.sh
            // (t-parser-a-02)
            // 2026-10-26 claude: Comments of every kind are now refused in a splice
            // (`holds_comment`, pin parser/test.rs
            // no_comment_stands_inside_a_splice_or_an_attribute). Attributes are still
            // accepted there, and the printer still breaks the splice across lines for
            // one (`"a [{ #[pure] let y = x; y }] b"`, which reads back the same).
            // Question for Eric: refuse attributes in a splice (a syntax rule: a
            // program that parses today would be refused), or keep them and have the
            // splice printer write attributes on the expression's line (`#[pure] let y
            // = x` parses on one line), departing from 'a tree holding an attribute has
            // no single-line form' for splices only?
            // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q75,
            // Attributes inside a string splice): yes, refuse (the recommendation:
            // refuse.)
            // 2026-10-28 qwen: Refused an attribute anywhere inside a string's splice:
            // parser holds_attr (parser/mod.rs) folds dec.attrs and trait method attrs,
            // interpolateexp's spliced refuses with \"an attribute cannot stand inside
            // a string's splice\". Pins: parser/test.rs
            // no_attribute_stands_inside_a_splice (refused in [{..}], in a template
            // \\[..] and on a trait method; a plain splice still parses); the expr
            // round-trip generator now strips attributes as well as comments from
            // splice expressions (expr/test.rs uncommented).
            // 2026-10-28 claude: Review (2026-10-28): the attribute half holds; the
            // comment half does not. holds_comment (parser/mod.rs) never checks
            // TraitMethod.eol, TraitExpr.below, SigItem.eol or Sig.below, so a trait or
            // dynamic sig comment inside a string splice parses and graphix fmt then
            // refuses its own output ('formatter bug: the formatted text lost or moved
            // a comment'). probes ~/tmp/xcr-review-2026-10-28/1/p/sp1.gx sp2.gx; no pin
            // covers these fields.
            match (holds_comment(&e), holds_attr(&e)) {
                (true, _) => refuse(end, "a comment cannot stand inside a string's splice")
                    .right(),
                (_, true) => refuse(
                    end,
                    "an attribute cannot stand inside a string's splice",
                )
                .right(),
                (false, false) => value(Intp::Expr(e)).left(),
            }
        };
        let interp_part = || attempt(
            between(token('['), sptoken(']'), (expr(), position())).then(spliced)
        );
        // A `[` that opens no well-formed `[expr]` ends the string parse;
        // the note names the escape.
        let bracket_note = || (position(), token('[')).then(|(pos, _)| {
            note_reason(
                pos,
                None,
                CompactString::const_new(
                    "`[` opens an interpolated expression inside a string; write \
                     `\\[` for a literal bracket (or a \"\"\"template\"\"\", where \
                     brackets are plain text)",
                ),
            );
            unexpected_any("string interpolation").map(|_: ()| unreachable!())
        });
        let chunk_part = || (
            position(),
            escaped_string(&GRAPHIX_MUST_ESC, &GRAPHIX_ESC),
            position(),
        )
            .then(|(pos, s, end)| {
                if s.is_empty() {
                    unexpected_any("empty string").right()
                } else {
                    value(Intp::Lit(pos, end, CompactString::from(s))).left()
                }
            });
        // Template form: brackets are content and the splice is marked
        // `\[expr]`; `\]` is an error; content ends at the first unescaped
        // `"""`; one newline right after the opener is stripped.
        let splice_part = || attempt(string("\\["))
            .with((expr(), position()))
            .skip(sptoken(']'))
            .then(spliced);
        let triple_run = || (
            position(),
            many1::<CompactString, _, _>(satisfy(|c| c != '"' && c != '\\')),
            position(),
        )
            .map(|(pos, s, end)| Intp::Lit(pos, end, s));
        let triple_escape = || (position(), attempt(token('\\').with(choice((
            token('n').map(|_| '\n'),
            token('r').map(|_| '\r'),
            token('t').map(|_| '\t'),
            token('0').map(|_| '\0'),
            token('"').map(|_| '"'),
            token('\\').map(|_| '\\'),
        )))), position())
            .map(|(pos, c, end)| Intp::Lit(pos, end, CompactString::from_iter([c])));
        let triple = (
            position(),
            between(
                attempt(string("\"\"\"")),
                string("\"\"\""),
                (
                    optional(token('\n')),
                    many(choice((
                        splice_part(),
                        triple_escape(),
                        triple_run(),
                        attempt((
                            position(),
                            token('"').skip(not_followed_by(string("\"\""))),
                            position(),
                        ))
                        .map(|(pos, _, end)| Intp::Lit(pos, end, CompactString::const_new("\""))),
                    ))),
                )
                    .map(|(_, toks)| toks),
            ),
        )
            .map(|(pos, toks): (_, LPooled<Vec<Intp>>)| {
                finish(pos, toks).written_as(StrForm::Template)
            });
        let single = (
            position(),
            between(
                token('"'),
                token('"'),
                many(choice((interp_part(), chunk_part(), bracket_note()))),
            ),
        )
            .map(|(pos, toks): (_, LPooled<Vec<Intp>>)| finish(pos, toks));
        grow(choice((triple, single)))
    }
}
