use crate::stack::ensure_sufficient;
use combine::stream::position::SourcePosition;
use combine::{
    ErrorOffset, ParseError, Parser, Stream, StreamOnce,
    error::{ParseResult, StreamError, Tracked},
    parser::ParseMode,
};
use compact_str::CompactString;
use std::{
    cell::{Cell, RefCell},
    sync::atomic::{AtomicUsize, Ordering},
};

/// Default [`max_nesting`].
///
/// Counted in parser recursion knots, not source constructs — one level
/// of `(1 + …)` costs three (`expr`, `arith`, `arith_term`) — so the
/// source nesting this admits is several times shallower. Hand-written
/// graphix nests to single digits; this is the point past which a
/// program is trying to exhaust the compiler rather than express
/// something.
pub const DEFAULT_MAX_NESTING: usize = 1000;

static MAX_NESTING: AtomicUsize = AtomicUsize::new(DEFAULT_MAX_NESTING);

/// How deeply a program may nest before the parser rejects it.
pub fn max_nesting() -> usize {
    MAX_NESTING.load(Ordering::Relaxed)
}

/// Raise or lower [`max_nesting`]. Process-global, like the trace flag:
/// the parser is a free function with no `ExecCtx` to hang it off.
///
/// The limit is what makes stack exhaustion unreachable rather than
/// merely expensive. [`ensure_sufficient`] moves a deep parse onto heap
/// segments, and the compiler's tree passes are guarded the same way,
/// but not every recursion in the pipeline can be: derived `Drop` glue
/// tears a deep `Type` down recursively with no function to wrap. The
/// limit bounds those too. Raising it past what the unguarded paths
/// survive trades a clean compile error for an abort.
pub fn set_max_nesting(depth: usize) {
    MAX_NESTING.store(depth, Ordering::Relaxed)
}

thread_local! {
    static DEPTH: Cell<usize> = const { Cell::new(0) };
    /// Set when a refusal happens. combine merges a committed error
    /// with whatever the surrounding alternatives expected, so the
    /// refusal's own message does not survive to the top — a program
    /// past the limit reported `Unexpected \`+\`` instead. The entry
    /// points check this flag and report the real reason.
    static REFUSED: Cell<bool> = const { Cell::new(false) };
}

/// Call before a parse; [`refused`] reads the result afterwards.
pub(super) fn clear_refused() {
    REFUSED.with(|r| r.set(false))
}

/// Did the last parse stop because it hit [`max_nesting`]?
pub(super) fn refused() -> bool {
    REFUSED.with(|r| r.get())
}

/// Record a refusal. Also called by the caps on the parser loops that
/// build a nested AST iteratively, which `GrowStack` cannot see.
pub(super) fn note_refused() {
    REFUSED.with(|r| r.set(true))
}

thread_local! {
    /// The furthest reason a parser refused something it could name —
    /// a reserved word where a name was expected, a `[` that opens no
    /// interpolation — kept beside its position and, for a refused
    /// token, its length. combine merges a refused alternative's
    /// message into the surrounding expectation set (a whole `let`
    /// reports "Unexpected `l`" at the statement's first column), so
    /// the reason is recorded here and reported when the failure lies
    /// on its line — inside the token itself when one is given, since
    /// a word refused as a name may have parsed as a literal in
    /// another alternative (`let x = true +;` probes `true` as a name).
    static REASON: RefCell<Option<Reason>> = const { RefCell::new(None) };
    /// Where the parse failed, set by the entry point's error mapping
    /// before [`parsing`] reports.
    static ERROR_POS: Cell<Option<SourcePosition>> = const { Cell::new(None) };
    /// The furthest point any branch of the parse reached. combine
    /// reports a failed statement at whichever alternative failed
    /// last, and `attempt` resets the input on the way out, so the
    /// branch that got deepest into the program — the one the author
    /// was writing — is otherwise forgotten. Every recursion knot
    /// records its input position here, on success as well (a failure
    /// in the combinator right after a knot — the `]` a string
    /// interpolation expects — is not seen by any knot, and the knot's
    /// own end is within a token of it).
    static FURTHEST: Cell<Option<SourcePosition>> = const { Cell::new(None) };
}

#[derive(Clone)]
struct Reason {
    pos: SourcePosition,
    /// The refused token's length in chars, when the reason explains
    /// a failure only inside that token.
    span: Option<usize>,
    reason: CompactString,
}

impl Reason {
    fn explains(&self, failure: SourcePosition) -> bool {
        failure.line == self.pos.line
            && match self.span {
                None => true,
                Some(n) => {
                    self.pos.column <= failure.column
                        && failure.column <= self.pos.column + n as i32
                }
            }
    }
}

fn key(p: SourcePosition) -> (i32, i32) {
    (p.line, p.column)
}

/// Record why something was refused at `pos`; a later refusal wins.
/// `span` is the refused token's length when the reason explains a
/// failure only inside that token.
pub(super) fn note_reason(
    pos: SourcePosition,
    span: Option<usize>,
    reason: CompactString,
) {
    REASON.with(|r| {
        let mut r = r.borrow_mut();
        if r.as_ref().is_none_or(|p| key(p.pos) <= key(pos)) {
            *r = Some(Reason { pos, span, reason });
        }
    })
}

/// Record where the parse failed.
pub(super) fn note_error_pos(pos: SourcePosition) {
    ERROR_POS.with(|p| p.set(Some(pos)))
}

fn note_furthest(pos: SourcePosition) {
    FURTHEST.with(|f| {
        if f.get().is_none_or(|p| key(p) < key(pos)) {
            f.set(Some(pos))
        }
    })
}

/// How a parse failed: the position reported and the message,
/// already rendered with the source line, a caret and any note.
#[derive(Debug)]
pub struct ParseFailure {
    pub pos: SourcePosition,
    pub msg: String,
}

impl std::fmt::Display for ParseFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

/// The source line at `pos` with a caret under its column, the line
/// windowed around the caret when it is long.
fn snippet(text: &str, pos: SourcePosition) -> String {
    let Some(line) = text.lines().nth((pos.line.max(1) - 1) as usize) else {
        return String::new();
    };
    let col = (pos.column.max(1) - 1) as usize;
    let chars: Vec<char> = line.chars().collect();
    const WIDTH: usize = 100;
    let start = if col > WIDTH { col - WIDTH / 2 } else { 0 };
    let end = chars.len().min(start + WIDTH);
    let shown: String = chars[start..end].iter().collect();
    let pad: String = chars[start..col.min(end)]
        .iter()
        .map(|c| if *c == '\t' { '\t' } else { ' ' })
        .collect();
    let lead = if start > 0 { "…" } else { "" };
    let trail = if end < chars.len() { "…" } else { "" };
    format!("    {lead}{shown}{trail}\n    {}{pad}^", if start > 0 { " " } else { "" })
}

/// Wrap a parse of `text`: clears the flags first, then reports the
/// nesting limit when that is what stopped the parse; otherwise the
/// FURTHEST point any branch reached, with the source line and a
/// caret — combine's own message only when it failed there too (an
/// alternative that failed earlier reports stale expectations) — and
/// the recorded reason when a refused name lies on that line.
pub(super) fn parsing<T, E: std::fmt::Display>(
    text: &str,
    f: impl FnOnce() -> Result<T, E>,
) -> Result<T, ParseFailure> {
    clear_refused();
    REASON.with(|r| *r.borrow_mut() = None);
    ERROR_POS.with(|p| p.set(None));
    FURTHEST.with(|p| p.set(None));
    f().map_err(|e| {
        let err_pos = ERROR_POS.with(|p| p.get()).unwrap_or_default();
        if refused() {
            return ParseFailure {
                pos: err_pos,
                msg: format!(
                    "expression nesting too deep (limit {}, see \
                     graphix_compiler::expr::parser::set_max_nesting)",
                    max_nesting()
                ),
            };
        }
        let furthest = FURTHEST.with(|p| p.get()).unwrap_or(err_pos);
        let pos = if key(furthest) > key(err_pos) { furthest } else { err_pos };
        let mut msg = if pos == err_pos {
            format!("{e}")
        } else {
            format!(
                "Parse error at line: {}, column: {}\nthe parser could not \
                 continue past this point",
                pos.line, pos.column
            )
        };
        let snippet = snippet(text, pos);
        if !snippet.is_empty() {
            msg.push('\n');
            msg.push_str(&snippet);
        }
        if let Some(r) = REASON.with(|r| r.borrow().clone())
            && r.explains(pos)
        {
            msg.push_str(&format!(
                "\n  note: at line: {}, column: {}: {}",
                r.pos.line, r.pos.column, r.reason
            ));
        }
        ParseFailure { pos, msg }
    })
}

/// Run `p` under [`ensure_sufficient`] and count it against
/// [`max_nesting`]. Wraps every recursion knot in the parser, so how
/// deeply a program may nest is bounded by an explicit limit rather
/// than by the stack of whichever thread parses it.
pub(super) fn grow<P>(p: P) -> GrowStack<P> {
    GrowStack(p)
}

pub(super) struct GrowStack<P>(P);

impl<Input, P> Parser<Input> for GrowStack<P>
where
    Input: Stream<Position = SourcePosition>,
    P: Parser<Input>,
{
    type Output = P::Output;
    type PartialState = P::PartialState;

    combine::parse_mode!(Input);

    #[inline]
    fn parse_mode_impl<M>(
        &mut self,
        mode: M,
        input: &mut Input,
        state: &mut Self::PartialState,
    ) -> ParseResult<Self::Output, <Input as StreamOnce>::Error>
    where
        M: ParseMode,
    {
        let depth = DEPTH.with(|d| {
            let n = d.get() + 1;
            d.set(n);
            n
        });
        let r = if depth > max_nesting() {
            note_refused();
            ParseResult::CommitErr(<Input as StreamOnce>::Error::from_error(
                input.position(),
                StreamError::message_static_message("expression nesting too deep"),
            ))
        } else {
            let Self(p) = self;
            ensure_sufficient(|| p.parse_mode(mode, input, state))
        };
        DEPTH.with(|d| d.set(d.get() - 1));
        // A token matcher advances past the token it rejects before
        // reporting a peek mismatch, so on failure the error's own
        // position is the exact one; on success the input's is.
        match &r {
            ParseResult::CommitOk(_) | ParseResult::PeekOk(_) => {
                note_furthest(input.position())
            }
            ParseResult::CommitErr(e) => note_furthest(e.position()),
            ParseResult::PeekErr(e) => note_furthest(e.error.position()),
        }
        r
    }

    #[inline]
    fn add_error(&mut self, error: &mut Tracked<<Input as StreamOnce>::Error>) {
        self.0.add_error(error)
    }

    #[inline]
    fn add_committed_expected_error(
        &mut self,
        error: &mut Tracked<<Input as StreamOnce>::Error>,
    ) {
        self.0.add_committed_expected_error(error)
    }

    #[inline]
    fn parser_count(&self) -> ErrorOffset {
        self.0.parser_count()
    }
}
