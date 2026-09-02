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
    /// a reserved word where a name was expected — kept beside its
    /// position. combine merges a refused alternative's message into
    /// the surrounding expectation set (a whole `let` reports
    /// "Unexpected `l`" at the statement's first column), so the
    /// reason is recorded here and reported when the failure lies at
    /// or before it.
    static REASON: RefCell<Option<(SourcePosition, CompactString)>> =
        const { RefCell::new(None) };
    /// Where the parse failed, set by the entry point's error mapping
    /// before [`parsing`] reports.
    static ERROR_POS: Cell<Option<SourcePosition>> = const { Cell::new(None) };
}

fn key(p: SourcePosition) -> (i32, i32) {
    (p.line, p.column)
}

/// Record why a name was refused at `pos`; a later refusal wins.
pub(super) fn note_reason(pos: SourcePosition, reason: CompactString) {
    REASON.with(|r| {
        let mut r = r.borrow_mut();
        if r.as_ref().is_none_or(|(p, _)| key(*p) <= key(pos)) {
            *r = Some((pos, reason));
        }
    })
}

/// Record where the parse failed.
pub(super) fn note_error_pos(pos: SourcePosition) {
    ERROR_POS.with(|p| p.set(Some(pos)))
}

/// Wrap a parse: clears the flags first, and reports the nesting
/// limit rather than combine's merged expectation set when that is
/// what actually stopped the parse, or the recorded reason when the
/// failure lies on the line of a refused name or before it (combine
/// reports a failed statement at its first column).
pub(super) fn parsing<T, E: std::fmt::Display>(
    f: impl FnOnce() -> Result<T, E>,
) -> Result<T, String> {
    clear_refused();
    REASON.with(|r| *r.borrow_mut() = None);
    ERROR_POS.with(|p| p.set(None));
    f().map_err(|e| {
        if refused() {
            return format!(
                "expression nesting too deep (limit {}, see \
                 graphix_compiler::expr::parser::set_max_nesting)",
                max_nesting()
            );
        }
        let reason = REASON.with(|r| r.borrow().clone());
        let err_pos = ERROR_POS.with(|p| p.get());
        match reason {
            Some((pos, reason)) if err_pos.is_none_or(|ep| ep.line <= pos.line) => {
                format!(
                    "{e}\n  note: at line: {}, column: {}: {reason}",
                    pos.line, pos.column
                )
            }
            _ => format!("{e}"),
        }
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
    Input: Stream,
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
