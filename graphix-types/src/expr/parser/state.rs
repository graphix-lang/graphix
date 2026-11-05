use super::input::Mark;
use crate::SourcePosition;
use compact_str::{CompactString, format_compact};
use netidx_value::parser::{Expected, Item, escapes};
use poolshark::local::LPooled;
use std::{
    fmt::{self, Write},
    sync::atomic::{AtomicUsize, Ordering},
};

/// What a pass of the parser keeps besides the parse.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Mode {
    /// The parse alone.
    Quick,
    /// The parse and what its failure needs reported: where it stopped
    /// and why.
    Diag,
    /// [`Mode::Diag`], and what was expected at this offset.
    Labels(usize),
}

/// Something the parse expected or met, as a report names it.
#[derive(Clone, PartialEq)]
pub(super) enum Info {
    Token(char),
    Text(CompactString),
}

impl Info {
    pub(super) const fn text(s: &'static str) -> Self {
        Info::Text(CompactString::const_new(s))
    }
}

impl fmt::Display for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Info::Token(c) => write!(f, "`{c}`"),
            Info::Text(s) => f.write_str(s),
        }
    }
}

/// What every failure at one point said: the token met there, what was
/// expected instead, and the refusals' messages.
#[derive(Default)]
pub(super) struct Labels {
    /// The token a failure there met, `Some(None)` the end of input.
    pub(super) token: Option<Option<char>>,
    pub(super) unexpected: LPooled<Vec<Info>>,
    pub(super) expected: LPooled<Vec<Info>>,
    pub(super) messages: LPooled<Vec<Info>>,
}

impl Labels {
    pub(super) fn expect(&mut self, i: Info) {
        self.expected.push(i)
    }

    pub(super) fn unexpect(&mut self, i: Info) {
        self.unexpected.push(i)
    }

    pub(super) fn message(&mut self, i: Info) {
        self.messages.push(i)
    }

    /// What one of netidx-value's parsers expected, or why it refused.
    pub(super) fn read(&mut self, e: Expected) {
        let info = |i: Item| match i {
            Item::Char(c) => Info::Token(c),
            Item::Text(s) => Info::text(s),
        };
        match e {
            Expected::OneOf(items) => items.iter().for_each(|i| self.expect(info(*i))),
            Expected::Escape(syntax) => {
                escapes(syntax).for_each(|i| self.expect(info(i)))
            }
            Expected::Refused(why) => self.message(Info::text(why)),
        }
    }
}

/// A value literal's failure past its start: where, the char met there
/// (`None` at the end of input), and what its parser expected.
#[derive(Clone, Copy)]
struct Literal {
    pos: SourcePosition,
    met: Option<char>,
    expected: Expected,
}

impl Literal {
    /// Whether `o`, a failure at the same point, says what this says.
    fn same(&self, o: &Literal) -> bool {
        self.met == o.met
            && match (self.expected, o.expected) {
                (Expected::OneOf(a), Expected::OneOf(b)) => a == b,
                (Expected::Escape(a), Expected::Escape(b)) => std::ptr::eq(a, b),
                (Expected::Refused(a), Expected::Refused(b)) => a == b,
                _ => false,
            }
    }

    /// What its failure said, as the failures at a reported point say.
    fn labels(&self) -> Labels {
        let mut l = Labels::default();
        if !matches!(self.expected, Expected::Refused(_)) {
            l.token = Some(self.met);
        }
        l.read(self.expected);
        l
    }
}

/// `v` without repeats, in the order each first appears.
fn unique(v: &[Info]) -> impl Iterator<Item = &Info> {
    v.iter().enumerate().filter(|(n, i)| !v[..*n].contains(i)).map(|(_, i)| i)
}

#[derive(Clone)]
struct Reason {
    pos: SourcePosition,
    /// The refused token's length in chars, when the reason explains
    /// a failure only inside that token.
    span: Option<usize>,
    reason: CompactString,
    said: Said,
}

/// Of what a [`Reason`] is said.
#[derive(Clone, Copy, PartialEq)]
enum Said {
    /// Of a refusal at its `pos`.
    Here,
    /// Of something read before this point, where the parse failed: a
    /// failure there is reported at the reason's `pos`.
    Before(SourcePosition),
    /// Of text read as a value literal to this point, which another
    /// reading may read otherwise: said only where no other reading
    /// parsed to it.
    Literal(SourcePosition),
}

impl Reason {
    /// Where it stands, as a later one wins.
    fn at(&self) -> (i32, i32) {
        match self.said {
            Said::Before(f) => key(f),
            Said::Here | Said::Literal(_) => key(self.pos),
        }
    }

    /// Whether it is of something read before `failure`.
    fn read_before(&self, failure: SourcePosition) -> bool {
        self.said == Said::Before(failure)
    }

    /// Whether it explains a failure at `failure`, where the furthest
    /// point a knot parsed to is `parsed`.
    fn explains(&self, failure: SourcePosition, parsed: Option<SourcePosition>) -> bool {
        match self.said {
            Said::Before(f) if f == failure => return true,
            Said::Literal(end) if parsed.is_some_and(|p| key(p) >= key(end)) => {
                return false;
            }
            Said::Here | Said::Before(_) | Said::Literal(_) => (),
        }
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

fn later(a: Option<SourcePosition>, b: SourcePosition) -> Option<SourcePosition> {
    match a {
        Some(a) if key(a) >= key(b) => Some(a),
        _ => Some(b),
    }
}

/// A parse's nesting count and, outside [`Mode::Quick`], what its report
/// needs.
pub(super) struct State {
    pub(super) mode: Mode,
    /// Recursion knots entered and not left.
    depth: usize,
    /// How deep what the knot in progress built so far goes, in knots and
    /// fold levels.
    below: usize,
    /// The deepest any knot or fold went, counted as the limit counts.
    reach: usize,
    /// Whether the limit refused a knot or fold since the measure in
    /// progress began.
    limited: bool,
    /// The furthest point a knot reached, by success or failure.
    furthest: Option<SourcePosition>,
    /// The furthest point a knot parsed to.
    furthest_ok: Option<SourcePosition>,
    /// Where the nesting limit stopped the parse.
    refused: Option<SourcePosition>,
    /// The furthest reason a rule refused something it could name.
    reason: Option<Reason>,
    /// The furthest value literal's failure past its start, which a
    /// lookahead or another alternative may roll back: what a report at
    /// its point says when nothing else does and nothing else failed there.
    literal: Option<Literal>,
    /// The furthest point another failure met: one no value literal's, or
    /// a literal's saying something else at the kept one's point.
    failed: Option<SourcePosition>,
    /// Under [`Mode::Labels`], what the failures there said.
    pub(super) labels: Labels,
    /// Inside a `not_followed_by`, whose failures expect nothing.
    pub(super) quiet: u32,
    /// Knots entered, which tests read as what a parse cost.
    #[cfg(test)]
    pub(super) knots: usize,
}

/// The state a measured parse set aside.
pub(super) struct Measure {
    below: usize,
    reach: usize,
    limited: bool,
    reason: Option<Reason>,
}

/// What a measured parse did to the state, which a replay of it does
/// again: its depth, and the reason it would leave noted were none noted
/// before it (a later one wins, so this alone decides what it leaves
/// noted after any).
pub(super) struct Effects {
    pub(super) depth: Depth,
    reason: Option<Reason>,
}

/// What a measured parse did to the depth bookkeeping.
#[derive(Clone, Copy)]
pub(super) struct Depth {
    /// What it added to the depth below the knot in progress.
    below: usize,
    /// How far past its starting depth its knots and folds went.
    reach: usize,
    /// Whether the limit refused a knot or fold in it, which makes the
    /// parse one of its starting depth alone.
    pub(super) limited: bool,
}

impl State {
    pub(super) fn new(mode: Mode) -> Self {
        Self {
            mode,
            depth: 0,
            below: 0,
            reach: 0,
            limited: false,
            furthest: None,
            furthest_ok: None,
            refused: None,
            reason: None,
            literal: None,
            failed: None,
            labels: Labels::default(),
            quiet: 0,
            #[cfg(test)]
            knots: 0,
        }
    }

    pub(super) fn diag(&self) -> bool {
        self.mode != Mode::Quick
    }

    /// Whether a failure at `off` is reported.
    pub(super) fn labels_at(&self, off: usize) -> bool {
        self.mode == Mode::Labels(off) && self.quiet == 0
    }

    /// Enter a knot at `pos`; false past [`max_nesting`], noted.
    pub(super) fn enter(&mut self, pos: SourcePosition) -> Result<usize, ()> {
        self.depth += 1;
        if self.depth > max_nesting() {
            self.depth -= 1;
            self.refuse(pos);
            return Err(());
        }
        self.reach = self.reach.max(self.depth);
        #[cfg(test)]
        {
            self.knots += 1
        }
        Ok(std::mem::replace(&mut self.below, 0))
    }

    /// Leave the knot [`State::enter`] entered, which `above` was built
    /// beside, having parsed to `end` (`ok`) or failed at `end`.
    pub(super) fn leave(&mut self, above: usize, ok: bool, end: SourcePosition) {
        self.depth -= 1;
        let below = if ok { self.below + 1 } else { 0 };
        self.below = above.max(below);
        if self.diag() {
            self.furthest = later(self.furthest, end);
            if ok {
                self.furthest_ok = later(self.furthest_ok, end);
            }
        }
    }

    /// Count a fold `n` levels deep over what the knot in progress parsed
    /// against [`max_nesting`] with the knots above it; past the limit
    /// the refusal is noted at `pos`.
    pub(super) fn fold_fits(&mut self, n: usize, pos: SourcePosition) -> bool {
        let below = self.below + n;
        if self.depth + below > max_nesting() {
            self.refuse(pos);
            false
        } else {
            self.below = below;
            self.reach = self.reach.max(self.depth + below);
            true
        }
    }

    /// Start a parse whose effects [`State::measured`] tells.
    pub(super) fn measure(&mut self) -> Measure {
        Measure {
            below: std::mem::take(&mut self.below),
            reach: std::mem::replace(&mut self.reach, self.depth),
            limited: std::mem::take(&mut self.limited),
            reason: self.reason.take(),
        }
    }

    /// What the parse [`State::measure`] started did to the state.
    pub(super) fn measured(&mut self, m: Measure) -> Effects {
        let depth = Depth {
            below: self.below,
            reach: self.reach - self.depth,
            limited: self.limited,
        };
        let effects =
            Effects { depth, reason: std::mem::replace(&mut self.reason, m.reason) };
        self.below = m.below;
        self.reach = m.reach;
        self.limited = m.limited;
        self.replay(&effects);
        effects
    }

    /// Whether a parse the limit refused nothing in, which did `d`, fits
    /// here.
    pub(super) fn fits(&self, d: Depth) -> bool {
        self.depth + d.reach <= max_nesting()
    }

    /// Recursion knots entered and not left.
    pub(super) fn depth(&self) -> usize {
        self.depth
    }

    /// Do to the state what a parse that did `e` does here.
    pub(super) fn replay(&mut self, e: &Effects) {
        let d = e.depth;
        self.below = self.below.max(d.below);
        self.reach = self.reach.max(self.depth + d.reach);
        self.limited |= d.limited;
        if let Some(r) = &e.reason {
            self.keep_reason(r.clone())
        }
    }

    fn refuse(&mut self, pos: SourcePosition) {
        self.limited = true;
        if self.refused.is_none_or(|p| key(p) < key(pos)) {
            self.refused = Some(pos)
        }
    }

    /// Record that the parse reached `pos`.
    pub(super) fn note_furthest(&mut self, pos: SourcePosition) {
        if self.diag() {
            self.furthest = later(self.furthest, pos)
        }
    }

    /// Record why something was refused at `pos`; a later refusal wins.
    pub(super) fn note_reason(
        &mut self,
        pos: SourcePosition,
        span: Option<usize>,
        reason: impl FnOnce() -> CompactString,
    ) {
        self.note(pos, span, Said::Here, reason)
    }

    /// [`State::note_reason`] of something read before `failed_at`, where
    /// the parse fails: a failure there is reported at `pos`.
    pub(super) fn note_read(
        &mut self,
        pos: SourcePosition,
        failed_at: SourcePosition,
        reason: &'static str,
    ) {
        self.note(pos, None, Said::Before(failed_at), || CompactString::const_new(reason))
    }

    /// [`State::note_reason`] of text read as a value literal from `pos`
    /// to `end`, which explains a failure only where no other reading
    /// parsed to `end`.
    pub(super) fn note_literal_reason(
        &mut self,
        pos: SourcePosition,
        end: SourcePosition,
        span: Option<usize>,
        reason: impl FnOnce() -> CompactString,
    ) {
        self.note(pos, span, Said::Literal(end), reason)
    }

    fn note(
        &mut self,
        pos: SourcePosition,
        span: Option<usize>,
        said: Said,
        reason: impl FnOnce() -> CompactString,
    ) {
        if self.diag() {
            self.keep_reason(Reason { pos, span, reason: reason(), said })
        }
    }

    /// Keep `r` unless the reason kept stands later.
    fn keep_reason(&mut self, r: Reason) {
        if self.reason.as_ref().is_none_or(|kept| kept.at() <= r.at()) {
            self.reason = Some(r);
        }
    }

    /// Record a value literal's failure past its start at `pos`, where it
    /// met `met`: a later one wins, and one saying something else at the
    /// same point is another failure there.
    pub(super) fn note_literal(
        &mut self,
        pos: SourcePosition,
        met: Option<char>,
        expected: Expected,
    ) {
        if !self.diag() {
            return;
        }
        let l = Literal { pos, met, expected };
        match self.literal {
            Some(kept) if key(kept.pos) > key(pos) => (),
            Some(kept) if key(kept.pos) == key(pos) && !kept.same(&l) => {
                self.note_failed(pos);
                self.literal = Some(l)
            }
            _ => self.literal = Some(l),
        }
    }

    /// Record a failure at `pos`.
    pub(super) fn note_failed(&mut self, pos: SourcePosition) {
        if self.diag() {
            self.failed = later(self.failed, pos)
        }
    }

    fn reason_explains(&self, pos: SourcePosition) -> bool {
        self.reason.as_ref().is_some_and(|r| r.explains(pos, self.furthest_ok))
    }

    /// Where a parse that failed at `err` reports, and whether that report
    /// names what was expected there.
    pub(super) fn report_at(&self, err: SourcePosition) -> (SourcePosition, bool) {
        if let Some(at) = self.nesting() {
            return (at, false);
        }
        let furthest = self.furthest.unwrap_or(err);
        let pos = if key(furthest) > key(err) { furthest } else { err };
        let read = self.reason.as_ref().is_some_and(|r| r.read_before(pos));
        (pos, pos == err && !read)
    }

    /// The nesting refusal that stopped the parse: one no knot parsed past.
    fn nesting(&self) -> Option<SourcePosition> {
        let passed = self.furthest_ok;
        self.refused.filter(|r| passed.is_none_or(|p| key(*r) >= key(p)))
    }

    /// Report a parse of `text` that failed at `err`, with what failures
    /// there said when [`State::report_at`] asked for them.
    pub(super) fn report(
        &self,
        text: &str,
        err: SourcePosition,
        labels: Option<&Labels>,
    ) -> ParseFailure {
        if let Some(at) = self.nesting() {
            let summary = format!(
                "expression nesting too deep (limit {}, see \
                 graphix_compiler::expr::parser::set_max_nesting)",
                max_nesting()
            );
            let mut msg = format!(
                "Parse error at line: {}, column: {}\n{summary}",
                at.line, at.column
            );
            let snippet = snippet(text, at);
            if !snippet.is_empty() {
                msg.push('\n');
                msg.push_str(&snippet);
            }
            return ParseFailure { pos: at, summary, msg };
        }
        let (mut pos, _) = self.report_at(err);
        let read = self
            .reason
            .as_ref()
            .filter(|r| r.read_before(pos))
            .map(|r| (r.pos, r.reason.clone()));
        let header = |p: SourcePosition| {
            format!("Parse error at line: {}, column: {}", p.line, p.column)
        };
        let render = |l: &Labels| Some(self.render(pos, l)).filter(|b| !b.is_empty());
        let literal = || {
            self.literal
                .filter(|l| l.pos == pos && !self.reason_explains(pos))
                .filter(|_| self.failed.is_none_or(|f| key(f) < key(pos)))
                .and_then(|l| render(&l.labels()))
        };
        let body = match read {
            Some(_) => None,
            None => labels.filter(|_| pos == err).and_then(render).or_else(literal),
        };
        let (mut msg, mut summary) = if let Some((at, reason)) = &read {
            pos = *at;
            (format!("{}\n{reason}", header(pos)), reason.to_string())
        } else if let Some(body) = body {
            (format!("{}\n{body}", header(pos)), body)
        } else {
            let summary = "the parser could not continue past this point".to_string();
            (format!("{}\n{summary}", header(pos)), summary)
        };
        let snippet = snippet(text, pos);
        if !snippet.is_empty() {
            msg.push('\n');
            msg.push_str(&snippet);
        }
        let reason = match read {
            Some(_) => None,
            None => self.reason.clone().filter(|r| r.explains(pos, self.furthest_ok)),
        };
        let note = match reason {
            Some(r) => Some((r.pos, r.reason)),
            None => unclosed_at_end(text, pos)
                .map(|(c, at)| (at, format_compact!("this `{c}` is not closed"))),
        };
        if let Some((at, reason)) = note {
            msg.push_str(&format!(
                "\n  note: at line: {}, column: {}: {reason}",
                at.line, at.column
            ));
            summary.push_str(&format!("\nnote: {reason}"));
        }
        ParseFailure { pos, msg, summary }
    }

    /// What the failures at `pos` said: the token met there unless a
    /// reason explains it or it is whitespace, what was expected, then
    /// the refusals, each line ended.
    fn render(&self, pos: SourcePosition, labels: &Labels) -> String {
        let token = labels.token;
        let explained = self.reason_explains(pos);
        let noise = token.is_some_and(|t| match t {
            Some(c) => explained || c.is_whitespace(),
            None => false,
        });
        let rest = !labels.unexpected.is_empty()
            || !labels.expected.is_empty()
            || !labels.messages.is_empty();
        let mut s = String::new();
        match token {
            Some(_) if noise && rest => (),
            Some(Some(c)) => writeln!(s, "Unexpected `{c}`").unwrap(),
            Some(None) => s.push_str("Unexpected end of input\n"),
            None => (),
        }
        for u in unique(&labels.unexpected) {
            writeln!(s, "Unexpected {u}").unwrap();
        }
        let n = unique(&labels.expected).count();
        for (i, e) in unique(&labels.expected).enumerate() {
            let sep = match i {
                0 => "Expected",
                _ if i + 1 < n => ",",
                _ => " or",
            };
            write!(s, "{sep} {e}").unwrap();
        }
        if n > 0 {
            s.push('\n');
        }
        for m in unique(&labels.messages) {
            writeln!(s, "{m}").unwrap();
        }
        s
    }
}

/// Default [`max_nesting`]. Counted in parser recursion knots and the
/// levels an iterative fold adds under them, not source constructs: one
/// level of `(1 + …)` costs four, three knots and its operator's fold.
pub const DEFAULT_MAX_NESTING: usize = 1000;

static MAX_NESTING: AtomicUsize = AtomicUsize::new(DEFAULT_MAX_NESTING);

/// How deeply a program may nest before the parser rejects it.
pub fn max_nesting() -> usize {
    MAX_NESTING.load(Ordering::Relaxed)
}

/// Raise or lower [`max_nesting`]. Process-global. The limit bounds the
/// depth of the AST, counted in parser knots and fold levels; types are
/// not bounded by it, so every walk keeps its own stack guard. Raising it
/// past what an unguarded walk survives trades an error for an abort.
pub fn set_max_nesting(depth: usize) {
    MAX_NESTING.store(depth, Ordering::Relaxed)
}

/// How a parse failed: the position reported and the message,
/// already rendered with the source line, a caret and any note.
#[derive(Debug)]
pub struct ParseFailure {
    pub pos: SourcePosition,
    pub msg: String,
    /// What went wrong and the note, for a reader shown where (an
    /// editor): no position, no source line.
    pub summary: String,
}

impl std::fmt::Display for ParseFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

/// Where `pos` is the end of `text`, the innermost bracket left open and
/// where it opens. Comments and string contents are skipped, the
/// expressions a string interpolates are not.
fn unclosed_at_end(text: &str, pos: SourcePosition) -> Option<(char, SourcePosition)> {
    #[derive(Clone, Copy)]
    enum Ctx {
        /// A bracket open in code.
        Open(char, SourcePosition),
        /// A string; `raw` holds the `#`s of a raw one, none for a plain one.
        Str { raw: Option<usize> },
        /// A `"""` template.
        Template,
        /// An interpolation, open until its `]`.
        Interp,
    }
    let mut stack: LPooled<Vec<Ctx>> = LPooled::take();
    let mut at = Mark::start();
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(text.chars());
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        let here = at.pos;
        let ahead = |k: usize| chars.get(i + k).copied();
        let mut advance = 1;
        match stack.last().copied() {
            Some(Ctx::Str { raw: None }) => match c {
                '\\' => advance = 2,
                '[' => stack.push(Ctx::Interp),
                '"' => {
                    stack.pop();
                }
                _ => (),
            },
            Some(Ctx::Str { raw: Some(n) }) => {
                if c == '"' && (1..=n).all(|k| ahead(k) == Some('#')) {
                    stack.pop();
                    advance = 1 + n;
                }
            }
            Some(Ctx::Template) => match c {
                '\\' if ahead(1) == Some('[') => {
                    stack.push(Ctx::Interp);
                    advance = 2;
                }
                '"' if ahead(1) == Some('"') && ahead(2) == Some('"') => {
                    stack.pop();
                    advance = 3;
                }
                _ => (),
            },
            _ => match c {
                '/' if ahead(1) == Some('/') => {
                    while i + advance < chars.len()
                        && !matches!(chars[i + advance], '\r' | '\n')
                    {
                        advance += 1;
                    }
                }
                '"' if ahead(1) == Some('"') && ahead(2) == Some('"') => {
                    stack.push(Ctx::Template);
                    advance = 3;
                }
                '"' => stack.push(Ctx::Str { raw: None }),
                'r' if matches!(ahead(1), Some('"' | '#')) => {
                    let mut n = 0;
                    while ahead(1 + n) == Some('#') {
                        n += 1;
                    }
                    if ahead(1 + n) == Some('"') {
                        stack.push(Ctx::Str { raw: Some(n) });
                        advance = 2 + n;
                    }
                }
                '(' | '[' | '{' => stack.push(Ctx::Open(c, here)),
                ')' | ']' | '}' => match stack.last() {
                    Some(Ctx::Interp) if c == ']' => {
                        stack.pop();
                    }
                    Some(Ctx::Open(..)) => {
                        stack.pop();
                    }
                    _ => (),
                },
                _ => (),
            },
        }
        for k in 0..advance {
            if let Some(c) = chars.get(i + k) {
                at.step(*c);
            }
        }
        i += advance;
    }
    if at.pos != pos {
        return None;
    }
    stack.iter().rev().find_map(|c| match c {
        Ctx::Open(c, at) => Some((*c, *at)),
        _ => None,
    })
}

/// The source line at `pos` with a caret under its column, the line
/// windowed around the caret when it is long. Past the last line (the
/// end of input) the caret follows the last line that holds anything.
/// `text`'s CRLFs were read as LF, so each "\r" and "\n" ends a line.
fn snippet(text: &str, pos: SourcePosition) -> String {
    let lines = || text.split(['\r', '\n']);
    // no line follows a final break
    let last = lines().count() - 1;
    let n = (pos.line.max(1) - 1) as usize;
    let (line, col) = match lines().nth(n).filter(|l| n < last || !l.is_empty()) {
        Some(line) => (line, (pos.column.max(1) - 1) as usize),
        None => match lines().filter(|l| !l.trim().is_empty()).last() {
            Some(line) => (line, line.chars().count()),
            None => return String::new(),
        },
    };
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
