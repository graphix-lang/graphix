use super::grow::{Info, Labels, Mode, State};
use crate::stack::ensure_sufficient;
use combine::{
    ParseResult, easy, error::StringStreamError, stream::position::SourcePosition,
};
use compact_str::{CompactString, format_compact};

/// A point in the text: its byte offset, its position as an editor counts
/// lines (a line ends at "\r\n", "\r" or "\n"), and where its line starts.
#[derive(Clone, Copy)]
pub(super) struct Mark {
    pub(super) off: usize,
    pub(super) pos: SourcePosition,
    after_cr: bool,
    line_start: usize,
}

impl Mark {
    fn step(&mut self, c: char) {
        self.off += c.len_utf8();
        match c {
            '\n' if self.after_cr => self.line_start = self.off,
            '\r' | '\n' => {
                self.pos = SourcePosition { line: self.pos.line + 1, column: 1 };
                self.line_start = self.off
            }
            _ => self.pos.column += 1,
        }
        self.after_cr = c == '\r';
    }
}

/// A failed parse: where it failed, and whether nothing may try another
/// way (the nesting limit). A rule that failed after consuming input
/// leaves the input moved, and no other alternative is tried; one that
/// failed without consuming leaves it where it was.
#[derive(Clone, Copy)]
pub(super) struct Fail {
    pub(super) at: Mark,
    pub(super) fatal: bool,
}

pub(super) type R<T> = Result<T, Fail>;

/// The lengths of [`Labels`] at a point.
#[derive(Clone, Copy)]
pub(super) struct Held {
    start: usize,
    unexpected: usize,
    expected: usize,
    messages: usize,
}

/// The text under parse and the parse's state.
pub(super) struct Input<'a> {
    text: &'a str,
    at: Mark,
    /// Printed data (`NaN`, a printed datetime) reads as a literal.
    pub(super) printed_data: bool,
    pub(super) g: State,
}

/// The first alternative that does not fail without consuming input; a
/// failure of them all is the furthest any reached.
macro_rules! choice {
    ($inp:expr, $($alt:expr),+ $(,)?) => {{
        let inp = &mut *$inp;
        let mut best = None;
        let held = inp.hold();
        'choice: {
            $(
                let tried = inp.hold();
                match inp.alt(&mut best, $alt) {
                    Ok(Some(v)) => {
                        inp.drop_since(held);
                        break 'choice Ok(v);
                    }
                    Ok(None) => (),
                    Err(e) => {
                        inp.drop_between(held, tried);
                        break 'choice Err(e);
                    }
                }
            )+
            Err(best.unwrap())
        }
    }};
}
pub(super) use choice;

/// Run one of netidx-value's combine parsers on the rest of the text: over
/// a plain `&str` in a quick pass, over `easy` where the failure is
/// reported.
macro_rules! bridge {
    ($inp:expr, $p:expr) => {
        $inp.bridge(
            |s| combine::Parser::parse_stream(&mut $p, s),
            |s| combine::Parser::parse_stream(&mut $p, s),
        )
    };
}
pub(super) use bridge;

type EasyResult<'a, T> = ParseResult<T, easy::ParseError<&'a str>>;

impl<'a> Input<'a> {
    pub(super) fn new(text: &'a str, mode: Mode, printed_data: bool) -> Self {
        let pos = SourcePosition::default();
        Self {
            text,
            at: Mark { off: 0, pos, after_cr: false, line_start: 0 },
            printed_data,
            g: State::new(mode),
        }
    }

    pub(super) fn mark(&self) -> Mark {
        self.at
    }

    pub(super) fn reset(&mut self, m: Mark) {
        self.at = m
    }

    pub(super) fn pos(&self) -> SourcePosition {
        self.at.pos
    }

    pub(super) fn rest(&self) -> &'a str {
        &self.text[self.at.off..]
    }

    pub(super) fn peek(&self) -> Option<char> {
        self.rest().chars().next()
    }

    /// Whether the input moved since `m`.
    pub(super) fn moved(&self, m: &Mark) -> bool {
        self.at.off != m.off
    }

    pub(super) fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.at.step(c);
        Some(c)
    }

    fn advance(&mut self, bytes: usize) {
        let end = self.at.off + bytes;
        while self.at.off < end {
            self.bump();
        }
    }

    /// The mark at `off`, at or after the input's.
    fn mark_at(&self, off: usize) -> Mark {
        let mut m = self.at;
        for c in self.text[m.off..off].chars() {
            m.step(c)
        }
        m
    }

    /// Whether nothing but whitespace precedes `m` on its line.
    pub(super) fn starts_line_at(&self, m: &Mark) -> bool {
        self.text[m.line_start..m.off].chars().all(char::is_whitespace)
    }

    /// The text since `m`.
    pub(super) fn since(&self, m: &Mark) -> &'a str {
        &self.text[m.off..self.at.off]
    }

    pub(super) fn fail_at(&self, at: Mark) -> Fail {
        Fail { at, fatal: false }
    }

    pub(super) fn fail(&self) -> Fail {
        self.fail_at(self.at)
    }

    /// What failures at the reported point said so far, when a parse
    /// from here could still fail past here.
    pub(super) fn hold(&self) -> Option<Held> {
        match self.g.mode {
            Mode::Labels(_) => {
                let l = &self.g.labels;
                Some(Held {
                    start: self.at.off,
                    unexpected: l.unexpected.len(),
                    expected: l.expected.len(),
                    messages: l.messages.len(),
                })
            }
            _ => None,
        }
    }

    /// Forget what failures said since `held`: a parse that went on past
    /// where it started absorbed them.
    pub(super) fn drop_since(&mut self, held: Option<Held>) {
        if let Some(h) = held
            && self.g.mode != Mode::Labels(h.start)
        {
            let l = &mut self.g.labels;
            l.unexpected.truncate(h.unexpected);
            l.expected.truncate(h.expected);
            l.messages.truncate(h.messages);
        }
    }

    /// Forget what failures said between `from` and `to`: what the
    /// alternatives before one that failed committed said.
    pub(super) fn drop_between(&mut self, from: Option<Held>, to: Option<Held>) {
        if let (Some(a), Some(b)) = (from, to) {
            let l = &mut self.g.labels;
            l.unexpected.drain(a.unexpected..b.unexpected);
            l.expected.drain(a.expected..b.expected);
            l.messages.drain(a.messages..b.messages);
        }
    }

    /// Record what a failure at `off` said; `token` where it met the token
    /// there.
    fn labels(&mut self, off: usize, token: bool, f: impl FnOnce(&mut Labels)) {
        if self.g.labels_at(off) {
            if token {
                self.g.labels.token = Some(self.text[off..].chars().next());
            }
            f(&mut self.g.labels)
        }
    }

    /// A failure at the token ahead.
    pub(super) fn mismatch(&mut self) -> Fail {
        self.labels(self.at.off, true, |_| ());
        self.fail()
    }

    /// A failure at the token ahead, where `what` was expected.
    pub(super) fn expect(&mut self, what: Info) -> Fail {
        self.labels(self.at.off, true, |l| l.expect(what));
        self.fail()
    }

    /// A refusal here, saying why.
    pub(super) fn because(&mut self, why: &'static str) -> Fail {
        self.because_at(self.at, why)
    }

    /// A refusal at `at`, saying why: an `and_then`'s, which stands where
    /// what it refused started.
    pub(super) fn because_at(&mut self, at: Mark, why: &'static str) -> Fail {
        self.labels(at.off, false, |l| l.message(Info::text(why)));
        self.fail_at(at)
    }

    /// A failure here that names what it met.
    pub(super) fn unexpected(&mut self, what: &'static str) -> Fail {
        self.labels(self.at.off, false, |l| l.unexpect(Info::text(what)));
        self.fail()
    }

    /// Note why something was refused at `pos`, and refuse here: `refuse`.
    pub(super) fn refuse(&mut self, pos: SourcePosition, why: &'static str) -> Fail {
        self.g.note_reason(pos, None, || CompactString::const_new(why));
        self.because(why)
    }

    /// [`Input::refuse`] as `and_then` refuses: the failure stands where
    /// the refused construct started.
    pub(super) fn refuse_from(
        &mut self,
        start: Mark,
        pos: SourcePosition,
        why: &'static str,
    ) -> Fail {
        self.g.note_reason(pos, None, || CompactString::const_new(why));
        self.because_at(start, why)
    }

    /// A refusal of something read before the input's point, at `pos`.
    pub(super) fn refuse_read(&mut self, pos: SourcePosition, why: &'static str) -> Fail {
        self.g.note_read(pos, self.at.pos, why);
        self.because(why)
    }

    pub(super) fn token(&mut self, c: char) -> R<char> {
        match self.peek() {
            Some(t) if t == c => {
                self.at.step(c);
                Ok(c)
            }
            _ => Err(self.expect(Info::Token(c))),
        }
    }

    pub(super) fn satisfy(&mut self, f: impl FnOnce(char) -> bool) -> R<char> {
        match self.peek() {
            Some(c) if f(c) => {
                self.at.step(c);
                Ok(c)
            }
            _ => Err(self.mismatch()),
        }
    }

    /// The longest run of chars satisfying `f`, maybe none.
    pub(super) fn take_while(&mut self, f: impl Fn(char) -> bool) -> &'a str {
        let m = self.at;
        while let Some(c) = self.peek()
            && f(c)
        {
            self.at.step(c)
        }
        self.since(&m)
    }

    /// [`Input::take_while`], at least one.
    pub(super) fn take_while1(&mut self, f: impl Fn(char) -> bool) -> R<&'a str> {
        match self.take_while(f) {
            "" => Err(self.mismatch()),
            s => Ok(s),
        }
    }

    /// `s`, committed once its first char matched.
    pub(super) fn string(&mut self, s: &'static str) -> R<&'static str> {
        if self.rest().starts_with(s) {
            self.advance(s.len());
            return Ok(s);
        }
        let start = self.at;
        let mut want = s.chars();
        while let (Some(c), Some(w)) = (self.peek(), want.next())
            && c == w
        {
            self.at.step(c)
        }
        self.labels(start.off, true, |l| l.expect(Info::text(s)));
        Err(self.fail_at(start))
    }

    pub(super) fn eof(&mut self) -> R<()> {
        match self.peek() {
            None => Ok(()),
            Some(_) => Err(self.expect(Info::text("end of input"))),
        }
    }

    pub(super) fn spaces(&mut self) {
        self.take_while(char::is_whitespace);
    }

    pub(super) fn spaces1(&mut self) -> R<()> {
        self.satisfy(char::is_whitespace)?;
        Ok(self.spaces())
    }

    /// `t` after any whitespace.
    pub(super) fn sptoken(&mut self, t: char) -> R<char> {
        self.spaces();
        self.token(t)
    }

    /// `s` after any whitespace.
    pub(super) fn spstring(&mut self, s: &'static str) -> R<&'static str> {
        self.spaces();
        self.string(s)
    }

    /// Run `f`; a failure is one that consumed nothing.
    pub(super) fn attempt<T>(&mut self, f: impl FnOnce(&mut Self) -> R<T>) -> R<T> {
        let m = self.at;
        let r = f(self);
        if let Err(e) = &r
            && !e.fatal
        {
            self.at = m
        }
        r
    }

    /// `f`, or `None` where it failed without consuming.
    pub(super) fn optional<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> R<T>,
    ) -> R<Option<T>> {
        let m = self.at;
        let held = self.hold();
        match f(self) {
            Ok(v) => {
                self.drop_since(held);
                Ok(Some(v))
            }
            Err(e) if !e.fatal && !self.moved(&m) => {
                self.drop_since(held);
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    /// [`Input::optional`] of an alternative of a choice, whose failure
    /// [`choice`] keeps the furthest of in `best`.
    pub(super) fn alt<T>(
        &mut self,
        best: &mut Option<Fail>,
        f: impl FnOnce(&mut Self) -> R<T>,
    ) -> R<Option<T>> {
        let m = self.at;
        match f(self) {
            Ok(v) => Ok(Some(v)),
            Err(e) if !e.fatal && !self.moved(&m) => {
                if best.is_none_or(|b| b.at.off < e.at.off) {
                    *best = Some(e)
                }
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    /// `f` repeated until it fails without consuming, each result to
    /// `each`.
    pub(super) fn many<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> R<T>,
        mut each: impl FnMut(T),
    ) -> R<()> {
        let start = self.at.off;
        loop {
            let held = self.hold();
            match self.optional(&mut f)? {
                Some(v) => each(v),
                None => {
                    // what stopped a run that read something is no
                    // alternative to what follows it
                    if self.at.off != start {
                        self.drop_between(held, self.hold())
                    }
                    return Ok(());
                }
            }
        }
    }

    /// `f`'s result, consuming nothing.
    pub(super) fn look_ahead<T>(&mut self, f: impl FnOnce(&mut Self) -> R<T>) -> R<T> {
        let m = self.at;
        let held = self.hold();
        let r = f(self);
        if r.is_ok() {
            self.drop_since(held)
        }
        if r.as_ref().is_ok() || r.as_ref().is_err_and(|e| !e.fatal) {
            self.at = m
        }
        r
    }

    /// Succeed, consuming nothing, where `f` fails.
    pub(super) fn not_followed_by<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> R<T>,
    ) -> R<()> {
        let m = self.at;
        self.g.quiet += 1;
        let r = f(self);
        self.g.quiet -= 1;
        match r {
            Err(e) if e.fatal => Err(e),
            Err(_) => {
                self.at = m;
                Ok(())
            }
            Ok(_) => {
                self.at = m;
                Err(self.fail())
            }
        }
    }

    /// Run a recursion knot: under [`ensure_sufficient`] and counted
    /// against the nesting limit.
    pub(super) fn knot<T>(&mut self, f: impl FnOnce(&mut Self) -> R<T>) -> R<T> {
        let Ok(above) = self.g.enter(self.at.pos) else {
            self.g.note_furthest(self.at.pos);
            return Err(Fail { at: self.at, fatal: true });
        };
        let r = ensure_sufficient(|| f(self));
        let end = match &r {
            Ok(_) => self.at.pos,
            Err(e) => e.at.pos,
        };
        self.g.leave(above, r.is_ok(), end);
        r
    }

    /// Count a fold `n` levels deep against the nesting limit, refused at
    /// `pos`.
    pub(super) fn fold_fits(&mut self, n: usize, pos: SourcePosition) -> R<()> {
        match self.g.fold_fits(n, pos) {
            true => Ok(()),
            false => Err(Fail { at: self.at, fatal: true }),
        }
    }

    /// Run a parser of netidx-value on the rest of the text.
    pub(super) fn bridge<T>(
        &mut self,
        quick: impl FnOnce(&mut &'a str) -> ParseResult<T, StringStreamError>,
        explain: impl FnOnce(&mut easy::Stream<&'a str>) -> EasyResult<'a, T>,
    ) -> R<T> {
        let rest = self.rest();
        if self.g.mode == Mode::Quick {
            let mut s = rest;
            let r = quick(&mut s);
            let used = rest.len() - s.len();
            return match r {
                ParseResult::CommitOk(v) | ParseResult::PeekOk(v) => {
                    self.advance(used);
                    Ok(v)
                }
                ParseResult::PeekErr(_) => Err(self.fail()),
                ParseResult::CommitErr(_) => {
                    self.advance(used);
                    Err(self.fail())
                }
            };
        }
        let mut s = easy::Stream(rest);
        let r = explain(&mut s);
        let used = rest.len() - s.0.len();
        let (errors, commit) = match r {
            ParseResult::CommitOk(v) | ParseResult::PeekOk(v) => {
                self.advance(used);
                return Ok(v);
            }
            ParseResult::PeekErr(e) => (e.error, false),
            ParseResult::CommitErr(e) => (e, true),
        };
        let at = self.mark_at(self.at.off + errors.position.translate_position(rest));
        if self.g.labels_at(at.off) {
            for e in errors.errors {
                let l = &mut self.g.labels;
                match e {
                    easy::Error::Unexpected(easy::Info::Token(c)) => {
                        l.token = Some(Some(c))
                    }
                    easy::Error::Unexpected(easy::Info::Static("end of input")) => {
                        l.token = Some(None)
                    }
                    easy::Error::Unexpected(i) => l.unexpect(info(i)),
                    easy::Error::Expected(i) => l.expect(info(i)),
                    easy::Error::Message(i) => l.message(info(i)),
                    easy::Error::Other(e) => {
                        l.message(Info::Text(format_compact!("{e}")))
                    }
                }
            }
        }
        if commit {
            self.advance(used)
        }
        Err(self.fail_at(at))
    }
}

fn info(i: easy::Info<char, &str>) -> Info {
    match i {
        easy::Info::Token(c) => Info::Token(c),
        easy::Info::Range(r) => Info::Text(format_compact!("`{r}`")),
        easy::Info::Owned(s) => Info::Text(s.into()),
        easy::Info::Static(s) => Info::text(s),
    }
}
