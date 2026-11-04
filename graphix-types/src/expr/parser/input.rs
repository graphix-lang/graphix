use super::state::{Depth, Info, Labels, Mode, State};
use crate::{expr::Attr, stack::ensure_sufficient};
use ahash::AHashMap;
use combine::stream::position::SourcePosition;
use compact_str::CompactString;
use netidx_value::parser::rd::{self, Expected, Item};
use poolshark::local::LPooled;

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
    /// The start of a text.
    pub(super) fn start() -> Self {
        Self { off: 0, pos: SourcePosition::default(), after_cr: false, line_start: 0 }
    }

    pub(super) fn step(&mut self, c: char) {
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

/// An attribute's parse, for [`Input::remembered`].
struct Remembered {
    r: R<Attr>,
    end: Mark,
    depth: Depth,
    /// What its failures at the reported point said, if any did.
    said: Option<Labels>,
}

/// Where an attribute starts and, for a parse the nesting limit refused
/// something in, the depth it started at: a parse that met the limit
/// holds at that depth alone.
type AttrKey = (usize, Option<usize>);

#[cfg(test)]
thread_local! {
    /// Under test, read every attribute again, as a parse without
    /// [`Input::remembered`] would.
    pub(super) static FORGET: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// The text under parse and the parse's state.
pub(super) struct Input<'a> {
    text: &'a str,
    at: Mark,
    /// Printed data (`NaN`, a printed datetime) reads as a literal.
    pub(super) printed_data: bool,
    pub(super) g: State,
    attrs: LPooled<AHashMap<AttrKey, Remembered>>,
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

impl<'a> Input<'a> {
    pub(super) fn new(text: &'a str, mode: Mode, printed_data: bool) -> Self {
        Self {
            text,
            at: Mark::start(),
            printed_data,
            g: State::new(mode),
            attrs: LPooled::take(),
        }
    }

    pub(super) fn mark(&self) -> Mark {
        self.at
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

    /// How much the failures at the reported point said so far; `None`
    /// outside [`Mode::Labels`].
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

    /// A refusal of what started at `at`, standing there, saying why.
    pub(super) fn because_at(&mut self, at: Mark, why: &'static str) -> Fail {
        self.labels(at.off, false, |l| l.message(Info::text(why)));
        self.fail_at(at)
    }

    /// A failure here that names what it met.
    pub(super) fn unexpected(&mut self, what: &'static str) -> Fail {
        self.labels(self.at.off, false, |l| l.unexpect(Info::text(what)));
        self.fail()
    }

    /// A refusal here of what was read up to `pos`, saying why.
    pub(super) fn refuse(&mut self, pos: SourcePosition, why: &'static str) -> Fail {
        self.g.note_reason(pos, None, || CompactString::const_new(why));
        self.because(why)
    }

    /// [`Input::refuse`] of what started at `start`, standing there.
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
        self.matching(s, true)
    }

    /// [`Input::string`] of a comment's opener, which a failure never
    /// names as expected: a comment may stand wherever whitespace may.
    pub(super) fn opener(&mut self, s: &'static str) -> R<&'static str> {
        self.matching(s, false)
    }

    fn matching(&mut self, s: &'static str, named: bool) -> R<&'static str> {
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
        self.labels(start.off, true, |l| {
            if named {
                l.expect(Info::text(s))
            }
        });
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

    /// The whitespace char a word read just now needs after it, which a
    /// failure names.
    pub(super) fn word_end(&mut self) -> R<()> {
        match self.peek() {
            Some(c) if c.is_whitespace() => {
                self.at.step(c);
                Ok(())
            }
            _ => Err(self.expect(Info::text("whitespace"))),
        }
    }

    /// The word `w` and the whitespace char after it.
    pub(super) fn word_space(&mut self, w: &'static str) -> R<()> {
        self.string(w)?;
        self.word_end()
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

    /// [`Input::attempt`] whose nesting refusal, too, is a failure that
    /// consumed nothing: a probe of text that what follows on its failure
    /// reads again at fewer knots a level, as a select's arms read the
    /// patterns a map access's probe read as expressions.
    pub(super) fn probe<T>(&mut self, f: impl FnOnce(&mut Self) -> R<T>) -> R<T> {
        let m = self.at;
        f(self).map_err(|e| {
            self.at = m;
            Fail { fatal: false, ..e }
        })
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

    /// Succeed, consuming nothing, where `f` fails; where it does not, a
    /// failure that names the token it met.
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
                Err(self.mismatch())
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

    /// `f`, the rest of an attribute, parsed once where it starts. Its
    /// arguments are whole expressions that a lookahead may read before
    /// the rule that keeps them, so nested attributes would be read again
    /// at every level. A parse that starts here ends alike at any depth
    /// where the nesting limit refuses nothing in it, and a parse the
    /// limit refused something in ends alike at the same depth, so where
    /// one did it is replayed: its end, what it did to the depth, and what
    /// its failures at the reported point said.
    pub(super) fn remembered(&mut self, f: impl FnOnce(&mut Self) -> R<Attr>) -> R<Attr> {
        let start = self.at.off;
        #[cfg(test)]
        if FORGET.get() {
            return f(self);
        }
        if self.g.quiet > 0 {
            return f(self);
        }
        let depth = self.g.depth();
        let hit = match self.attrs.get(&(start, None)) {
            Some(m) if self.g.fits(m.depth) => Some(m),
            _ => self.attrs.get(&(start, Some(depth))),
        };
        if let Some(m) = hit {
            self.at = m.end;
            self.g.replay(m.depth);
            if let Some(said) = &m.said {
                let l = &mut self.g.labels;
                l.token = said.token.or(l.token);
                l.unexpected.extend(said.unexpected.iter().cloned());
                l.expected.extend(said.expected.iter().cloned());
                l.messages.extend(said.messages.iter().cloned());
            }
            return m.r.clone();
        }
        let held = self.hold();
        let token = self.g.labels.token.take();
        let measure = self.g.measure();
        let r = f(self);
        let d = self.g.measured(measure);
        let l = &mut self.g.labels;
        let met = l.token;
        l.token = met.or(token);
        let said = held
            .filter(|h| {
                met.is_some()
                    || l.unexpected.len() > h.unexpected
                    || l.expected.len() > h.expected
                    || l.messages.len() > h.messages
            })
            .map(|h| {
                let tail = |v: &[Info], n: usize| {
                    let mut t: LPooled<Vec<Info>> = LPooled::take();
                    t.extend(v[n..].iter().cloned());
                    t
                };
                Labels {
                    token: met,
                    unexpected: tail(&l.unexpected, h.unexpected),
                    expected: tail(&l.expected, h.expected),
                    messages: tail(&l.messages, h.messages),
                }
            });
        let m = Remembered { r: r.clone(), end: self.at, depth: d, said };
        self.attrs.insert((start, d.limited.then_some(depth)), m);
        r
    }

    /// Run one of netidx-value's parsers on the rest of the text: what it
    /// read, or where it stopped and what it expected there. A failure past
    /// the start consumed the text before it.
    pub(super) fn value_part<T>(
        &mut self,
        p: impl FnOnce(&'a str) -> Result<(T, usize), rd::Failure>,
    ) -> R<T> {
        let f = match p(self.rest()) {
            Ok((v, used)) => {
                self.advance(used);
                return Ok(v);
            }
            Err(f) => f,
        };
        let at = self.mark_at(self.at.off + f.offset);
        let expect = |l: &mut Labels, i: Item| match i {
            Item::Char(c) => l.expect(Info::Token(c)),
            Item::Text(s) => l.expect(Info::text(s)),
        };
        match f.expected {
            Expected::OneOf(items) => {
                self.labels(at.off, true, |l| items.iter().for_each(|i| expect(l, *i)))
            }
            Expected::Escape(must_esc, esc) => self.labels(at.off, true, |l| {
                rd::escapes(must_esc, esc).for_each(|c| expect(l, Item::Char(c)))
            }),
            Expected::Refused(why) => {
                self.labels(at.off, false, |l| l.message(Info::text(why)))
            }
        }
        if f.offset > 0 {
            self.at = at
        }
        Err(self.fail_at(at))
    }
}
