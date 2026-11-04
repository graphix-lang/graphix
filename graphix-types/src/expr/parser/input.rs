use super::state::{Effects, Info, Labels, Mode, State};
use crate::{AddrMap, SourcePosition, expr::Expr, stack::ensure_sufficient};
use compact_str::CompactString;
use netidx_value::parser::{Expected, Failure};
use poolshark::local::LPooled;

/// A point in a text whose CRLFs were read as LF: its byte offset, its
/// position as an editor counts lines of the text as written (there every
/// "\r" and "\n" ends one), and where its line starts.
#[derive(Clone, Copy)]
pub(super) struct Mark {
    pub(super) off: usize,
    pub(super) pos: SourcePosition,
    line_start: usize,
}

impl Mark {
    /// The start of a text.
    pub(super) fn start() -> Self {
        Self { off: 0, pos: SourcePosition::default(), line_start: 0 }
    }

    pub(super) fn step(&mut self, c: char) {
        self.off += c.len_utf8();
        match c {
            '\r' | '\n' => {
                self.pos = SourcePosition { line: self.pos.line + 1, column: 1 };
                self.line_start = self.off
            }
            _ => self.pos.column += 1,
        }
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

/// An expression's parse, for [`Input::remembered`].
struct Remembered {
    r: R<Expr>,
    end: Mark,
    /// The depth it started at, where alone a parse the nesting limit
    /// refused something in holds.
    from: usize,
    effects: Effects,
    /// What its failures at the reported point said, if any did.
    said: Option<Labels>,
}

#[cfg(test)]
thread_local! {
    /// Under test, read every expression again, as a parse without
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
    /// Lookaheads and probes in progress, whose expressions what follows
    /// them may read again.
    speculating: u32,
    /// The expressions they parsed, by where each starts.
    memo: LPooled<AddrMap<Remembered>>,
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
            speculating: 0,
            memo: LPooled::take(),
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

    pub(super) fn fail_at(&mut self, at: Mark) -> Fail {
        self.g.note_failed(at.pos);
        Fail { at, fatal: false }
    }

    pub(super) fn fail(&mut self) -> Fail {
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
        self.speculate(f).map_err(|e| {
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
        let r = self.speculate(f);
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

    // XCR claude for claude: [perf/medium] The arms of a select read again
    // what the map-access probe in postfix_op (`select x{` read as `x{ k }`)
    // already parsed, and this memo covers only attributes, so another way
    // back into expressions is still exponential: a string pattern parses
    // its whole `"[..]"` or `"""\[..]"""` splice through `interpolated`
    // and refuses only after. `select x{ "[` nested N deep: 13.5G
    // instructions at N=16, 54G at N=18 (398 bytes), doubling a level; the
    // same text as runtime data through `str::parse` (parse_literal_value)
    // costs 15.4G at N=16, so a short untrusted string stalls the evaluator,
    // the formatter and the LSP. Main (combine) has it too, about 7x slower.
    // Fix the cause: remember what the probe parsed, by offset and depth,
    // so the arms replay it (it would replace this attribute-only memo and
    // its `AttrKey`, whose `Option<usize>` repeats `Remembered.depth.limited`);
    // or narrowly, refuse a string pattern at its first unescaped `[` (a
    // template's `\[`) before parsing the splice. Pin under a knot budget at
    // N=18. Found by review r3 of lane J. probes:
    // ~/ptmp/cr29/scratch/review-rd/r3-2/strpat/{sp_14,sp_16,sps_100,tp_14,rt_16}.gx,
    // RESULTS.txt (probe-reread-01)
    // 2026-11-04 claude: Fixed at the cause: Input::remembered keeps every expression
    // a lookahead or a probe parses (Input::speculate counts them), by where it
    // starts, and replays one an expr() reads again there: its end, its effects
    // (depth, and now the reason and literal failure it would leave noted, which
    // State::measure sets aside so a replay re-notes them; the attribute memo dropped
    // those) and what its failures said. It replaces the attribute memo and AttrKey:
    // an attribute's arguments are expressions a struct's lookahead or a probe read
    // first. The key is the offset alone; an entry the limit cut short holds at its
    // starting depth (Remembered.from). Exact over both repos: graphix fmt --stdout
    // of all 2941 .gx/.gxi, base-r14 against this, differs only in
    // t-parser-a-int-overflow-diag.gx's message (value-literal-labels-01); the gxfmt
    // harness and expr_spans pass. sp_18 costs 0.021 G instructions (base-r14 56 G),
    // rt_16 through str::parse 1.40 G with the stdlib (15.8 G). fmt of the corpus in
    // one process: 16.45 G against 16.53 G. Pins:
    // expr::parser::test::string_patterns_after_a_probe_parse_once (270 knots at
    // N=18, as source and as data; with FORGET 4980735) and
    // a_remembered_expression_parses_as_read_again (FORGET against the memo on every
    // cut, deletion and stray closer).
    // 2026-11-04 claude: Review r14: the replay's re-noting of the reason and the
    // literal failure changes the state only on a tie at one point (notes only grow
    // and a later one wins a tie), where it leaves what reading the expression again
    // leaves; no text is known to reach such a tie, so it is pinned at State:
    // expr::parser::test::a_replay_notes_its_reason_and_literal_failure_again (fails
    // without it). The cost left near the nesting limit: probe-limit-cost-01.
    // 2026-11-04 claude: Review r14 round 3: a replay re-notes the reason alone now;
    // a value literal's failure needs none (value-literal-labels-01's note), so
    // Effects carries the depth and the reason. Pin:
    // expr::parser::test::a_replay_notes_its_reason_again.
    /// `f`, a parse that what follows it may read again.
    fn speculate<T>(&mut self, f: impl FnOnce(&mut Self) -> R<T>) -> R<T> {
        self.speculating += 1;
        let r = f(self);
        self.speculating -= 1;
        r
    }

    // CR claude for claude: [perf/low] Near the nesting limit a remembered
    // parse is still superlinear, bounded by the limit: an entry the limit
    // cut short replays only at the depth it started at, and `select x{ "[`
    // nested near the limit reaches each inner offset at about 50 depths, a
    // parse each (fsp_150: 22896 offset-depth pairs over 449 offsets).
    // `graphix fmt --stdout` costs 34M / 50M / 202M / 345M / 1.20G / 1.56G
    // instructions at N = 95 / 100 / 105 / 110 / 150 / 195, level past 190; the
    // template form 1.90G at 150 and 2.46G at 195, on 3.9 KB; str::parse of
    // the ftp_150 text about 1.2G above N=20. Main 968fa9bc0 is exponential
    // there. Replaying a cut entry at another depth needs a rule for where a
    // deeper start is cut that is exact without the parse; until then such
    // a string costs about a second, not a stall. Found by review r14 of lane
    // K. probes: ~/ptmp/cr29/scratch/review-r14/K-r1-2/deep/{fsp,ftp}_*.gx,
    // rt_tp_150.gx (probe-limit-cost-01)
    /// `f`, an expression, parsed once where it starts when a lookahead or
    /// a probe parses it first: what follows them reads it again (a block,
    /// the arguments of an attribute a struct's lookahead read; a select's
    /// arms, the splices of the string patterns a map access's probe read),
    /// so nested ones would be read again at every level. A parse that
    /// starts here ends alike at any depth where the nesting limit refuses
    /// nothing in it, and a parse the limit refused something in ends
    /// alike at the same depth, so where one did it is replayed: its end,
    /// what it did to the state, and what its failures at the reported
    /// point said.
    pub(super) fn remembered(&mut self, f: impl FnOnce(&mut Self) -> R<Expr>) -> R<Expr> {
        #[cfg(test)]
        if FORGET.get() {
            return f(self);
        }
        if self.g.quiet > 0 || (self.speculating == 0 && self.memo.is_empty()) {
            return f(self);
        }
        let start = self.at.off;
        let depth = self.g.depth();
        let hit = self.memo.get(&start).filter(|m| match m.effects.depth.limited {
            true => m.from == depth,
            false => self.g.fits(m.effects.depth),
        });
        if let Some(m) = hit {
            self.at = m.end;
            self.g.replay(&m.effects);
            if let Some(said) = &m.said {
                let l = &mut self.g.labels;
                l.token = said.token.or(l.token);
                l.unexpected.extend(said.unexpected.iter().cloned());
                l.expected.extend(said.expected.iter().cloned());
                l.messages.extend(said.messages.iter().cloned());
            }
            return m.r.clone();
        }
        if self.speculating == 0 {
            return f(self);
        }
        let held = self.hold();
        let token = self.g.labels.token.take();
        let measure = self.g.measure();
        let r = f(self);
        let effects = self.g.measured(measure);
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
        let m = Remembered { r: r.clone(), end: self.at, from: depth, effects, said };
        self.memo.insert(start, m);
        r
    }

    /// Run one of netidx-value's parsers on the rest of the text: what it
    /// read, or where it stopped and what it expected there. A failure past
    /// the start consumed the text before it.
    pub(super) fn value_part<T>(
        &mut self,
        p: impl FnOnce(&'a str) -> Result<(T, usize), Failure>,
    ) -> R<T> {
        let f = match p(self.rest()) {
            Ok((v, used)) => {
                self.advance(used);
                return Ok(v);
            }
            Err(f) => f,
        };
        // XCR claude for claude: [bug/low] The labels this gives a value
        // literal's failure never reach a user: `let a = error:"\q";`,
        // `error:"\u{4g}"` and `error:"\u{d800}"` report only "the parser could
        // not continue past this point", at the right column, where a graphix
        // string's `"\q"` names the escapes it expected, so netidx's expected
        // lists and its surrogate and past-10FFFF reasons are lost. State::report
        // renders labels only where its position is `err`, which looks like the
        // filter that drops them. Pre-existing: base-r10 and base-r11 print the
        // same. probe: ~/ptmp/cr29/scratch/r11-Y/uescape-r2/gx/{q,u4g,usur}.gx
        // under `graphix --check` (value-literal-labels-01)
        // 2026-11-04 claude: The labels were said, but a later alternative's
        // success dropped them (primary's choice takes error as a reference, the
        // parse then fails at the colon) and the report sits at the furthest
        // point, where it gathers none. value_part now also notes a failure past
        // a literal's start in State (note_literal, the furthest kept like a
        // reason), and State::report says it where nothing else does:
        // Labels::read is the one rendering of netidx's Expected, for both.
        // error:"\q" reports Unexpected q and the escapes, u{4g} a hex digit or
        // }, u{d800} the surrogate and u{110000} the past-10FFFF reason. Also
        // changes an out-of-range integer literal's body from could-not-continue
        // to netidx's invalid unsigned integer, its note unchanged (u8:256). Pin:
        // expr::parser::test::a_value_literal_failure_says_what_was_expected
        // (failed before the change).
        // 2026-11-04 claude: Review r14: a literal's failure is now the report's
        // body only where no note explains the point, so u8:256, i8:-200, u8:-1
        // and duration:1e300y read as on main (could not continue, then the range
        // or unit note) and the census has no message change left; int_range_note
        // takes whitespace around the colon as the value parser does, so u8: 256
        // and u8 :256 name the range too. Pins:
        // expr::parser::test::an_out_of_range_integer_literal_names_its_range
        // (fails without either change) and a map access's string in
        // a_value_literal_failure_says_what_was_expected. A literal in a pattern
        // or an index: literal-pattern-labels-01; the duration note's exponent:
        // duration-exponent-note-01.
        // 2026-11-04 claude: Review r14 round 3: a literal's failure was said
        // where it was one reading of several, so a struct whose first field is
        // named like a literal's prefix ({duration: 5, {bytes: 1), {f64: 1), { a:
        // duration, {u8:\n300) reported the literal's failure, and {datetime:
        // "\q"} netidx's escapes for what is a graphix string. State::report now
        // says a literal's failure only where no other failure met its point
        // (State::failed: every Input::fail_at in a diag pass, and a second
        // literal failing at the same point saying something else); each of those
        // reads as on main again. int_range_note is main's again (no whitespace
        // around the colon), so {u8: 300 keeps main's 'this { is not closed'; u8:
        // 256 and u8:\n256 say the value parser's 'invalid unsigned integer'. The
        // literal is no longer replayed from the memo: kept positions only grow
        // and a tie is noted when it happens, so a replay changed nothing. Pins:
        // expr::parser::test::a_value_literal_failure_is_said_only_where_it_alone_failed
        // (fails without State::failed, and on {datetime: "\q"} without the tie)
        // and an_out_of_range_integer_literal_names_its_range (fails with the
        // spaced int_range_note). Main's own phantom range note at {u8:300:
        // int-range-field-01.
        // CR claude for claude: [bug/low] A literal's failure is said where its
        // reading is the only one that gets that far, though the text names no
        // literal: `{datetime: "\u{41}\q", b: 1}` reports netidx's escape list at
        // the q for a graphix string field (field a reports the u), `{error:
        // "\u{41}\u{d800}"}` the surrogate, `f(bytes: x)` (a labeled argument
        // missing its #) and `x{bytes: n}` "base64 decode failed", `f(duration:
        // 5)` an expected . or e, `[u8: x]` an expected digit. Main 968fa9bc0
        // says "the parser could not continue past this point" at the same
        // positions. Each text is invalid on every reading. Found by review r14
        // of lane K (final). probes: ~/ptmp/cr29/scratch/review-r14/K-r3-2/p/
        // {st_dt_u41q,st_err_u41sur,call_bytes,mapkey_bytes,call_dur,arr_u8}.gx
        // (show2.sh; residual.txt) (literal-reach-labels-01)
        let at = self.mark_at(self.at.off + f.offset);
        let token = !matches!(f.expected, Expected::Refused(_));
        self.labels(at.off, token, |l| l.read(f.expected));
        if f.offset == 0 {
            return Err(self.fail_at(at));
        }
        let met = self.text[at.off..].chars().next();
        self.g.note_literal(at.pos, met, f.expected);
        self.at = at;
        Err(Fail { at, fatal: false })
    }
}
