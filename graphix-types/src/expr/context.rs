//! Where an error arose and what it passed through on its way out.

use crate::expr::{Expr, Origin, Source};
use combine::stream::position::SourcePosition;
use poolshark::local::LPooled;
use std::fmt::{self, Write};
use triomphe::Arc;

/// An expression an error passed through on its way out. Built only by
/// [`At::at`], which also records the [`ErrorSite`].
pub struct ErrorContext(Expr, usize);

impl ErrorContext {
    pub fn expr(&self) -> &Expr {
        &self.0
    }
}

/// The most frames [`At::at`] records above an error's site: the chain
/// is torn down and searched recursively, and an AST is as deep as its
/// program is long.
const MAX_FRAMES: usize = 128;

impl fmt::Debug for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for ErrorContext {}

/// `at: <pos>[ in <source>], in: <the start of the expression>..`, the
/// start left out of an expression that names what a lowering bound,
/// which is no text the program wrote.
impl fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const MAX: usize = 38;
        /// Keeps the first `MAX` bytes written and stops the write.
        struct Head(LPooled<String>, bool);
        impl Write for Head {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                let room = MAX - self.0.len();
                if s.len() <= room {
                    return self.0.write_str(s);
                }
                let mut end = room;
                while !s.is_char_boundary(end) {
                    end += 1
                }
                self.0.push_str(&s[..end]);
                self.1 = true;
                Err(fmt::Error)
            }
        }
        let mut head = Head(LPooled::take(), false);
        // CR claude for claude: [bug/low] The snippet is the Expr's flat Display. For a
        // decorated expression that Display starts with write_leading_flat's newline
        // and puts each attribute on its own line. So a frame reads `in: ` and then
        // `#[serial]` and `s + 1` on lines of their own, and the 38-byte head spends
        // its room on the attributes. Fix: write `print::Bare(&self.0)`, which leaves
        // the decorations off, and have `Head` turn every newline into a space so a
        // snippet stays on one line even with a decorated child. probe:
        // design/review-2026-10-26/repro/t-print-decorated-error-snippet.gx
        // (t-print-02)
        let complete = self.0.mentions_lowered() || write!(head, "{}", self.0).is_ok();
        let Head(snippet, truncated) = head;
        if !complete && !truncated {
            return Err(fmt::Error);
        }
        let suffix = if truncated { ".." } else { "" };
        write!(f, "at: {}", self.0.pos)?;
        match &self.0.ori.source {
            Source::Program | Source::Unspecified => (),
            source => write!(f, " in {source}")?,
        }
        match snippet.is_empty() {
            true => Ok(()),
            false => write!(f, ", in: {snippet}{suffix}"),
        }
    }
}

/// The first expression an error passed through: where it arose. An
/// error chain holds one, under every [`ErrorContext`]; tooling
/// downcasts to it for the error's position.
pub struct ErrorSite(ErrorContext);

impl ErrorSite {
    pub fn expr(&self) -> &Expr {
        self.0.expr()
    }
}

impl fmt::Debug for ErrorSite {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Display for ErrorSite {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl std::error::Error for ErrorSite {}

/// Record that an error passed through the expression `spec`.
pub trait At {
    fn at(self, spec: &Expr) -> Self;

    /// Place an error that has no site at `spec`, adding no frame to one
    /// that has.
    fn sited(self, spec: &Expr) -> Self;
}

impl At for anyhow::Error {
    /// A frame for the expression the latest frame already names adds
    /// nothing, and neither does one for an expression a lowering built
    /// ([`Expr::is_lowered`]), which the program did not write; the latest
    /// frame is found first, the site only under none, and an error always
    /// gets its site.
    fn at(self, spec: &Expr) -> Self {
        let has_site = self.downcast_ref::<ErrorSite>().is_some()
            || self.downcast_ref::<ErrorContext>().is_some();
        if has_site && spec.is_lowered() {
            return self;
        }
        let frames = match self.downcast_ref::<ErrorContext>() {
            Some(ErrorContext(e, n)) if e.id == spec.id || *n >= MAX_FRAMES => {
                return self;
            }
            Some(ErrorContext(_, n)) => n + 1,
            None => match self.downcast_ref::<ErrorSite>() {
                Some(site) if site.expr().id == spec.id => return self,
                Some(_) => 1,
                None => return self.context(ErrorSite(ErrorContext(spec.clone(), 0))),
            },
        };
        self.context(ErrorContext(spec.clone(), frames))
    }

    fn sited(self, spec: &Expr) -> Self {
        match self.downcast_ref::<ErrorSite>().is_some()
            || self.downcast_ref::<ErrorContext>().is_some()
        {
            true => self,
            false => self.context(ErrorSite(ErrorContext(spec.clone(), 0))),
        }
    }
}

impl<T> At for anyhow::Result<T> {
    fn at(self, spec: &Expr) -> Self {
        self.map_err(|e| e.at(spec))
    }

    fn sited(self, spec: &Expr) -> Self {
        self.map_err(|e| e.sited(spec))
    }
}

/// Where in its source an error is: a parse's failure, or a check's
/// finding that only a position carries.
pub struct ParserContext {
    pub ori: Arc<Origin>,
    pub pos: SourcePosition,
}

impl fmt::Debug for ParserContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for ParserContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "at {}", self.pos)?;
        match &self.ori.source {
            Source::Program | Source::Unspecified => Ok(()),
            source => write!(f, " in {source}"),
        }
    }
}

impl std::error::Error for ParserContext {}
