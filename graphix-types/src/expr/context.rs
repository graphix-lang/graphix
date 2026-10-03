//! Where an error arose and what it passed through on its way out.

use crate::expr::{Expr, Origin, Source};
use combine::stream::position::SourcePosition;
use poolshark::local::LPooled;
use std::fmt::{self, Write};
use triomphe::Arc;

/// An expression an error passed through on its way out. Built only by
/// [`At::at`], which also records the [`ErrorSite`].
pub struct ErrorContext(Expr);

impl ErrorContext {
    pub fn expr(&self) -> &Expr {
        &self.0
    }
}

impl fmt::Debug for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for ErrorContext {}

/// `at: <pos>[ in <source>], in: <the start of the expression>..`
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
        let complete = write!(head, "{}", self.0).is_ok();
        let Head(snippet, truncated) = head;
        if !complete && !truncated {
            return Err(fmt::Error);
        }
        let suffix = if truncated { ".." } else { "" };
        write!(f, "at: {}", self.0.pos)?;
        match &self.0.ori.source {
            Source::Internal(_) | Source::Unspecified => (),
            source => write!(f, " in {source}")?,
        }
        write!(f, ", in: {snippet}{suffix}")
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
}

impl At for anyhow::Error {
    fn at(self, spec: &Expr) -> Self {
        let cx = ErrorContext(spec.clone());
        match self.downcast_ref::<ErrorSite>() {
            Some(_) => self.context(cx),
            None => self.context(ErrorSite(cx)),
        }
    }
}

impl<T> At for anyhow::Result<T> {
    fn at(self, spec: &Expr) -> Self {
        self.map_err(|e| e.at(spec))
    }
}

/// Where a parse failed.
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
        write!(f, "parse error at {}", self.pos)?;
        match &self.ori.source {
            Source::Internal(_) | Source::Unspecified => Ok(()),
            source => write!(f, " in {source}"),
        }
    }
}

impl std::error::Error for ParserContext {}
