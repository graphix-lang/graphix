//! Where an error arose and what it passed through on its way out.

use crate::{
    SourcePosition,
    expr::{Expr, Origin, Source, parser, print::Bare},
};
use arcstr::ArcStr;
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

    fn source_text(&self) -> Option<&str> {
        let text = self.0.ori.text.strip_prefix('\u{feff}').unwrap_or(&self.0.ori.text);
        let offset = |pos: SourcePosition| {
            let line = parser::lines(text).nth(usize::try_from(pos.line - 1).ok()?)?;
            let column = usize::try_from(pos.column - 1).ok()?;
            let byte =
                line.char_indices().map(|(i, _)| i).chain([line.len()]).nth(column)?;
            Some(line.as_ptr() as usize - text.as_ptr() as usize + byte)
        };
        let start = offset(self.0.pos)?;
        let end = offset(self.0.end.get()?)?;
        (start < end).then(|| text.get(start..end)).flatten()
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
/// original expression recovered when a lowering rewrote its names and
/// the source span is available.
impl fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const MAX: usize = 38;
        /// Keeps the first `MAX` bytes written, on one line (a newline
        /// is one space), and stops the write.
        struct Head(LPooled<String>, bool);
        impl Head {
            fn push(&mut self, s: &str) -> fmt::Result {
                let room = MAX - self.0.len();
                if s.len() <= room {
                    self.0.push_str(s);
                    return Ok(());
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
        impl Write for Head {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                let mut lines = parser::lines(s);
                if let Some(first) = lines.next() {
                    self.push(first)?;
                }
                for line in lines {
                    if !(self.0.is_empty() || self.0.ends_with(' ')) {
                        self.push(" ")?;
                    }
                    self.push(line)?;
                }
                Ok(())
            }
        }
        let mut head = Head(LPooled::take(), false);
        let complete = if self.0.mentions_lowered() {
            self.source_text()
                .and_then(|source| parser::parse_one(source).ok())
                .is_none_or(|written| write!(head, "{}", Bare(&written)).is_ok())
        } else {
            write!(head, "{}", Bare(&self.0)).is_ok()
        };
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

    /// Place an error that has no site at `pos` in `ori`, a place only a
    /// position names (an interface item), adding nothing to one that has.
    fn placed(self, ori: &Arc<Origin>, pos: SourcePosition) -> Self;
}

/// Whether `e` knows where it arose.
fn has_site(e: &anyhow::Error) -> bool {
    e.downcast_ref::<ErrorSite>().is_some()
        || e.downcast_ref::<ErrorContext>().is_some()
        || e.downcast_ref::<ParserContext>().is_some()
}

impl At for anyhow::Error {
    /// A frame for the expression the latest frame already names adds
    /// nothing, and neither does one for an expression a lowering built
    /// ([`Expr::is_lowered`]), which the program did not write; the latest
    /// frame is found first, the site only under none, and an error always
    /// gets its site.
    fn at(self, spec: &Expr) -> Self {
        if spec.is_lowered() && has_site(&self) {
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
                None if self.downcast_ref::<ParserContext>().is_some() => 1,
                None => return self.context(ErrorSite(ErrorContext(spec.clone(), 0))),
            },
        };
        self.context(ErrorContext(spec.clone(), frames))
    }

    fn sited(self, spec: &Expr) -> Self {
        match has_site(&self) {
            true => self,
            false => self.context(ErrorSite(ErrorContext(spec.clone(), 0))),
        }
    }

    fn placed(self, ori: &Arc<Origin>, pos: SourcePosition) -> Self {
        match has_site(&self) {
            true => self,
            false => self.context(ParserContext { ori: ori.clone(), pos }),
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

    fn placed(self, ori: &Arc<Origin>, pos: SourcePosition) -> Self {
        self.map_err(|e| e.placed(ori, pos))
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

/// What a check tells above an error it explains: why it refused, or what
/// to annotate. A tool that shows an error's leaf alone leads it with
/// [`Told::lines`].
pub struct Told(Arc<Line>);

/// One thing told, and what was told under it in the chain.
struct Line {
    text: ArcStr,
    under: Option<Arc<Line>>,
}

impl Told {
    /// `e` with `text` told above it, unless the nearest thing told
    /// under it already says `text`.
    pub fn over(e: anyhow::Error, text: &str) -> anyhow::Error {
        let under = match e.downcast_ref::<Told>() {
            Some(told) if told.0.text == text => return e,
            told => told.map(|t| t.0.clone()),
        };
        e.context(Told(Arc::new(Line { text: text.into(), under })))
    }

    /// Everything told from here down the chain, outermost first.
    pub fn lines(&self) -> impl Iterator<Item = &str> {
        std::iter::successors(Some(&*self.0), |l| l.under.as_deref())
            .map(|l| l.text.as_str())
    }
}

impl fmt::Debug for Told {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Told {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.text)
    }
}

impl std::error::Error for Told {}

#[cfg(test)]
mod test {
    use super::{At, Origin, Told, parser};
    use crate::expr::{Expr, ExprKind};

    /// A note told again over itself, through a frame, is in the chain
    /// once; a different one between them keeps both.
    #[test]
    fn a_note_told_over_itself_is_told_once() {
        let at = |line| ExprKind::NoOp.to_expr(crate::SourcePosition { line, column: 1 });
        let e = Told::over(anyhow::anyhow!("leaf").at(&at(2)), "annotate `fs`");
        let e = Told::over(e.at(&at(1)), "annotate `fs`");
        let message = format!("{e:#}");
        assert_eq!(message.matches("annotate `fs`").count(), 1, "{message}");
        let e = Told::over(Told::over(e, "annotate `g`"), "annotate `fs`");
        let told = e.downcast_ref::<Told>().unwrap();
        assert_eq!(
            told.lines().collect::<Vec<_>>(),
            ["annotate `fs`", "annotate `g`", "annotate `fs`"]
        );
    }

    #[test]
    fn a_rewritten_frame_keeps_its_source_text() {
        fn lower(expr: &Expr) -> Expr {
            let mut expr = expr.map_children(&mut lower);
            if matches!(expr.kind, ExprKind::Ref { .. }) {
                expr.kind = ExprKind::Ref { name: ["#seq_snapshot"].into() };
            }
            expr
        }
        for source in [
            "\u{feff}let a = 1; a + \"s\"",
            "let a = 1; a + \"s\"",
            "let a = \"é\";\r\na +\r\n\"s\"",
            "let a = \"é\";\r\n{\r\n0;\r\n// comment\r\na + \"s\"\r\n}",
        ] {
            let parsed =
                parser::parse(Origin { text: source.into(), ..Origin::default() })
                    .unwrap();
            let e = anyhow::anyhow!("boom").at(&lower(parsed.last().unwrap()));
            let message = compact_str::format_compact!("{e}");
            assert!(message.contains("in:"), "{message}");
            assert!(message.contains("a + \"s\""), "{message}");
            assert!(
                !message.contains("#seq") && !message.contains("comment"),
                "{message}"
            );
        }
    }

    #[test]
    fn a_frame_names_its_expression_on_one_line() {
        let bind: Expr = "let x =\n    #[serial]\n    s + 1".parse().unwrap();
        let ExprKind::Bind(b) = &bind.kind else { panic!("not a bind: {bind}") };
        let e = anyhow::anyhow!("boom").at(&b.value).at(&bind);
        let frames: Vec<String> = e.chain().map(|c| c.to_string()).collect();
        let snippets: Vec<&str> = frames
            .iter()
            .filter_map(|f| f.split_once(", in: ").map(|(_, s)| s))
            .collect();
        assert_eq!(snippets, ["let x = #[serial] s + 1", "s + 1"], "{frames:?}");
    }
}
