use crate::{
    expr::{
        Decorations, Expr, Origin, Sig,
        parser::{parse, parse_sig},
        print::{PrettyBuf, PrettyDisplay},
    },
    format_with_flags,
};
use anyhow::{Result, bail};
use enumflags2::BitFlags;
use poolshark::local::LPooled;
use std::fmt::Write;

pub const DEFAULT_WIDTH: usize = 80;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    /// a `.gx` program or module
    Program,
    /// a `.gxi` interface
    Interface,
}

impl SourceKind {
    pub fn of_path(path: &std::path::Path) -> Self {
        match path.extension().and_then(|e| e.to_str()) {
            Some("gxi") => Self::Interface,
            _ => Self::Program,
        }
    }
}

enum Parsed {
    Program(triomphe::Arc<[Expr]>),
    Interface(Sig),
}

impl Parsed {
    fn new(kind: SourceKind, text: &str) -> Result<Self> {
        let ori = Origin::from_str(text);
        Ok(match kind {
            SourceKind::Program => Self::Program(parse(ori)?),
            SourceKind::Interface => Self::Interface(parse_sig(ori)?),
        })
    }

    fn print(&self, buf: &mut PrettyBuf) -> Result<()> {
        match self {
            Self::Program(exprs) => {
                for (i, e) in exprs.iter().enumerate() {
                    e.fmt_pretty(buf)?;
                    if i < exprs.len() - 1 {
                        buf.kill_newline();
                        writeln!(buf, ";")?
                    }
                }
            }
            Self::Interface(sig) => sig.fmt_pretty_inner(buf)?,
        }
        Ok(())
    }

    fn decorations(&self) -> LPooled<Vec<Decorations>> {
        let mut acc: LPooled<Vec<Decorations>> = LPooled::take();
        if let Self::Program(exprs) = self {
            for e in exprs.iter() {
                e.fold((), &mut |(), e| {
                    if let Some(d) = &e.dec {
                        acc.push((**d).clone())
                    }
                })
            }
        }
        acc
    }

    /// The first place `other` says something else, as the text each
    /// side prints there.
    fn difference(&self, other: &Self) -> Option<(String, String)> {
        fn children(e: &Expr) -> Vec<Expr> {
            let mut acc = vec![];
            e.for_each_child(&mut |c| acc.push(c.clone()));
            acc
        }
        fn smallest(a: &Expr, b: &Expr) -> (String, String) {
            let (ac, bc) = (children(a), children(b));
            if ac.len() == bc.len() {
                if let Some((a, b)) = ac.iter().zip(bc.iter()).find(|(a, b)| a != b) {
                    return smallest(a, b);
                }
            }
            (format!("{}: {a}", a.pos), format!("{}: {b}", b.pos))
        }
        fn count(what: &str, a: usize, b: usize) -> Option<(String, String)> {
            (a != b).then(|| (format!("{a} {what}"), format!("{b} {what}")))
        }
        match (self, other) {
            (Self::Program(a), Self::Program(b)) => {
                let differ = a.iter().zip(b.iter()).find(|(a, b)| a != b);
                differ
                    .map(|(a, b)| smallest(a, b))
                    .or_else(|| count("expressions", a.len(), b.len()))
            }
            (Self::Interface(a), Self::Interface(b)) => {
                let differ = a.items.iter().zip(b.items.iter()).find(|(a, b)| a != b);
                differ
                    .map(|(a, b)| (a.to_string(), b.to_string()))
                    .or_else(|| count("items", a.len(), b.len()))
            }
            (Self::Program(_), Self::Interface(_))
            | (Self::Interface(_), Self::Program(_)) => unreachable!(),
        }
    }
}

fn layout(kind: SourceKind, text: &str, width: usize) -> Result<(Parsed, PrettyBuf)> {
    let parsed = Parsed::new(kind, text)?;
    let mut buf = PrettyBuf::new(width);
    format_with_flags(BitFlags::empty(), || parsed.print(&mut buf))?;
    Ok((parsed, buf))
}

/// `format_source` without the reparse: for debugging the formatter.
pub fn format_source_unchecked(
    kind: SourceKind,
    text: &str,
    width: usize,
) -> Result<LPooled<String>> {
    Ok(layout(kind, text, width)?.1.buf)
}

/// `text` laid out canonically. The result is reparsed and refused unless
/// it says exactly what `text` said, comments and attributes included.
pub fn format_source(
    kind: SourceKind,
    text: &str,
    width: usize,
) -> Result<LPooled<String>> {
    let (parsed, buf) = layout(kind, text, width)?;
    let reparsed = match Parsed::new(kind, &buf.buf) {
        Ok(p) => p,
        Err(e) => bail!("formatter bug: the formatted text does not parse: {e:?}"),
    };
    if let Some((was, now)) = parsed.difference(&reparsed) {
        bail!(
            "formatter bug: the formatted text says something else\nwas {was}\nnow {now}"
        )
    }
    if parsed.decorations() != reparsed.decorations() {
        bail!("formatter bug: the formatted text lost a comment or an attribute")
    }
    Ok(buf.buf)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stable(kind: SourceKind, src: &str) {
        for width in [0, DEFAULT_WIDTH] {
            let once = format_source(kind, src, width).unwrap();
            let twice = format_source(kind, &once, width).unwrap();
            assert_eq!(*once, *twice, "not idempotent at width {width}")
        }
    }

    #[test]
    fn seq_trigger_reads_back_as_written() {
        stable(SourceKind::Program, "seqq let (a, b) = (x$, y) { f(a, b)? }");
        stable(
            SourceKind::Program,
            "seq let c = select r { `C(c) => c, _ => never() } { g(c)? }",
        );
        stable(SourceKind::Program, "seq a + b { h()? }");
        stable(SourceKind::Program, "seq (m{k}) { h()? }");
        stable(SourceKind::Program, "seq ({ a }) { h()? }");
        stable(SourceKind::Program, "seq (flush(x)) { h()? }");
    }

    #[test]
    fn template_string_brackets_and_splices() {
        stable(SourceKind::Program, r#"println("\nneeds [req.what] for a\[0\]")"#);
        stable(SourceKind::Program, "println(\"\"\"a [plain] \\[x] \\\\[y]\nb\"\"\")");
    }

    #[test]
    fn applied_self_receiver() {
        let src = "trait Coll { val map: fn(self<'a>, f: fn(x: 'a) -> 'b) -> self<'b> }";
        stable(SourceKind::Interface, src);
        stable(SourceKind::Program, src);
    }

    #[test]
    fn primitive_sets_print_as_written() {
        stable(SourceKind::Interface, "type Sint = [i8, i16, i32, z32, i64, z64]");
    }
}
