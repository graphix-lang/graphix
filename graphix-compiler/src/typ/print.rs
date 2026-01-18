use crate::{
    expr::print::{PrettyBuf, PrettyDisplay},
    typ::Type,
    PrintFlag, PRINT_FLAGS,
};
use netidx::publisher::Typ;
use std::fmt::{self, Write};

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abstract { .. } => Ok(()),
            Self::Bottom => write!(f, "_"),
            Self::Any => write!(f, "Any"),
            Self::Ref { scope: _, name, params } => {
                write!(f, "{name}")?;
                if !params.is_empty() {
                    write!(f, "<")?;
                    for (i, t) in params.iter().enumerate() {
                        write!(f, "{t}")?;
                        if i < params.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Self::TVar(tv) => write!(f, "{tv}"),
            Self::Fn(t) => write!(f, "{t}"),
            Self::Error(t) => write!(f, "Error<{t}>"),
            Self::Array(t) => write!(f, "Array<{t}>"),
            Self::Map { key, value } => write!(f, "Map<{key}, {value}>"),
            Self::ByRef(t) => write!(f, "&{t}"),
            Self::Tuple(ts) => {
                write!(f, "(")?;
                for (i, t) in ts.iter().enumerate() {
                    write!(f, "{t}")?;
                    if i < ts.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Self::Variant(tag, ts) if ts.len() == 0 => {
                write!(f, "`{tag}")
            }
            Self::Variant(tag, ts) => {
                write!(f, "`{tag}(")?;
                for (i, t) in ts.iter().enumerate() {
                    write!(f, "{t}")?;
                    if i < ts.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
            Self::Struct(ts) => {
                write!(f, "{{")?;
                for (i, (n, t)) in ts.iter().enumerate() {
                    write!(f, "{n}: {t}")?;
                    if i < ts.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Set(s) => {
                write!(f, "[")?;
                for (i, t) in s.iter().enumerate() {
                    write!(f, "{t}")?;
                    if i < s.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Self::Primitive(s) => {
                let replace = PRINT_FLAGS.get().contains(PrintFlag::ReplacePrims);
                if replace && *s == Typ::number() {
                    write!(f, "Number")
                } else if replace && *s == Typ::float() {
                    write!(f, "Float")
                } else if replace && *s == Typ::real() {
                    write!(f, "Real")
                } else if replace && *s == Typ::integer() {
                    write!(f, "Int")
                } else if replace && *s == Typ::unsigned_integer() {
                    write!(f, "Uint")
                } else if replace && *s == Typ::signed_integer() {
                    write!(f, "Sint")
                } else if s.len() == 0 {
                    write!(f, "[]")
                } else if s.len() == 1 {
                    write!(f, "{}", s.iter().next().unwrap())
                } else {
                    let mut s = *s;
                    macro_rules! builtin {
                        ($set:expr, $name:literal) => {
                            if replace && s.contains($set) {
                                s.remove($set);
                                write!(f, $name)?;
                                if !s.is_empty() {
                                    write!(f, ", ")?
                                }
                            }
                        };
                    }
                    write!(f, "[")?;
                    builtin!(Typ::number(), "Number");
                    builtin!(Typ::real(), "Real");
                    builtin!(Typ::float(), "Float");
                    builtin!(Typ::integer(), "Int");
                    builtin!(Typ::unsigned_integer(), "Uint");
                    builtin!(Typ::signed_integer(), "Sint");
                    for (i, t) in s.iter().enumerate() {
                        write!(f, "{t}")?;
                        if i < s.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "]")
                }
            }
        }
    }
}

impl PrettyDisplay for Type {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        match self {
            Self::Abstract { .. } => Ok(()),
            Self::Bottom => writeln!(buf, "_"),
            Self::Any => writeln!(buf, "Any"),
            Self::Ref { scope: _, name, params } => {
                if params.is_empty() {
                    writeln!(buf, "{name}")
                } else {
                    writeln!(buf, "{name}<")?;
                    buf.with_indent(2, |buf| {
                        for (i, t) in params.iter().enumerate() {
                            t.fmt_pretty(buf)?;
                            if i < params.len() - 1 {
                                buf.kill_newline();
                                writeln!(buf, ",")?;
                            }
                        }
                        Ok(())
                    })?;
                    writeln!(buf, ">")
                }
            }
            Self::TVar(tv) => writeln!(buf, "{tv}"),
            Self::Fn(t) => t.fmt_pretty(buf),
            Self::Error(t) => {
                writeln!(buf, "Error<")?;
                buf.with_indent(2, |buf| t.fmt_pretty(buf))?;
                writeln!(buf, ">")
            }
            Self::Array(t) => {
                writeln!(buf, "Array<")?;
                buf.with_indent(2, |buf| t.fmt_pretty(buf))?;
                writeln!(buf, ">")
            }
            Self::Map { key, value } => {
                writeln!(buf, "Map<")?;
                buf.with_indent(2, |buf| {
                    key.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ",")?;
                    value.fmt_pretty(buf)
                })?;
                writeln!(buf, ">")
            }
            Self::ByRef(t) => {
                write!(buf, "&")?;
                t.fmt_pretty(buf)
            }
            Self::Tuple(ts) => {
                writeln!(buf, "(")?;
                buf.with_indent(2, |buf| {
                    for (i, t) in ts.iter().enumerate() {
                        t.fmt_pretty(buf)?;
                        if i < ts.len() - 1 {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, ")")
            }
            Self::Variant(tag, ts) if ts.is_empty() => writeln!(buf, "`{tag}"),
            Self::Variant(tag, ts) => {
                writeln!(buf, "`{tag}(")?;
                buf.with_indent(2, |buf| {
                    for (i, t) in ts.iter().enumerate() {
                        t.fmt_pretty(buf)?;
                        if i < ts.len() - 1 {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, ")")
            }
            Self::Struct(ts) => {
                writeln!(buf, "{{")?;
                buf.with_indent(2, |buf| {
                    for (i, (n, t)) in ts.iter().enumerate() {
                        write!(buf, "{n}: ")?;
                        buf.with_indent(2, |buf| t.fmt_pretty(buf))?;
                        if i < ts.len() - 1 {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, "}}")
            }
            Self::Set(s) => {
                writeln!(buf, "[")?;
                buf.with_indent(2, |buf| {
                    for (i, t) in s.iter().enumerate() {
                        t.fmt_pretty(buf)?;
                        if i < s.len() - 1 {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, "]")
            }
            Self::Primitive(_) => {
                // Primitives are simple enough to just use Display
                writeln!(buf, "{self}")
            }
        }
    }
}
