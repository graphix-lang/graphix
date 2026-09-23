use crate::{
    PRINT_FLAGS, PrintFlag,
    expr::{
        WrittenAt,
        print::{PrettyBuf, PrettyDisplay},
    },
    print_as_written,
    typ::{Type, TypeRef},
};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::Typ;
use smallvec::SmallVec;
use std::fmt::{self, Write};

/// A set's members in print order: canonical, or under `AsWritten` the
/// members nobody wrote first and then the written ones as written.
// CR claude for eric: [bug] Only variants and refs carry a written position, so
// every other member the user wrote (an Array, tuple, struct, map, fn, tvar)
// counts as "nobody wrote it" and moves to the front. probe: `graphix fmt` turns
// `` type T = [`A, Array<i64>, `B, (i64, string), {x: i64}] `` into
// `` [Array<i64>, (i64, string), { x: i64 }, `A, `B] ``; CLAUDE.md promises only
// that primitives print first. Every member kind needs its written position.
fn set_members(s: &[Type]) -> SmallVec<[&Type; 16]> {
    let mut members: SmallVec<[&Type; 16]> = s.iter().collect();
    if print_as_written() {
        members.sort_by_key(|t| match t {
            Type::Variant(_, _, at) => {
                Some(at.order()).filter(|o| *o != WrittenAt::NOWHERE.order())
            }
            Type::Ref(r) => r.pos.map(|p| (p.line, p.column)),
            _ => None,
        });
    }
    members
}

/// A primitive set; `bracketed` is false for a member of a larger set,
/// whose brackets already hold it.
fn write_primitives(
    f: &mut fmt::Formatter<'_>,
    mut s: BitFlags<Typ>,
    bracketed: bool,
) -> fmt::Result {
    let replace = PRINT_FLAGS.get().contains(PrintFlag::ReplacePrims);
    // CR claude for eric: [structure] The six class names are listed twice, once
    // for the exact match and once in the `builtin!` subset pass, in different
    // orders. One `[(BitFlags<Typ>, &str); 6]` table serves both.
    if replace && s == Typ::number() {
        write!(f, "Number")
    } else if replace && s == Typ::float() {
        write!(f, "Float")
    } else if replace && s == Typ::real() {
        write!(f, "Real")
    } else if replace && s == Typ::integer() {
        write!(f, "Int")
    } else if replace && s == Typ::unsigned_integer() {
        write!(f, "Uint")
    } else if replace && s == Typ::signed_integer() {
        write!(f, "Sint")
    } else if s.len() == 0 {
        write!(f, "[]")
    } else if s.len() == 1 {
        write!(f, "{}", s.iter().next().unwrap())
    } else {
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
        if bracketed {
            write!(f, "[")?;
        }
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
        if bracketed {
            write!(f, "]")?;
        }
        Ok(())
    }
}

/// A member of a set as it prints between the set's brackets.
struct SetMember<'a>(&'a Type);

impl fmt::Display for SetMember<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Type::Primitive(p) => write_primitives(f, *p, false),
            t => write!(f, "{t}"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        crate::stack::ensure_sufficient(|| self.fmt_inner(f))
    }
}

impl Type {
    // CR claude for eric: [structure] The "write each item, then `, ` unless it is
    // the last" loop is hand-written seven times here (Abstract and Ref params,
    // Tuple, Variant, Struct, Set, primitives) and its `kill_newline` + `,` twin
    // five times in `fmt_pretty_inner` (more in fntyp.rs). One separated-list
    // helper per printer.
    fn fmt_inner(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abstract { id, params } => {
                // CR claude for eric: [risk] The name comes from the process-global
                // `ABSTRACT_NAMES`, filled only by `AbstractId::of`; an id decoded
                // from an image in a process that never minted it prints as
                // `abstract` / `<abstract#N>`, so printed types can differ between
                // a cold and a warm start (suspected). `GxAbstract` carries its
                // name for this reason; the type could too.
                match id.name() {
                    Some(name) => write!(f, "{name}")?,
                    None if params.is_empty() => return write!(f, "abstract"),
                    None => write!(f, "<abstract#{}>", id.0)?,
                }
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
            Self::App(c, a) => match Type::app_filled(c, a) {
                Some(filled) => write!(f, "{filled}"),
                None => write!(f, "{c}<{a}>"),
            },
            Self::Hole => write!(f, "'_"),
            Self::Bottom => write!(f, "_"),
            Self::Any => write!(f, "Any"),
            Self::Ref(TypeRef { scope: _, name, params, .. }) => {
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
            Self::List(t) => write!(f, "List<{t}>"),
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
            Self::Variant(tag, ts, _) if ts.len() == 0 => {
                write!(f, "`{tag}")
            }
            Self::Variant(tag, ts, _) => {
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
                let mut written: SmallVec<[_; 16]> = ts.iter().collect();
                if print_as_written() {
                    written.sort_by_key(|(_, _, at)| at.order());
                }
                write!(f, "{{ ")?;
                for (i, (n, t, _)) in written.iter().enumerate() {
                    write!(f, "{n}: {t}")?;
                    if i < ts.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, " }}")
            }
            Self::Set(s) => {
                write!(f, "[")?;
                for (i, t) in set_members(s).iter().enumerate() {
                    write!(f, "{}", SetMember(t))?;
                    if i < s.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Self::Primitive(s) => write_primitives(f, *s, true),
        }
    }
}

/// Whether the type's text opens with a bracket that its last line
/// closes: `{`, `(`, `[`, `` `Tag( ``, `Array<`, `fn(`. The expression
/// printer's `opens_with_bracket`, for types.
fn opens_with_bracket(t: &Type) -> bool {
    match t {
        Type::Struct(_)
        | Type::Tuple(_)
        | Type::Set(_)
        | Type::Array(_)
        | Type::List(_)
        | Type::Error(_)
        | Type::Map { .. }
        | Type::Fn(_) => true,
        Type::Variant(_, args, _) => !args.is_empty(),
        Type::Ref(TypeRef { params, .. }) => !params.is_empty(),
        Type::ByRef(t) => opens_with_bracket(t),
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::TVar(_)
        | Type::Abstract { .. }
        | Type::App(..)
        | Type::Hole => false,
    }
}

/// A lone argument that opens with a bracket hugs the brackets it
/// stands between, as a lone bracketed call argument does: `` `Tag({ ``,
/// `` `Outer(`Inner([ ``, `Array<{`.
fn pretty_lone_arg(
    buf: &mut PrettyBuf,
    open: &str,
    t: &Type,
    close: &str,
) -> fmt::Result {
    if opens_with_bracket(t) {
        write!(buf, "{open}")?;
        t.fmt_pretty(buf)?;
        buf.kill_newline();
        return writeln!(buf, "{close}");
    }
    writeln!(buf, "{open}")?;
    buf.nested(|buf| t.fmt_pretty(buf))?;
    writeln!(buf, "{close}")
}

impl PrettyDisplay for Type {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        match self {
            Self::Abstract { .. } | Self::App(..) | Self::Hole => writeln!(buf, "{self}"),
            Self::Bottom => writeln!(buf, "_"),
            Self::Any => writeln!(buf, "Any"),
            Self::Ref(TypeRef { scope: _, name, params, .. }) => {
                if params.is_empty() {
                    writeln!(buf, "{name}")
                } else {
                    writeln!(buf, "{name}<")?;
                    buf.nested(|buf| {
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
            Self::Error(t) => pretty_lone_arg(buf, "Error<", t, ">"),
            Self::Array(t) => pretty_lone_arg(buf, "Array<", t, ">"),
            Self::List(t) => pretty_lone_arg(buf, "List<", t, ">"),
            Self::Map { key, value } => {
                writeln!(buf, "Map<")?;
                buf.nested(|buf| {
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
                buf.nested(|buf| {
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
            Self::Variant(tag, ts, _) if ts.is_empty() => writeln!(buf, "`{tag}"),
            Self::Variant(tag, ts, _) if ts.len() == 1 => {
                pretty_lone_arg(buf, &format_compact!("`{tag}("), &ts[0], ")")
            }
            Self::Variant(tag, ts, _) => {
                writeln!(buf, "`{tag}(")?;
                buf.nested(|buf| {
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
                let mut written: SmallVec<[_; 16]> = ts.iter().collect();
                if print_as_written() {
                    written.sort_by_key(|(_, _, at)| at.order());
                }
                writeln!(buf, "{{")?;
                buf.nested(|buf| {
                    for (i, (n, t, _)) in written.iter().enumerate() {
                        write!(buf, "{n}: ")?;
                        buf.nested(|buf| t.fmt_pretty(buf))?;
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
                buf.nested(|buf| {
                    for (i, t) in set_members(s).iter().enumerate() {
                        match t {
                            Type::Primitive(_) => writeln!(buf, "{}", SetMember(t))?,
                            t => t.fmt_pretty(buf)?,
                        }
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
                writeln!(buf, "{self}")
            }
        }
    }
}
