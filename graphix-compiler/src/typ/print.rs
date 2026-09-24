use crate::{
    PRINT_FLAGS, PrintFlag,
    expr::{
        WrittenAt,
        print::{PrettyBuf, PrettyDisplay},
    },
    print_as_written,
    typ::{Type, TypeRef},
};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::Typ;
use smallvec::SmallVec;
use std::fmt::{self, Write};

/// A set's members in print order: canonical, or under `AsWritten` the
/// members nobody wrote first and then the written ones as written.
// XCR claude for eric: only variants and refs carry a written position, so the rest
// of a union's non-primitive members move to the front. Carrying one for every
// member kind reshapes `Type` (134 match sites across the review's packages);
// recommend a positions list on `Type::Set` itself, beside the members, post-merge.
fn set_members(s: &[Type]) -> SmallVec<[&Type; 16]> {
    let mut members: SmallVec<[&Type; 16]> = s.iter().collect();
    if print_as_written() {
        members.sort_by_key(|t| match t {
            Type::Variant(_, _, at) => at.get().map(|p| (p.line, p.column)),
            Type::Ref(r) => r.pos.map(|p| (p.line, p.column)),
            _ => None,
        });
    }
    members
}

/// The named classes of primitives, a class before any class inside it.
const CLASSES: [(fn() -> BitFlags<Typ>, &str); 6] = [
    (Typ::number, "Number"),
    (Typ::real, "Real"),
    (Typ::float, "Float"),
    (Typ::integer, "Int"),
    (Typ::unsigned_integer, "Uint"),
    (Typ::signed_integer, "Sint"),
];

/// Each of `items`, `, ` between them.
fn write_list<T: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    items: impl IntoIterator<Item = T>,
) -> fmt::Result {
    for (i, t) in items.into_iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?
        }
        write!(f, "{t}")?
    }
    Ok(())
}

/// Each of `items` on its own line, one level in, `,` after all but the
/// last; the members of a set when `set`.
fn pretty_list<'a>(
    buf: &mut PrettyBuf,
    items: impl IntoIterator<Item = &'a Type>,
    set: bool,
) -> fmt::Result {
    buf.nested(|buf| {
        for (i, t) in items.into_iter().enumerate() {
            if i > 0 {
                buf.kill_newline();
                writeln!(buf, ",")?;
            }
            match t {
                Type::Primitive(_) if set => writeln!(buf, "{}", SetMember(t))?,
                t => t.fmt_pretty(buf)?,
            }
        }
        Ok(())
    })
}

/// A primitive set; `bracketed` is false for a member of a larger set,
/// whose brackets already hold it.
fn write_primitives(
    f: &mut fmt::Formatter<'_>,
    mut s: BitFlags<Typ>,
    bracketed: bool,
) -> fmt::Result {
    let replace = PRINT_FLAGS.get().contains(PrintFlag::ReplacePrims);
    if replace && let Some((_, name)) = CLASSES.iter().find(|(c, _)| s == c()) {
        return write!(f, "{name}");
    }
    match s.len() {
        0 => write!(f, "[]"),
        1 => write!(f, "{}", s.iter().next().unwrap()),
        _ => {
            if bracketed {
                write!(f, "[")?;
            }
            let mut names: SmallVec<[&str; 8]> = SmallVec::new();
            if replace {
                for (class, name) in CLASSES.iter() {
                    if s.contains(class()) {
                        s.remove(class());
                        names.push(name)
                    }
                }
            }
            write_list(f, names.iter().copied().chain(s.iter().map(|t| t.name())))?;
            if bracketed {
                write!(f, "]")?;
            }
            Ok(())
        }
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

/// A struct type's fields in print order: as written under `AsWritten`.
fn struct_fields(
    ts: &[(ArcStr, Type, WrittenAt)],
) -> SmallVec<[&(ArcStr, Type, WrittenAt); 16]> {
    let mut written: SmallVec<[_; 16]> = ts.iter().collect();
    if print_as_written() {
        written.sort_by_key(|(_, _, at)| at.order());
    }
    written
}

impl Type {
    fn fmt_inner(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abstract { id, params } => {
                // XCR claude for eric: not reproduced: cold and warm runs of type errors
                // and casts over program and stdlib abstract types print the ref's name
                // (`sys::fs::watch::Watcher`) or the value's tag (`Counter(3)`), never an
                // id. If one surfaces, register the name where the image decodes the id.
                match id.name() {
                    Some(name) => write!(f, "{name}")?,
                    None if params.is_empty() => return write!(f, "abstract"),
                    None => write!(f, "<abstract#{}>", id.0)?,
                }
                if !params.is_empty() {
                    write!(f, "<")?;
                    write_list(f, params.iter())?;
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
                    write_list(f, params.iter())?;
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
                write_list(f, ts.iter())?;
                write!(f, ")")
            }
            Self::Variant(tag, ts, _) if ts.is_empty() => write!(f, "`{tag}"),
            Self::Variant(tag, ts, _) => {
                write!(f, "`{tag}(")?;
                write_list(f, ts.iter())?;
                write!(f, ")")
            }
            Self::Struct(ts) => {
                write!(f, "{{ ")?;
                /// `name: type`
                struct Field<'a>(&'a ArcStr, &'a Type);
                impl fmt::Display for Field<'_> {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        write!(f, "{}: {}", self.0, self.1)
                    }
                }
                write_list(f, struct_fields(ts).into_iter().map(|(n, t, _)| Field(n, t)))?;
                write!(f, " }}")
            }
            Self::Set(s) => {
                write!(f, "[")?;
                write_list(f, set_members(s).into_iter().map(SetMember))?;
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
                    pretty_list(buf, params.iter(), false)?;
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
                pretty_list(buf, ts.iter(), false)?;
                writeln!(buf, ")")
            }
            Self::Variant(tag, ts, _) if ts.is_empty() => writeln!(buf, "`{tag}"),
            Self::Variant(tag, ts, _) if ts.len() == 1 => {
                pretty_lone_arg(buf, &format_compact!("`{tag}("), &ts[0], ")")
            }
            Self::Variant(tag, ts, _) => {
                writeln!(buf, "`{tag}(")?;
                pretty_list(buf, ts.iter(), false)?;
                writeln!(buf, ")")
            }
            Self::Struct(ts) => {
                writeln!(buf, "{{")?;
                buf.nested(|buf| {
                    for (i, (n, t, _)) in struct_fields(ts).into_iter().enumerate() {
                        if i > 0 {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                        }
                        write!(buf, "{n}: ")?;
                        buf.nested(|buf| t.fmt_pretty(buf))?;
                    }
                    Ok(())
                })?;
                writeln!(buf, "}}")
            }
            Self::Set(s) => {
                writeln!(buf, "[")?;
                pretty_list(buf, set_members(s), true)?;
                writeln!(buf, "]")
            }
            Self::Primitive(_) => {
                writeln!(buf, "{self}")
            }
        }
    }
}
