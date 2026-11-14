use crate::{
    PRINT_FLAGS, PrintFlag, Restore, SourcePosition,
    expr::{
        TypeNote, TypeNotes, WrittenAt,
        print::{PrettyBuf, PrettyDisplay, item_end, item_sep, write_comments},
    },
    print_as_written,
    typ::{Bound, Mutability, Type, TypeRef, strictly_identical, tvar::Printing},
};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::Cell,
    fmt::{self, Write},
};

/// A set's members in print order: canonical, or under `AsWritten` the
/// members nobody wrote first and then the written ones as written.
fn set_members(s: &[Type]) -> SmallVec<[&Type; 16]> {
    let mut members: SmallVec<[&Type; 16]> = s.iter().collect();
    if print_as_written() {
        members.sort_by_key(|t| t.written_at().map(|p| (p.line, p.column)));
    }
    members
}

impl Type {
    /// Where a union's member was written, for the members that record
    /// it: a variant and a named type.
    pub fn written_at(&self) -> Option<SourcePosition> {
        match self {
            Type::Variant(_, _, at) => at.get(),
            Type::Ref(r) => r.pos,
            _ => None,
        }
    }
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
/// last, with the comments `note` finds for it.
fn pretty_items<'a, T>(
    buf: &mut PrettyBuf,
    items: impl IntoIterator<Item = T>,
    note: impl Fn(&T) -> Option<&'a TypeNote>,
    mut item: impl FnMut(&mut PrettyBuf, T) -> fmt::Result,
) -> fmt::Result {
    buf.nested(|buf| {
        let mut prev: Option<&TypeNote> = None;
        for (i, it) in items.into_iter().enumerate() {
            let n = note(&it);
            if i > 0 {
                item_sep(buf, ",", prev.and_then(|p| p.comments.eol.text()))?
            }
            if let Some(n) = n {
                write_comments(buf, n.comments.above.lines())?
            }
            item(buf, it)?;
            prev = n;
        }
        match prev {
            Some(p) => item_end(buf, p.comments.eol.text(), p.below.lines()),
            None => Ok(()),
        }
    })
}

/// Each of `items` on its own line, one level in, `,` after all but the
/// last.
fn pretty_list<'a>(
    buf: &mut PrettyBuf,
    items: impl IntoIterator<Item = &'a Type>,
) -> fmt::Result {
    pretty_items(buf, items, |_| None, |buf, t| t.fmt_pretty(buf))
}

/// The note on what was written at `at`.
fn noted(notes: Option<&TypeNotes>, at: Option<SourcePosition>) -> Option<&TypeNote> {
    notes.zip(at).and_then(|(notes, at)| notes.at(at))
}

/// `t` beside `notes`, when its definition keeps any.
fn pretty_child(buf: &mut PrettyBuf, t: &Type, notes: Option<&TypeNotes>) -> fmt::Result {
    match notes {
        Some(notes) => Noted(t, notes).fmt_pretty(buf),
        None => t.fmt_pretty(buf),
    }
}

/// A type beside the comments its definition keeps on its fields and
/// members, which print only as written.
pub struct Noted<'a>(pub &'a Type, pub &'a TypeNotes);

impl fmt::Display for Noted<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyDisplay for Noted<'_> {
    fn decorated(&self) -> bool {
        print_as_written() && self.1.any_in(self.0)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.0.pretty(buf, Some(self.1))
    }
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
        if SHOWN.get() == usize::MAX && crate::print_standing() {
            return write_capped(f, self, Brief::MOST);
        }
        let _printing = Printing::enter(|| self.clone());
        crate::stack::ensure_sufficient(|| self.fmt_inner(f))
    }
}

/// A type in a diagnostic: its first [`Brief::BYTES`] as printed under the
/// current flags, tvars dereferenced. A type that shares its parts prints
/// as a tree, 2^depth long, and the printer stops at the first write
/// refused; a union's members print into scratch no further than the cap
/// needs ([`write_deref_set`]). Any other print where cells print as they
/// stand (a message, the REPL's type, a hover) is cut at [`Brief::MOST`].
pub struct Brief<'a>(pub &'a Type);

impl Brief<'_> {
    pub const BYTES: usize = 512;
    pub const MOST: usize = 1 << 16;
    /// The longest declaration a refusal writes for a program to copy.
    pub const DECLARATION: usize = 1024;

    /// `typ` as printed under the current flags, `None` where it is
    /// longer than `bytes`; the print stops at the first byte past them.
    pub fn within(typ: &Type, bytes: usize) -> Option<CompactString> {
        struct Limit {
            out: CompactString,
            left: usize,
        }
        impl Write for Limit {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                self.left = self.left.checked_sub(s.len()).ok_or(fmt::Error)?;
                self.out.push_str(s);
                Ok(())
            }
        }
        let mut limit = Limit { out: CompactString::new(""), left: bytes };
        let _shown = Restore::replace(&SHOWN, SHOWN.get().min(bytes));
        write!(limit, "{typ}").ok().map(|()| limit.out)
    }
}

impl fmt::Display for Brief<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let flags = PRINT_FLAGS.get() | PrintFlag::DerefTVars;
        crate::format_with_flags(flags, || write_capped(f, self.0, Self::BYTES))
    }
}

/// `typ`'s first `bytes` as printed under the current flags, then `…`
/// where it was cut.
fn write_capped(f: &mut fmt::Formatter<'_>, typ: &Type, bytes: usize) -> fmt::Result {
    struct Capped<'a, 'b> {
        f: &'a mut fmt::Formatter<'b>,
        left: usize,
    }
    impl Write for Capped<'_, '_> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            if s.len() <= self.left {
                self.left -= s.len();
                return self.f.write_str(s);
            }
            let mut end = self.left;
            while !s.is_char_boundary(end) {
                end -= 1;
            }
            self.left = 0;
            self.f.write_str(&s[..end])?;
            Err(fmt::Error)
        }
    }
    let mut capped = Capped { f, left: bytes };
    let _shown = Restore::replace(&SHOWN, SHOWN.get().min(bytes));
    match write!(capped, "{typ}") {
        Err(_) if capped.left == 0 => capped.f.write_str("…"),
        r => r,
    }
}

thread_local! {
    /// No less than what the capped print being written still shows;
    /// unbounded outside one.
    static SHOWN: Cell<usize> = const { Cell::new(usize::MAX) };
    /// A part of a member prints alone to be compared ([`Parts`]): a union
    /// refuses to print, since it would print each of its members.
    static ALONE: Cell<bool> = const { Cell::new(false) };
}

#[cfg(test)]
thread_local! {
    pub(super) static MEMBER_PRINTS: Cell<usize> = const { Cell::new(0) };
}

/// A union's scratch: it takes writes while its text is no longer than
/// `cap`, the write that passes it whole, and refuses every one after.
struct Scratch<'a> {
    text: &'a mut String,
    cap: usize,
}

impl Write for Scratch<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.text.len() > self.cap {
            return Err(fmt::Error);
        }
        self.text.push_str(s);
        Ok(())
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
            Self::Bound(Bound::Concrete) => write!(f, "Concrete"),
            Self::Bound(Bound::Data) => write!(f, "Data"),
            Self::Bound(Bound::Function) => write!(f, "Function"),
            Self::Bound(Bound::Singleton) => write!(f, "Singleton"),
            Self::Bound(Bound::OneNumber) => write!(f, "OneNumber"),
            Self::Bound(Bound::NoError) => write!(f, "NoError"),
            Self::Bound(Bound::Discernible) => write!(f, "Discernible"),
            Self::Bound(Bound::Ordered) => write!(f, "Ordered"),
            Self::Bound(Bound::JsonData) => write!(f, "JsonData"),
            Self::Bound(Bound::TomlData) => write!(f, "TomlData"),
            Self::Bottom => write!(f, "_"),
            Self::Any => write!(f, "Any"),
            Self::Ref(tr) => {
                let TypeRef { name, params, .. } = &**tr;
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
            Self::Map(m) => write!(f, "Map<{}, {}>", m.key, m.value),
            Self::ByRef(m, t) => write!(f, "{}{t}", m.prefix()),
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
                write_list(
                    f,
                    struct_fields(ts).into_iter().map(|(n, t, _)| Field(n, t)),
                )?;
                write!(f, " }}")
            }
            Self::Set(s) if crate::print_standing() => write_deref_set(f, s),
            Self::Set(s) => {
                write!(f, "[")?;
                write_list(f, set_members(s).into_iter().map(SetMember))?;
                write!(f, "]")
            }
            Self::Primitive(s) => write_primitives(f, *s, true),
        }
    }
}

/// A set printed where cells print as they stand
/// ([`crate::print_standing`]): a member identical to an earlier one (two
/// names of one cell), or printing as it does where either holds a cell
/// (two cells bound to one type), prints once, and a lone member prints
/// bare. A member that prints as a kept one by their parts ([`Parts`])
/// does not print, so a part many members share prints once. Any other
/// prints into scratch only as far as
/// the farther of what [`SHOWN`] reaches and the longest member kept,
/// past which it prints like none of them: a member whose text passes
/// that is kept as far as it printed, and once the kept text passes
/// [`SHOWN`] the next member that is not identical to one kept counts as
/// kept unprinted, since none of it shows.
// CR claude for claude: [risk/low] Members alike but for two copies of one union that are not one
// node, or for cells holding such copies, still print whole to be compared, so nesting them costs
// 2^depth as before: `Parts` tells a union only by its node, and a cell holding one refuses to
// print alone (`ALONE`), since printing both copies there would cost three prints of the level
// below where the member's text costs two. contains::tests::
// members_alike_but_for_cells_holding_a_union_print_as_before: 1534 member prints at depth 10
// for 10 levels of text. A fix tells two unions alike by their members (each member's verdict
// against the other's, in one context) or reuses a union's text across copies printed under the
// same cells. No program has shown such a chain. (print-lookalike-copies-exp-01)
// 2026-11-14 claude: r25 fixer, no code change (BLOCKED, not forced). The test pin
// members_alike_but_for_cells_holding_a_union_print_as_before still shows 1534 prints at depth 10
// on base-r25, so the CR is current. A fix that tells two unions alike by their members must also
// reproduce write_deref_set's dedup, which depends on identity, not text alone: two tvar_free
// members that print alike are both kept unless strictly_identical (print.rs write_deref_set, the
// !(k.tvar_free() && t.tvar_free()) guards), so pairwise alike of the members is not enough: the
// identity pattern among members (which pairs are strictly_identical, which fall to Parts, which
// to text) has to match between the two copies, plus the SHOWN/left budgets that depend on
// position, plus a cell bound to a union has to be derefed by alike where print_alike now refuses
// it (ALONE). Reusing a union's text across copies needs a memo keyed by structure under the same
// cells, i.e. a new walk. Both add machinery to Parts, which already carries an open unsoundness
// CR (print-parts-renamed-01) in the same code; the cost is exponential only for a message type
// nesting unions of cells that each hold a copy of the level below, and no program has shown one.
// Recommendation: leave this CR as the record (or delete it) unless a program shows the chain; if
// one does, the right fix is probably to make unions built by one inference share a node, not to
// teach the printer to match copies.
fn write_deref_set(f: &mut fmt::Formatter<'_>, s: &[Type]) -> fmt::Result {
    if ALONE.get() {
        return Err(fmt::Error);
    }
    let cap = SHOWN.get();
    let mut text: LPooled<String> = LPooled::take();
    let mut kept: SmallVec<[(&Type, usize, usize); 16]> = SmallVec::new();
    for t in set_members(s) {
        if kept.iter().any(|(k, _, _)| strictly_identical(k, t)) {
            continue;
        }
        let start = text.len();
        if start > cap {
            kept.push((t, start, start));
            break;
        }
        let as_kept = kept.iter().any(|&(k, a, b)| {
            Parts { text: &mut text, left: b - a }.members(k, t)
                && !(k.tvar_free() && t.tvar_free())
        });
        if as_kept {
            continue;
        }
        #[cfg(test)]
        MEMBER_PRINTS.with(|n| n.set(n.get() + 1));
        let longest = kept.iter().map(|(_, a, b)| b - a).max().unwrap_or(0);
        let reach = cap.max(start + longest);
        let printed = {
            let _shown = Restore::replace(&SHOWN, reach - start);
            write!(Scratch { text: &mut text, cap: reach }, "{}", SetMember(t))
        };
        if text.len() <= reach {
            printed?;
            let printed = &text[start..];
            let again = kept.iter().any(|(k, a, b)| {
                &text[*a..*b] == printed && !(k.tvar_free() && t.tvar_free())
            });
            if again {
                text.truncate(start);
                continue;
            }
        }
        kept.push((t, start, text.len()));
        if text.len() > cap && kept.len() > 1 {
            break;
        }
    }
    match &kept[..] {
        [(t @ Type::Primitive(_), _, _)] => write!(f, "{t}"),
        [(_, a, b)] => f.write_str(&text[*a..*b]),
        kept => {
            write!(f, "[")?;
            write_list(f, kept.iter().map(|(_, a, b)| &text[*a..*b]))?;
            write!(f, "]")
        }
    }
}

/// Whether a member prints as a kept one did, told from their parts, so
/// a part the two share prints for neither: one node prints as itself (a
/// var only under one name: two of one cell may print apart), and two of
/// one head as their parts do. A cell, or a part standing where the other
/// holds one, prints alone after `text` beside its counterpart, the two
/// within the `left` bytes the kept member's text leaves them, a union in
/// either refusing to print ([`ALONE`]). False where that cannot tell: two
/// heads, or a union, function or application that is not one node.
// XCR claude for claude: [risk/low] `Parts` takes a kept member's part to print now as it did when
// the member printed, and it need not: a cell's printed name can change within one message. A
// generated name's cell prints its own name until a written name of it prints (`printed_name`),
// and a signature's quantifier can number apart on a second print (`quantifiers_apart`). So a
// member whose text differs from every kept one can be dropped: k = ('_6@X, c1), t = ('b@X, i64),
// u = ('z@X, c3), X one open cell, c1 and c3 open cells of one generated name, prints
// "[('_6: unbound, '_7: unbound), ('b: unbound, i64)]" where the text alone keeps u as
// "('b: unbound, '_7: unbound)": k's '_6 reprints as 'b once t named X. `strictly_identical`
// already merges whole members on the same assumption. A fix settles a cell's printed name at its
// first print in a message (a generated name's too), or has `Parts` refuse a reprint of a kept
// part after a name it shows changed. No program has shown it. probe:
// ~/ptmp/cr29/scratch/r24/24f/ple-review/registry.rs (print-parts-renamed-01)
// 2026-11-14 claude: r25 fixer, STALE on base-r25: the CR's probe prints all three members under
// one name ('_6 for X throughout), since printed_name (tvar.rs) records a generated name's cell
// at its first print in the message, so a kept part reprints as it first did and no later member
// renames it. Pin
// contains::tests::members_over_a_cell_named_by_a_later_member_print_as_their_text_would holds
// the probe's exact text; with the generated-name case of printed_name not recorded it fails with
// the CR's text '[('_6: unbound, '_7: unbound), ('b: unbound, i64)]'. A signature's quantifier
// cannot reach Parts: Parts never descends a function type (only one node), and a quantifier cell
// outside its signature is recorded persistently at its own first print.
struct Parts<'a> {
    text: &'a mut String,
    left: usize,
}

impl Parts<'_> {
    /// Two members, neither a cell, whose text would be the member's.
    fn members(&mut self, k: &Type, t: &Type) -> bool {
        !matches!(k, Type::TVar(_)) && !matches!(t, Type::TVar(_)) && self.alike(k, t)
    }

    fn alike(&mut self, k: &Type, t: &Type) -> bool {
        let one_node = match (k, t) {
            (Type::TVar(a), Type::TVar(b)) => a.name == b.name && a.same_cell(b),
            _ => k.node_key().is_some_and(|a| t.node_key() == Some(a)),
        };
        if one_node {
            return true;
        }
        crate::stack::ensure_sufficient(|| match (k, t) {
            (Type::TVar(_), _) | (_, Type::TVar(_)) => self.print_alike(k, t),
            (Type::Tuple(a), Type::Tuple(b)) => self.all(a, b),
            (Type::Variant(x, a, _), Type::Variant(y, b, _)) => x == y && self.all(a, b),
            (Type::Struct(a), Type::Struct(b)) => {
                let (a, b) = (struct_fields(a), struct_fields(b));
                a.len() == b.len()
                    && a.iter()
                        .zip(&b)
                        .all(|((n, x, _), (m, y, _))| n == m && self.alike(x, y))
            }
            (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::Error(a), Type::Error(b)) => self.alike(a, b),
            (Type::ByRef(m, a), Type::ByRef(n, b)) => m == n && self.alike(a, b),
            (Type::Map(a), Type::Map(b)) => {
                self.alike(&a.key, &b.key) && self.alike(&a.value, &b.value)
            }
            (Type::Ref(a), Type::Ref(b)) => {
                a.name == b.name && self.all(&a.params, &b.params)
            }
            (
                Type::Abstract { id: x, params: a },
                Type::Abstract { id: y, params: b },
            ) => x == y && self.all(a, b),
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (Type::Bound(a), Type::Bound(b)) => a == b,
            (Type::Bottom, Type::Bottom)
            | (Type::Any, Type::Any)
            | (Type::Hole, Type::Hole) => true,
            _ => false,
        })
    }

    fn all(&mut self, a: &[Type], b: &[Type]) -> bool {
        a.len() == b.len() && a.iter().zip(b).all(|(a, b)| self.alike(a, b))
    }

    fn print_alike(&mut self, k: &Type, t: &Type) -> bool {
        let _alone = Restore::replace(&ALONE, true);
        let start = self.text.len();
        let kept =
            write!(Scratch { text: &mut *self.text, cap: start + self.left }, "{k}");
        let mid = self.text.len();
        let len = mid - start;
        let alike = kept.is_ok()
            && len <= self.left
            && write!(Scratch { text: &mut *self.text, cap: mid + len }, "{t}").is_ok()
            && self.text[mid..] == self.text[start..mid];
        self.text.truncate(start);
        if alike {
            self.left -= len;
        }
        alike
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
        Type::Ref(tr) => !tr.params.is_empty(),
        Type::ByRef(Mutability::Shared, t) => opens_with_bracket(t),
        Type::ByRef(Mutability::Mut, _) => false,
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::TVar(_)
        | Type::Abstract { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Bound(_) => false,
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
        buf.with_tail(close.chars().count(), |buf| t.fmt_pretty(buf))?;
        buf.kill_newline();
        return writeln!(buf, "{close}");
    }
    writeln!(buf, "{open}")?;
    buf.nested(|buf| t.fmt_pretty(buf))?;
    writeln!(buf, "{close}")
}

impl PrettyDisplay for Type {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let _printing = Printing::enter(|| self.clone());
        self.pretty(buf, None)
    }
}

impl Type {
    fn pretty(&self, buf: &mut PrettyBuf, notes: Option<&TypeNotes>) -> fmt::Result {
        match self {
            Self::Abstract { .. } | Self::App(..) | Self::Hole | Self::Bound(_) => {
                writeln!(buf, "{self}")
            }
            Self::Bottom => writeln!(buf, "_"),
            Self::Any => writeln!(buf, "Any"),
            Self::Ref(tr) => {
                let TypeRef { name, params, .. } = &**tr;
                match &params[..] {
                    [] => writeln!(buf, "{name}"),
                    [p] => pretty_lone_arg(buf, &format_compact!("{name}<"), p, ">"),
                    params => {
                        writeln!(buf, "{name}<")?;
                        pretty_list(buf, params.iter())?;
                        writeln!(buf, ">")
                    }
                }
            }
            Self::TVar(tv) => writeln!(buf, "{tv}"),
            Self::Fn(t) => t.fmt_pretty(buf),
            Self::Error(t) => pretty_lone_arg(buf, "Error<", t, ">"),
            Self::Array(t) => pretty_lone_arg(buf, "Array<", t, ">"),
            Self::List(t) => pretty_lone_arg(buf, "List<", t, ">"),
            Self::Map(m) => {
                writeln!(buf, "Map<")?;
                buf.nested(|buf| {
                    m.key.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ",")?;
                    m.value.fmt_pretty(buf)
                })?;
                writeln!(buf, ">")
            }
            Self::ByRef(m, t) => {
                write!(buf, "{}", m.prefix())?;
                t.fmt_pretty(buf)
            }
            Self::Tuple(ts) => {
                writeln!(buf, "(")?;
                pretty_list(buf, ts.iter())?;
                writeln!(buf, ")")
            }
            Self::Variant(tag, ts, _) if ts.is_empty() => writeln!(buf, "`{tag}"),
            Self::Variant(tag, ts, _) if ts.len() == 1 => {
                pretty_lone_arg(buf, &format_compact!("`{tag}("), &ts[0], ")")
            }
            Self::Variant(tag, ts, _) => {
                writeln!(buf, "`{tag}(")?;
                pretty_list(buf, ts.iter())?;
                writeln!(buf, ")")
            }
            Self::Struct(ts) => {
                writeln!(buf, "{{")?;
                pretty_items(
                    buf,
                    struct_fields(ts),
                    |(_, _, at)| noted(notes, at.get()),
                    |buf, (n, t, _)| {
                        write!(buf, "{n}: ")?;
                        // a bracketed value hugs its name; any other nests
                        if opens_with_bracket(t) {
                            pretty_child(buf, t, notes)
                        } else {
                            buf.nested(|buf| pretty_child(buf, t, notes))
                        }
                    },
                )?;
                writeln!(buf, "}}")
            }
            Self::Set(s) => {
                writeln!(buf, "[")?;
                pretty_items(
                    buf,
                    set_members(s),
                    |t| noted(notes, t.written_at()),
                    |buf, t| match t {
                        Type::Primitive(_) => writeln!(buf, "{}", SetMember(t)),
                        t => pretty_child(buf, t, notes),
                    },
                )?;
                writeln!(buf, "]")
            }
            Self::Primitive(_) => {
                writeln!(buf, "{self}")
            }
        }
    }
}
