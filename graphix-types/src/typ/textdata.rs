//! Which types JSON and TOML read back as they were written
//! (`'a: JsonData`, `'a: TomlData`): a type whose every union tells its
//! members apart by a tag. A member other than `null` is a variant, by its
//! tag, or an abstract type, which the formats write inside an object
//! keyed by its unqualified name (`{"Port": 80}`); no two members share a
//! tag, or the union is one member and `null`. Anything else is refused:
//! `[i64, string]`, `[Port, u8]`, `` [Transparent<Port>, `A] ``, an open
//! member beside another. A `Transparent<T>`, written bare, has no tag, and
//! is refused over a type `T` known not to be abstract, where it would
//! change nothing; over an open variable it is not judged. Inside the data
//! an abstract type's `from_value` reads, a cast of data, every abstract
//! value is written bare, so no abstract type carries a tag there. A
//! typedef whose parameters grow as it recurses is refused: its expansions
//! never end.

use crate::{
    env::Env,
    expr::ModPath,
    typ::{AbstractId, Typ, Type, cast::grows_on, params_size},
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// Why JSON or TOML may not read back what they write of a type
/// ([`Type::text_refusal`]).
#[derive(Debug, Clone)]
pub enum TextRefusal {
    /// A member with no tag in a union holding another member but `null`;
    /// `read_by` names the abstract type whose `from_value` reads the data
    /// the union is in, where abstract values carry no tag.
    Untagged { union: Type, member: Type, read_by: Option<ArcStr> },
    /// Two members of a union under one tag.
    SharedTag { union: Type, tag: ArcStr, a: Type, b: Type },
    /// `Transparent` over a type that is not abstract.
    Transparent(Type),
    /// A typedef whose parameters grow as it recurses.
    Grows(ModPath),
}

/// Whose types a [`Type::text_refusal`] judges: a `Transparent` over a
/// type known not to be abstract is refused as a program wrote it
/// (`Written`), and left to the definition's check in an instance's body
/// checked again (`Instance`), where the type is the definition's at a
/// call's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextSite {
    Written,
    Instance,
}

/// A named type a walk expands: a typedef by its definition, or an
/// abstract type's representation by its identity.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Named {
    Def(usize),
    Abstract(AbstractId),
}

/// A [`Walk::same`] comparison: the pairs it is inside, the names each
/// side expanded on the way with their arguments' size, and each pair it
/// found apart, or alike without leaning on a pair it was inside
/// (`leaned`).
#[derive(Default)]
struct Same {
    path: LPooled<Vec<(Type, Type)>>,
    left: LPooled<Vec<(usize, usize)>>,
    right: LPooled<Vec<(usize, usize)>>,
    known: LPooled<AHashMap<(Type, Type), bool>>,
    leaned: bool,
}

struct Walk<'a> {
    env: &'a Env,
    /// The abstract type whose `from_value` reads the data the walk is in;
    /// `None` in the text itself.
    read_by: Option<ArcStr>,
    site: TextSite,
    /// Each named type expanded, by what it names, whether it is in data
    /// a `from_value` reads, and its arguments.
    seen: LPooled<AHashSet<(Named, bool, Arc<[Type]>)>>,
    /// The named types the walk is inside, each with its arguments' size.
    path: LPooled<Vec<(Named, usize)>>,
}

impl<'a> Walk<'a> {
    fn new(env: &'a Env, site: TextSite) -> Self {
        Walk { env, read_by: None, site, seen: LPooled::take(), path: LPooled::take() }
    }

    /// The walk of `body`, the expansion of the named type `named` at
    /// `params`, in the data `read_by`'s `from_value` reads, unless the
    /// walk expanded it there already.
    fn named(
        &mut self,
        named: Named,
        read_by: Option<ArcStr>,
        name: impl FnOnce() -> ModPath,
        params: &Arc<[Type]>,
        body: Type,
    ) -> Result<(), TextRefusal> {
        if !self.seen.insert((named, read_by.is_some(), params.clone())) {
            return Ok(());
        }
        let size = params_size(params);
        if grows_on(&self.path, &named, size) {
            return Err(TextRefusal::Grows(name()));
        }
        let outer = std::mem::replace(&mut self.read_by, read_by);
        self.path.push((named, size));
        let r = self.walk(&body);
        self.path.pop();
        self.read_by = outer;
        r
    }

    /// The typedef reference `t`'s body at its arguments; `None` when it
    /// names nothing known. Fills no cell and adds no constraint.
    fn expand(&self, t: &Type) -> Option<(usize, Type)> {
        let Type::Ref(tr) = t else { return None };
        let def = tr.resolve_peek(self.env)?;
        let known = def.bindings(&tr.params)?;
        Some((def.def_key(), def.typ.replace_tvars(&known)))
    }

    /// `t` through its variables and names to its constructor; `None` for
    /// an open variable or a name that names nothing known.
    fn head(&self, t: &Type) -> Option<Type> {
        let mut t = t.clone();
        loop {
            t = match &t {
                Type::TVar(tv) => tv.binding()?,
                Type::Ref(_) => self.expand(&t)?.1,
                _ => return Some(t),
            }
        }
    }

    fn walk(&mut self, t: &Type) -> Result<(), TextRefusal> {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.walk(&b),
                None => Ok(()),
            },
            Type::Ref(tr) => match self.expand(t) {
                Some((def, body)) => {
                    let read_by = self.read_by.clone();
                    self.named(
                        Named::Def(def),
                        read_by,
                        || tr.name.clone(),
                        &tr.params,
                        body,
                    )
                }
                None => Ok(()),
            },
            Type::Set(_) => self.union(t),
            Type::Primitive(p) if (*p & !(Typ::Null | Typ::Error)).len() > 1 => {
                self.union(t)
            }
            Type::Array(e) | Type::List(e) => self.walk(e),
            Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                ts.iter().try_for_each(|t| self.walk(t))
            }
            Type::Struct(fs) => fs.iter().try_for_each(|(_, t, _)| self.walk(t)),
            Type::Map(m) => {
                self.walk(&m.key)?;
                self.walk(&m.value)
            }
            Type::Abstract { id, params } => self.abstract_type(t, *id, params),
            Type::Primitive(_)
            | Type::Any
            | Type::Bottom
            | Type::Error(_)
            | Type::Fn(_)
            | Type::ByRef(..)
            | Type::App(..)
            | Type::Hole
            | Type::Bound(_) => Ok(()),
        })
    }

    /// An abstract type: its representation where its `Cast`
    /// implementation reads or writes it as that, in the data its
    /// `from_value` reads where it writes one; a `Transparent`'s
    /// parameter, which must be an abstract type.
    fn abstract_type(
        &mut self,
        t: &Type,
        id: AbstractId,
        params: &Arc<[Type]>,
    ) -> Result<(), TextRefusal> {
        let Some(rep) = self.env.abstract_reps.get(&id) else { return Ok(()) };
        let body = rep.instantiate_with(params);
        let read_by = if id == AbstractId::transparent() {
            match self.head(&body) {
                Some(h)
                    if self.site == TextSite::Written
                        && !matches!(h, Type::Abstract { .. }) =>
                {
                    return Err(TextRefusal::Transparent(t.clone()));
                }
                _ => self.read_by.clone(),
            }
        } else {
            match (
                self.env.written_as_representation(id),
                self.env.read_as_representation(id),
            ) {
                (false, false) => return Ok(()),
                (_, true) => self.read_by.clone(),
                (true, false) => Some(rep.name.clone()),
            }
        };
        let name = || ModPath::from([rep.name.as_str()]);
        self.named(Named::Abstract(id), read_by, name, params, body)
    }

    /// The members of the union `t` past every union and name in them,
    /// each its constructor beside the type written; an open variable is
    /// its own constructor. An error is no member: no data reads into one.
    /// `seen` holds the names expanded, each once.
    fn members(
        &self,
        t: &Type,
        out: &mut SmallVec<[(Type, Type); 8]>,
        seen: &mut Vec<Type>,
    ) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.members(&b, out, seen),
                None => out.push((t.clone(), t.clone())),
            },
            Type::Set(ms) => ms.iter().for_each(|m| self.members(m, out, seen)),
            Type::Primitive(p) => (*p & !Typ::Error)
                .iter()
                .for_each(|b| out.push((Type::Primitive(b.into()), t.clone()))),
            Type::Bottom | Type::Error(_) => (),
            Type::Ref(_) if seen.contains(t) => (),
            Type::Ref(_) => match self.head(t) {
                Some(h @ (Type::Set(_) | Type::Primitive(_))) => {
                    seen.push(t.clone());
                    self.members(&h, out, seen);
                }
                Some(h) => out.push((h, t.clone())),
                None => out.push((t.clone(), t.clone())),
            },
            t => out.push((t.clone(), t.clone())),
        })
    }

    /// The tag a member writes under: a variant's, or an abstract type's
    /// name in the text; `None` for a member with none.
    fn tag(&self, head: &Type) -> Option<ArcStr> {
        match head {
            Type::Variant(tag, _, _) => Some(tag.clone()),
            Type::Abstract { id, .. }
                if self.read_by.is_none() && *id != AbstractId::transparent() =>
            {
                match self.env.abstract_reps.get(id) {
                    Some(r) => Some(r.name.clone()),
                    None => id.name(),
                }
            }
            _ => None,
        }
    }

    /// Whether `a` and `b` are one type, their names and variables
    /// expanded: unions alike as sets of members, and a pair met again
    /// inside itself alike.
    fn same(&self, cx: &mut Same, a: &Type, b: &Type) -> bool {
        if a == b {
            return true;
        }
        let key = (a.clone(), b.clone());
        if let Some(r) = cx.known.get(&key) {
            return *r;
        }
        if cx.path.contains(&key) {
            cx.leaned = true;
            return true;
        }
        let leaned = std::mem::replace(&mut cx.leaned, false);
        cx.path.push(key.clone());
        let r = crate::stack::ensure_sufficient(|| self.same_inner(cx, a, b));
        cx.path.pop();
        if !r || !cx.leaned {
            cx.known.insert(key, r);
        }
        cx.leaned |= leaned;
        r
    }

    fn same_inner(&self, cx: &mut Same, a: &Type, b: &Type) -> bool {
        let union = |t: &Type| match t {
            Type::Set(_) => true,
            Type::Primitive(p) => p.len() != 1,
            _ => false,
        };
        let all = |cx: &mut Same, xs: &[Type], ys: &[Type]| {
            xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| self.same(cx, x, y))
        };
        match (a, b) {
            (Type::TVar(tv), b) if let Some(a) = tv.binding() => self.same(cx, &a, b),
            (a, Type::TVar(tv)) if let Some(b) = tv.binding() => self.same(cx, a, &b),
            (Type::Ref(tr), b) => {
                let Some((def, body)) = self.expand(a) else { return false };
                let size = params_size(&tr.params);
                if grows_on(&cx.left, &def, size) {
                    return false;
                }
                cx.left.push((def, size));
                let r = self.same(cx, &body, b);
                cx.left.pop();
                r
            }
            (a, Type::Ref(tr)) => {
                let Some((def, body)) = self.expand(b) else { return false };
                let size = params_size(&tr.params);
                if grows_on(&cx.right, &def, size) {
                    return false;
                }
                cx.right.push((def, size));
                let r = self.same(cx, a, &body);
                cx.right.pop();
                r
            }
            (a, b) if union(a) || union(b) => {
                let (mut xs, mut ys) = (SmallVec::new(), SmallVec::new());
                self.members(a, &mut xs, &mut vec![]);
                self.members(b, &mut ys, &mut vec![]);
                let covers = |cx: &mut Same, xs: &[(Type, Type)], ys: &[(Type, Type)]| {
                    xs.iter().all(|(x, _)| ys.iter().any(|(y, _)| self.same(cx, x, y)))
                };
                covers(cx, &xs, &ys) && covers(cx, &ys, &xs)
            }
            (Type::Array(x), Type::Array(y))
            | (Type::List(x), Type::List(y))
            | (Type::Error(x), Type::Error(y)) => self.same(cx, x, y),
            (Type::ByRef(m, x), Type::ByRef(n, y)) => m == n && self.same(cx, x, y),
            (Type::Tuple(xs), Type::Tuple(ys)) => all(cx, xs, ys),
            (Type::Variant(s, xs, _), Type::Variant(t, ys, _)) => {
                s == t && all(cx, xs, ys)
            }
            (Type::Struct(xs), Type::Struct(ys)) => {
                xs.len() == ys.len()
                    && xs
                        .iter()
                        .zip(ys.iter())
                        .all(|((n, x, _), (m, y, _))| n == m && self.same(cx, x, y))
            }
            (Type::Map(m0), Type::Map(m1)) => {
                self.same(cx, &m0.key, &m1.key) && self.same(cx, &m0.value, &m1.value)
            }
            (
                Type::Abstract { id: i, params: p },
                Type::Abstract { id: j, params: q },
            ) => i == j && all(cx, p, q),
            _ => false,
        }
    }

    fn union(&mut self, t: &Type) -> Result<(), TextRefusal> {
        let mut members: SmallVec<[(Type, Type); 8]> = SmallVec::new();
        self.members(t, &mut members, &mut vec![]);
        let null = Type::Primitive(Typ::Null.into());
        members.retain(|(h, _)| *h != null);
        let mut tags: SmallVec<[(ArcStr, &Type, &Type); 8]> = SmallVec::new();
        for (head, written) in members.iter() {
            let Some(tag) = self.tag(head) else {
                if members.iter().any(|(h, _)| h != head) {
                    let (member, read_by) = (head.clone(), self.read_by.clone());
                    return Err(TextRefusal::Untagged {
                        union: t.clone(),
                        member,
                        read_by,
                    });
                }
                continue;
            };
            match tags.iter().find(|(t, _, _)| *t == tag) {
                Some((_, a, a_written)) if !self.same(&mut Same::default(), a, head) => {
                    let (a, b) = ((*a_written).clone(), written.clone());
                    return Err(TextRefusal::SharedTag { union: t.clone(), tag, a, b });
                }
                Some(_) => (),
                None => tags.push((tag, head, written)),
            }
        }
        members.iter().try_for_each(|(_, written)| self.walk(written))
    }
}

impl Type {
    /// Why JSON or TOML may not read back what they write of this type:
    /// a union whose members a tag does not tell apart, a `Transparent`
    /// over a type that is not abstract, a typedef growing its
    /// parameters; `None` when they read back every value of it.
    pub fn text_refusal(&self, env: &Env, site: TextSite) -> Option<TextRefusal> {
        Walk::new(env, site).walk(self).err()
    }

    /// The map at this type's root, past names, variables and a union's
    /// members, that a TOML writer writes as the array of its `[key,
    /// value]` pairs (its key type is known and is not `string`), which a
    /// TOML document, a table, cannot be.
    pub fn toml_root_pairs(&self, env: &Env) -> Option<Type> {
        let walk = Walk::new(env, TextSite::Written);
        let mut members: SmallVec<[(Type, Type); 8]> = SmallVec::new();
        walk.members(self, &mut members, &mut vec![]);
        members.into_iter().find_map(|(head, written)| match &head {
            Type::Map(m) => match walk.head(&m.key)? {
                Type::Primitive(p) if p == Typ::String => None,
                _ => Some(written),
            },
            _ => None,
        })
    }
}

/// Why a TOML document cannot be `map`, a map written as its pairs
/// ([`Type::toml_root_pairs`]); `None` where its type is not at hand.
pub fn toml_root_refusal(map: Option<&Type>) -> CompactString {
    let what = match map {
        Some(m) => format_compact!("{m}, a map whose key type is not string,"),
        None => CompactString::const_new("a map whose key type is not string"),
    };
    format_compact!(
        "a TOML document is a table, and {what} is written as the array of its \
         [key, value] pairs: put it in a struct's field"
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{expr::parser::parse_type, typ::cast::tests::typed};

    fn refused(t: &str) -> bool {
        let env = Env::default();
        parse_type(t).unwrap().text_refusal(&env, TextSite::Written).is_some()
    }

    /// [`refused`] over the typedefs `defs`.
    fn named(defs: &[(&str, &str)], t: &str) -> bool {
        let (env, t) = typed(defs, t);
        t.text_refusal(&env, TextSite::Written).is_some()
    }

    #[test]
    fn unions_tell_their_members_apart_by_a_tag() {
        assert!(!refused("[`A, `B(i64), `C(string, f64), null]"));
        assert!(!refused("[i64, null]"));
        assert!(!refused("[{a: i64}, null]"));
        assert!(!refused("Array<[`A(Array<i64>), `B({x: [f32, null]})]>"));
        assert!(!refused("[[`A, `B], `C]"));
        assert!(!refused("[[`A, null], `A]"));
        assert!(refused("[u8, i64]"));
        assert!(refused("[i64, f64]"));
        assert!(refused("[i64, string]"));
        assert!(refused("[`A, string]"));
        assert!(refused("[`A, null, string]"));
        assert!(refused("[`A(i64), `A(string)]"));
        assert!(refused("[`A, `A(i64)]"));
        assert!(refused("[{a: i64}, {b: i64}]"));
        assert!(refused("[Array<i64>, null, Map<string, i64>]"));
        assert!(refused("Array<[i64, string]>"));
        assert!(refused("{a: [`A, bool]}"));
        assert!(refused("Map<[i64, string], i64>"));
        assert!(!refused("{a: [i64, Error<string>]}"));
        assert!(!refused("[i64, error]"));
        assert!(!refused("[`A, error]"));
        assert!(!refused("Map<[`A, `B], [`C(i64), null]>"));
    }

    #[test]
    fn named_members_are_judged_by_their_constructors() {
        assert!(!named(&[("AB", "[`A, `B]")], "[AB, `C]"));
        assert!(!named(&[("AB", "[`A, `B]")], "[AB, `B]"));
        assert!(!named(&[("O<'a>", "['a, null]")], "O<O<[`A, `B]>>"));
        assert!(named(&[("O<'a>", "['a, null]")], "[O<i64>, `B]"));
        assert!(named(&[("AB", "[`A, `B(i64)]")], "[AB, `B(string)]"));
        assert!(named(&[("S", "{a: i64}")], "[S, `A]"));
        assert!(!named(&[("S", "{a: [S, null]}")], "[S, null]"));
        assert!(!named(
            &[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")],
            "{k: R<i64>, l: R<[`A, `B]>}"
        ));
        assert!(named(&[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")], "[R<i64>, R<string>]"));
        assert!(named(&[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")], "R<[i64, string]>"));
        assert!(!named(
            &[("P<'a, 'b>", "[`N, `C('a, P<'b, 'a>)]")],
            "{k: P<i64, string>}"
        ));
    }

    // Members under one tag that are one type, written through names or
    // not, are one member.
    #[test]
    fn one_type_under_one_tag_through_names() {
        let k = [("K", "string"), ("AK", "[`A(K), `B]")];
        assert!(!named(&k, "Array<[`A(K), `A(string), `B]>"));
        assert!(!named(&k, "Array<[AK, `A(string)]>"));
        assert!(!named(&k, "[`A([K, null]), `A([string, null])]"));
        assert!(!named(&k, "[`A({a: K, b: i64}), `A({a: string, b: i64})]"));
        assert!(named(&k, "[`A(K), `A(i64)]"));
        assert!(named(&[("K", "[string, null]")], "[`A(K), `A(string)]"));
        let r = [
            ("R", "[`Nil, `Cons(i64, R)]"),
            ("S", "[`Nil, `Cons(i64, S)]"),
            ("T", "[`Nil, `Cons(string, T)]"),
        ];
        assert!(!named(&r, "[`A(R), `A(S)]"));
        assert!(named(&r, "[`A(R), `A(T)]"));
    }

    // A map at a TOML document's root is a table only under string keys.
    #[test]
    fn a_map_written_as_pairs_is_no_document() {
        let pairs = |defs: &[(&str, &str)], t: &str| {
            let (env, t) = typed(defs, t);
            t.toml_root_pairs(&env).map(|m| m.to_string())
        };
        assert_eq!(pairs(&[], "Map<i64, string>").as_deref(), Some("Map<i64, string>"));
        assert_eq!(pairs(&[], "[Map<[`A, `B], i64>, null]").is_some(), true);
        assert_eq!(pairs(&[("M", "Map<i64, i64>")], "M").as_deref(), Some("M"));
        assert_eq!(pairs(&[("K", "string")], "Map<K, i64>"), None);
        assert_eq!(pairs(&[], "Map<string, i64>"), None);
        assert_eq!(pairs(&[], "{m: Map<i64, i64>}"), None);
    }

    // A union's members past the unions its names hold expand each name
    // once, however many paths reach it.
    #[test]
    fn union_members_expand_each_name_once() {
        let defs = crate::typ::cast::tests::union_dag(20);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "[L20, `D]");
        let walk = Walk::new(&env, TextSite::Written);
        let mut out = SmallVec::new();
        walk.members(&t, &mut out, &mut vec![]);
        let leaves: Vec<String> = out.iter().map(|(h, _)| h.to_string()).collect();
        assert_eq!(leaves, ["`A", "`B", "`A", "`C", "`D"]);
        assert!(t.text_refusal(&env, TextSite::Written).is_none());
    }

    // A typedef expands once for each list of arguments it is applied to,
    // so a recursion that passes its parameters on is a cycle; one that
    // grows them at every level never ends, and is refused.
    #[test]
    fn a_typedef_growing_its_parameters_is_refused() {
        assert!(!named(&[("A<'a>", "[`Here('a), `Next(A<Array<i64>>)]")], "A<i64>"));
        let (env, t) = typed(&[("N<'a>", "[`Nil, `Cons('a, N<Array<'a>>)]")], "N<i64>");
        assert!(matches!(
            t.text_refusal(&env, TextSite::Written),
            Some(TextRefusal::Grows(n)) if n.to_string() == "N"
        ));
    }
}
