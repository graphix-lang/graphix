//! A canonical key for types read as regular trees: every resolved
//! reference expanded, recursion included, so two types equal under
//! equi-recursive containment (`type R = [i64, Array<R>]` and `type S =
//! [i64, Array<R>]`) have one key, whatever form each was written in.

use crate::typ::{Type, TypeRef, cast::grows_on, params_size};
use ahash::AHashMap;
use compact_str::CompactString;
use compact_str::format_compact;
use netidx_core::pack::encode_varint;
use netidx_core::path::Path;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// A node of the expanded graph. `Forward` stands for a reference while
/// its expansion is built; the graph is read through it once built.
enum Node {
    Leaf(LPooled<Vec<u8>>),
    Ordered(LPooled<Vec<u8>>, SmallVec<[usize; 4]>),
    /// A union's members other than its primitives, in no order.
    Union(SmallVec<[usize; 4]>),
    Forward(Option<usize>),
}

#[derive(Default)]
struct Graph {
    nodes: Vec<Node>,
    /// Each reference by its definition and its arguments.
    refs: AHashMap<(usize, Arc<[Type]>), usize>,
    /// The definitions and parameter sizes of the references being
    /// expanded, outermost first.
    path: Vec<(usize, usize)>,
}

fn label(tag: u8, rest: &[u8]) -> LPooled<Vec<u8>> {
    let mut l: LPooled<Vec<u8>> = LPooled::take();
    l.push(tag);
    l.extend_from_slice(rest);
    l
}

impl Graph {
    fn push(&mut self, n: Node) -> usize {
        self.nodes.push(n);
        self.nodes.len() - 1
    }

    fn leaf(&mut self, tag: u8, rest: &[u8]) -> usize {
        self.push(Node::Leaf(label(tag, rest)))
    }

    fn ordered(&mut self, tag: u8, rest: &[u8], kids: &[Type]) -> usize {
        let kids = kids.iter().map(|k| self.node(k)).collect();
        self.push(Node::Ordered(label(tag, rest), kids))
    }

    /// The expansion of a resolved reference, else `None`.
    fn expand(t: &Type) -> Option<(usize, Type)> {
        let Type::Ref(tr) = t else { return None };
        Some((tr.def_key()?, tr.expand_cell()?))
    }

    /// The name of the definition `tr` resolved to, in the scope it is
    /// defined in.
    fn defined_name(tr: &TypeRef) -> CompactString {
        let base = Path::basename(&tr.name.0).unwrap_or("");
        match tr.resolved() {
            Some(r) => format_compact!("{}::{base}", r.canonical_scope()),
            None => format_compact!("{}", tr.name),
        }
    }

    fn node(&mut self, t: &Type) -> usize {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.node(&b),
                None => self.leaf(b'v', &[]),
            },
            // CR claude for claude: [bug/low] A reference that does not expand here is
            // keyed by its written name, and a nested reference in an inline written
            // type argument reaches the key unresolved: with `type R<'a> = [`Nil,
            // `Cons('a, R<'a>)]; type B<'x> = Abstract<i64>`, `B<R<i64>>` and
            // `B<[`Nil, `Cons(i64, R<i64>)]>` are equal types whose values compare
            // unequal (`c == e` false), where the same argument through an alias `type
            // E = [`Nil, `Cons(i64, R<i64>)]` compares equal; a map keyed by such
            // values takes two entries for one key. Main a084f9887 hangs on these
            // programs (regular-grows-01), so nothing saw it before. Resolve the nested
            // reference where the written argument is seeded, or key an unexpanded
            // reference by the definition it names. probes:
            // ~/ptmp/cr29/scratch/review-r6/B2-r1-1/rg8.gx, rg9.gx (regular-inline-ref-01)
            // 2026-11-02 claude: Parked with default-typedef-outlived-01 (for eric). Lane
            // E filled written names at the check's end: a let's annotation first
            // (ada956703), then every type outside a definition (6e09af91c,
            // seed_node_types, about +0.2% on a build). Reviews found values of one
            // written type keyed apart by the road that decided them (E4-r1-2 ri11: false
            // 2 1 where main gives true 1 1), and names filled in the wrong scope. Keying
            // an unexpanded reference by the definition it names needs that name resolved
            // where it was written, which is the parked question.
            Type::Ref(tr) => match Self::expand(t) {
                None => {
                    let name = format_compact!("{}", tr.name);
                    self.ordered(b'r', name.as_bytes(), &tr.params)
                }
                Some((def, body)) => {
                    let key = (def, tr.params.clone());
                    if let Some(id) = self.refs.get(&key) {
                        return *id;
                    }
                    // an application whose parameters grow at every level
                    // never closes into a regular tree: it is its
                    // definition over its parameters
                    let size = params_size(&tr.params);
                    if grows_on(&self.path, &def, size) {
                        let name = Self::defined_name(tr);
                        return self.ordered(b'g', name.as_bytes(), &tr.params);
                    }
                    let id = self.push(Node::Forward(None));
                    self.refs.insert(key, id);
                    self.path.push((def, size));
                    let body = self.node(&body);
                    self.path.pop();
                    self.nodes[id] = Node::Forward(Some(body));
                    id
                }
            },
            Type::Set(_) | Type::Primitive(_) | Type::Bottom => {
                let mut prims = enumflags2::BitFlags::empty();
                let mut members: SmallVec<[usize; 4]> = SmallVec::new();
                self.members(t, &mut prims, &mut members);
                if !prims.is_empty() {
                    let p = self.leaf(b'p', &prims.bits().to_le_bytes());
                    members.push(p);
                }
                match &members[..] {
                    [] => self.leaf(b'_', &[]),
                    [m] => *m,
                    _ => self.push(Node::Union(members)),
                }
            }
            Type::Any => self.leaf(b'a', &[]),
            Type::Hole => self.leaf(b'h', &[]),
            Type::Error(t) => self.ordered(b'e', &[], std::slice::from_ref(&**t)),
            Type::Array(t) => self.ordered(b'A', &[], std::slice::from_ref(&**t)),
            Type::List(t) => self.ordered(b'L', &[], std::slice::from_ref(&**t)),
            Type::ByRef(m, t) => {
                let m = format_compact!("{m:?}");
                self.ordered(b'&', m.as_bytes(), std::slice::from_ref(&**t))
            }
            Type::Tuple(ts) => self.ordered(b't', &[], ts),
            Type::Struct(fs) => {
                let mut names: LPooled<Vec<u8>> = LPooled::take();
                for (n, _, _) in fs.iter() {
                    encode_varint(n.len() as u64, &mut *names);
                    names.extend_from_slice(n.as_bytes());
                }
                let kids: LPooled<Vec<Type>> =
                    fs.iter().map(|(_, t, _)| t.clone()).collect();
                self.ordered(b's', &names, &kids)
            }
            Type::Variant(tag, ts, _) => self.ordered(b'V', tag.as_bytes(), ts),
            Type::Map { key, value } => {
                self.ordered(b'm', &[], &[(**key).clone(), (**value).clone()])
            }
            Type::Abstract { id, params } => {
                let id = format_compact!("{id:?}");
                self.ordered(b'b', id.as_bytes(), params)
            }
            // a function, an application and a bound are compared as
            // they print
            Type::Fn(_) | Type::App(..) | Type::Bound(_) => {
                let s = format_compact!("{t}");
                self.leaf(b'f', s.as_bytes())
            }
        })
    }

    /// A union's members, nested unions flattened through bindings and
    /// references (a typedef is contractive, so a union never reaches
    /// itself without a constructor between), its primitives merged.
    fn members(
        &mut self,
        t: &Type,
        prims: &mut enumflags2::BitFlags<crate::typ::Typ>,
        out: &mut SmallVec<[usize; 4]>,
    ) {
        crate::stack::ensure_sufficient(|| match t {
            Type::Bottom => (),
            Type::Primitive(p) => *prims |= *p,
            Type::Set(ts) => ts.iter().for_each(|m| self.members(m, prims, out)),
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.members(&b, prims, out),
                None => out.push(self.node(t)),
            },
            Type::Ref(_) => match Self::expand(t) {
                Some((_, body)) if matches!(body, Type::Set(_) | Type::Primitive(_)) => {
                    self.members(&body, prims, out)
                }
                _ => out.push(self.node(t)),
            },
            t => out.push(self.node(t)),
        })
    }

    /// The node `n` stands for once every forward is followed.
    fn target(&self, mut n: usize) -> usize {
        while let Node::Forward(Some(t)) = self.nodes[n] {
            n = t
        }
        n
    }
}

/// One node's signature in a refinement round: its label and its
/// children's ranks, a union's sorted and deduplicated.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Sig<'a> {
    label: &'a [u8],
    union: bool,
    kids: SmallVec<[usize; 4]>,
}

/// The key of `roots` read as regular trees: equal exactly when each
/// root is the same regular tree as its counterpart.
pub(crate) fn key(roots: &[Type]) -> Arc<[u8]> {
    let mut g = Graph::default();
    let roots: SmallVec<[usize; 4]> = roots.iter().map(|t| g.node(t)).collect();
    let n = g.nodes.len();
    let kids_of = |i: usize| -> (&[u8], bool, SmallVec<[usize; 4]>) {
        match &g.nodes[i] {
            Node::Leaf(l) => (&l[..], false, SmallVec::new()),
            Node::Ordered(l, ks) => {
                (&l[..], false, ks.iter().map(|k| g.target(*k)).collect())
            }
            Node::Union(ks) => (&[], true, ks.iter().map(|k| g.target(*k)).collect()),
            Node::Forward(_) => (&[], false, SmallVec::new()),
        }
    };
    let live: LPooled<Vec<usize>> =
        (0..n).filter(|i| !matches!(g.nodes[*i], Node::Forward(_))).collect();
    // ranks by sorted signature, so they depend on the trees alone, never
    // on the order the graph was built in
    let mut rank: LPooled<Vec<usize>> = LPooled::take();
    rank.resize(n, 0);
    let mut classes = 0;
    loop {
        let sigs: LPooled<Vec<(usize, Sig)>> = live
            .iter()
            .map(|i| {
                let (label, union, ks) = kids_of(*i);
                let mut kids: SmallVec<[usize; 4]> =
                    ks.iter().map(|k| rank[*k]).collect();
                if union {
                    kids.sort_unstable();
                    kids.dedup();
                }
                (*i, Sig { label, union, kids })
            })
            .collect();
        let mut distinct: LPooled<Vec<&Sig>> = sigs.iter().map(|(_, s)| s).collect();
        distinct.sort();
        distinct.dedup();
        let next: LPooled<Vec<usize>> = sigs
            .iter()
            .map(|(_, s)| distinct.binary_search(&s).expect("present"))
            .collect();
        let count = distinct.len();
        drop(distinct);
        for ((i, _), r) in sigs.iter().zip(next.iter()) {
            rank[*i] = *r
        }
        if count == classes {
            break;
        }
        classes = count;
    }
    // a representative per class, and the class's children by rank
    let mut rep: LPooled<AHashMap<usize, usize>> = LPooled::take();
    for i in live.iter() {
        rep.entry(rank[*i]).or_insert(*i);
    }
    let mut out: Vec<u8> = Vec::new();
    let mut seen: LPooled<AHashMap<usize, u64>> = LPooled::take();
    let mut stack: LPooled<Vec<usize>> = LPooled::take();
    for r in roots.iter() {
        stack.push(rank[g.target(*r)]);
        while let Some(c) = stack.pop() {
            if let Some(k) = seen.get(&c) {
                out.push(0);
                encode_varint(*k, &mut out);
                continue;
            }
            let visit = seen.len() as u64;
            seen.insert(c, visit);
            let (label, union, ks) = kids_of(rep[&c]);
            let mut kids: SmallVec<[usize; 4]> = ks.iter().map(|k| rank[*k]).collect();
            if union {
                kids.sort_unstable();
                kids.dedup();
            }
            out.push(if union { 2 } else { 1 });
            encode_varint(label.len() as u64, &mut out);
            out.extend_from_slice(label);
            encode_varint(kids.len() as u64, &mut out);
            stack.extend(kids.iter().rev());
        }
    }
    Arc::from(out)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::env::Env;

    // A typedef applied to arguments is one node for each list of them,
    // so a recursion that passes its parameter on is a cycle.
    #[test]
    fn an_applied_recursive_typedef_is_a_cycle() {
        let defs =
            [("R<'a>", "[`Nil, `Cons('a, R<'a>)]"), ("S", "[`Nil, `Cons(u64, S)]")];
        let typed = |t| Env::typed_at_root(&defs, t).unwrap();
        let ((_r_env, r), (_s_env, s)) = (typed("R<u64>"), typed("S"));
        let (_i_env, i) = typed("R<i64>");
        assert_eq!(key(&[r.clone()]), key(&[s]));
        assert_ne!(key(&[r]), key(&[i]));
    }

    // A typedef whose arguments grow as it recurses is no regular tree:
    // its key ends, and tells its applications apart by their arguments.
    #[test]
    fn a_growing_application_has_a_key() {
        let defs = [("N<'a>", "[`Nil, `Cons('a, N<Array<'a>>)]")];
        let typed = |t| Env::typed_at_root(&defs, t).unwrap();
        let ((_a_env, a), (_b_env, b)) = (typed("N<i64>"), typed("N<i64>"));
        let (_c_env, c) = typed("N<u64>");
        assert_eq!(key(&[a.clone()]), key(&[b]));
        assert_ne!(key(&[a]), key(&[c]));
    }
}
