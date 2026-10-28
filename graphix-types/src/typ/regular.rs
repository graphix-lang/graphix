//! A canonical key for types read as regular trees: every resolved
//! reference expanded, recursion included, so two types equal under
//! equi-recursive containment (`type R = [i64, Array<R>]` and `type S =
//! [i64, Array<R>]`) have one key, whatever form each was written in.

use crate::typ::Type;
use ahash::AHashMap;
use compact_str::format_compact;
use netidx_core::pack::encode_varint;
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
    /// Each reference by its definition and its arguments' nodes.
    refs: AHashMap<(usize, SmallVec<[usize; 4]>), usize>,
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
        let r = tr.resolved()?;
        let known = r.bindings(&tr.params)?;
        Some((r.def_key(), r.typ.replace_tvars(&known)))
    }

    fn node(&mut self, t: &Type) -> usize {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => self.node(&b),
                None => self.leaf(b'v', &[]),
            },
            Type::Ref(tr) => match Self::expand(t) {
                None => {
                    let name = format_compact!("{}", tr.name);
                    self.ordered(b'r', name.as_bytes(), &tr.params)
                }
                Some((def, body)) => {
                    let args: SmallVec<[usize; 4]> =
                        tr.params.iter().map(|p| self.node(p)).collect();
                    if let Some(id) = self.refs.get(&(def, args.clone())) {
                        return *id;
                    }
                    let id = self.push(Node::Forward(None));
                    self.refs.insert((def, args), id);
                    let body = self.node(&body);
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
