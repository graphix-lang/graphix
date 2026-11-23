use crate::{
    AddrMap, AddrSet, PRINT_FLAGS, PrintFlag, SourcePosition,
    dbgenv::gxdbg_typeref,
    env::Env,
    expr::{ModPath, Origin, Source, WrittenAt},
    format_with_flags,
    image::{self, KeyedNode},
    stack::ensure_sufficient,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack as PackTrait, PackError, encode_varint},
    path::Path,
};
use netidx_value::Typ;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::{IsoPoolable, local::LPooled};
use smallvec::SmallVec;
use std::{
    cell::{OnceCell, RefCell},
    cmp::Ordering,
    collections::hash_map::Entry,
    fmt::Debug,
    hash::{Hash, Hasher},
    iter, mem,
    ops::{ControlFlow, Deref, DerefMut},
    sync::{self, LazyLock, OnceLock, Weak},
};
use triomphe::Arc;

pub mod bounds;
mod cast;
pub use cast::{
    CastInput, Indiscernible, IsAFlags, Open, ShallowScrutinee, numbers_ordered,
};
mod contains;
pub(crate) use contains::find_impl;
pub use contains::{AmbiguousMember, InfiniteType, TypeMismatch};
pub(crate) mod expansive;
pub mod fntyp;
pub mod holds;
mod matches;
mod normalize;
pub use normalize::{NormKey, norm_key};
mod print;
pub use print::{Brief, Noted};
mod readoff;
pub(crate) mod regular;
mod textdata;
pub use textdata::{TextRefusal, toml_root_refusal};
mod setops;
pub(crate) use setops::strictly_identical;
#[doc(hidden)]
pub mod settle;
pub(crate) mod tval;
pub(crate) mod variance;
pub use tval::member_of;
#[doc(hidden)]
pub mod tvar;

pub use fntyp::{
    DefaultType, FnArgKind, FnArgType, FnType, ParseDepth, check_bounds_acyclic,
    declared_name, seed_conjuncts, written_name,
};
pub use tval::{Naked, NakedPrefix, PlainScalar, TVal};
pub use tvar::TVar;

struct AndAc(bool);

impl FromIterator<bool> for AndAc {
    fn from_iter<T: IntoIterator<Item = bool>>(iter: T) -> Self {
        AndAc(iter.into_iter().all(|b| b))
    }
}

impl Type {
    /// What a walk that visits each shared node once keys it by: a
    /// composite's content allocation, a reference's, a var's cell;
    /// `None` for a leaf and for `App`, whose content is two allocations.
    pub fn shared_identity(&self) -> Option<usize> {
        match self {
            Type::TVar(tv) => Some(tv.cell_addr()),
            Type::Ref(r) => Some(Arc::as_ptr(r).addr()),
            t => node_addr(t),
        }
    }

    /// What a rewrite keys the nodes it rebuilds by: [`Self::node_key`]
    /// of a node with parts ([`node_addr`]), so `None` for a var, which
    /// it takes by its cell, and for a tag or an abstract type with no
    /// parameter.
    pub(super) fn part_key(&self) -> Option<NodeKey> {
        match self {
            Type::Variant(_, ts, _) | Type::Abstract { params: ts, .. }
                if ts.is_empty() =>
            {
                None
            }
            t => node_addr(t).map(|a| (a, t.name_word())),
        }
    }

    /// What two nodes over one content allocation may differ by: a
    /// variant's tag, an abstract type's id.
    fn name_word(&self) -> usize {
        match self {
            Type::Variant(tag, _, _) => tag.as_ptr().addr(),
            Type::Abstract { id, .. } => id.0 as usize,
            _ => 0,
        }
    }

    /// What a walk that remembers answers about shared nodes keys one by:
    /// its [`Type::shared_identity`] and what that leaves out, which two
    /// nodes over one content allocation may differ by (a variant's tag,
    /// an abstract type's id).
    pub fn node_key(&self) -> Option<NodeKey> {
        Some((self.shared_identity()?, self.name_word()))
    }

    /// [`Self::param_key`], each named type applied to parameters under
    /// this one that `app` knows by two words of its own (the same for
    /// every application of one definition to the same parameters, and
    /// for no other) keyed by them: the key of a parameter nested deep is
    /// then short.
    pub(super) fn param_key_with(&self, app: AppWords<'_>) -> ParamKey {
        let mut words = SmallVec::new();
        self.key_words(&mut words, &mut KeyWalk { at: LPooled::take(), app });
        ParamKey(words)
    }

    /// The words of [`Type::param_key`]: a tag, then the part's content; a
    /// shared node met again in this key by the position its first words
    /// start at (`walk.at`, by its [`Type::node_key`]).
    fn key_words(&self, out: &mut SmallVec<[usize; 8]>, walk: &mut KeyWalk) {
        #[cfg(test)]
        KEY_STEPS.with(|n| n.set(n.get() + 1));
        let each = |ts: &[Type], out: &mut SmallVec<[usize; 8]>, walk: &mut KeyWalk| {
            out.push(ts.len());
            ts.iter().for_each(|t| t.key_words(out, walk))
        };
        if let Some(k) = self.node_key() {
            match walk.at.entry(k) {
                Entry::Occupied(e) => return out.extend([7, *e.get()]),
                Entry::Vacant(e) => {
                    e.insert(out.len());
                }
            }
        }
        ensure_sufficient(|| match self {
            Type::Bottom => out.push(0),
            Type::Any => out.push(1),
            Type::Hole => out.push(2),
            Type::Bound(b) => out.extend([3, *b as usize]),
            Type::Primitive(p) => out.extend([4, p.bits() as usize]),
            Type::TVar(tv) => out.extend([5, tv.cell_addr()]),
            Type::Fn(f) if !f.quantifiers.is_empty() => {
                out.extend([6, Arc::as_ptr(f).addr()])
            }
            Type::Ref(r) => {
                let known = match r.params.is_empty() {
                    true => None,
                    false => (walk.app)(self),
                };
                match known {
                    Some([def, id]) => out.extend([8, def, id]),
                    None => {
                        out.extend([10, Arc::as_ptr(&r.resolved).addr()]);
                        each(&r.params, out, walk)
                    }
                }
            }
            Type::Set(ts) => {
                out.push(11);
                each(ts, out, walk)
            }
            Type::Tuple(ts) => {
                out.push(12);
                each(ts, out, walk)
            }
            Type::Error(t) => {
                out.push(13);
                t.key_words(out, walk)
            }
            Type::Array(t) => {
                out.push(14);
                t.key_words(out, walk)
            }
            Type::List(t) => {
                out.push(15);
                t.key_words(out, walk)
            }
            Type::ByRef(Mutability::Shared, t) => {
                out.push(16);
                t.key_words(out, walk)
            }
            Type::ByRef(Mutability::Mut, t) => {
                out.push(17);
                t.key_words(out, walk)
            }
            Type::Struct(fs) => {
                out.extend([18, fs.len()]);
                for (name, t, _) in fs.iter() {
                    text_words(name, out);
                    t.key_words(out, walk)
                }
            }
            Type::Variant(tag, ts, _) => {
                out.push(19);
                text_words(tag, out);
                each(ts, out, walk)
            }
            Type::Map(m) => {
                out.push(20);
                m.key.key_words(out, walk);
                m.value.key_words(out, walk)
            }
            Type::Abstract { id, params } => {
                out.extend([21, id.0 as usize]);
                each(params, out, walk)
            }
            Type::App(c, a) => {
                out.push(22);
                c.key_words(out, walk);
                a.key_words(out, walk)
            }
            Type::Fn(f) => {
                let FnType {
                    args,
                    vargs,
                    rtype,
                    throws,
                    explicit_throws,
                    quantifiers: _,
                    lambda_ids,
                } = &**f;
                let own = lambda_ids.own().map_or(0, |id| id.inner() as usize + 1);
                out.extend([23, own, args.len()]);
                for a in args.iter() {
                    match &a.kind {
                        FnArgKind::Positional { name: None } => out.push(0),
                        FnArgKind::Positional { name: Some(n) } => {
                            out.push(1);
                            text_words(n, out)
                        }
                        FnArgKind::Labeled { name, default } => {
                            let own = default.as_ref().and_then(|d| d.own());
                            let kind = match default {
                                None => 0,
                                Some(DefaultType::Param) => 1,
                                Some(DefaultType::Of(_)) => 2,
                                Some(DefaultType::Each) => 3,
                            };
                            out.extend([2, kind]);
                            text_words(name, out);
                            if let Some(d) = own {
                                d.key_words(out, walk)
                            }
                        }
                    }
                    a.typ.key_words(out, walk)
                }
                match vargs {
                    None => out.push(0),
                    Some(t) => {
                        out.push(1);
                        t.key_words(out, walk)
                    }
                }
                rtype.key_words(out, walk);
                throws.key_words(out, walk);
                out.push(*explicit_throws as usize)
            }
        })
    }
}

/// `s` as key words: its length, then its bytes eight to a word.
fn text_words(s: &str, out: &mut SmallVec<[usize; 8]>) {
    out.push(s.len());
    out.extend(s.as_bytes().chunks(8).map(|c| {
        let mut w = [0u8; 8];
        w[..c.len()].copy_from_slice(c);
        u64::from_le_bytes(w) as usize
    }))
}

/// A shared node as [`Type::node_key`] keys it: its identity, then a
/// variant's tag or an abstract type's id.
pub type NodeKey = (usize, usize);

/// What a walk knows an application by, if it does
/// ([`Type::param_key_with`]).
pub(super) type AppWords<'a> = &'a mut dyn FnMut(&Type) -> Option<[usize; 2]>;

/// One key's walk: where each shared node's first words start, and the
/// applications its owner knows.
struct KeyWalk<'a> {
    at: LPooled<AHashMap<NodeKey, usize>>,
    app: AppWords<'a>,
}

/// A typedef application's parameter as [`Type::param_key`] keys it: by
/// its content, a variable by its cell, a name by its resolution and
/// parameters (or by what the walk knows it as), a function type with
/// quantifiers of its own by its node and any other with the lambda it
/// is, a shared node met again by where its first words start.
/// Each tag fixes the words that follow it, so equal keys are one type,
/// though not one set of linked lambdas: a function type's
/// `lambda_ids` past its own id are left out, so a memo hit on a second,
/// content-equal pair skips the link its containment would make.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamKey(SmallVec<[usize; 8]>);

/// A typedef application as a walk that meets it many times keys it: its
/// definition and its parameters' keys ([`Type::param_key`]).
pub type AppKey = (usize, SmallVec<[ParamKey; 2]>);

impl TypeRef {
    /// The [`AppKey`] of this application of the definition `def`.
    pub fn app_key(&self, def: usize) -> AppKey {
        self.app_key_with(def, &mut |_| None)
    }

    /// [`Self::app_key`] over [`Type::param_key_with`].
    pub(super) fn app_key_with(&self, def: usize, app: AppWords<'_>) -> AppKey {
        (def, self.params.iter().map(|p| p.param_key_with(app)).collect())
    }
}

/// The typedef applications a walk has expanded, by [`AppKey`], each held
/// so its keys stay its own.
#[derive(Default)]
pub(crate) struct Unfolded(LPooled<AHashMap<AppKey, Type>>);

impl Unfolded {
    /// Whether the walk meets `t`, a resolved typedef application, for
    /// the first time.
    pub(crate) fn first(&mut self, t: &Type) -> bool {
        let Type::Ref(tr) = t else { return true };
        let Some(def) = tr.def_key() else { return true };
        match self.0.entry(tr.app_key(def)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(t.clone());
                true
            }
        }
    }
}

/// The pairs of shared composites a walk over two types found to give
/// one answer that holds for the rest of the walk (a conjunctive walk's
/// agreements, a disjunctive walk's falses), so a DAG is walked once per
/// pair of nodes rather than once per path.
/// A walk remembers nothing for its first `NodePairs::EAGER` nodes,
/// which is every walk of a small type. Each pair is held for the walk,
/// so an address stays its node's.
#[derive(Default)]
pub(super) struct NodePairs {
    seen: u32,
    found: Option<LPooled<AHashMap<(NodeKey, NodeKey), (Type, Type)>>>,
}

impl NodePairs {
    pub(super) const EAGER: u32 = 64;

    /// The key of the node pair `t0`, `t1`, when both are shared
    /// composites and the walk has grown past `EAGER`.
    #[inline]
    pub(super) fn key(&mut self, t0: &Type, t1: &Type) -> Option<(NodeKey, NodeKey)> {
        if self.seen < Self::EAGER {
            self.seen += 1;
            return None;
        }
        match (t0, t1) {
            (Type::TVar(_), _) | (_, Type::TVar(_)) => None,
            _ => Some((t0.node_key()?, t1.node_key()?)),
        }
    }

    pub(super) fn contains(&self, key: (NodeKey, NodeKey)) -> bool {
        self.found.as_ref().is_some_and(|f| f.contains_key(&key))
    }

    pub(super) fn insert(&mut self, key: (NodeKey, NodeKey), t0: &Type, t1: &Type) {
        self.found
            .get_or_insert_with(LPooled::take)
            .insert(key, (t0.clone(), t1.clone()));
    }

    pub(super) fn clear(&mut self) {
        if let Some(f) = &mut self.found {
            f.clear()
        }
    }
}

impl Type {
    /// Whether `self` and `other` are one shared node ([`node_addr`]).
    pub fn same_node(&self, other: &Type) -> bool {
        node_addr(self).is_some_and(|a| node_addr(other) == Some(a))
    }
}

/// The address of a composite's content allocation, its low bits telling
/// apart the kinds (and a reference's mutability) that can wrap one
/// allocation, and of a named type applied to parameters, for walks that
/// visit each shared node once; `None` for leaves, a name with no
/// parameter among them, and for `App`, whose content is two allocations.
pub(super) fn node_addr(t: &Type) -> Option<usize> {
    const _: () = assert!(align_of::<Type>() >= 8);
    let slice = |a: &Arc<[Type]>, kind: usize| (**a).as_ptr().addr() | kind;
    let boxed = |a: &Arc<Type>, kind: usize| (&**a as *const Type).addr() | kind;
    match t {
        Type::Set(a) => Some(slice(a, 0)),
        Type::Tuple(a) => Some(slice(a, 1)),
        Type::Variant(_, a, _) => Some(slice(a, 2)),
        Type::Abstract { params: a, .. } => Some(slice(a, 3)),
        Type::Struct(a) => Some((**a).as_ptr().addr()),
        Type::Fn(f) => Some((&**f as *const FnType).addr()),
        Type::Array(a) => Some(boxed(a, 0)),
        Type::List(a) => Some(boxed(a, 1)),
        Type::Error(a) => Some(boxed(a, 2)),
        Type::ByRef(Mutability::Shared, a) => Some(boxed(a, 3)),
        Type::ByRef(Mutability::Mut, a) => Some(boxed(a, 4)),
        Type::Map(m) => Some((&**m as *const MapType).addr()),
        Type::Ref(r) if !r.params.is_empty() => Some(Arc::as_ptr(r).addr()),
        Type::Ref(_)
        | Type::App(..)
        | Type::Hole
        | Type::Bound(_)
        | Type::Primitive(_)
        | Type::Any
        | Type::Bottom
        | Type::TVar(_) => None,
    }
}

/// A `collect_tvars` walk's state: each node walked (`nodes` holds it, so
/// a normalized conjunct dropped mid-walk cannot lend its address to a
/// node never walked), one normal form for every conjunct it meets, so a
/// conjunct met again is the node walked before, and the quantifiers of
/// the signature it is in (`scope`, [`FnType::in_name_scope`]).
struct TVarsSeen {
    nodes: LPooled<AHashMap<usize, Type>>,
    norm: normalize::NormCx,
    scope: Option<LPooled<AHashSet<ArcStr>>>,
}

impl TVarsSeen {
    fn take() -> Self {
        Self { nodes: LPooled::take(), norm: normalize::NormCx::take(), scope: None }
    }

    /// Whether the walk passes `tv`'s conjuncts. A signature's walk passes
    /// those of its own quantifiers alone: a cell declared outside it is
    /// its declarer's, whose bounds name its declarer's variables, never
    /// the signature's. A walk no signature encloses passes every cell's.
    fn passes(&self, tv: &TVar) -> bool {
        self.scope.as_ref().is_none_or(|own| own.contains(&tv.name))
    }
}

/// A relation's question about two types, by their [`RefHist::ref_id`]s.
type RefPair = (Option<usize>, Option<usize>);

/// A walk's answer for a pair of types, kept with the pair, which holds
/// every address the pair's [`RefPair`] reads while the answer is kept.
struct Kept<A> {
    answer: A,
    _sides: (Type, Type),
}

impl<A> Kept<A> {
    fn new(answer: A, t0: &Type, t1: &Type) -> Self {
        Kept { answer, _sides: (t0.clone(), t1.clone()) }
    }
}

/// What a memo holds beside a key that reads a type's address
/// ([`Type::shared_identity`]), so no other type takes the address while
/// the key stands: a var's cell, which a merge takes from the var; any
/// other node itself.
enum Held {
    Cell { _cell: tvar::CellRef },
    Node { _node: Type },
}

impl Held {
    fn of(t: &Type) -> Self {
        match t {
            Type::TVar(tv) => Held::Cell { _cell: tv.cell() },
            t => Held::Node { _node: t.clone() },
        }
    }
}

/// A relation's cycle memo: ids for the types a walk meets, and
/// `inner`, the relation's own record of the pairs in progress.
/// A pooled container taken at its first write: most walks never
/// write theirs.
struct Lazy<T: IsoPoolable>(Option<LPooled<T>>);

impl<T: IsoPoolable> Default for Lazy<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: IsoPoolable> Lazy<T> {
    fn new() -> Self {
        Self(None)
    }

    fn get(&self) -> Option<&T> {
        self.0.as_deref()
    }

    fn get_mut(&mut self) -> &mut T {
        self.0.get_or_insert_with(LPooled::take)
    }
}

struct RefHist<H: IsoPoolable> {
    /// Taken from the pool at the first write; `empty` answers reads before.
    inner: Option<LPooled<H>>,
    empty: OnceCell<H>,
    /// Definition key → the resolution it came from, pinned so the key
    /// is not reused.
    ref_defs: Lazy<AddrMap<sync::Arc<ResolvedRef>>>,
    /// (Definition key, param list address) → the param list, pinned,
    /// and its application's id: a ref met again keeps its id.
    ref_nodes: Lazy<AHashMap<(usize, usize), (Arc<[Type]>, usize)>>,
    /// (Definition key, the params' [`Self::param_id`]s) → id.
    ref_apps: Lazy<AHashMap<(usize, SmallVec<[usize; 4]>), usize>>,
    /// Content identity → id for non-Ref types, so the cycle memo does
    /// not conflate distinct finite sub-problems, each type held so the
    /// address its key reads stays its own.
    content_ids: Lazy<AHashMap<NormKey, (usize, Held)>>,
    /// Structure → id for composite non-Ref types, open cells by their
    /// cell: an expansion rebuilt at every visit meets its earlier self.
    /// A hash bucket holds the types themselves, matched by
    /// `union_identical_in`.
    shape_ids: Lazy<IntMap<u64, SmallVec<[(Type, usize); 1]>>>,
    /// The hashes `shape_ids` takes that read no cell, kept for every
    /// later one.
    fixed_hashes: Lazy<FixedHashes>,
    next_id: usize,
}

/// The shape hashes a [`RefHist`] keeps for all its walks: those of the
/// shared nodes whose hash reads no cell, each node held.
type FixedHashes = AddrMap<(Type, u64)>;

/// A hash of `t`'s structure consistent with `union_identical`: a bound
/// cell is its binding, an open cell its cell (inside a function type,
/// whose identity pairs its cells, any open cell alike), a union's members
/// in any order.
fn shape_hash(t: &Type, fixed: &mut FixedHashes) -> u64 {
    let mut memo = ShapeMemo { outside: LPooled::take(), in_fn: Lazy::new() };
    shape_hash_memo(t, false, &mut memo, fixed).0
}

/// The hashes one [`shape_hash`] took of shared nodes that read a cell,
/// by identity, those taken inside a function type apart.
struct ShapeMemo {
    outside: LPooled<AddrMap<u64>>,
    in_fn: Lazy<AddrMap<u64>>,
}

impl ShapeMemo {
    fn get(&self, in_fn: bool, k: usize) -> Option<u64> {
        match in_fn {
            false => self.outside.get(&k).copied(),
            true => self.in_fn.get().and_then(|m| m.get(&k)).copied(),
        }
    }

    fn insert(&mut self, in_fn: bool, k: usize, h: u64) {
        match in_fn {
            false => self.outside.insert(k, h),
            true => self.in_fn.get_mut().insert(k, h),
        };
    }
}

/// [`shape_hash`], and whether it read no cell: each shared node hashed
/// once (`memo`, by identity), and one that read no cell once for all
/// (`fixed`). `in_fn`: `t` is inside a function type.
fn shape_hash_memo(
    t: &Type,
    in_fn: bool,
    memo: &mut ShapeMemo,
    fixed: &mut FixedHashes,
) -> (u64, bool) {
    let key = t.shared_identity();
    if let Some(k) = key {
        if let Some((_, h)) = fixed.get(&k) {
            return (*h, true);
        }
        if let Some(h) = memo.get(in_fn, k) {
            return (h, false);
        }
    }
    let (h, cell_free) = shape_hash_node(t, in_fn, memo, fixed);
    match key {
        Some(k) if cell_free => {
            fixed.insert(k, (t.clone(), h));
        }
        Some(k) => memo.insert(in_fn, k, h),
        None => (),
    }
    (h, cell_free)
}

fn shape_hash_node(
    t: &Type,
    in_fn: bool,
    memo: &mut ShapeMemo,
    fixed: &mut FixedHashes,
) -> (u64, bool) {
    #[cfg(test)]
    SHAPE_HASH_STEPS.with(|n| n.set(n.get() + 1));
    ensure_sufficient(|| {
        let mut h = ahash::AHasher::default();
        let mut cell_free = true;
        let mut part = |t: &Type, in_fn: bool, memo: &mut ShapeMemo| {
            let (h, free) = shape_hash_memo(t, in_fn, memo, fixed);
            cell_free &= free;
            h
        };
        match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => return (part(&b, in_fn, memo), false),
                None if in_fn => return (0, false),
                None => {
                    (0u8, tv.cell_addr()).hash(&mut h);
                    return (h.finish(), false);
                }
            },
            Type::Set(ts) => {
                let members =
                    ts.iter().fold(0u64, |a, t| a.wrapping_add(part(t, in_fn, memo)));
                (1u8, members).hash(&mut h)
            }
            Type::Fn(f) => {
                (2u8, f.args.len(), f.vargs.is_some(), f.quantifiers.len()).hash(&mut h);
                for a in f.args.iter() {
                    part(&a.typ, true, memo).hash(&mut h)
                }
                if let Some(v) = &f.vargs {
                    part(v, true, memo).hash(&mut h)
                }
                (part(&f.rtype, true, memo), part(&f.throws, true, memo)).hash(&mut h)
            }
            Type::Primitive(p) => (3u8, p.bits()).hash(&mut h),
            Type::Ref(tr) => (4u8, &tr.scope, &tr.name).hash(&mut h),
            Type::Variant(tag, _, _) => (5u8, tag).hash(&mut h),
            Type::Struct(fs) => {
                6u8.hash(&mut h);
                fs.iter().for_each(|(n, _, _)| n.hash(&mut h))
            }
            Type::Abstract { id, .. } => (7u8, id).hash(&mut h),
            Type::ByRef(m, _) => (8u8, m.tag()).hash(&mut h),
            t => (9u8, mem::discriminant(t)).hash(&mut h),
        }
        if !matches!(t, Type::Set(_) | Type::Fn(_)) {
            t.for_each_child(&mut |c| part(c, in_fn, memo).hash(&mut h));
        }
        (h.finish(), cell_free)
    })
}

/// How [`Type::scope_refs`] copies the variables of a written type: each
/// cell once, so what was one variable stays one, or each name once, so
/// same-named variables are one.
pub(super) enum Copies<'a> {
    Cells(&'a mut AHashMap<usize, TVar>),
    /// The variable of each name, and the cells of the written type met
    /// so far.
    Names(&'a mut AHashMap<ArcStr, TVar>, LPooled<AHashSet<usize>>),
}

impl Copies<'_> {
    fn get(&self, tv: &TVar) -> Option<TVar> {
        match self {
            Copies::Cells(m) => m.get(&tv.cell_addr()).cloned(),
            Copies::Names(m, _) => m.get(&tv.name).cloned(),
        }
    }

    fn insert(&mut self, tv: &TVar, copy: TVar) {
        match self {
            Copies::Cells(m) => {
                m.insert(tv.cell_addr(), copy);
            }
            Copies::Names(m, _) => {
                m.insert(tv.name.clone(), copy);
            }
        }
    }

    /// `f` with the names `quantifiers` declares its own: none of them is
    /// a variable from outside while it runs.
    pub(super) fn within<T>(
        &mut self,
        quantifiers: &[ArcStr],
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let Copies::Names(m, _) = self else { return f(self) };
        let outer: SmallVec<[(ArcStr, Option<TVar>); 4]> =
            quantifiers.iter().map(|q| (q.clone(), m.remove(q))).collect();
        let r = f(self);
        let Copies::Names(m, _) = self else { unreachable!() };
        for (q, tv) in outer {
            m.remove(&q);
            if let Some(tv) = tv {
                m.insert(q, tv);
            }
        }
        r
    }
}

#[cfg(test)]
thread_local! {
    /// The nodes [`shape_hash`] has hashed on this thread.
    pub(super) static SHAPE_HASH_STEPS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
    /// The nodes [`Type::param_key`] has keyed on this thread.
    pub(super) static KEY_STEPS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
}

impl<H: IsoPoolable> Deref for RefHist<H> {
    type Target = H;

    fn deref(&self) -> &H {
        self.inner.as_deref().unwrap_or_else(|| self.empty.get_or_init(H::empty))
    }
}

impl<H: IsoPoolable> DerefMut for RefHist<H> {
    fn deref_mut(&mut self) -> &mut H {
        self.inner.get_or_insert_with(LPooled::take)
    }
}

impl<H: IsoPoolable> RefHist<H> {
    fn new() -> Self {
        RefHist {
            inner: None,
            empty: OnceCell::new(),
            ref_defs: Lazy::new(),
            ref_nodes: Lazy::new(),
            ref_apps: Lazy::new(),
            content_ids: Lazy::new(),
            shape_ids: Lazy::new(),
            fixed_hashes: Lazy::new(),
            next_id: 0,
        }
    }

    fn next(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// A stable id for a type: a Ref keys on its definition and params
    /// (the cell is filled first, so one ref has one id in a walk); a
    /// bound cell is its binding; an open cell keys on the cell, a
    /// primitive on its bits, anything else (a constructor application
    /// and a hole included) on its content. `None` only for an
    /// unresolvable name, which expands to nothing, so no walk recurses
    /// under its key.
    fn ref_id(&mut self, t: &Type, env: &Env) -> Option<usize> {
        let d = mem::discriminant(t);
        let k = match t {
            Type::Ref(tr) => {
                let r = tr.resolve_in(env)?;
                let key = r.def_key();
                let node = (key, (*tr.params).as_ptr().addr());
                if let Some((_, id)) = self.ref_nodes.get().and_then(|m| m.get(&node)) {
                    return Some(*id);
                }
                self.ref_defs.get_mut().entry(key).or_insert(r);
                let params = tr.params.iter().map(|p| self.param_id(p, env)).collect();
                let id = match self.ref_apps.get_mut().entry((key, params)) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let id = self.next_id;
                        self.next_id += 1;
                        *e.insert(id)
                    }
                };
                self.ref_nodes.get_mut().insert(node, (tr.params.clone(), id));
                return Some(id);
            }
            Type::TVar(tv) => match tv.binding() {
                Some(b) => return self.ref_id(&b, env),
                None => (d, tv.cell_addr(), 0),
            },
            Type::Primitive(p) => (d, p.bits() as usize, 0),
            Type::Abstract { id, params } => {
                (d, (**params).as_ptr().addr(), id.0 as usize)
            }
            Type::Any | Type::Bottom => (d, 0, 0),
            t => return Some(self.shape_id(t, env)),
        };
        if let Some((id, _)) = self.content_ids.get().and_then(|m| m.get(&k)) {
            return Some(*id);
        }
        let id = self.next();
        self.content_ids.get_mut().insert(k, (id, Held::of(t)));
        Some(id)
    }

    /// The id of a ref's parameter, equal for parameters `union_identical`
    /// holds identical: [`Self::ref_id`]'s, but an abstract type and an
    /// unresolvable name by their structure.
    fn param_id(&mut self, t: &Type, env: &Env) -> usize {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if let Some(b) = tv.binding() => self.param_id(&b, env),
            Type::Abstract { .. } => self.shape_id(t, env),
            t => self.ref_id(t, env).unwrap_or_else(|| self.shape_id(t, env)),
        })
    }

    /// The id of `t` by its structure, open cells by their cell, names by
    /// the definition they mean in `env`.
    fn shape_id(&mut self, t: &Type, env: &Env) -> usize {
        let h = shape_hash(t, self.fixed_hashes.get_mut());
        let found = self.shape_ids.get().and_then(|m| m.get(&h)).and_then(|b| {
            b.iter()
                .find(|(s, _)| setops::union_identical_in(env, s, t))
                .map(|(_, id)| *id)
        });
        if let Some(id) = found {
            return id;
        }
        let id = self.next();
        self.shape_ids.get_mut().entry(h).or_default().push((t.clone(), id));
        id
    }
}

/// Whether a typedef's body returns to the definition with no
/// constructor between ([`Type::reaches_unguarded`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unguarded {
    Reaches,
    Guarded,
    /// A name on the way is not defined yet.
    Unknown,
}

impl Unguarded {
    pub(crate) fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::Reaches, _) | (_, Self::Reaches) => Self::Reaches,
            (Self::Unknown, _) | (_, Self::Unknown) => Self::Unknown,
            _ => Self::Guarded,
        }
    }
}

/// The identity of an abstract type: the low 64 bits of
/// [`abstract_uuid`] of its canonical path. Its `Pack` impl is
/// `uuid_id_codec!`'s.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct AbstractId(u64);

impl nohash::IsEnabled for AbstractId {}

/// The refusal of a function type with quantifiers of its own anywhere
/// but a declared signature's top.
pub const QUANTIFIED_FN: &str = "a function type with type variables of its own \
     (`fn<'a: ..>(..)`, or a trait as a parameter's type, `fn(c: Collection)`) \
     stands only as a definition's signature: declare the variables on the \
     definition (`'a: Number |f: fn(x: 'a) -> 'a| ..`), or bind the generic \
     function with `let` and call it by name";

/// The v5 UUID namespace abstract type identities are derived from,
/// so the compile-time [`AbstractId`] and a value's runtime tag agree
/// across processes and builds.
const ABSTRACT_NAMESPACE: uuid::Uuid = uuid::Uuid::from_bytes([
    0x1f, 0x64, 0x9a, 0x2e, 0x7b, 0xd5, 0x4c, 0x8a, 0x9f, 0x3e, 0x21, 0xb7, 0x5c, 0x0d,
    0xe6, 0x42,
]);

/// The UUID of the abstract type at `path` (`package::module::Name`).
/// Rust-backed abstract types register their wrapper under it.
pub fn abstract_uuid(path: &str) -> uuid::Uuid {
    uuid::Uuid::new_v5(&ABSTRACT_NAMESPACE, path.as_bytes())
}

/// Where a declaration is written, the same in every process: its source
/// and position, and a source named by its text alone (a REPL input, an
/// embedded program) by the text too.
// CR claude for eric: [bug/medium] For an Unspecified or Program origin the place is
// the text's hash and the position, not the origin's `load`. So a type a block or body
// declares in a REPL input has the same `AbstractId` as one an earlier input with the
// same text declared. Entering one line twice with `impl Eq for C` in a block is
// refused, `conflicting implementation: Eq is already implemented for C`, while the
// first input lives: the shell keeps the previous output through the next compile and a
// `let` input for the session. `GXHandle::compile` of one text twice is refused the
// same way. The origin's scope tag already includes its load (`Origin::loaded`). Adding
// `ori.load` to the place here would give each input its own types, as it has its own
// scopes. probe: design/review-2026-11-16/repro/x-leaks-repl-same-line-conflict.sh
// (x-leaks-02)
pub fn declaration_place(ori: &Origin, pos: SourcePosition) -> CompactString {
    match &ori.source {
        Source::File(_) | Source::Vfs(_) | Source::Netidx(_) => {
            format_compact!("{}:{pos}", ori.source)
        }
        Source::Program | Source::Unspecified => {
            let (_, text) = abstract_uuid(&ori.text).as_u64_pair();
            format_compact!("{}:{text:x}:{pos}", ori.source)
        }
    }
}

/// Who names an abstract type in [`ABSTRACT_NAMES`].
enum NamedBy {
    /// A declaration: the name stays.
    Declaration,
    /// So many live loads of a dynamic module, each its own type's
    /// ([`AbstractId::in_load`]): the name goes with the last.
    Loads(u32),
}

/// The names of the abstract types minted in this process, for
/// diagnostics (`Type::Abstract` carries only the id).
static ABSTRACT_NAMES: LazyLock<Mutex<IntMap<AbstractId, (ArcStr, NamedBy)>>> =
    LazyLock::new(|| Mutex::new(IntMap::default()));

impl AbstractId {
    /// The identity of the abstract type `name` defined in `scope`:
    /// the low 64 bits of [`abstract_uuid`] of its canonical path.
    pub fn of(scope: &ModPath, name: &str) -> Self {
        let path = format_compact!("{scope}::{name}");
        let (_, lo) = abstract_uuid(&path).as_u64_pair();
        AbstractId(lo).named(name)
    }

    /// The identity of the abstract type `name` declared at `pos` in
    /// `ori`, in `scope`. A component a function body or block opens
    /// (`#fn..`, `#do..`) takes no part: a module's type is its path
    /// without them (an interface's declaration and its implementation's
    /// are one type),
    /// and a type declared in a body is its place there, so every
    /// instance of the enclosing function, cold or warm, declares one. A
    /// dynamic module whose path crosses a body is told from a same-named
    /// one in another body by `site`, the place its `mod` statement is
    /// written ([`declaration_place`]); a file module is its file.
    pub fn declared(
        scope: &ModPath,
        name: &str,
        pos: SourcePosition,
        ori: &Origin,
        site: Option<&str>,
    ) -> Self {
        // CR claude for claude: [structure/low] These lines are `env::crosses_body`
        // (any component generated), `env::is_block_scope` (the last one is) and the
        // block-component filter of `crate::written_scope`, written again with
        // `starts_with('#')`; `Env::own_types` and `UnresolvableRef`'s Display do the
        // same. So what makes a scope component generated is decided by
        // `is_block_component` and by five inline copies. Two of those copies key an
        // abstract type's identity and a load's types across processes and images. Fix:
        // call `crosses_body` (made pub(crate)), `is_block_scope` and
        // `is_block_component` instead of the inline tests. Found by reading; not run.
        // (x-dup-07)
        let parts = || Path::parts(&scope.0);
        if !parts().any(|p| p.starts_with('#')) {
            return Self::of(scope, name);
        }
        let stable: ModPath = parts().filter(|p| !p.starts_with('#')).collect();
        let in_body = parts().last().is_some_and(|p| p.starts_with('#'));
        let place = match (in_body, site) {
            (true, _) => {
                format_compact!("{stable}::#{}::{name}", declaration_place(ori, pos))
            }
            (false, Some(site)) => format_compact!("{stable}::#{site}::{name}"),
            (false, None) => return Self::of(&stable, name),
        };
        let (_, lo) = abstract_uuid(&place).as_u64_pair();
        AbstractId(lo).named(name)
    }

    /// This id, of a type `name` that the load `key` of a dynamic
    /// module's source declares and its interface does not: each load's
    /// type is its own ([`crate::env::LoadedBody`]).
    // CR claude for eric: [bug/medium] A load's own type takes an id hashed from its
    // base id and the load's key, and values of different abstract types order by id,
    // so the order among the types one source declares changes with the load's number:
    // the same text, reloaded unchanged, sorts `[C(1), D(1), E(1), F(1)]` a different
    // way at most loads, and each slot of a module in a collection its own way. Order
    // the own types of one load by their base ids (carry the base id beside the load in
    // what the value's ordering reads). probe:
    // design/review-2026-11-16/repro/x-dyn-modules-own-type-order.gx (x-dyn-modules-07)
    pub(crate) fn in_load(self, key: &str) -> Self {
        let (_, lo) = abstract_uuid(&format_compact!("{:x}@{key}", self.0)).as_u64_pair();
        AbstractId(lo)
    }

    /// This id, its declared name recorded for diagnostics: by a mint,
    /// and by a restored definition, which mints nothing.
    pub(crate) fn named(self, name: &str) -> Self {
        match ABSTRACT_NAMES.lock().entry(self) {
            Entry::Occupied(mut e) => e.get_mut().1 = NamedBy::Declaration,
            Entry::Vacant(e) => {
                e.insert((ArcStr::from(name), NamedBy::Declaration));
            }
        }
        self
    }

    /// Record `name` for this id, a load's own type, while the load
    /// lives ([`Self::load_gone`]).
    pub(crate) fn named_by_load(self, name: &str) {
        match ABSTRACT_NAMES.lock().entry(self) {
            Entry::Occupied(mut e) => {
                if let NamedBy::Loads(n) = &mut e.get_mut().1 {
                    *n += 1
                }
            }
            Entry::Vacant(e) => {
                e.insert((ArcStr::from(name), NamedBy::Loads(1)));
            }
        }
    }

    /// A load that named this id ([`Self::named_by_load`]) is gone.
    pub(crate) fn load_gone(self) {
        if let Entry::Occupied(mut e) = ABSTRACT_NAMES.lock().entry(self)
            && let NamedBy::Loads(n) = &mut e.get_mut().1
        {
            *n -= 1;
            if *n == 0 {
                e.remove();
            }
        }
    }

    /// The core type `Transparent<'a>`, which JSON and TOML write bare.
    pub fn transparent() -> Self {
        static ID: LazyLock<AbstractId> =
            LazyLock::new(|| AbstractId::of(&ModPath::from(["core"]), "Transparent"));
        *ID
    }

    /// The type's declared name, if this process has minted the id.
    pub fn name(&self) -> Option<ArcStr> {
        ABSTRACT_NAMES.lock().get(self).map(|(name, _)| name.clone())
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        AbstractId(i)
    }
}

/// The identity of a trait: the low 64 bits of a v5 UUID of its
/// canonical path, so an interface's declaration and the
/// implementation's re-declaration name one trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitId(u64);

const TRAIT_NAMESPACE: uuid::Uuid = uuid::Uuid::from_bytes([
    0x8c, 0x2b, 0x41, 0x9d, 0xe3, 0x07, 0x4f, 0x6a, 0xb1, 0x5d, 0x77, 0x0a, 0x2e, 0x93,
    0xc8, 0x15,
]);

impl TraitId {
    pub fn of(scope: &ModPath, name: &str) -> Self {
        let path = format_compact!("{scope}::{name}");
        let (_, lo) = uuid::Uuid::new_v5(&TRAIT_NAMESPACE, path.as_bytes()).as_u64_pair();
        TraitId(lo)
    }

    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn from_inner(i: u64) -> Self {
        TraitId(i)
    }
}

impl nohash::IsEnabled for TraitId {}

/// The core traits, whose implementations the runtime consults on its
/// own: `Eq`, `Ord` and `Display` ride the value, `Cast` carries an
/// abstract type across data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreTrait {
    Eq,
    Ord,
    Display,
    Cast,
}

static CORE_IDS: LazyLock<[TraitId; 4]> = LazyLock::new(|| {
    let core = ModPath::from(["core"]);
    [
        TraitId::of(&core, "Eq"),
        TraitId::of(&core, "Ord"),
        TraitId::of(&core, "Display"),
        TraitId::of(&core, "Cast"),
    ]
});

impl CoreTrait {
    pub fn id(self) -> TraitId {
        CORE_IDS[self as usize]
    }

    pub fn of_id(id: TraitId) -> Option<Self> {
        [Self::Eq, Self::Ord, Self::Display, Self::Cast]
            .into_iter()
            .find(|t| t.id() == id)
    }

    /// Whether every type has the trait by its structure, and its
    /// methods are the operators and printers: `Eq`, `Ord`, `Display`.
    pub fn structural(self) -> bool {
        self != Self::Cast
    }

    /// Whether a method an implementation leaves out is the type's
    /// representation itself: `Cast`'s.
    pub fn representation_default(id: TraitId) -> bool {
        Self::of_id(id) == Some(Self::Cast)
    }

    /// Whether the runtime calls the method `name` and a program never
    /// does: every `Cast` method, and `Eq`'s `hash`. An implementation
    /// may leave one out.
    pub fn runtime_method(id: TraitId, name: &str) -> bool {
        match Self::of_id(id) {
            Some(Self::Cast) => true,
            Some(Self::Eq) => name == "hash",
            _ => false,
        }
    }
}

/// What a `TypeRef`'s name means: the definition [`Type::lookup_ref`]
/// reads, held by its `TypeDef` and weakly by the ref's write-once
/// `resolved` cell, so a ref resolved once in its native env is
/// env-independent after. A recursive definition's body reaches its own
/// cell, so a strong cell would be a cycle.
#[derive(Debug)]
#[doc(hidden)]
pub struct ResolvedRef {
    canonical_scope: ModPath,
    pos: SourcePosition,
    ori: Arc<Origin>,
    params: Arc<[(TVar, Option<Type>)]>,
    typ: Type,
    /// [`expansive::expansive`] found it regular with every name it
    /// reached defined, so no flow through it comes back to a definition
    /// made later.
    pub(crate) regular: OnceLock<()>,
    /// How the body meets each parameter ([`variance::meets`]), once
    /// every name it reaches is defined.
    pub(crate) meets: OnceLock<Arc<[variance::Meets]>>,
}

impl ResolvedRef {
    /// Whether parameter `i` stands once in the body, under nothing but
    /// unions, tuples, structs, variants and errors: then the union of
    /// two applications differing only there is exactly the application
    /// at the union of the two.
    pub(crate) fn param_is_linear(&self, i: usize) -> bool {
        fn go(t: &Type, name: &ArcStr, under: bool, n: &mut usize) {
            match t {
                Type::TVar(tv) if &tv.name == name => *n += if under { 2 } else { 1 },
                Type::Set(_)
                | Type::Tuple(_)
                | Type::Struct(_)
                | Type::Variant(..)
                | Type::Error(_) => {
                    ensure_sufficient(|| t.for_each_child(&mut |c| go(c, name, under, n)))
                }
                t => {
                    ensure_sufficient(|| t.for_each_child(&mut |c| go(c, name, true, n)))
                }
            }
        }
        let Some((formal, _)) = self.params.get(i) else { return false };
        let mut n = 0;
        go(&self.typ, &formal.name, false, &mut n);
        n == 1
    }

    /// Whether the body holds parameter `i` under a constructor, through
    /// the filled cells of the typedefs it names.
    pub(crate) fn param_guarded(&self, i: usize) -> bool {
        self.guarded_memo(i, &mut LPooled::take())
    }

    /// `memo` holds each (definition, parameter) asked and its answer,
    /// false while it is asked: a path back to it adds nothing a shorter
    /// path does not.
    fn guarded_memo(&self, i: usize, memo: &mut AHashMap<(usize, usize), bool>) -> bool {
        fn go(
            t: &Type,
            name: &ArcStr,
            under: bool,
            memo: &mut AHashMap<(usize, usize), bool>,
        ) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => under && &tv.name == name,
                Type::Set(ms) => ms.iter().any(|m| go(m, name, under, memo)),
                Type::Ref(r) => {
                    let def = r.resolved();
                    r.params.iter().enumerate().any(|(j, p)| {
                        let guarded = under
                            || def.as_ref().is_some_and(|d| d.guarded_memo(j, memo));
                        go(p, name, guarded, memo)
                    })
                }
                t => t
                    .try_for_each_child(&mut |c| match go(c, name, true, memo) {
                        true => ControlFlow::Break(()),
                        false => ControlFlow::Continue(()),
                    })
                    .is_break(),
            })
        }
        let key = (self.def_key(), i);
        if let Some(guarded) = memo.get(&key) {
            return *guarded;
        }
        let Some((formal, _)) = self.params.get(i) else { return false };
        memo.insert(key, false);
        let guarded = go(&self.typ, &formal.name, false, memo);
        memo.insert(key, guarded);
        guarded
    }

    pub(crate) fn new(
        canonical_scope: ModPath,
        pos: SourcePosition,
        ori: Arc<Origin>,
        params: Arc<[(TVar, Option<Type>)]>,
        typ: Type,
    ) -> Self {
        Self {
            canonical_scope,
            pos,
            ori,
            params,
            typ,
            regular: OnceLock::new(),
            meets: OnceLock::new(),
        }
    }

    pub(crate) fn pos(&self) -> SourcePosition {
        self.pos
    }

    pub(crate) fn ori(&self) -> &Arc<Origin> {
        &self.ori
    }

    pub(crate) fn params(&self) -> &Arc<[(TVar, Option<Type>)]> {
        &self.params
    }

    pub fn typ(&self) -> &Type {
        &self.typ
    }

    /// Refuse `args` written for `name` unless they are one per parameter.
    fn check_arity(&self, name: &ModPath, args: &[Type]) -> Result<()> {
        if self.params.len() != args.len() {
            bail!("{} expects {} type parameters", name, self.params.len());
        }
        Ok(())
    }

    /// The definition's parameters by name, bound to `args`; `None` when
    /// the arity differs.
    pub(crate) fn bindings(
        &self,
        args: &[Type],
    ) -> Option<LPooled<AHashMap<ArcStr, Type>>> {
        if self.params.len() != args.len() {
            return None;
        }
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        for ((tv, _), arg) in self.params.iter().zip(args.iter()) {
            known.insert(tv.name.clone(), arg.clone());
        }
        Some(known)
    }

    /// The definition's identity: every cell filled from one `TypeDef`
    /// shares its params allocation.
    pub(crate) fn def_key(&self) -> usize {
        Arc::as_ptr(&self.params) as *const () as usize
    }

    #[doc(hidden)]
    pub fn canonical_scope(&self) -> &ModPath {
        &self.canonical_scope
    }
}

/// What a type reference's name resolved to, written once: a typedef
/// (held weakly; its `TypeDef` owns it) or a trait, which a bound names.
#[doc(hidden)]
#[derive(Debug, Clone, Default)]
pub enum Resolution {
    #[default]
    Open,
    Def(Weak<ResolvedRef>),
    Trait(TraitId),
}

/// A reference to a named typedef, e.g. `Foo` or `Result<i64, string>`.
/// `pos`/`ori` are IDE metadata and `resolved` is the write-once name
/// resolution cell ([`ResolvedRef`]); neither is part of type identity
/// or of the syntax codec's packed form, while an image session writes
/// all three (and keys them), so a restored ref keeps its resolution and
/// never resolves again. The cell depends on (scope, name, env) but not
/// `params`: [`TypeRef::with_params`] shares it, [`TypeRef::with_scope`]
/// mints a new one. Never overwrite a filled cell — clones share it.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub scope: ModPath,
    pub name: ModPath,
    pub params: Arc<[Type]>,
    pub pos: Option<SourcePosition>,
    pub ori: Option<Arc<Origin>>,
    pub(in crate::typ) resolved: Arc<Mutex<Resolution>>,
}

/// A type reference as a path walk keys it: its definition and the
/// parameters it is applied to, so `Id<i64>` and `Id<&i64>` are two.
pub(crate) type RefKey = (usize, Arc<[Type]>);

pub(crate) fn resolved_encode(
    r: &ResolvedRef,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let ResolvedRef { canonical_scope, pos, ori, params, typ, regular: _, meets: _ } = r;
    canonical_scope.encode(buf)?;
    image::pos_encode(pos, buf)?;
    image::origin_encode(ori, buf)?;
    params.encode(buf)?;
    typ.encode(buf)
}

pub(crate) fn resolved_decode(buf: &mut impl Buf) -> Result<ResolvedRef, PackError> {
    Ok(ResolvedRef {
        canonical_scope: PackTrait::decode(buf)?,
        pos: image::pos_decode(buf)?,
        ori: image::origin_decode(buf)?,
        params: PackTrait::decode(buf)?,
        typ: PackTrait::decode(buf)?,
        regular: OnceLock::new(),
        meets: OnceLock::new(),
    })
}

/// The syntax codec writes the name and parameters and mints a fresh
/// cell; under an image the position, origin and the shared resolution
/// cell travel too, so a restored session never re-resolves.
impl PackTrait for TypeRef {
    fn encoded_len(&self) -> usize {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        let base = scope.encoded_len() + name.encoded_len() + params.encoded_len();
        if !image::is_encoding() {
            return base;
        }
        let pos = 1 + pos.as_ref().map_or(0, image::pos_len);
        let ori = 1 + ori.as_ref().map_or(0, image::origin_len);
        base + pos + ori + image::refcell_len(resolved)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        scope.encode(buf)?;
        name.encode(buf)?;
        params.encode(buf)?;
        if !image::is_encoding() {
            return Ok(());
        }
        match pos {
            None => buf.put_u8(0),
            Some(p) => {
                buf.put_u8(1);
                image::pos_encode(p, buf)?;
            }
        }
        match ori {
            None => buf.put_u8(0),
            Some(o) => {
                buf.put_u8(1);
                image::origin_encode(o, buf)?;
            }
        }
        image::refcell_encode(resolved, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let scope = PackTrait::decode(buf)?;
        let name = PackTrait::decode(buf)?;
        let params = PackTrait::decode(buf)?;
        if !image::is_decoding() {
            return Ok(TypeRef::new(scope, name, params, None, None));
        }
        let pos = match u8::decode(buf)? {
            0 => None,
            1 => Some(image::pos_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let ori = match u8::decode(buf)? {
            0 => None,
            1 => Some(image::origin_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let resolved = image::refcell_decode(buf)?;
        Ok(TypeRef { scope, name, params, pos, ori, resolved })
    }
}

impl TypeRef {
    pub fn new(
        scope: ModPath,
        name: ModPath,
        params: Arc<[Type]>,
        pos: Option<SourcePosition>,
        ori: Option<Arc<Origin>>,
    ) -> Self {
        Self { scope, name, params, pos, ori, resolved: Arc::default() }
    }

    /// A `TypeRef` with no source-position info.
    pub fn synthetic(scope: ModPath, name: ModPath, params: Arc<[Type]>) -> Self {
        Self::new(scope, name, params, None, None)
    }

    /// This ref with different `params`, sharing the resolution cell.
    #[doc(hidden)]
    pub fn with_params(&self, params: Arc<[Type]>) -> Self {
        Self { params, ..self.clone() }
    }

    /// This ref re-scoped, with a new resolution cell pre-filled from
    /// this ref's cell when that is resolved (a filled cell is the
    /// name's final target; a new scope may not even reach it). An
    /// unfilled cell stays fresh.
    pub(crate) fn with_scope(&self, scope: ModPath, params: Arc<[Type]>) -> Self {
        Self {
            scope,
            name: self.name.clone(),
            params,
            pos: self.pos,
            ori: self.ori.clone(),
            resolved: Arc::new(Mutex::new(self.resolved.lock().clone())),
        }
    }

    /// Expand this ref through its filled cell, env-free, substituting
    /// params as `lookup_ref` would. `None` when the cell is empty or
    /// the arity mismatches. No constraint checks.
    pub fn expand_cell(&self) -> Option<Type> {
        let r = self.resolved()?;
        Some(r.typ.replace_tvars(&*r.bindings(&self.params)?))
    }

    /// The definition the cell holds; `None` when it is empty, or when
    /// the definition is gone (see [`Self::resolve_in`]).
    pub(crate) fn resolved(&self) -> Option<sync::Arc<ResolvedRef>> {
        match &*self.resolved.lock() {
            Resolution::Def(w) => w.upgrade(),
            Resolution::Open | Resolution::Trait(_) => None,
        }
    }

    /// [`Self::resolved`] of a cell that is not open; `None` while it is.
    fn held(&self) -> Option<Option<sync::Arc<ResolvedRef>>> {
        match &*self.resolved.lock() {
            Resolution::Open => None,
            Resolution::Def(w) => Some(w.upgrade()),
            Resolution::Trait(_) => Some(None),
        }
    }

    /// The trait the cell holds: a bound resolved once, where its name
    /// was visible.
    pub(crate) fn trait_id(&self) -> Option<TraitId> {
        match &*self.resolved.lock() {
            Resolution::Trait(id) => Some(*id),
            Resolution::Open | Resolution::Def(_) => None,
        }
    }

    /// Fill an open cell with the trait the name means in `env`; the
    /// trait the cell holds, if any.
    pub(crate) fn resolve_trait_in(&self, env: &Env) -> Option<TraitId> {
        if let Some(id) = self.trait_id() {
            return Some(id);
        }
        let Ok(Some(crate::env::TypeName::Trait(id))) =
            env.resolve_type_name(&self.scope, &self.name)
        else {
            return None;
        };
        let mut cell = self.resolved.lock();
        match &*cell {
            Resolution::Open => {
                *cell = Resolution::Trait(id);
                Some(id)
            }
            Resolution::Trait(id) => Some(*id),
            Resolution::Def(_) => None,
        }
    }

    /// Identity of the resolution cell: shared by the rebuilds of one ref.
    #[doc(hidden)]
    pub fn cell_addr(&self) -> usize {
        Arc::as_ptr(&self.resolved) as *const () as usize
    }

    /// [`ResolvedRef::def_key`] of the filled cell.
    #[doc(hidden)]
    pub fn def_key(&self) -> Option<usize> {
        self.resolved().map(|r| r.def_key())
    }

    /// Whether two references name one definition: an open cell names
    /// what its name means in `env`, so the answer does not depend on
    /// which cells a walk filled; two that name nothing, by the written
    /// scope and name. Fills no cell.
    pub(crate) fn same_definition(&self, other: &Self, env: &Env) -> bool {
        let same_name = || self.scope == other.scope && self.name == other.name;
        if self.is_open() && other.is_open() && same_name() {
            return true;
        }
        match (self.resolve_peek(env), other.resolve_peek(env)) {
            (Some(a), Some(b)) => a.def_key() == b.def_key(),
            (None, None) => same_name(),
            _ => false,
        }
    }

    /// [`Self::same_definition`] by what the cells hold, for a walk with
    /// no env: a filled cell and an open one are two definitions.
    pub(crate) fn same_filled_definition(&self, other: &Self) -> bool {
        self.same_held(self.resolved(), other, other.resolved())
    }

    /// [`Self::same_filled_definition`] where neither cell is open; `None`
    /// where one is, whose fill may change the answer.
    pub(crate) fn same_settled_definition(&self, other: &Self) -> Option<bool> {
        Some(self.same_held(self.held()?, other, other.held()?))
    }

    fn same_held(
        &self,
        def: Option<sync::Arc<ResolvedRef>>,
        other: &Self,
        other_def: Option<sync::Arc<ResolvedRef>>,
    ) -> bool {
        match (def, other_def) {
            (Some(a), Some(b)) => a.def_key() == b.def_key(),
            (None, None) => self.scope == other.scope && self.name == other.name,
            _ => false,
        }
    }

    /// Hold the arguments to `def`'s parameter bounds, as an expansion
    /// does: an open argument cell takes its bound when `commit`, any
    /// other argument must meet it, refused by error when `commit`, else
    /// `false`.
    fn hold_bounds_of(&self, env: &Env, def: &ResolvedRef, commit: bool) -> Result<bool> {
        if def.params.iter().all(|(_, b)| b.is_none()) {
            return Ok(true);
        }
        let Some(known) = def.bindings(&self.params) else { return Ok(true) };
        for ((_, bound), arg) in def.params.iter().zip(self.params.iter()) {
            let Some(bound) = bound else { continue };
            let bound = bound.replace_tvars(&known);
            match arg.open_cell(env) {
                Some(tv) => {
                    if commit {
                        tv.add_body_bound(env, bound)?
                    }
                }
                None if commit => {
                    for tv in bound.open_members(env, arg) {
                        tv.add_body_bound(env, bound.clone())?
                    }
                    bound.check_contains(env, arg)?
                }
                None => {
                    if !bound.decided_holds(env, arg)? {
                        return Ok(false);
                    }
                }
            }
        }
        Ok(true)
    }

    /// Hold this reference and `twin`, which name one definition, to its
    /// parameter bounds ([`Self::hold_bounds_of`]): what a walk that takes
    /// two applications of one definition by their arguments owes each, as
    /// their expansions would. An open cell takes its twin's definition,
    /// as an expansion would have filled it. Two open cells stay open: the
    /// walk may run under an env that does not see their scope, where the
    /// name means an outer shadow.
    pub(crate) fn hold_bounds_with(
        &self,
        twin: &Self,
        env: &Env,
        commit: bool,
    ) -> Result<bool> {
        let def = match (self.resolved(), twin.resolved()) {
            (Some(d), None) => {
                twin.fill(&d);
                Some(d)
            }
            (None, Some(d)) => {
                self.fill(&d);
                Some(d)
            }
            (Some(d), Some(_)) => Some(d),
            (None, None) => self.resolve_peek(env),
        };
        match def {
            Some(def) => Ok(self.hold_bounds_of(env, &def, commit)?
                && twin.hold_bounds_of(env, &def, commit)?),
            None => Ok(true),
        }
    }

    /// Fill an open cell with `def`.
    fn fill(&self, def: &sync::Arc<ResolvedRef>) {
        let mut cell = self.resolved.lock();
        if let Resolution::Open = &*cell {
            *cell = Resolution::Def(sync::Arc::downgrade(def))
        }
    }

    fn is_open(&self) -> bool {
        matches!(&*self.resolved.lock(), Resolution::Open)
    }

    /// Does the name mean something: its filled cell, a typedef visible
    /// in `env`, or a trait (a bound)? Never writes the cell: a fill
    /// before every name is registered can capture a shadowed target.
    #[doc(hidden)]
    pub fn names_something(&self, env: &Env) -> bool {
        self.means(env).is_some()
    }

    /// What the name means in `env`, as [`Self::names_something`] reads it.
    #[doc(hidden)]
    pub fn means(&self, env: &Env) -> Option<NameMeans> {
        if self.resolved().is_some() || self.resolve_pure(env).is_some() {
            Some(NameMeans::Type)
        } else if env.trait_of_ref(self).is_some() {
            Some(NameMeans::Trait)
        } else {
            None
        }
    }

    /// Refuse this application of a name that resolves when it gives the
    /// definition the wrong number of parameters, or a parameter holding
    /// no open cell that the parameter's bound refuses. A parameter that
    /// is a bare open variable takes the parameter's bound, as an
    /// expansion gives it ([`Self::hold_bounds_of`]); no other cell is
    /// written.
    pub fn check_application(&self, env: &Env) -> Result<()> {
        let Some(def) = self.resolve_peek(env) else { return Ok(()) };
        def.check_arity(&self.name, &self.params)?;
        let known = def.bindings(&self.params).expect("arity checked");
        for ((_, bound), arg) in def.params.iter().zip(self.params.iter()) {
            let Some(bound) = bound else { continue };
            let bound = bound.replace_tvars(&known);
            match arg.open_cell(env) {
                Some(tv) => tv.add_cell_constraint(bound),
                None if !arg.has_unbound() && !bound.has_unbound() => {
                    bound.check_contains(env, arg)?
                }
                None => (),
            }
        }
        Ok(())
    }

    /// The filled cell's definition, else the name's in its scope now;
    /// fills nothing.
    pub(crate) fn resolve_peek(&self, env: &Env) -> Option<sync::Arc<ResolvedRef>> {
        match &*self.resolved.lock() {
            Resolution::Def(w) => w.upgrade(),
            Resolution::Trait(_) => None,
            Resolution::Open => self.resolve_pure(env),
        }
    }

    pub(crate) fn resolve_pure(&self, env: &Env) -> Option<sync::Arc<ResolvedRef>> {
        env.resolve_type_name(&self.scope, &self.name)
            .map(|hit| match hit {
                Some(crate::env::TypeName::Def(def)) => Some(def),
                Some(crate::env::TypeName::Trait(_)) | None => None,
            })
            .ok()
            .flatten()
    }

    /// The refusal of this name where a statement its block writes below
    /// it would change what it means ([`crate::env::ChangesLater`]): a
    /// `use` that imports it, or a module a `use` above it imports it
    /// from. A `use` and its module are written above the names they bring.
    pub fn imported_below(&self, env: &Env) -> Option<anyhow::Error> {
        match self.resolved() {
            Some(_) => None,
            None => {
                self.resolve_error(env).filter(|e| e.is::<crate::env::ChangesLater>())
            }
        }
    }

    /// Why the name does not resolve, when that is a mistake in the path
    /// (an ambiguous glob, `super` past the root, a missing module) and
    /// not an absent name: the error a report names in place of
    /// [`UnresolvableRef`].
    pub fn resolve_error(&self, env: &Env) -> Option<anyhow::Error> {
        env.resolve_type_name(&self.scope, &self.name).err()
    }

    /// Resolve this ref's name in `env` and fill the cell if empty;
    /// `None` iff the name is not visible and the cell is empty. An
    /// existing resolution wins. The snapshot is computed without the
    /// cell lock held (resolution can re-enter). Fills only this ref,
    /// not the snapshot's nested refs: mid-compile the env is
    /// incomplete, and a nested name can resolve to an outer shadow.
    /// A cell whose definition is gone (a type that outlived the env
    /// entry that defined it) is `None` too, and logged: the name may
    /// mean something else now, so it is never re-resolved.
    #[doc(hidden)]
    pub fn resolve_in(&self, env: &Env) -> Option<sync::Arc<ResolvedRef>> {
        let dead = || {
            log::error!(
                "type `{}` outlived its definition in `{}`",
                self.name,
                self.scope
            );
            None
        };
        match &*self.resolved.lock() {
            Resolution::Def(w) => return w.upgrade().or_else(dead),
            Resolution::Trait(_) => return None,
            Resolution::Open => (),
        }
        let r = self.resolve_pure(env)?;
        let mut cell = self.resolved.lock();
        match &*cell {
            Resolution::Def(w) => w.upgrade().or_else(dead),
            Resolution::Trait(_) => None,
            Resolution::Open => {
                *cell = Resolution::Def(sync::Arc::downgrade(&r));
                Some(r)
            }
        }
    }

    /// Refuse this name, a type or a trait, unless `env` resolves it,
    /// whatever its cell holds; an empty cell is filled from `env`.
    pub fn resolve_only_in(&self, env: &Env) -> Result<()> {
        use crate::env::TypeName;
        let Some(hit) = env.resolve_type_name(&self.scope, &self.name)? else {
            return Err(UnresolvableRef::error(self, env));
        };
        let mut cell = self.resolved.lock();
        if let Resolution::Open = &*cell {
            *cell = match hit {
                TypeName::Def(d) => Resolution::Def(sync::Arc::downgrade(&d)),
                TypeName::Trait(id) => Resolution::Trait(id),
            }
        }
        Ok(())
    }
}

impl Default for TypeRef {
    fn default() -> Self {
        Self {
            scope: ModPath::root(),
            name: ModPath::root(),
            params: Arc::from(Vec::<Type>::new()),
            pos: None,
            ori: None,
            resolved: Arc::default(),
        }
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        self.scope == other.scope
            && self.name == other.name
            && self.params == other.params
    }
}

impl Eq for TypeRef {}

impl PartialOrd for TypeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::hash::Hash for TypeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.scope.hash(state);
        self.name.hash(state);
        self.params.hash(state);
    }
}

impl Ord for TypeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.scope
            .cmp(&other.scope)
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.params.cmp(&other.params))
    }
}

/// A bound a cell's constraints may hold beside its types: what a type
/// binding the cell must be, judged as a whole rather than contained.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bound {
    /// The conjunct `'a: Concrete`: whatever binds the cell is fully known,
    /// with no open cell and no ⊥ in it, so a type-directed builtin can act
    /// on it.
    Concrete,
    /// The conjunct `'a: Data`: whatever binds the cell is data a writer
    /// can write: no reference, no function and no abstract type without
    /// a `Cast` implementation; an open cell holds.
    Data,
    /// The conjunct `'a: Function`: whatever binds the cell is a function
    /// type, so a builtin wrapping a function can read its signature.
    Function,
    /// The conjunct `'a: Singleton`: whatever binds the cell is one type,
    /// not a union of several (arithmetic is `fn<'a: Number + Singleton>`).
    Singleton,
    /// The conjunct `'a: OneNumber`: whatever binds the cell holds at
    /// most one numeric type (`[i64, null]`, not `[i64, f64, null]`).
    OneNumber,
    /// The conjunct `'a: NoError`: no value of whatever binds the cell is
    /// an error (`Result`'s success type).
    NoError,
    /// The conjunct `'a: Discernible`: no union anywhere in whatever binds
    /// the cell holds two members with one runtime form
    /// ([`Type::rep_ambiguity`]), so its values compare and hash as their
    /// types do.
    Discernible,
    /// The conjunct `'a: Ordered`: `Discernible`, and no reference where a
    /// comparison looks ([`Type::ref_in`]): a reference's value is its
    /// cell, which neither orders nor names what it points to, so values
    /// of the type order, hash and compare by value alone.
    Ordered,
    /// The conjunct `'a: JsonData`: every union in whatever binds the cell
    /// tells its members apart by a tag ([`Type::text_refusal`]), so what
    /// `json::read` reads is what was written; an open cell holds.
    JsonData,
    /// `JsonData` for TOML.
    TomlData,
}

/// Type depth is not bounded by source nesting (`let x1 = [x0]; let x2
/// = [x1]; ..` builds a type as deep as the program is long), so every
/// recursive walk, `Eq`, `Ord`, `Hash` and `Debug` included, runs under
/// [`ensure_sufficient`].
#[derive(Clone)]
pub enum Type {
    Bottom,
    Any,
    Primitive(BitFlags<Typ>),
    /// Boxed: inline, a ref (two paths, params, position, origin and
    /// cell) would make every type half again as large.
    Ref(Arc<TypeRef>),
    Fn(Arc<FnType>),
    Set(Arc<[Type]>),
    TVar(TVar),
    Error(Arc<Type>),
    Array(Arc<Type>),
    /// The native linked list. The runtime rep is private to
    /// `node::list`.
    List(Arc<Type>),
    ByRef(Mutability, Arc<Type>),
    Tuple(Arc<[Type]>),
    Struct(Arc<[(ArcStr, Type, WrittenAt)]>),
    Variant(ArcStr, Arc<[Type]>, WrittenAt),
    Map(Arc<MapType>),
    Abstract {
        id: AbstractId,
        params: Arc<[Type]>,
    },
    /// A type constructor applied to one argument (`self<'a>`,
    /// `'c<i64>`). The constructor is a type variable that binds to a
    /// type with a [`Type::Hole`] in its last parameter; once bound the
    /// application is its filled type ([`Type::app`]).
    App(Arc<Type>, Arc<Type>),
    /// The hole in a type constructor, written `'_` (`impl Collection
    /// for Array<'_>`). Legal nowhere else.
    Hole,
    /// A conjunct a cell's constraints may hold ([`Bound`]). Legal only as
    /// a constraint.
    Bound(Bound),
}

/// A map type's key and value types, in one allocation so a map many
/// paths share is one node ([`node_addr`]).
#[derive(Debug)]
pub struct MapType {
    pub key: Type,
    pub value: Type,
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ensure_sufficient(|| match self {
            Self::Bottom => f.write_str("Bottom"),
            Self::Any => f.write_str("Any"),
            Self::Hole => f.write_str("Hole"),
            Self::Bound(Bound::Concrete) => f.write_str("Concrete"),
            Self::Bound(Bound::Data) => f.write_str("Data"),
            Self::Bound(Bound::Function) => f.write_str("Function"),
            Self::Bound(Bound::Singleton) => f.write_str("Singleton"),
            Self::Bound(Bound::OneNumber) => f.write_str("OneNumber"),
            Self::Bound(Bound::NoError) => f.write_str("NoError"),
            Self::Bound(Bound::Discernible) => f.write_str("Discernible"),
            Self::Bound(Bound::Ordered) => f.write_str("Ordered"),
            Self::Bound(Bound::JsonData) => f.write_str("JsonData"),
            Self::Bound(Bound::TomlData) => f.write_str("TomlData"),
            Self::Primitive(p) => f.debug_tuple("Primitive").field(p).finish(),
            Self::Ref(r) => f.debug_tuple("Ref").field(r).finish(),
            Self::Fn(t) => f.debug_tuple("Fn").field(t).finish(),
            Self::Set(ts) => f.debug_tuple("Set").field(ts).finish(),
            Self::TVar(tv) => f.debug_tuple("TVar").field(tv).finish(),
            Self::Error(t) => f.debug_tuple("Error").field(t).finish(),
            Self::Array(t) => f.debug_tuple("Array").field(t).finish(),
            Self::List(t) => f.debug_tuple("List").field(t).finish(),
            Self::ByRef(m, t) => f.debug_tuple("ByRef").field(m).field(t).finish(),
            Self::Tuple(ts) => f.debug_tuple("Tuple").field(ts).finish(),
            Self::Struct(fs) => f.debug_tuple("Struct").field(fs).finish(),
            Self::Variant(tag, ts, at) => {
                f.debug_tuple("Variant").field(tag).field(ts).field(at).finish()
            }
            Self::Map(m) => f
                .debug_struct("Map")
                .field("key", &m.key)
                .field("value", &m.value)
                .finish(),
            Self::Abstract { id, params } => f
                .debug_struct("Abstract")
                .field("id", id)
                .field("params", params)
                .finish(),
            Self::App(c, a) => f.debug_tuple("App").field(c).field(a).finish(),
        })
    }
}

/// Whether a reference may be written through: `&T` reads, `&mut T`
/// also writes.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, netidx_derive::Pack,
)]
pub enum Mutability {
    Shared,
    Mut,
}

impl Mutability {
    fn tag(self) -> u8 {
        match self {
            Mutability::Shared => tag::BYREF,
            Mutability::Mut => tag::BYREF_MUT,
        }
    }

    /// The type and expression prefix: `&` or `&mut `.
    pub fn prefix(self) -> &'static str {
        match self {
            Mutability::Shared => "&",
            Mutability::Mut => "&mut ",
        }
    }
}

mod tag {
    pub const BOTTOM: u8 = 0;
    pub const ANY: u8 = 1;
    pub const PRIMITIVE: u8 = 2;
    pub const REF: u8 = 3;
    pub const FN: u8 = 4;
    pub const SET: u8 = 5;
    pub const TVAR: u8 = 6;
    pub const ERROR: u8 = 7;
    pub const ARRAY: u8 = 8;
    pub const LIST: u8 = 9;
    pub const BYREF: u8 = 10;
    pub const TUPLE: u8 = 11;
    pub const STRUCT: u8 = 12;
    pub const VARIANT: u8 = 13;
    pub const MAP: u8 = 14;
    pub const ABSTRACT: u8 = 15;
    pub const APP: u8 = 16;
    pub const HOLE: u8 = 17;
    pub const CONCRETE: u8 = 18;
    pub const FUNCTION: u8 = 19;
    pub const SINGLETON: u8 = 20;
    pub const ONE_NUMBER: u8 = 21;
    pub const BYREF_MUT: u8 = 22;
    pub const DISCERNIBLE: u8 = 23;
    pub const ORDERED: u8 = 24;
    pub const DATA: u8 = 25;
    pub const NO_ERROR: u8 = 26;
    pub const JSON_DATA: u8 = 27;
    pub const TOML_DATA: u8 = 28;
}

pub(super) fn key_text(s: &str, out: &mut Vec<u8>) {
    encode_varint(s.len() as u64, out);
    out.put_slice(s.as_bytes());
}

fn key_list(ts: &Arc<[Type]>, out: &mut Vec<u8>) {
    let keep = || KeyedNode::Types(ts.clone());
    image::shared_key(<[Type]>::as_ptr(ts) as usize, keep, out, |out| {
        encode_varint(ts.len() as u64, out);
        for t in ts.iter() {
            t.content_key(out);
        }
    })
}

fn key_map(m: &Arc<MapType>, out: &mut Vec<u8>) {
    let keep = || KeyedNode::Map(m.clone());
    image::shared_key(Arc::as_ptr(m) as usize, keep, out, |out| {
        m.key.content_key(out);
        m.value.content_key(out)
    })
}

fn key_one(t: &Arc<Type>, out: &mut Vec<u8>) {
    let keep = || KeyedNode::Type(t.clone());
    image::shared_key(Arc::as_ptr(t) as usize, keep, out, |out| t.content_key(out))
}

impl TypeRef {
    fn content_key(&self, out: &mut Vec<u8>) {
        let TypeRef { scope, name, params, pos, ori, resolved } = self;
        key_text(scope, out);
        key_text(name, out);
        key_list(params, out);
        match pos {
            None => out.put_u8(0),
            Some(p) => {
                out.put_u8(1);
                out.put_i32_le(p.line);
                out.put_i32_le(p.column);
            }
        }
        out.put_u64_le(ori.as_ref().map_or(0, |o| Arc::as_ptr(o) as usize as u64));
        out.put_u64_le(Arc::as_ptr(resolved) as *const () as usize as u64);
    }
}

impl Type {
    /// The canonical bytes the image keys this type by: the structure,
    /// with every shared leaf (a variable, a resolution cell, an
    /// origin, a lambda ids cell) by identity.
    pub(crate) fn content_key(&self, out: &mut Vec<u8>) {
        ensure_sufficient(|| self.content_key_inner(out))
    }

    fn content_key_inner(&self, out: &mut Vec<u8>) {
        match self {
            Type::Bottom => out.put_u8(tag::BOTTOM),
            Type::Any => out.put_u8(tag::ANY),
            Type::Hole => out.put_u8(tag::HOLE),
            Type::Bound(Bound::Concrete) => out.put_u8(tag::CONCRETE),
            Type::Bound(Bound::Data) => out.put_u8(tag::DATA),
            Type::Bound(Bound::Function) => out.put_u8(tag::FUNCTION),
            Type::Bound(Bound::Singleton) => out.put_u8(tag::SINGLETON),
            Type::Bound(Bound::OneNumber) => out.put_u8(tag::ONE_NUMBER),
            Type::Bound(Bound::NoError) => out.put_u8(tag::NO_ERROR),
            Type::Bound(Bound::Discernible) => out.put_u8(tag::DISCERNIBLE),
            Type::Bound(Bound::Ordered) => out.put_u8(tag::ORDERED),
            Type::Bound(Bound::JsonData) => out.put_u8(tag::JSON_DATA),
            Type::Bound(Bound::TomlData) => out.put_u8(tag::TOML_DATA),
            Type::Primitive(p) => {
                out.put_u8(tag::PRIMITIVE);
                out.put_u64_le(p.bits() as u64);
            }
            Type::Ref(r) => {
                out.put_u8(tag::REF);
                r.content_key(out);
            }
            Type::Fn(f) => {
                out.put_u8(tag::FN);
                let keep = || KeyedNode::Fn(f.clone());
                image::shared_key(Arc::as_ptr(f) as usize, keep, out, |out| {
                    f.content_key(out)
                });
            }
            Type::TVar(tv) => {
                out.put_u8(tag::TVAR);
                out.put_u64_le(tv.wrapper_addr() as u64);
            }
            Type::Set(ts) => {
                out.put_u8(tag::SET);
                key_list(ts, out);
            }
            Type::Tuple(ts) => {
                out.put_u8(tag::TUPLE);
                key_list(ts, out);
            }
            Type::Error(t) => {
                out.put_u8(tag::ERROR);
                key_one(t, out);
            }
            Type::Array(t) => {
                out.put_u8(tag::ARRAY);
                key_one(t, out);
            }
            Type::List(t) => {
                out.put_u8(tag::LIST);
                key_one(t, out);
            }
            Type::ByRef(m, t) => {
                out.put_u8(m.tag());
                key_one(t, out);
            }
            Type::Struct(fs) => {
                out.put_u8(tag::STRUCT);
                image::shared_key(
                    <[(ArcStr, Type, WrittenAt)]>::as_ptr(fs) as usize,
                    || KeyedNode::Fields(fs.clone()),
                    out,
                    |out| {
                        encode_varint(fs.len() as u64, out);
                        for (n, t, _) in fs.iter() {
                            key_text(n, out);
                            t.content_key(out);
                        }
                    },
                );
            }
            Type::Variant(name, ts, _) => {
                out.put_u8(tag::VARIANT);
                key_text(name, out);
                key_list(ts, out);
            }
            Type::Map(m) => {
                out.put_u8(tag::MAP);
                key_map(m, out);
            }
            Type::Abstract { id, params } => {
                out.put_u8(tag::ABSTRACT);
                out.put_u64_le(id.0);
                key_list(params, out);
            }
            Type::App(c, a) => {
                out.put_u8(tag::APP);
                key_one(c, out);
                key_one(a, out);
            }
        }
    }

    fn shape_len(&self) -> usize {
        ensure_sufficient(|| self.shape_len_inner())
    }

    fn shape_len_inner(&self) -> usize {
        1 + match self {
            Type::Bottom | Type::Any | Type::Hole | Type::Bound(_) => 0,
            Type::Primitive(p) => p.encoded_len(),
            Type::Ref(r) => r.encoded_len(),
            Type::Fn(f) => f.encoded_len(),
            Type::TVar(tv) => tv.encoded_len(),
            Type::Set(ts) | Type::Tuple(ts) => ts.encoded_len(),
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => {
                t.encoded_len()
            }
            Type::Struct(fs) => fs.encoded_len(),
            Type::Variant(name, ts, _) => name.encoded_len() + ts.encoded_len(),
            Type::Map(m) => m.key.encoded_len() + m.value.encoded_len(),
            Type::Abstract { id, params } => id.encoded_len() + params.encoded_len(),
            Type::App(c, a) => c.encoded_len() + a.encoded_len(),
        }
    }

    fn shape_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        ensure_sufficient(|| self.shape_encode_inner(buf))
    }

    fn shape_encode_inner(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            Type::Bottom => Ok(buf.put_u8(tag::BOTTOM)),
            Type::Any => Ok(buf.put_u8(tag::ANY)),
            Type::Hole => Ok(buf.put_u8(tag::HOLE)),
            Type::Bound(Bound::Concrete) => Ok(buf.put_u8(tag::CONCRETE)),
            Type::Bound(Bound::Data) => Ok(buf.put_u8(tag::DATA)),
            Type::Bound(Bound::Function) => Ok(buf.put_u8(tag::FUNCTION)),
            Type::Bound(Bound::Singleton) => Ok(buf.put_u8(tag::SINGLETON)),
            Type::Bound(Bound::OneNumber) => Ok(buf.put_u8(tag::ONE_NUMBER)),
            Type::Bound(Bound::NoError) => Ok(buf.put_u8(tag::NO_ERROR)),
            Type::Bound(Bound::Discernible) => Ok(buf.put_u8(tag::DISCERNIBLE)),
            Type::Bound(Bound::Ordered) => Ok(buf.put_u8(tag::ORDERED)),
            Type::Bound(Bound::JsonData) => Ok(buf.put_u8(tag::JSON_DATA)),
            Type::Bound(Bound::TomlData) => Ok(buf.put_u8(tag::TOML_DATA)),
            Type::Primitive(p) => {
                buf.put_u8(tag::PRIMITIVE);
                p.encode(buf)
            }
            Type::Ref(r) => {
                buf.put_u8(tag::REF);
                r.encode(buf)
            }
            Type::Fn(f) => {
                buf.put_u8(tag::FN);
                f.encode(buf)
            }
            Type::TVar(tv) => {
                buf.put_u8(tag::TVAR);
                tv.encode(buf)
            }
            Type::Set(ts) => {
                buf.put_u8(tag::SET);
                ts.encode(buf)
            }
            Type::Tuple(ts) => {
                buf.put_u8(tag::TUPLE);
                ts.encode(buf)
            }
            Type::Error(t) => {
                buf.put_u8(tag::ERROR);
                t.encode(buf)
            }
            Type::Array(t) => {
                buf.put_u8(tag::ARRAY);
                t.encode(buf)
            }
            Type::List(t) => {
                buf.put_u8(tag::LIST);
                t.encode(buf)
            }
            Type::ByRef(m, t) => {
                buf.put_u8(m.tag());
                t.encode(buf)
            }
            Type::Struct(fs) => {
                buf.put_u8(tag::STRUCT);
                fs.encode(buf)
            }
            Type::Variant(name, ts, _) => {
                buf.put_u8(tag::VARIANT);
                name.encode(buf)?;
                ts.encode(buf)
            }
            Type::Map(m) => {
                buf.put_u8(tag::MAP);
                m.key.encode(buf)?;
                m.value.encode(buf)
            }
            Type::Abstract { id, params } => {
                buf.put_u8(tag::ABSTRACT);
                id.encode(buf)?;
                params.encode(buf)
            }
            Type::App(c, a) => {
                buf.put_u8(tag::APP);
                c.encode(buf)?;
                a.encode(buf)
            }
        }
    }

    fn shape_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        Ok(match buf.get_u8() {
            tag::BOTTOM => Type::Bottom,
            tag::ANY => Type::Any,
            tag::HOLE => Type::Hole,
            tag::CONCRETE => Type::Bound(Bound::Concrete),
            tag::DATA => Type::Bound(Bound::Data),
            tag::FUNCTION => Type::Bound(Bound::Function),
            tag::SINGLETON => Type::Bound(Bound::Singleton),
            tag::ONE_NUMBER => Type::Bound(Bound::OneNumber),
            tag::NO_ERROR => Type::Bound(Bound::NoError),
            tag::DISCERNIBLE => Type::Bound(Bound::Discernible),
            tag::ORDERED => Type::Bound(Bound::Ordered),
            tag::JSON_DATA => Type::Bound(Bound::JsonData),
            tag::TOML_DATA => Type::Bound(Bound::TomlData),
            tag::PRIMITIVE => Type::Primitive(PackTrait::decode(buf)?),
            tag::REF => Type::Ref(PackTrait::decode(buf)?),
            tag::FN => Type::Fn(PackTrait::decode(buf)?),
            tag::TVAR => Type::TVar(PackTrait::decode(buf)?),
            tag::SET => Type::Set(PackTrait::decode(buf)?),
            tag::TUPLE => Type::Tuple(PackTrait::decode(buf)?),
            tag::ERROR => Type::Error(PackTrait::decode(buf)?),
            tag::ARRAY => Type::Array(PackTrait::decode(buf)?),
            tag::LIST => Type::List(PackTrait::decode(buf)?),
            tag::BYREF => Type::ByRef(Mutability::Shared, PackTrait::decode(buf)?),
            tag::BYREF_MUT => Type::ByRef(Mutability::Mut, PackTrait::decode(buf)?),
            tag::STRUCT => Type::Struct(PackTrait::decode(buf)?),
            tag::VARIANT => {
                let name = PackTrait::decode(buf)?;
                Type::Variant(name, PackTrait::decode(buf)?, WrittenAt::NOWHERE)
            }
            tag::MAP => {
                let key = PackTrait::decode(buf)?;
                Type::map(key, PackTrait::decode(buf)?)
            }
            tag::ABSTRACT => {
                let id = PackTrait::decode(buf)?;
                Type::Abstract { id, params: PackTrait::decode(buf)? }
            }
            tag::APP => {
                let c = PackTrait::decode(buf)?;
                Type::App(c, PackTrait::decode(buf)?)
            }
            _ => return Err(PackError::UnknownTag),
        })
    }
}

/// Under an image session a type is an object keyed by its content,
/// written once and referenced afterwards.
impl PackTrait for Type {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() { image::type_len(self) } else { self.shape_len() }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::type_encode(self, buf, |b| self.shape_encode(b))
        } else {
            self.shape_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        // outside bytes reach this (a value's abstract params): one level
        // per byte
        // CR claude for claude: [doc-drift/low] The comment above says outside bytes
        // reach this decode through a value's abstract params, but `GxAbstract::decode`
        // refuses outside an image session (pin
        // `abstract_value_decodes_only_under_an_image`), so only an image's bytes and a
        // packed AST's reach here. The guard is still needed, since depth follows the
        // bytes; the comment names a path that no longer exists. Delete it or say what
        // does reach the decode. Found by reading; not run. (t-typ-mod-05)
        ensure_sufficient(|| {
            if image::is_decoding() {
                image::object_decode(buf, |b| Self::shape_decode(b), |b| Self::decode(b))
            } else {
                Self::shape_decode(buf)
            }
        })
    }
}

/// Structural equality with content-Arc pointer shortcuts (the
/// copy-on-write walks share aggressively). Exhaustive on `self` so a
/// new variant fails to compile.
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bottom, _) => matches!(other, Type::Bottom),
            (Type::Any, _) => matches!(other, Type::Any),
            (Type::Hole, _) => matches!(other, Type::Hole),
            (Type::Bound(Bound::Concrete), _) => {
                matches!(other, Type::Bound(Bound::Concrete))
            }
            (Type::Bound(Bound::Data), _) => matches!(other, Type::Bound(Bound::Data)),
            (Type::Bound(Bound::Function), _) => {
                matches!(other, Type::Bound(Bound::Function))
            }
            (Type::Bound(Bound::Singleton), _) => {
                matches!(other, Type::Bound(Bound::Singleton))
            }
            (Type::Bound(Bound::OneNumber), _) => {
                matches!(other, Type::Bound(Bound::OneNumber))
            }
            (Type::Bound(Bound::NoError), _) => {
                matches!(other, Type::Bound(Bound::NoError))
            }
            (Type::Bound(Bound::Discernible), _) => {
                matches!(other, Type::Bound(Bound::Discernible))
            }
            (Type::Bound(Bound::Ordered), _) => {
                matches!(other, Type::Bound(Bound::Ordered))
            }
            (Type::Bound(Bound::JsonData), _) => {
                matches!(other, Type::Bound(Bound::JsonData))
            }
            (Type::Bound(Bound::TomlData), _) => {
                matches!(other, Type::Bound(Bound::TomlData))
            }
            (Type::Primitive(a), _) => matches!(other, Type::Primitive(b) if a == b),
            _ => ensure_sufficient(|| self.eq_composite(other)),
        }
    }
}

impl Eq for Type {}

impl Type {
    fn eq_composite(&self, other: &Self) -> bool {
        fn slice_eq(a: &Arc<[Type]>, b: &Arc<[Type]>) -> bool {
            (**a).as_ptr() == (**b).as_ptr() || **a == **b
        }
        fn one_eq(a: &Arc<Type>, b: &Arc<Type>) -> bool {
            Arc::ptr_eq(a, b) || a == b
        }
        match self {
            Type::Bottom => matches!(other, Type::Bottom),
            Type::Any => matches!(other, Type::Any),
            Type::Hole => matches!(other, Type::Hole),
            Type::Bound(Bound::Concrete) => matches!(other, Type::Bound(Bound::Concrete)),
            Type::Bound(Bound::Data) => matches!(other, Type::Bound(Bound::Data)),
            Type::Bound(Bound::Function) => matches!(other, Type::Bound(Bound::Function)),
            Type::Bound(Bound::Singleton) => {
                matches!(other, Type::Bound(Bound::Singleton))
            }
            Type::Bound(Bound::OneNumber) => {
                matches!(other, Type::Bound(Bound::OneNumber))
            }
            Type::Bound(Bound::NoError) => matches!(other, Type::Bound(Bound::NoError)),
            Type::Bound(Bound::Discernible) => {
                matches!(other, Type::Bound(Bound::Discernible))
            }
            Type::Bound(Bound::Ordered) => matches!(other, Type::Bound(Bound::Ordered)),
            Type::Bound(Bound::JsonData) => matches!(other, Type::Bound(Bound::JsonData)),
            Type::Bound(Bound::TomlData) => matches!(other, Type::Bound(Bound::TomlData)),
            Type::Primitive(a) => matches!(other, Type::Primitive(b) if a == b),
            Type::Ref(a) => matches!(other, Type::Ref(b) if a == b),
            Type::Fn(a) => {
                matches!(other, Type::Fn(b) if Arc::ptr_eq(a, b) || a == b)
            }
            Type::Set(a) => matches!(other, Type::Set(b) if slice_eq(a, b)),
            Type::TVar(a) => matches!(other, Type::TVar(b) if a == b),
            Type::Error(a) => matches!(other, Type::Error(b) if one_eq(a, b)),
            Type::Array(a) => matches!(other, Type::Array(b) if one_eq(a, b)),
            Type::List(a) => matches!(other, Type::List(b) if one_eq(a, b)),
            Type::ByRef(m, a) => {
                matches!(other, Type::ByRef(n, b) if m == n && one_eq(a, b))
            }
            Type::Tuple(a) => matches!(other, Type::Tuple(b) if slice_eq(a, b)),
            Type::Struct(a) => matches!(
                other,
                Type::Struct(b) if (**a).as_ptr() == (**b).as_ptr() || **a == **b
            ),
            Type::Variant(t0, a, _) => {
                matches!(other, Type::Variant(t1, b, _) if t0 == t1 && slice_eq(a, b))
            }
            Type::Map(m0) => matches!(
                other,
                Type::Map(m1) if Arc::ptr_eq(m0, m1)
                    || (m0.key == m1.key && m0.value == m1.value)
            ),
            Type::Abstract { id: i0, params: p0 } => matches!(
                other,
                Type::Abstract { id: i1, params: p1 } if i0 == i1 && slice_eq(p0, p1)
            ),
            Type::App(c0, a0) => matches!(
                other,
                Type::App(c1, a1) if one_eq(c0, c1) && one_eq(a0, a1)
            ),
        }
    }

    /// The variant's position in declaration order (its image tag).
    fn rank(&self) -> u8 {
        match self {
            Type::Bottom => tag::BOTTOM,
            Type::Any => tag::ANY,
            Type::Primitive(_) => tag::PRIMITIVE,
            Type::Ref(_) => tag::REF,
            Type::Fn(_) => tag::FN,
            Type::Set(_) => tag::SET,
            Type::TVar(_) => tag::TVAR,
            Type::Error(_) => tag::ERROR,
            Type::Array(_) => tag::ARRAY,
            Type::List(_) => tag::LIST,
            Type::ByRef(m, _) => m.tag(),
            Type::Tuple(_) => tag::TUPLE,
            Type::Struct(_) => tag::STRUCT,
            Type::Variant(..) => tag::VARIANT,
            Type::Map { .. } => tag::MAP,
            Type::Abstract { .. } => tag::ABSTRACT,
            Type::App(..) => tag::APP,
            Type::Hole => tag::HOLE,
            Type::Bound(Bound::Concrete) => tag::CONCRETE,
            Type::Bound(Bound::Data) => tag::DATA,
            Type::Bound(Bound::Function) => tag::FUNCTION,
            Type::Bound(Bound::Singleton) => tag::SINGLETON,
            Type::Bound(Bound::OneNumber) => tag::ONE_NUMBER,
            Type::Bound(Bound::NoError) => tag::NO_ERROR,
            Type::Bound(Bound::Discernible) => tag::DISCERNIBLE,
            Type::Bound(Bound::Ordered) => tag::ORDERED,
            Type::Bound(Bound::JsonData) => tag::JSON_DATA,
            Type::Bound(Bound::TomlData) => tag::TOML_DATA,
        }
    }

    /// Two types of one variant, field by field in declaration order.
    fn cmp_fields(&self, other: &Self) -> Ordering {
        fn slice_cmp<T: Ord>(a: &Arc<[T]>, b: &Arc<[T]>) -> Ordering {
            if (**a).as_ptr() == (**b).as_ptr() { Ordering::Equal } else { a.cmp(b) }
        }
        fn one_cmp<T: Ord>(a: &Arc<T>, b: &Arc<T>) -> Ordering {
            if Arc::ptr_eq(a, b) { Ordering::Equal } else { a.cmp(b) }
        }
        match (self, other) {
            (Type::Primitive(a), Type::Primitive(b)) => a.cmp(b),
            (Type::Ref(a), Type::Ref(b)) => one_cmp(a, b),
            (Type::Fn(a), Type::Fn(b)) => one_cmp(a, b),
            (Type::TVar(a), Type::TVar(b)) => a.cmp(b),
            (Type::Set(a), Type::Set(b)) | (Type::Tuple(a), Type::Tuple(b)) => {
                slice_cmp(a, b)
            }
            (Type::Error(a), Type::Error(b))
            | (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::ByRef(_, a), Type::ByRef(_, b)) => one_cmp(a, b),
            (Type::Struct(a), Type::Struct(b)) => slice_cmp(a, b),
            (Type::Variant(t0, a, w0), Type::Variant(t1, b, w1)) => {
                t0.cmp(t1).then_with(|| slice_cmp(a, b)).then_with(|| w0.cmp(w1))
            }
            (Type::Map(m0), Type::Map(m1)) if Arc::ptr_eq(m0, m1) => Ordering::Equal,
            (Type::Map(m0), Type::Map(m1)) => {
                m0.key.cmp(&m1.key).then_with(|| m0.value.cmp(&m1.value))
            }
            (
                Type::Abstract { id: i0, params: p0 },
                Type::Abstract { id: i1, params: p1 },
            ) => i0.cmp(i1).then_with(|| slice_cmp(p0, p1)),
            (Type::App(c0, a0), Type::App(c1, a1)) => {
                one_cmp(c0, c1).then_with(|| one_cmp(a0, a1))
            }
            _ => Ordering::Equal,
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> Ordering {
        self.rank().cmp(&other.rank()).then_with(|| match self {
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Bound(_)
            | Type::Primitive(_) => self.cmp_fields(other),
            _ => ensure_sufficient(|| self.cmp_fields(other)),
        })
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rank().hash(state);
        match self {
            Type::Bottom | Type::Any | Type::Hole | Type::Bound(_) => (),
            Type::Primitive(p) => p.hash(state),
            t => ensure_sufficient(|| match t {
                Type::Ref(r) => r.hash(state),
                Type::Fn(f) => f.hash(state),
                Type::TVar(tv) => tv.hash(state),
                Type::Set(ts) | Type::Tuple(ts) => ts.hash(state),
                Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => {
                    t.hash(state)
                }
                Type::Struct(fs) => fs.hash(state),
                Type::Variant(tag, ts, at) => {
                    tag.hash(state);
                    ts.hash(state);
                    at.hash(state)
                }
                Type::Map(m) => {
                    m.key.hash(state);
                    m.value.hash(state)
                }
                Type::Abstract { id, params } => {
                    id.hash(state);
                    params.hash(state)
                }
                Type::App(c, a) => {
                    c.hash(state);
                    a.hash(state)
                }
                Type::Bottom
                | Type::Any
                | Type::Hole
                | Type::Bound(_)
                | Type::Primitive(_) => (),
            }),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Bottom
    }
}

impl Type {
    /// A `'static` ⊥ for a node whose type is bottom: with drop glue,
    /// `&Type::Bottom` is not promoted.
    pub const BOTTOM: &'static Type = &Type::Bottom;

    /// The type of a literal value, in an expression or a pattern: its
    /// primitive, or for `error:<v>` the error of its payload's type. A
    /// literal no type names (`abstract:..`, a collection) is refused.
    pub fn of_literal(v: &netidx_value::Value) -> Result<Type> {
        use netidx_value::Value;
        ensure_sufficient(|| match v {
            Value::Error(p) => Ok(Type::Error(Arc::new(Type::of_literal(p)?))),
            Value::Abstract(_) => {
                bail!(
                    "an abstract value literal has no type: build it with its constructor"
                )
            }
            Value::Array(_) | Value::Map(_) => {
                bail!("a collection value literal has no type: write it as an expression")
            }
            v => Ok(Type::Primitive(Typ::get(v).into())),
        })
    }
}

/// Field drop glue runs after `drop` returns and cannot be guarded, so
/// a composite that owns the last reference to its content drops its
/// fields here, under the guard, out of a `ManuallyDrop` copy; the glue
/// then drops the leaf left behind. A shared content only decrements.
impl Drop for Type {
    fn drop(&mut self) {
        use std::{mem::ManuallyDrop, ptr};
        let last = match self {
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Bound(_)
            | Type::Primitive(_)
            | Type::TVar(_) => false,
            Type::Ref(tr) => tr.is_unique() && tr.params.is_unique(),
            Type::Fn(f) => f.is_unique(),
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.is_unique(),
            Type::Abstract { params, .. } => params.is_unique(),
            Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(_, a) => {
                a.is_unique()
            }
            Type::Struct(fs) => fs.is_unique(),
            Type::Map(m) => m.is_unique(),
            Type::App(a, b) => a.is_unique() || b.is_unique(),
        };
        if !last {
            return;
        }
        let t = ManuallyDrop::new(mem::take(self));
        // SAFETY: each field is read out once and `t` is never dropped.
        ensure_sufficient(|| unsafe {
            match &*t {
                Type::Bottom
                | Type::Any
                | Type::Hole
                | Type::Bound(_)
                | Type::Primitive(_) => (),
                Type::Ref(r) => drop(ptr::read(r)),
                Type::Fn(f) => drop(ptr::read(f)),
                Type::TVar(tv) => drop(ptr::read(tv)),
                Type::Set(ts) | Type::Tuple(ts) => drop(ptr::read(ts)),
                Type::Error(a) | Type::Array(a) | Type::List(a) | Type::ByRef(_, a) => {
                    drop(ptr::read(a))
                }
                Type::Struct(fs) => drop(ptr::read(fs)),
                Type::Variant(tag, ts, _) => {
                    drop(ptr::read(tag));
                    drop(ptr::read(ts))
                }
                Type::Map(m) => drop(ptr::read(m)),
                Type::Abstract { id: _, params } => drop(ptr::read(params)),
                Type::App(c, a) => {
                    drop(ptr::read(c));
                    drop(ptr::read(a))
                }
            }
        })
    }
}

/// A classifiable resolution failure from [`Type::lookup_ref`]: the
/// name, and where it is written.
#[derive(Debug)]
pub struct UnresolvableRef {
    pub name: ModPath,
    pub scope: ModPath,
    pub pos: Option<SourcePosition>,
    pub ori: Option<Arc<Origin>>,
}

impl UnresolvableRef {
    pub fn of(tr: &TypeRef) -> Self {
        Self {
            name: tr.name.clone(),
            scope: tr.scope.clone(),
            pos: tr.pos,
            ori: tr.ori.clone(),
        }
    }

    /// The error a report names: a mistake in the path when there is
    /// one, else this.
    pub fn error(tr: &TypeRef, env: &Env) -> anyhow::Error {
        tr.resolve_error(env).unwrap_or_else(|| anyhow::Error::new(Self::of(tr)))
    }
}

/// `undefined type T[ in m][ at <pos>[ in <source>]]`: the scope only when
/// it is a module a program names (a block's or function's is minted).
impl std::fmt::Display for UnresolvableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "undefined type {}", self.name)?;
        let minted =
            netidx_core::path::Path::parts(&self.scope.0).any(|p| p.starts_with('#'));
        if !minted && &*self.scope.0 != "/" {
            write!(f, " in {}", self.scope)?;
        }
        if let Some(pos) = self.pos {
            write!(f, " at {pos}")?;
            match self.ori.as_ref().map(|o| &o.source) {
                None | Some(Source::Program | Source::Unspecified) => (),
                Some(source) => write!(f, " in {source}")?,
            }
        }
        Ok(())
    }
}

impl std::error::Error for UnresolvableRef {}

impl Type {
    /// Read-only walk over this type's immediate structural children.
    /// `TVar` is a leaf: cell contents are per-walk policy. A recursive
    /// walk matches its interesting arms and routes the rest here; see
    /// [`Self::cow_children`] for rebuild walks.
    pub(crate) fn try_for_each_child<B>(
        &self,
        f: &mut impl FnMut(&Type) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole
            | Type::Bound(_) => ControlFlow::Continue(()),
            Type::App(c, a) => {
                f(c)?;
                f(a)
            }
            Type::Ref(tr) => {
                for t in tr.params.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Abstract { id: _, params } => {
                for t in params.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                for t in ts.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Struct(fs) => {
                for (_, t, _) in fs.iter() {
                    f(t)?;
                }
                ControlFlow::Continue(())
            }
            Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => f(t),
            Type::Map(m) => {
                f(&m.key)?;
                f(&m.value)
            }
            Type::Fn(ft) => ft.try_for_each_type(f),
        }
    }

    /// The definitions this type's filled cells name, and theirs, each
    /// once: through bindings and conjuncts.
    #[doc(hidden)]
    pub fn named_defs(
        &self,
        cells: &mut AHashSet<usize>,
        out: &mut AHashMap<usize, sync::Arc<ResolvedRef>>,
    ) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if cells.insert(tv.cell_addr()) {
                    if let Some(b) = tv.binding() {
                        b.named_defs(cells, out)
                    }
                    for c in tv.cell_above() {
                        c.named_defs(cells, out)
                    }
                }
            }
            Type::Ref(tr) => {
                if let Some(r) = tr.resolved()
                    && !out.contains_key(&(sync::Arc::as_ptr(&r) as usize))
                {
                    out.insert(sync::Arc::as_ptr(&r) as usize, r.clone());
                    r.typ.named_defs(cells, out);
                    for c in r.params.iter().filter_map(|(_, c)| c.as_ref()) {
                        c.named_defs(cells, out)
                    }
                }
                self.for_each_child(&mut |c| c.named_defs(cells, out))
            }
            // a shared node's address is another allocation than any cell's
            t if node_addr(t).is_some_and(|n| !cells.insert(n)) => (),
            Type::Fn(ft) => ft.for_each_part(&mut |t, _| t.named_defs(cells, out)),
            t => t.for_each_child(&mut |c| c.named_defs(cells, out)),
        })
    }

    /// The traits this type names as a bound, each once: a reference
    /// whose cell holds a trait, in a variable's binding or conjuncts or
    /// in a part. A definition's gate fills the cell of every bound its
    /// signature declares, and a call copies a callee's filled cells. A
    /// typedef's body names no trait (a written type refuses one,
    /// [`Self::rewrite_trait_args`]), so only a typedef's arguments are
    /// walked.
    #[doc(hidden)]
    pub fn named_traits(&self, cells: &mut AddrSet, out: &mut SmallVec<[TraitId; 2]>) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if cells.insert(tv.cell_addr()) {
                    if let Some(b) = tv.binding() {
                        b.named_traits(cells, out)
                    }
                    for c in tv.cell_above() {
                        c.named_traits(cells, out)
                    }
                }
            }
            Type::Ref(tr) => {
                if let Some(id) = tr.trait_id()
                    && !out.contains(&id)
                {
                    out.push(id)
                }
                self.for_each_child(&mut |c| c.named_traits(cells, out))
            }
            t if node_addr(t).is_some_and(|n| !cells.insert(n)) => (),
            Type::Fn(ft) => ft.for_each_type(&mut |t| t.named_traits(cells, out)),
            t => t.for_each_child(&mut |c| c.named_traits(cells, out)),
        })
    }

    /// Whether a trait method's `self` occurs in this type.
    pub fn mentions_self(&self) -> bool {
        crate::stack::ensure_sufficient(|| match self {
            Type::TVar(tv) if &*tv.name == "self" => true,
            t => {
                let mut found = false;
                t.for_each_child(&mut |c| found |= c.mentions_self());
                found
            }
        })
    }

    /// Whether a trait method's `self` occurs in this type where a value
    /// of it is taken in: under a function's parameter an odd number of
    /// times, or as the referent of a writable reference or an abstract
    /// type's parameter, through the typedefs the type names. Where it
    /// only flows out, a union's call returns each member's own.
    pub fn mentions_self_taken_in(&self, env: &Env) -> bool {
        self.self_taken_in(env, false, &mut SmallVec::new())
    }

    fn self_taken_in(
        &self,
        env: &Env,
        taken: bool,
        seen: &mut SmallVec<[(usize, bool); 4]>,
    ) -> bool {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => taken && &*tv.name == "self",
            Type::Fn(ft) => {
                ft.bound_mentions_self()
                    || ft.args.iter().any(|a| a.typ.self_taken_in(env, !taken, seen))
                    || ft
                        .vargs
                        .as_ref()
                        .is_some_and(|t| t.self_taken_in(env, !taken, seen))
                    || ft.rtype.self_taken_in(env, taken, seen)
                    || ft.throws.self_taken_in(env, taken, seen)
            }
            Type::ByRef(Mutability::Mut, t) => t.mentions_self(),
            Type::Abstract { params, .. } => params.iter().any(Type::mentions_self),
            // CR claude for claude: [bug/medium] `seen` keys a typedef by its
            // definition and polarity, not by the application, so a second application
            // of one typedef is never walked: over `type F<'a> = {cb: fn(d: 'a) ->
            // string}` the result `(F<i64>, F<self>)` answers that `self` is not taken
            // in, and `TraitDef::self_beyond_receiver` lets a union self call through.
            // The check accepts `T::mk(v)` over `v: [i64, string]` and the build then
            // refuses it with a type mismatch; with another typedef of the same body in
            // the first slot the check refuses as intended. Key `seen` by the
            // definition, the polarity and the parameters, as `Type::reaches_unguarded`
            // keys its own. probe:
            // design/review-2026-11-16/repro/t-typ-mod-self-taken-in-seen.gx
            // (t-typ-mod-02)
            Type::Ref(tr) => match self.lookup_ref_probe(env) {
                Some((key, body)) => {
                    !seen.contains(&(key, taken)) && {
                        seen.push((key, taken));
                        body.self_taken_in(env, taken, seen)
                    }
                }
                None => tr.params.iter().any(Type::mentions_self),
            },
            t => {
                let mut found = false;
                t.for_each_child(&mut |c| found |= c.self_taken_in(env, taken, seen));
                found
            }
        })
    }

    /// [`Self::try_for_each_child`] without early exit.
    #[doc(hidden)]
    pub fn for_each_child(&self, f: &mut impl FnMut(&Type)) {
        let _ = self.try_for_each_child::<()>(&mut |t| {
            f(t);
            ControlFlow::Continue(())
        });
    }

    /// Whether a value of `v` may be a value of `self` by their outermost
    /// constructors alone: two arrays, lists, maps, errors or function
    /// types; tuples of one arity; structs of one field set; variants of
    /// one tag and arity; two references, unless `self` is writable and `v`
    /// read-only; one abstract type; one typedef by its written name; one
    /// constructor variable applied (`self<'c>`).
    pub fn takes_constructor_of(&self, v: &Type) -> bool {
        match (self, v) {
            (Type::Array(_), Type::Array(_))
            | (Type::List(_), Type::List(_))
            | (Type::Map(_), Type::Map(_))
            | (Type::Error(_), Type::Error(_))
            | (Type::Fn(_), Type::Fn(_)) => true,
            (Type::ByRef(m, _), Type::ByRef(n, _)) => {
                *m == Mutability::Shared || *n == Mutability::Mut
            }
            (Type::Tuple(a), Type::Tuple(b)) => a.len() == b.len(),
            (Type::Struct(a), Type::Struct(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.0 == y.0)
            }
            (Type::Variant(t, a, _), Type::Variant(u, b, _)) => {
                t == u && a.len() == b.len()
            }
            (Type::Abstract { id: a, .. }, Type::Abstract { id: b, .. }) => a == b,
            (Type::Ref(a), Type::Ref(b)) => a.scope == b.scope && a.name == b.name,
            (Type::App(f, _), Type::App(g, _)) => {
                matches!((&**f, &**g), (Type::TVar(f), Type::TVar(g)) if f.name == g.name)
            }
            _ => false,
        }
    }

    /// Whether one value may be a value of both `self` and `t` by their
    /// outermost constructors ([`Self::takes_constructor_of`] either way).
    pub fn shares_constructor(&self, t: &Type) -> bool {
        self.takes_constructor_of(t) || t.takes_constructor_of(self)
    }

    /// Each own quantifier ([`FnType::each_quantifier`]) of every function
    /// type under this one, itself included, bindings followed.
    pub fn each_quantifier(&self, f: &mut impl FnMut(&ArcStr, &TVar)) {
        self.quantifiers_seen(false, &mut LPooled::take(), f)
    }

    /// [`Self::each_quantifier`]; with `open_only`, a cell proved ground
    /// is passed by: its binding holds no quantifier whose cell is open.
    pub(crate) fn quantifiers_seen(
        &self,
        open_only: bool,
        seen: &mut AHashSet<usize>,
        f: &mut impl FnMut(&ArcStr, &TVar),
    ) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if seen.insert(tv.cell_addr())
                    && !(open_only && tv.ground_now())
                    && let Some(b) = tv.binding()
                {
                    b.quantifiers_seen(open_only, seen, f)
                }
            }
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            Type::Fn(ft) => {
                ft.each_quantifier(|name, tv| f(name, tv));
                ft.for_each_part(&mut |t, _| t.quantifiers_seen(open_only, seen, f))
            }
            t => t.for_each_child(&mut |c| c.quantifiers_seen(open_only, seen, f)),
        })
    }

    /// Each function type this type holds outside any other function type,
    /// bindings followed, and what flows into a cell still open: the
    /// function values a holder of this type may carry.
    pub fn each_held_fn(&self, f: &mut impl FnMut(&FnType)) {
        self.held_fns_seen(&mut LPooled::take(), f)
    }

    fn held_fns_seen(&self, seen: &mut AHashSet<usize>, f: &mut impl FnMut(&FnType)) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                if seen.insert(tv.cell_addr()) {
                    match tv.binding() {
                        Some(b) => b.held_fns_seen(seen, f),
                        None => {
                            tv.cell_lower().iter().for_each(|t| t.held_fns_seen(seen, f))
                        }
                    }
                }
            }
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            Type::Fn(ft) => f(ft),
            t => t.for_each_child(&mut |c| c.held_fns_seen(seen, f)),
        })
    }

    /// Rebuild this type's immediate structural children through `f`
    /// (`None` from `f` means unchanged); `None` when nothing changed.
    /// Leaves, `TVar` included, return `None`. `Ref` params rebuild
    /// through [`TypeRef::with_params`], sharing the resolution cell.
    #[doc(hidden)]
    pub fn cow_children(
        &self,
        f: &mut impl FnMut(&Type) -> Option<Type>,
    ) -> Option<Type> {
        match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::TVar(_)
            | Type::Hole
            | Type::Bound(_) => None,
            Type::App(c, a) => match (f(c), f(a)) {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
            Type::Ref(tr) => Type::cow_slice(&tr.params, |t| f(t))
                .map(|params| Type::Ref(Arc::new(tr.with_params(params)))),
            Type::Abstract { id, params } => Type::cow_slice(params, |t| f(t))
                .map(|params| Type::Abstract { id: *id, params }),
            Type::Error(t) => f(t).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => f(t).map(|t| Type::Array(Arc::new(t))),
            Type::List(t) => f(t).map(|t| Type::List(Arc::new(t))),
            Type::ByRef(m, t) => f(t).map(|t| Type::ByRef(*m, Arc::new(t))),
            Type::Map(m) => match (f(&m.key), f(&m.value)) {
                (None, None) => None,
                (k, v) => Some(Type::map(
                    k.unwrap_or_else(|| m.key.clone()),
                    v.unwrap_or_else(|| m.value.clone()),
                )),
            },
            Type::Tuple(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Tuple),
            Type::Variant(tag, ts, at) => Type::cow_slice(ts, |t| f(t))
                .map(|ts| Type::Variant(tag.clone(), ts, *at)),
            Type::Set(ts) => Type::cow_slice(ts, |t| f(t)).map(Type::Set),
            Type::Struct(fs) => {
                Type::cow_slice(fs, |(n, t, at)| f(t).map(|t| (n.clone(), t, *at)))
                    .map(Type::Struct)
            }
            Type::Fn(ft) => ft.cow_walk(|t| f(t)).map(|ft| Type::Fn(Arc::new(ft))),
        }
    }

    pub fn empty_tvar() -> Self {
        Type::TVar(TVar::default())
    }

    pub fn map(key: Type, value: Type) -> Type {
        Type::Map(Arc::new(MapType { key, value }))
    }

    /// Apply a constructor to an argument: a concrete constructor (a
    /// type with a hole) is filled, a variable stays an application
    /// until it binds.
    pub fn app(ctor: Type, arg: Type) -> Type {
        match &ctor {
            Type::TVar(_) => Type::App(Arc::new(ctor), Arc::new(arg)),
            c => c
                .fill_hole(&arg)
                .unwrap_or_else(|| Type::App(Arc::new(ctor), Arc::new(arg))),
        }
    }

    /// A bound constructor's application, filled: the constructor
    /// dereferenced through its cell and its hole replaced by `arg`.
    /// `None` while the constructor is an open variable.
    pub(crate) fn app_filled(ctor: &Type, arg: &Type) -> Option<Type> {
        ctor.with_deref(|c| match c {
            None | Some(Type::TVar(_)) => None,
            Some(c) => c.fill_hole(arg),
        })
    }

    /// The other side of a constructor application, dereferenced and
    /// [`Self::decompose`]d: a reference by name, a bare alias through
    /// its expansion.
    #[doc(hidden)]
    pub fn app_split(t: &Type, env: &Env) -> Result<Option<(Type, Type)>> {
        let Some(t) = t.deref_cloned() else { return Ok(None) };
        if let Some(parts) = t.decompose() {
            return Ok(Some(parts));
        }
        match &t {
            Type::Ref(tr) if tr.params.is_empty() => Ok(t.lookup_ref(env)?.decompose()),
            _ => Ok(None),
        }
    }

    /// How a trait signature spells its receiver: `applied` if `self`
    /// occurs as a constructor (`self<'a>`), `bare` if it occurs as a
    /// type. A trait uses one form throughout.
    pub(crate) fn self_shape(&self, applied: &mut bool, bare: &mut bool) {
        ensure_sufficient(|| match self {
            Type::App(c, a) if matches!(&**c, Type::TVar(tv) if &*tv.name == "self") => {
                *applied = true;
                a.self_shape(applied, bare)
            }
            Type::TVar(tv) if &*tv.name == "self" => *bare = true,
            t => t.for_each_child(&mut |c| c.self_shape(applied, bare)),
        })
    }

    /// The number of holes in this type.
    #[doc(hidden)]
    pub fn holes(&self) -> usize {
        ensure_sufficient(|| match self {
            Type::Hole => 1,
            t => {
                let mut n = 0;
                t.for_each_child(&mut |c| n += c.holes());
                n
            }
        })
    }

    /// The name of the element a constructor quantifier `q` is applied
    /// to; a fn type lists it among its quantifiers, so a call copies it.
    pub fn elem_name(q: &str) -> ArcStr {
        format_compact!("{q}#elem").as_str().into()
    }

    /// The type of a parameter whose written type is the trait `tr`:
    /// the fresh bounded quantifier `tv`, applied to a fresh element
    /// when the trait is a constructor trait (`|c: Collection|` ≡
    /// `'c: Collection, c: 'c<'e>`).
    #[doc(hidden)]
    pub fn trait_param(env: &Env, tv: TVar, tr: &TypeRef) -> Type {
        let hole = env
            .trait_of_ref(tr)
            .and_then(|tid| env.trait_def(tid))
            .is_some_and(|d| d.hole);
        if hole {
            Type::App(Arc::new(Type::TVar(tv)), Arc::new(Type::empty_tvar()))
        } else {
            Type::TVar(tv)
        }
    }

    /// Whether `tc` bounds by a constructor trait (`Collection`), whose
    /// implementations are constructors (`Array<'_>`).
    #[doc(hidden)]
    pub fn is_ctor_trait_bound(env: &Env, tc: &Type) -> bool {
        match tc {
            Type::Ref(tr) => env
                .trait_of_ref(tr)
                .and_then(|tid| env.trait_def(tid))
                .is_some_and(|d| d.hole),
            _ => false,
        }
    }

    /// This type with each occurrence of a quantifier `ctors` names
    /// applied to that quantifier's element: a variable bounded by a
    /// constructor trait (`'c: Collection`) stands for a constructor
    /// applied (`'c<'e>`), as a trait-typed parameter does
    /// ([`Self::trait_param`]).
    #[doc(hidden)]
    pub fn apply_ctor_quantifiers(&self, ctors: &[(ArcStr, Type)]) -> Type {
        self.apply_ctor_quantifiers_int(ctors).unwrap_or_else(|| self.clone())
    }

    fn apply_ctor_quantifiers_int(&self, ctors: &[(ArcStr, Type)]) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => {
                ctors.iter().find(|(name, _)| *name == tv.name).map(|(_, elem)| {
                    Type::App(Arc::new(self.clone()), Arc::new(elem.clone()))
                })
            }
            Type::App(c, a) => a
                .apply_ctor_quantifiers_int(ctors)
                .map(|a| Type::App(c.clone(), Arc::new(a))),
            t => t.cow_children(&mut |c| c.apply_ctor_quantifiers_int(ctors)),
        })
    }

    /// This type with its hole replaced by `arg`; `None` if it has no
    /// hole (it is not a constructor).
    pub fn fill_hole(&self, arg: &Type) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::Hole => Some(arg.clone()),
            t => t.cow_children(&mut |c| c.fill_hole(arg)),
        })
    }

    /// The constructor form of this type (last parameter replaced by a
    /// hole) with that parameter; `None` if the outermost form has no
    /// parameters. Syntactic: a reference is taken by name.
    pub fn decompose(&self) -> Option<(Type, Type)> {
        match self {
            Type::Array(t) => Some((Type::Array(Arc::new(Type::Hole)), (**t).clone())),
            Type::List(t) => Some((Type::List(Arc::new(Type::Hole)), (**t).clone())),
            Type::Map(m) => Some((Type::map(m.key.clone(), Type::Hole), m.value.clone())),
            Type::Ref(tr) if !tr.params.is_empty() => {
                let n = tr.params.len() - 1;
                let params = Arc::from_iter(
                    tr.params.iter().take(n).cloned().chain(iter::once(Type::Hole)),
                );
                Some((Type::Ref(Arc::new(tr.with_params(params))), tr.params[n].clone()))
            }
            Type::Abstract { id, params } if !params.is_empty() => {
                let n = params.len() - 1;
                let ps = Arc::from_iter(
                    params.iter().take(n).cloned().chain(iter::once(Type::Hole)),
                );
                Some((Type::Abstract { id: *id, params: ps }, params[n].clone()))
            }
            _ => None,
        }
    }

    /// No TVar anywhere beneath (Ref params, not expansions). A
    /// tvar-free type's identity is stable, so it can key a cache.
    pub fn tvar_free(&self) -> bool {
        ensure_sufficient(|| match self {
            Type::TVar(_) => false,
            t => t
                .try_for_each_child(&mut |c| {
                    if c.tvar_free() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(())
                    }
                })
                .is_continue(),
        })
    }

    /// Every name this written type holds that names nothing in `env`
    /// now ([`TypeRef::names_something`]), into `out`: a name's
    /// parameters and a variable's bounds included, a variable's binding
    /// (inferred, not written) not. A name that does resolve is refused
    /// where it is applied wrongly ([`TypeRef::check_application`]). True
    /// when some name means a trait.
    #[doc(hidden)]
    pub fn unresolved_names(&self, env: &Env, out: &mut Vec<TypeRef>) -> Result<bool> {
        let mut res = Ok(());
        let mut trait_named = false;
        self.for_each_written_name(&mut |tr| {
            if res.is_ok() {
                res = match tr.means(env) {
                    Some(m) => {
                        trait_named |= m == NameMeans::Trait;
                        tr.check_application(env)
                    }
                    None => Ok(out.push(tr.clone())),
                }
            }
        });
        res.map(|()| trait_named)
    }

    /// [`TypeRef::resolve_only_in`] of every name this written type holds,
    /// as [`Self::unresolved_names`] walks them; the first refusal.
    pub fn resolve_names_only_in(&self, env: &Env) -> Result<()> {
        let mut res = Ok(());
        self.for_each_written_name(&mut |tr| {
            if res.is_ok() {
                res = tr.resolve_only_in(env)
            }
        });
        res
    }

    /// Each name this written type holds, under any constructor, a
    /// typedef's arguments and its variables' bounds included, no typedef
    /// looked through.
    #[doc(hidden)]
    pub fn for_each_written_name(&self, f: &mut impl FnMut(&TypeRef)) {
        fn go(t: &Type, seen: &mut AddrSet, f: &mut impl FnMut(&TypeRef)) {
            ensure_sufficient(|| match t {
                Type::Ref(tr) => {
                    f(tr);
                    tr.params.iter().for_each(|p| go(p, seen, f))
                }
                Type::TVar(tv) if seen.insert(tv.cell_addr()) => {
                    tv.cell_above().iter().for_each(|c| go(c, seen, f))
                }
                Type::TVar(_) => (),
                Type::Fn(ft) => ft.for_each_type(&mut |t| go(t, seen, f)),
                t => t.for_each_child(&mut |c| go(c, seen, f)),
            })
        }
        go(self, &mut LPooled::take(), f)
    }

    /// Fill the resolution cell of every `Type::Ref` reachable from
    /// this type against `env`, for a type about to outlive the env
    /// that gives its names meaning. Names not visible are skipped
    /// (they fill at their first in-context lookup) and make the
    /// result false. Recurses through filled snapshot bodies and
    /// through cells' bindings and bounds.
    pub fn seed_refs(&self, env: &Env) -> bool {
        self.seed_refs_seen(env, &mut LPooled::take())
    }

    /// [`Self::seed_refs`] over types that share `seen`, the cells and
    /// nodes already walked.
    #[doc(hidden)]
    pub fn seed_refs_seen(&self, env: &Env, seen: &mut AddrSet) -> bool {
        self.seed_walk(env, seen, true)
    }

    /// [`Self::seed_refs_seen`] but for the bodies of the definitions
    /// the names fill to, which [`Env::seed_typedef_refs`] seeds.
    #[doc(hidden)]
    pub fn seed_names_seen(&self, env: &Env, seen: &mut AddrSet) -> bool {
        self.seed_walk(env, seen, false)
    }

    /// [`Self::seed_names_seen`] for the names whose arguments hold no
    /// type variable alone: such a name means the same wherever it is
    /// read, so filling it here changes nothing a later reader sees.
    #[doc(hidden)]
    pub fn seed_closed_names_seen(&self, env: &Env, seen: &mut AddrSet) {
        self.seed_walk_where(env, seen, false, &|tr| {
            tr.params.iter().all(|p| p.tvar_free())
        });
    }

    fn seed_walk(&self, env: &Env, seen: &mut AddrSet, bodies: bool) -> bool {
        self.seed_walk_where(env, seen, bodies, &|_| true)
    }

    fn seed_walk_where(
        &self,
        env: &Env,
        seen: &mut AddrSet,
        bodies: bool,
        fills: &dyn Fn(&TypeRef) -> bool,
    ) -> bool {
        fn go(
            t: &Type,
            env: &Env,
            seen: &mut AddrSet,
            bodies: bool,
            fills: &dyn Fn(&TypeRef) -> bool,
        ) -> bool {
            ensure_sufficient(|| {
                // CR claude for claude: [risk/low] A node or resolution cell already in
                // `seen` answers true here, so the result is right only for the first
                // walk over a `seen`: a later type sharing a node that holds a name
                // `env` does not see gets true from `Type::seed_refs_seen` or
                // `Type::seed_names_seen`, whose `seen` the caller shares across types.
                // Every caller of those two discards the result today, and
                // `Env::seed_typedef_refs_under`, which sets `seeded` from it, uses
                // `Type::seed_refs` with a set of its own, so nothing is wrong yet; a
                // caller that set a `seeded` flag from the shared form would skip an
                // unseeded definition for good. Return `()` from the two `_seen` forms
                // and keep the bool on `seed_refs` alone. Found by reading; not run.
                // (t-typ-mod-04)
                if let Some(node) = node_addr(t)
                    && !seen.insert(node)
                {
                    return true;
                }
                let mut all = true;
                match t {
                    // Keyed on the cell: with_params clones share it.
                    Type::Ref(tr)
                        if fills(tr) && seen.insert(Arc::as_ptr(&tr.resolved).addr()) =>
                    {
                        match tr.resolve_in(env) {
                            None if tr.resolve_trait_in(env).is_some() => (),
                            None => all = false,
                            Some(_) if !bodies => (),
                            Some(r) => {
                                for (_, c) in r.params.iter() {
                                    if let Some(c) = c {
                                        all &= go(c, env, seen, bodies, fills);
                                    }
                                }
                                all &= go(&r.typ, env, seen, bodies, fills);
                            }
                        }
                    }
                    Type::TVar(tv) => {
                        let cell = tv.cell();
                        if !seen.insert(cell.data_ptr().addr()) {
                            return true;
                        }
                        let (binding, cons, lower) = {
                            let cell = cell.read();
                            (
                                cell.binding.clone(),
                                SmallVec::<[Type; 1]>::from(cell.above.all()),
                                cell.lower.clone(),
                            )
                        };
                        for t in binding.iter().chain(cons.iter()).chain(lower.iter()) {
                            all &= go(t, env, seen, bodies, fills);
                        }
                    }
                    _ => (),
                }
                t.for_each_child(&mut |c| all &= go(c, env, seen, bodies, fills));
                all
            })
        }
        go(self, env, seen, bodies, fills)
    }

    /// Whether this type reaches the definition `def` (a
    /// [`ResolvedRef::def_key`]) again through unions, aliases (expanded
    /// with their params) and bound cells alone, with no constructor
    /// between: compared by the definition, so a renaming import is the
    /// same type. A name not defined yet leaves the answer
    /// [`Unguarded::Unknown`]. The walk ends: a cycle among other
    /// definitions was refused when its last member was defined.
    pub(crate) fn reaches_unguarded(&self, env: &Env, def: usize) -> Unguarded {
        fn go(
            t: &Type,
            env: &Env,
            def: usize,
            seen: &mut SmallVec<[(usize, Arc<[Type]>); 8]>,
        ) -> Unguarded {
            let any = |ts: &mut dyn Iterator<Item = Unguarded>| {
                ts.fold(Unguarded::Guarded, |acc, u| acc.or(u))
            };
            ensure_sufficient(|| match t {
                Type::Set(ts) => any(&mut ts.iter().map(|t| go(t, env, def, seen))),
                Type::TVar(tv) => match tv.binding() {
                    Some(b) => go(&b, env, def, seen),
                    None => Unguarded::Guarded,
                },
                Type::App(c, a) => match Type::app_filled(c, a) {
                    Some(f) => go(&f, env, def, seen),
                    None => Unguarded::Guarded,
                },
                Type::Ref(tr) => {
                    let Some(r) = tr.resolve_pure(env) else { return Unguarded::Unknown };
                    let key = r.def_key();
                    if key == def {
                        return Unguarded::Reaches;
                    }
                    // a definition explored on this walk already folded its
                    // verdict in, and a cycle not through `def` is refused at
                    // its own check
                    if seen.iter().any(|(k, ps)| *k == key && **ps == *tr.params) {
                        return Unguarded::Guarded;
                    }
                    let Some(known) = r.bindings(&tr.params) else {
                        return Unguarded::Guarded;
                    };
                    seen.push((key, tr.params.clone()));
                    go(&r.typ.replace_tvars(&known), env, def, seen)
                }
                _ => Unguarded::Guarded,
            })
        }
        go(self, env, def, &mut SmallVec::new())
    }

    /// The abstract type this names without a lookup: itself, or the
    /// body of the typedef its filled resolution cell holds.
    pub(crate) fn resolved_abstract(&self) -> Option<AbstractId> {
        match self {
            Type::Abstract { id, .. } => Some(*id),
            Type::Ref(tr) => match tr.resolved()?.typ {
                Type::Abstract { id, .. } => Some(id),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn lookup_ref(&self, env: &Env) -> Result<Type> {
        Ok(self
            .lookup_ref_with(env, true)?
            .expect("a committing lookup refuses by error"))
    }

    /// [`Self::lookup_ref`] for a relation's walk. Committing, a violated
    /// parameter bound is an error and an open argument takes the bound
    /// as a conjunct; a probe (`!commit`) binds nothing and answers a
    /// violated bound with `None`.
    #[doc(hidden)]
    pub fn lookup_ref_with(&self, env: &Env, commit: bool) -> Result<Option<Type>> {
        self.lookup_ref_by(env, commit, TypeRef::resolve_in)
    }

    /// [`Self::lookup_ref`] that fills no resolution cell: a name whose
    /// cell is empty resolves in its scope as it stands, so a lookup made
    /// while the statements that may yet declare it compile decides
    /// nothing for later ones.
    #[doc(hidden)]
    pub fn lookup_ref_peek(&self, env: &Env) -> Result<Type> {
        Ok(self
            .lookup_ref_by(env, true, TypeRef::resolve_peek)?
            .expect("a committing lookup refuses by error"))
    }

    /// [`Self::lookup_ref_with`]'s probe that fills no resolution cell,
    /// as [`Self::lookup_ref_peek`], with the key of the definition it
    /// found: a walk that only looks decides no name for a check that may
    /// resolve it in another environment. `None` where it does not
    /// resolve or a parameter bound is violated.
    #[doc(hidden)]
    pub fn lookup_ref_probe(&self, env: &Env) -> Option<(usize, Type)> {
        self.lookup_ref_def(env).map(|(def, t)| (def.def_key(), t))
    }

    /// [`Self::lookup_ref_probe`] with the place the definition was
    /// declared at in place of its key: two cells filled from one
    /// declaration name it alike.
    #[doc(hidden)]
    pub fn lookup_ref_declared(
        &self,
        env: &Env,
    ) -> Option<((ModPath, SourcePosition), Type)> {
        self.lookup_ref_def(env)
            .map(|(def, t)| ((def.canonical_scope.clone(), def.pos), t))
    }

    fn lookup_ref_def(&self, env: &Env) -> Option<(sync::Arc<ResolvedRef>, Type)> {
        let Type::Ref(tr) = self else { return None };
        let def = tr.resolve_peek(env)?;
        let t = self.lookup_ref_by(env, false, TypeRef::resolve_peek).ok()??;
        Some((def, t))
    }

    fn lookup_ref_by(
        &self,
        env: &Env,
        commit: bool,
        resolve: fn(&TypeRef, &Env) -> Option<sync::Arc<ResolvedRef>>,
    ) -> Result<Option<Type>> {
        match self {
            Self::Ref(tr) => {
                let TypeRef { scope, name, params, pos, ori, resolved: _ } = &**tr;
                let resolved = resolve(tr, env).ok_or_else(|| {
                    if gxdbg_typeref() {
                        eprintln!(
                            "TYPEREF-MISS {name} in {scope}; typedef scopes with the name:"
                        );
                        for (s, m) in env.lexical.typedefs.into_iter() {
                            if m.into_iter().any(|(n, _)| name.ends_with(n.as_str())) {
                                eprintln!("  {s}");
                            }
                        }
                    }
                    UnresolvableRef::error(tr, env)
                })?;
                let ResolvedRef {
                    canonical_scope,
                    pos: _,
                    ori: _,
                    params: _,
                    typ: def_typ,
                    regular: _,
                    meets: _,
                } = &*resolved;
                resolved.check_arity(name, params)?;
                if env.ide.is_lsp() {
                    if let (Some(pos), Some(ori)) = (pos, ori) {
                        env.push_type_ref(crate::ide::TypeRefSite {
                            pos: *pos,
                            ori: ori.clone(),
                            scope: scope.clone(),
                            name: name.clone(),
                            canonical_scope: canonical_scope.clone(),
                        });
                    }
                }
                if !tr.hold_bounds_of(env, &resolved, commit)? {
                    return Ok(None);
                }
                let known = resolved.bindings(params).expect("arity checked");
                // a raise extends a chain rather than nesting it, so a chain
                // of a chain is that chain
                if env.is_core_errchain(&resolved)
                    && let [payload] = &params[..]
                    && let Some(error) = payload.chain_error_type(env)?
                {
                    let known = resolved.bindings(&[error]).expect("arity checked");
                    return Ok(Some(def_typ.replace_tvars(&known)));
                }
                Ok(Some(def_typ.replace_tvars(&known)))
            }
            t => Ok(Some(t.clone())),
        }
    }

    /// Core's `ErrChain<payload>`.
    pub fn err_chain(payload: Type) -> Type {
        static CORE: LazyLock<ModPath> = LazyLock::new(|| ModPath::from(["core"]));
        static NAME: LazyLock<ModPath> = LazyLock::new(|| ModPath::from(["ErrChain"]));
        // CR claude for eric: [bug/medium] This reference is resolved by name in
        // whatever env expands it, and a dynamic module's body compiled under
        // `Env::imported_view` has an empty `/core` unless its sig imports it. A body
        // that uses only syntax (`catch(e) select e.0.error { .. }; (x +? 1)?`) fails
        // to load with `undefined type ErrChain in core`, a name it never wrote;
        // `catch(e) "[e]"` loads, and so does the same body once the sig says `use
        // core::ErrChain;`. design/module_system.md says operators and syntax need no
        // import. Either every view keeps core's `ErrChain` and the types it names, as
        // a sig's traits are granted, or this ref carries core's definition in its cell
        // and `Env::is_core_errchain` compares against that instead of the view's
        // `/core`. probe: design/review-2026-11-16/repro/t-typ-mod-errchain-sandbox.gx
        // (t-typ-mod-03)
        Type::Ref(Arc::new(TypeRef::synthetic(
            CORE.clone(),
            NAME.clone(),
            Arc::from_iter([payload]),
        )))
    }

    /// `E` when every value of this type is a link of an error chain,
    /// which a raise extends: core's `ErrChain<E>`, a struct of its field
    /// names that `ErrChain<E>` contains (`E` its `error` field), or a
    /// union of chains (`E` the union of theirs).
    pub fn chain_error_type(&self, env: &Env) -> Result<Option<Type>> {
        thread_local! {
            static JUDGING: RefCell<Vec<Type>> = const { RefCell::new(Vec::new()) };
        }
        struct Judging;
        impl Drop for Judging {
            fn drop(&mut self) {
                JUDGING.with_borrow_mut(|j| j.pop());
            }
        }
        // XCR claude for claude: [bug/high] Whether a raised payload is an error chain is
        // decided here while its variables are open, and the answer changes once an
        // instance decides them. A bare variable is no chain, so in `|x: 'a| { catch(e)
        // r <- e ~ e.0.error; error(x)?; r }` the binder is `ErrChain<'a>` and
        // `e.0.error` is `'a`; called with a chain, the instance's raise extends that
        // chain and `e.0.error` is the chain's own error, handed back as an `ErrChain`.
        // A struct of the four field names over variables is judged a chain by the
        // probe in the `Type::Struct` arm below, and an instance whose fields make it
        // no chain is wrapped whole, so a result typed `string` holds the struct. The
        // check accepts both; the JIT yields the mistyped value and the node-walk logs
        // `a coverage hole in the check` (graphix-fuzz: DIVERGENCE). Needs a rule:
        // refuse a read of a chain whose payload's chain-ness is undecided at the
        // definition, naming the annotation, or decide the wrap from the definition's
        // type for every instance. No pin has the catch inside the generic function
        // (`generic_reraise_keeps_the_chain` catches outside). probe:
        // design/review-2026-11-16/repro/t-typ-mod-generic-chain-payload.gx and
        // design/review-2026-11-16/repro/t-typ-mod-generic-chain-lookalike.gx
        // (t-typ-mod-01)
        // 2026-11-20 claude: Eric ruled (2026-11-20, Q-K): flatten error chains as the
        // run time does: a chain over a chain is that chain, a read of the error of a
        // chain over a still-generic payload inside its definition is refused naming an
        // annotation, the printer prints a chain of a chain as the chain, and no user
        // type may be named ErrChain.
        // 2026-11-20 claude: Fixed on bounds-spike: a .error read or a relation meeting
        // core's ErrChain over a payload not decided yet waits in the fixing loop until
        // the payload is (Solving::chains, Chained); a .error read over ErrChain<'a>, 'a
        // a declared variable whose bounds admit a chain, is refused
        // (Solving::chain_read); a raise of a struct of a chain's four fields whose
        // cause, ori or pos is a variable is refused (bounds::strip,
        // chain_shaped_over_variables). Both probes are refused. Pins:
        // lang::errors::chain_error_over_a_declared_variable_is_refused,
        // chain_error_over_a_variable_no_chain_meets,
        // raise_of_a_chain_shaped_struct_over_variables_is_refused,
        // generic_reraise_keeps_the_chain, generic_reraise_reads_the_first_error,
        // generic_reraise_error_is_not_a_chain.
        if let Type::TVar(tv) = self {
            return match tv.binding() {
                Some(t) => t.chain_error_type(env),
                None => Ok(None),
            };
        }
        // a recursive definition met again inside its own judgment is
        // no chain there
        if JUDGING.with_borrow(|j| j.contains(self)) {
            return Ok(None);
        }
        JUDGING.with_borrow_mut(|j| j.push(self.clone()));
        let _judging = Judging;
        match self {
            Type::Ref(tr) => match tr.resolve_pure(env) {
                Some(r) if env.is_core_errchain(&r) => match &tr.params[..] {
                    [payload] => Ok(Some(
                        payload.chain_error_type(env)?.unwrap_or_else(|| payload.clone()),
                    )),
                    _ => Ok(None),
                },
                _ => match self.lookup_ref_by(env, false, TypeRef::resolve_peek)? {
                    Some(t) => t.chain_error_type(env),
                    None => Ok(None),
                },
            },
            Type::Struct(fields) => {
                const NAMES: [&str; 4] = ["cause", "error", "ori", "pos"];
                if fields.len() != NAMES.len()
                    || !fields.iter().zip(NAMES).all(|((n, _, _), m)| n.as_str() == m)
                {
                    return Ok(None);
                }
                let error = fields[1].1.clone();
                let fits = Type::err_chain(error.clone()).decided_holds(env, self)?;
                Ok(fits.then_some(error))
            }
            Type::Set(ts) if !ts.is_empty() => {
                let mut errors: LPooled<Vec<Type>> = LPooled::take();
                for t in ts.iter() {
                    match t.chain_error_type(env)? {
                        Some(e) => errors.push(e),
                        None => return Ok(None),
                    }
                }
                Ok(Some(Type::Set(Arc::from_iter(errors.drain(..)))))
            }
            _ => Ok(None),
        }
    }

    /// Push a `TypeRefSite` for every `Type::Ref` beneath that carries
    /// a source position. The caller gates on `env.ide.is_lsp()`.
    pub fn record_ide_refs(&self, env: &Env, fallback_scope: &ModPath) {
        ensure_sufficient(|| match self {
            Type::Ref(tr) => {
                if let (Some(pos), Some(ori)) = (tr.pos, &tr.ori) {
                    let canonical_scope = match tr.resolve_pure(env) {
                        Some(r) => r.canonical_scope.clone(),
                        None => fallback_scope.clone(),
                    };
                    env.push_type_ref(crate::ide::TypeRefSite {
                        pos,
                        ori: ori.clone(),
                        scope: tr.scope.clone(),
                        name: tr.name.clone(),
                        canonical_scope,
                    });
                }
                for p in tr.params.iter() {
                    p.record_ide_refs(env, fallback_scope);
                }
            }
            Type::TVar(tv) => {
                if let Some(t) = tv.binding() {
                    t.record_ide_refs(env, fallback_scope);
                }
            }
            t => t.for_each_child(&mut |c| c.record_ide_refs(env, fallback_scope)),
        })
    }

    pub fn boolean() -> Self {
        Self::Primitive(Typ::Bool.into())
    }

    /// A `Bottom` anywhere in the type's own structure (fn signatures
    /// and references are leaves): the diagnostic for `_` written in
    /// type position where a wildcard was meant.
    pub fn has_bottom(&self) -> bool {
        ensure_sufficient(|| {
            self.with_deref(|t| match t {
                Some(Type::Bottom) => true,
                None | Some(Type::Fn(_)) => false,
                Some(t) => t
                    .try_for_each_child(&mut |c| match c.has_bottom() {
                        true => ControlFlow::Break(()),
                        false => ControlFlow::Continue(()),
                    })
                    .is_break(),
            })
        })
    }

    /// Whether this type is a function type through bindings and typedef
    /// names: a formal so typed is a callback formal however it is
    /// written. Only looks: it fills no resolution cell and binds nothing.
    pub fn is_function(&self, env: &Env) -> bool {
        ensure_sufficient(|| match self.deref_cloned() {
            Some(Type::Fn(_)) => true,
            Some(r @ Type::Ref(_)) => {
                r.lookup_ref_probe(env).is_some_and(|(_, t)| t.is_function(env))
            }
            _ => false,
        })
    }

    /// The open members of the union `t` this reaches as it reaches an
    /// open cell, where this is a predicate: a union meets one only where
    /// each member does.
    fn open_members(&self, env: &Env, t: &Type) -> SmallVec<[TVar; 2]> {
        match (self, t.deref_cloned()) {
            (Type::Bound(_), Some(Type::Set(ref ms))) => {
                ms.iter().filter_map(|m| m.open_cell(env)).collect()
            }
            _ => SmallVec::new(),
        }
    }

    /// The open cell this type is: an unbound variable, or one bound to
    /// another type that is, through typedef names. Fills nothing.
    pub(crate) fn open_cell(&self, env: &Env) -> Option<TVar> {
        ensure_sufficient(|| match self {
            Self::TVar(tv) => match tv.binding() {
                None => Some(tv.clone()),
                Some(t) => t.open_cell(env),
            },
            Self::Ref(_) => {
                self.lookup_ref_probe(env).and_then(|(_, t)| t.open_cell(env))
            }
            _ => None,
        })
    }

    /// The dereferenced type, cloned; `None` for an unbound cell.
    pub fn deref_cloned(&self) -> Option<Self> {
        self.with_deref(|t| t.cloned())
    }

    /// `f` over this type with bound cells and filled applications
    /// looked through; `None` for an open cell. `f` runs while the read
    /// guard of every cell on the chain is held: an `f` (or a walk it
    /// starts) that writes one of those cells deadlocks.
    pub fn with_deref<R, F: FnOnce(Option<&Self>) -> R>(&self, f: F) -> R {
        match self {
            // A filled application is its filled type to every walk.
            Self::App(c, a) => match Self::app_filled(c, a) {
                Some(filled) => ensure_sufficient(|| filled.with_deref(f)),
                None => f(Some(self)),
            },
            Self::Hole | Self::Bound(_) => f(Some(self)),
            Self::Bottom
            | Self::Abstract { .. }
            | Self::Any
            | Self::Primitive(_)
            | Self::Fn(_)
            | Self::Set(_)
            | Self::Error(_)
            | Self::Array(_)
            | Self::List(_)
            | Self::ByRef(..)
            | Self::Tuple(_)
            | Self::Struct(_)
            | Self::Variant(_, _, _)
            | Self::Ref(_)
            | Self::Map { .. } => f(Some(self)),
            Self::TVar(tv) => {
                let mut cell = tv.cell();
                loop {
                    let next = match &cell.read().binding {
                        Some(Self::TVar(next)) => next.cell(),
                        _ => break,
                    };
                    cell = next;
                }
                match cell.read().binding.as_ref() {
                    Some(t) => ensure_sufficient(|| t.with_deref(f)),
                    None => f(None),
                }
            }
        }
    }

    /// A predicate conjunct: a bound a type passes or fails, never a type
    /// a cell binds to. Exhaustive, so a new variant is decided here.
    pub fn is_predicate(&self) -> bool {
        match self {
            Type::Bound(_) => true,
            Type::Bottom
            | Type::Any
            | Type::Hole
            | Type::Primitive(_)
            | Type::Ref(_)
            | Type::Fn(_)
            | Type::Set(_)
            | Type::TVar(_)
            | Type::Error(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::ByRef(..)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(..)
            | Type::Map { .. }
            | Type::Abstract { .. }
            | Type::App(..) => false,
        }
    }

    /// A bound's conjunct: a trait or a predicate itself, or a type
    /// holding neither a hole nor a trait, which only a parameter's type
    /// may be.
    pub fn check_bound(&self, env: &Env) -> Result<()> {
        match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => Ok(()),
            t if t.is_predicate() => Ok(()),
            t => t.rewrite_trait_args(env).map(|_| ()),
        }
    }

    /// A trait named as a parameter's type (`fn(s: Read)`) becomes a
    /// fresh bounded quantifier `fn<'s: Read>(s: 's)` named `#s`; a
    /// trait anywhere else is an error. A function type with quantifiers
    /// of its own is refused ([`QUANTIFIED_FN`]): only a declared
    /// signature's top has them ([`Self::rewrite_declared_args`]).
    /// Returns the rewritten type.
    pub fn rewrite_trait_args(&self, env: &Env) -> Result<Type> {
        self.rewrite_written(env, false)
    }

    /// [`Self::rewrite_trait_args`] over a declared signature (an
    /// interface `val`, a trait method), whose own quantifiers at its top
    /// are its scheme's, which each use picks.
    pub fn rewrite_declared_args(&self, env: &Env) -> Result<Type> {
        self.rewrite_written(env, true)
    }

    /// The trait a name of this written type means, outside every
    /// variable's bounds: none where the type was written below the
    /// trait ([`Self::rewrite_trait_args`]).
    pub fn trait_as_type(&self, env: &Env) -> Option<ModPath> {
        ensure_sufficient(|| match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => Some(tr.name.clone()),
            Type::Ref(tr) => tr.params.iter().find_map(|p| p.trait_as_type(env)),
            Type::TVar(_) => None,
            Type::Fn(ft) => {
                let FnType { args, vargs, rtype, throws, .. } = &**ft;
                let parts = args.iter().map(|a| &a.typ).chain(vargs.iter());
                parts.chain([rtype, throws]).find_map(|t| t.trait_as_type(env))
            }
            t => {
                let mut found = None;
                t.for_each_child(&mut |c| {
                    if found.is_none() {
                        found = c.trait_as_type(env)
                    }
                });
                found
            }
        })
    }

    /// Refuse a function type with quantifiers of its own, written or a
    /// trait as a parameter's type, anywhere in a written type that
    /// [`Self::rewrite_trait_args`] does not read (a catch's annotation, a
    /// cast's target, a type test).
    pub fn refuse_quantified_fns(&self, env: &Env) -> Result<()> {
        let mut found = false;
        let _ = self.try_for_each_written_fn(&mut |ft| {
            found = !ft.quantifiers.is_empty()
                || ft.args.iter().any(
                    |a| matches!(&a.typ, Type::Ref(tr) if env.trait_of_ref(tr).is_some()),
                );
            match found {
                true => ControlFlow::Break(()),
                false => ControlFlow::Continue(()),
            }
        });
        match found {
            true => bail!("{QUANTIFIED_FN}"),
            false => Ok(()),
        }
    }

    fn try_for_each_written_fn(
        &self,
        f: &mut impl FnMut(&FnType) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        ensure_sufficient(|| {
            if let Type::Fn(ft) = self {
                f(ft)?;
            }
            self.try_for_each_child(&mut |c| c.try_for_each_written_fn(f))
        })
    }

    fn rewrite_written(&self, env: &Env, scheme: bool) -> Result<Type> {
        if self.holes() > 0 {
            bail!(
                "'_ is the hole of a constructor trait's implementation target \
                 (`impl Collection for Array<'_>`); it is not a type"
            )
        }
        Ok(self.rewrite_trait_args_int(env, scheme)?.unwrap_or_else(|| self.clone()))
    }

    /// The rewrite of what changed; `None` for a type it leaves alone.
    fn rewrite_trait_args_int(&self, env: &Env, scheme: bool) -> Result<Option<Type>> {
        ensure_sufficient(|| self.rewrite_trait_args_inner(env, scheme))
    }

    fn rewrite_trait_args_inner(&self, env: &Env, scheme: bool) -> Result<Option<Type>> {
        match self {
            Type::Ref(tr) if env.trait_of_ref(tr).is_some() => bail!(
                "trait {} used as a type: a trait is a bound — write it as a \
                 parameter's type (`fn(x: {})`) or a quantifier's (`fn<'a: {}>`)",
                tr.name,
                tr.name,
                tr.name
            ),
            Type::Fn(ft) => {
                if !scheme && !ft.quantifiers.is_empty() {
                    bail!("{QUANTIFIED_FN}")
                }
                for (_, c) in ft.constraint_view().iter() {
                    c.check_bound(env)?;
                }
                let mut quantifiers: LPooled<Vec<ArcStr>> =
                    ft.quantifiers.iter().cloned().collect();
                let mut changed = false;
                let mut part = |t: &Type| -> Result<Type> {
                    Ok(match t.rewrite_trait_args_int(env, false)? {
                        Some(r) => {
                            changed = true;
                            r
                        }
                        None => t.clone(),
                    })
                };
                let mut args: LPooled<Vec<FnArgType>> = LPooled::take();
                let mut params_changed = false;
                for (i, a) in ft.args.iter().enumerate() {
                    let typ = match &a.typ {
                        Type::Ref(tr) if env.settled_trait_of_ref(tr)?.is_some() => {
                            let name: ArcStr = match a.name() {
                                Some(n) => format_compact!("#{n}").as_str().into(),
                                None => format_compact!("#arg{i}").as_str().into(),
                            };
                            let tv = TVar::empty_generic(name.clone());
                            tv.add_cell_constraint(a.typ.clone());
                            // a fn type's element is a quantifier, copied per call
                            let t = match &Type::trait_param(env, tv, tr) {
                                Type::App(c, _) => {
                                    let elem = Type::elem_name(&name);
                                    quantifiers.push(elem.clone());
                                    let elem = Type::TVar(TVar::empty_generic(elem));
                                    Type::App(c.clone(), Arc::new(elem))
                                }
                                t => t.clone(),
                            };
                            if !quantifiers.contains(&name) {
                                quantifiers.push(name);
                            }
                            params_changed = true;
                            t
                        }
                        t => part(t)?,
                    };
                    let kind = match &a.kind {
                        FnArgKind::Labeled {
                            name,
                            default: Some(DefaultType::Of(d)),
                        } => FnArgKind::Labeled {
                            name: name.clone(),
                            default: Some(DefaultType::Of(part(d)?)),
                        },
                        kind => kind.clone(),
                    };
                    args.push(FnArgType { kind, typ });
                }
                let vargs = ft.vargs.as_ref().map(|t| part(t)).transpose()?;
                let rtype = part(&ft.rtype)?;
                let throws = part(&ft.throws)?;
                let ctors = ctor_quantifiers(ft, env);
                if !scheme && !quantifiers.is_empty() {
                    bail!("{QUANTIFIED_FN}")
                }
                if !changed && !params_changed && ctors.is_empty() {
                    return Ok(None);
                }
                quantifiers.extend(ctors.iter().map(|(q, _)| Type::elem_name(q)));
                let ft = FnType {
                    args: Arc::from_iter(args.drain(..)),
                    vargs,
                    rtype,
                    throws,
                    explicit_throws: ft.explicit_throws,
                    quantifiers: Arc::from_iter(quantifiers.drain(..)),
                    lambda_ids: ft.lambda_ids.clone(),
                };
                if ctors.is_empty() {
                    return Ok(Some(Type::Fn(Arc::new(ft))));
                }
                let ft =
                    ft.cow_walk(|t| t.apply_ctor_quantifiers_int(&ctors)).unwrap_or(ft);
                Ok(Some(Type::Fn(Arc::new(ft))))
            }
            t => {
                let mut err = None;
                let r =
                    t.cow_children(&mut |c| match c.rewrite_trait_args_int(env, false) {
                        Ok(r) => r,
                        Err(e) => {
                            err = Some(e);
                            None
                        }
                    });
                match err {
                    Some(e) => Err(e),
                    None => Ok(r),
                }
            }
        }
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Type {
        let mut copies: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.scope_refs_with(scope, &mut copies)
    }

    /// [`Self::scope_refs`] over `copies`, shared with the other parts of
    /// one type.
    #[doc(hidden)]
    pub fn scope_refs_with(
        &self,
        scope: &ModPath,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Type {
        let mut copies = Copies::Cells(copies);
        self.scope_refs_int(Some(scope), &mut copies).unwrap_or_else(|| self.clone())
    }

    /// [`Self::scope_refs`] where same-named variables are one variable:
    /// a name `names` holds is that variable, and any other is copied once
    /// and goes into `names`. A nested fn type's own quantifiers are its own.
    pub fn scope_refs_named(
        &self,
        scope: &ModPath,
        names: &mut AHashMap<ArcStr, TVar>,
    ) -> Type {
        let mut copies = Copies::Names(names, LPooled::take());
        self.scope_refs_int(Some(scope), &mut copies).unwrap_or_else(|| self.clone())
    }

    /// [`Self::scope_refs_named`] where the type's names stay as they
    /// were scoped.
    pub fn named_in(&self, names: &mut AHashMap<ArcStr, TVar>) -> Type {
        let mut copies = Copies::Names(names, LPooled::take());
        self.scope_refs_int(None, &mut copies).unwrap_or_else(|| self.clone())
    }

    /// `None` when no `Ref` or `TVar` is beneath; a `Ref` takes `scope`
    /// where there is one.
    pub(super) fn scope_refs_int(
        &self,
        scope: Option<&ModPath>,
        copies: &mut Copies,
    ) -> Option<Type> {
        #[cfg(test)]
        fntyp::TVAR_WALK_STEPS.with(|c| c.set(c.get() + 1));
        ensure_sufficient(|| self.scope_refs_int_inner(scope, copies))
    }

    fn scope_refs_int_inner(
        &self,
        scope: Option<&ModPath>,
        copies: &mut Copies,
    ) -> Option<Type> {
        match self {
            Type::TVar(tv) => {
                if let Some(copy) = copies.get(tv) {
                    // a bound written at any occurrence of a name is the
                    // variable's
                    if let Copies::Names(_, met) = copies
                        && met.insert(tv.cell_addr())
                        && !copy.same_cell(tv)
                    {
                        let cons: SmallVec<[Type; 1]> =
                            tv.cell().read().above.declared().into();
                        for c in cons.iter() {
                            let c = c
                                .scope_refs_int(scope, copies)
                                .unwrap_or_else(|| c.clone());
                            copy.add_cell_constraint(c)
                        }
                    }
                    return Some(Type::TVar(copy));
                }
                let (bound, cons) = {
                    let cell = tv.cell();
                    let cell = cell.read();
                    (
                        cell.binding.clone(),
                        SmallVec::<[Type; 1]>::from(cell.above.declared()),
                    )
                };
                let fresh = tv.fresh_copy();
                copies.insert(tv, fresh.clone());
                if let Copies::Names(_, met) = copies {
                    met.insert(tv.cell_addr());
                }
                if let Some(typ) = bound {
                    fresh.bind(typ.scope_refs_int(scope, copies).unwrap_or(typ))
                }
                // The re-minted cell keeps the conjunction (an annotated
                // bound lives only there).
                for c in cons.iter() {
                    let c = c.scope_refs_int(scope, copies).unwrap_or_else(|| c.clone());
                    fresh.add_cell_constraint(c);
                }
                Some(Type::TVar(fresh))
            }
            Type::Ref(tr) => {
                let params = Arc::from_iter(tr.params.iter().map(|t| {
                    t.scope_refs_int(scope, copies).unwrap_or_else(|| t.clone())
                }));
                let scope = scope.unwrap_or(&tr.scope).clone();
                Some(Type::Ref(Arc::new(tr.with_scope(scope, params))))
            }
            Type::Fn(ft) => Some(Type::Fn(Arc::new(ft.scope_refs_in(scope, copies)))),
            t => t.cow_children(&mut |c| c.scope_refs_int(scope, copies)),
        }
    }

    /// A unification view of this type with every `Any` leaf replaced
    /// by a throwaway TVar, sharing the existing cells so bindings made
    /// through the view land in the original. Select's arm typecheck
    /// unifies pattern predicates through it: `T.contains(Any)` is
    /// false, which would stop the walk at a `_` slot before later
    /// slots' binds narrowed.
    pub fn any_as_tvar(&self) -> Type {
        self.any_as_tvar_int().unwrap_or_else(|| self.clone())
    }

    /// `None` when no `Any` is beneath. `Ref`/`Abstract` params and
    /// `Fn` signatures are leaves: the select arm walk never descends
    /// them.
    fn any_as_tvar_int(&self) -> Option<Type> {
        ensure_sufficient(|| match self {
            Type::Any => Some(Type::empty_tvar()),
            Type::Ref(_) | Type::Fn(_) | Type::Abstract { .. } => None,
            t => t.cow_children(&mut |c| c.any_as_tvar_int()),
        })
    }
}

/// An id that is the low 64 bits of a uuid: a fixed eight bytes, where a
/// varint would take ten.
macro_rules! uuid_id_codec {
    ($id:ty) => {
        impl PackTrait for $id {
            fn encoded_len(&self) -> usize {
                self.inner().encoded_len()
            }

            fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
                self.inner().encode(buf)
            }

            fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
                Ok(<$id>::from_inner(u64::decode(buf)?))
            }
        }
    };
}

uuid_id_codec!(AbstractId);
uuid_id_codec!(TraitId);

/// The declared quantifiers of `ft` bounded by constructor traits alone,
/// each with an element of its own ([`Type::apply_ctor_quantifiers`]).
fn ctor_quantifiers(ft: &FnType, env: &Env) -> LPooled<Vec<(ArcStr, Type)>> {
    let mut out: LPooled<Vec<(ArcStr, Type)>> = LPooled::take();
    if ft.quantifiers.is_empty() {
        return out;
    }
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut named);
    for q in ft.quantifiers.iter() {
        let Some(tv) = named.get(q) else { continue };
        let cons = tv.cell_above();
        if !tv.is_bound()
            && !cons.is_empty()
            && cons.iter().all(|c| Type::is_ctor_trait_bound(env, c))
        {
            out.push((q.clone(), Type::TVar(TVar::empty_generic(Type::elem_name(q)))));
        }
    }
    out
}

/// What a written name means ([`TypeRef::means`]).
#[doc(hidden)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum NameMeans {
    Type,
    Trait,
}
