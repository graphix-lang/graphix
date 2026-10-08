use super::{
    Nop, WakeBit,
    callsite::{CallSite, Feeds, QuietAtRoot, publish_production},
    collection::CollectionIntrinsic,
    compiler::compile,
    pattern::StructPatternNode,
    produce_constant,
};
use crate::SourcePosition;
use crate::{
    Apply, ApplyView, BindId, BindMode, CFlag, CompileCtx, ExecCtx, InitFn, LambdaId,
    LambdaInstanceId, Node, NodeView, Refs, Rt, Scope, TagValue, Update, UserEvent,
    dbgenv,
    effects::{EffectKind, RecursionKind},
    env::{Bind, Env},
    expr::{self, Arg, ArgKind, At, Expr, ExprId, LambdaBody, Origin},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, call_result_needs_value_widening, widen_result_to_value,
        },
    },
    image::{
        self, ImageBuf, ImageDecoder, lexical_decode, lexical_encode,
        nodes::{NodeTag, decode_node, put_tag},
    },
    profile::{self, Phase},
    stack::ensure_sufficient,
    typ::{
        FnArgKind, FnArgType, FnType, ResolvedRef, TVar, Type,
        tvar::{AtLevel, InTask, Level, RigidGate},
    },
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use netidx_value::Value;
use nohash::IntSet;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    fmt,
    hash::Hash,
    mem,
    sync::{
        Arc as SArc, OnceLock, Weak,
        atomic::{AtomicBool, Ordering},
    },
};
use triomphe::Arc;

/// What a definition's check settled, by the expression id of each node
/// of its body: an instance takes its types from here instead of checking
/// again. An id two nodes share has no row.
#[derive(Debug, Default)]
pub struct DefTable {
    types: AHashMap<ExprId, Type>,
    ftypes: AHashMap<ExprId, FnType>,
    /// What a node derives beside its own type, in the order the node
    /// lists it: a select's arm predicates and pattern binds, a catch's
    /// error binds.
    aux: AHashMap<ExprId, Box<[Type]>>,
    /// The table of each lambda literal of the body, by its id.
    lambdas: AHashMap<ExprId, SArc<DefTable>>,
    /// The typedefs the rows name, owned here: one the body declares
    /// is in the env only while the body compiles.
    typedefs: Vec<SArc<ResolvedRef>>,
}

/// One instance's cell map, shared with the lambdas its body defines:
/// their own instances read the enclosing body's cells through it.
type Known = SArc<Mutex<AHashMap<usize, TVar>>>;

/// What a definition's instances substitute: its table and, for a lambda
/// defined in an instance, the enclosing instance's map, through which
/// the table's cells of the enclosing definition are renamed first.
#[derive(Debug, Clone)]
pub struct Tables {
    table: SArc<DefTable>,
    outer: Option<Known>,
}

impl DefTable {
    /// Every type the table's rows hold but the signatures'.
    fn rows(&self) -> impl Iterator<Item = &Type> {
        self.types.values().chain(self.aux.values().flat_map(|ts| ts.iter()))
    }

    /// Bind to ⊥ every open cell of the table's that `owner`'s gate
    /// created, but its signature (`exempt`) does not reach: nothing
    /// bounded it, and an instance reads the table as the check left it.
    pub(crate) fn settle_open(
        &self,
        env: &Env,
        owner: LambdaId,
        exempt: &AHashSet<usize>,
    ) -> Result<()> {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        for t in self.rows() {
            crate::typ::settle::position_cells(t, &mut cells);
        }
        for ft in self.ftypes.values() {
            ft.for_each_part(&mut |t, constraint| {
                if !constraint {
                    crate::typ::settle::position_cells(t, &mut cells)
                }
            });
        }
        for (addr, tv) in cells.iter() {
            if !exempt.contains(addr)
                && !tv.is_bound()
                && !tv.requires_concrete()
                && tv.level().owner() == Some(owner)
            {
                tv.settle_or_bottom(env)?;
            }
        }
        Ok(())
    }

    /// The check of `body`, whose typedefs are in `env`: every type
    /// reference a row holds is resolved there now, so no instance looks
    /// a name up by a scope only this check had (an image relocates the
    /// ids a block's scope is named by, never the names). The body's own
    /// binds are in `checked`, the check's environment.
    fn record<R: Rt, E: UserEvent>(body: &Node<R, E>, env: &Env, checked: &Env) -> Self {
        let mut table = Self::default();
        let mut shared: LPooled<AHashSet<ExprId>> = LPooled::take();
        fusion::for_each_node(body, &mut |n| {
            let id = n.spec().id;
            if table.types.insert(id, n.typ().clone()).is_some() {
                shared.insert(id);
            }
            match n.view() {
                NodeView::CallSite(cs) => {
                    if let Some(ft) = cs.ftype.as_ref() {
                        table.ftypes.insert(id, (**ft).clone());
                    }
                }
                NodeView::Lambda(l) => {
                    let def = l.def_value().downcast_ref::<LambdaDef<R, E>>();
                    if let Some(t) = def.and_then(|d| d.table.get()) {
                        table.lambdas.insert(id, t.table.clone());
                    }
                }
                NodeView::Select(s) => {
                    table.aux.insert(id, s.aux_types(checked));
                }
                NodeView::Catch(c) => {
                    table.aux.insert(id, c.aux_types(checked));
                }
                _ => (),
            }
        });
        for id in shared.drain() {
            table.types.remove(&id);
            table.ftypes.remove(&id);
            table.lambdas.remove(&id);
            table.aux.remove(&id);
        }
        let mut seen: LPooled<IntSet<usize>> = LPooled::take();
        for t in table.rows() {
            t.seed_refs_seen(env, &mut seen);
        }
        for ft in table.ftypes.values() {
            ft.for_each_part(&mut |t, _| {
                t.seed_refs_seen(env, &mut seen);
            });
        }
        table.own_typedefs();
        table
    }

    fn own_typedefs(&mut self) {
        let mut cells: LPooled<AHashSet<usize>> = LPooled::take();
        let mut defs: LPooled<AHashMap<usize, SArc<ResolvedRef>>> = LPooled::take();
        for t in self.rows() {
            t.named_defs(&mut cells, &mut defs);
        }
        for ft in self.ftypes.values() {
            ft.for_each_part(&mut |t, _| t.named_defs(&mut cells, &mut defs));
        }
        self.typedefs.clear();
        self.typedefs.extend(defs.drain().map(|(_, r)| r));
    }

    /// The table an image carries for `tables`: a lambda an instance
    /// defined has its rows renamed through the enclosing instance's map
    /// already, as its instances read them.
    fn imaged(tables: &Tables) -> SArc<DefTable> {
        match &tables.outer {
            None => tables.table.clone(),
            Some(outer) => SArc::new(tables.table.renamed(&outer.lock())),
        }
    }

    /// This table with an enclosing definition's cells renamed through
    /// `known`, its lambdas' tables included, which name them too.
    fn renamed(&self, known: &AHashMap<usize, TVar>) -> DefTable {
        ensure_sufficient(|| {
            let mut table = DefTable {
                types: self
                    .types
                    .iter()
                    .map(|(id, t)| (*id, t.rename_with(known)))
                    .collect(),
                ftypes: self
                    .ftypes
                    .iter()
                    .map(|(id, ft)| (*id, rename_fn(ft, known)))
                    .collect(),
                aux: self
                    .aux
                    .iter()
                    .map(|(id, ts)| {
                        (*id, ts.iter().map(|t| t.rename_with(known)).collect())
                    })
                    .collect(),
                lambdas: self
                    .lambdas
                    .iter()
                    .map(|(id, t)| (*id, SArc::new(t.renamed(known))))
                    .collect(),
                typedefs: vec![],
            };
            table.own_typedefs();
            table
        })
    }

    fn image_encode(
        table: &SArc<DefTable>,
        buf: &mut impl BufMut,
    ) -> Result<(), PackError> {
        let key = SArc::as_ptr(table) as usize;
        image::object_encode(
            &key,
            |k| (*k, table.clone()),
            |e| &mut e.ext::<image::Compiled>().def_tables,
            buf,
            |buf| {
                encode_varint(table.types.len() as u64, buf);
                for (id, t) in table.types.iter() {
                    id.encode(buf)?;
                    t.encode(buf)?;
                }
                encode_varint(table.ftypes.len() as u64, buf);
                for (id, ft) in table.ftypes.iter() {
                    id.encode(buf)?;
                    ft.encode(buf)?;
                }
                encode_varint(table.aux.len() as u64, buf);
                for (id, ts) in table.aux.iter() {
                    id.encode(buf)?;
                    encode_varint(ts.len() as u64, buf);
                    for t in ts.iter() {
                        t.encode(buf)?;
                    }
                }
                encode_varint(table.lambdas.len() as u64, buf);
                for (id, l) in table.lambdas.iter() {
                    id.encode(buf)?;
                    Self::image_encode(l, buf)?;
                }
                encode_varint(table.typedefs.len() as u64, buf);
                for r in table.typedefs.iter() {
                    image::resolved_encode(r, buf)?;
                }
                Ok(())
            },
        )
    }

    fn image_decode(buf: &mut impl Buf) -> Result<SArc<DefTable>, PackError> {
        fn len(buf: &mut &[u8]) -> Result<usize, PackError> {
            let n = decode_varint(buf)? as usize;
            if n > buf.remaining() {
                return Err(PackError::BufferShort);
            }
            Ok(n)
        }
        image::foreign_decode(
            buf,
            |sub| {
                let n = len(sub)?;
                let mut types = AHashMap::with_capacity(n);
                for _ in 0..n {
                    types.insert(ExprId::decode(sub)?, Type::decode(sub)?);
                }
                let n = len(sub)?;
                let mut ftypes = AHashMap::with_capacity(n);
                for _ in 0..n {
                    ftypes.insert(ExprId::decode(sub)?, FnType::decode(sub)?);
                }
                let n = len(sub)?;
                let mut aux = AHashMap::with_capacity(n);
                for _ in 0..n {
                    let id = ExprId::decode(sub)?;
                    let m = len(sub)?;
                    let ts =
                        (0..m).map(|_| Type::decode(sub)).collect::<Result<_, _>>()?;
                    aux.insert(id, ts);
                }
                let n = len(sub)?;
                let mut lambdas = AHashMap::with_capacity(n);
                for _ in 0..n {
                    lambdas.insert(ExprId::decode(sub)?, Self::image_decode(sub)?);
                }
                let n = len(sub)?;
                let typedefs = (0..n)
                    .map(|_| image::resolved_decode(sub))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(SArc::new(DefTable { types, ftypes, aux, lambdas, typedefs }))
            },
            |b| Self::image_decode(b),
        )
    }
}

fn rename_fn(ft: &FnType, known: &AHashMap<usize, TVar>) -> FnType {
    match &Type::Fn(Arc::new(ft.clone())).rename_with(known) {
        Type::Fn(ft) => (**ft).clone(),
        _ => unreachable!("a renamed function type is a function type"),
    }
}

/// A definition's check tables as an image writes them, `None` for a
/// definition with none.
pub(crate) fn tables_encode(
    tables: Option<&Tables>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    match tables {
        None => Ok(buf.put_u8(0)),
        Some(t) => {
            buf.put_u8(1);
            DefTable::image_encode(&DefTable::imaged(t), buf)
        }
    }
}

/// A restored definition's tables are read from the image by its first
/// instance.
pub(crate) fn tables_decode(buf: &mut impl Buf) -> Result<DefTables, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        0 => Ok(DefTables::default()),
        1 => {
            let ord = image::object_ref(buf)?;
            let decoder =
                image::decoding(|d| d.shared()).ok_or(PackError::InvalidFormat)?;
            Ok(DefTables { tables: OnceLock::new(), imaged: Some((decoder, ord)) })
        }
        _ => Err(PackError::UnknownTag),
    }
}

/// A definition's check tables: set by its check, or, for a definition
/// restored from an image, read from it when an instance first asks.
#[derive(Debug, Default)]
pub struct DefTables {
    tables: OnceLock<Tables>,
    imaged: Option<(Weak<Mutex<ImageDecoder>>, u32)>,
}

impl DefTables {
    pub(crate) fn get(&self) -> Option<&Tables> {
        if let Some(t) = self.tables.get() {
            return Some(t);
        }
        let (decoder, ord) = self.imaged.as_ref()?;
        let decoder = decoder.upgrade()?;
        let mut dec = decoder.lock();
        // restored cells belong to no compile task, whichever reads them
        let _task = InTask::enter(0);
        let table = image::DecodeImage::with(&mut dec, || {
            image::foreign_at(*ord, |b| DefTable::image_decode(b))
        });
        match table {
            Ok(table) => self.set(Tables { table, outer: None }),
            Err(e) => log::error!("a restored definition's tables did not decode: {e:?}"),
        }
        self.tables.get()
    }

    pub(crate) fn set(&self, tables: Tables) {
        let _ = self.tables.set(tables);
    }
}

/// An instance's view of its definition's [`DefTable`]: each row
/// instantiated through one cell map, which starts from the definition's
/// signature unified with the instance's.
pub struct InstanceTypes {
    tables: Tables,
    known: Known,
    open: IntSet<LambdaId>,
}

impl InstanceTypes {
    /// `None` when the instance's signature does not hold the
    /// definition's: a type-system bug the instance's own check reports.
    fn new<R: Rt, E: UserEvent>(
        ctx: &CompileCtx<R, E>,
        tables: Tables,
        def: &FnType,
        instance: &FnType,
    ) -> Option<Self> {
        let open = ctx.rec_defs.clone();
        let mut known = AHashMap::default();
        let def = def.instantiate_with(&mut known, &open);
        instance.check_contains(&ctx.env, &def).ok()?;
        Some(Self { tables, known: SArc::new(Mutex::new(known)), open })
    }

    /// A row read from this definition's check, renamed through the
    /// enclosing instance's map, then instantiated through this one's.
    fn instance_of(&self, t: &Type) -> Type {
        let t = match &self.tables.outer {
            Some(outer) => t.rename_with(&outer.lock()),
            None => t.clone(),
        };
        t.instantiate_with(&mut self.known.lock(), &self.open)
    }

    /// The type the definition's check settled for the node at `id`.
    pub(crate) fn typ(&mut self, id: ExprId) -> Option<Type> {
        let t = self.tables.table.types.get(&id)?;
        Some(self.instance_of(t))
    }

    /// Bind `typ`, a node's own type, to the row for `id`
    /// ([`Type::take_row`]); false when there is none, or when an open
    /// cell of `typ` found no part of it, and the caller derives the
    /// type. A node of an instance can be born knowing a type its
    /// definition's check widened (`d.domain` born `string` where the
    /// check unified it with a formal's `[Array<i64>, string]`): the
    /// instance's knowledge stands.
    pub(crate) fn settle(&mut self, id: ExprId, typ: &Type) -> bool {
        self.typ(id).is_some_and(|t| typ.take_row(&t))
    }

    /// What the node at `id` derived beside its own type, in its order
    /// ([`DefTable::aux`]).
    pub(crate) fn aux(&mut self, id: ExprId) -> Option<SmallVec<[Type; 8]>> {
        let ts = self.tables.table.aux.get(&id)?;
        Some(ts.iter().map(|t| self.instance_of(t)).collect())
    }

    /// The signature the definition's check settled for the call at `id`.
    pub(crate) fn ftype(&mut self, id: ExprId) -> Option<FnType> {
        let ft = self.tables.table.ftypes.get(&id)?;
        match self.instance_of(&Type::Fn(Arc::new(ft.clone()))) {
            Type::Fn(ref ft) => Some((**ft).clone()),
            _ => None,
        }
    }

    /// The tables of the lambda literal at `id` of this instance's body.
    pub(crate) fn lambda(&self, id: ExprId) -> Option<Tables> {
        // renamed through every enclosing map, outermost first: this
        // instance's map is keyed by cells the enclosing one renamed
        let table = self.tables.table.lambdas.get(&id)?;
        let table = match &self.tables.outer {
            None => table.clone(),
            Some(outer) => SArc::new(table.renamed(&outer.lock())),
        };
        Some(Tables { table, outer: Some(self.known.clone()) })
    }
}

pub struct LambdaDef<R: Rt, E: UserEvent> {
    pub id: LambdaId,
    pub env: Env,
    pub scope: Scope,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R, E>,
    /// A builtin definition's check `Apply`, built by the definition gate
    /// ([`Self::builtin_check`]).
    pub check: Mutex<Option<Box<dyn Apply<R, E>>>>,
    /// What the definition's check settled, for its instances; shared
    /// with `init`, which hands it to each instance.
    pub table: SArc<DefTables>,
    /// Sync/async effect, computed by `analysis::infer_effects`: the
    /// body's own, joined with every instance's an analysis reached (a
    /// resolved callback's instance joins its HOF instance's; a call
    /// through an unresolved parameter is async), so it only grows.
    pub intrinsic_effect: Mutex<EffectKind>,
    /// The body holds no per-activation state: every builtin it reaches
    /// is `Effect::Stateless`, no `<-` targets its own binding, every
    /// callee is stateless. A tail loop reuses one activation only then.
    pub stateless: AtomicBool,
    /// How this lambda recurses, computed by `analysis::analyze`. The
    /// operational tail-loop gate is `GXLambda::tail_loop`, not this.
    pub recursion: Mutex<RecursionKind>,
    /// The lambda expression this def was compiled from; stable across
    /// instance re-compiles, which is what [`crate::FnArgIdentity`] keys on.
    pub source: ExprId,
    pub origin: DefOrigin,
    /// The definition's level: the cells it owns are its signature's
    /// and its body's.
    pub level: Level,
}

/// Where a definition came from: a lambda expression, whose `init` is
/// a function of these and which an image carries as data, or Rust
/// code building an `Apply` at runtime, which no image can carry.
pub enum DefOrigin {
    Source { body: DefBody, flags: BitFlags<CFlag>, spec: Expr },
    Runtime,
}

/// What a source definition's instances run.
#[derive(Debug, Clone)]
pub enum DefBody {
    Expr(Expr),
    /// A traversal the compiler builds (`'array_map`, ..).
    Collection(CollectionIntrinsic),
    /// A Rust builtin, by its registered name.
    BuiltIn(ArcStr),
}

impl DefBody {
    pub(crate) fn of(body: &LambdaBody) -> Self {
        match body {
            LambdaBody::Expr(e) => DefBody::Expr(e.clone()),
            LambdaBody::Builtin(name) => match CollectionIntrinsic::from_name(name) {
                Some(intrinsic) => DefBody::Collection(intrinsic),
                None => DefBody::BuiltIn(name.clone()),
            },
        }
    }
}

impl<R: Rt, E: UserEvent> LambdaDef<R, E> {
    /// The check `Apply` of a builtin definition; `None` for any other.
    /// It holds `None` until the definition gate builds it, and for a
    /// definition restored from an image until its first call site
    /// rebuilds it.
    pub(crate) fn builtin_check(&self) -> Option<&Mutex<Option<Box<dyn Apply<R, E>>>>> {
        match &self.origin {
            DefOrigin::Source { body: DefBody::BuiltIn(_), .. } => Some(&self.check),
            DefOrigin::Source { .. } | DefOrigin::Runtime => None,
        }
    }
}

impl<R: Rt, E: UserEvent> fmt::Debug for LambdaDef<R, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.origin {
            DefOrigin::Source { spec, .. } => write!(f, "{spec}"),
            DefOrigin::Runtime => write!(f, "{}", self.typ),
        }
    }
}

impl<R: Rt, E: UserEvent> PartialEq for LambdaDef<R, E> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<R: Rt, E: UserEvent> Eq for LambdaDef<R, E> {}

// CR claude for eric: [bug] A function value orders by its LambdaId here and a
// reference by its BindId (`Value::U64`, bind.rs:1282), but both ids are now minted in
// parallel: in the per-statement compile tasks (`typecheck1_statements`,
// node/mod.rs:1089), in slot builds (`build_fresh`, collection.rs:927) and in forked
// branches. So `array::sort`, `<` and map key order over functions, references or
// values holding them change from run to run in the default configuration, even with
// GRAPHIX_PAR=off (RAYON_NUM_THREADS=1 makes them stable), and the forked node-walk
// diverges from the serial one. design/parallel_eval.md §8 still lists both sites as to
// fix. graphix-fuzz check misses the compile-task case because it drops a serial run
// that disagrees with itself as nondeterminism. probe:
// design/review-2026-10-05/repro/x-diff-par-02.gx (x-diff-par-02)
// 2026-10-08 claude: the reference half is gone: `Ordered` refuses a
// reference, so the probe's second sort is refused. Functions still order by
// LambdaId, and `array::sort` over four closures prints a different order per run.
// Needs a ruling: (a) `Ordered` refuses a function type as it does a reference
// (== and != keep comparing by identity, which is deterministic); an `Any` holding
// functions would still order by id at run time; or (b) ids minted in a compile
// task are relocated to program order at its join, which also fixes the watch/db
// handle orders §8 lists. I recommend (a): a function has no order a program
// could mean.
impl<R: Rt, E: UserEvent> PartialOrd for LambdaDef<R, E> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<R: Rt, E: UserEvent> Ord for LambdaDef<R, E> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<R: Rt, E: UserEvent> Hash for LambdaDef<R, E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<R: Rt, E: UserEvent> Pack for LambdaDef<R, E> {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(&self, _buf: &mut impl BufMut) -> Result<(), PackError> {
        Err(PackError::Application(0))
    }

    fn decode(_buf: &mut impl Buf) -> Result<Self, PackError> {
        Err(PackError::Application(0))
    }
}

/// A call-site instance of a Graphix lambda, produced by
/// [`LambdaDef::init`] when a `CallSite` resolves to it (lazily at
/// runtime or statically in `typecheck1`). Fusion reaches the body
/// through [`ApplyView::Lambda`].
#[derive(Debug)]
pub struct GXLambda<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by the next update
    slept: WakeBit,
    id: LambdaId,
    instance_id: LambdaInstanceId,
    args: Box<[StructPatternNode]>,
    body: Node<R, E>,
    typ: Arc<FnType>,
    /// `true` iff this lambda is pure, self-tail-recursive and has
    /// loop-able formals; set by `analysis::analyze`. The JIT emits a
    /// native loop; the node-walk dispatches every call.
    tail_loop: AtomicBool,
    self_recursive: AtomicBool,
    self_bind: Mutex<Option<BindId>>,
    /// The dispatch's return slot, lent to the owning `CallSite`.
    resident: TagValue,
    /// `true` until the first dispatch, which seeds the fresh formal
    /// ids' value channel from the args' quiet productions; true in an
    /// image, which is written before any cycle.
    first_dispatch: bool,
    /// The def-side lexical env the body was compiled under. The body
    /// typechecks under it too: the caller's env, which drives the
    /// checks, may lack the defining module's private typedefs.
    env: Env,
    typing: Typing,
}

/// How an instance's check types its body.
#[derive(Debug)]
pub(crate) enum Typing {
    /// It is its definition's check, which checks the body.
    Definition,
    /// From the definition's check, substituted by the instance's
    /// signature: the definition's tables and signature.
    Substituted(SArc<DefTables>, Arc<FnType>),
    /// Restored from an image, written after its check.
    Restored,
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    /// The definition's id, shared by every instance of it.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    pub fn instance_id(&self) -> LambdaInstanceId {
        self.instance_id
    }

    /// The compiled body.
    pub fn body(&self) -> &Node<R, E> {
        &self.body
    }

    pub(crate) fn inline_callback_body(&self) -> Option<&Node<R, E>> {
        match self.body.view() {
            NodeView::MapQ(map) => map.callback_body(),
            NodeView::FoldQ(fold) => fold.callback_body(),
            _ => None,
        }
    }

    /// Argument-binding patterns, parallel to `self.typ().args`.
    pub fn args(&self) -> &[StructPatternNode] {
        &self.args
    }

    /// This instance's resolved `FnType` (same as `Apply::typ()`).
    pub fn typ(&self) -> &Arc<FnType> {
        &self.typ
    }

    /// The tail-loop gate (see the `tail_loop` field).
    pub fn tail_loop(&self) -> bool {
        self.tail_loop.load(Ordering::Relaxed)
    }

    /// Set the tail-loop gate; `&self` so analysis can mark through a shared `&Node`.
    pub fn set_tail_loop(&self, v: bool) {
        self.tail_loop.store(v, Ordering::Relaxed)
    }

    pub fn self_recursive(&self) -> bool {
        self.self_recursive.load(Ordering::Relaxed)
    }

    pub fn set_self_recursive(&self, recursive: bool) {
        self.self_recursive.store(recursive, Ordering::Relaxed)
    }

    pub fn self_bind(&self) -> Option<BindId> {
        *self.self_bind.lock()
    }

    pub fn set_self_bind(&self, bind: Option<BindId>) {
        *self.self_bind.lock() = bind;
    }
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    /// The types an instance takes from its definition's check; `None`
    /// for the definition's own check, which checks the body. An
    /// instance its definition's check cannot type is a compiler bug.
    fn instance_types(&self, ctx: &CompileCtx<R, E>) -> Result<Option<InstanceTypes>> {
        let (table, def) = match &self.typing {
            Typing::Definition => return Ok(None),
            Typing::Restored => {
                bail!("a restored instance of {:?} is checked again", self.id)
            }
            Typing::Substituted(table, def) => (table, def),
        };
        if dbgenv::graphix_no_subst() {
            return Ok(None);
        }
        let Some(tables) = table.get() else {
            bail!(
                "an instance of {:?}, whose definition's check recorded no types",
                self.id
            )
        };
        match InstanceTypes::new(ctx, tables.clone(), def, &self.typ) {
            Some(types) => Ok(Some(types)),
            None => bail!("an instance at {} of a definition typed {def}", self.typ),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for GXLambda<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.id.encode(buf)?;
        self.instance_id.encode(buf)?;
        image::slice_encode(&self.args, buf)?;
        self.typ.encode(buf)?;
        self.body.image_encode(buf)?;
        self.tail_loop.load(Ordering::Relaxed).encode(buf)?;
        self.self_recursive.load(Ordering::Relaxed).encode(buf)?;
        self.self_bind.lock().encode(buf)?;
        lexical_encode(&self.env, buf)
    }

    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::Lambda(self)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = self.slept.take();
        let first = mem::replace(&mut self.first_dispatch, false);
        // the formals' value channel seeds from a quiet arg production on
        // the first dispatch and after a wake
        let root = if first || woke { QuietAtRoot::Stand } else { QuietAtRoot::Skip };
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            let tv = arg.update(ctx);
            publish_production(ctx, Feeds::Pattern(pat), tv, false, root);
        }
        // an interrupted dispatch is not a bottom: it rides its last result
        if ctx.control.interrupted() {
            return self.resident.ride();
        }
        // a collection intrinsic is part of its call site
        let flags = match self.body.view() {
            NodeView::MapQ(_) | NodeView::FoldQ(_) => ctx.fork,
            _ => ctx.fork.body(),
        };
        let res = ctx.with_fork_flags(flags, |ctx| self.body.update(ctx).clone());
        self.resident.set(res)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        let mut p = profile::phase(Phase::InstanceCheck);
        profile::instance(&mut p, self.instance_id, self.id, self.body.spec());
        elab_audit::enter();
        let res = (|| {
            // an instance's formals and return are its site's, which the
            // definition's check covers
            let types = self.instance_types(ctx)?;
            for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter())
            {
                wrap!(arg, arg.typecheck0(ctx))?;
                if types.is_none() {
                    wrap!(arg, typ.check_contains_rigid(&ctx.env, &arg.typ()))?;
                }
            }
            let env = self.env.clone();
            ctx.with_restored(env, |ctx| match types {
                Some(mut types) => {
                    wrap!(self.body, self.body.typecheck0_instance(ctx, &mut types))
                }
                None => {
                    wrap!(self.body, self.body.typecheck0(ctx))?;
                    wrap!(
                        self.body,
                        self.typ.rtype.check_contains_rigid(&ctx.env, &self.body.typ())
                    )
                }
            })
        })();
        elab_audit::leave(ctx.def_gate_depth, "typecheck0", self.body.spec(), &res);
        res?;
        profile::instance_signature(self.instance_id, &self.typ, || None);
        Ok(())
    }

    /// Drives the body's `typecheck1`; the driving `CallSite::typecheck1`
    /// already walked the args.
    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        let _elaboration = profile::elaboration(self.instance_id);
        elab_audit::enter();
        let env = self.env.clone();
        let res =
            ctx.with_restored(env, |ctx| wrap!(self.body, self.body.typecheck1(ctx)));
        elab_audit::leave(ctx.def_gate_depth, "typecheck1", self.body.spec(), &res);
        res
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        let res = match self.body.view() {
            NodeView::MapQ(map) => map.emit_clif_call(callsite, cx)?,
            NodeView::FoldQ(fold) => fold.emit_clif_call(callsite, cx)?,
            _ => None,
        };
        // the loop emits the lambda's own return shape; a callsite widened
        // to a union must hand its consumers a Value pair
        match res {
            Some(cv)
                if call_result_needs_value_widening(callsite.typ(), &self.typ.rtype) =>
            {
                Ok(Some(widen_result_to_value(cx, &self.typ.rtype, cv)?))
            }
            res => Ok(res),
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        if dbgenv::gxdbg_instance_fusion() {
            let before = ctx.fusion.stats.failed.len();
            let fused_before = ctx.fusion.stats.fused;
            let r = fusion::fuse(&mut self.body, ctx);
            eprintln!(
                "INSTANCE-FUSION GXLambda::fuse id={:?} fused_delta={} new_failures:",
                self.id,
                ctx.fusion.stats.fused - fused_before
            );
            for failure in &ctx.fusion.stats.failed[before..] {
                eprintln!("  INSTANCE-FUSION-FAIL {:?}: {}", failure.id, failure.reason);
            }
            return r;
        }
        fusion::fuse(&mut self.body, ctx)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        for pat in &self.args {
            pat.ids(&mut |id| {
                refs.bound.insert(id);
            })
        }
        self.body.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.body.delete(ctx);
        for n in &self.args {
            n.ids(&mut |id| {
                ctx.fn_forward_resolutions.remove(&id);
            });
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        // a recursion shrinking one level does not shrink the external
        // calls it made
        super::deselecting_arm(false, || self.body.sleep(ctx));
    }
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    pub(super) fn new(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        id: LambdaId,
        typ: Arc<FnType>,
        typing: Typing,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        tid: ExprId,
        body: Expr,
    ) -> Result<Self> {
        let origin = body.ori.clone();
        let build = |ctx: &mut CompileCtx<R, E>, _: &[StructPatternNode]| {
            compile(ctx, flags, body, scope, tid)
        };
        Self::new_with_body(ctx, id, typ, typing, argspec, args, scope, origin, build)
    }

    pub(super) fn new_collection(
        ctx: &mut CompileCtx<R, E>,
        id: LambdaId,
        typ: Arc<FnType>,
        typing: Typing,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        tid: ExprId,
        spec: Expr,
        intrinsic: CollectionIntrinsic,
    ) -> Result<Self> {
        let origin = spec.ori.clone();
        Self::new_with_body(
            ctx,
            id,
            typ.clone(),
            typing,
            argspec,
            args,
            scope,
            origin,
            |ctx, argpats| intrinsic.build(ctx, spec, scope, tid, &typ, argpats),
        )
    }

    fn new_with_body(
        ctx: &mut CompileCtx<R, E>,
        id: LambdaId,
        typ: Arc<FnType>,
        typing: Typing,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        origin: Arc<Origin>,
        build_body: impl FnOnce(
            &mut CompileCtx<R, E>,
            &[StructPatternNode],
        ) -> Result<Node<R, E>>,
    ) -> Result<Self> {
        if args.len() != argspec.len() {
            bail!("arity mismatch, expected {} arguments", argspec.len())
        }
        // a narrower `typ` would truncate the zip below and silently drop
        // parameters
        if argspec.len() != typ.args.len() {
            bail!(
                "instance signature has {} parameters, the definition has {}",
                typ.args.len(),
                argspec.len()
            )
        }
        // the instance's env is read for type names alone: the binds from
        // before the formals are the definition's, shared by every instance
        let binds = ctx.env.binds.clone();
        let mut argpats: LPooled<Vec<StructPatternNode>> = LPooled::take();
        for (a, atyp) in argspec.iter().zip(typ.args.iter()) {
            let pattern = StructPatternNode::compile(
                ctx,
                &atyp.typ,
                &a.pattern,
                scope,
                a.pos.0,
                origin.clone(),
            )?;
            if pattern.is_refutable() {
                bail!(
                    "refutable patterns are not allowed in lambda arguments {}",
                    a.pattern
                )
            }
            argpats.push(pattern);
        }
        let mut p = profile::phase(Phase::InstanceGraph);
        let body = build_body(ctx, &argpats)?;
        let instance_id = LambdaInstanceId::new();
        profile::instance(&mut p, instance_id, id, body.spec());
        drop(p);
        Ok(Self {
            slept: WakeBit::default(),
            id,
            instance_id,
            args: Box::from_iter(argpats.drain(..)),
            typ,
            body,
            tail_loop: AtomicBool::new(false),
            self_recursive: AtomicBool::new(false),
            self_bind: Mutex::new(None),
            resident: TagValue::phantom(),
            first_dispatch: true,
            env: Env { binds, ..ctx.env.lexical() },
            typing,
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let id = LambdaId::decode(buf)?;
        let instance_id = LambdaInstanceId::decode(buf)?;
        let args = Vec::<StructPatternNode>::decode(buf)?.into_boxed_slice();
        let typ: Arc<FnType> = Pack::decode(buf)?;
        let body = decode_node(ctx, buf)?;
        let tail_loop = bool::decode(buf)?;
        let self_recursive = bool::decode(buf)?;
        let self_bind = Option::<BindId>::decode(buf)?;
        let env = lexical_decode(buf)?;
        Ok(Self {
            slept: WakeBit::default(),
            id,
            instance_id,
            args,
            body,
            typ,
            tail_loop: AtomicBool::new(tail_loop),
            self_recursive: AtomicBool::new(self_recursive),
            self_bind: Mutex::new(self_bind),
            resident: TagValue::phantom(),
            first_dispatch: true,
            env,
            typing: Typing::Restored,
        })
    }
}

#[derive(Debug)]
pub(crate) struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    name: ArcStr,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
}

impl<R: Rt, E: UserEvent> BuiltInLambda<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let typ = Arc::new(FnType::decode(buf)?);
        let name = ArcStr::decode(buf)?;
        let decode = ctx.builtin_decoder(&name).ok_or_else(|| {
            log::warn!("the image names an unregistered builtin {name}");
            PackError::InvalidFormat
        })?;
        let apply = decode(ctx, from, buf)?;
        Ok(Self { typ, name, apply })
    }
}

/// Stands in for a builtin this binary does not have, under an IDE
/// check: a package under development declares builtins only its own
/// build registers. Like any builtin it is typed by its declared
/// signature alone; it never produces, and the check never runs it.
#[derive(Debug)]
struct UnknownBuiltIn(TagValue);

impl UnknownBuiltIn {
    fn init<R: Rt, E: UserEvent>(
        _: &mut CompileCtx<R, E>,
        _: &FnType,
        _: Option<&FnType>,
        _: &Scope,
        _: &[Node<R, E>],
        _: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self(TagValue::phantom())))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for UnknownBuiltIn {
    fn update(&mut self, _: &mut ExecCtx<'_, R, E>, _: &mut [Node<R, E>]) -> &TagValue {
        &self.0
    }

    fn image_encode(&self, _: &mut ImageBuf) -> Result<(), PackError> {
        Err(PackError::Application(crate::image::NOT_IMAGED))
    }

    fn sleep(&mut self, _: &mut ExecCtx<'_, R, E>) {}
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.typ.encode(buf)?;
        self.name.encode(buf)?;
        self.apply.image_encode(buf)
    }

    /// Fusion sees the wrapped builtin's own view, named.
    fn view(&self) -> ApplyView<'_, R, E> {
        match self.apply.view() {
            ApplyView::BuiltIn(_) => ApplyView::BuiltIn(&self.name),
            v => v,
        }
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        // the trait default's `Ok(None)` would silently de-fuse every builtin
        self.apply.emit_clif(callsite, cx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.apply.fuse(ctx)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        self.apply.update(ctx, from)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        if args.len() < self.typ.args.len()
            || (args.len() > self.typ.args.len() && self.typ.vargs.is_none())
        {
            let vargs = if self.typ.vargs.is_some() { "at least " } else { "" };
            bail!(
                "expected {}{} arguments got {}",
                vargs,
                self.typ.args.len(),
                args.len()
            )
        }
        for i in 0..args.len() {
            wrap!(args[i], args[i].typecheck0(ctx))?;
            let atyp = if i < self.typ.args.len() {
                &self.typ.args[i].typ
            } else {
                self.typ.vargs.as_ref().unwrap()
            };
            wrap!(args[i], atyp.check_contains_rigid(&ctx.env, &args[i].typ()))?
        }
        self.apply.typecheck0(ctx, args)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        args: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.apply.typecheck1(ctx, args, resolved)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.apply.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.apply.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.apply.sleep(ctx);
    }
}

#[derive(Debug)]
pub struct Lambda {
    spec: Expr,
    def: Value,
    typ: Type,
    resident: TagValue,
}

impl Lambda {
    /// The definition's `LambdaId`.
    pub fn lambda_id<R: Rt, E: UserEvent>(&self) -> Option<LambdaId> {
        self.def.downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
    }

    /// The wrapped `LambdaDef` `Value`, which this node emits at init.
    pub fn def_value(&self) -> &Value {
        &self.def
    }

    /// The literal's source identity (`LambdaDef::source`).
    pub fn source_id(&self) -> ExprId {
        self.spec.id
    }
}

/// The `init` of a definition: how a call site builds an instance from
/// the source body (or a builtin) in the definition's environment and
/// its `fn` block under `scope`, the definition's own. A function of
/// its data, so an image can rebuild it.
pub(crate) fn make_init<R: Rt, E: UserEvent>(
    id: LambdaId,
    flags: BitFlags<CFlag>,
    def_env: Env,
    scope: &Scope,
    def_typ: Arc<FnType>,
    def_argspec: Arc<[Arg]>,
    def_spec: Expr,
    body: DefBody,
    table: SArc<DefTables>,
) -> InitFn<R, E> {
    let def_scope = scope.append_block("fn", id.inner());
    SArc::new(move |scope, ctx, args, mode, tid| {
        // the definition's names, the call site's handlers
        let scope =
            Scope { dynamic: scope.dynamic.clone(), lexical: def_scope.lexical.clone() };
        ctx.with_restored(def_env.clone(), |ctx| match &body {
            DefBody::Expr(body) => {
                instantiate(ctx, mode, &def_typ, &table, |ctx, typ, typing| {
                    let argspec = def_argspec.clone();
                    GXLambda::new(
                        ctx,
                        flags,
                        id,
                        typ,
                        typing,
                        argspec,
                        args,
                        &scope,
                        tid,
                        body.clone(),
                    )
                })
            }
            DefBody::Collection(intrinsic) => {
                instantiate(ctx, mode, &def_typ, &table, |ctx, typ, typing| {
                    GXLambda::new_collection(
                        ctx,
                        id,
                        typ,
                        typing,
                        def_argspec.clone(),
                        args,
                        &scope,
                        tid,
                        def_spec.clone(),
                        *intrinsic,
                    )
                })
            }
            DefBody::BuiltIn(name) => {
                let init = match ctx.registry.builtins.get(&**name).map(|b| b.init) {
                    Some(init) => init,
                    None if ctx.env.ide.is_lsp() => UnknownBuiltIn::init as _,
                    None => bail!("unknown builtin function {name}"),
                };
                let typ = instance_type(ctx, mode, &def_typ)?;
                let resolved = match mode {
                    BindMode::Definition => None,
                    BindMode::Static { .. } | BindMode::Dynamic(_) => Some(&*typ),
                };
                let apply = init(ctx, &def_typ, resolved, &scope, args, tid)?;
                Ok(Box::new(BuiltInLambda { typ, name: name.clone(), apply }) as Box<_>)
            }
        })
    })
}

/// Do `a` and `b` list the same parameters, kind for kind?
pub(crate) fn same_parameters(a: &FnType, b: &FnType) -> bool {
    a.args.len() == b.args.len()
        && a.args.iter().zip(b.args.iter()).all(|(a, b)| a.kind == b.kind)
}

/// Build an instance at the signature `mode` names ([`instance_type`]).
fn instantiate<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    mode: BindMode<'_>,
    def_typ: &Arc<FnType>,
    table: &SArc<DefTables>,
    build: impl FnOnce(&mut CompileCtx<R, E>, Arc<FnType>, Typing) -> Result<GXLambda<R, E>>,
) -> Result<Box<dyn Apply<R, E>>> {
    let typing = match mode {
        BindMode::Definition => Typing::Definition,
        BindMode::Static { .. } | BindMode::Dynamic(_) => {
            Typing::Substituted(table.clone(), def_typ.clone())
        }
    };
    let typ = instance_type(ctx, mode, def_typ)?;
    Ok(Box::new(build(ctx, typ, typing)?))
}

/// The signature an instance is built at: the site's, unless a dynamic
/// bind's runtime callee lists other parameters than the site's view (a
/// label the site omits, defaulted): then a call's copy of the
/// definition's, fitted to the site's view as a call fits it.
fn instance_type<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    mode: BindMode<'_>,
    def_typ: &Arc<FnType>,
) -> Result<Arc<FnType>> {
    Ok(match mode {
        BindMode::Static { instance } => Arc::new(instance.clone()),
        BindMode::Dynamic(r) if same_parameters(r, def_typ) => Arc::new(r.clone()),
        BindMode::Dynamic(r) => {
            let copy = def_typ.instantiate(&ctx.rec_defs);
            r.check_contains(&ctx.env, &copy)?;
            Arc::new(copy)
        }
        BindMode::Definition => def_typ.clone(),
    })
}

impl Lambda {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        l: &expr::LambdaExpr,
        _top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let mut s: LPooled<Vec<&ArcStr>> = LPooled::take();
        for a in l.args.iter() {
            a.pattern.with_names(&mut |n| s.push(n));
        }
        let len = s.len();
        s.sort();
        s.dedup();
        if len != s.len() {
            bail!("arguments must have unique names");
        }
        let id = LambdaId::new();
        let level = Level::definition(id);
        let _level = AtLevel::enter(level);
        // a quantifier bounded by constructor traits alone stands for a
        // constructor applied: each occurrence is `'c<'c#elem>`
        let mut ctors: LPooled<Vec<(ArcStr, Type)>> = LPooled::take();
        for (tv, _) in l.constraints.iter() {
            let bounds = || l.constraints.iter().filter(|(t, _)| t.name == tv.name);
            if ctors.iter().any(|(n, _)| *n == tv.name)
                || !bounds().all(|(_, tc)| {
                    Type::is_ctor_trait_bound(&ctx.env, &tc.scope_refs(&scope.lexical))
                })
            {
                continue;
            }
            let elem =
                TVar::empty_named(format_compact!("{}#elem", tv.name).as_str().into());
            ctors.push((tv.name.clone(), Type::TVar(elem)));
        }
        // a written type, as the definition's signature holds it
        let written = |env: &Env, t: &Type| -> Result<Type> {
            Ok(t.scope_refs(&scope.lexical)
                .rewrite_trait_args(env)?
                .apply_ctor_quantifiers(&ctors))
        };
        let vargs = match l.vargs.as_ref() {
            None => None,
            Some(None) => Some(None),
            Some(Some(t)) => Some(Some(written(&ctx.env, t)?)),
        };
        let rtype = l.rtype.as_ref().map(|t| written(&ctx.env, t)).transpose()?;
        let throws = l.throws.as_ref().map(|t| written(&ctx.env, t)).transpose()?;
        // a trait as a parameter's type is a fresh bounded quantifier
        // (`|s: Read|` ≡ `'s: Read |s: 's|`), joined to the declared ones
        // so the def gate holds it rigid
        let mut trait_quantifiers: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        let mut argspec: LPooled<Vec<Arg>> = LPooled::take();
        for (i, a) in l.args.iter().enumerate() {
            let constraint = match &a.constraint {
                None => None,
                Some(typ) => match &typ.scope_refs(&scope.lexical) {
                    t @ Type::Ref(tr) if ctx.env.trait_of_ref(tr).is_some() => {
                        let name: ArcStr = match a.pattern.single_bind() {
                            Some(n) => format_compact!("#{n}").as_str().into(),
                            None => format_compact!("#arg{i}").as_str().into(),
                        };
                        let tv = TVar::empty_named(name);
                        trait_quantifiers.push((tv.clone(), t.clone()));
                        Some(Type::trait_param(&ctx.env, tv, tr))
                    }
                    _ => Some(written(&ctx.env, typ)?),
                },
            };
            argspec.push(Arg {
                kind: a.kind.clone(),
                pattern: a.pattern.clone(),
                constraint,
                pos: a.pos,
            });
        }
        let argspec = Arc::from_iter(argspec.drain(..));
        // One scoped var per quantifier: a `'a: A + B` bound is a pair
        // per conjunct over one var.
        let mut scoped: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut constraints: LPooled<Vec<_>> = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                let tv = scoped
                    .entry(tv.name.clone())
                    .or_insert_with(|| tv.scope_refs(&scope.lexical))
                    .clone();
                let tc = tc.scope_refs(&scope.lexical);
                tc.check_bound(&ctx.env)?;
                Ok((tv, tc))
            })
            .collect::<Result<_>>()?;
        constraints.extend(trait_quantifiers.drain(..));
        let body = DefBody::of(&l.body);
        let builtin = match &l.body {
            LambdaBody::Expr(_) => None,
            LambdaBody::Builtin(name) => Some(name),
        };
        if let DefBody::BuiltIn(builtin) = &body
            && ctx.registry.builtins.get(builtin.as_str()).is_none()
        {
            if !ctx.env.ide.is_lsp() {
                bail!("unknown builtin function {builtin}")
            }
            // the `'name` that ends the lambda's text
            let end = spec.end.0;
            let len = builtin.chars().count() as i32 + 1;
            let pos = SourcePosition { column: (end.column - len).max(1), ..end };
            let pos = if end == expr::WrittenAt::NOWHERE.0 { spec.pos } else { pos };
            let msg = format_args!(
                "unknown builtin function {builtin}: this graphix was not built \
                 with it, so calls are checked against its signature only"
            );
            ctx.env.warn(flags, &spec, pos, end, msg)?;
        }
        if let Some(builtin) = builtin {
            if flags.contains(CFlag::NoBuiltins) {
                bail!("defining builtins is not allowed in this context")
            }
            for a in argspec.iter() {
                if a.constraint.is_none() {
                    bail!(
                        "builtin function {builtin} requires all arguments to have type annotations"
                    )
                }
            }
            if rtype.is_none() {
                bail!("builtin function {builtin} requires a return type annotation")
            }
        }
        let typ = {
            let args = Arc::from_iter(argspec.iter().map(|a| {
                let kind = match (&a.kind, a.pattern.single_bind()) {
                    (ArgKind::Positional, name) => {
                        FnArgKind::Positional { name: name.cloned() }
                    }
                    (_, None) => FnArgKind::Positional { name: None },
                    (kind, Some(name)) => FnArgKind::Labeled {
                        name: name.clone(),
                        has_default: kind.default().is_some(),
                    },
                };
                let typ = match a.constraint.as_ref() {
                    Some(t) => t.clone(),
                    None => Type::empty_tvar(),
                };
                FnArgType { kind, typ }
            }));
            let vargs = match vargs {
                Some(Some(t)) => Some(t.clone()),
                Some(None) => Some(Type::empty_tvar()),
                None => None,
            };
            let rtype = rtype.clone().unwrap_or_else(|| Type::empty_tvar());
            let explicit_throws = throws.is_some();
            let throws = throws.clone().unwrap_or_else(|| Type::empty_tvar());
            let ft = FnType {
                args,
                vargs,
                rtype,
                throws,
                explicit_throws,
                ..FnType::default()
            };
            Arc::new(ft.declaring(&constraints))
        };
        typ.lambda_ids.set_id(id);
        typ.claim(level);
        let table = SArc::new(DefTables::default());
        let init = make_init(
            id,
            flags,
            ctx.env.lexical(),
            scope,
            typ.clone(),
            argspec.clone(),
            spec.clone(),
            body.clone(),
            table.clone(),
        );
        let (intrinsic_effect, stateless) = match &body {
            DefBody::Expr(_) | DefBody::Collection(_) => (EffectKind::Sync, true),
            DefBody::BuiltIn(name) => {
                let effect = ctx.builtin_effect(name);
                (effect.kind(), effect.is_stateless())
            }
        };
        // No signature ref seeding here: the module tree is mid-registration
        // and a name's final target may not be registered yet. Cells fill
        // at typecheck.
        let def = ctx.registry.lambdawrap.wrap(LambdaDef {
            id,
            typ: typ.clone(),
            env: ctx.env.lexical(),
            argspec,
            init,
            scope: scope.clone(),
            check: Mutex::new(None),
            table,
            intrinsic_effect: Mutex::new(intrinsic_effect),
            stateless: AtomicBool::new(stateless),
            recursion: Mutex::new(RecursionKind::NotRecursive),
            source: spec.id,
            origin: DefOrigin::Source { body, flags, spec: spec.clone() },
            level,
        });
        ctx.lambda_defs.insert(id, def.clone());
        Ok(Node::new(Self {
            spec,
            def: def.clone(),
            typ: Type::Fn(typ),
            // a lambda literal is a constant: present from birth (see Constant)
            resident: TagValue::stale(def),
        }))
    }
}

impl Lambda {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let id = LambdaId::decode(buf)?;
        let typ = Type::decode(buf)?;
        let def = ctx.lambda_defs.get(&id).ok_or(PackError::InvalidFormat)?.clone();
        Ok(Node::new(Self { spec, typ, resident: TagValue::stale(def.clone()), def }))
    }
}

/// A definition's gate: its body checks once, over a `Nop` per declared
/// argument, raising to a faux catch that collects its throws. Its
/// declared tvars are rigid (the body must be well-typed for any 'a;
/// anonymous '_N inference cells stay bindable) and a self-call knots to
/// its own cells (`ExecCtx::rec_defs`). Every path leaves by `close`.
struct DefGate<R: Rt, E: UserEvent> {
    def: LambdaId,
    depth: u32,
    _at: AtLevel,
    sig: Arc<FnType>,
    faux_id: BindId,
    args: LPooled<Vec<Node<R, E>>>,
    scope: Scope,
    rigid: LPooled<Vec<RigidGate>>,
}

impl<R: Rt, E: UserEvent> DefGate<R, E> {
    fn open(ctx: &mut CompileCtx<R, E>, def: &LambdaDef<R, E>) -> Self {
        let at = AtLevel::enter(def.level);
        let args = def.typ.args.iter().map(|at| Nop::new(at.typ.clone())).collect();
        let faux_id = BindId::new();
        ctx.env.by_id.insert(
            faux_id,
            Arc::new(Bind {
                doc: None,
                export: false,
                id: faux_id,
                name: "faux".into(),
                scope: def.scope.lexical.clone(),
                typ: Type::empty_tvar(),
                pos: SourcePosition::default(),
                ori: Arc::new(Origin::default()),
                facet: None,
            }),
        );
        let scope = def.scope.with_catch((faux_id, ExprId::new()), false);
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        def.typ.collect_tvars(&mut named);
        named.retain(|name, _| !name.starts_with('_'));
        let rigid = named.values().map(|tv| tv.open_rigid()).collect();
        ctx.rec_defs.insert(def.id);
        ctx.def_gate_depth += 1;
        ctx.pending_settles.push(Vec::new());
        Self {
            def: def.id,
            depth: def.level.depth(),
            _at: at,
            sig: def.typ.clone(),
            faux_id,
            args,
            scope,
            rigid,
        }
    }

    /// The error type the body raised to the gate's catch.
    /// What the body raised: ⊥ when nothing joined the faux catch; an
    /// open callback's `throws 'e` stays the cell.
    fn thrown(&self, ctx: &CompileCtx<R, E>) -> Type {
        match ctx.env.by_id.get(&self.faux_id).map(|b| &b.typ) {
            Some(Type::TVar(tv)) => tv.binding().unwrap_or(Type::Bottom),
            Some(t) => t.clone(),
            None => Type::Bottom,
        }
    }

    /// The body's sites settle with the enclosing statement, all but the
    /// cells this signature reaches: those stay open, generalized.
    fn close(
        mut self,
        ctx: &mut CompileCtx<R, E>,
        table: Option<(SArc<DefTable>, &Expr)>,
    ) {
        let mut frame = ctx.pending_settles.pop().expect("gate settle frame");
        ctx.def_gate_depth -= 1;
        self.sig.generalize(self.depth);
        for s in frame.iter_mut() {
            if let crate::PendingSettle::Site { sigs, .. } = s {
                sigs.push(self.sig.clone());
            }
        }
        if let Some((table, spec)) = table {
            let mut sig: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            self.sig.reached_cells(&mut sig);
            frame.push(crate::PendingSettle::Body {
                table,
                owner: self.def,
                exempt: sig.keys().copied().collect(),
                spec: Arc::new(spec.clone()),
            });
        }
        ctx.pending_settles.last_mut().expect("root settle frame").extend(frame);
        ctx.rec_defs.remove(&self.def);
        ctx.env.by_id.remove(&self.faux_id);
        self.rigid.clear();
    }
}

/// A builtin's check `Apply` for a site whose definition holds none
/// (restored from an image, or taken by a concurrent site): the builtin
/// over a private copy of its checked signature, so no site writes the
/// definition's cells.
pub(crate) fn build_builtin_check<R: Rt, E: UserEvent>(
    def: &LambdaDef<R, E>,
    ctx: &mut CompileCtx<R, E>,
) -> Result<Box<dyn Apply<R, E>>> {
    let sig = def.typ.reset_tvars();
    let mut args: LPooled<Vec<Node<R, E>>> =
        sig.args.iter().map(|at| Nop::new(at.typ.clone())).collect();
    (def.init)(&def.scope, ctx, &mut args, BindMode::Dynamic(&sig), ExprId::new())
        .and_then(|mut f| f.typecheck0(ctx, &mut args).map(|()| f))
}

/// The definition's check of its labeled defaults, under the gate:
/// each default compiles in the def's scope and must fit its parameter.
/// Against a declared tvar it must fit the tvar's constraints, not the
/// variable, since a default is allowed to instantiate the variable at
/// a site that omits the argument (`CallSite::check_omitted_defaults`).
fn check_defaults<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    def: &LambdaDef<R, E>,
    scope: &Scope,
) -> Result<()> {
    let flags = match &def.origin {
        DefOrigin::Source { flags, .. } => {
            let mut flags = *flags;
            flags.remove(CFlag::WarnUnhandled);
            flags
        }
        DefOrigin::Runtime => return Ok(()),
    };
    for (arg, at) in def.argspec.iter().zip(def.typ.args.iter()) {
        let Some(expr) = arg.kind.default() else { continue };
        let mut node = ctx.with_restored(def.env.clone(), |ctx| {
            compile(ctx, flags, expr.clone(), scope, ExprId::new())
        })?;
        let res = node.typecheck0(ctx).and_then(|()| {
            let typ = node.typ().clone();
            match &at.typ {
                // CR claude for claude: [bug] Each conjunct is committed against the
                // default's type, so an open cell there is bound to the whole bound:
                // `Number ⊇ 'k` binds the cell to Number, and the next conjunct,
                // Singleton, then refuses it. `let scale = |k| { let mul = |#by = k, x|
                // x * by; mul(3) }; scale(2)` is refused at `k` with "Singleton does
                // not contain Number", while the same helper reading `k` in its body
                // runs and prints 6. The cell that gets bound belongs to the
                // environment: after `let d = never(); let f = 'a: Number |#x: 'a = d,
                // y: 'a| [x, y]; d <- 2`, `d` stays Number, so `d + 1` is refused, and
                // so is every call that omits `#x` (`f(3)`). This arm also matches
                // inferred parameter cells (`by` is not a declared tvar), and a
                // declared variable nested in the parameter type gets no conjunct
                // treatment at all (`|#x: Array<'a> = [1], y: 'a| -> 'a y` is refused
                // at the definition). Narrowing the default's open cells by the
                // conjuncts (`TVar::narrow_cell`, as `op::constrain_operand` does)
                // would check them without deciding the cell. probe:
                // design/review-2026-10-05/repro/x-typecheck-generics-F11.gx
                // (x-typecheck-generics-F11)
                // 2026-10-08 claude: a parameter that is a declared or inferred
                // variable now narrows the default's cells by each conjunct
                // (op::constrain_operand): `scale` and the `d` case check. Open: a
                // declared variable nested in the parameter type (`|#x: Array<'a> =
                // [1], y: 'a|`) is still held rigid against the default.
                // each conjunct narrows the default's open cells, which
                // may be the environment's, rather than deciding them
                Type::TVar(tv) if !tv.is_bound() => tv
                    .cell_constraints()
                    .iter()
                    .try_for_each(|c| super::op::constrain_operand(&ctx.env, c, &typ)),
                t => t.check_contains(&ctx.env, &typ),
            }
        });
        let res = res.at(&node.spec());
        ctx.discard(node);
        res?;
    }
    Ok(())
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Lambda, buf);
        self.spec.encode(buf)?;
        self.lambda_id::<R, E>().ok_or(PackError::InvalidFormat)?.encode(buf)?;
        self.typ.encode(buf)
    }

    /// A lambda literal is a constant.
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        produce_constant(ctx.event, &mut self.resident, || self.def.clone())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // a retained def keeps its `LambdaIds` link-graph nodes alive, and
        // `typecheck1`'s `ids()` walks grow with them
        if let Some(def) = self.def.downcast_ref::<LambdaDef<R, E>>() {
            ctx.lambda_defs.remove(&def.id);
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R, E>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
        let spec = &self.spec;
        crate::defer_unresolved_names(ctx, &Type::Fn(def.typ.clone()), spec);
        // Every arg, defaulted labeled ones included, checks as a Nop of
        // its declared type; the defaults themselves are checked after
        // the body (`check_defaults`), and again per omitting call site
        // (`CallSite::check_omitted_defaults`), where one may narrow that
        // site's cells.
        let mut gate = DefGate::open(ctx, def);
        let res = (def.init)(
            &gate.scope,
            ctx,
            &mut gate.args,
            BindMode::Definition,
            ExprId::new(),
        )
        .at(spec);
        let res = res.and_then(|mut f| {
            let ftyp = f.typ().clone();
            // fn-typed params knot like self-calls: a call to `f` unifies
            // against the param's own declared cells (`ExecCtx::def_gate_params`)
            let mut param_knot: LPooled<Vec<BindId>> = LPooled::take();
            if let ApplyView::Lambda(g) = f.view() {
                for (pat, at) in g.args().iter().zip(ftyp.args.iter()) {
                    if at.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
                        && let Some(id) = pat.single_bind_id()
                    {
                        ctx.def_gate_params.insert(id);
                        param_knot.push(id);
                    }
                }
            }
            let res = f.typecheck0(ctx, &mut gate.args).at(spec);
            for id in param_knot.drain(..) {
                ctx.def_gate_params.remove(&id);
            }
            if res.is_ok()
                && let ApplyView::Lambda(g) = f.view()
            {
                let table = SArc::new(DefTable::record(&g.body, &g.env, &ctx.env));
                def.table.set(Tables { table, outer: None });
            }
            // a builtin's check `Apply` is retained for `CallSite::typecheck1`;
            // a user body is not re-checked per call site
            match def.builtin_check() {
                None => ctx.discard_apply(f),
                Some(check) => *check.lock() = Some(f),
            }
            res?;
            let inferred_throws =
                gate.thrown(ctx).scope_refs(&def.scope.lexical).normalize();
            ftyp.throws.check_contains(&ctx.env, &inferred_throws).at(spec)?;
            Ok(())
        });
        let res = res.and_then(|()| check_defaults(ctx, def, &gate.scope));
        gate.close(ctx, def.table.get().map(|t| (t.table.clone(), spec)));
        // what the body bound is the signature: a call's types follow from
        // it without elaborating the body
        self.typ.unbind_vacuous_tvars();
        res
    }

    /// A lambda an instance defines takes its signature from the
    /// enclosing definition's check, at its own level, and generalizes it
    /// as its gate would; its instances read its table from there too.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R, E>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
        let tables = match def.builtin_check() {
            None => types.lambda(self.spec.id),
            Some(_) => None,
        };
        let Some(tables) = tables else { return self.typecheck0(ctx) };
        let level = def.level;
        let row = {
            let _at = AtLevel::enter(level);
            types.typ(self.spec.id)
        };
        let Some(row) = row else { return self.typecheck0(ctx) };
        {
            let _at = AtLevel::enter(level);
            self.typ.check_contains(&ctx.env, &row).at(&self.spec)?;
        }
        def.typ.generalize(def.level.depth());
        def.table.set(tables);
        Ok(())
    }

    /// A definition has no children here; the body is checked per call
    /// site through `GXLambda::typecheck1`.
    fn typecheck1(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Lambda(self)
    }
}

/// `GRAPHIX_ELAB_AUDIT`: reports every error an instance's check raises
/// outside a definition gate, once, at the innermost instance that
/// raised it. The definition and call-site checks are meant to leave
/// elaboration nothing to refuse.
pub(crate) mod elab_audit {
    use crate::{dbgenv, expr::Expr};
    use anyhow::Result;
    use std::cell::Cell;

    thread_local! {
        static REPORTED: Cell<bool> = const { Cell::new(false) };
    }

    pub(crate) fn enter() {
        if dbgenv::graphix_elab_audit() {
            REPORTED.set(false)
        }
    }

    pub(crate) fn leave<T>(gate_depth: usize, kind: &str, spec: &Expr, res: &Result<T>) {
        if let Err(e) = res
            && dbgenv::graphix_elab_audit()
            && gate_depth == 0
            && !REPORTED.replace(true)
        {
            report(kind, spec, format_args!("{e:#}"))
        }
    }

    pub(crate) fn report(kind: &str, spec: &Expr, what: std::fmt::Arguments) {
        let text = spec.to_string().replace('\n', " ");
        let text = text.get(..160).unwrap_or(&text);
        eprintln!(
            "ELAB-AUDIT {kind} at {:?}:{} `{text}`: {what}",
            spec.ori.source, spec.pos
        );
        if std::env::var_os("GRAPHIX_ELAB_AUDIT").is_some_and(|v| v == "bt") {
            eprintln!("{}", std::backtrace::Backtrace::force_capture());
        }
    }
}
