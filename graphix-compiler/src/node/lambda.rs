use super::{
    Nop, WakeBit,
    callsite::{CallSite, Feeds, QuietAtRoot, publish_production},
    collection::CollectionIntrinsic,
    compiler::compile,
    error::RaiseProbe,
    pattern::{Holder, StructPatternNode, rigid_refusal},
    produce_constant,
    select::ArmDiscriminants,
    solving,
};
use crate::SourcePosition;
use crate::{
    AddrSet, Apply, ApplyView, BindId, BindMode, CFlag, CompileCtx, ExecCtx, InitFn,
    LambdaId, LambdaInstanceId, Node, NodeView, Refs, Rt, Scope, TagValue, Update,
    bailat, dbgenv,
    effects::{LambdaFacts, NoLoop, RecursionKind},
    env::{Bind, Env, Lexical},
    expr::{self, Arg, ArgKind, At, Expr, ExprId, LambdaBody, ModPath, Origin},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, call_result_needs_value_widening, widen_result_to_value,
        },
    },
    ide::{HeldSpan, Ide},
    image::{
        self, ImageBuf, ImageDecoder, lexical_decode, lexical_encode,
        nodes::{NodeTag, decode_node, put_tag},
    },
    profile::{self, Phase},
    prune::BoundaryRef,
    reference::{Captures, Lives},
    stack::ensure_sufficient,
    typ::{
        FnArgKind, FnArgType, FnType, ResolvedRef, TVar, TraitId, Type, TypeMismatch,
        bounds, check_bounds_acyclic,
        settle::VariableUnion,
        tvar::{AtLevel, Declaring, InTask, Level, RigidGate, TaskId, current_level},
    },
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack, PackError, encode_varint},
    path::Path,
};
use netidx_value::Value;
use nohash::IntSet;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::{OnceCell, RefCell},
    cmp::Ordering,
    fmt,
    hash::Hash,
    mem,
    sync::{Arc as SArc, OnceLock, Weak},
};
use triomphe::Arc;

thread_local! {
    static METHOD_CELLS: RefCell<LPooled<AHashMap<usize, TVar>>> =
        RefCell::new(LPooled::take());
}

/// The cells a method's annotation minted from the declared signature
/// ([`Type::scope_refs_with`]), by the cell each was minted from, `self`
/// apart, while the method's value compiles: the signature the value's
/// lambda takes from the declaration holds those cells and no copy of its
/// own.
#[must_use]
pub(super) struct MethodCells(LPooled<AHashMap<usize, TVar>>);

impl MethodCells {
    pub(super) fn enter(mut cells: LPooled<AHashMap<usize, TVar>>) -> Self {
        cells.retain(|_, tv| &*tv.name != "self");
        Self(METHOD_CELLS.replace(cells))
    }

    fn current() -> LPooled<AHashMap<usize, TVar>> {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        METHOD_CELLS
            .with_borrow(|m| cells.extend(m.iter().map(|(a, tv)| (*a, tv.clone()))));
        cells
    }
}

impl Drop for MethodCells {
    fn drop(&mut self) {
        METHOD_CELLS.set(mem::take(&mut self.0))
    }
}

/// What a definition's check settled, by the expression id of each node
/// of its body: an instance takes its types from here instead of checking
/// again. An id two nodes share has no row.
#[derive(Debug, Default)]
pub struct DefTable {
    /// Only closed rows may share discriminants across substitutions.
    discriminants: Mutex<AHashMap<ExprId, Option<Arc<ArmDiscriminants>>>>,
    types: AHashMap<ExprId, Type>,
    ftypes: AHashMap<ExprId, FnType>,
    /// What a node derives beside its own type, in the order the node
    /// lists it: a select's arm predicates and pattern binds, a catch's
    /// error binds, what a `?` or `$` strips.
    aux: AHashMap<ExprId, Box<[Type]>>,
    /// The table of each lambda literal of the body, by its id.
    lambdas: AHashMap<ExprId, SArc<DefTable>>,
    /// The typedefs the rows name, owned here: a block's is in the env
    /// only while the block compiles, and the registry of block types
    /// holds it weakly.
    typedefs: Vec<SArc<ResolvedRef>>,
    /// The body's own effect facts (`analysis::def_body_facts`), which
    /// every definition this table types starts from.
    facts: LambdaFacts,
    /// The body's calls through its own parameters, and the enclosing
    /// definitions' parameters it calls, by definition and position, which
    /// an instance resolves to its own formals (`ParamCalls`).
    calls_own: bool,
    calls_captured: Box<[(LambdaId, u32)]>,
    captures: Box<[Captured]>,
    /// The rows of the labeled defaults' nodes, which `settle_open` leaves
    /// open: a call's bind narrows them by its own view of the label. Only
    /// the definition's check reads it.
    defaults: AHashSet<ExprId>,
    /// Under a check by bounds, the copy of a generic binding's scheme
    /// each call site through a read of it took, by the address of the
    /// scheme's cell each copies: an instance's call is an instance of
    /// that copy as the instance has it. Not imaged: a restored
    /// definition's calls find their instances' types again.
    copies: AHashMap<ExprId, Box<[(usize, TVar)]>>,
}

#[derive(Debug, Clone, netidx_derive::Pack)]
struct Captured {
    up: u32,
    path: ModPath,
    name: CompactString,
}

impl Captured {
    fn new(scope: &ModPath, binding: ModPath, name: CompactString) -> Self {
        let common = Path::parts(&scope.0)
            .zip(Path::parts(&binding.0))
            .take_while(|(a, b)| a == b)
            .count();
        Self {
            up: (Path::levels(&scope.0) - common) as u32,
            path: ModPath::from_iter(Path::parts(&binding.0).skip(common)),
            name,
        }
    }

    fn id(&self, env: &Lexical, scope: &ModPath) -> Option<BindId> {
        let base = Path::dirnames(&scope.0).rev().nth(self.up as usize)?;
        let mut path: LPooled<String> = LPooled::take();
        let scope = if &*self.path.0 == "/" {
            base
        } else {
            path.push_str(base.trim_end_matches('/'));
            path.push_str(&self.path.0);
            path.as_str()
        };
        env.binds.get(scope).and_then(|bs| bs.get(&self.name)).copied()
    }
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
    fn with(
        types: AHashMap<ExprId, Type>,
        ftypes: AHashMap<ExprId, FnType>,
        aux: AHashMap<ExprId, Box<[Type]>>,
        lambdas: AHashMap<ExprId, SArc<DefTable>>,
        typedefs: Vec<SArc<ResolvedRef>>,
        facts: LambdaFacts,
        calls_own: bool,
        calls_captured: Box<[(LambdaId, u32)]>,
        captures: Box<[Captured]>,
    ) -> Self {
        Self {
            discriminants: Mutex::default(),
            types,
            ftypes,
            aux,
            lambdas,
            typedefs,
            facts,
            calls_own,
            calls_captured,
            captures,
            defaults: AHashSet::default(),
            copies: AHashMap::default(),
        }
    }

    /// Every type the table's rows hold but the signatures'.
    fn rows(&self) -> impl Iterator<Item = &Type> {
        self.types.values().chain(self.aux.values().flat_map(|ts| ts.iter()))
    }

    /// Every type the table holds: its rows, then each function type's
    /// parts, with whether the part is a constraint.
    fn for_each_type(&self, f: &mut impl FnMut(&Type, bool)) {
        for t in self.rows() {
            f(t, false)
        }
        for ft in self.ftypes.values() {
            ft.for_each_part(&mut |t, constraint| f(t, constraint));
        }
    }

    /// The open cells the rows of the labeled defaults hold, by address:
    /// a call's bind narrows them, so no site settle binds them.
    fn default_cells(&self) -> AHashSet<usize> {
        if self.defaults.is_empty() {
            return AHashSet::default();
        }
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let default = |id: &ExprId| self.defaults.contains(id);
        // CR claude for claude: [structure/low] `default_cells` and `settle_open` each
        // write out the same three loops (the rows, the aux rows, each signature's
        // parts that are not constraints, through `settle::position_cells`), differing
        // only in which ids they keep; `for_each_row` is that walk already but for the
        // constraint flag, and `own_typedefs` repeats the owning half of
        // `seed_and_own`. A row kind added later has four walks to join. Hand
        // `for_each_row`'s callback the constraint flag, as `for_each_type` does, and
        // build both cell sets with it; have `seed_and_own` seed and then call
        // `own_typedefs`. Found by reading; not run. (c-lambda-06)
        let rows = self.types.iter().filter(|(id, _)| default(id)).map(|(_, t)| t);
        let aux =
            self.aux.iter().filter(|(id, _)| default(id)).flat_map(|(_, ts)| ts.iter());
        for t in rows.chain(aux) {
            crate::typ::settle::position_cells(t, &mut cells)
        }
        for (_, ft) in self.ftypes.iter().filter(|(id, _)| default(id)) {
            ft.for_each_part(&mut |t, constraint| {
                if !constraint {
                    crate::typ::settle::position_cells(t, &mut cells)
                }
            })
        }
        cells.keys().copied().collect()
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
        let body = |id: &ExprId| !self.defaults.contains(id);
        let rows = self.types.iter().filter(|(id, _)| body(id)).map(|(_, t)| t);
        let aux =
            self.aux.iter().filter(|(id, _)| body(id)).flat_map(|(_, ts)| ts.iter());
        for t in rows.chain(aux) {
            crate::typ::settle::position_cells(t, &mut cells)
        }
        for (_, ft) in self.ftypes.iter().filter(|(id, _)| body(id)) {
            ft.for_each_part(&mut |t, constraint| {
                if !constraint {
                    crate::typ::settle::position_cells(t, &mut cells)
                }
            })
        }
        let mut quantifiers: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.for_each_type(&mut |t, _| {
            t.each_quantifier(&mut |_, tv| {
                if let Some(open) = tv.open_cell() {
                    for c in open.cell_constraints().iter() {
                        c.reached_cells(&mut quantifiers)
                    }
                    quantifiers.insert(open.cell_addr(), open);
                }
            })
        });
        for (addr, tv) in cells.iter() {
            if !exempt.contains(addr)
                && !quantifiers.contains_key(addr)
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
    fn record<R: Rt>(
        body: &Node<R>,
        def_env: &Lexical,
        checked: &Env,
        facts: LambdaFacts,
        calls_own: bool,
        calls_captured: Box<[(LambdaId, u32)]>,
        captures: &mut AHashSet<(ModPath, CompactString)>,
    ) -> Self {
        let mut table = Self { facts, calls_own, calls_captured, ..Self::default() };
        table.add_rows(body, checked, &mut |n| {
            capture_names(n, def_env, checked, captures)
        });
        table.seed_and_own(def_env, checked, |_| true);
        table
    }

    /// Fill every name the rows `of` an id hold, in the definition's
    /// environment (a block's own types through the registry of block
    /// types), and own what they name.
    fn seed_and_own(
        &mut self,
        def_env: &Lexical,
        checked: &Env,
        of: impl Fn(&ExprId) -> bool,
    ) {
        let mut env = checked.clone();
        env.lexical = def_env.clone();
        let mut seen: LPooled<AddrSet> = LPooled::take();
        let mut cells: LPooled<AHashSet<usize>> = LPooled::take();
        let mut defs: LPooled<AHashMap<usize, SArc<ResolvedRef>>> = LPooled::take();
        for r in self.typedefs.drain(..) {
            defs.insert(SArc::as_ptr(&r) as usize, r);
        }
        self.for_each_row(&of, &mut |t| {
            t.seed_refs_seen(&env, &mut seen);
            t.named_defs(&mut cells, &mut defs)
        });
        self.typedefs.extend(defs.drain().map(|(_, r)| r));
    }

    /// Every type the rows `of` an id hold, signatures' parts included.
    fn for_each_row(&self, of: &impl Fn(&ExprId) -> bool, f: &mut impl FnMut(&Type)) {
        for (_, t) in self.types.iter().filter(|(id, _)| of(id)) {
            f(t)
        }
        for (_, ts) in self.aux.iter().filter(|(id, _)| of(id)) {
            ts.iter().for_each(&mut *f)
        }
        for (_, ft) in self.ftypes.iter().filter(|(id, _)| of(id)) {
            ft.for_each_part(&mut |t, _| f(t))
        }
    }

    /// The rows of the definition's labeled defaults as its check
    /// compiled them: a call's bind types its default from them, as an
    /// instance types its body.
    fn record_defaults<R: Rt>(
        &mut self,
        defaults: &[Node<R>],
        def_env: &Lexical,
        checked: &Env,
    ) {
        let mut ids: LPooled<AHashSet<ExprId>> = LPooled::take();
        for n in defaults {
            self.add_rows(n, checked, &mut |n| {
                ids.insert(n.spec().id);
            });
        }
        if !ids.is_empty() {
            self.seed_and_own(def_env, checked, |id| ids.contains(id));
            self.defaults.extend(ids.drain());
        }
    }

    /// A row for each node of the tree at `root`, `visit`ing each; an id
    /// two nodes share has none.
    fn add_rows<R: Rt>(
        &mut self,
        root: &Node<R>,
        checked: &Env,
        visit: &mut impl FnMut(&Node<R>),
    ) {
        let mut shared: LPooled<AHashSet<ExprId>> = LPooled::take();
        fusion::for_each_node(root, &mut |n| {
            visit(n);
            let id = n.spec().id;
            if self.types.insert(id, n.typ().clone()).is_some() {
                shared.insert(id);
            }
            match n.view() {
                NodeView::CallSite(cs) => {
                    if let Some(ft) = cs.ftype.as_ref() {
                        self.ftypes.insert(id, (**ft).clone());
                    }
                    if let Some(copies) = cs.copies() {
                        let pairs = copies.iter().map(|(a, v)| (*a, v.clone()));
                        self.copies.insert(id, pairs.collect());
                    }
                }
                NodeView::Lambda(l) => {
                    let def = l.def_value().downcast_ref::<LambdaDef<R>>();
                    if let Some(t) = def.and_then(|d| d.table.get()) {
                        self.lambdas.insert(id, t.table.clone());
                    }
                }
                NodeView::Select(s) => {
                    self.aux.insert(id, s.aux_types(checked));
                }
                NodeView::Catch(c) => {
                    self.aux.insert(id, c.aux_types(checked));
                }
                NodeView::Qop(q) => {
                    self.aux.insert(id, Box::new([q.strip.removed()]));
                }
                _ => (),
            }
        });
        for id in shared.drain() {
            self.types.remove(&id);
            self.ftypes.remove(&id);
            self.lambdas.remove(&id);
            self.aux.remove(&id);
        }
    }

    fn own_typedefs(&mut self) {
        let mut cells: LPooled<AHashSet<usize>> = LPooled::take();
        let mut defs: LPooled<AHashMap<usize, SArc<ResolvedRef>>> = LPooled::take();
        for r in self.typedefs.drain(..) {
            defs.insert(SArc::as_ptr(&r) as usize, r);
        }
        self.for_each_type(&mut |t, _| t.named_defs(&mut cells, &mut defs));
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
            let mut table = DefTable::with(
                self.types.iter().map(|(id, t)| (*id, t.rename_with(known))).collect(),
                self.ftypes.iter().map(|(id, ft)| (*id, ft.rename_with(known))).collect(),
                self.aux
                    .iter()
                    .map(|(id, ts)| {
                        (*id, ts.iter().map(|t| t.rename_with(known)).collect())
                    })
                    .collect(),
                self.lambdas
                    .iter()
                    .map(|(id, t)| (*id, SArc::new(t.renamed(known))))
                    .collect(),
                self.typedefs.clone(),
                self.facts,
                self.calls_own,
                self.calls_captured.clone(),
                self.captures.clone(),
            );
            let rename =
                |v: &TVar| known.get(&v.cell_addr()).cloned().unwrap_or(v.clone());
            table.copies = self
                .copies
                .iter()
                .map(|(id, cs)| (*id, cs.iter().map(|(a, v)| (*a, rename(v))).collect()))
                .collect();
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
                encode_varint(table.captures.len() as u64, buf);
                for capture in &table.captures {
                    capture.encode(buf)?;
                }
                table.facts.encode(buf)?;
                table.calls_own.encode(buf)?;
                encode_varint(table.calls_captured.len() as u64, buf);
                for (lid, i) in table.calls_captured.iter() {
                    lid.encode(buf)?;
                    i.encode(buf)?;
                }
                Ok(())
            },
        )
    }

    fn image_decode(buf: &mut impl Buf) -> Result<SArc<DefTable>, PackError> {
        image::foreign_decode(
            buf,
            |sub| {
                let n = image::count_decode(sub)?;
                let mut types = AHashMap::with_capacity(n);
                for _ in 0..n {
                    types.insert(ExprId::decode(sub)?, Type::decode(sub)?);
                }
                let n = image::count_decode(sub)?;
                let mut ftypes = AHashMap::with_capacity(n);
                for _ in 0..n {
                    ftypes.insert(ExprId::decode(sub)?, FnType::decode(sub)?);
                }
                let n = image::count_decode(sub)?;
                let mut aux = AHashMap::with_capacity(n);
                for _ in 0..n {
                    let id = ExprId::decode(sub)?;
                    let m = image::count_decode(sub)?;
                    let ts =
                        (0..m).map(|_| Type::decode(sub)).collect::<Result<_, _>>()?;
                    aux.insert(id, ts);
                }
                let n = image::count_decode(sub)?;
                let mut lambdas = AHashMap::with_capacity(n);
                for _ in 0..n {
                    lambdas.insert(ExprId::decode(sub)?, Self::image_decode(sub)?);
                }
                let n = image::count_decode(sub)?;
                let typedefs = (0..n)
                    .map(|_| image::resolved_decode(sub))
                    .collect::<Result<Vec<_>, _>>()?;
                let n = image::count_decode(sub)?;
                let captures =
                    (0..n).map(|_| Pack::decode(sub)).collect::<Result<_, _>>()?;
                let facts = LambdaFacts::decode(sub)?;
                let calls_own = bool::decode(sub)?;
                let n = image::count_decode(sub)?;
                let calls_captured = (0..n)
                    .map(|_| Ok((LambdaId::decode(sub)?, u32::decode(sub)?)))
                    .collect::<Result<_, _>>()?;
                Ok(SArc::new(DefTable::with(
                    types,
                    ftypes,
                    aux,
                    lambdas,
                    typedefs,
                    facts,
                    calls_own,
                    calls_captured,
                    captures,
                )))
            },
            |b| Self::image_decode(b),
        )
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
        let _task = InTask::enter(TaskId::OUTSIDE);
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
#[derive(Debug, Clone)]
pub struct InstanceTypes {
    tables: Tables,
    known: Known,
    open: IntSet<LambdaId>,
}

/// The copy the instance at `instance` holds of each open cell of its
/// definition's signature `def`, by the cell's address: each bound to the
/// instance's type ([`FnType::held_at`]). `None` when the instance's
/// signature does not hold the definition's.
// CR claude for claude: [bug/medium] `held_at` refuses a site signature the check
// itself made. A body returning `[Array<i64>, Array<'x>]` with `'x` open (the payloads
// of `select true { true => B([1]), false => B([]) }` over `type B<'p> = Abstract<'p>`,
// read by `select b { B(x) => x }`) is called at a site whose settle binds `'x` to
// bottom and leaves `[Array<_>, Array<i64>]`, where the definition's open member could
// go to either. The check accepts the call and the build refuses 'an instance at fn(q:
// i64) -> [Array<_>, Array<i64>] of a definition typed ..'; the same statements at the
// top level build. Normalize the settled return first (a member holding bottom that
// another member contains goes, as `joined_lower` does for bounds), or place the
// settled cell at its member. probe:
// design/review-2026-11-16/repro/x-check-build-instance-signature-abstract-union.gx
// (x-check-build-05)
fn instance_cells<R: Rt>(
    ctx: &CompileCtx<R>,
    def: &FnType,
    instance: &FnType,
    leaves: &[ArcStr],
) -> Option<AHashMap<usize, TVar>> {
    // a definition checked by bounds is a scheme whose cells carry what
    // each instance has to decide: the instance's types are a copy of
    // them, fixed where the copy is used as the instance's signature
    let scheme = Type::Fn(Arc::new(def.clone()));
    if bounds::scheme(&scheme) {
        let at = Type::Fn(Arc::new(instance.clone()));
        return bounds::instance(&ctx.env, &scheme, &at, leaves)
            .inspect_err(|e| {
                bounds::debug(|| format!("instance of {scheme} at {at}: {e:#}"))
            })
            .ok();
    }
    // every open cell of the signature is the instance's own copy, a cell
    // no closed gate owns included, so the check binds nothing another
    // instance reads
    def.held_at(&ctx.env, instance).ok().map(|(_, known)| known)
}

impl InstanceTypes {
    /// The types of `def`'s default of the label `name`, at `root`, as its
    /// check recorded them, each open cell its definition owns copied.
    /// Where the check was by bounds they are the scheme's, and the
    /// default's are those of the instance `site` calls.
    pub(crate) fn of_default<R: Rt>(
        ctx: &CompileCtx<R>,
        def: &LambdaDef<R>,
        name: &ArcStr,
        root: ExprId,
        site: Option<&FnType>,
    ) -> Result<Self> {
        match def.table.get() {
            // CR claude for claude: [bug/medium] A labeled default whose root
            // expression is lowered (a trait method used as a value, `#d: fn(x: i64) ->
            // i64 = Show::show`; a `seq` or `seqq`) passes the check, and every call
            // that omits the label fails the build with "the default of #d, which its
            // definition's check recorded no types of": compile replaces the root by
            // its lowering, so the table has rows for the lowered ids and none for
            // `root`. A default that only contains the lowered form builds. Fix: record
            // the root's row under the written expression's id when a lowering stands
            // in for it, keep a default's lowering with its definition as a body's is,
            // and pin both forms. probe:
            // design/review-2026-11-16/repro/x-engine-seq-errors-lowered-default-check-build.gx
            // (x-check-build-default-seq-fallback.gx shows a seq inside a default
            // lowered again at the call). (x-engine-seq-errors-04)
            Some(tables) if tables.table.types.contains_key(&root) => {
                let known = site
                    .filter(|_| ctx.bounds)
                    .and_then(|site| {
                        instance_cells(ctx, &def.typ, site, std::slice::from_ref(name))
                    })
                    .unwrap_or_default();
                Ok(Self {
                    tables: tables.clone(),
                    known: SArc::new(Mutex::new(known)),
                    open: ctx.rec_defs.clone(),
                })
            }
            _ => bail!(
                "the default of #{name}, which its definition's check recorded no types of"
            ),
        }
    }

    /// `None` when the instance's signature does not hold the
    /// definition's: a type-system bug, which `instance_types` refuses.
    fn new<R: Rt>(
        ctx: &CompileCtx<R>,
        tables: Tables,
        def: &FnType,
        instance: &FnType,
    ) -> Option<Self> {
        let open = ctx.rec_defs.clone();
        let known = instance_cells(ctx, def, instance, &[])?;
        Some(Self { tables, known: SArc::new(Mutex::new(known)), open })
    }

    /// The types of an instance a call builds through a read of a generic
    /// binding, at the copy of the scheme the read took, which the check
    /// fixed: no second solve need find them again. `None` where the
    /// read's scheme is not the definition's own (a val's declaration is
    /// another).
    fn copied<R: Rt>(
        ctx: &CompileCtx<R>,
        table: &SArc<DefTables>,
        def: &Arc<FnType>,
        copies: &AHashMap<usize, TVar>,
    ) -> Option<Self> {
        let tables = table.get()?;
        let sig = Type::Fn(def.clone());
        if !bounds::cells_in(&sig).iter().all(|c| copies.contains_key(&c.cell_addr())) {
            return None;
        }
        // a copy the check left open is no instance's types
        if copies.values().any(|v| !bounds::cells_in(&Type::TVar(v.clone())).is_empty()) {
            return None;
        }
        Some(Self {
            tables: tables.clone(),
            known: SArc::new(Mutex::new(copies.clone())),
            open: ctx.rec_defs.clone(),
        })
    }

    /// A row read from this definition's check, renamed through the
    /// enclosing instance's map, then instantiated through this one's.
    fn instance_of(&self, t: &Type) -> Type {
        let t = match &self.tables.outer {
            Some(outer) => t.rename_with(&outer.lock()),
            None => t.clone(),
        };
        // a row a check by bounds made holds scheme cells: the instance's
        // own, or those of a generic definition inside it
        if bounds::scheme(&t) {
            return bounds::specialize(&t, &mut self.known.lock());
        }
        t.instantiate_with(&mut self.known.lock(), &self.open)
    }

    /// The type of the node at `id` as its definition's check left it,
    /// over the definition's own variables.
    pub(crate) fn definitions(&self, id: ExprId) -> Option<&Type> {
        self.tables.table.types.get(&id)
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
    /// instance's knowledge stands where either holds the other, and the
    /// instance is refused where a closed born type and a closed row are
    /// disjoint.
    pub(crate) fn settle<R: Rt>(
        &mut self,
        ctx: &CompileCtx<R>,
        spec: &Expr,
        typ: &Type,
    ) -> Result<bool> {
        self.settle_in(&ctx.env, spec.id, typ).at(spec)
    }

    fn settle_in(&mut self, env: &Env, id: ExprId, typ: &Type) -> Result<bool> {
        let Some(row) = self.typ(id) else { return Ok(false) };
        let born_closed = !typ.has_unbound();
        if !typ.take_row(&row) {
            return Ok(false);
        }
        if born_closed
            && !row.has_unbound()
            && !row.contains(env, typ)?
            && !typ.contains(env, &row)?
        {
            return crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                bail!(
                    "a node of an instance is {typ} where its definition's check settled {row}"
                )
            });
        }
        Ok(true)
    }

    /// What the node at `id` derived beside its own type, in its order
    /// ([`DefTable::aux`]).
    /// The copy of a generic binding's scheme the definition's call site
    /// at `id` took, as this instance has it: each cell, the scheme's and
    /// the copy's, through the enclosing instance's map, then this one's
    /// (a scheme the body defines is the instance's own copy of it).
    pub(crate) fn call_copies(&mut self, id: ExprId) -> Option<AHashMap<usize, TVar>> {
        let pairs = self.tables.table.copies.get(&id)?.clone();
        let outer = |addr: usize| match &self.tables.outer {
            Some(outer) => outer.lock().get(&addr).cloned(),
            None => None,
        };
        let renamed: SmallVec<[(usize, TVar); 8]> = pairs
            .iter()
            .map(|(a, d)| {
                let a = outer(*a).map(|t| t.cell_addr()).unwrap_or(*a);
                (a, outer(d.cell_addr()).unwrap_or_else(|| d.clone()))
            })
            .collect();
        let mut out = AHashMap::default();
        for (addr, d) in renamed {
            let _ = self.instance_of(&Type::TVar(d.clone()));
            let known = self.known.lock();
            let key = known.get(&addr).map(|t| t.cell_addr()).unwrap_or(addr);
            let v = known.get(&d.cell_addr()).cloned().unwrap_or(d);
            // a copy this instance has not settled is no instance's types
            if !bounds::cells_in(&Type::TVar(v.clone())).is_empty() {
                return None;
            }
            out.insert(key, v);
        }
        Some(out)
    }

    pub(crate) fn aux(&mut self, id: ExprId) -> Option<SmallVec<[Type; 8]>> {
        let ts = self.tables.table.aux.get(&id)?;
        Some(ts.iter().map(|t| self.instance_of(t)).collect())
    }

    pub(crate) fn arm_discriminants(
        &self,
        id: ExprId,
        arg: ExprId,
        arms: usize,
    ) -> Option<Arc<ArmDiscriminants>> {
        let table = &self.tables.table;
        table
            .discriminants
            .lock()
            .entry(id)
            .or_insert_with(|| {
                let scrutinee = table.types.get(&arg)?;
                let predicates = table.aux.get(&id)?;
                if scrutinee.has_unbound() || predicates.iter().any(Type::has_unbound) {
                    return None;
                }
                Some(Arc::new(ArmDiscriminants::new(scrutinee.clone(), arms)))
            })
            .clone()
    }

    /// The signature the definition's check settled for the call at `id`.
    pub(crate) fn ftype(&mut self, id: ExprId) -> Option<FnType> {
        let ft = self.tables.table.ftypes.get(&id)?;
        let ft = match &self.tables.outer {
            Some(outer) => ft.rename_with(&outer.lock()),
            None => ft.clone(),
        };
        Some(ft.instantiate_with(&mut self.known.lock(), &self.open))
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

/// What every compile of a definition's body shares
/// (`CompileCtx::body`).
pub type BodyShare = SArc<Mutex<BodyShared>>;

/// The bindings [`LambdaDef::restore_captures`] put back.
pub(crate) struct Restored(SmallVec<[BindId; 2]>);

impl Restored {
    /// Take them out of `env` again.
    pub(crate) fn forget(self, env: &mut Env) {
        for id in self.0 {
            env.by_id.remove(&id);
        }
    }
}

/// A definition a returned function names, and the bindings it captures,
/// as its enclosing definition's check left them.
#[derive(Debug)]
pub(crate) struct Held {
    id: LambdaId,
    binds: Vec<Arc<Bind>>,
}

#[derive(Debug, Default)]
pub struct BodyShared {
    /// Each expression the body rewrites (a seq's machine, a trait
    /// method's eta expansion), by expression: one body compiles each
    /// under one scope.
    pub(crate) lowerings: AHashMap<ExprId, Expr>,
    /// The specs the definition's check put in an `Arc`, by expression:
    /// an instance's node takes the check's (`CompileCtx::shared_spec`).
    pub(crate) specs: AHashMap<ExprId, Arc<Expr>>,
    /// What each lambda literal in the body shares, by its expression:
    /// every compile of the body makes a definition of it, and they share
    /// one.
    pub(crate) literals: AHashMap<ExprId, BodyShare>,
    /// A collection intrinsic's prototype call, by the expression id
    /// every compile of the intrinsic gives it.
    pub(crate) prototype: Option<ExprId>,
    /// The definition of this literal that its enclosing definition's
    /// check made and a function the enclosing definition returns names:
    /// it stays registered until the enclosing definition is deleted, so
    /// a later input's call of that result finds the defaults it omits.
    pub(crate) held: Option<Held>,
    /// Whether the literal sits in another definition's body: its
    /// enclosing definition's deletion releases what it holds, not its own.
    pub(crate) nested: bool,
    /// The kernels the run-time binds of the definition's instances built
    /// (`fusion::fuse_instance`).
    pub(crate) bound: crate::fusion::BoundKernels,
}

/// A type written in a body: its same-named variables are one variable,
/// and one a definition around the body declares (a nested fn type's own
/// quantifiers are its own) is the innermost such definition's; any other
/// is the written type's own (`Level::Written`). Where `judged`, a
/// function type it writes holds no union of two of its own variables
/// ([`Type::variable_union`]); an impl method's annotation is judged at its
/// impl.
pub(crate) fn body_annotation<R: Rt>(
    ctx: &CompileCtx<R>,
    t: &Type,
    judged: bool,
) -> Result<()> {
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    enclosing_tvars(ctx, &mut known);
    let enclosing: LPooled<AHashSet<ArcStr>> = known.keys().cloned().collect();
    t.alias_tvars(&mut known);
    for (_, tv) in known.iter().filter(|(name, _)| !enclosing.contains(*name)) {
        tv.own_written()
    }
    if judged && written_own(&known, &enclosing) > 1 {
        t.variable_union(&|tv| !enclosing.contains(&tv.name))?
    }
    Ok(())
}

/// How many of the variables `known` names a type wrote, not one of
/// `enclosing`: a union of two of them needs two.
fn written_own(known: &AHashMap<ArcStr, TVar>, enclosing: &AHashSet<ArcStr>) -> usize {
    known.keys().filter(|n| !n.starts_with('_') && !enclosing.contains(*n)).count()
}

/// The variables the definitions around the body being compiled declare,
/// by name, the innermost's where two share one ([`Around::declared`]).
pub(super) fn enclosing_tvars<R: Rt>(
    ctx: &CompileCtx<R>,
    known: &mut AHashMap<ArcStr, TVar>,
) {
    if let Some(body) = &ctx.body {
        for tv in body.around.declared.iter().rev() {
            known.entry(tv.name.clone()).or_insert_with(|| tv.clone());
        }
    }
}

/// The variables the signature `def` writes ([`FnType::written_tvars`]),
/// as a body compiled under it names them: the definition's own cells,
/// or, through `known` (an instance's map of its definition's cells), the
/// instance's copies. One the definition holds bound to a type (a lambda
/// no binding names is its one instance) is a cell of its name bound to
/// that type as the instance holds it, so the name never reaches a
/// variable a definition further out declares alike.
fn written_tvars(
    def: &FnType,
    known: Option<&AHashMap<usize, TVar>>,
) -> LPooled<Vec<TVar>> {
    let held = |tv: TVar| match known {
        None => tv,
        Some(known) => match &Type::TVar(tv.clone()).rename_with(known) {
            Type::TVar(tv) => tv.clone(),
            t => TVar::named(tv.name.clone(), t.clone()),
        },
    };
    def.written_tvars().drain(..).map(|(_, tv)| held(tv)).collect()
}

/// What a body takes from the definitions lexically around it, innermost
/// last.
#[derive(Debug, Clone)]
pub struct Around {
    /// The variables their signatures write, each the cell this compile
    /// holds for it.
    pub declared: Arc<[TVar]>,
    /// The types of their instances, which a run-time bind's kernels are
    /// kept by ([`crate::fusion::BoundKey`]).
    pub instances: Arc<[Arc<FnType>]>,
}

impl Around {
    fn nothing() -> Self {
        Self { declared: Arc::from_iter([]), instances: Arc::from_iter([]) }
    }
}

/// The body compiling now: what it shares, and whether this compile is
/// its definition's check, which alone records specs.
#[derive(Debug, Clone)]
pub(crate) struct BodyCompile {
    pub(crate) share: BodyShare,
    pub(crate) checking: bool,
    pub(crate) types: Option<InstanceTypes>,
    pub(crate) around: Around,
}

impl BodyShared {
    /// Hand `out` the id each literal under `this` holds, none held after.
    fn release_held(
        this: &BodyShare,
        seen: &mut AHashSet<usize>,
        out: &mut impl FnMut(LambdaId),
    ) {
        let children: SmallVec<[BodyShare; 4]> =
            this.lock().literals.values().cloned().collect();
        for child in children {
            if seen.insert(SArc::as_ptr(&child) as usize) {
                if let Some(held) = child.lock().held.take() {
                    out(held.id)
                }
                Self::release_held(&child, seen, out)
            }
        }
    }

    /// Visit `this` and every literal's share under it, each once.
    pub(crate) fn visit(
        this: &BodyShare,
        seen: &mut AHashSet<usize>,
        f: &mut impl FnMut(&Self),
    ) {
        if seen.insert(SArc::as_ptr(this) as usize) {
            let l = this.lock();
            f(&l);
            for child in l.literals.values() {
                Self::visit(child, seen, f)
            }
        }
    }
}

pub struct LambdaDef<R: Rt> {
    pub id: LambdaId,
    pub env: Lexical,
    pub scope: Scope,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R>,
    /// What the definition's check settled, for its instances; shared
    /// with `init`, which hands it to each instance.
    pub table: SArc<DefTables>,
    /// What every compile of its body shares.
    pub shared: BodyShare,
    /// Sync/async effect and statelessness, computed by
    /// `analysis::infer_effects`: the body's own, joined with every
    /// instance's an analysis reached (a resolved callback's instance
    /// joins its HOF instance's; a call through an unresolved parameter
    /// is async), so they only degrade. Stateless: the body holds no
    /// per-activation state (every builtin it reaches is
    /// `Effect::Stateless` or `Effect::Io`, no `<-` targets its own
    /// binding, every callee is stateless); a tail loop reuses one
    /// activation only then.
    pub facts: Mutex<LambdaFacts>,
    /// The function parameters its body calls that its check could not
    /// resolve.
    pub param_calls: Mutex<ParamCalls>,
    /// How this lambda recurses, computed by `analysis::analyze`. The
    /// operational tail-loop gate is `GXLambda::tail_loop`, not this.
    pub recursion: Mutex<RecursionKind>,
    /// Why a `TailCalls` recursion builds no loop, when its body is pure.
    pub no_loop: Mutex<Option<NoLoop>>,
    /// The lambda expression this def was compiled from; stable across
    /// instance re-compiles, which is what [`crate::FnArgIdentity`] keys on.
    pub source: ExprId,
    pub origin: DefOrigin,
    /// The definition's level: the cells it owns are its signature's
    /// and its body's.
    pub level: Level,
    pub captures: OnceLock<Captures>,
}

impl<R: Rt> LambdaDef<R> {
    /// Keep registered each definition of a literal in this body that
    /// `rtype`, the type of what the body returns, names.
    fn hold_returned(&self, ctx: &CompileCtx<R>, rtype: &Type) {
        rtype.each_held_fn(&mut |ft| {
            for id in ft.lambda_ids.ids().iter() {
                let Some(inner) = ctx
                    .lambda_defs
                    .get(id)
                    .and_then(|v| v.downcast_ref::<LambdaDef<R>>())
                else {
                    continue;
                };
                let own = self
                    .shared
                    .lock()
                    .literals
                    .get(&inner.source)
                    .is_some_and(|s| SArc::ptr_eq(s, &inner.shared));
                if own {
                    let mut binds = Vec::new();
                    inner.captured_ids(&mut |id| {
                        binds.extend(ctx.env.by_id.get(&id).cloned())
                    });
                    inner.shared.lock().held = Some(Held { id: inner.id, binds })
                }
            }
        })
    }

    /// The definition of the lambda expression `spec`, compiled or decoded
    /// from an image: its instances' `init` built from its parts, the
    /// analysis facts as given, nothing else of a run.
    pub(crate) fn of_source(
        id: LambdaId,
        env: Lexical,
        scope: Scope,
        argspec: Arc<[Arg]>,
        typ: Arc<FnType>,
        table: SArc<DefTables>,
        shared: BodyShare,
        facts: LambdaFacts,
        param_calls: ParamCalls,
        recursion: RecursionKind,
        source: ExprId,
        level: Level,
        body: DefBody,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        enclosing: Around,
    ) -> Self {
        let init = make_init(
            id,
            flags,
            env.clone(),
            &scope,
            typ.clone(),
            argspec.clone(),
            spec.clone(),
            body.clone(),
            enclosing.clone(),
            table.clone(),
            shared.clone(),
        );
        let origin = DefOrigin::Source { body, flags, spec: (*spec).clone(), enclosing };
        Self::with(
            id,
            env,
            scope,
            argspec,
            typ,
            init,
            table,
            shared,
            facts,
            param_calls,
            recursion,
            source,
            origin,
            level,
        )
    }

    /// The definition an `Apply` built by Rust at run time stands for:
    /// `init` builds it, and an analysis knows nothing of it.
    pub fn runtime(
        id: LambdaId,
        env: Lexical,
        scope: Scope,
        argspec: Arc<[Arg]>,
        typ: Arc<FnType>,
        init: InitFn<R>,
        source: ExprId,
        level: Level,
    ) -> Self {
        Self::with(
            id,
            env,
            scope,
            argspec,
            typ,
            init,
            Default::default(),
            Default::default(),
            LambdaFacts::ASYNC,
            Default::default(),
            RecursionKind::NotRecursive,
            source,
            DefOrigin::Runtime,
            level,
        )
    }

    fn with(
        id: LambdaId,
        env: Lexical,
        scope: Scope,
        argspec: Arc<[Arg]>,
        typ: Arc<FnType>,
        init: InitFn<R>,
        table: SArc<DefTables>,
        shared: BodyShare,
        facts: LambdaFacts,
        param_calls: ParamCalls,
        recursion: RecursionKind,
        source: ExprId,
        origin: DefOrigin,
        level: Level,
    ) -> Self {
        LambdaDef {
            id,
            env,
            scope,
            argspec,
            typ,
            init,
            table,
            shared,
            facts: Mutex::new(facts),
            param_calls: Mutex::new(param_calls),
            recursion: Mutex::new(recursion),
            no_loop: Mutex::new(None),
            source,
            origin,
            level,
            captures: OnceLock::new(),
        }
    }
}

/// Where a definition came from: a lambda expression, whose `init` is
/// a function of these and which an image carries as data, or Rust
/// code building an `Apply` at runtime, which no image can carry.
pub enum DefOrigin {
    Source {
        body: DefBody,
        flags: BitFlags<CFlag>,
        spec: Expr,
        /// What the definitions lexically around the literal give its
        /// body, as the compile that met the literal held them.
        enclosing: Around,
    },
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

/// The function parameters a definition's body calls that its check could
/// not resolve: its own, which a caller passes, or a builtin it is handed
/// supplies from data, and its enclosing definitions', which each instance
/// resolves (`CompileCtx::fn_forward_resolutions`).
#[derive(Debug, Clone, Default, PartialEq, Eq, netidx_derive::Pack)]
pub struct ParamCalls {
    /// It calls its own, or an enclosing instance's that the instance did
    /// not name: resolved by no instance.
    pub own: bool,
    pub captured: SmallVec<[BindId; 2]>,
}

/// The order the program writes two places in: by their loads (a
/// session's inputs in the order given), then their origins, then where
/// each stands in its own.
pub(crate) fn place_order(
    a: (&Arc<Origin>, SourcePosition),
    b: (&Arc<Origin>, SourcePosition),
) -> Ordering {
    let ori = if Arc::ptr_eq(a.0, b.0) {
        Ordering::Equal
    } else {
        a.0.load
            .cmp(&b.0.load)
            .then_with(|| a.0.partial_cmp(b.0).unwrap_or(Ordering::Equal))
    };
    ori.then(a.1.cmp(&b.1))
}

impl<R: Rt> LambdaDef<R> {
    /// The dispatchers of the traits the definition's signature names as
    /// a bound.
    fn bound_ids(&self, env: &Env, f: &mut impl FnMut(BindId)) {
        env.bound_dispatchers(&self.typ, f)
    }

    /// Put back into `env` the bindings this definition captures that
    /// its enclosing definition's check made and the end of that check
    /// closed, and say which; `None` where one it captures is still gone.
    pub(crate) fn restore_captures(&self, env: &mut Env) -> Option<Restored> {
        let mut gone: SmallVec<[BindId; 2]> = SmallVec::new();
        self.captured_ids(&mut |id| {
            if env.by_id.get(&id).is_none() {
                gone.push(id)
            }
        });
        let shared = self.shared.lock();
        let mut put: SmallVec<[BindId; 2]> = SmallVec::new();
        let mut binds: SmallVec<[&Arc<Bind>; 2]> = SmallVec::new();
        for id in gone {
            let held = shared.held.as_ref().filter(|h| h.id == self.id)?;
            binds.push(held.binds.iter().find(|b| b.id == id)?);
            put.push(id)
        }
        for b in binds {
            env.by_id.insert(b.id, b.clone());
        }
        Some(Restored(put))
    }

    pub(crate) fn captured_ids(&self, f: &mut impl FnMut(BindId)) {
        if let Some(tables) = self.table.get() {
            for capture in &tables.table.captures {
                if let Some(id) = capture.id(&self.env, &self.scope.lexical) {
                    f(id);
                }
            }
        }
    }

    /// Degrade the facts by `facts`; they never improve.
    pub(crate) fn join_facts(&self, facts: LambdaFacts) {
        let mut f = self.facts.lock();
        *f = f.join(facts);
    }

    /// Whether the definition is a Rust builtin's.
    pub(crate) fn is_builtin(&self) -> bool {
        self.builtin_name().is_some()
    }

    /// The Rust builtin the definition is, by its registered name.
    pub(crate) fn builtin_name(&self) -> Option<&ArcStr> {
        match &self.origin {
            DefOrigin::Source { body: DefBody::BuiltIn(name), .. } => Some(name),
            _ => None,
        }
    }

    /// The order the program writes `self` and `other` in: a source
    /// definition by its load (a session's inputs in the order given), then
    /// its origin, then where its lambda is written, before one Rust built
    /// at run time; the id decides only between two of one place.
    pub(crate) fn written_order(&self, other: &Self) -> Ordering {
        let place = match (&self.origin, &other.origin) {
            (DefOrigin::Source { spec: a, .. }, DefOrigin::Source { spec: b, .. }) => {
                place_order((&a.ori, a.pos), (&b.ori, b.pos))
            }
            (DefOrigin::Source { .. }, DefOrigin::Runtime) => Ordering::Less,
            (DefOrigin::Runtime, DefOrigin::Source { .. }) => Ordering::Greater,
            (DefOrigin::Runtime, DefOrigin::Runtime) => Ordering::Equal,
        };
        place.then(self.id.cmp(&other.id))
    }

    /// Whether `other` is another compile of this definition's lambda
    /// where every name and every variable the definitions around it
    /// declare means what it means here: the two write the same defaults
    /// over the same bindings. False says nothing.
    pub(crate) fn same_literal(&self, other: &Self) -> bool {
        let declared = match (&self.origin, &other.origin) {
            (
                DefOrigin::Source { enclosing: a, .. },
                DefOrigin::Source { enclosing: b, .. },
            ) => {
                let (a, b) = (&a.declared, &b.declared);
                a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(a, b)| a.cell_addr() == b.cell_addr())
            }
            (DefOrigin::Runtime, _) | (_, DefOrigin::Runtime) => false,
        };
        declared
            && self.source == other.source
            && self.scope.lexical == other.scope.lexical
            && self.env.same_snapshot(&other.env)
    }

    /// `f` compiling an expression the signature holds, a labeled
    /// default: a type it writes names the variables of the definitions
    /// around this one, as the signature does.
    pub(crate) fn in_signature<T>(
        &self,
        ctx: &mut CompileCtx<R>,
        types: Option<InstanceTypes>,
        f: impl FnOnce(&mut CompileCtx<R>) -> T,
    ) -> T {
        let around = match &self.origin {
            DefOrigin::Source { enclosing, .. } => enclosing.clone(),
            DefOrigin::Runtime => Around::nothing(),
        };
        let outer = ctx.body.replace(BodyCompile {
            // CR claude for claude: [bug/medium] Every compile of a labeled default
            // gets a new share, so `compiler::lowering` lowers a `seq` in the default
            // again at each call that omits it, with new expression ids: the
            // definition's table has no row for the lowered nodes and the bind checks
            // them in full (`elab_audit::fallback`, one line a node). That check is not
            // the definition's: `let s = {f: |x| x}; let t = 1; seq t { let a = s.f(1);
            // s.f("s") }` as a default passes the check and is refused by the build
            // ('i64 does not contain string'), and builds as a function's body. Keep
            // one share per definition for its defaults, so the bind takes the check's
            // lowering. The root-lowered default in
            // design/review-2026-11-16/repro/x-engine-seq-errors-lowered-default-check-build.gx
            // has the same cause. probe:
            // design/review-2026-11-16/repro/x-check-build-default-seq-fallback.gx
            // (x-check-build-03)
            // CR claude for claude: [bug/high] Each compile of a labeled default gets a
            // share of its own here, so `compiler::lowering` rewrites a default's seq
            // or trait-method name afresh, with new expression ids, at every compile,
            // where a body's compiles share one rewrite. `DefTable::record_defaults`
            // keys rows by the check's ids and `InstanceTypes::of_default` asks for a
            // row at the written default's id, which a lowered root never has: `|#show:
            // fn(x: i64) -> string = Desc::desc, y: i64| show(y)` and `|#v: i64 = seq {
            // .. }, y: i64| ..` pass the check and every bind that omits the label is
            // refused ("the default of #show, which its definition's check recorded no
            // types of"); at a run-time bind the call is bottom for good. A lowered
            // part below the root has no rows either and is checked in full in each
            // instance. Compile a definition's defaults under a share it keeps, and
            // have `of_default` look for the row of the lowering's root. probe:
            // design/review-2026-11-16/repro/c-lambda-default-lowered-root.gx
            // (c-lambda-03)
            share: BodyShare::default(),
            checking: false,
            types,
            around,
        });
        let r = f(ctx);
        ctx.body = outer;
        r
    }
}

impl<R: Rt> fmt::Debug for LambdaDef<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.origin {
            DefOrigin::Source { spec, .. } => write!(f, "{spec}"),
            DefOrigin::Runtime => write!(f, "{}", self.typ),
        }
    }
}

impl<R: Rt> PartialEq for LambdaDef<R> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<R: Rt> Eq for LambdaDef<R> {}

/// Functions have no order (`Ordered` refuses them); under `Any` one orders
/// by its id, minted in run order, so such an order is nondeterministic.
impl<R: Rt> PartialOrd for LambdaDef<R> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<R: Rt> Ord for LambdaDef<R> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl<R: Rt> Hash for LambdaDef<R> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<R: Rt> Pack for LambdaDef<R> {
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
pub struct GXLambda<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by the next update
    // CR claude for claude: [readability/low] The comment above calls this bit "wake
    // catch-up", a mechanism that no longer exists: all the bit does is make the
    // dispatch after a wake stand the formals' quiet productions (`QuietAtRoot::Stand`
    // in `update`), which `WakeBit`'s own doc already says. The same line sits on the
    // `slept` field of twelve other nodes (node/op.rs, array.rs, map.rs, data.rs).
    // Delete them. Found by reading; not run. (c-lambda-07)
    slept: WakeBit,
    id: LambdaId,
    instance_id: LambdaInstanceId,
    args: Box<[StructPatternNode]>,
    body: Node<R>,
    typ: Arc<FnType>,
    /// How the body calls itself ([`SelfCall`]), when it does.
    self_call: Mutex<Option<SelfCall>>,
    /// The dispatch's return slot, lent to the owning `CallSite`.
    resident: TagValue,
    /// `true` until the first dispatch, which seeds the fresh formal
    /// ids' value channel from the args' quiet productions; true in an
    /// image, which is written before any cycle.
    first_dispatch: bool,
    /// The def-side lexical env the body was compiled under. The body
    /// typechecks under it too: the caller's env, which drives the
    /// checks, may lack the defining module's private typedefs.
    env: Lexical,
    typing: Typing,
    /// The runtime this activation counts against, when a cycle bound it
    /// (`stack::activation_born`).
    charged: Option<usize>,
}

/// How an instance's check types its body.
#[derive(Debug)]
pub(crate) enum Typing {
    /// It is its definition's check, which checks the body.
    Definition,
    /// From the definition's check, substituted by the instance's
    /// signature: the definition's tables and signature.
    Substituted(SArc<DefTables>, Arc<FnType>),
    /// The construction's substitution map, consumed by the first check.
    Prepared(Box<(Typing, InstanceTypes)>),
    /// Restored from an image, written after its check.
    Restored,
}

/// The callback instance whose body the collection loop at `node` (a
/// collection-bodied lambda's body) runs, which a fused loop emits inline.
pub(crate) fn loop_callback<R: Rt>(node: &Node<R>) -> Option<&GXLambda<R>> {
    match node.view() {
        NodeView::MapQ(map) => map.callback(),
        NodeView::FoldQ(fold) => fold.callback(),
        _ => None,
    }
}

impl<R: Rt> GXLambda<R> {
    /// The definition's id, shared by every instance of it.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    pub fn instance_id(&self) -> LambdaInstanceId {
        self.instance_id
    }

    /// The compiled body.
    pub fn body(&self) -> &Node<R> {
        &self.body
    }

    pub(crate) fn body_mut(&mut self) -> &mut Node<R> {
        &mut self.body
    }

    /// The walk-pruning boundary around the body.
    pub(crate) fn body_boundary(&self) -> Option<&BoundaryRef> {
        self.body.as_prune().map(|p| p.boundary_ref())
    }

    /// The callback instance of a collection-bodied lambda, whose body a
    /// fused loop emits inline.
    pub(crate) fn inline_callback(&self) -> Option<&GXLambda<R>> {
        loop_callback(&self.body)
    }

    /// Argument-binding patterns, parallel to `self.typ().args`.
    pub fn args(&self) -> &[StructPatternNode] {
        &self.args
    }

    /// This instance's resolved `FnType` (same as `Apply::typ()`).
    pub fn typ(&self) -> &Arc<FnType> {
        &self.typ
    }

    /// The tail-loop gate (see the `self_call` field).
    pub fn tail_loop(&self) -> bool {
        self.self_call.lock().is_some_and(|s| s.tail_loop)
    }

    /// The binding the body calls itself through.
    pub fn self_bind(&self) -> Option<BindId> {
        self.self_call.lock().map(|s| s.bind)
    }

    /// Set how the body calls itself; `&self` so analysis can mark
    /// through a shared `&Node`.
    pub fn set_self_call(&self, call: Option<SelfCall>) {
        *self.self_call.lock() = call;
    }
}

/// How an instance's body calls itself: through `bind`, and as one native
/// loop when `tail_loop` (its self-calls are tails over loop-able
/// formals; set by `analysis::analyze`, the JIT emits the loop and the
/// node-walk dispatches every call).
#[derive(Debug, Clone, Copy, netidx_derive::Pack)]
pub struct SelfCall {
    pub bind: BindId,
    pub tail_loop: bool,
}

impl<R: Rt> GXLambda<R> {
    /// The types an instance takes from its definition's check; `None`
    /// for the definition's own check, which checks the body. An
    /// instance its definition's check cannot type is a compiler bug.
    fn instance_types(&mut self, ctx: &CompileCtx<R>) -> Result<Option<InstanceTypes>> {
        // CR claude for claude: [structure/low] `Typing::Prepared` wraps another
        // `Typing`, so a prepared `Prepared`, and a `Prepared` reaching
        // `Typing::instance_types`, are states the type allows and the code rules out
        // by hand: this swap through a `Typing::Restored` placeholder with its
        // `unreachable!()`, the `Prepared` arm of `Typing::instance_types` that nothing
        // reaches, and the `(None, Typing::Substituted(..))` arm in `GXLambda::new` (a
        // substituted typing gives types or an error, never `None`). Hold the
        // construction's map beside the typing, `prepared: Option<Box<InstanceTypes>>`,
        // taken by the first check, and delete the variant and the three arms. Found by
        // reading; not run. (c-lambda-05)
        if matches!(self.typing, Typing::Prepared(_)) {
            let Typing::Prepared(prepared) =
                mem::replace(&mut self.typing, Typing::Restored)
            else {
                unreachable!()
            };
            let (typing, types) = *prepared;
            self.typing = typing;
            return Ok(Some(types));
        }
        self.typing.instance_types(ctx, self.id, &self.typ)
    }
}

impl Typing {
    fn instance_types<R: Rt>(
        &self,
        ctx: &CompileCtx<R>,
        id: LambdaId,
        typ: &FnType,
    ) -> Result<Option<InstanceTypes>> {
        let (table, def) = match self {
            Typing::Definition => return Ok(None),
            Typing::Restored => {
                bail!("a restored instance of {:?} is checked again", id)
            }
            Typing::Substituted(table, def) => (table, def),
            Typing::Prepared(prepared) => return Ok(Some(prepared.1.clone())),
        };
        let Some(tables) = table.get() else {
            bail!("an instance of {:?}, whose definition's check recorded no types", id)
        };
        match InstanceTypes::new(ctx, tables.clone(), def, typ) {
            Some(types) => Ok(Some(types)),
            None => crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                bail!("an instance at {} of a definition typed {def}", typ)
            }),
        }
    }
}

impl<R: Rt> Apply<R> for GXLambda<R> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.id.encode(buf)?;
        self.instance_id.encode(buf)?;
        image::slice_encode(&self.args, buf)?;
        self.typ.encode(buf)?;
        self.body.image_encode(buf)?;
        self.self_call.lock().encode(buf)?;
        lexical_encode(&self.env, buf)
    }

    fn view(&self) -> ApplyView<'_, R> {
        ApplyView::Lambda(self)
    }

    fn for_each_body_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.body)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let woke = self.slept.take();
        let first = mem::replace(&mut self.first_dispatch, false);
        // the formals' value channel seeds from a quiet arg production on
        // the first dispatch and after a wake
        let root = if first || woke { QuietAtRoot::Stand } else { QuietAtRoot::Skip };
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            let tv = arg.update(ctx);
            publish_production(ctx, Feeds::Pattern(pat), tv, root);
        }
        // an interrupted dispatch is not a bottom: it rides its last result
        // CR claude for claude: [bug/high] After a stop (a budget or the interrupt)
        // this dispatch rides its last result, but the rest of the cycle's update pass
        // still runs: a node fed by an input that fired and by a ridden call fires with
        // a pair of values the program never computes, and the effects below it act. A
        // file write lands with `account 3 balance 200` where account 3's balance is
        // 300, println prints the same, and with fusion on `send_batch` still sends the
        // cut cycle's batch, which the shell shows. Once the runtime is stopped nothing
        // should fire: effect builtins, `<-` and raises return quietly when the control
        // is stopped, and the cut cycle's batch is dropped; pin it with a stop
        // mid-cycle and a write after it. probe:
        // design/review-2026-11-16/repro/x-recursion-stop-torn-effects.gx
        // (x-recursion-01)
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
        ctx: &mut CompileCtx<R>,
        args: &mut [Node<R>],
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
                    let body = self.body.typ().clone();
                    let rtype = &self.typ.rtype;
                    // CR claude for claude: [bug/medium] A `let rec` whose body nests
                    // its own result in a tuple, a struct or an array (`0 => null, _ =>
                    // (n, f(n - 1))`) is refused here with `type mismatch null does not
                    // contain [null, (i64, null)]`: the return cell took the first
                    // arm's type and the second no longer fits. The same function over
                    // variants gets the refusal that names the fix, a named recursive
                    // type and an annotation, and with `type P = [null, (i64, P)]` and
                    // `-> P` this one checks. When the return was not written and the
                    // body's type holds the return's type as a proper part, tell that
                    // refusal here too, and pin it. probe:
                    // design/review-2026-11-16/repro/x-recursion-rec-return-mismatch.gx
                    // (x-recursion-04)
                    let contain =
                        rtype.check_contains_rigid(&ctx.env, &body).map_err(|e| {
                            let method = ctx
                                .lambda_defs
                                .get(&self.id)
                                .and_then(|d| d.downcast_ref::<LambdaDef<R>>())
                                .is_some_and(|d| method_value(&d.scope));
                            match method {
                                true => e,
                                false => picked_return(&ctx.env, rtype, &body, e),
                            }
                        });
                    wrap!(self.body, contain)?;
                    let top = self.body.spec().id;
                    wrap!(
                        self.body,
                        super::callsite::check_hidden_defaults(
                            ctx,
                            BitFlags::empty(),
                            &Scope::root(),
                            top,
                            &self.typ.rtype,
                            &body,
                            super::callsite::Narrowing::Copy
                        )
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
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        _resolved: &FnType,
    ) -> Result<()> {
        let _elaboration = profile::elaboration(self.instance_id);
        elab_audit::enter();
        let env = self.env.clone();
        let body = &mut self.body;
        let res = ctx.with_restored(env, |ctx| wrap!(body, body.typecheck1(ctx)));
        elab_audit::leave(ctx.def_gate_depth, "typecheck1", self.body.spec(), &res);
        res
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        let recursion =
            callsite.recursion_callback().or_else(|| callsite.shared_loop_callback(self));
        let res = match self.body.view() {
            NodeView::MapQ(map) => map.emit_clif_call(callsite, recursion, cx)?,
            NodeView::FoldQ(fold) => fold.emit_clif_call(callsite, recursion, cx)?,
            _ => None,
        };
        // the loop emits the lambda's own return shape; a callsite widened
        // to a union must hand its consumers a Value pair
        match res {
            Some(cv)
                if call_result_needs_value_widening(
                    cx,
                    callsite.typ(),
                    &self.typ.rtype,
                )? =>
            {
                Ok(Some(widen_result_to_value(cx, &self.typ.rtype, cv)?))
            }
            res => Ok(res),
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        if dbgenv::gxdbg_instance_fusion() {
            let before = ctx.fusion.stats.failed.len();
            let fused_before = ctx.fusion.stats.fused;
            let r = fusion::fuse_instance_body(self, ctx);
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
        fusion::fuse_instance_body(self, ctx)
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if self.charged.take() == Some(Arc::as_ptr(&ctx.control) as usize) {
            ctx.control.activation_died()
        }
        self.body.delete(ctx);
        for n in &self.args {
            n.ids(&mut |id| {
                ctx.fn_forward_resolutions.remove(&id);
            });
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        // a recursion shrinking one level does not shrink the external
        // calls it made
        super::deselecting_arm(false, || self.body.sleep(ctx));
    }
}

impl<R: Rt> GXLambda<R> {
    fn with(
        id: LambdaId,
        instance_id: LambdaInstanceId,
        args: Box<[StructPatternNode]>,
        body: Node<R>,
        typ: Arc<FnType>,
        self_call: Option<SelfCall>,
        env: Lexical,
        typing: Typing,
        charged: Option<usize>,
    ) -> Self {
        Self {
            slept: WakeBit::default(),
            id,
            instance_id,
            args,
            body,
            typ,
            self_call: Mutex::new(self_call),
            resident: TagValue::phantom(),
            first_dispatch: true,
            env,
            typing,
            charged,
        }
    }

    pub(super) fn new(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        id: LambdaId,
        typ: Arc<FnType>,
        typing: Typing,
        argspec: Arc<[Arg]>,
        args: &[Node<R>],
        scope: &Scope,
        tid: ExprId,
        body: Expr,
    ) -> Result<Self> {
        let origin = body.ori.clone();
        let instance_id = LambdaInstanceId::new();
        // a construction may bring the instance's types made already
        let (typing, prepared) = match typing {
            Typing::Prepared(prepared) => {
                let (typing, types) = *prepared;
                (typing, Some(types))
            }
            typing => (typing, None),
        };
        let types = match prepared {
            Some(types) => Some(types),
            None => {
                let mut p = profile::phase(Phase::InstanceCheck);
                profile::instance(&mut p, instance_id, id, &body);
                elab_audit::enter();
                let res = typing.instance_types(ctx, id, &typ);
                elab_audit::leave(ctx.def_gate_depth, "instance-signature", &body, &res);
                res?
            }
        };
        // what a type the body writes names: the instance's variables,
        // under the definition's names
        let written = match (&types, &typing) {
            (Some(types), Typing::Substituted(_, def)) => {
                written_tvars(def, Some(&types.known.lock()))
            }
            (None, Typing::Substituted(_, def)) => {
                match instance_cells(ctx, def, &typ, &[]) {
                    Some(known) => written_tvars(def, Some(&known)),
                    None => LPooled::take(),
                }
            }
            (_, _) => written_tvars(&typ, None),
        };
        let own = typ.clone();
        let build = |ctx: &mut CompileCtx<R>, _: &[StructPatternNode]| {
            let outer = ctx.body.as_mut().map(|b| {
                let around = &b.around;
                let declared = match written.is_empty() {
                    true => around.declared.clone(),
                    false => {
                        around.declared.iter().chain(written.iter()).cloned().collect()
                    }
                };
                let instances = around.instances.iter().chain([&own]).cloned().collect();
                (
                    mem::replace(&mut b.types, types.clone()),
                    mem::replace(&mut b.around, Around { declared, instances }),
                )
            });
            let body = compile(ctx, flags, body, scope, tid);
            if let Some((types, around)) = outer {
                let b = ctx.body.as_mut().expect("compiling body");
                b.types = types;
                b.around = around;
            }
            body
        };
        let instance = Self::new_with_body(
            ctx,
            id,
            instance_id,
            typ,
            typing,
            argspec,
            args,
            scope,
            origin,
            build,
        )?;
        Ok(match types {
            Some(types) => Self {
                typing: Typing::Prepared(Box::new((instance.typing, types))),
                ..instance
            },
            None => instance,
        })
    }

    // CR claude for eric: [perf/medium] Every run-time instance of a body that holds a
    // collection call (a recursive activation, a slot's callback) keeps about 17-25 KB
    // for that call and spends about 100 us building it in the debug build, whether or
    // not the call is ever dispatched; an ordinary user function call in the same place
    // costs about 2.5 KB. Plain recursion measures 11-13 KB a level, 29-33 KB with one
    // unreached array::map in its other arm, 47 KB with two unreached folds; a stateful
    // slot 8-10 KB, 31-34 KB with one nested array::map. So a 100k-row map whose
    // callback does not fuse and holds one nested collection call keeps about 3 GB, and
    // a 200k-deep recursion through fold/map callbacks under --no-fusion passes 6 GB
    // where the fused twin answers at once. The callee is built with its enclosing
    // instance (Callee::Static), prototype slot and call included. Building it at the
    // site's first dispatch in a run-time instance, as Callee::Imaged does, or sharing
    // the defining instance's prototype instead of building one per instance, would
    // remove the cost for calls that never run; which part holds the bytes is not
    // measured. probe:
    // design/review-2026-11-16/repro/x-engine-collections-collection-call-instance-cost.py
    // (x-engine-collections-01)
    pub(super) fn new_collection(
        ctx: &mut CompileCtx<R>,
        id: LambdaId,
        typ: Arc<FnType>,
        typing: Typing,
        argspec: Arc<[Arg]>,
        args: &[Node<R>],
        scope: &Scope,
        tid: ExprId,
        spec: Arc<Expr>,
        intrinsic: CollectionIntrinsic,
        prototype: ExprId,
    ) -> Result<Self> {
        let origin = spec.ori.clone();
        Self::new_with_body(
            ctx,
            id,
            LambdaInstanceId::new(),
            typ.clone(),
            typing,
            argspec,
            args,
            scope,
            origin,
            |ctx, argpats| {
                intrinsic.build(ctx, spec, scope, tid, &typ, argpats, prototype)
            },
        )
    }

    fn new_with_body(
        ctx: &mut CompileCtx<R>,
        id: LambdaId,
        instance_id: LambdaInstanceId,
        typ: Arc<FnType>,
        typing: Typing,
        argspec: Arc<[Arg]>,
        args: &[Node<R>],
        scope: &Scope,
        origin: Arc<Origin>,
        build_body: impl FnOnce(&mut CompileCtx<R>, &[StructPatternNode]) -> Result<Node<R>>,
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
        let binds = ctx.env.lexical.binds.clone();
        let mut argpats: LPooled<Vec<StructPatternNode>> = LPooled::take();
        for (a, atyp) in argspec.iter().zip(typ.args.iter()) {
            {
                let ids = expr::StructurePattern::abstracts_at(&ctx.env, &scope.lexical);
                if let Some(tv) = a.pattern.meets_rigid(&ctx.env, &ids, &atyp.typ)? {
                    let (pat, typ) = (&a.pattern, &atyp.typ);
                    let e = rigid_refusal(&ctx.env, &ids, pat, typ, &tv, Holder::Formal);
                    return Err(e);
                }
            }
            // under a check by bounds a parameter taken apart is a
            // select of one arm over it
            if bounds::active()
                && a.pattern.single_bind().is_none()
                && !matches!(a.pattern, expr::StructurePattern::Ignore)
            {
                if a.constraint.is_some() {
                    StructPatternNode::named_before(ctx, &atyp.typ)?
                }
                let pattern = StructPatternNode::compile_shape(
                    ctx,
                    &a.pattern,
                    Holder::Formal,
                    scope,
                    a.pos.0,
                    origin.clone(),
                )?;
                let parts = pattern.by_bounds(ctx)?;
                super::pattern::destructure(ctx, parts, &atyp.typ, a.pos.0)?;
                argpats.push(pattern);
                continue;
            }
            let pattern = StructPatternNode::compile(
                ctx,
                &atyp.typ,
                &a.pattern,
                Holder::Formal,
                scope,
                a.pos.0,
                origin.clone(),
            )?;
            // CR claude for claude: [bug/medium] `covers` answers false for any union,
            // so a formal's tuple or struct pattern over a union of same-shape members
            // is refused as refutable. `array::map([(1, `A), (2, `B(1))], |(i, v)| i)`
            // is refused "refutable patterns are not allowed in lambda arguments (i,
            // v)", and so is `|{a, b}|` over a union of structs. Yet `let (i, v) = p`
            // over the same value checks, because the let judges each member (bind.rs
            // `fits_every_member`, `retype_by_member`). Fix: judge the formal the way
            // the let does, and pin this message, which no test names today. probe:
            // design/review-2026-11-16/repro/x-kind-markers-formal-union-of-tuples.sh
            // Also reported as (x-rerun-3-01). (x-kind-markers-03)
            if !pattern.covers(&ctx.env, &atyp.typ, false) {
                bail!(
                    "refutable patterns are not allowed in lambda arguments {}",
                    a.pattern
                )
            }
            argpats.push(pattern);
        }
        let mut p = profile::phase(Phase::InstanceGraph);
        let body = super::prune::Prune::compile(ctx, |ctx| build_body(ctx, &argpats))?;
        profile::instance(&mut p, instance_id, id, body.spec());
        drop(p);
        Ok(Self::with(
            id,
            instance_id,
            Box::from_iter(argpats.drain(..)),
            body,
            typ,
            None,
            Lexical { binds, ..ctx.env.lexical() },
            typing,
            crate::stack::activation_born(),
        ))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let id = LambdaId::decode(buf)?;
        let instance_id = LambdaInstanceId::decode(buf)?;
        let args = Vec::<StructPatternNode>::decode(buf)?.into_boxed_slice();
        let typ: Arc<FnType> = Pack::decode(buf)?;
        let body = decode_node(ctx, buf)?;
        let self_call = Option::<SelfCall>::decode(buf)?;
        let env = lexical_decode(buf)?;
        // an image holds compile-time instances, which no cycle charged
        Ok(Self::with(
            id,
            instance_id,
            args,
            body,
            typ,
            self_call,
            env,
            Typing::Restored,
            None,
        ))
    }
}

#[derive(Debug)]
pub(crate) struct BuiltInLambda<R: Rt> {
    typ: Arc<FnType>,
    name: ArcStr,
    apply: Box<dyn Apply<R> + Send + Sync + 'static>,
}

impl<R: Rt> BuiltInLambda<R> {
    fn with(
        typ: Arc<FnType>,
        name: ArcStr,
        apply: Box<dyn Apply<R> + Send + Sync + 'static>,
    ) -> Self {
        Self { typ, name, apply }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let typ = Arc::new(FnType::decode(buf)?);
        let name = ArcStr::decode(buf)?;
        let decode = ctx.builtin_decoder(&name).ok_or_else(|| {
            log::warn!("the image names an unregistered builtin {name}");
            PackError::InvalidFormat
        })?;
        let apply = decode(ctx, from, buf)?;
        Ok(Self::with(typ, name, apply))
    }
}

/// Stands in for a builtin this binary does not have, under an IDE
/// check: a package under development declares builtins only its own
/// build registers. Like any builtin it is typed by its declared
/// signature alone; it never produces, and the check never runs it.
#[derive(Debug)]
struct UnknownBuiltIn(TagValue);

impl UnknownBuiltIn {
    fn init<R: Rt>(
        _: &mut CompileCtx<R>,
        _: &FnType,
        _: Option<&FnType>,
        _: &Scope,
        _: &[Node<R>],
        _: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self(TagValue::phantom())))
    }
}

impl<R: Rt> Apply<R> for UnknownBuiltIn {
    fn update(&mut self, _: &mut ExecCtx<'_, R>, _: &mut [Node<R>]) -> &TagValue {
        &self.0
    }

    fn image_encode(&self, _: &mut ImageBuf) -> Result<(), PackError> {
        Err(PackError::Application(crate::image::NOT_IMAGED))
    }

    fn sleep(&mut self, _: &mut ExecCtx<'_, R>) {}
}

impl<R: Rt> Apply<R> for BuiltInLambda<R> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.typ.encode(buf)?;
        self.name.encode(buf)?;
        self.apply.image_encode(buf)
    }

    /// Fusion sees the wrapped builtin's own view, named.
    fn view(&self) -> ApplyView<'_, R> {
        match self.apply.view() {
            ApplyView::BuiltIn(_) | ApplyView::Opaque => ApplyView::BuiltIn(&self.name),
            v => v,
        }
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        // the trait default's `Ok(None)` would silently de-fuse every builtin
        self.apply.emit_clif(callsite, cx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.apply.fuse(ctx)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        self.apply.update(ctx, from)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R>,
        args: &mut [Node<R>],
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
        ctx: &mut CompileCtx<R>,
        args: &mut [Node<R>],
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.apply.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.apply.sleep(ctx);
    }

    fn reborn(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.apply.reborn(ctx);
    }
}

#[derive(Debug)]
pub struct Lambda {
    spec: Arc<Expr>,
    def: Value,
    typ: Type,
    /// A binding names it (a `let` of it, or of a data literal holding
    /// it): its gate generalizes its signature. Anywhere else its
    /// signature is one instance, its enclosing level's.
    pub(crate) named: bool,
    /// A lambda literal is a constant: present from birth (see Constant).
    resident: TagValue,
}

impl Lambda {
    /// The definition's `LambdaId`.
    pub fn lambda_id<R: Rt>(&self) -> Option<LambdaId> {
        self.def.downcast_ref::<LambdaDef<R>>().map(|d| d.id)
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
pub(crate) fn make_init<R: Rt>(
    id: LambdaId,
    flags: BitFlags<CFlag>,
    def_env: Lexical,
    scope: &Scope,
    def_typ: Arc<FnType>,
    def_argspec: Arc<[Arg]>,
    def_spec: Arc<Expr>,
    body: DefBody,
    enclosing: Around,
    table: SArc<DefTables>,
    shared: BodyShare,
) -> InitFn<R> {
    let def_scope = scope.append_place("fn", &def_spec, None);
    SArc::new(move |scope, ctx, args, mode, tid| {
        let env = match mode {
            BindMode::Definition => def_env.clone(),
            BindMode::Static { .. } => {
                let mut env = def_env.called_in(scope.load.as_ref());
                ctx.env.name_kept_impls(&mut env);
                env
            }
            BindMode::Dynamic(_, kept) => {
                let mut env = def_env.called_in(scope.load.as_ref());
                if let Some(kept) = kept {
                    kept.name_in(&mut env)
                }
                env
            }
        };
        // the definition's names, the call site's handlers
        let scope = Scope {
            dynamic: scope.dynamic.clone(),
            lexical: def_scope.lexical.clone(),
            load: env.body.clone(),
        };
        ctx.with_restored(env, |ctx| match &body {
            DefBody::Expr(body) => {
                let compiling = BodyCompile {
                    share: shared.clone(),
                    checking: matches!(mode, BindMode::Definition),
                    types: None,
                    around: enclosing.clone(),
                };
                let outer = ctx.body.replace(compiling);
                let instance =
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
                    });
                ctx.body = outer;
                instance
            }
            DefBody::Collection(intrinsic) => {
                let prototype = *shared.lock().prototype.get_or_insert_with(ExprId::new);
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
                        prototype,
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
                    BindMode::Static { .. } | BindMode::Dynamic(..) => Some(&*typ),
                };
                let apply = init(ctx, &def_typ, resolved, &scope, args, tid)?;
                let builtin = BuiltInLambda::with(typ, name.clone(), apply);
                Ok(Box::new(builtin) as Box<_>)
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
fn instantiate<R: Rt>(
    ctx: &mut CompileCtx<R>,
    mode: BindMode<'_>,
    def_typ: &Arc<FnType>,
    table: &SArc<DefTables>,
    build: impl FnOnce(&mut CompileCtx<R>, Arc<FnType>, Typing) -> Result<GXLambda<R>>,
) -> Result<Box<dyn Apply<R>>> {
    let typing = match mode {
        BindMode::Definition => Typing::Definition,
        BindMode::Static { copies: Some(copies), .. }
            if let Some(types) = InstanceTypes::copied(ctx, table, def_typ, copies) =>
        {
            let substituted = Typing::Substituted(table.clone(), def_typ.clone());
            Typing::Prepared(Box::new((substituted, types)))
        }
        BindMode::Static { .. } | BindMode::Dynamic(..) => {
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
fn instance_type<R: Rt>(
    ctx: &mut CompileCtx<R>,
    mode: BindMode<'_>,
    def_typ: &Arc<FnType>,
) -> Result<Arc<FnType>> {
    Ok(match mode {
        BindMode::Static { instance, .. } => Arc::new(instance.clone()),
        BindMode::Dynamic(r, _) if same_parameters(r, def_typ) => Arc::new(r.clone()),
        BindMode::Dynamic(r, _) => Arc::new(fit_callee(ctx, r, def_typ)?),
        BindMode::Definition => def_typ.clone(),
    })
}

/// A call's copy of the definition's signature `def_typ`, fitted to the
/// site's view `site` of it as a call fits it: the one rule a static and
/// a dynamic bind both take when the views list other parameters.
pub(crate) fn fit_callee<R: Rt>(
    ctx: &CompileCtx<R>,
    site: &FnType,
    def_typ: &FnType,
) -> Result<FnType> {
    // a definition a check by bounds made generic: the instance the
    // site's view decides, with its types; under a check by bounds one it
    // decided whole is held to the view by the same relation
    let scheme = Type::Fn(Arc::new(def_typ.clone()));
    if bounds::scheme(&scheme) || (ctx.bounds && !scheme.has_unbound()) {
        let view = Type::Fn(Arc::new(site.clone()));
        let mut known = bounds::instance(&ctx.env, &scheme, &view, &[])?;
        return match &bounds::specialize(&scheme, &mut known) {
            Type::Fn(ft) => Ok((**ft).clone()),
            _ => bail!("BUG: a function type's instance is no function type"),
        };
    }
    let copy = def_typ.instantiate(&ctx.rec_defs);
    site.check_contains(&ctx.env, &copy)?;
    Ok(copy)
}

impl Lambda {
    pub(crate) fn compile<R: Rt>(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        l: &expr::LambdaExpr,
        _top_id: ExprId,
    ) -> Result<Node<R>> {
        let mut flags = flags;
        flags.remove(CFlag::SeqStep);
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
        let ctor_bound = |tc: &Type| {
            Type::is_ctor_trait_bound(&ctx.env, &tc.scope_refs(&scope.lexical))
        };
        for (tv, tc) in l.constraints.iter() {
            let bounds = || l.constraints.iter().filter(|(t, _)| t.name == tv.name);
            if !ctor_bound(tc)
                || ctors.iter().any(|(n, _)| *n == tv.name)
                || !bounds().all(|(_, tc)| ctor_bound(tc))
            {
                continue;
            }
            let elem =
                TVar::empty_named(format_compact!("{}#elem", tv.name).as_str().into());
            ctors.push((tv.name.clone(), Type::TVar(elem)));
        }
        // a written type, as the definition's signature holds it
        let minted = MethodCells::current();
        let written = |env: &Env, t: &Type| -> Result<Type> {
            let mut copies: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            copies.extend(minted.iter().map(|(a, tv)| (*a, tv.clone())));
            Ok(t.scope_refs_with(&scope.lexical, &mut copies)
                .rewrite_trait_args(env)?
                .apply_ctor_quantifiers(&ctors))
        };
        let signature = l.args.iter().filter_map(|a| a.constraint.as_ref());
        let signature = signature
            .chain(l.rtype.iter())
            .chain(l.throws.iter())
            .chain(l.constraints.iter().map(|(_, tc)| tc));
        let constrained: OnceCell<LPooled<AHashSet<ArcStr>>> = OnceCell::new();
        let outer: OnceCell<LPooled<AHashMap<ArcStr, TVar>>> = OnceCell::new();
        let own = |tv: &TVar| {
            constrained
                .get_or_init(|| {
                    l.constraints.iter().map(|(c, _)| c.name.clone()).collect()
                })
                .contains(&tv.name)
                || !outer
                    .get_or_init(|| {
                        let mut known = LPooled::take();
                        enclosing_tvars(ctx, &mut known);
                        known
                    })
                    .contains_key(&tv.name)
        };
        let mut union = None;
        for t in signature {
            crate::warn_written(ctx, flags, &t.scope_refs(&scope.lexical), &mut |u| {
                if union.is_none() {
                    union = VariableUnion::of(u, &own)
                }
            })?;
        }
        // a lambda in a method's value is judged at its method
        // (traits::written_method_unions), where the method's own lambda is
        // told from what its trait fills in
        if let Some(e) = union
            && !method_value(scope)
        {
            return Err(e.into());
        }
        if l.vargs.is_some() && matches!(l.body, LambdaBody::Expr(_)) {
            bail!("a variadic argument (`@args`) is for builtins only")
        }
        // a trait as a parameter's type is a fresh bounded quantifier
        // (`|s: Read|` ≡ `'s: Read |s: 's|`), joined to the declared ones
        // so the def gate holds it rigid
        let mut trait_quantifiers: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        let mut param = |env: &Env, name: ArcStr, typ: &Type| -> Result<Type> {
            match &typ.scope_refs(&scope.lexical) {
                // CR claude for claude: [bug/medium] A trait written as a parameter's
                // type (`|x: Show|`, lowered here) or as a declared bound (`'a: Show`)
                // records no IDE site. Hover and definition there answer nothing.
                // References of `Show` list the `use`, the `Show::show` prefixes, the
                // declaration and the impl head, but not these sites. `Impl::compile`
                // records an impl head's trait as a `ModuleRefSite`. Fix: record one
                // the same way wherever a written type or bound resolves to a trait,
                // both here and where a declared quantifier's bound is resolved. probe:
                // design/review-2026-11-16/repro/lsp-trait-in-type-position.py (lsp-03)
                t @ Type::Ref(tr) if env.settled_trait_of_ref(tr)?.is_some() => {
                    let tv = TVar::empty_named(name);
                    trait_quantifiers.push((tv.clone(), t.clone()));
                    Ok(Type::trait_param(env, tv, tr))
                }
                _ => written(env, typ),
            }
        };
        let vargs = match l.vargs.as_ref() {
            None => None,
            Some(None) => Some(None),
            Some(Some(t)) => Some(Some(param(&ctx.env, arcstr::literal!("#args"), t)?)),
        };
        let rtype = l.rtype.as_ref().map(|t| written(&ctx.env, t)).transpose()?;
        let throws = l.throws.as_ref().map(|t| written(&ctx.env, t)).transpose()?;
        let mut argspec: LPooled<Vec<Arg>> = LPooled::take();
        for (i, a) in l.args.iter().enumerate() {
            let constraint = match &a.constraint {
                None => None,
                Some(typ) => {
                    let name: ArcStr = match a.pattern.single_bind() {
                        Some(n) => format_compact!("#{n}").as_str().into(),
                        None => format_compact!("#arg{i}").as_str().into(),
                    };
                    Some(param(&ctx.env, name, typ)?)
                }
            };
            argspec.push(Arg {
                comments: a.comments.clone(),
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
        check_bounds_acyclic(&constraints)?;
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
            // CR claude for eric: [bug/high] Only a dynamic module's body is refused a
            // `'name` builtin body, so any script binds any registered builtin under a
            // signature of its own, and nothing compares that signature with what the
            // builtin returns. Fused code reads the result as the declared type: `|x:
            // string| -> i64 'str_trim` yields a heap address where the node-walk logs
            // and bottoms, and `|s: string| -> string 'str_len` aborts the process in
            // graphix_shape_mismatch. A package author's wrong return type fails the
            // same way. Refuse a builtin body outside the package that registered the
            // name (tests opt in), check a scalar fast call's returned kind in the
            // kernel as the Value path does and bottom with a logged failure, and have
            // `run!` audit every builtin value against its site's resolved return type.
            // probe: design/review-2026-11-16/repro/a-stdlib-builtin-sig-trusted.gx
            // (a-stdlib-01)
            if flags.contains(CFlag::NoBuiltins) {
                bail!("defining builtins is not allowed in this context")
            }
            // CR claude for eric: [structure/medium] A builtin's signature is written
            // twice by rule: the binding lambda must annotate every argument and the
            // return here (`let trim = |s: string| -> string 'str_trim`), and the
            // package's `.gxi` declares the same type again (`val trim: fn(s: string)
            // -> string`). The stdlib holds about 420 such pairs and netidx-admin 74,
            // and the scaffold `graphix package create` writes starts a new package
            // with two. Where the module's interface declares the val, let the binding
            // take its type from it (`let trim = 'str_trim`), keeping the annotated
            // lambda for a builtin with labeled defaults, which an interface cannot
            // state. Found by reading; not run. (a-packages-05)
            if argspec.iter().any(|a| a.constraint.is_none())
                || matches!(l.vargs, Some(None))
            {
                bail!(
                    "builtin function {builtin} requires all arguments to have type annotations"
                )
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
            let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            enclosing_tvars(ctx, &mut known);
            known.retain(|name, _| {
                !constraints.iter().any(|(tv, _)| tv.name == *name)
                    && !ctors
                        .iter()
                        .any(|(_, e)| matches!(e, Type::TVar(e) if e.name == *name))
            });
            // a variable the definitions around this one declare, and this
            // one does not, is theirs
            ft.alias_tvars(&mut known);
            for (_, tc) in constraints.iter() {
                tc.alias_tvars(&mut known);
            }
            let ft = ft.declaring(&constraints)?;
            ft.check_quantifiers_held()?;
            ft
        };
        typ.lambda_ids.set_id(id);
        typ.claim(level);
        let table = SArc::new(DefTables::default());
        let (shared, enclosing) = match &ctx.body {
            Some(outer) => {
                let share =
                    outer.share.lock().literals.entry(spec.id).or_default().clone();
                share.lock().nested = true;
                (share, outer.around.clone())
            }
            None => (BodyShare::default(), Around::nothing()),
        };
        let facts = match &body {
            DefBody::Expr(_) | DefBody::Collection(_) => LambdaFacts::PURE,
            DefBody::BuiltIn(name) => ctx.builtin_effect(name).facts(),
        };
        // No signature ref seeding here: the module tree is mid-registration
        // and a name's final target may not be registered yet. Cells fill
        // at typecheck.
        let def = ctx.registry.lambdawrap.wrap(LambdaDef::of_source(
            id,
            ctx.env.lexical(),
            scope.clone(),
            argspec,
            typ.clone(),
            table,
            shared,
            facts,
            ParamCalls::default(),
            RecursionKind::NotRecursive,
            spec.id,
            level,
            body,
            flags,
            spec.clone(),
            enclosing,
        ));
        ctx.lambda_defs.insert(id, def.clone());
        Ok(Self::with(spec, def, Type::Fn(typ)))
    }
}

impl Lambda {
    pub(crate) fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let id = LambdaId::decode(buf)?;
        let typ = Type::decode(buf)?;
        let def = ctx.lambda_defs.get(&id).ok_or(PackError::InvalidFormat)?.clone();
        Ok(Self::with(spec, def, typ))
    }

    fn with<R: Rt>(spec: Arc<Expr>, def: Value, typ: Type) -> Node<R> {
        Node::new(Self {
            spec,
            def: def.clone(),
            typ,
            named: false,
            resident: TagValue::stale(def),
        })
    }
}

/// A definition's gate: its body checks once, over a `Nop` per declared
/// argument, raising to a faux catch that collects its throws. Its
/// declared tvars are rigid (the body must be well-typed for any 'a;
/// anonymous '_N inference cells stay bindable) and a self-call knots to
/// its own cells (`CompileCtx::rec_defs`). Every path leaves by `close`.
struct DefGate<R: Rt> {
    def: LambdaId,
    depth: u32,
    /// The level the definition stands at, where an unnamed one's
    /// signature belongs.
    outer: Level,
    _at: AtLevel,
    sig: Arc<FnType>,
    faux: RaiseProbe,
    args: LPooled<Vec<Node<R>>>,
    scope: Scope,
    rigid: LPooled<Vec<RigidGate>>,
    _declaring: Declaring,
    /// Each quantifier the definition declares, at the depth it had at the
    /// gate's opening.
    declared: LPooled<Vec<(TVar, u32)>>,
    /// The parts of the signature left to inference (a parameter by its
    /// name, the return as `None`) whose cell is outside the definition at
    /// the gate's opening: a function something writes is one instance.
    unwritten: LPooled<Vec<(Option<ArcStr>, TVar)>>,
    /// Under a check by bounds, how the definition closes.
    bounds: Option<Closing>,
}

/// How a definition's check by bounds closes.
#[derive(Clone, Copy)]
enum Closing {
    /// A lambda no binding names that declares no variable: checked at
    /// the level around it, as any expression there.
    Inline,
    /// A definition of its own: the cells the check met since `mark` are
    /// fixed, all but its signature's, which are the scheme's. Where no
    /// binding names it, it then stands as one instance of that scheme.
    Scheme { mark: usize, named: bool },
}

impl<R: Rt> DefGate<R> {
    fn open(ctx: &mut CompileCtx<R>, def: &LambdaDef<R>, binding_names: bool) -> Self {
        let outer = current_level();
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        def.typ.collect_tvars_through_bounds(&mut named);
        named.retain(|name, _| !name.starts_with('_'));
        let bounds = bounds::active().then(|| match binding_names || !named.is_empty() {
            true => Closing::Scheme { mark: solving(|s| s.mark()), named: binding_names },
            false => Closing::Inline,
        });
        let at = match bounds {
            Some(Closing::Inline) => {
                def.typ.claim(outer);
                AtLevel::enter(outer)
            }
            _ => AtLevel::enter(def.level),
        };
        let args = def.typ.args.iter().map(|at| Nop::new(at.typ.clone())).collect();
        let (faux, scope) = RaiseProbe::open(ctx, &def.scope);
        let mut seen: LPooled<AddrSet> = LPooled::take();
        for tv in named.values() {
            Type::TVar(tv.clone()).seed_names_seen(&ctx.env, &mut seen);
        }
        let rigid = named.values().map(|tv| tv.open_rigid()).collect();
        let quantified = def.typ.quantifiers.iter().filter_map(|q| named.get(q));
        let declaring =
            Declaring::enter(named.values().cloned(), quantified.clone().cloned());
        let declared = quantified.map(|tv| (tv.clone(), tv.level().depth())).collect();
        let outside = |t: &Type| match t {
            Type::TVar(c)
                if c.name.starts_with('_') && c.level().depth() < def.level.depth() =>
            {
                Some(c.clone())
            }
            _ => None,
        };
        let unwritten = def
            .typ
            .args
            .iter()
            .filter_map(|a| Some((Some(a.name()?.clone()), outside(&a.typ)?)))
            .chain(outside(&def.typ.rtype).map(|c| (None, c)))
            .collect();
        ctx.rec_defs.insert(def.id);
        ctx.def_gate_depth += 1;
        ctx.pending_settles.push(crate::SettleFrame::default());
        Self {
            def: def.id,
            depth: def.level.depth(),
            outer,
            _at: at,
            sig: def.typ.clone(),
            faux,
            args,
            scope,
            rigid,
            _declaring: declaring,
            declared,
            unwritten,
            bounds,
        }
    }

    /// A quantifier the definition declares stays its own: the body binds
    /// no cell outside the definition to a type holding it (which would
    /// lower it to that cell's depth), since each call picks it anew. A
    /// variable the signature only names is lowered with that cell, and
    /// the definition is monomorphic in it.
    fn check_declared_kept(&self) -> Result<()> {
        if self.bounds.is_some() {
            return Ok(());
        }
        for (tv, depth) in self.declared.iter() {
            if tv.level().depth() < *depth {
                if let Some(fix) = self.unwritten_holding(tv) {
                    bail!(
                        "'{0} escapes the definition through its own signature: \
                         a function something writes is one instance, so what its \
                         signature leaves unwritten is a cell outside it; {fix}, or \
                         declare no '{0}",
                        tv.name
                    )
                }
                // CR claude for claude: [bug/low] The hint `give that a type holding
                // them all (the bound of 'a, or Any)` points at a type the program
                // cannot write when the bound is Concrete, NoError or Singleton. Those
                // are keywords, so `let y: Concrete = never()` is a parse error ("can't
                // use keyword as a type name").
                // lang::types::a_named_definition_writing_outside_is_told_what_it_hands_out
                // pins this message for exactly `'a: Concrete`. Fix: name the bound
                // only when it is a type a let can carry, say Any otherwise, and pin
                // the annotated twin (`let y: Any = never()` checks). probe:
                // design/review-2026-11-16/repro/tests-types-b-escape-hint-bound.gx
                // (tests-types-b-03)
                bail!(
                    "'{0} escapes the definition: the body hands a value of type '{0} \
                     to something outside it, which would have to hold every '{0} a \
                     call picks; give that a type holding them all (the bound of '{0}, \
                     or Any), or declare no '{0}",
                    tv.name
                )
            }
        }
        Ok(())
    }

    /// The annotation that keeps `tv` the definition's, where a part of the
    /// signature left unwritten holds it.
    fn unwritten_holding(&self, tv: &TVar) -> Option<CompactString> {
        let (part, cell) = self.unwritten.iter().find(|(_, cell)| {
            let mut reached: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            Type::TVar(cell.clone()).reached_cells(&mut reached);
            reached.values().any(|c| c.same_cell(tv))
        })?;
        let written = Type::TVar(cell.clone()).written_form(&mut |c| {
            self.declared
                .iter()
                .find(|(tv, _)| tv.same_cell(c))
                .map(|(tv, _)| Type::TVar(tv.clone()))
        });
        Some(match (part, written) {
            (None, Some(t)) => format_compact!("write its return type, `-> {t}`"),
            (None, None) => format_compact!("write its return type"),
            (Some(name), Some(t)) => format_compact!("write `{name}: {t}`"),
            (Some(name), None) => format_compact!("write `{name}`'s type"),
        })
    }

    /// What the body raised.
    fn thrown(&self, ctx: &CompileCtx<R>) -> Type {
        self.faux.raised(ctx)
    }

    /// The body's sites settle with the enclosing statement, all but the
    /// cells this signature reaches: those stay open, generalized where a
    /// binding names the definition, else its enclosing level's.
    /// Under a check by bounds, where no binding names a definition of
    /// its own, the one instance of its scheme the lambda stands as: its
    /// type, and its cell for each of the scheme's, by that cell's
    /// address.
    fn close(
        mut self,
        ctx: &mut CompileCtx<R>,
        spec: &Expr,
        table: Option<SArc<DefTable>>,
        named: bool,
    ) -> Result<Option<(Type, AHashMap<usize, TVar>)>> {
        let mut frame = ctx.pending_settles.pop().expect("gate settle frame");
        ctx.def_gate_depth -= 1;
        let mut stands = None;
        let mut closed = Ok(());
        match (self.bounds, named) {
            (Some(Closing::Inline), _) => (),
            (Some(Closing::Scheme { mark, named }), _) => {
                let sig = Type::Fn(self.sig.clone());
                closed = solving(|s| s.generalize(&ctx.env, &sig, self.depth - 1, mark))
                    .at(spec);
                self.rigid.clear();
                for (tv, _) in self.declared.iter() {
                    solving(|s| s.own(tv))
                }
                if !named && closed.is_ok() {
                    let mut copies = AHashMap::default();
                    let one = solving(|s| {
                        s.instantiate_into(&sig, self.depth - 1, self.outer, &mut copies)
                    });
                    stands = Some((one, copies))
                }
            }
            (None, true) => {
                ctx.env.with_ide(|ide| self.tell_held(ide, spec));
                self.sig.generalize(self.depth)
            }
            (None, false) => self.sig.claim(self.outer),
        }
        let defaults = table.as_deref().map(DefTable::default_cells).unwrap_or_default();
        for s in frame.iter_mut() {
            if let crate::PendingSettle::Site { sigs, exempt, .. } = s {
                sigs.push(self.sig.clone());
                exempt.extend(defaults.iter().copied());
            }
        }
        let mut sig: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.sig.reached_cells(&mut sig);
        let mut lowered = Ok(());
        if !sig.is_empty() {
            for s in frame.iter() {
                if let crate::PendingSettle::Site { ftype, .. } = s
                    && lowered.is_ok()
                {
                    lowered = ftype.lower_into_kept(&ctx.env, &sig)
                }
            }
        }
        if let Some(table) = table {
            frame.push(crate::PendingSettle::Body {
                table,
                owner: self.def,
                exempt: sig.keys().copied().collect(),
                spec: Arc::new(spec.clone()),
            });
        }
        ctx.pending_settles.last_mut().expect("root settle frame").append(frame);
        ctx.rec_defs.remove(&self.def);
        self.faux.close(ctx);
        self.rigid.clear();
        closed.and(lowered.at(spec)).map(|()| stands)
    }

    /// Tell the editor the quantifiers the close is about to generalize,
    /// which a type shown in the definition's text holds as its own.
    fn tell_held(&self, ide: &mut Ide, spec: &Expr) {
        let Some(end) = spec.end.get() else { return };
        let mut cells = SmallVec::new();
        self.sig.each_quantifier(|name, tv| {
            if let Some(c) = tv.open_cell()
                && !c.level().is_generic()
                && c.level().depth() >= self.depth
            {
                cells.push(c.renamed(name.clone()))
            }
        });
        if !cells.is_empty() {
            ide.held.push(HeldSpan { ori: spec.ori.clone(), pos: spec.pos, end, cells })
        }
    }
}

/// Whether a lambda compiled in `scope` is in a method's value outside
/// every lambda's body: what its signature leaves out is its trait's.
fn method_value(scope: &Scope) -> bool {
    Path::parts(&*scope.lexical)
        .filter(|p| crate::is_fn_block(p) || crate::is_method_block(p))
        .last()
        .is_some_and(crate::is_method_block)
}

/// The refusal of a body whose type the written return would hold were
/// its variables open: each call picks those, a type narrower than a
/// bound included. Told as the annotation that fixes it.
fn picked_return(
    env: &Env,
    rtype: &Type,
    body: &Type,
    e: anyhow::Error,
) -> anyhow::Error {
    if e.downcast_ref::<TypeMismatch>().is_none() {
        return e;
    }
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    rtype.collect_tvars(&mut named);
    let mut picked: LPooled<Vec<TVar>> = named
        .drain()
        .map(|(_, tv)| tv)
        .filter(|tv| !tv.generated() && tv.is_rigid() && tv.binding().is_none())
        .collect();
    if picked.is_empty()
        || !rtype
            .reset_tvars()
            .contains_with_flags(BitFlags::empty(), env, body)
            .unwrap_or(false)
    {
        return e;
    }
    picked.sort_by(|a, b| a.name.cmp(&b.name));
    crate::format_with_flags(crate::PrintFlag::ReplacePrims, || {
        use std::fmt::Write;
        let mut vars = CompactString::new("");
        for (i, tv) in picked.iter().enumerate() {
            let sep = match i {
                0 => "",
                i if i + 1 == picked.len() => " and ",
                _ => ", ",
            };
            let _ = write!(vars, "{sep}{tv}");
        }
        let s = if picked.len() == 1 { "" } else { "s" };
        let fix = match body.written_form(&mut |_| None) {
            Some(t) => {
                format_compact!("drop `-> {rtype}`, or write the body's type, `-> {t}`")
            }
            None => format_compact!("drop `-> {rtype}`, or write the body's type"),
        };
        anyhow!(
            "{e}: a call picks {vars}, and the body's value need not be of the type{s} \
             it picks; {fix}"
        )
    })
}

/// Whether `t` holds an unbound cell `f` accepts.
fn holds_unbound(t: &Type, f: &impl Fn(&TVar) -> bool) -> bool {
    fn go(t: &Type, f: &impl Fn(&TVar) -> bool, seen: &mut AHashSet<usize>) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                seen.insert(tv.cell_addr())
                    && match tv.binding() {
                        Some(b) => go(&b, f, seen),
                        None => f(tv),
                    }
            }
            t => {
                let mut found = false;
                t.for_each_child(&mut |c| found = found || go(c, f, seen));
                found
            }
        })
    }
    go(t, f, &mut LPooled::take())
}

fn capture_id(
    id: BindId,
    outer: &Lexical,
    checked: &Env,
    captures: &mut AHashSet<(ModPath, CompactString)>,
) {
    if let Some(b) = checked.by_id.get(&id)
        && outer.binds.get(&b.scope).and_then(|bs| bs.get(&b.name)) == Some(&id)
    {
        captures.insert((b.scope.clone(), b.name.clone()));
    }
}

/// The methods of the impls of every trait `captures` names a dispatcher
/// or the anchor of, or names as a bound in a type, which a call through
/// it may reach once the impl is given back.
fn capture_impls<R: Rt>(
    def: &LambdaDef<R>,
    checked: &Env,
    captures: &mut AHashSet<(ModPath, CompactString)>,
) {
    let mut traits: LPooled<AHashSet<TraitId>> = LPooled::take();
    let mut cells: LPooled<AddrSet> = LPooled::take();
    let mut bounds: SmallVec<[TraitId; 2]> = SmallVec::new();
    for (scope, name) in captures.iter() {
        let id = def.env.binds.get(scope).and_then(|bs| bs.get(name));
        traits.extend(id.and_then(|id| checked.trait_of_binding(*id, name)));
        if let Some(b) = id.and_then(|id| checked.by_id.get(id)) {
            b.typ.named_traits(&mut cells, &mut bounds)
        }
    }
    traits.extend(bounds);
    let mut found: LPooled<Vec<(ModPath, CompactString)>> = LPooled::take();
    for t in traits.drain() {
        checked.impl_bindings(t, &def.env, &def.scope.lexical, &mut |s, n| {
            found.push((s.clone(), n.clone()))
        })
    }
    captures.extend(found.drain(..))
}

fn capture_names<R: Rt>(
    node: &Node<R>,
    outer: &Lexical,
    checked: &Env,
    captures: &mut AHashSet<(ModPath, CompactString)>,
) {
    let mut add = |id| capture_id(id, outer, checked, captures);
    match node.view() {
        NodeView::Ref(r) => add(r.id),
        NodeView::Connect(c) => add(c.id),
        NodeView::ConnectDeref(c) => add(c.src_id),
        NodeView::Lambda(l) => {
            if let Some(d) = l.def_value().downcast_ref::<LambdaDef<R>>() {
                d.captured_ids(&mut add);
            }
        }
        _ => (),
    }
}

/// The definition's check of its labeled defaults, under the gate:
/// each default compiles in the def's scope and must fit its parameter.
/// Against a declared tvar it must fit the tvar's constraints, not the
/// variable, since a default is allowed to instantiate the variable at
/// a site that omits the argument (`CallSite::check_omitted_defaults`).
/// A default whose type holds an enclosing definition's declared
/// variable fits its parameter whole: a call instantiates no variable of
/// the definitions around it, and an omitting site outside them sees
/// their schemes (`callsite::Judge`).
fn check_defaults<R: Rt>(
    ctx: &mut CompileCtx<R>,
    def: &LambdaDef<R>,
    scope: &Scope,
    captures: &mut AHashSet<(ModPath, CompactString)>,
    checked: &mut Vec<Node<R>>,
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
        let (node, res) = ctx.with_restored(def.env.clone(), |ctx| {
            let mut node = def.in_signature(ctx, None, |ctx| {
                compile(ctx, flags, expr.clone(), scope, ExprId::new())
            })?;
            super::bind::name_value(&mut node, true);
            let res = judge_default(ctx, def, at, &mut node);
            Ok::<_, anyhow::Error>((node, res))
        })?;
        if let Err(e) = res {
            let e = e.at(&node.spec());
            ctx.discard(node);
            return Err(e);
        }
        fusion::for_each_node(&node, &mut |n| {
            capture_names(n, &def.env, &ctx.env, captures);
        });
        checked.push(node);
    }
    Ok(())
}

/// The default `node` of the parameter `at` checked against it, in the
/// lexical environment the default compiled in.
fn judge_default<R: Rt>(
    ctx: &mut CompileCtx<R>,
    def: &LambdaDef<R>,
    at: &FnArgType,
    node: &mut Node<R>,
) -> Result<()> {
    node.typecheck0(ctx).and_then(|()| {
        let typ = node.typ().clone();
        if bounds::active() {
            // a parameter whose type a call decides takes the default at
            // each call that leaves the label out, and at no other; over a
            // declared variable, some instance of it has to
            let some_instance = |ctx: &mut CompileCtx<R>| match at.typ.has_rigid() {
                true => at.typ.reset_tvars().check_contains(&ctx.env, &typ),
                false => Ok(()),
            };
            return match (at.label(), bounds::cells_in(&at.typ).first()) {
                (Some(label), Some(v)) => {
                    some_instance(ctx)?;
                    bounds::defaulted(v, label.clone(), typ.clone(), at.typ.clone());
                    Ok(())
                }
                (_, Some(_)) => some_instance(ctx),
                (_, None) => at.typ.check_contains(&ctx.env, &typ),
            };
        }
        let depth = def.level.depth();
        let enclosing =
            |tv: &TVar| matches!(tv.level(), Level::Def { depth: d, .. } if d < depth);
        if at.typ.has_unbound()
            && holds_unbound(&typ, &|tv| !tv.is_rigid() && enclosing(tv))
        {
            bail!(
                "default reads an enclosing open type: annotate the factory's \
                     parameter or give the defaulted parameter a closed type"
            );
        }
        let declared = holds_unbound(&typ, &|tv| tv.is_rigid() && enclosing(tv));
        match &at.typ {
            // no call instantiates an enclosing definition's variable the
            // default sits under: the default fits for every instance of it,
            // or not at all
            _ if declared => at.typ.check_contains(&ctx.env, &typ),
            // each conjunct narrows the default's open cells, which
            // may be the environment's, rather than deciding them
            Type::TVar(tv) if !tv.is_bound() => tv
                .cell_constraints()
                .iter()
                .try_for_each(|c| super::op::constrain_operand(&ctx.env, c, &typ)),
            // a variable inside the type is checked as any instance of
            // it: each omitting call checks the default again
            t => {
                let held = match t.has_unbound() {
                    true => t.reset_tvars(),
                    false => t.clone(),
                };
                held.check_contains(&ctx.env, &typ)
            }
        }
    })
}

impl<R: Rt> Update<R> for Lambda {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Lambda, buf);
        self.spec.encode(buf)?;
        self.lambda_id::<R>().ok_or(PackError::InvalidFormat)?.encode(buf)?;
        self.typ.encode(buf)
    }

    /// A lambda literal is a constant.
    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        if let Some(def) = self.def.downcast_ref::<LambdaDef<R>>() {
            def.captures.get_or_init(|| {
                let mut ids: LPooled<Vec<BindId>> = LPooled::take();
                def.captured_ids(&mut |id| ids.push(id));
                if let Some(body) = &def.env.body {
                    body.dispatch_bindings(&mut |id| ids.push(id))
                }
                Lives::capture(ctx.control.lives(), ids.drain(..))
            });
        }
        produce_constant(ctx.event, &mut self.resident, || self.def.clone())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Some(def) = self.def.downcast_ref::<LambdaDef<R>>() {
            let (held, nested) = {
                let shared = def.shared.lock();
                (shared.held.as_ref().is_some_and(|h| h.id == def.id), shared.nested)
            };
            if !held {
                ctx.lambda_defs.remove(&def.id);
            }
            if !nested {
                let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
                BodyShared::release_held(&def.shared, &mut seen, &mut |id| {
                    ctx.lambda_defs.remove(&id);
                });
            }
            ctx.def_assertions.lock().retain(|a| a.id != def.id);
            ctx.fusion.forget_def(def.id);
        }
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
        let spec: &Expr = &self.spec;
        crate::defer_written_names(ctx, &Type::Fn(def.typ.clone()), spec)?;
        // Every arg, defaulted labeled ones included, checks as a Nop of
        // its declared type; the defaults themselves are checked after
        // the body (`check_defaults`), and again per omitting call site
        // (`CallSite::check_omitted_defaults`), where one may narrow that
        // site's cells. A check by bounds checks them first: a call in the
        // body that leaves a label out takes the default its parameter
        // records.
        let mut gate = DefGate::open(ctx, def, self.named);
        let mut table = None;
        let mut captures: LPooled<AHashSet<(ModPath, CompactString)>> = LPooled::take();
        let mut defaults: LPooled<Vec<Node<R>>> = LPooled::take();
        let by_bounds = bounds::active();
        let res = match by_bounds {
            true => check_defaults(ctx, def, &gate.scope, &mut captures, &mut defaults),
            false => Ok(()),
        };
        let res = res.and_then(|()| {
            (def.init)(
                &gate.scope,
                ctx,
                &mut gate.args,
                BindMode::Definition,
                ExprId::new(),
            )
            .at(spec)
        });
        let res = res.and_then(|mut f| {
            let ftyp = f.typ().clone();
            // fn-typed params knot like self-calls: a call to `f` unifies
            // against the param's own declared cells (`CompileCtx::def_gate_params`)
            let mut param_knot: LPooled<Vec<BindId>> = LPooled::take();
            if let ApplyView::Lambda(g) = f.view() {
                for (i, (pat, at)) in g.args().iter().zip(ftyp.args.iter()).enumerate() {
                    if at.typ.is_function(&ctx.env)
                        && let Some(id) = pat.single_bind_id()
                    {
                        ctx.def_gate_params.insert(id, (def.id, i as u32));
                        param_knot.push(id);
                    }
                }
            }
            let res = f.typecheck0(ctx, &mut gate.args).at(spec);
            for id in param_knot.drain(..) {
                ctx.def_gate_params.remove(&id);
            }
            if res.is_ok() {
                def.hold_returned(ctx, &ftyp.rtype);
            }
            if res.is_ok()
                && let ApplyView::Lambda(g) = f.view()
            {
                let (facts, calls) = crate::analysis::def_body_facts(g, ctx);
                def.join_facts(facts);
                let captured = calls
                    .captured
                    .iter()
                    .filter_map(|id| ctx.def_gate_params.get(id).copied())
                    .collect();
                table = Some(DefTable::record(
                    &g.body,
                    &g.env,
                    &ctx.env,
                    facts,
                    calls.own,
                    captured,
                    &mut captures,
                ));
                *def.param_calls.lock() = calls;
            }
            ctx.discard_apply(f);
            res?;
            let inferred_throws = gate.thrown(ctx).normalize();
            ftyp.throws.check_contains(&ctx.env, &inferred_throws).at(spec)?;
            Ok(())
        });
        if res.is_ok() {
            def.bound_ids(&ctx.env, &mut |id| {
                capture_id(id, &def.env, &ctx.env, &mut captures)
            });
            capture_impls(def, &ctx.env, &mut captures);
        }
        let res = res
            .and_then(|()| match by_bounds {
                true => Ok(()),
                false => {
                    check_defaults(ctx, def, &gate.scope, &mut captures, &mut defaults)
                }
            })
            .and_then(|()| gate.check_declared_kept().at(spec));
        let mut checked = None;
        if res.is_ok()
            && (table.is_some() || !captures.is_empty() || !defaults.is_empty())
        {
            let mut table = table.unwrap_or_else(|| DefTable {
                facts: *def.facts.lock(),
                ..DefTable::default()
            });
            table.record_defaults(&defaults, &def.env, &ctx.env);
            table.captures = captures
                .drain()
                .map(|(scope, name)| Captured::new(&def.scope.lexical, scope, name))
                .collect();
            checked = Some(SArc::new(table));
        }
        for n in defaults.drain(..) {
            ctx.discard(n)
        }
        if !bounds::active()
            && let Some(table) = checked.take()
        {
            def.table.set(Tables { table, outer: None })
        }
        let table = checked.clone().or_else(|| def.table.get().map(|t| t.table.clone()));
        let closed = gate.close(ctx, spec, table, self.named);
        if bounds::active() {
            // a lambda that stands as one instance of its scheme reads its
            // check's types as that instance has them
            let one = closed.as_ref().ok().and_then(|stands| stands.as_ref());
            if let Some(table) = checked {
                let table = match one {
                    Some((_, copies)) => SArc::new(table.renamed(copies)),
                    None => table,
                };
                def.table.set(Tables { table, outer: None })
            }
            return res.and(closed).map(|stands| {
                if let Some((t, _)) = stands {
                    self.typ = t
                }
            });
        }
        // what the body bound is the signature: a call's types follow from
        // it without elaborating the body
        self.typ.unbind_vacuous_tvars();
        res.and(closed.map(|_| ()))
    }

    /// A lambda an instance defines takes its signature from the
    /// enclosing definition's check, at its own level, its cells placed
    /// at the row and then its written parameters taking the row's, and
    /// generalizes it as its gate would; its instances read its table
    /// from there too.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
        let tables = match def.is_builtin() {
            false => types.lambda(self.spec.id),
            true => None,
        };
        let Some(tables) = tables else { return self.typecheck0(ctx) };
        let level = def.level;
        let row = {
            let _at = AtLevel::enter(level);
            types.typ(self.spec.id)
        };
        let Some(row) = row else {
            elab_audit::fallback(&self.spec);
            return self.typecheck0(ctx);
        };
        // a definition a check by bounds made generic is that scheme, as
        // the instance holds it
        if bounds::scheme(&row) {
            if !bounds::take(&self.typ, &row) {
                bailat!(self.spec, "BUG: a lambda's signature took no row")
            }
            def.join_facts(tables.table.facts);
            def.table.set(tables);
            return Ok(());
        }
        {
            let _at = AtLevel::enter(level);
            self.typ.place_cells(&ctx.env, &row);
            if let (Type::Fn(own), Type::Fn(row)) = (&self.typ, &row) {
                row.pre_unify_params(&ctx.env, own).at(&self.spec)?;
            }
            self.typ.check_contains(&ctx.env, &row).at(&self.spec)?;
        }
        match self.named {
            true => def.typ.generalize(def.level.depth()),
            false => def.typ.claim(current_level()),
        }
        let mut own = tables.table.calls_own;
        let mut captured = SmallVec::new();
        for k in tables.table.calls_captured.iter() {
            match ctx.instance_fn_params.get(k) {
                Some(id) => captured.push(*id),
                None => own = true,
            }
        }
        *def.param_calls.lock() = ParamCalls { own, captured };
        def.join_facts(tables.table.facts);
        def.table.set(tables);
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Lambda(self)
    }
}

/// `GRAPHIX_ELAB_AUDIT`: reports every error an instance's check raises
/// outside a definition gate, once, at the innermost instance that
/// raised it. The definition and call-site checks are meant to leave
/// elaboration nothing to refuse.
// CR claude for claude: [test-gap/low] The audit reports a refusal only from an
// instance's own check (`leave`), the unify-back and the fallback. Two refusals of
// programs the check accepted print nothing under GRAPHIX_ELAB_AUDIT=1: the site settle
// drained after `typecheck1` (probe:
// design/review-2026-11-16/repro/x-check-build-fnvalue-builtin-unread.gx) and a default
// judged at the bind in `callsite::checked_default` (probe:
// design/review-2026-11-16/repro/x-check-build-default-never-let.gx), though CLAUDE.md
// says the audit reports elaboration refusing what the check accepted. Report in
// `check_and_fuse_inner` for any error of `typecheck1` or of the settle drained after
// it. (x-check-build-08)
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

    /// A node its definition's table has no row for (its id is shared,
    /// a default compiled at two sites) is checked in full.
    pub(crate) fn fallback(spec: &Expr) {
        if dbgenv::graphix_elab_audit() {
            report("fallback", spec, format_args!("no check-table row: checked in full"))
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

#[cfg(test)]
mod settle_tests {
    use super::*;
    use netidx_value::Typ;

    fn instance_of(id: ExprId, row: Type) -> InstanceTypes {
        let mut table = DefTable::default();
        table.types.insert(id, row);
        InstanceTypes {
            tables: Tables { table: SArc::new(table), outer: None },
            known: Known::default(),
            open: IntSet::default(),
        }
    }

    #[test]
    fn a_closed_born_type_disjoint_from_a_closed_row_is_refused() {
        let (env, id) = (Env::default(), ExprId::new());
        let mut types = instance_of(id, Type::Primitive(Typ::I64.into()));
        let err = types
            .settle_in(&env, id, &Type::Primitive(Typ::String.into()))
            .expect_err("string where the row is i64");
        assert!(
            err.to_string()
                .contains("is string where its definition's check settled i64")
        );
    }

    #[test]
    fn a_closed_born_type_beside_a_row_that_holds_it_or_is_held_by_it_stands() {
        let (env, id) = (Env::default(), ExprId::new());
        let wide = Type::Set(Arc::from_iter([
            Type::Primitive(Typ::I64.into()),
            Type::Primitive(Typ::String.into()),
        ]));
        let string = Type::Primitive(Typ::String.into());
        let mut types = instance_of(id, wide.clone());
        assert!(types.settle_in(&env, id, &string).unwrap());
        let mut types = instance_of(id, string);
        assert!(types.settle_in(&env, id, &wide).unwrap());
    }
}
