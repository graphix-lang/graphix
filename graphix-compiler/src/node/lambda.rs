use super::{
    Nop, WakeBit,
    callsite::{CallSite, Feeds, QuietAtRoot, publish_production},
    collection::CollectionIntrinsic,
    compiler::compile,
    pattern::{Holder, StructPatternNode, rigid_refusal},
    produce_constant,
    select::ArmDiscriminants,
};
use crate::SourcePosition;
use crate::{
    AddrSet, Apply, ApplyView, BindId, BindMode, CFlag, CompileCtx, ExecCtx, HandlerKind,
    InitFn, LambdaId, LambdaInstanceId, Node, NodeView, Refs, Rt, Scope, TagValue,
    Update, dbgenv,
    effects::{LambdaFacts, NoLoop, RecursionKind},
    env::{Bind, Env, Lexical},
    expr::{self, Arg, ArgKind, At, Expr, ExprId, LambdaBody, ModPath, Origin},
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
    reference::{Captures, Lives},
    stack::ensure_sufficient,
    typ::{
        FnArgKind, FnArgType, FnType, ResolvedRef, TVar, Type, check_bounds_acyclic,
        tvar::{AtLevel, InTask, Level, RigidGate, TaskId},
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
    fmt,
    hash::Hash,
    mem,
    sync::{Arc as SArc, OnceLock, Weak},
};
use triomphe::Arc;

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
    sig: Arc<FnType>,
}

/// The definition's signature `def` as the instance at `instance` holds
/// it: each open cell copied under its name into `known` and bound to the
/// instance's type. `None` when the instance's signature does not hold
/// the definition's.
// CR claude for claude: [bug/medium] A rank-2 formal whose quantifier is bounded by a
// variable of its definition fails every instance here, so the check accepts what the
// build refuses: `let outer = |#k: fn<'a: 'c>(x: 'a) -> 'a, z: 'c| z; println(outer(#k:
// |x| x, 1))` passes --check and the build refuses the call's instance ("an instance at
// fn(#k: fn<'a: i64>(x: 'a) -> 'a, z: i64) -> i64 of a definition typed fn(#k: fn<'a:
// 'c>(x: 'a) -> 'a, z: 'c) -> '_N"); GRAPHIX_ELAB_AUDIT=1 reports instance-signature,
// and under GRAPHIX_NO_SUBST=1 it runs and prints 1. The same with the formal called
// (o10, o10b) and with 'c declared ('c: Any, o10d); main ec53d4846 the same. Not traced:
// the copy instantiate_owned makes of the formal's bound, or check_contains holding the
// formal's bounds exactly, are where to look. Found by review r9 of lane R (R-r1-2).
// probes: ~/ptmp/cr29/scratch/r9-R/{o10,o10b,o10c,o10d}.gx (rank2-bound-instance-01)
// 2026-11-03 claude: Closed by refusing, at the definition's check:
// Type::rewrite_trait_args runs FnType::check_bounds_closed on every function type a
// written type holds (a formal, a field, an element, a let or body annotation, a typedef
// body, an abstract's representation, a return type, a bound), so a rank-2 quantifier's
// bound names only written types and its own fn<..>'s quantifiers: "a rank-2
// quantifier's bound names 'c, which this function type does not quantify: bound it by a
// written type". An interface val's and a trait method's own quantifiers are their
// scheme's and exempt (Type::rewrite_declared_args); a sibling bound (fn<'a: Any, 'b:
// ['a, null]>) stays. o10, o10b, o10c and o10d (declared 'c: main's build refuses it in
// every mode) and the reviewers' tdef4/5/9/12, s15-17, leak1/7/13, esc1-3 are refused by
// --check; the round-9 approaches (late round, quantifier_admits taking the conjunct)
// are withdrawn. Pins that fail with the rule disabled (120 of 128 runs):
// lang::types::rank2_bound_*_is_refused* (29) and
// lang::interfaces::a_rank2_bound_in_a_val_names_only_its_own_quantifiers;
// rank2_bound_by_a_written_type_runs and rank2_bound_naming_a_sibling_quantifier_runs
// hold the passing forms. Lost, which main runs: s15-17 and sc_z2 (the positions' order
// happened to decide the bound first), and a let or body annotation, a return type or a
// typedef applied to a written type
// (~/ptmp/cr29/scratch/r11-R/{let1,body1,ret1,td1,td2}.gx); the --check census of both
// repos has no verdict change. rank2-structured-bound-01 and
// body-annotation-bound-quantifier-01 (round-9 CRs that never landed) are moot: their
// shapes are refused.
// 2026-11-03 claude: Review r11 fixes: an impl's method and a default body declare the
// method, so their let annotation (the generated one, or one the impl writes) is
// rewritten as a declared signature (Bind::compile, is_method_block): its own
// quantifiers keep any bound, as at the trait's declaration; main runs all of these
// right (t1, t3, t6, u1-u8). Pins that fail without it (12 of 12 runs):
// lang::types::a_trait_methods_own_bound_names_any_variable_at_its_impls,
// _at_its_default,
// a_ctor_trait_methods_own_bound_names_its_element_at_its_impls_and_default. A bound
// naming self or a constructor trait's element stays refused (a labeled parameter before
// self is the hole, s9/s10): CR rank2-self-bound-01 for Eric; pins
// rank2_bound_naming_self_is_refused and
// rank2_bound_naming_a_ctor_traits_element_is_refused (8 of 8 runs fail without the
// rule). Lost too, which main runs: a positional parameter's bound naming self or the
// element (t7, t8, t9, t11, t12), a lambda's quantifier bounded by a written fn type
// naming another variable ('k: fn<'a: 'c>(..) |#k: 'k, z: 'c|, e3-e3e), an interface val
// returning such a type (v3), never<fn<'a: 'c>(..)> (never1), an abstract's
// representation bounded by its parameter (abs1). Corrected: an enclosing written
// quantifier (enc_q2) passes main's check and its build refuses it,
// rank2-bound-instance-01's own class, not a main check refusal. probes:
// ~/ptmp/cr29/scratch/r11-R/r1fix/.
// 2026-11-03 claude: Corrected (review r12 of lane R): the note above says a labeled
// parameter before self is the hole for self as for the element. It is for the element
// alone (s9, s10). No shape of a rank-2 bound naming self is: main runs every
// labeled-self method its check passes right in every mode (s7i, s7s, s11, s15, s16),
// and its check refuses the rest (a generic impl, s7 and s14; a default body, s8 and
// s8i). So the pin rank2_bound_naming_self_is_refused (s7i) holds a disclosed
// over-refusal, and the loss list adds labeled self over a closed impl target (s7i, s7s,
// s11, s15, s16) to the positional t7, t8, t11. Whether to let self stand in a rank-2
// bound is CR rank2-self-bound-01 (its note has the corrected options); the rule here is
// unchanged, and the element's refusal is this hole's. probes:
// ~/ptmp/cr29/scratch/review-r11/R-r2-1/t/, review-r11/R-r2-2/.
// 2026-11-03 claude: Landed (main e5ce6689a) for every site but two: a catch's
// annotation and a seq try's with(e: T) never pass through rewrite_trait_args
// (Catch::compile binds c.constraint raw), so a written fn<'a: ['c, null]> there naming
// the definition's 'c is not held to check_bounds_closed. When the annotated error
// escapes into the result, the original class returns: c1 ('c: Any |z: 'c| { let r =
// never(); catch(e: Error<ErrChain<`F(fn<'a: ['c, null]>(x: 'a) -> 'a)>>) r <- e;
// error(`F(|x| x))?; (r, z) }, then (k(z), k(null))) passes --check, logs 'a run-time
// bind at k(z) did not type' and prints nothing; ELAB_AUDIT instance-signature; NO_SUBST
// refuses the build. Same for c1b (undeclared 'c), c5 (labeled formal) and c3 (seq try
// .. with: ELAB_AUDIT unify-back). Main shares it. Next: hold catch and with annotations
// to the rule, pin c1 and c3, list catch4 (main runs it) as a loss, and drop
// design/tvar_constraints.md's exemption of a catch's annotation. probes:
// ~/ptmp/cr29/scratch/review-r11/R-r3-1/ex/{c1_catch_ann,c1b_catch_ann_undeclared,c5_catch_ann_labeled_formal,c3_seq_try_with,catch4}.gx
// (run with that dir's p.sh)
fn instance_sig<R: Rt>(
    ctx: &CompileCtx<R>,
    def: &FnType,
    instance: &FnType,
    known: &mut AHashMap<usize, TVar>,
) -> Option<Arc<FnType>> {
    // every open cell of the signature is the instance's own copy, a cell
    // no closed gate owns (a rank-2 formal's copy) included, so the check
    // binds nothing another instance reads
    let def = def.instantiate_owned(known);
    instance.check_contains(&ctx.env, &def).ok()?;
    Some(Arc::new(def))
}

impl InstanceTypes {
    /// The types of `def`'s labeled default at `root` as its check
    /// recorded them, each open cell its definition owns copied; `None`
    /// where it recorded none or under `GRAPHIX_NO_SUBST`.
    pub(crate) fn of_default<R: Rt>(
        ctx: &CompileCtx<R>,
        def: &LambdaDef<R>,
        root: ExprId,
    ) -> Option<Self> {
        if dbgenv::graphix_no_subst() {
            return None;
        }
        let tables = def.table.get()?;
        tables.table.types.contains_key(&root).then(|| Self {
            tables: tables.clone(),
            known: Known::default(),
            open: ctx.rec_defs.clone(),
            sig: def.typ.clone(),
        })
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
        let mut known = AHashMap::default();
        let sig = instance_sig(ctx, def, instance, &mut known)?;
        Some(Self { tables, known: SArc::new(Mutex::new(known)), open, sig })
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
}

/// A type written in a body: its same-named variables are one variable,
/// and one a definition around the body declares (a nested fn type's own
/// quantifiers are its own) is the innermost such definition's; any other
/// is the written type's own (`Level::Written`).
pub(crate) fn body_annotation<R: Rt>(ctx: &CompileCtx<R>, t: &Type) {
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    enclosing_tvars(ctx, &mut known);
    let enclosing: LPooled<AHashSet<ArcStr>> = known.keys().cloned().collect();
    t.alias_tvars(&mut known);
    for (_, tv) in known.iter().filter(|(name, _)| !enclosing.contains(*name)) {
        tv.own_written()
    }
}

/// The variables the definitions around the body being compiled declare,
/// by name, the innermost's where two share one: those each definition's
/// signature writes, never a cell its check named after a body's written
/// type (a result bound to a let's annotated type).
fn enclosing_tvars<R: Rt>(ctx: &CompileCtx<R>, known: &mut AHashMap<ArcStr, TVar>) {
    if let Some(body) = &ctx.body {
        for typ in body.declared.iter().rev() {
            let written = typ
                .lambda_ids
                .own()
                .and_then(|id| ctx.lambda_defs.get(&id))
                .and_then(|d| d.downcast_ref::<LambdaDef<R>>())
                .map(|d| {
                    let mut names: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                    d.typ.collect_tvars(&mut names);
                    names
                });
            let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            typ.collect_tvars(&mut named);
            for (name, tv) in named.drain() {
                let writes = written.as_ref().is_none_or(|w| w.contains_key(&name));
                if !name.starts_with('_') && writes {
                    known.entry(name).or_insert(tv);
                }
            }
        }
    }
}

/// The body compiling now: what it shares, and whether this compile is
/// its definition's check, which alone records specs.
#[derive(Debug, Clone)]
pub(crate) struct BodyCompile {
    pub(crate) share: BodyShare,
    pub(crate) checking: bool,
    pub(crate) types: Option<InstanceTypes>,
    /// The signatures of the definitions around the body, innermost last.
    pub(crate) declared: Arc<[Arc<FnType>]>,
}

impl BodyShared {
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
        enclosing: Arc<[Arc<FnType>]>,
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
        /// The signatures of the definitions lexically around the literal,
        /// innermost last, as the compile that met the literal held them.
        enclosing: Arc<[Arc<FnType>]>,
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

impl<R: Rt> LambdaDef<R> {
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

    /// `f` compiling an expression the signature holds, a labeled
    /// default: a type it writes names the variables of the definitions
    /// around this one, as the signature does.
    pub(crate) fn in_signature<T>(
        &self,
        ctx: &mut CompileCtx<R>,
        types: Option<InstanceTypes>,
        f: impl FnOnce(&mut CompileCtx<R>) -> T,
    ) -> T {
        let declared = match &self.origin {
            DefOrigin::Source { enclosing, .. } => enclosing.clone(),
            DefOrigin::Runtime => Arc::from_iter([]),
        };
        let outer = ctx.body.replace(BodyCompile {
            share: BodyShare::default(),
            checking: false,
            types,
            declared,
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
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<R: Rt> Ord for LambdaDef<R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
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

impl<R: Rt> GXLambda<R> {
    /// The definition's id, shared by every instance of it.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    /// The ids this instance's formals bind.
    pub(crate) fn formal_ids(&self) -> LPooled<IntSet<BindId>> {
        let mut ids: LPooled<IntSet<BindId>> = LPooled::take();
        for pat in self.args.iter() {
            pat.ids(&mut |id| {
                ids.insert(id);
            })
        }
        ids
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

    /// The callback instance of a collection-bodied lambda, whose body a
    /// fused loop emits inline.
    pub(crate) fn inline_callback(&self) -> Option<&GXLambda<R>> {
        match self.body.view() {
            NodeView::MapQ(map) => map.callback(),
            NodeView::FoldQ(fold) => fold.callback(),
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
    /// Whether the instance checks its body again (`GRAPHIX_NO_SUBST`),
    /// where its definition's types would do.
    fn rechecks(&self) -> bool {
        matches!(self, Typing::Substituted(..)) && dbgenv::graphix_no_subst()
    }

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
        if self.rechecks() {
            return Ok(None);
        }
        let Some(tables) = table.get() else {
            bail!("an instance of {:?}, whose definition's check recorded no types", id)
        };
        match InstanceTypes::new(ctx, tables.clone(), def, typ) {
            Some(types) => Ok(Some(types)),
            // CR claude for claude: [bug/medium] A lambda literal held in a
            // struct, tuple or array and passed a value holding itself passes
            // --check, then its run-time bind is refused here and the call never
            // produces: `{ let s = {f: |x| 0}; s.f(s) }` prints "binding the callee
            // failed: a run-time bind at s.f(s) did not type: an instance at fn(x:
            // { f: fn(x: _) -> i64 }) -> i64 of a definition typed fn(x: _) ->
            // i64" (main 24ae12e91 and cr30-u1). The same for `s.f([s])`,
            // `s.f({a: s})`, `let s = (|x| 0, 1); s.0([s])`, `let s = [|x| 0];
            // s[0]$([s])` and `s.f(s.f)`; `s.f(1)`, `s.f(|y| 2)`, `s.f(t)` over
            // another struct and `let f = |x| 0; let s = {f}; s.f([s])` run, and
            // s.f(s) runs and prints 0 under GRAPHIX_NO_SUBST=1, so the check is
            // right to accept it. Reading `s` does not copy the literal's
            // signature (a let-bound lambda's read does), so the call's argument
            // type holds the definition's own parameter cell, and the call
            // site's terminal settle (`FnType::settle_terminal` from
            // `PendingSettle::drain`; GRAPHIX_DBG_BIND=1 shows SETTLE-BOTTOM of
            // that cell, GRAPHIX_DBG_BIND_BT the stack) binds it to ⊥: it exempts
            // the enclosing definitions' signatures (`sigs`), not the callee's.
            // The definition's signature becomes fn(x: ⊥) -> i64, which
            // `instance_sig` cannot hold the instance to. Leave a cell a
            // definition's signature owns out of every site's terminal settle,
            // or copy a lambda literal's signature where its value is read.
            // probes: ~/tmp/cr29/scratch/review3/u1/s01-s05.gx, t03.gx, n07.gx,
            // n15.gx (self-arg-instance-01)
            // 2026-10-30 claude: Reading the field into a let first fails the same
            // way: `{ let s = {f: |x| 0}; let g = s.f; g(s) }` passes --check and
            // its run-time bind is refused (main 24ae12e91 and cr30-u1; review
            // probe ~/tmp/cr29/scratch/review4/u1/c02.gx).
            // 2026-11-01 claude: A call site's terminal settle
            // (FnType::settle_terminal) now leaves a generic cell nothing bounds
            // (no conjunct, no lower bound): a closed definition's own signature
            // cell, which a lambda literal held in a struct, tuple or array carries
            // into a call's argument types uncopied, and which ⊥ would narrow for
            // every other call. A bounded generic cell is settled as before, so an
            // unread Concrete cell of a callback literal is still refused
            // (lang::types::concrete_targets_are_known_where_they_settle,
            // parse_unannotated_in_callback_err). Pin
            // lang::types::holder_of_a_lambda_passed_to_it (s.f(s), s.f([s]),
            // s.f({a: s}), s.f(s.f), g(s) with g = s.f, t.0([t]), a[0]$([a])) fails
            // without it (the run-time bind refused) and passes under
            // GRAPHIX_NO_SUBST=1.
            // 2026-11-01 claude: Review (r6 B2) found the settle exemption covered
            // only unbounded cells, and the cause, a read of a holder sharing the
            // definition's own cells, still stood: a bounded cell settled to ⊥
            // (s.f(s), s.f(s.f), h(s) over {f: |x| x == x}), an annotation or a
            // typed formal bound the scheme (let t: {f: fn(x: i64) -> i64} = s;
            // arrays and tuples alike), a later REPL input too. Now a let nothing
            // writes whose value holds a lambda or a generalized binding's value,
            // in a data literal (a struct, its update, a tuple, a variant, an
            // array, a list, a map: bind::holds_function), is generalized
            // (Env::poly_binds): each read takes a fresh placeholder at compile
            // and, in typecheck0, a copy of every closed definition's signature
            // its type holds
            // (Type::scheme_read, Fresh::Scheme), so nothing outside the definition
            // writes its scheme; a read inside such a holder's own value shares the
            // type (Signature::InHolder), which keeps a chain of holders a DAG
            // (polyleaf/polysel/fnleaf chains at N=18-20 check in 2.0-3.8G
            // instructions, as main). Pins
            // lang::types::holder_reads_copy_its_signatures (every reviewer shape
            // in one program; main refuses it at the check) and
            // holder_of_an_earlier_input_stays_generic (the REPL shape) fail
            // without it; holder_of_a_lambda_passed_to_it holds. Probes
            // ~/ptmp/cr29/scratch/r6-B/w4/.
            // 2026-11-01 claude: The earlier fix's settle exemption
            // (settle_terminal leaving an unbounded generic cell) is removed: with
            // holder reads copying, no site reaches a definition's own cell through
            // a holder, and every pin above, lang::types, lang::functions and
            // lang::byref pass without it; a literal passed straight to a call
            // (h({f: |x| 0}).f(1)) is decided by that call, as a callback literal
            // is (probes d2-d4.gx run alike).
            // 2026-11-01 claude: A lambda a let holds through a block's value or a
            // select's arms is left out: generalizing those too broke must_reject
            // family 1 (a block-wrapped lambda is the documented monomorphic
            // binding), and the split it would close (a call instantiates such a
            // binding while a value use binds its cells) is pre-existing and asks a
            // rule: CR holder-block-select-01.
            // 2026-11-01 claude: Review (r6 B2, round 2) found two holes in the
            // InHolder share. (1) A destructuring let with one written name shared
            // the holder's definition cells into that name, which is not
            // generalized, so its writer or annotation decided the definition after
            // an earlier read copied it (check passed, run-time bind refused; main
            // refuses at the check). Now the value shares only when no name the
            // pattern binds is written; otherwise every read in it is a copy. Pins
            // lang::types::holder_destructured_beside_a_written_name and _in_a_body
            // fail without it. (2) A read in an array, list or map part of a
            // holder's value was a copy, so a chain holding a link twice there
            // doubled its type at each link (([x], [x]) at 20 links: 73G
            // instructions, main 2.4G). share_in_holder now descends every data
            // literal but a with-update's fields and a map's keys: a literal's
            // union of elements binds only function members' cells (meet_open_fns),
            // and a shared read is of a holder, whose type is data (a function
            // binding's read is always a copy), so the share decides no definition.
            // Lane debug --check at 18-20 links: arrpair, mix, three-let, ([x, y],
            // [x, y]), ([x, x], [x, x]), lists, maps, variants 1.4-2.7G (main
            // 1.3-2.6G); a with-update chain does not type (its fields keep the
            // source's types). Pin
            // lang::types::holder_chain_through_collections_stays_a_dag (type node
            // count; 9722 nodes without it at 10 links, bound 176) fails without
            // it. Probes ~/ptmp/cr29/scratch/r6-B/w5/.
            // 2026-11-01 claude: Review (r6 B2, round 3) found holder chains still
            // exponential where a link reads the last through a call's argument, a
            // block, a select arm or a struct update (callpair at 18 links 99G
            // instructions, main 2.4G), and the earlier notes overclaimed: 'keeps a
            // chain of holders a DAG' held only for reads in data parts, and 'a
            // with-update chain does not type' was wrong (it types and grew). Now
            // every read of a generalized binding in a holder's value, wherever it
            // stands there (a lambda body it holds and a nested holder's value
            // included), copies through one ReadCopies (CompileCtx::holder_reads
            // under bind::ValueReads::Shared): two reads of one binding take one
            // copy and a read of a type an earlier read made keeps it, so a link
            // holds one copy of the last; Signature::InHolder, the uncopied share
            // of the definitions' own cells, is gone. The value is typed as if it
            // read each binding once (pin
            // lang::types::holder_value_reads_a_binding_once: {a: s, b: k(s)} with
            // k typed i64 makes t.a.f i64; the lane before this accepted
            // t.a.f("x"), main refuses it). An annotated let, or one binding a
            // written name, still copies at each read (ValueReads::Own). Lane debug
            // --check at 14/16/18 links, before -> after (main 1.6-3.3G
            // throughout): callmix 4.6/13.8/49.6G -> 1.6/2.8/2.2G; callpair
            // 12.5/42.2/99.2G -> 2.1/2.2/2.6G; blockmix 3.9/7.5/27.9G ->
            // 2.8/1.4/2.1G; selmix 3.5/6.4/15.8G -> 2.3/2.3/2.0G; withone
            // 3.0/5.7/10.7G -> 1.8/1.5/1.6G; withpair 3.6/8.1/16.3G ->
            // 1.3/1.9/1.9G; a lambda-body link {a: x, g: || x} 5.5/9.0/31.3G ->
            // 1.8/1.6/1.7G. Pin lang::types::holder_chain_stays_a_dag (renamed from
            // holder_chain_through_collections_stays_a_dag, every shape: distinct
            // type nodes and one type for two parts reading the last link) fails
            // without it (blocks 5139 nodes; calls and call pairs two types).
            // guicheck main 91.579G, lane before 91.406G, after 91.152G. Probes
            // ~/ptmp/cr29/scratch/r6-B/fix3/gen.
            // 2026-11-01 claude: Review (r6 B2, round 3, low): a let forwarding a
            // holder to a pattern that writes one of its names (let (a, b) = h; a
            // <- ..) read h once for both names, so the written name's writer
            // decided what the generalized one held and statement order decided
            // acceptance (m/f1 refused, f2 accepted; fb1/fb2 in a body). Now such a
            // let gives each name a copy of its own part (Bind::apart, in
            // complete_shape and complete_partial, a partial struct pattern
            // included). Pins
            // lang::types::holder_forwarded_beside_a_written_name{,_in_a_body}
            // (both orders, tuple and partial struct patterns) fail without it. The
            // shared copy also fixes md1 (a module with no interface writing a
            // parent's destructured holder name decided the definition: check
            // passed, s.f("x")'s run-time bind refused; pin
            // lang::modules::module_writes_an_outer_holder_name); what remains
            // there, order-dependent acceptance through the module's writer, is CR
            // module-holder-write-01.
            // 2026-11-01 claude: A partial struct pattern with a whole bind (let
            // all@ {p: m, ..} = k; m <- ..) keeps reading one copy for its names:
            // the whole bind takes the pattern's type, which holds the names'
            // cells, so typing its names apart refused it ("{ p: _ } does not
            // contain {..}", main and the lane before accepted). The m.f(5) line of
            // lang::types::holder_forwarded_beside_a_written_name pins it. That
            // whole bind's type is itself wrong (the pattern's fields only, so a
            // field read takes the wrong index at run time, main the same): CR
            // partial-whole-bind-01.
            // 2026-11-01 claude: Landed on main with fix 3 (the reads in a holder's
            // value share one copy per binding; a forwarded destructure copies per
            // name). The coordinator measured the B2-r3-2 chains at N=14, 16, 18
            // and 30: lane and main within noise (1.6-3.0G instructions, flat),
            // every verdict the same. Fix 3 was not adversarially re-reviewed, so
            // this stays an XCR for review at the meeting and the soak. Pins:
            // lang::types::holder_chain_stays_a_dag,
            // holder_value_reads_a_binding_once,
            // holder_forwarded_beside_a_written_name{,_in_a_body}.
            // 2026-11-01 claude: Lane B round 6 item 1 (partial-whole-bind-01): a
            // partial struct pattern with a whole bind no longer reads one copy.
            // The whole bind is a fresh cell (Bind::compile retypes the pattern's
            // fresh_ids under Deferred::Partial) typed in complete_partial by its
            // part of the value's members, so a forwarded destructure beside a
            // written name gives every name its own copy, the capture included: let
            // al2@ {p: m2, ..} = k; m2 <- ..; al2.p.f("x") now checks and prints
            // "x" (refused before). The m.f(5) line of
            // holder_forwarded_beside_a_written_name still passes; new pin
            // lang::types::holder_forwarded_through_a_partial_capture.
            // 2026-11-02 claude: Re-review r8 (fix 3 holds for what it changed: the
            // Shared-read chains stay flat, every pin and earlier review shape
            // passes) found three open problems, each on main bbb06c0c6, probes
            // ~/ptmp/cr29/scratch/review-r8/xcr-self-arg/ (NOTES.txt, r.sh). (1)
            // medium: a lambda literal in a data literal passed to a collection
            // call: array::map([{f: |x| x}], |h| h.f(1)) (g/e1.gx) passes --check,
            // ELAB-AUDIT reports it and the run refuses h.f(1) "an instance at
            // fn(x: i64) -> i64 of a definition typed fn(x: _) -> _"; list::map
            // (e3), array::filter (e4), a callback returning the literal (e5, h5,
            // d6) alike; the fuzzer says AGREE (both engines fail alike). The 11-01
            // note that a literal passed straight to a call is decided by that call
            // held for user functions only (d3, d7): removing the settle exemption
            // is the likely cause. (2) medium: the original s.f(s) through a value
            // holds_function does not see as a data literal: { let c = 1; let s =
            // {f: c ~ |x| 0}; s.f(s) } (a/v1.gx), let g = (|x| 0, 1).0; g(g) (v2),
            // and w2 w3 w6 w7 (a sample, a projection, an index): the mechanism of
            // holder-block-select-01 (option b there covers these, option a does
            // not). (3) medium perf: chains with a written name or an annotation
            // grow exponentially through CompileCtx::monomorphic, which copies each
            // occurrence of a shared node holding a function (c/t-ownone-N
            // 2.7/3.2/12.0G instructions at N=10/12/14, killed at 8G RSS at 16;
            // o-wtup 61.9G at 18; u-wsingle needs no holder, so likely older): new
            // CR holder-mono-chain-01. Low: an interface-declared holder accepts by
            // statement order (m/m3 vs m9); NO_SUBST refuses array::map([s, s], |h|
            // h.f(true)) which default runs (g/g5-g8, n3). Shape (1) is mine to
            // fix; shape (2) goes with holder-block-select-01.
            // 2026-11-02 claude: Shape (1) is the same rule question as shape (2):
            // in array::map([{f: |x| x}], |h| h.f(1)) the array is checked before
            // the callback, so h.f has the closed literal's type and the call
            // copies it, and the statement's settle then makes the literal fn(x: ⊥)
            // -> ⊥; with a user function whose callback formal comes first (d3.gx,
            // app({f: |x| x}, |h| h.f(true))) the call meets an open cell and
            // decides the literal, so argument order decides acceptance. Option (b)
            // of holder-block-select-01 (a call through a value no generalized
            // binding names shares its cells instead of instantiating) closes (1)
            // and (2) alike, in any order. Readdressed to eric with that CR.
            // 2026-11-03 claude: Eric ruled (2026-11-03): D11 b (filled by claude
            // at his word) in ~/ptmp/cr-discuss.md, under his rule for type holes
            // (reject and require an annotation, then a --check census: it comes
            // back to him only if the stdlib, the book's examples or netidx-admin
            // would need a new annotation): a call through a value no generic
            // binding names shares its type, as a value use does (ML's value
            // restriction); calls at two types through such a lambda are refused,
            // the fix a plain `let f = |x| ..`.
            // 2026-11-04 claude: Round 15 lane M built D11 (b) and D12 (a) on
            // branch cr50-M-parked (fb0072ecd..f90e93c94) and was parked. D12 held
            // up (a written chain of 18 holders checks in 1.34G where main is
            // killed at 150 s). D11 failed review three times in new ways, each the
            // same cause at a new site: a call through a value no generic binding
            // names shares its definition's cells, but wherever a generic copy
            // meets those cells (a holder's read, then a struct update, then an
            // array, map, list or select of composites) the merge keeps the copy's
            // generic cell, so a later read copies what the shared call decided and
            // the run refuses what the check passed (main runs those). The lane
            // also refuses some well-typed arrays of composites with "expected fn
            // not [fn, fn]". Back to Eric as the D11/D12 follow-up in
            // ~/ptmp/cr29/next-discuss.md; probes
            // ~/ptmp/cr29/scratch/review-r15/M-r*/.
            // 2026-11-05 claude: Eric ruled (2026-11-05) on the D11/D12 follow-up:
            // redo D11 (b) on the textbook fix, after lane Q (rank-2 removal)
            // lands. (1) A merge takes the lower of its two cells' levels
            // (TVar::merge_one). (2) A check against any instance (check_defaults
            // first) uses rigid cells, never live generic ones. (3) Every other
            // relation against a generic copy instantiates first or uses rigid
            // cells. (4) D11 (b) is built on that, without lane M's local claims.
            // Start by re-running this CR's probes on Q's result. Analysis:
            // ~/ptmp/cr-discuss.md, Ruled 11-05.
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
            let recheck = self.typing.rechecks();
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
                    // an unwritten result is a cell the body binds
                    let written = !matches!(&self.typ.rtype, Type::TVar(_));
                    let body = &mut self.body;
                    let res =
                        super::rechecking_instance(recheck, || body.typecheck0(ctx));
                    wrap!(self.body, res)?;
                    let body = self.body.typ().clone();
                    let contain = |t: &Type| t.check_contains_rigid(&ctx.env, &body);
                    let rtype = &self.typ.rtype;
                    match written {
                        true => wrap!(
                            self.body,
                            super::callsite::contain_generally(
                                ctx, rtype, &self.body, contain
                            )
                        )?,
                        false => wrap!(self.body, contain(rtype))?,
                    }
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
        let recheck = self.typing.rechecks();
        let body = &mut self.body;
        let res = ctx.with_restored(env, |ctx| {
            super::rechecking_instance(recheck, || wrap!(body, body.typecheck1(ctx)))
        });
        elab_audit::leave(ctx.def_gate_depth, "typecheck1", self.body.spec(), &res);
        res
    }

    fn emit_clif(
        &self,
        callsite: &CallSite<R>,
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
        // CR claude for claude: [bug/medium] A DAG of union bounds checks and its
        // build refuses the instance's signature: `let f = 'a0: [Array<'a1>,
        // Array<'a2>, null], 'a1: [Array<'a2>, Array<'a3>, null], 'a2:
        // [Array<'a3>, Array<'a4>, null], 'a3: [Array<'a4>, null], 'a4: Number
        // |x: ('a0, 'a1, 'a2, 'a3, 'a4)| x; f(never())` passes `graphix --check
        // --no-cache` and `--expand` refuses "an instance at fn(x: (...)) of
        // definition typed fn(...)", GRAPHIX_ELAB_AUDIT reporting
        // instance-signature. Three links (udag_3) pass, four fail; taking only
        // 'a0, or dropping it, passes. Main 968fa9bc0 is the same. Found by
        // review r14 of lane K (final). probes:
        // ~/ptmp/cr29/scratch/review-r14/K-r3-1/m/u4.gx, p/udag_4.gx, udag4.err
        // (union-dag-elab-01)
        let types = {
            let mut p = profile::phase(Phase::InstanceCheck);
            profile::instance(&mut p, instance_id, id, &body);
            elab_audit::enter();
            let res = typing.instance_types(ctx, id, &typ);
            elab_audit::leave(ctx.def_gate_depth, "instance-signature", &body, &res);
            res?
        };
        // what a type the body writes names: the instance's variables,
        // under the definition's names
        let sig = match (&types, &typing) {
            (Some(types), _) => types.sig.clone(),
            (None, Typing::Substituted(_, def)) => {
                instance_sig(ctx, def, &typ, &mut AHashMap::default())
                    .unwrap_or_else(|| typ.clone())
            }
            (None, _) => typ.clone(),
        };
        let build = |ctx: &mut CompileCtx<R>, _: &[StructPatternNode]| {
            let outer = ctx.body.as_mut().map(|b| {
                let declared = b.declared.iter().cloned().chain([sig]).collect();
                (
                    mem::replace(&mut b.types, types.clone()),
                    mem::replace(&mut b.declared, declared),
                )
            });
            let body = compile(ctx, flags, body, scope, tid);
            if let Some((types, declared)) = outer {
                let b = ctx.body.as_mut().expect("compiling body");
                b.types = types;
                b.declared = declared;
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
            let pattern = StructPatternNode::compile(
                ctx,
                &atyp.typ,
                &a.pattern,
                scope,
                a.pos.0,
                origin.clone(),
            )?;
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
    enclosing: Arc<[Arc<FnType>]>,
    table: SArc<DefTables>,
    shared: BodyShare,
) -> InitFn<R> {
    let def_scope = scope.append_place("fn", &def_spec, None);
    SArc::new(move |scope, ctx, args, mode, tid| {
        // the definition's names, the call site's handlers
        let scope =
            Scope { dynamic: scope.dynamic.clone(), lexical: def_scope.lexical.clone() };
        ctx.with_restored(def_env.clone(), |ctx| match &body {
            DefBody::Expr(body) => {
                let compiling = BodyCompile {
                    share: shared.clone(),
                    checking: matches!(mode, BindMode::Definition),
                    types: None,
                    declared: enclosing.clone(),
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
                    BindMode::Static { .. } | BindMode::Dynamic(_) => Some(&*typ),
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
fn instance_type<R: Rt>(
    ctx: &mut CompileCtx<R>,
    mode: BindMode<'_>,
    def_typ: &Arc<FnType>,
) -> Result<Arc<FnType>> {
    Ok(match mode {
        BindMode::Static { instance } => Arc::new(instance.clone()),
        BindMode::Dynamic(r) if same_parameters(r, def_typ) => Arc::new(r.clone()),
        BindMode::Dynamic(r) => Arc::new(fit_callee(ctx, r, def_typ)?),
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
        let written = |env: &Env, t: &Type| -> Result<Type> {
            Ok(t.scope_refs(&scope.lexical)
                .rewrite_trait_args(env)?
                .apply_ctor_quantifiers(&ctors))
        };
        if l.vargs.is_some() && matches!(l.body, LambdaBody::Expr(_)) {
            bail!("a variadic argument (`@args`) is for builtins only")
        }
        // a trait as a parameter's type is a fresh bounded quantifier
        // (`|s: Read|` ≡ `'s: Read |s: 's|`), joined to the declared ones
        // so the def gate holds it rigid
        let mut trait_quantifiers: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        let mut param = |env: &Env, name: ArcStr, typ: &Type| -> Result<Type> {
            match &typ.scope_refs(&scope.lexical) {
                t @ Type::Ref(tr) if env.trait_of_ref(tr).is_some() => {
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
            if flags.contains(CFlag::NoBuiltins) {
                bail!("defining builtins is not allowed in this context")
            }
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
            // a variable the definitions around this one declare, and this
            // one does not, is theirs
            let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            enclosing_tvars(ctx, &mut known);
            known.retain(|name, _| {
                !constraints.iter().any(|(tv, _)| tv.name == *name)
                    && !ctors
                        .iter()
                        .any(|(_, e)| matches!(e, Type::TVar(e) if e.name == *name))
            });
            ft.alias_tvars(&mut known);
            let ft = ft.declaring(&constraints);
            ft.check_quantifiers_held()?;
            Arc::new(ft)
        };
        typ.lambda_ids.set_id(id);
        typ.claim(level);
        let table = SArc::new(DefTables::default());
        let (shared, enclosing) = match &ctx.body {
            Some(outer) => (
                outer.share.lock().literals.entry(spec.id).or_default().clone(),
                outer.declared.clone(),
            ),
            None => (BodyShare::default(), Arc::from_iter([])),
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
        Node::new(Self { spec, def: def.clone(), typ, resident: TagValue::stale(def) })
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
    _at: AtLevel,
    sig: Arc<FnType>,
    faux_id: BindId,
    args: LPooled<Vec<Node<R>>>,
    scope: Scope,
    rigid: LPooled<Vec<RigidGate>>,
    /// Each quantifier the definition declares, at the depth it had at the
    /// gate's opening.
    declared: LPooled<Vec<(TVar, u32)>>,
}

impl<R: Rt> DefGate<R> {
    fn open(ctx: &mut CompileCtx<R>, def: &LambdaDef<R>) -> Self {
        let at = AtLevel::enter(def.level);
        let args = def.typ.args.iter().map(|at| Nop::new(at.typ.clone())).collect();
        let faux_id = BindId::new();
        ctx.env.by_id.insert(
            faux_id,
            Arc::new(Bind {
                doc: None,
                id: faux_id,
                name: "#faux".into(),
                scope: def.scope.lexical.clone(),
                typ: Type::empty_tvar(),
                pos: SourcePosition::default(),
                ori: Arc::new(Origin::default()),
                binder: None,
            }),
        );
        let scope = def.scope.with_catch((faux_id, ExprId::new()), HandlerKind::Catch);
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        // CR claude for claude: [bug/low] A declared variable that only the bound of a
        // bound names is not held rigid in the definition's check, so the body decides it:
        // `'a: [i64, string], 'c: ['a, null], 'b: ['c, `X] |x: 'b| -> [`X, null, i64] x`
        // checks and binds the declared 'a to i64, and a call with "str" is then refused
        // against 'a: i64 (with 'a: Any and a return of [`X, null], 'c's bound collapses
        // to null). Two levels ('a: [i64, string], 'b: ['a, null]) are held and refused.
        // It breaks "a def's declared tvars are rigid in its body check"; the effect is a
        // narrowed signature, not a wrong answer. Likely `named` here collects a
        // variable's bounds one level deep (FnType::collect_tvars); every declared
        // quantifier reached through bounds should be gated. Type::covers_by_parts holds
        // such a cell rigid while it probes (split_members), the coverage half of it.
        // probes: ~/ptmp/cr29/scratch/review-r14/B-r2-2/lt/{c10,c11,c16,c17,c18}.gx
        // (bound-chain-rigid-01)
        def.typ.collect_tvars(&mut named);
        named.retain(|name, _| !name.starts_with('_'));
        // a rank-2 formal's own quantifiers are held too: a use of the
        // formal in the body takes an instance of it, never binds them.
        // One an earlier task created and marked is any instance of itself
        // to this task already, which writes nothing of it.
        let mut quantifiers: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        def.typ.inner_quantifiers(&mut quantifiers);
        quantifiers.retain(|_, tv| !(tv.earlier_task() && tv.is_formal_quantifier()));
        for tv in quantifiers.values() {
            tv.mark_formal_quantifier()
        }
        let rigid = named
            .values()
            .map(|tv| tv.open_rigid())
            .chain(quantifiers.values().map(|tv| tv.open_quantifier_rigid()))
            .collect();
        let declared = def
            .typ
            .quantifiers
            .iter()
            .filter_map(|q| named.get(q))
            .map(|tv| (tv.clone(), tv.level().depth()))
            .collect();
        ctx.rec_defs.insert(def.id);
        ctx.def_gate_depth += 1;
        ctx.pending_settles.push(crate::SettleFrame::default());
        Self {
            def: def.id,
            depth: def.level.depth(),
            _at: at,
            sig: def.typ.clone(),
            faux_id,
            args,
            scope,
            rigid,
            declared,
        }
    }

    /// A quantifier the definition declares stays its own: the body binds
    /// no cell outside the definition to a type holding it (which would
    /// lower it to that cell's depth), since each call picks it anew. A
    /// variable the signature only names is lowered with that cell, and
    /// the definition is monomorphic in it.
    fn check_declared_kept(&self) -> Result<()> {
        for (tv, depth) in self.declared.iter() {
            if tv.level().depth() < *depth {
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

    /// What the body raised: ⊥ when nothing joined the faux catch; an
    /// open callback's `throws 'e` stays the cell.
    fn thrown(&self, ctx: &CompileCtx<R>) -> Type {
        match ctx.env.by_id.get(&self.faux_id).map(|b| &b.typ) {
            Some(Type::TVar(tv)) => tv.binding().unwrap_or(Type::Bottom),
            Some(t) => t.clone(),
            None => Type::Bottom,
        }
    }

    /// The body's sites settle with the enclosing statement, all but the
    /// cells this signature reaches: those stay open, generalized.
    fn close(mut self, ctx: &mut CompileCtx<R>, table: Option<(SArc<DefTable>, &Expr)>) {
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
        ctx.pending_settles.last_mut().expect("root settle frame").append(frame);
        ctx.rec_defs.remove(&self.def);
        ctx.env.by_id.remove(&self.faux_id);
        self.rigid.clear();
    }
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

fn capture_names<R: Rt>(
    node: &Node<R>,
    outer: &Lexical,
    checked: &Env,
    captures: &mut AHashSet<(ModPath, CompactString)>,
) {
    let mut add = |id| {
        if let Some(b) = checked.by_id.get(&id)
            && outer.binds.get(&b.scope).and_then(|bs| bs.get(&b.name)) == Some(&id)
        {
            captures.insert((b.scope.clone(), b.name.clone()));
        }
    };
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
// XCR claude for claude: [bug/low] A recursive typedef a labeled default's block declares
// outlives its definition: the default's check node, whose TypeDef node owns the
// definition, is discarded here, while the parameter's type, which the default decided,
// still names it. `{ let f = |#d = { type L<'a> = [`Nil, `Cons('a, L<'a>)]; let l:
// L<i64> = `Cons(1, `Nil); l }, x: i64| (d, x); println(f(1)); 0 }` logs "type `L`
// outlived its definition" (ERROR, graphix_types::typ) in every engine and session under
// graphix-fuzz run; the values agree and the shell, which shows only the failure target,
// prints nothing. Own the definitions the default's type names past the check, as
// DefTable::typedefs owns a body's. probes: ~/ptmp/cr29/scratch/review-r6/B2-r1-1/d1j.gx,
// d1.gx, d8.gx (default-typedef-outlived-01)
// 2026-11-02 claude: Parked after lane E's three rounds (r6 wave 4, branch cr40-E, not
// landed). Owning the definitions at the omitting call site (e3c0ff5af) and at the
// definition's check (458a6697e), then filling a default's names in its own env before
// the discard (32eb6eb04, DefaultNames), then narrowing that fill (964011ad4): each
// review found a new shape, and the last round regressed main (review-r6/E4-r3-2 p5m: t
// == wrong true; p3: the check passes, the build refuses). The class is wider than a log
// line. On main, a type a default's block writes names a typedef of its own block or of
// the enclosing body, the default's checked node is discarded, and the name is later
// filled in another env or not at all. Wrong casts follow (E4-r3-1 a1: a json::read into
// a type the default decided gives 'undefined type L'; a1ns accepts data of the outer
// typedef of that name). So do wrong type tests of an abstract's parameters (rt1b:
// default ((0, 0), 1) where the body gives ((1, 0), 1)), and one engine divergence: a
// default a struct field hides (r6-E/r6G2/h3.gx: interp produces nothing, jit answers).
// See next-discuss.md 'type names in a scope that ends'. Readdressed to eric.
// 2026-11-03 claude: Eric ruled (2026-11-03): D10 (b) in ~/ptmp/cr-discuss.md: keep a
// block's type definitions findable after the block while anything names them (they sit
// under a scope unique to the block), so a late lookup finds the right definition and
// every default can take its definition's types; a short design pass first, then a lane.
// 2026-11-04 claude: Fixed by D10 (b) (lane N r15): DefTable owns every typedef its
// body's and its defaults' blocks declare (add_rows takes each TypeDef node's
// definition), the registry Env::block_types finds a default's block type after the
// default's check is discarded, and record_defaults keeps rows for every default.
// d1j/d1/d8 log nothing; h3.gx agrees in every engine; a1.gx reads the default's L, a1ns
// refuses the outer L's data. Pins lang::block_types::default_block_type_through_a_field
// (all four modes fail with the registry disabled), json_reads_a_defaults_block_type
// (base refuses its build), lang::select::default_at_a_bind_declares_a_recursive_type
// (fails in all four with the registry disabled),
// lang::image::a_restored_block_type_is_found.
// 2026-11-04 claude: Correction to the 2026-11-04 note (review N-r1-2):
// default_block_type_through_a_field and
// lang::select::default_at_a_bind_declares_a_recursive_type do not depend on the registry
// lookup; the first depends on record_defaults keeping rows (with names_type_anywhere
// restored its interp mode fails 'type L outlived its definition'), the second passes on
// main. DefTable no longer takes each block TypeDef node's definition (no program showed
// a loss without it): it owns what its rows name. The registry lookup is pinned by the D2
// shapes (lang::block_types::block_type_over_an_outer_one_*),
// abstract_parameter_a_default_type_test_decides_compares and
// lang::image::a_restored_block_type_is_found.
// 2026-11-04 claude: Round 15 lane N built D10 (b) on branch cr50-N-parked
// (b7bb932ee..d20bed90e) and was parked one finding short: D2's fusion aborts are fixed
// at the root (a registry of block typedefs that outlive their block, a block's name
// meaning its type throughout the block), census 0 changes, gate green, and guicheck
// drops from 94G to 42G instructions (DefTable::record now fills rows in an env with the
// import tables). Left: the registry is keyed by scope names holding ExprIds, which a
// restore renumbers, so a block compiled again in a warm session declares a second
// definition; a function above a block's typedef that outlives its slot runs cold and
// fails warm ("type L outlived its definition"), including programs main runs (an outer
// type of the same name; an abstract constructor), and one shape binds warm and reads a
// wrong value. CR block-type-image-scope-01 on the branch. Back to Eric as the D10
// follow-up in ~/ptmp/cr29/next-discuss.md; probes ~/ptmp/cr29/scratch/review-r15/N-r*/.
// 2026-11-05 claude: Eric ruled (2026-11-05) on the D10 follow-up: (b), and the
// whole-block rule is confirmed (a name a block declares means the block's type
// throughout the block, its two silent value changes included). Lane N (cr50-N-parked)
// resumes: a block's generated scope name comes from its source position, qualified by
// the source's identity (file or module, a REPL input's number), never an ExprId, so a
// cold run and a warm image agree (block-type-image-scope-01 on the branch); then it
// lands.
// 2026-11-05 claude: Lane N r16: rebased onto main 462ccf70d (gate green before any
// change); Eric confirmed the whole-block rule (2026-11-05, D10 follow-up), and block
// scopes are now named by source place, not ExprId (block-type-image-scope-01), so the
// registry this fix rests on finds a restored block's definition warm as cold. The pins
// named above still pass at the lane's final HEAD.
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
        let declared = holds_unbound(&typ, &|tv| {
            tv.is_rigid() && (enclosing(tv) || tv.is_quantifier_rigid())
        });
        match &at.typ {
            // no call instantiates an enclosing definition's variable or
            // a rank-2 formal's quantifier the default sits under: the
            // default fits for every instance of it, or not at all
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
                let contain = |t: &Type| t.check_contains(&ctx.env, &typ);
                super::callsite::contain_generally(ctx, &held, node, contain)
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
            ctx.lambda_defs.remove(&def.id);
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
        crate::defer_unresolved_names(ctx, &Type::Fn(def.typ.clone()), spec)?;
        // Every arg, defaulted labeled ones included, checks as a Nop of
        // its declared type; the defaults themselves are checked after
        // the body (`check_defaults`), and again per omitting call site
        // (`CallSite::check_omitted_defaults`), where one may narrow that
        // site's cells.
        let mut gate = DefGate::open(ctx, def);
        let mut table = None;
        let mut captures: LPooled<AHashSet<(ModPath, CompactString)>> = LPooled::take();
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
            // against the param's own declared cells (`CompileCtx::def_gate_params`)
            let mut param_knot: LPooled<Vec<BindId>> = LPooled::take();
            if let ApplyView::Lambda(g) = f.view() {
                for (i, (pat, at)) in g.args().iter().zip(ftyp.args.iter()).enumerate() {
                    if at.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
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
        let mut defaults: LPooled<Vec<Node<R>>> = LPooled::take();
        let res = res
            .and_then(|()| {
                check_defaults(ctx, def, &gate.scope, &mut captures, &mut defaults)
            })
            .and_then(|()| gate.check_declared_kept().at(spec));
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
            def.table.set(Tables { table: SArc::new(table), outer: None });
        }
        for n in defaults.drain(..) {
            ctx.discard(n)
        }
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
        {
            let _at = AtLevel::enter(level);
            self.typ.check_adopts(&ctx.env, &row).at(&self.spec)?;
        }
        def.typ.generalize(def.level.depth());
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
