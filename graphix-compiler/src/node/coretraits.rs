//! The core traits `Eq`, `Ord`, `Display` and `Cast`.
//!
//! A user implementation is consulted from `Value`'s own
//! `eq`/`partial_cmp`/`Debug` on a `GxAbstract`, and from the cast walk
//! and the writers for `Cast`, through a thread-local handle loaned by
//! [`with_hooks`]; without a loan or an implementation the structural
//! case applies, and `Cast` refuses. A bottoming implementation resolves
//! per key like NaN: a bottom key sorts below every real key and equal
//! to other bottom keys.

use super::genn::SynthCall;
use crate::{
    BindId, Event, ExecCtx, Rt, Scope, TagValue, View,
    abstract_value::{self, CastHook, GxAbstract, ValueHookDispatch},
    env::{Env, ImplDef},
    expr::ExprId,
    typ::{AbstractId, CoreTrait, FnType, TVar, Type},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::cmp::Ordering;
use triomphe::Arc;

/// A core trait method the runtime calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Method {
    Eq,
    Hash,
    Cmp,
    Fmt,
    ToValue,
    FromValue,
}

impl Method {
    fn of(self) -> CoreTrait {
        match self {
            Self::Eq | Self::Hash => CoreTrait::Eq,
            Self::Cmp => CoreTrait::Ord,
            Self::Fmt => CoreTrait::Display,
            Self::ToValue | Self::FromValue => CoreTrait::Cast,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::Eq => "eq",
            Self::Hash => "hash",
            Self::Cmp => "cmp",
            Self::Fmt => "fmt",
            Self::ToValue => "to_value",
            Self::FromValue => "from_value",
        }
    }

    /// The types of a site's arguments, for the implementing type `typ`.
    fn arg_types(self, typ: &Type) -> SmallVec<[Type; 2]> {
        match self {
            Self::Eq | Self::Cmp => SmallVec::from_iter([typ.clone(), typ.clone()]),
            Self::Hash | Self::Fmt | Self::ToValue => SmallVec::from_iter([typ.clone()]),
            Self::FromValue => SmallVec::from_iter([Type::Any]),
        }
    }
}

/// A hook: the implementation's method binding, the type it was found
/// for and the method's signature at that instantiation.
#[derive(Debug, Clone)]
struct Hook {
    bind: BindId,
    typ: Type,
    ftype: Arc<FnType>,
}

/// The hook of the method `m` of the implementation `im`, at `target`;
/// `open` is the substitution of the declared variables that matched it.
/// The method's own signature is instantiated through it: a declared
/// variable settles to ⊥ once the body is checked, which a plain reset
/// would carry as a solved fact. `None` is a `Cast` method the
/// implementation leaves out: the representation.
fn hook_of(
    env: &Env,
    m: Method,
    im: &ImplDef,
    target: &Type,
    open: &AHashMap<ArcStr, Type>,
) -> Result<Option<Hook>> {
    let t = m.of();
    let def = env
        .trait_def(t.id())
        .ok_or_else(|| anyhow!("core trait {:?} is not defined", t))?;
    let decl = def
        .methods
        .iter()
        .find(|d| d.name == m.name())
        .ok_or_else(|| anyhow!("core trait {} lacks {}", def.name, m.name()))?;
    let bind = match im.methods.get(m.name()).copied().or(decl.default) {
        Some(bind) => bind,
        None if CoreTrait::runtime_method(t.id(), m.name()) => return Ok(None),
        None => {
            return Err(anyhow!(
                "impl {} for {} has no {}",
                def.name,
                im.target,
                m.name()
            ));
        }
    };
    let Some(ftype) = method_ftype(env, bind) else {
        return Err(anyhow!("core trait method {bind:?} is not a function"));
    };
    Ok(Some(Hook {
        bind,
        typ: target.clone(),
        ftype: Arc::new(ftype.replace_tvars(open)),
    }))
}

/// What a cached site was built against: the trait's impl list, which
/// a registration or removal replaces, and the registry's reload
/// generation.
#[derive(Clone)]
struct Version {
    impls: Option<Arc<Vec<Arc<ImplDef>>>>,
    generation: u64,
}

impl Version {
    fn same(&self, other: &Self) -> bool {
        self.generation == other.generation
            && match (&self.impls, &other.impls) {
                (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                (None, None) => true,
                _ => false,
            }
    }
}

/// The implementation of `t` that applies to a value of the abstract
/// type `typ`: a fresh head (the declared variables open, carrying
/// their bounds) must contain the type and be contained by it, so
/// `Pair<'a, 'a>` binds one `'a` for both positions, `Marker<'a: Mark>`
/// checks the bound, and `Marker<i64>` does not cover
/// `Marker<[i64, string]>`. A type with an open cell has no
/// implementation yet.
fn impl_for(
    env: &Env,
    t: CoreTrait,
    typ: &Type,
) -> Option<(Arc<ImplDef>, LPooled<AHashMap<ArcStr, Type>>)> {
    let Type::Abstract { id, .. } = typ else { return None };
    if typ.has_unbound() {
        return None;
    }
    for (im, canonical) in impls_on(env, t, *id) {
        let open: LPooled<AHashMap<ArcStr, Type>> = im
            .params
            .iter()
            .map(|tv| (tv.name.clone(), Type::TVar(TVar::empty_named(tv.name.clone()))))
            .collect();
        for tv in im.params.iter() {
            let Some(Type::TVar(fresh)) = open.get(&tv.name) else { continue };
            for c in tv.cell_constraints() {
                fresh.add_cell_constraint(c.replace_tvars(&open));
            }
        }
        let head = canonical.replace_tvars(&open);
        let applies = head.contains(env, typ).unwrap_or(false)
            && typ.contains(env, &head).unwrap_or(false);
        if applies {
            return Some((im.clone(), open));
        }
    }
    None
}

/// The implementations of `t` whose target is the abstract type `id`,
/// with the target looked through.
fn impls_on(
    env: &Env,
    t: CoreTrait,
    id: AbstractId,
) -> impl Iterator<Item = (Arc<ImplDef>, Type)> {
    env.impls_of(t.id())
        .into_iter()
        .flat_map(|l| (0..l.len()).map(move |i| l[i].clone()))
        .filter_map(move |im| {
            let canonical = match &im.target {
                Type::Ref(_) => im.target.lookup_ref(env).ok()?,
                t => t.clone(),
            };
            match &canonical {
                Type::Abstract { id: target, .. } if *target == id => {
                    Some((im, canonical))
                }
                _ => None,
            }
        })
}

/// The method signature behind a binding, for the `Impl` node's
/// prototype call sites.
pub(crate) fn method_ftype(env: &Env, bind: BindId) -> Option<Arc<FnType>> {
    match env.by_id.get(&bind).map(|b| &b.typ) {
        Some(Type::Fn(ft)) => Some(ft.clone()),
        _ => None,
    }
}

/// One hook call site: a static call to the implementation's method
/// binding over synthesized argument bindings the dispatch writes
/// before each call.
struct HookSite<R: Rt> {
    call: SynthCall<R>,
    first: bool,
}

/// The hook of one implementation and a pool of sites bound at one
/// concrete instantiation; a re-entrant dispatch uses a fresh site. No
/// hook is the representation.
struct Candidate<R: Rt> {
    hook: Option<Hook>,
    pool: Vec<HookSite<R>>,
}

/// The state for one `(trait, AbstractId)` pair, valid while the
/// trait's implementation list is the one it was resolved against:
/// per concrete instantiation met, the implementation that applies
/// (`None` = structural).
struct SiteEntry<R: Rt> {
    version: Version,
    by_type: SmallVec<[(Type, Option<Candidate<R>>); 1]>,
}

impl<R: Rt> SiteEntry<R> {
    /// The slot of the instantiation `typ`, resolved at its first sight.
    fn slot(&mut self, env: &Env, m: Method, typ: &Type) -> usize {
        match self.by_type.iter().position(|(seen, _)| seen == typ) {
            Some(i) => i,
            None => {
                let candidate = impl_for(env, m.of(), typ).and_then(|(im, open)| {
                    match hook_of(env, m, &im, typ, &open) {
                        Ok(hook) => Some(Candidate { hook, pool: Vec::new() }),
                        Err(e) => {
                            log::error!("core trait lookup for {typ}: {e:?}");
                            None
                        }
                    }
                });
                self.by_type.push((typ.clone(), candidate));
                self.by_type.len() - 1
            }
        }
    }

    fn candidate(&mut self, i: usize, typ: &Type) -> Option<&mut Candidate<R>> {
        match self.by_type.get_mut(i) {
            Some((seen, c)) if seen == typ => c.as_mut(),
            _ => None,
        }
    }
}

/// The per-context registry, keyed `(method, tag)`. An entry resolves on
/// first use and is rebuilt when the trait's implementation list
/// changes or a dynamic module loads ([`Version`]). The seam runs its sites over events of
/// its own (`spare`, the ones not in a dispatch), never the caller's,
/// so a loan lends the context to nothing but the dispatch.
pub struct CoreHookSites<R: Rt> {
    sites: AHashMap<(u8, AbstractId), SiteEntry<R>>,
    spare: Vec<Event>,
    /// Bumped at each dynamic module load: a site built before it may
    /// call a method binding the load rebound.
    generation: u64,
}

impl<R: Rt> CoreHookSites<R> {
    pub(crate) fn is_empty(&self) -> bool {
        self.sites.is_empty()
    }

    /// Retire every site: a dynamic module loaded.
    pub(crate) fn reloaded(&mut self) {
        self.generation += 1
    }

    fn take_event(&mut self) -> Event {
        self.spare.pop().unwrap_or_else(Event::new)
    }

    fn give_event(&mut self, mut event: Event) {
        event.clear();
        self.spare.push(event);
    }
}

impl<R: Rt> Default for CoreHookSites<R> {
    fn default() -> Self {
        Self { sites: AHashMap::new(), spare: Vec::new(), generation: 0 }
    }
}

impl<R: Rt> std::fmt::Debug for CoreHookSites<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CoreHookSites({})", self.sites.len())
    }
}

fn build_site<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    m: Method,
    h: &Hook,
) -> Result<HookSite<R>> {
    let top_id = ExprId::new();
    let prefix = format_compact!("#seam{}", top_id.inner());
    let types = m.arg_types(&h.typ).into_iter();
    // built mid-cycle, as a run-time bind is: a task of its own, its
    // settles drained when it returns
    let call = super::with_runtime_settles(ctx, |ctx| {
        SynthCall::build(ctx, &Scope::root(), &prefix, h.bind, &h.ftype, types, top_id)
    });
    ctx.apply_deferred();
    Ok(HookSite { call: call?, first: true })
}

/// What a dispatch found.
enum Hooked {
    /// No implementation applies.
    None,
    /// A `Cast` method the implementation leaves out.
    Representation,
    /// The implementation ran: its value, or `None` when it produced none
    /// this cycle (the bottom the callers' rules resolve).
    Ran(Option<Value>),
}

impl Hooked {
    /// A structural trait's answer: `None` = no implementation.
    fn ran(self) -> Option<Option<Value>> {
        match self {
            Self::Ran(v) => Some(v),
            Self::None | Self::Representation => None,
        }
    }

    fn cast(self) -> Option<CastHook> {
        match self {
            Self::None => None,
            Self::Representation => Some(CastHook::Representation),
            Self::Ran(v) => Some(CastHook::Ran(v)),
        }
    }
}

/// Run the method `m` of the implementation for the abstract type `typ`
/// on `args`.
fn call_hook<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    m: Method,
    typ: &Type,
    args: &[Value],
) -> Hooked {
    let mut event = ctx.core_hook_sites.lock().take_event();
    // a site outlives the walk that built it and runs on spare events
    // only: it registers, and so unregisters, under no boundary
    let boundary = ctx.boundary.take();
    let r = call_hook_over(&mut ctx.with_event(&mut event), m, typ, args);
    ctx.boundary = boundary;
    ctx.core_hook_sites.lock().give_event(event);
    r
}

/// The site loaned out of the registry for one dispatch: its slot, so
/// it returns to the pool it came from, or is deleted when the entry
/// was rebuilt meanwhile.
struct Loan<R: Rt> {
    key: (u8, AbstractId),
    version: Version,
    slot: usize,
    typ: Type,
    site: HookSite<R>,
}

enum Taken<R: Rt> {
    None,
    Representation,
    Site(Loan<R>),
}

/// Take a site for `typ` out of the registry, building one when the
/// pool is empty. The entry stays in the registry, so a re-entrant
/// dispatch for the same tag finds it; a nested call takes another site.
/// The registry is locked only while it is read or written: a site is
/// built and run outside it.
fn take_site<R: Rt>(ctx: &mut ExecCtx<'_, R>, m: Method, typ: &Type) -> Taken<R> {
    let Type::Abstract { id, .. } = typ else { return Taken::None };
    let key = (m as u8, *id);
    let impls = ctx.env.impls.get(&m.of().id()).cloned();
    let (version, stale, found) = {
        let mut reg = ctx.core_hook_sites.lock();
        let version = Version { impls, generation: reg.generation };
        let stale = match reg.sites.get(&key) {
            Some(e) if !e.version.same(&version) => reg.sites.remove(&key),
            _ => None,
        };
        let entry = reg.sites.entry(key).or_insert_with(|| SiteEntry {
            version: version.clone(),
            by_type: SmallVec::new(),
        });
        let slot = entry.slot(&ctx.cx.env, m, typ);
        let found = entry.candidate(slot, typ).map(|c| match &c.hook {
            None => None,
            Some(hook) => Some((slot, c.pool.pop().ok_or_else(|| hook.clone()))),
        });
        (version, stale, found)
    };
    for (_, c) in stale.into_iter().flat_map(|e| e.by_type) {
        for mut s in c.into_iter().flat_map(|c| c.pool) {
            s.call.delete(ctx);
        }
    }
    let (slot, site) = match found {
        None => return Taken::None,
        Some(None) => return Taken::Representation,
        Some(Some(found)) => found,
    };
    let site = match site {
        Ok(s) => s,
        Err(hook) => match build_site(ctx, m, &hook) {
            Ok(s) => s,
            Err(e) => {
                log::error!("core trait site for {}: {e:?}", hook.typ);
                // the registry was unlocked while the site was built: clear
                // the slot only if it is still this version's and this type's
                if let Some(e) = ctx.core_hook_sites.lock().sites.get_mut(&key)
                    && e.version.same(&version)
                    && let Some((seen, c)) = e.by_type.get_mut(slot)
                    && seen == typ
                {
                    *c = None;
                }
                return Taken::None;
            }
        },
    };
    Taken::Site(Loan { key, version, slot, typ: typ.clone(), site })
}

fn return_site<R: Rt>(ctx: &mut ExecCtx<'_, R>, loan: Loan<R>) {
    let rejected = {
        let mut reg = ctx.core_hook_sites.lock();
        let pool = reg
            .sites
            .get_mut(&loan.key)
            .filter(|e| e.version.same(&loan.version))
            .and_then(|e| e.candidate(loan.slot, &loan.typ));
        match pool {
            Some(c) => {
                c.pool.push(loan.site);
                None
            }
            None => Some(loan.site),
        }
    };
    if let Some(mut site) = rejected {
        site.call.delete(ctx)
    }
}

fn call_hook_over<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    m: Method,
    typ: &Type,
    args: &[Value],
) -> Hooked {
    let mut loan = match take_site(ctx, m, typ) {
        Taken::None => return Hooked::None,
        Taken::Representation => return Hooked::Representation,
        Taken::Site(loan) => loan,
    };
    let s = &mut loan.site;
    for (id, v) in s.call.args.iter().zip(args.iter()) {
        ctx.deliver(*id, TagValue::fired(v.clone()));
    }
    let view =
        if std::mem::replace(&mut s.first, false) { View::Birth } else { View::Cycle };
    let site = &mut s.call.site;
    let r = ctx.under(view, |ctx| {
        let tv = site.update(ctx);
        if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) }
    });
    return_site(ctx, loan);
    Hooked::Ran(r)
}

/// The context a `with_hooks` frame lent as a hook's `state`.
///
/// # Safety
/// `state` is that frame's context, the frame is live, and nothing else
/// uses the context while the dispatch runs.
unsafe fn lent<'s, R: Rt>(state: *mut u8) -> &'s mut ExecCtx<'s, R> {
    unsafe { &mut *(state as *mut ExecCtx<'s, R>) }
}

/// Re-wrap a `GxAbstract` (received by reference inside the vtable
/// call) as the `Value` a hook site's argument binding carries.
fn as_value(g: &GxAbstract) -> Value {
    abstract_value::wrap(g.id, g.name.clone(), g.params.clone(), g.payload.clone())
}

fn warn_pair_bottom(t: CoreTrait, a: &GxAbstract, answer: impl std::fmt::Display) {
    log::warn!(
        "core {:?} implementation for {} bottoms on a pair whose keys are both \
         real (neither self-comparison bottoms) — an inconsistent implementation; \
         answering {answer}",
        t,
        a.name
    );
}

/// Run the method `m` on a pair of one abstract type: `None` = no
/// implementation, or a pair constructed at two instantiations, which
/// takes the structural case.
fn pair<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    m: Method,
    a: &GxAbstract,
    b: &GxAbstract,
) -> Option<Option<Value>> {
    let typ = a.typ();
    if b.typ() != typ {
        return None;
    }
    call_hook(ctx, m, &typ, &[as_value(a), as_value(b)]).ran()
}

/// Does the implementation bottom on the key `k`?
fn key_bottoms<R: Rt>(ctx: &mut ExecCtx<'_, R>, m: Method, k: &GxAbstract) -> bool {
    matches!(pair(ctx, m, k, k), Some(None))
}

fn dispatch_eq_applies<R: Rt>(state: *mut u8, a: &GxAbstract) -> bool {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    impls_on(&ctx.env, CoreTrait::Eq, a.id).next().is_some()
}

fn dispatch_eq<R: Rt>(state: *mut u8, a: &GxAbstract, b: &GxAbstract) -> Option<bool> {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    match pair(ctx, Method::Eq, a, b)? {
        Some(Value::Bool(x)) => Some(x),
        Some(v) => {
            log::warn!("core Eq for {} returned a non-bool {v:?}", a.name);
            Some(false)
        }
        // bottom keys are equal to each other and to nothing real
        None => {
            let ab = key_bottoms(ctx, Method::Eq, a);
            let bb = key_bottoms(ctx, Method::Eq, b);
            Some(match (ab, bb) {
                (true, true) => true,
                (true, false) | (false, true) => false,
                (false, false) => {
                    // XCR claude for claude: [bug/low] This logs "answering Equal" and
                    // then returns false: an inconsistent Eq pair compares unequal on
                    // both engines. design/traits.md says such a pair is answered Equal
                    // and that eq follows the same shape. Decide which answer an
                    // inconsistent Eq pair gets, make the code and the design agree,
                    // have warn_pair_bottom name the answer each trait gives, and pin
                    // it next to core_bottom_key_rule (no fixture covers this branch).
                    // probe:
                    // design/review-2026-10-26/repro/tests-fusion-traits-eq-inconsistent-pair.gx
                    // (tests-fusion-traits-06)
                    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q35, An
                    // inconsistent Eq pair): yes, fix the doc (the recommendation: unequal:
                    // nothing merges on an inconsistent impl. Fix the doc.)
                    // 2026-10-27 codex: Documented q35's unequal result for an inconsistent Eq
                    // pair. The implementation already returned false; its warning incorrectly
                    // said Equal. The warning now says false, and design/traits.md describes
                    // that result. Pin lang::traits::core_inconsistent_eq_pair_is_unequal passes
                    // in all four modes before and after (documented existing behavior). The
                    // Rust pin lang::traits::inconsistent_eq_warning_matches_answer failed with
                    // the old warning deliberately restored and passed after restoration of the
                    // fix. The selected trait suite passes 579 tests, excluding nine
                    // socket-backed fixtures.
                    warn_pair_bottom(CoreTrait::Eq, a, false);
                    false
                }
            })
        }
    }
}

fn ordering_of(v: &Value) -> Option<Ordering> {
    match crate::rep::variant(v)? {
        (tag, []) if &**tag == "Less" => Some(Ordering::Less),
        (tag, []) if &**tag == "Equal" => Some(Ordering::Equal),
        (tag, []) if &**tag == "Greater" => Some(Ordering::Greater),
        _ => None,
    }
}

fn dispatch_cmp<R: Rt>(
    state: *mut u8,
    a: &GxAbstract,
    b: &GxAbstract,
) -> Option<Ordering> {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    match pair(ctx, Method::Cmp, a, b)? {
        Some(v) => match ordering_of(&v) {
            Some(o) => Some(o),
            None => {
                log::warn!("core Ord for {} returned a non-Ordering {v:?}", a.name);
                Some(Ordering::Equal)
            }
        },
        // a structural fallback per pair would break the total order;
        // per key it stays total: bottom keys below every real key,
        // equal among themselves
        None => {
            let ab = key_bottoms(ctx, Method::Cmp, a);
            let bb = key_bottoms(ctx, Method::Cmp, b);
            Some(match (ab, bb) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                (false, false) => {
                    warn_pair_bottom(CoreTrait::Ord, a, "Equal");
                    Ordering::Equal
                }
            })
        }
    }
}

fn dispatch_hash<R: Rt>(state: *mut u8, a: &GxAbstract) -> Option<Value> {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    call_hook(ctx, Method::Hash, &a.typ(), &[as_value(a)]).ran()?
}

fn dispatch_fmt<R: Rt>(state: *mut u8, a: &GxAbstract) -> Option<ArcStr> {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    match call_hook(ctx, Method::Fmt, &a.typ(), &[as_value(a)]).ran()? {
        Some(Value::String(s)) => Some(s),
        Some(v) => {
            log::warn!("core Display for {} returned a non-string {v:?}", a.name);
            None
        }
        None => {
            log::warn!(
                "core Display for {} produced no value; printing structurally",
                a.name
            );
            None
        }
    }
}

fn dispatch_to_value<R: Rt>(state: *mut u8, a: &GxAbstract) -> Option<CastHook> {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    call_hook(ctx, Method::ToValue, &a.typ(), &[as_value(a)]).cast()
}

fn dispatch_from_value<R: Rt>(
    state: *mut u8,
    target: &Type,
    v: &Value,
) -> Option<CastHook> {
    // SAFETY: with_hooks lent its context for this dispatch.
    let ctx = unsafe { lent::<R>(state) };
    call_hook(ctx, Method::FromValue, target, std::slice::from_ref(v)).cast()
}

/// Can a core-trait hook fire: is any core trait implemented?
pub fn hooks_live(env: &Env) -> bool {
    [CoreTrait::Eq, CoreTrait::Ord, CoreTrait::Display, CoreTrait::Cast]
        .into_iter()
        .any(|t| env.impls.get(&t.id()).is_some_and(|l| !l.is_empty()))
}

/// Loan the context to the value seam for the duration of `f`: any
/// `Value` comparison or print inside honors a core-trait
/// implementation. `f` sees no context, so the loan is exclusive; a
/// hook dispatches through the context over an event of the seam's
/// own. Loans nest.
pub fn with_hooks<R: Rt, T>(ctx: &mut ExecCtx<'_, R>, f: impl FnOnce() -> T) -> T {
    if !hooks_live(&ctx.env) {
        return f();
    }
    let handle = ValueHookDispatch {
        state: ctx as *mut ExecCtx<'_, R> as *mut u8,
        eq: dispatch_eq::<R>,
        eq_applies: dispatch_eq_applies::<R>,
        hash: dispatch_hash::<R>,
        cmp: dispatch_cmp::<R>,
        fmt: dispatch_fmt::<R>,
        to_value: dispatch_to_value::<R>,
        from_value: dispatch_from_value::<R>,
    };
    abstract_value::with_value_hooks(&handle, f)
}

/// Render or convert under the loan: `f` reads through the `Env` it is
/// given, the context's own when no hook can fire, else a snapshot
/// (the maps are persistent, so a clone shares every node), so a hook
/// that builds or runs its site through the context invalidates
/// nothing `f` reads.
pub fn with_display_hooks<R: Rt, T>(
    ctx: &mut ExecCtx<'_, R>,
    f: impl FnOnce(&Env) -> T,
) -> T {
    if !hooks_live(&ctx.env) {
        return f(&ctx.env);
    }
    let env = ctx.env.clone();
    with_hooks(ctx, || f(&env))
}
