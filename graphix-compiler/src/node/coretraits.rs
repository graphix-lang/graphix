//! The core traits `Eq`, `Ord` and `Display`.
//!
//! A user implementation is consulted from `Value`'s own
//! `eq`/`partial_cmp`/`Debug` on a `GxAbstract`, through a thread-local
//! handle loaned by [`with_hooks`]; without a loan or an
//! implementation the structural case applies. A bottoming
//! implementation resolves per key like NaN: a bottom key sorts below
//! every real key and equal to other bottom keys.

use super::genn;
use crate::{
    BindId, Event, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    abstract_value::{self, GxAbstract, ValueHookDispatch},
    env::{Env, ImplDef},
    expr::{ExprId, ModPath},
    typ::{AbstractId, FnType, IsAFlags, TVar, TraitId, Type},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{cmp::Ordering, sync::LazyLock};
use triomphe::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreTrait {
    Eq,
    Ord,
    Display,
}

static CORE_IDS: LazyLock<[TraitId; 3]> = LazyLock::new(|| {
    let core = ModPath::from(["core"]);
    [TraitId::of(&core, "Eq"), TraitId::of(&core, "Ord"), TraitId::of(&core, "Display")]
});

impl CoreTrait {
    pub fn id(self) -> TraitId {
        CORE_IDS[self as usize]
    }

    pub fn of_id(id: TraitId) -> Option<Self> {
        [Self::Eq, Self::Ord, Self::Display].into_iter().find(|t| t.id() == id)
    }

    fn method(self) -> &'static str {
        match self {
            Self::Eq => "eq",
            Self::Ord => "cmp",
            Self::Display => "fmt",
        }
    }

    fn arity(self) -> usize {
        match self {
            Self::Eq | Self::Ord => 2,
            Self::Display => 1,
        }
    }
}

/// A hook: the implementation's method binding, the type it was found
/// for (the argument type of the call site) and the method's signature
/// at that instantiation.
#[derive(Debug, Clone)]
struct Hook {
    bind: BindId,
    typ: Type,
    ftype: Arc<FnType>,
}

/// The hook of the implementation `im` of `t`, whose sites bind their
/// arguments at `target`; `open` is the substitution of the declared
/// variables that matched it. The method's own signature is
/// instantiated through it: a declared variable settles to ⊥ once the
/// body is checked, which a plain reset would carry as a solved fact.
fn hook_of(
    env: &Env,
    t: CoreTrait,
    im: &ImplDef,
    target: &Type,
    open: &AHashMap<ArcStr, Type>,
) -> Result<Hook> {
    let def = env
        .trait_def(t.id())
        .ok_or_else(|| anyhow!("core trait {:?} is not defined", t))?;
    let m = def
        .methods
        .iter()
        .find(|m| m.name == t.method())
        .ok_or_else(|| anyhow!("core trait {} lacks {}", def.name, t.method()))?;
    let Some(bind) = im.methods.get(t.method()).copied().or(m.default) else {
        return Err(anyhow!("impl {} for {} has no {}", def.name, im.target, m.name));
    };
    let ftype = match env.by_id.get(&bind).map(|b| &b.typ) {
        Some(Type::Fn(ft)) => Arc::new(ft.replace_tvars(open)),
        _ => return Err(anyhow!("core trait method {bind:?} is not a function")),
    };
    Ok(Hook { bind, typ: target.clone(), ftype })
}

/// The identity of the trait's implementation list: a registration or
/// removal replaces it.
/// The trait's implementation list, whose identity is the version an
/// entry was resolved against: a registration or removal replaces it.
fn impls_version(env: &Env, t: CoreTrait) -> Option<Arc<Vec<Arc<ImplDef>>>> {
    env.impls.get(&t.id()).cloned()
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
    let list = env.impls.get(&t.id())?;
    for im in list.iter() {
        let canonical = match &im.target {
            Type::Ref(_) => match im.target.lookup_ref(env) {
                Ok(t) => t,
                Err(_) => continue,
            },
            t => t.clone(),
        };
        match &canonical {
            Type::Abstract { id: target, .. } if target == id => (),
            _ => continue,
        }
        let open: LPooled<AHashMap<ArcStr, Type>> = im
            .params
            .iter()
            .map(|tv| {
                let fresh = TVar::empty_named(tv.name.clone());
                for c in tv.cell_constraints() {
                    fresh.add_cell_constraint(c);
                }
                (tv.name.clone(), Type::TVar(fresh))
            })
            .collect();
        let head = canonical.replace_tvars(&open);
        let applies = head.contains(env, typ).unwrap_or(false)
            && typ.contains(env, &head).unwrap_or(false);
        if applies {
            return Some((im.clone(), open));
        }
    }
    None
}

/// The method signature behind a binding, for the `Impl` node's
/// prototype call sites.
pub(crate) fn method_ftype(env: &Env, bind: BindId) -> Option<Arc<FnType>> {
    match env.by_id.get(&bind).map(|b| &b.typ) {
        Some(Type::Fn(ft)) => Some(ft.clone()),
        _ => None,
    }
}

/// The member of a union `ts` that `v` belongs to: the first strict
/// match, else the first structured plain match, else the first plain
/// match.
pub(crate) fn union_member(env: &Env, ts: &[Type], v: &Value) -> Option<usize> {
    let blind = |t: &Type| {
        t.with_deref(|t| matches!(t, None | Some(Type::Any) | Some(Type::Bottom)))
    };
    ts.iter()
        .position(|t| t.is_a_with(env, IsAFlags::Strict.into(), v))
        .or_else(|| ts.iter().position(|t| !blind(t) && t.is_a(env, v)))
        .or_else(|| ts.iter().position(|t| t.is_a(env, v)))
}

/// One hook call site: a static call to the implementation's method
/// binding over synthesized argument bindings the dispatch writes
/// before each call.
struct HookSite<R: Rt, E: UserEvent> {
    site: Node<R, E>,
    args: SmallVec<[BindId; 2]>,
    first: bool,
}

/// The hook of one implementation and a pool of sites bound at one
/// concrete instantiation; a re-entrant dispatch uses a fresh site.
struct Candidate<R: Rt, E: UserEvent> {
    hook: Hook,
    pool: Vec<HookSite<R, E>>,
}

/// The state for one `(trait, AbstractId)` pair, valid while the
/// trait's implementation list is the one it was resolved against:
/// per concrete instantiation met, the implementation that applies
/// (`None` = structural).
struct SiteEntry<R: Rt, E: UserEvent> {
    version: Option<Arc<Vec<Arc<ImplDef>>>>,
    by_type: SmallVec<[(Type, Option<Candidate<R, E>>); 1]>,
}

impl<R: Rt, E: UserEvent> SiteEntry<R, E> {
    /// The slot of the instantiation `typ`, resolved at its first sight.
    fn slot(&mut self, env: &Env, t: CoreTrait, typ: &Type) -> usize {
        match self.by_type.iter().position(|(seen, _)| seen == typ) {
            Some(i) => i,
            None => {
                let candidate =
                    impl_for(env, t, typ).and_then(|(im, open)| {
                        match hook_of(env, t, &im, typ, &open) {
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

    fn candidate(&mut self, i: usize, typ: &Type) -> Option<&mut Candidate<R, E>> {
        match self.by_type.get_mut(i) {
            Some((seen, c)) if seen == typ => c.as_mut(),
            _ => None,
        }
    }
}

/// The per-context registry, keyed `(trait, tag)`. An entry resolves on
/// first use and is rebuilt when the trait's implementation list
/// changes (`impls_version`). The seam runs its sites over events of
/// its own (`template`, the user event a loan seeded it with; `spare`,
/// the ones not in a dispatch), never the caller's, so a loan needs no
/// event from the caller and lends the context to nothing but the
/// dispatch.
pub struct CoreHookSites<R: Rt, E: UserEvent> {
    sites: ahash::AHashMap<(u8, AbstractId), SiteEntry<R, E>>,
    template: Option<E>,
    spare: Vec<Event<E>>,
}

impl<R: Rt, E: UserEvent> CoreHookSites<R, E> {
    pub(crate) fn is_empty(&self) -> bool {
        self.sites.is_empty()
    }

    fn take_event(&mut self) -> Option<Event<E>> {
        self.spare.pop().or_else(|| self.template.clone().map(Event::new))
    }

    fn give_event(&mut self, mut event: Event<E>) {
        event.clear();
        self.spare.push(event);
    }
}

impl<R: Rt, E: UserEvent> Default for CoreHookSites<R, E> {
    fn default() -> Self {
        Self { sites: ahash::AHashMap::new(), template: None, spare: Vec::new() }
    }
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for CoreHookSites<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CoreHookSites({})", self.sites.len())
    }
}

fn build_site<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    t: CoreTrait,
    h: &Hook,
) -> Result<HookSite<R, E>> {
    let ftype = &h.ftype;
    let scope = Scope::root();
    let top_id = ExprId::new();
    let mut args: SmallVec<[BindId; 2]> = SmallVec::new();
    let mut nodes: SmallVec<[Node<R, E>; 2]> = SmallVec::new();
    for k in 0..t.arity() {
        let name = format_compact!("#seam{}_{k}", top_id.inner());
        let (id, n) = genn::bind(ctx, &scope.lexical, &name, h.typ.clone(), top_id);
        args.push(id);
        nodes.push(n);
    }
    let fnode = genn::reference(ctx, h.bind, Type::Fn(ftype.clone()), top_id);
    let mut site = genn::apply(fnode, scope, nodes, &ftype, top_id);
    site.typecheck0(ctx)?;
    site.typecheck1(ctx)?;
    Ok(HookSite { site, args, first: true })
}

/// Run the implementation of `t` on `args`, values of one abstract
/// type. `None` = no implementation; `Some(None)` = it produced no
/// value this cycle (the bottom the callers' rules resolve);
/// `Some(Some(v))` = its result.
fn call_hook<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    t: CoreTrait,
    args: &[&GxAbstract],
) -> Option<Option<Value>> {
    let mut event = ctx.core_hook_sites.take_event()?;
    let r = call_hook_over(ctx, &mut event, t, args);
    ctx.core_hook_sites.give_event(event);
    r
}

/// The site loaned out of the registry for one dispatch: its slot, so
/// it returns to the pool it came from, or is deleted when the entry
/// was rebuilt meanwhile.
struct Loan<R: Rt, E: UserEvent> {
    key: (u8, AbstractId),
    version: Option<Arc<Vec<Arc<ImplDef>>>>,
    slot: usize,
    typ: Type,
    site: HookSite<R, E>,
}

fn same_version(
    a: &Option<Arc<Vec<Arc<ImplDef>>>>,
    b: &Option<Arc<Vec<Arc<ImplDef>>>>,
) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
        (None, None) => true,
        _ => false,
    }
}

/// Take a site for `args` out of the registry, building one when the
/// pool is empty. The entry stays in the registry, so a re-entrant
/// dispatch for the same tag finds it; a nested call takes another site.
// XCR codex for eric: [CR15, P2] done: the entry stays in the registry
// and only the site is loaned; a site returns to its slot or is deleted
// when the entry was rebuilt meanwhile.
// XCR codex for eric: [CR02, P1] done: every operand must be constructed
// at the instantiation the implementation was resolved for.
fn take_site<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    t: CoreTrait,
    args: &[&GxAbstract],
) -> Option<Loan<R, E>> {
    let typ = args[0].typ();
    // an implementation is for one instantiation: a pair constructed at
    // two takes the structural case
    if args[1..].iter().any(|g| g.typ() != typ) {
        return None;
    }
    let key = (t as u8, args[0].id);
    let version = impls_version(&ctx.env, t);
    let stale = ctx
        .core_hook_sites
        .sites
        .get(&key)
        .is_some_and(|e| !same_version(&e.version, &version));
    if stale {
        let mut e = ctx.core_hook_sites.sites.remove(&key).unwrap();
        for (_, c) in e.by_type.iter_mut() {
            for mut s in c.iter_mut().flat_map(|c| c.pool.drain(..)) {
                s.site.delete(ctx);
            }
        }
    }
    let entry = ctx.core_hook_sites.sites.entry(key).or_insert_with(|| SiteEntry {
        version: version.clone(),
        by_type: SmallVec::new(),
    });
    let slot = entry.slot(&ctx.env, t, &typ);
    let c = entry.candidate(slot, &typ)?;
    let site = match c.pool.pop() {
        Some(s) => s,
        None => {
            let hook = c.hook.clone();
            match build_site(ctx, t, &hook) {
                Ok(s) => s,
                Err(e) => {
                    log::error!("core trait site for {}: {e:?}", hook.typ);
                    if let Some(e) = ctx.core_hook_sites.sites.get_mut(&key) {
                        e.by_type[slot].1 = None;
                    }
                    return None;
                }
            }
        }
    };
    Some(Loan { key, version, slot, typ, site })
}

fn return_site<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<R, E>, mut loan: Loan<R, E>) {
    let pool = ctx
        .core_hook_sites
        .sites
        .get_mut(&loan.key)
        .filter(|e| same_version(&e.version, &loan.version))
        .and_then(|e| e.candidate(loan.slot, &loan.typ));
    match pool {
        Some(c) => c.pool.push(loan.site),
        None => loan.site.site.delete(ctx),
    }
}

fn call_hook_over<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    t: CoreTrait,
    args: &[&GxAbstract],
) -> Option<Option<Value>> {
    let mut loan = take_site(ctx, t, args)?;
    let s = &mut loan.site;
    // every dispatch is a fresh invocation
    s.site.reset_replay(ctx);
    for (id, g) in s.args.iter().zip(args.iter()) {
        let v = as_value(g);
        ctx.rt.store_insert(*id, TagValue::fired(v.clone()));
        event.variables.insert(*id, TagValue::fired(v));
    }
    if s.first {
        s.first = false;
        event.init = true;
    }
    let tv = s.site.update(ctx, event);
    let r = if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) };
    event.init = false;
    return_site(ctx, loan);
    Some(r)
}

struct HookState<R: Rt, E: UserEvent> {
    ctx: *mut ExecCtx<R, E>,
}

/// Re-wrap a `GxAbstract` (received by reference inside the vtable
/// call) as the `Value` a hook site's argument binding carries.
// XCR codex for eric: [CR16, P2] done: resolution reads the borrowed box;
// only a dispatch that found an implementation wraps its arguments.
fn as_value(g: &GxAbstract) -> Value {
    abstract_value::wrap(g.id, g.name.clone(), g.params.clone(), g.payload.clone())
}

fn warn_pair_bottom(t: CoreTrait, a: &GxAbstract) {
    log::warn!(
        "core {:?} implementation for {} bottoms on a pair whose keys are both \
         real (neither self-comparison bottoms) — an inconsistent implementation; \
         answering Equal",
        t,
        a.name
    );
}

/// Does the implementation bottom on the key `k`?
fn key_bottoms<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    t: CoreTrait,
    k: &GxAbstract,
) -> bool {
    matches!(call_hook(ctx, t, &[k, k]), Some(None))
}

fn dispatch_eq<R: Rt, E: UserEvent>(
    state: *mut u8,
    a: &GxAbstract,
    b: &GxAbstract,
) -> Option<bool> {
    // SAFETY: `state` points into the live `eval_with_hooks` frame.
    let s = unsafe { &mut *(state as *mut HookState<R, E>) };
    let ctx = unsafe { &mut *s.ctx };
    match call_hook(ctx, CoreTrait::Eq, &[a, b])? {
        Some(Value::Bool(x)) => Some(x),
        Some(v) => {
            log::warn!("core Eq for {} returned a non-bool {v:?}", a.name);
            Some(false)
        }
        // bottom keys are equal to each other and to nothing real
        None => {
            let ab = key_bottoms(ctx, CoreTrait::Eq, a);
            let bb = key_bottoms(ctx, CoreTrait::Eq, b);
            Some(match (ab, bb) {
                (true, true) => true,
                (true, false) | (false, true) => false,
                (false, false) => {
                    warn_pair_bottom(CoreTrait::Eq, a);
                    false
                }
            })
        }
    }
}

fn ordering_of(v: &Value) -> Option<Ordering> {
    match v {
        Value::String(s) if &**s == "Less" => Some(Ordering::Less),
        Value::String(s) if &**s == "Equal" => Some(Ordering::Equal),
        Value::String(s) if &**s == "Greater" => Some(Ordering::Greater),
        _ => None,
    }
}

fn dispatch_cmp<R: Rt, E: UserEvent>(
    state: *mut u8,
    a: &GxAbstract,
    b: &GxAbstract,
) -> Option<Ordering> {
    // SAFETY: as in `dispatch_eq`.
    let s = unsafe { &mut *(state as *mut HookState<R, E>) };
    let ctx = unsafe { &mut *s.ctx };
    match call_hook(ctx, CoreTrait::Ord, &[a, b])? {
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
            let ab = key_bottoms(ctx, CoreTrait::Ord, a);
            let bb = key_bottoms(ctx, CoreTrait::Ord, b);
            Some(match (ab, bb) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                (false, false) => {
                    warn_pair_bottom(CoreTrait::Ord, a);
                    Ordering::Equal
                }
            })
        }
    }
}

fn dispatch_fmt<R: Rt, E: UserEvent>(state: *mut u8, a: &GxAbstract) -> Option<ArcStr> {
    // SAFETY: as in `dispatch_eq`.
    let s = unsafe { &mut *(state as *mut HookState<R, E>) };
    let ctx = unsafe { &mut *s.ctx };
    match call_hook(ctx, CoreTrait::Display, &[a])? {
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

/// Can a core-trait hook fire: is any core trait implemented?
pub fn hooks_live(env: &Env) -> bool {
    [CoreTrait::Eq, CoreTrait::Ord, CoreTrait::Display]
        .into_iter()
        .any(|t| env.impls.get(&t.id()).is_some_and(|l| !l.is_empty()))
}

/// Remember the user event the seam's own events are made from. Every
/// loan that has an event seeds; a loan inside a builtin's `eval` has
/// none and needs its caller's (`CachedArgs::update`) to have.
pub fn seed<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<R, E>, event: &Event<E>) {
    if ctx.core_hook_sites.template.is_none() {
        ctx.core_hook_sites.template = Some(event.user.clone());
    }
}

/// Loan the context to the value seam for the duration of `f`: any
/// `Value` comparison or print inside honors a core-trait
/// implementation. `f` sees no context, so the loan is exclusive; a
/// hook dispatches through the context over an event of the seam's
/// own. Loans nest.
pub fn with_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &Event<E>,
    f: impl FnOnce() -> T,
) -> T {
    seed(ctx, event);
    eval_with_hooks(ctx, f)
}

/// [`with_hooks`] inside a builtin's `eval`, which has no event: armed
/// when a loan with one came first, else `f` runs unarmed (structural).
pub fn eval_with_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    f: impl FnOnce() -> T,
) -> T {
    if !hooks_live(&ctx.env) || ctx.core_hook_sites.template.is_none() {
        return f();
    }
    let mut state = HookState::<R, E> { ctx: ctx as *mut _ };
    let handle = ValueHookDispatch {
        state: &mut state as *mut HookState<R, E> as *mut u8,
        eq: dispatch_eq::<R, E>,
        cmp: dispatch_cmp::<R, E>,
        fmt: dispatch_fmt::<R, E>,
    };
    let _guard = abstract_value::arm_value_hooks(&handle as *const _);
    f()
}

/// Render or convert under the loan: `f` reads through the `Env` it is
/// given, the context's own when no hook can fire, else a snapshot
/// (the maps are persistent, so a clone shares every node), so a hook
/// that builds or runs its site through the context invalidates
/// nothing `f` reads.
pub fn with_display_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &Event<E>,
    f: impl FnOnce(&Env) -> T,
) -> T {
    seed(ctx, event);
    eval_with_display_hooks(ctx, f)
}

/// [`with_display_hooks`] inside a builtin's `eval`.
pub fn eval_with_display_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    f: impl FnOnce(&Env) -> T,
) -> T {
    if !hooks_live(&ctx.env) || ctx.core_hook_sites.template.is_none() {
        return f(&ctx.env);
    }
    let env = ctx.env.clone();
    eval_with_hooks(ctx, || f(&env))
}
