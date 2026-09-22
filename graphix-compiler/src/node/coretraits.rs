//! The core traits `Eq`, `Ord` and `Display`.
//!
//! A user implementation is consulted from `Value`'s own
//! `eq`/`partial_cmp`/`Debug` on a `GxAbstract`, through a thread-local
//! handle loaned by [`with_value_hooks`]; without a loan or an
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

/// A hook: the implementation's method binding and the type it was
/// found for (the argument type of the call site).
#[derive(Debug, Clone)]
struct Hook {
    bind: BindId,
    typ: Type,
}

/// The hook of the implementation `im` of `t`, whose sites bind their
/// arguments at `target`.
fn hook_of(env: &Env, t: CoreTrait, im: &ImplDef, target: &Type) -> Result<Hook> {
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
    Ok(Hook { bind, typ: target.clone() })
}

/// The identity of the trait's implementation list: a registration or
/// removal replaces it.
/// The trait's implementation list, whose identity is the version an
/// entry was resolved against: a registration or removal replaces it.
// XCR codex for eric: CR18 — done: the entry holds the list `Arc`, so its
// allocation cannot be reused while the entry stands.
fn impls_version(env: &Env, t: CoreTrait) -> Option<Arc<Vec<Arc<ImplDef>>>> {
    env.impls.get(&t.id()).cloned()
}

/// Every implementation of `t` whose target is the abstract type `id`,
/// with the payload type the target's parameters give it.
fn candidates_for<R: Rt, E: UserEvent>(
    env: &Env,
    t: CoreTrait,
    id: AbstractId,
) -> SmallVec<[Candidate<R, E>; 1]> {
    let mut out = SmallVec::new();
    let Some(list) = env.impls.get(&t.id()) else { return out };
    for im in list.iter() {
        let canonical = match &im.target {
            Type::Ref(_) => match im.target.lookup_ref(env) {
                Ok(t) => t,
                Err(_) => continue,
            },
            t => t.clone(),
        };
        let Type::Abstract { id: target, .. } = &canonical else { continue };
        if *target != id {
            continue;
        }
        // The declared variables are open here: a bound is checked at
        // the definition, and the settled cell says nothing about a use.
        let target = if im.params.is_empty() {
            canonical
        } else {
            let open: LPooled<AHashMap<ArcStr, Type>> = im
                .params
                .iter()
                .map(|tv| {
                    (tv.name.clone(), Type::TVar(TVar::empty_named(tv.name.clone())))
                })
                .collect();
            canonical.replace_tvars(&open)
        };
        match hook_of(env, t, im, &target) {
            Ok(hook) => out.push(Candidate { target, hook, pool: Vec::new() }),
            Err(e) => log::error!("core trait lookup for {}: {e:?}", im.target),
        }
    }
    out
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

/// One implementation for a `(trait, AbstractId)` pair: its target
/// (the abstract type at the parameters it covers; a declared type
/// variable is fresh, matching any), the hook and a pool of built
/// sites; a re-entrant dispatch uses a fresh site.
struct Candidate<R: Rt, E: UserEvent> {
    target: Type,
    hook: Hook,
    pool: Vec<HookSite<R, E>>,
}

/// The state for one `(trait, AbstractId)` pair, valid while the
/// trait's implementation list is the one it was resolved against.
struct SiteEntry<R: Rt, E: UserEvent> {
    version: Option<Arc<Vec<Arc<ImplDef>>>>,
    candidates: SmallVec<[Candidate<R, E>; 1]>,
}

impl<R: Rt, E: UserEvent> SiteEntry<R, E> {
    /// The candidate for `v`: the first whose target covers the type
    /// `v` was constructed at (`impl Eq for Box<i64>` is not consulted
    /// for a `Box<string>`).
    // XCR codex for eric: CR10 — done: a value carries the type arguments
    // it was constructed at (`GxAbstract::params`) and a candidate is
    // consulted only when its target covers them.
    fn choose(&mut self, env: &Env, v: &Value) -> Option<&mut Candidate<R, E>> {
        let typ = abstract_value::get(v)?.typ();
        self.candidates
            .iter_mut()
            .find(|c| c.target.could_match(env, &typ).unwrap_or(false))
    }
}

/// The per-context registry, keyed `(trait, tag)`. An entry resolves on
/// first use and is rebuilt when the trait's implementation list
/// changes (`impls_version`).
pub struct CoreHookSites<R: Rt, E: UserEvent>(
    ahash::AHashMap<(u8, AbstractId), SiteEntry<R, E>>,
);

impl<R: Rt, E: UserEvent> CoreHookSites<R, E> {
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<R: Rt, E: UserEvent> Default for CoreHookSites<R, E> {
    fn default() -> Self {
        Self(ahash::AHashMap::new())
    }
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for CoreHookSites<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CoreHookSites({})", self.0.len())
    }
}

fn build_site<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    t: CoreTrait,
    h: &Hook,
) -> Result<HookSite<R, E>> {
    let ftype = match ctx.env.by_id.get(&h.bind).map(|b| b.typ.clone()) {
        Some(Type::Fn(ft)) => ft,
        _ => return Err(anyhow!("core trait method {:?} is not a function", h.bind)),
    };
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

/// Run the implementation of `t` for tag `id` on `args`.
/// `None` = no implementation; `Some(None)` = it produced no value
/// this cycle (the bottom the callers' rules resolve); `Some(Some(v))`
/// = its result.
fn call_hook<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    t: CoreTrait,
    id: AbstractId,
    args: &[&Value],
) -> Option<Option<Value>> {
    let key = (t as u8, id);
    let version = impls_version(&ctx.env, t);
    let same = |a: &Option<Arc<_>>, b: &Option<Arc<_>>| match (a, b) {
        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
        (None, None) => true,
        _ => false,
    };
    let mut entry = match ctx.core_hook_sites.0.remove(&key) {
        Some(e) if same(&e.version, &version) => e,
        stale => {
            if let Some(mut e) = stale {
                for c in e.candidates.iter_mut() {
                    for mut s in c.pool.drain(..) {
                        s.site.delete(ctx);
                    }
                }
            }
            SiteEntry { version, candidates: candidates_for(&ctx.env, t, id) }
        }
    };
    let r = match entry.choose(&ctx.env, args[0]) {
        None => None,
        Some(c) => {
            let site = match c.pool.pop() {
                Some(s) => Ok(s),
                None => build_site(ctx, t, &c.hook),
            };
            match site {
                Err(e) => {
                    log::error!("core trait site for {}: {e:?}", c.hook.typ);
                    entry.candidates.clear();
                    None
                }
                Ok(mut s) => {
                    // every dispatch is a fresh invocation
                    s.site.reset_replay(ctx);
                    for (id, v) in s.args.iter().zip(args.iter()) {
                        ctx.rt.store_insert(*id, TagValue::fired((*v).clone()));
                        event.variables.insert(*id, TagValue::fired((*v).clone()));
                    }
                    let saved = event.init;
                    if s.first {
                        s.first = false;
                        event.init = true;
                    }
                    let tv = s.site.update(ctx, event);
                    let r =
                        if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) };
                    event.init = saved;
                    c.pool.push(s);
                    Some(r)
                }
            }
        }
    };
    ctx.core_hook_sites.0.insert(key, entry);
    r
}

struct HookState<R: Rt, E: UserEvent> {
    ctx: *mut ExecCtx<R, E>,
    event: *mut Event<E>,
}

/// Re-wrap a `GxAbstract` (received by reference inside the vtable
/// call) as the `Value` a hook site's argument binding carries.
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
    event: &mut Event<E>,
    t: CoreTrait,
    k: &Value,
) -> bool {
    let id = match abstract_value::get(k) {
        Some(g) => g.id,
        None => return false,
    };
    matches!(call_hook(ctx, event, t, id, &[k, k]), Some(None))
}

fn dispatch_eq<R: Rt, E: UserEvent>(
    state: *mut u8,
    a: &GxAbstract,
    b: &GxAbstract,
) -> Option<bool> {
    // SAFETY: `state` points into the live `with_value_hooks` frame.
    let s = unsafe { &mut *(state as *mut HookState<R, E>) };
    let (ctx, event) = unsafe { (&mut *s.ctx, &mut *s.event) };
    let (av, bv) = (as_value(a), as_value(b));
    match call_hook(ctx, event, CoreTrait::Eq, a.id, &[&av, &bv])? {
        Some(Value::Bool(x)) => Some(x),
        Some(v) => {
            log::warn!("core Eq for {} returned a non-bool {v:?}", a.name);
            Some(false)
        }
        // bottom keys are equal to each other and to nothing real
        None => {
            let ab = key_bottoms(ctx, event, CoreTrait::Eq, &av);
            let bb = key_bottoms(ctx, event, CoreTrait::Eq, &bv);
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
    let (ctx, event) = unsafe { (&mut *s.ctx, &mut *s.event) };
    let (av, bv) = (as_value(a), as_value(b));
    match call_hook(ctx, event, CoreTrait::Ord, a.id, &[&av, &bv])? {
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
            let ab = key_bottoms(ctx, event, CoreTrait::Ord, &av);
            let bb = key_bottoms(ctx, event, CoreTrait::Ord, &bv);
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
    let (ctx, event) = unsafe { (&mut *s.ctx, &mut *s.event) };
    let av = as_value(a);
    match call_hook(ctx, event, CoreTrait::Display, a.id, &[&av])? {
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

/// A map key comparison must honor a core `Ord` impl on the key type.
/// `f` sees no context, so the loan is exclusive.
pub fn with_key_ord_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    f: impl FnOnce() -> T,
) -> T {
    unsafe { with_value_hooks(ctx, event, |_, _| f()) }
}

/// Render under the value-hook loan: `f` prints through the `Env` it is
/// given. When no hook can fire that is the context's own; else it is a
/// snapshot (the maps are persistent, so a clone shares every node), so
/// a `Display` hook that builds or runs its site through the context
/// invalidates nothing the printer reads. `f` sees no context, so the
/// loan is exclusive.
// XCR codex for eric: CR25 — done for the printers: they read a snapshot
// under an exclusive loan (this function); the kernel run reads one too.
// `with_value_hooks` stays the unsafe seam for a caller that updates its
// children under the loan and holds nothing from the context across the
// operation.
pub fn with_display_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    f: impl FnOnce(&Env) -> T,
) -> T {
    if !hooks_live(&ctx.env) {
        return f(&ctx.env);
    }
    let env = ctx.env.clone();
    unsafe { with_value_hooks(ctx, event, |_, _| f(&env)) }
}

/// Loan `ctx`/`event` to the value seam for the duration of `f`. Call
/// it around any operation that compares or prints `Value`s and should
/// honor core-trait implementations. Loans nest. `f` receives the same
/// `ctx`/`event` back; the handle's raw pointers alias them and are
/// used only while `f`'s frame is suspended inside a `Value` operation.
///
/// SAFETY: a hook runs a Graphix call site over `ctx` and `event` from
/// inside a `Value` comparison or print, while `f` still holds its own
/// `&mut` to both, so `f` must not keep a reference derived from `ctx`
/// or `event` live across any `Value` operation: read what you need
/// before comparing or printing, and nothing after depends on a borrow
/// taken before.
pub unsafe fn with_value_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    f: impl FnOnce(&mut ExecCtx<R, E>, &mut Event<E>) -> T,
) -> T {
    if !hooks_live(&ctx.env) {
        return f(ctx, event);
    }
    let mut state = HookState::<R, E> { ctx: ctx as *mut _, event: event as *mut _ };
    let handle = ValueHookDispatch {
        state: &mut state as *mut HookState<R, E> as *mut u8,
        eq: dispatch_eq::<R, E>,
        cmp: dispatch_cmp::<R, E>,
        fmt: dispatch_fmt::<R, E>,
    };
    let _guard = abstract_value::arm_value_hooks(&handle as *const _);
    f(ctx, event)
}
