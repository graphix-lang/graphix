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
    env::Env,
    expr::{ExprId, ModPath},
    typ::{AbstractId, FnType, IsAFlags, TraitId, Type},
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx_value::Value;
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

/// The implementation of `t` for `typ`, as a hook.
fn hook_for(env: &Env, t: CoreTrait, typ: &Type) -> Result<Option<Hook>> {
    let Some(im) = env.find_impl(t.id(), typ)? else { return Ok(None) };
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
    Ok(Some(Hook { bind, typ: typ.clone() }))
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

/// The state for one `(trait, AbstractId)` pair: `None` once the type
/// is known to have no implementation, else the hook and a pool of
/// built sites; a re-entrant dispatch uses a fresh site.
enum SiteEntry<R: Rt, E: UserEvent> {
    None,
    Impl { hook: Hook, pool: Vec<HookSite<R, E>> },
}

/// The per-context registry, keyed `(trait, tag)`. Entries resolve on
/// first use and stick: an implementation registered later is not
/// picked up.
pub struct CoreHookSites<R: Rt, E: UserEvent>(
    ahash::AHashMap<(u8, AbstractId), SiteEntry<R, E>>,
);

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
    let mut entry = match ctx.core_hook_sites.0.remove(&key) {
        Some(e) => e,
        None => {
            let typ = Type::Abstract { id, params: Arc::from_iter([]) };
            match hook_for(&ctx.env, t, &typ) {
                Ok(Some(hook)) => SiteEntry::Impl { hook, pool: Vec::new() },
                Ok(None) => SiteEntry::None,
                Err(e) => {
                    log::error!("core trait lookup for {typ}: {e:?}");
                    SiteEntry::None
                }
            }
        }
    };
    let r = match &mut entry {
        SiteEntry::None => None,
        SiteEntry::Impl { hook, pool } => {
            let site = match pool.pop() {
                Some(s) => Ok(s),
                None => build_site(ctx, t, hook),
            };
            match site {
                Err(e) => {
                    log::error!("core trait site for {}: {e:?}", hook.typ);
                    entry = SiteEntry::None;
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
                    match &mut entry {
                        SiteEntry::Impl { pool, .. } => pool.push(s),
                        SiteEntry::None => unreachable!(),
                    }
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
    abstract_value::wrap(g.id, g.name.clone(), g.payload.clone())
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

/// A map key comparison must honor a core `Ord` impl on the key type.
pub fn with_key_ord_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    f: impl FnOnce() -> T,
) -> T {
    with_value_hooks(ctx, event, |_, _| f())
}

/// Loan `ctx`/`event` to the value seam for the duration of `f`. Call
/// it around any operation that compares or prints `Value`s and should
/// honor core-trait implementations. Loans nest. `f` receives the same
/// `ctx`/`event` back; the handle's raw pointers alias them and are
/// used only while `f`'s frame is suspended inside a `Value` operation.
pub fn with_value_hooks<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    f: impl FnOnce(&mut ExecCtx<R, E>, &mut Event<E>) -> T,
) -> T {
    let live = [CoreTrait::Eq, CoreTrait::Ord, CoreTrait::Display]
        .into_iter()
        .any(|t| ctx.env.impls.get(&t.id()).is_some_and(|l| !l.is_empty()));
    if !live {
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
