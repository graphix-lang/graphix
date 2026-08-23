//! The core traits `Eq`, `Ord` and `Display` (`design/traits.md` §8).
//!
//! Equality, ordering and printing are type-directed walks over a
//! `Value`. A type with an implementation of the corresponding core
//! trait is handled by that implementation wherever it occurs in the
//! static type; everything else is the structural case, which stays
//! the one Rust loop (`Value::eq`, `Value::partial_cmp`, the typed
//! printer `TVal`). A [`Plan`] is the static type annotated with the
//! hook positions; [`Hooks`] are the call sites a node owns, one per
//! hook, through which the walk invokes an implementation
//! synchronously inside the cycle.

use super::genn;
use crate::{
    BindId, Event, ExecCtx, Node, PrintFlag, Refs, Rt, Scope, TagValue, UserEvent,
    env::Env,
    expr::{ExprId, ModPath},
    format_with_flags,
    typ::{FnType, IsAFlags, TVal, TraitId, Type},
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{cmp::Ordering, fmt, sync::LazyLock};
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
pub struct Hook {
    pub bind: BindId,
    pub typ: Type,
}

#[derive(Debug)]
enum PlanNode {
    Structural,
    Hook(usize),
    Array(usize),
    Map(usize, usize),
    /// tuple elements / struct fields / variant arguments, in type order
    Fields(Box<[usize]>),
    Union(Box<[usize]>),
}

/// The static type of a site annotated with its hook positions. A node
/// is `hooked` iff a hook is reachable from it; the walk takes the
/// structural case for any node that isn't. `build` answers `None`
/// when the root isn't — the fast path, and the only case for a
/// program that implements no core trait.
#[derive(Debug)]
pub struct Plan {
    nodes: Vec<PlanNode>,
    hooked: Vec<bool>,
    root: usize,
    pub hooks: Vec<Hook>,
}

struct Builder<'a> {
    env: &'a Env,
    t: CoreTrait,
    nodes: Vec<PlanNode>,
    hooks: Vec<Hook>,
    memo: LPooled<ahash::AHashMap<compact_str::CompactString, usize>>,
}

impl<'a> Builder<'a> {
    fn push(&mut self, n: PlanNode) -> usize {
        self.nodes.push(n);
        self.nodes.len() - 1
    }

    fn hook_for(&mut self, typ: &Type) -> Result<Option<usize>> {
        let Some(im) = self.env.find_impl(self.t.id(), typ)? else { return Ok(None) };
        let def = self
            .env
            .trait_def(self.t.id())
            .ok_or_else(|| anyhow!("core trait {:?} is not defined", self.t))?;
        let m =
            def.methods.iter().find(|m| m.name == self.t.method()).ok_or_else(|| {
                anyhow!("core trait {} lacks {}", def.name, self.t.method())
            })?;
        let Some(bind) = im.methods.get(self.t.method()).copied().or(m.default) else {
            return Err(anyhow!("impl {} for {} has no {}", def.name, im.target, m.name));
        };
        self.hooks.push(Hook { bind, typ: typ.clone() });
        Ok(Some(self.hooks.len() - 1))
    }

    fn walk(&mut self, typ: &Type) -> Result<usize> {
        typ.with_deref(|t| match t {
            None => Ok(self.push(PlanNode::Structural)),
            Some(t) => self.walk_deref(t),
        })
    }

    fn walk_deref(&mut self, typ: &Type) -> Result<usize> {
        if let Some(h) = self.hook_for(typ)? {
            return Ok(self.push(PlanNode::Hook(h)));
        }
        match typ {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::Fn(_)
            | Type::Error(_)
            | Type::ByRef(_)
            | Type::Abstract { .. }
            | Type::TVar(_) => Ok(self.push(PlanNode::Structural)),
            Type::Ref(_) => {
                let key =
                    format_with_flags(PrintFlag::DerefTVars, || format_compact!("{typ}"));
                if let Some(i) = self.memo.get(&key) {
                    return Ok(*i);
                }
                let i = self.push(PlanNode::Structural);
                self.memo.insert(key, i);
                let expanded = typ.lookup_ref(self.env)?;
                let j = self.walk(&expanded)?;
                // the Ref node IS its expansion; cycles point back at `i`
                let n = std::mem::replace(&mut self.nodes[j], PlanNode::Structural);
                self.nodes[i] = n;
                self.alias(j, i);
                Ok(i)
            }
            Type::Array(et) => {
                let e = self.walk(et)?;
                Ok(self.push(PlanNode::Array(e)))
            }
            Type::Map { key, value } => {
                let k = self.walk(key)?;
                let v = self.walk(value)?;
                Ok(self.push(PlanNode::Map(k, v)))
            }
            Type::Tuple(ts) => {
                let f = ts.iter().map(|t| self.walk(t)).collect::<Result<Box<[_]>>>()?;
                Ok(self.push(PlanNode::Fields(f)))
            }
            Type::Struct(fs) => {
                let f =
                    fs.iter().map(|(_, t)| self.walk(t)).collect::<Result<Box<[_]>>>()?;
                Ok(self.push(PlanNode::Fields(f)))
            }
            Type::Variant(_, ts) => {
                let f = ts.iter().map(|t| self.walk(t)).collect::<Result<Box<[_]>>>()?;
                Ok(self.push(PlanNode::Fields(f)))
            }
            Type::Set(ts) => {
                let m = ts.iter().map(|t| self.walk(t)).collect::<Result<Box<[_]>>>()?;
                Ok(self.push(PlanNode::Union(m)))
            }
        }
    }

    /// Every edge to `from` now points at `to` (`from` was moved into `to`).
    fn alias(&mut self, from: usize, to: usize) {
        let fix = |i: &mut usize| {
            if *i == from {
                *i = to
            }
        };
        for n in self.nodes.iter_mut() {
            match n {
                PlanNode::Structural | PlanNode::Hook(_) => {}
                PlanNode::Array(e) => fix(e),
                PlanNode::Map(k, v) => {
                    fix(k);
                    fix(v)
                }
                PlanNode::Fields(f) | PlanNode::Union(f) => f.iter_mut().for_each(fix),
            }
        }
    }
}

impl Plan {
    /// `None` when no implementation of `t` is reachable from `typ`.
    pub fn build(env: &Env, t: CoreTrait, typ: &Type) -> Result<Option<Plan>> {
        if env.impls.get(&t.id()).is_none_or(|l| l.is_empty()) {
            return Ok(None);
        }
        let mut b = Builder {
            env,
            t,
            nodes: Vec::new(),
            hooks: Vec::new(),
            memo: LPooled::take(),
        };
        let root = b.walk(typ)?;
        let Builder { nodes, hooks, .. } = b;
        // a node is hooked iff a hook is reachable from it — a fixpoint,
        // since a recursive type's nodes form a cycle
        let mut hooked: Vec<bool> =
            nodes.iter().map(|n| matches!(n, PlanNode::Hook(_))).collect();
        loop {
            let mut changed = false;
            for (i, n) in nodes.iter().enumerate() {
                if hooked[i] {
                    continue;
                }
                let h = match n {
                    PlanNode::Structural | PlanNode::Hook(_) => false,
                    PlanNode::Array(e) => hooked[*e],
                    PlanNode::Map(k, v) => hooked[*k] || hooked[*v],
                    PlanNode::Fields(f) | PlanNode::Union(f) => {
                        f.iter().any(|i| hooked[*i])
                    }
                };
                if h {
                    hooked[i] = true;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        if !hooked[root] {
            return Ok(None);
        }
        Ok(Some(Plan { nodes, hooked, root, hooks }))
    }

    pub fn root(&self) -> usize {
        self.root
    }

    /// The hook at the root, if the whole type has an implementation.
    pub fn root_hook(&self) -> Option<&Hook> {
        match self.nodes[self.root] {
            PlanNode::Hook(h) => Some(&self.hooks[h]),
            _ => None,
        }
    }
}

/// The call sites a node owns for its plan's hooks. Each hook's site
/// calls the implementation's method binding directly with synthesized
/// argument bindings the walk writes before each call — the same
/// delivery a collection slot uses for its callback.
pub struct Hooks<R: Rt, E: UserEvent> {
    sites: Vec<HookSite<R, E>>,
}

struct HookSite<R: Rt, E: UserEvent> {
    site: Node<R, E>,
    args: SmallVec<[BindId; 2]>,
    first: bool,
}

impl<R: Rt, E: UserEvent> fmt::Debug for Hooks<R, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Hooks({})", self.sites.len())
    }
}

impl<R: Rt, E: UserEvent> Hooks<R, E> {
    pub fn build(
        ctx: &mut ExecCtx<R, E>,
        plan: &Plan,
        t: CoreTrait,
        scope: &Scope,
        site_id: u64,
        top_id: ExprId,
    ) -> Result<Self> {
        let mut sites = Vec::with_capacity(plan.hooks.len());
        for (i, h) in plan.hooks.iter().enumerate() {
            let ftype = match ctx.env.by_id.get(&h.bind).map(|b| b.typ.clone()) {
                Some(Type::Fn(ft)) => ft,
                _ => {
                    return Err(anyhow!(
                        "core trait method {:?} is not a function",
                        h.bind
                    ));
                }
            };
            let mut args: SmallVec<[BindId; 2]> = SmallVec::new();
            let mut nodes: SmallVec<[Node<R, E>; 2]> = SmallVec::new();
            for k in 0..t.arity() {
                let name = format_compact!("#hook{site_id}_{i}_{k}");
                let (id, n) =
                    genn::bind(ctx, &scope.lexical, &name, h.typ.clone(), top_id);
                args.push(id);
                nodes.push(n);
            }
            let fnode = genn::reference(ctx, h.bind, Type::Fn(ftype.clone()), top_id);
            let mut site = genn::apply(fnode, scope.clone(), nodes, &ftype, top_id);
            site.typecheck0(ctx)?;
            site.typecheck1(ctx)?;
            sites.push(HookSite { site, args, first: true });
        }
        Ok(Self { sites })
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node<R, E>> {
        self.sites.iter().map(|s| &s.site)
    }

    /// Deliver `args` and run the hook's call site. `None` is a bottom
    /// production — the implementation produced no value this cycle.
    fn call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        hook: usize,
        args: &[&Value],
    ) -> Option<Value> {
        let h = &mut self.sites[hook];
        for (id, v) in h.args.iter().zip(args.iter()) {
            ctx.rt.store_insert(*id, TagValue::fired((*v).clone()));
            event.variables.insert(*id, TagValue::fired((*v).clone()));
        }
        let saved = event.init;
        if h.first {
            h.first = false;
            event.init = true;
        }
        let tv = h.site.update(ctx, event);
        let r = if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) };
        event.init = saved;
        r
    }

    pub fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for s in self.sites.iter_mut() {
            s.site.delete(ctx);
            for id in s.args.iter() {
                ctx.env.unbind_variable(*id);
            }
        }
    }

    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for s in self.sites.iter_mut() {
            s.site.sleep(ctx)
        }
    }

    pub fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        for s in self.sites.iter_mut() {
            s.site.reset_replay(ctx)
        }
    }

    pub fn refs(&self, refs: &mut Refs) {
        for s in self.sites.iter() {
            s.site.refs(refs);
            for id in s.args.iter() {
                refs.bound.insert(*id);
            }
        }
    }
}

/// The member of a union `ts` that `v` belongs to — the typed printer's
/// rule: the first STRICT match, else the first structured plain
/// match, else the first plain match.
pub(crate) fn union_member(env: &Env, ts: &[Type], v: &Value) -> Option<usize> {
    let blind = |t: &Type| {
        t.with_deref(|t| matches!(t, None | Some(Type::Any) | Some(Type::Bottom)))
    };
    ts.iter()
        .position(|t| t.is_a_with(env, IsAFlags::Strict.into(), v))
        .or_else(|| ts.iter().position(|t| !blind(t) && t.is_a(env, v)))
        .or_else(|| ts.iter().position(|t| t.is_a(env, v)))
}

/// A walk's outcome: the structural answer, or bottom when a hook
/// produced nothing.
type Walk<T> = Option<T>;

/// The hooked equality walk: `Value::eq` everywhere no hook is
/// reachable, the implementation at every hook. Short-circuits on the
/// first inequality, as `Value::eq` does.
pub fn eq<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    plan: &Plan,
    hooks: &mut Hooks<R, E>,
    typ: &Type,
    l: &Value,
    r: &Value,
) -> Walk<bool> {
    eq_at(ctx, event, plan, hooks, plan.root(), typ, l, r)
}

fn eq_at<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    plan: &Plan,
    hooks: &mut Hooks<R, E>,
    node: usize,
    typ: &Type,
    l: &Value,
    r: &Value,
) -> Walk<bool> {
    if !plan.hooked[node] {
        return Some(l == r);
    }
    match &plan.nodes[node] {
        PlanNode::Structural => Some(l == r),
        PlanNode::Hook(h) => match hooks.call(ctx, event, *h, &[l, r])? {
            Value::Bool(b) => Some(b),
            _ => None,
        },
        PlanNode::Array(e) => match (l, r) {
            (Value::Array(la), Value::Array(ra)) => {
                if la.len() != ra.len() {
                    return Some(false);
                }
                let et = elem_type(typ);
                for (a, b) in la.iter().zip(ra.iter()) {
                    if !eq_at(ctx, event, plan, hooks, *e, &et, a, b)? {
                        return Some(false);
                    }
                }
                Some(true)
            }
            _ => Some(l == r),
        },
        PlanNode::Map(k, v) => match (l, r) {
            (Value::Map(lm), Value::Map(rm)) => {
                if lm.len() != rm.len() {
                    return Some(false);
                }
                let (kt, vt) = map_types(typ);
                for ((lk, lv), (rk, rv)) in lm.into_iter().zip(rm.into_iter()) {
                    if !eq_at(ctx, event, plan, hooks, *k, &kt, lk, rk)?
                        || !eq_at(ctx, event, plan, hooks, *v, &vt, lv, rv)?
                    {
                        return Some(false);
                    }
                }
                Some(true)
            }
            _ => Some(l == r),
        },
        PlanNode::Fields(f) => match (l, r) {
            (Value::Array(la), Value::Array(ra)) => {
                if la.len() != ra.len() {
                    return Some(false);
                }
                let fts = field_types(typ);
                let (lf, rf) = fields(typ, la, ra);
                if lf.len() != f.len() {
                    return Some(l == r);
                }
                // a struct's field names and a variant's tag compare
                // structurally — one static type, so they agree
                if !prefix_eq(typ, la, ra) {
                    return Some(false);
                }
                for (i, (a, b)) in lf.iter().zip(rf.iter()).enumerate() {
                    if !eq_at(ctx, event, plan, hooks, f[i], &fts[i], a, b)? {
                        return Some(false);
                    }
                }
                Some(true)
            }
            _ => Some(l == r),
        },
        PlanNode::Union(m) => {
            let Type::Set(ts) = typ else { return Some(l == r) };
            match (union_member(&ctx.env, ts, l), union_member(&ctx.env, ts, r)) {
                (Some(a), Some(b)) if a == b => {
                    let t = ts[a].clone();
                    eq_at(ctx, event, plan, hooks, m[a], &t, l, r)
                }
                _ => Some(l == r),
            }
        }
    }
}

/// The hooked ordering walk — `Value::partial_cmp`'s order (depth-first
/// lexicographic, length as the tiebreak, the type discriminant first
/// across shapes) with the implementation at every hook.
pub fn cmp<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    plan: &Plan,
    hooks: &mut Hooks<R, E>,
    typ: &Type,
    l: &Value,
    r: &Value,
) -> Walk<Ordering> {
    cmp_at(ctx, event, plan, hooks, plan.root(), typ, l, r)
}

fn structural_cmp(l: &Value, r: &Value) -> Ordering {
    l.partial_cmp(r).unwrap_or(Ordering::Equal)
}

fn cmp_at<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    plan: &Plan,
    hooks: &mut Hooks<R, E>,
    node: usize,
    typ: &Type,
    l: &Value,
    r: &Value,
) -> Walk<Ordering> {
    if !plan.hooked[node] {
        return Some(structural_cmp(l, r));
    }
    match &plan.nodes[node] {
        PlanNode::Structural => Some(structural_cmp(l, r)),
        PlanNode::Hook(h) => ordering_of(&hooks.call(ctx, event, *h, &[l, r])?),
        PlanNode::Array(e) => match (l, r) {
            (Value::Array(la), Value::Array(ra)) => {
                let et = elem_type(typ);
                for (a, b) in la.iter().zip(ra.iter()) {
                    match cmp_at(ctx, event, plan, hooks, *e, &et, a, b)? {
                        Ordering::Equal => {}
                        o => return Some(o),
                    }
                }
                Some(la.len().cmp(&ra.len()))
            }
            _ => Some(structural_cmp(l, r)),
        },
        PlanNode::Map(k, v) => match (l, r) {
            (Value::Map(lm), Value::Map(rm)) => {
                let (kt, vt) = map_types(typ);
                for ((lk, lv), (rk, rv)) in lm.into_iter().zip(rm.into_iter()) {
                    match cmp_at(ctx, event, plan, hooks, *k, &kt, lk, rk)? {
                        Ordering::Equal => {}
                        o => return Some(o),
                    }
                    match cmp_at(ctx, event, plan, hooks, *v, &vt, lv, rv)? {
                        Ordering::Equal => {}
                        o => return Some(o),
                    }
                }
                Some(lm.len().cmp(&rm.len()))
            }
            _ => Some(structural_cmp(l, r)),
        },
        PlanNode::Fields(f) => match (l, r) {
            (Value::Array(la), Value::Array(ra)) => {
                let fts = field_types(typ);
                let (lf, rf) = fields(typ, la, ra);
                if lf.len() != f.len() || rf.len() != f.len() {
                    return Some(structural_cmp(l, r));
                }
                match prefix_cmp(typ, la, ra) {
                    Ordering::Equal => {}
                    o => return Some(o),
                }
                for (i, (a, b)) in lf.iter().zip(rf.iter()).enumerate() {
                    match cmp_at(ctx, event, plan, hooks, f[i], &fts[i], a, b)? {
                        Ordering::Equal => {}
                        o => return Some(o),
                    }
                }
                Some(la.len().cmp(&ra.len()))
            }
            _ => Some(structural_cmp(l, r)),
        },
        PlanNode::Union(m) => {
            let Type::Set(ts) = typ else { return Some(structural_cmp(l, r)) };
            match (union_member(&ctx.env, ts, l), union_member(&ctx.env, ts, r)) {
                (Some(a), Some(b)) if a == b => {
                    let t = ts[a].clone();
                    cmp_at(ctx, event, plan, hooks, m[a], &t, l, r)
                }
                _ => Some(structural_cmp(l, r)),
            }
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

fn elem_type(typ: &Type) -> Type {
    match typ {
        Type::Array(et) => (**et).clone(),
        _ => Type::Any,
    }
}

fn map_types(typ: &Type) -> (Type, Type) {
    match typ {
        Type::Map { key, value } => ((**key).clone(), (**value).clone()),
        _ => (Type::Any, Type::Any),
    }
}

fn field_types(typ: &Type) -> LPooled<Vec<Type>> {
    match typ {
        Type::Tuple(ts) | Type::Variant(_, ts) => ts.iter().cloned().collect(),
        Type::Struct(fs) => fs.iter().map(|(_, t)| t.clone()).collect(),
        _ => LPooled::take(),
    }
}

/// The element values a `Fields` plan ranges over: a tuple's elements,
/// a struct's field VALUES (each `[name, value]` pair's second), a
/// variant's arguments (after the tag).
fn fields<'a>(
    typ: &Type,
    la: &'a [Value],
    ra: &'a [Value],
) -> (LPooled<Vec<&'a Value>>, LPooled<Vec<&'a Value>>) {
    let pick = |a: &'a [Value]| -> LPooled<Vec<&'a Value>> {
        match typ {
            Type::Struct(_) => a
                .iter()
                .filter_map(|p| match p {
                    Value::Array(p) if p.len() == 2 => Some(&p[1]),
                    _ => None,
                })
                .collect(),
            Type::Variant(_, _) => a.iter().skip(1).collect(),
            _ => a.iter().collect(),
        }
    };
    (pick(la), pick(ra))
}

/// The structurally-compared parts that precede each field in the
/// Value layout — a variant's tag, a struct's field names.
fn prefix_eq(typ: &Type, la: &[Value], ra: &[Value]) -> bool {
    match typ {
        Type::Variant(_, _) => la.first() == ra.first(),
        Type::Struct(_) => la.iter().zip(ra.iter()).all(|(a, b)| match (a, b) {
            (Value::Array(a), Value::Array(b)) => a.first() == b.first(),
            _ => false,
        }),
        _ => true,
    }
}

fn prefix_cmp(typ: &Type, la: &[Value], ra: &[Value]) -> Ordering {
    match typ {
        Type::Variant(_, _) => match (la.first(), ra.first()) {
            (Some(a), Some(b)) => structural_cmp(a, b),
            _ => Ordering::Equal,
        },
        Type::Struct(_) => {
            for (a, b) in la.iter().zip(ra.iter()) {
                if let (Value::Array(a), Value::Array(b)) = (a, b)
                    && let (Some(a), Some(b)) = (a.first(), b.first())
                {
                    match structural_cmp(a, b) {
                        Ordering::Equal => {}
                        o => return o,
                    }
                }
            }
            Ordering::Equal
        }
        _ => Ordering::Equal,
    }
}

/// The typed printer's hook interface: the environment for the
/// structural steps, and the implementation call at a hook.
pub trait FmtHooks {
    fn env(&self) -> &Env;
    /// `None` is a bottom production — the print bottoms.
    fn call(&mut self, hook: usize, v: &Value) -> Option<ArcStr>;
}

pub struct NoHooks<'a>(pub &'a Env);

impl FmtHooks for NoHooks<'_> {
    fn env(&self) -> &Env {
        self.0
    }

    fn call(&mut self, _: usize, _: &Value) -> Option<ArcStr> {
        None
    }
}

/// The runtime hook caller: the node's hook sites over the cycle's
/// context.
pub struct SiteHooks<'a, R: Rt, E: UserEvent> {
    pub ctx: &'a mut ExecCtx<R, E>,
    pub event: &'a mut Event<E>,
    pub hooks: &'a mut Hooks<R, E>,
}

impl<R: Rt, E: UserEvent> FmtHooks for SiteHooks<'_, R, E> {
    fn env(&self) -> &Env {
        &self.ctx.env
    }

    fn call(&mut self, hook: usize, v: &Value) -> Option<ArcStr> {
        match self.hooks.call(self.ctx, self.event, hook, &[v])? {
            Value::String(s) => Some(s),
            _ => None,
        }
    }
}

/// Print `v` at `typ` into `w` through the plan's hooks. `Err` is a
/// bottom: a hook produced nothing (or the writer failed).
pub fn fmt<R: Rt, E: UserEvent>(
    w: &mut dyn fmt::Write,
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    plan: &Plan,
    hooks: &mut Hooks<R, E>,
    typ: &Type,
    v: &Value,
) -> fmt::Result {
    let mut h = SiteHooks { ctx, event, hooks };
    TVal::fmt_planned(w, &mut h, Some((plan, plan.root())), typ, v)
}

impl Plan {
    /// The plan node for a step of the typed printer's walk — `None`
    /// once no hook is reachable (the rest of the subtree is
    /// structural).
    fn step(
        &self,
        node: usize,
        f: impl FnOnce(&PlanNode) -> Option<usize>,
    ) -> Option<(&Plan, usize)> {
        if !self.hooked[node] {
            return None;
        }
        let i = f(&self.nodes[node])?;
        if self.hooked[i] { Some((self, i)) } else { None }
    }

    pub(crate) fn is_hook(&self, node: usize) -> Option<usize> {
        match self.nodes[node] {
            PlanNode::Hook(h) if self.hooked[node] => Some(h),
            _ => None,
        }
    }

    pub(crate) fn array_elem(&self, node: usize) -> Option<(&Plan, usize)> {
        self.step(node, |n| match n {
            PlanNode::Array(e) => Some(*e),
            _ => None,
        })
    }

    pub(crate) fn map_key(&self, node: usize) -> Option<(&Plan, usize)> {
        self.step(node, |n| match n {
            PlanNode::Map(k, _) => Some(*k),
            _ => None,
        })
    }

    pub(crate) fn map_value(&self, node: usize) -> Option<(&Plan, usize)> {
        self.step(node, |n| match n {
            PlanNode::Map(_, v) => Some(*v),
            _ => None,
        })
    }

    pub(crate) fn field(&self, node: usize, i: usize) -> Option<(&Plan, usize)> {
        self.step(node, |n| match n {
            PlanNode::Fields(f) => f.get(i).copied(),
            _ => None,
        })
    }

    pub(crate) fn member(&self, node: usize, i: usize) -> Option<(&Plan, usize)> {
        self.step(node, |n| match n {
            PlanNode::Union(m) => m.get(i).copied(),
            _ => None,
        })
    }

    /// A type ref or bound tvar steps through the same node.
    pub(crate) fn same(&self, node: usize) -> Option<(&Plan, usize)> {
        if self.hooked[node] { Some((self, node)) } else { None }
    }
}

/// The method signature the core traits declare, for the `Impl` node's
/// prototype call sites.
pub fn method_ftype(env: &Env, bind: BindId) -> Option<Arc<FnType>> {
    match env.by_id.get(&bind).map(|b| &b.typ) {
        Some(Type::Fn(ft)) => Some(ft.clone()),
        _ => None,
    }
}
