use super::{
    Held, WakeBit,
    compiler::compile,
    pattern::{SliceKind, StructPatternNode},
};
use crate::image::nodes::{NodeTag, decode_node, put_tag, tag_len};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent,
    expr::{Expr, ExprId, ExprKind, Pattern},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    node::pattern::PatternNode,
    typ::Type,
    wrap,
};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::BytesMut;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::Typ;
use netidx_value::Value;
use poolshark::local::LPooled;
use std::sync::atomic::Ordering;

atomic_id!(SelectId);

/// The selected arm index (`usize::MAX` = none), writable through
/// `&self`. Semantic state: survives sleep and `reset_replay`, and
/// anything that rebuilds a node tree must preserve it.
#[derive(Debug)]
pub(crate) struct SelCell(std::sync::atomic::AtomicUsize);

impl SelCell {
    fn new() -> Self {
        Self(std::sync::atomic::AtomicUsize::new(usize::MAX))
    }

    pub(crate) fn get(&self) -> Option<usize> {
        match self.0.load(Ordering::Relaxed) {
            usize::MAX => None,
            i => Some(i),
        }
    }

    pub(crate) fn set(&self, v: Option<usize>) {
        self.0.store(v.unwrap_or(usize::MAX), Ordering::Relaxed)
    }
}

#[derive(Debug)]
pub struct Select<R: Rt, E: UserEvent> {
    pub(crate) selected: SelCell,
    pub arg: Held<R, E>,
    pub arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
    pub typ: Type,
    pub(crate) spec: Expr,
    /// In a frame, a tail re-selection rides the arm's tag instead of
    /// firing.
    pub(crate) tail_dispatch_select: std::sync::atomic::AtomicBool,
    /// Bit i = arm i's guard was consulted by the last re-match (arms
    /// >= 64 are always consulted). Quiet cycles read it so a standing
    /// bottom on a consulted guard keeps the select bottom.
    consulted_guard_mask: u64,
    resident: TagValue,
    /// Set by `sleep()`; the next update re-matches against the present
    /// scrutinee, which may have moved while no reader was awake.
    slept: WakeBit,
    arm_facts: Option<LazyArmFacts>,
}

/// Facts about the arms that need the sealed scrutinee type or the
/// analysis facts, so they are built on the first update.
#[derive(Debug)]
struct LazyArmFacts {
    tracked: TrackedFires,
    /// Per arm: sleep on deselect? False only for a pure non-recursive
    /// body, which is neither slept nor updated while untaken.
    sleep_on_deselect: Vec<bool>,
}

impl LazyArmFacts {
    fn build<R: Rt, E: UserEvent>(
        ctx: &ExecCtx<R, E>,
        scrut: &Type,
        arms: &mut [(PatternNode<R, E>, Node<R, E>)],
    ) -> Self {
        for (pat, _) in arms.iter_mut() {
            pat.seal_shallow(&ctx.env, scrut);
        }
        LazyArmFacts {
            tracked: TrackedFires::init(&ctx.env, arms),
            sleep_on_deselect: arms
                .iter()
                .map(|(_, n)| crate::analysis::arm_sleeps_on_deselect(ctx, n))
                .collect(),
        }
    }
}

/// What a consulted-guard mask says about this cycle's emission.
struct EmissionPlanes {
    /// A non-bottom fire was consumed.
    sound: bool,
    /// Any consumed input fired, bottom or not.
    anyfire: bool,
    /// A consulted guard's current channel is bottom.
    consulted_bottom: bool,
}

fn emission_planes(
    guard_tags: &[Option<Tag>],
    arg_prod: Tag,
    bottomed: bool,
    mask: u64,
) -> EmissionPlanes {
    let mut sound = !bottomed && arg_prod.triggers();
    let mut anyfire = arg_prod.triggers();
    let mut consulted_bottom = false;
    for (i, t) in guard_tags.iter().enumerate() {
        if i < 64 && mask & (1 << i) == 0 {
            continue;
        }
        if let Some(t) = t {
            if t.triggers() {
                anyfire = true;
                if !t.is_bottom() {
                    sound = true;
                }
            }
            if t.is_bottom() {
                consulted_bottom = true;
            }
        }
    }
    EmissionPlanes { sound, anyfire, consulted_bottom }
}

/// Evaluate the taken arm, consuming its tracked fire bits with the
/// catch-up deliveries scoped to it.
fn evaluate_arm<R: Rt, E: UserEvent>(
    tracked: &mut TrackedFires,
    arm: &mut Node<R, E>,
    i: usize,
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
) -> (Tag, Option<Value>) {
    let injected = tracked.deliver(ctx, event, i);
    let tv = arm.update(ctx, event);
    let t = tv.tag();
    let v = if t.is_bottom() { None } else { Some(tv.value_cloned()) };
    TrackedFires::restore(event, injected);
    (t, v)
}

impl<R: Rt, E: UserEvent> Select<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let arg = Held::image_decode(ctx, buf)?;
        let n = decode_varint(buf)? as usize;
        let mut arms = Vec::with_capacity(n);
        for _ in 0..n {
            let pat = PatternNode::image_decode(ctx, buf)?;
            let body = decode_node(ctx, buf)?;
            arms.push((pat, body));
        }
        let typ = Type::decode(buf)?;
        let spec = Expr::decode(buf)?;
        let tail_dispatch_select = bool::decode(buf)?;
        Ok(Node::new(Self {
            selected: SelCell::new(),
            arg,
            arms,
            typ,
            spec,
            tail_dispatch_select: std::sync::atomic::AtomicBool::new(
                tail_dispatch_select,
            ),
            consulted_guard_mask: 0,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            arm_facts: None,
        }))
    }

    /// Build a `Select` node from an already-compiled scrutinee
    /// expression and a vector of (pattern, arm body) pairs.
    #[allow(dead_code)]
    pub fn new(
        arg: Node<R, E>,
        arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
        typ: Type,
        spec: Expr,
    ) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            arg: Held::new(arg),
            arms,
            selected: SelCell::new(),
            tail_dispatch_select: std::sync::atomic::AtomicBool::new(false),
            consulted_guard_mask: 0,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            arm_facts: None,
        })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        arg: &Expr,
        arms: &[(Pattern, Expr)],
    ) -> Result<Node<R, E>> {
        let arg = Held::new(compile(ctx, flags, arg.clone(), scope, top_id)?);
        let arms = arms
            .iter()
            .map(|(pat, spec)| {
                let scope = scope.append_block("sel", SelectId::new().0);
                let pat = PatternNode::compile(
                    ctx,
                    flags,
                    pat,
                    &scope,
                    top_id,
                    spec.pos,
                    spec.ori.clone(),
                )
                .with_context(|| format!("in select at {}", spec.pos))?;
                pat.structure_predicate.ids(&mut |id| ctx.env.mark_pattern_bind(id));
                let n = compile(ctx, flags, spec.clone(), &scope, top_id)?;
                Ok((pat, n))
            })
            .collect::<Result<Vec<_>>>()
            .with_context(|| format!("in select at {}", spec.pos))?;
        let typ = Type::empty_tvar();
        Ok(Node::new(Self {
            spec,
            typ,
            arg,
            arms,
            selected: SelCell::new(),
            tail_dispatch_select: std::sync::atomic::AtomicBool::new(false),
            consulted_guard_mask: 0,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            arm_facts: None,
        }))
    }
}

/// Collect every array/list member of `t` (through refs, bound tvars
/// and set nesting). `depth` caps ref chains; hitting the cap stops
/// collecting, which can only make exhaustiveness stricter.
fn array_members(
    env: &crate::env::Env,
    t: &Type,
    out: &mut smallvec::SmallVec<[Type; 4]>,
    depth: usize,
) -> Result<()> {
    if depth > 64 {
        return Ok(());
    }
    match t {
        Type::Array(_) | Type::List(_) => out.push(t.clone()),
        Type::Set(ts) => {
            for t in ts.iter() {
                array_members(env, t, out, depth + 1)?
            }
        }
        Type::Ref(_) => {
            let t = t.lookup_ref(env)?;
            array_members(env, &t, out, depth + 1)?
        }
        Type::TVar(_) => {
            if let Some(t) = t.deref_cloned() {
                array_members(env, &t, out, depth + 1)?
            }
        }
        _ => (),
    }
    Ok(())
}

/// Do lengths `exacts ∪ [rest, ∞)` cover every array length? Complete
/// coverage needs a rest bound with every length below it exact.
fn lens_complete(exacts: &[usize], rest: Option<usize>) -> bool {
    match rest {
        None => false,
        Some(r) => (0..r).all(|n| exacts.contains(&n)),
    }
}

/// Is the length range `== k` (`exact`) / `>= k` (rest form) entirely
/// inside `exacts ∪ [rest, ∞)`?
fn range_covered(k: usize, exact: bool, exacts: &[usize], rest: Option<usize>) -> bool {
    let covered = |n: usize| rest.is_some_and(|r| n >= r) || exacts.contains(&n);
    if exact {
        covered(k)
    } else {
        match rest {
            None => false,
            Some(r) => (k..r.max(k)).all(covered),
        }
    }
}

/// Sleep an arm the select is deselecting. Under `deselecting_arm` a
/// recursive-edge callee inside the arm is deleted, not retained
/// (`CallSite::sleep`), so unreached activations are shed.
fn deselect_sleep<R: Rt, E: UserEvent>(arm: &mut Node<R, E>, ctx: &mut ExecCtx<R, E>) {
    let saved = ctx.deselecting_arm;
    ctx.deselecting_arm = true;
    arm.sleep(ctx);
    ctx.deselecting_arm = saved;
}

/// Wake-catch-up fire tracking (design/wake_catchup.md): one fire bit
/// per arm-body input, set when the input fires and consumed by the
/// arm evaluation that reads it, so a woken arm receives exactly the
/// fires no selected reader saw, once, at the current standing value.
/// Guards, the scrutinee and pattern binds (of any enclosing select)
/// are not tracked. Survives sleep and `reset_replay`; frames excluded.
#[derive(Debug, Default)]
struct TrackedFires {
    /// Per arm: the body's free refs, keyed by the input they are
    /// tracked under (a destructuring `let`'s siblings share their
    /// group's representative, `Env::facet_of`). Refreshed at each
    /// deselect, when the arm's subtree is fully materialized.
    per_arm: Vec<nohash::IntMap<BindId, smallvec::SmallVec<[BindId; 2]>>>,
    /// The union of `per_arm`'s keys.
    all: nohash::IntSet<BindId>,
    /// Sound fires no arm evaluation has consumed yet.
    pending: nohash::IntSet<BindId>,
}

impl TrackedFires {
    fn arm_refs<R: Rt, E: UserEvent>(
        env: &crate::env::Env,
        pat: &PatternNode<R, E>,
        arm: &Node<R, E>,
    ) -> nohash::IntMap<BindId, smallvec::SmallVec<[BindId; 2]>> {
        let mut r = Refs::default();
        arm.refs(&mut r);
        pat.structure_predicate.ids(&mut |id| {
            r.bound.insert(id);
        });
        let mut out: nohash::IntMap<BindId, smallvec::SmallVec<[BindId; 2]>> =
            nohash::IntMap::default();
        for id in r.refed.difference(&r.bound).copied() {
            if !env.is_pattern_bind(id) {
                out.entry(env.facet_of(id)).or_default().push(id);
            }
        }
        out
    }

    fn init<R: Rt, E: UserEvent>(
        env: &crate::env::Env,
        arms: &[(PatternNode<R, E>, Node<R, E>)],
    ) -> Self {
        let per_arm: Vec<_> =
            arms.iter().map(|(pat, n)| Self::arm_refs(env, pat, n)).collect();
        let all = per_arm.iter().flat_map(|m| m.keys().copied()).collect();
        TrackedFires { per_arm, all, pending: nohash::IntSet::default() }
    }

    fn refresh_arm<R: Rt, E: UserEvent>(
        &mut self,
        env: &crate::env::Env,
        i: usize,
        pat: &PatternNode<R, E>,
        arm: &Node<R, E>,
    ) {
        self.per_arm[i] = Self::arm_refs(env, pat, arm);
        self.all = self.per_arm.iter().flat_map(|m| m.keys().copied()).collect();
        self.pending.retain(|id| self.all.contains(id));
    }

    /// Record this cycle's sound fires of tracked inputs. Runs before
    /// routing, so the taken arm consumes same-cycle fires immediately
    /// and no-arm cycles accumulate them for a future waker.
    fn observe<R: Rt, E: UserEvent>(&mut self, ctx: &ExecCtx<R, E>, event: &Event<E>) {
        if ctx.frame_depth > 0 {
            return;
        }
        let mut newly: smallvec::SmallVec<[BindId; 8]> = smallvec::SmallVec::new();
        for id in self.all.iter() {
            if self.pending.contains(id) {
                continue;
            }
            if let Some(super::VarRead::Delivered(tv)) = super::read_var(ctx, event, id) {
                let t = tv.tag();
                if t.is_fired() && !t.is_bottom() {
                    newly.push(*id);
                }
            }
        }
        for id in newly {
            self.pending.insert(id);
        }
    }

    /// Consume the bits arm `i` reads, injecting one catch-up FIRED
    /// delivery at the current standing value for each input not
    /// delivered live this cycle. Returns the injected entries for
    /// [`Self::restore`]; a bottomed or vanished input spends its bit
    /// and injects nothing.
    fn deliver<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<R, E>,
        event: &mut Event<E>,
        i: usize,
    ) -> smallvec::SmallVec<[(BindId, Option<TagValue>); 4]> {
        let mut injected: smallvec::SmallVec<[(BindId, Option<TagValue>); 4]> =
            smallvec::SmallVec::new();
        if ctx.frame_depth > 0 || self.pending.is_empty() {
            return injected;
        }
        let Some(set) = self.per_arm.get(i) else { return injected };
        let keys: smallvec::SmallVec<[BindId; 8]> =
            self.pending.iter().filter(|id| set.contains_key(id)).copied().collect();
        for key in keys {
            self.pending.remove(&key);
            for id in set[&key].iter().copied() {
                let standing = match super::read_var(ctx, event, &id) {
                    Some(super::VarRead::Delivered(_)) => None,
                    Some(super::VarRead::Standing(tv)) if !tv.tag().is_bottom() => {
                        Some(tv.value_cloned())
                    }
                    _ => None,
                };
                if let Some(v) = standing {
                    let prev = event.variables.insert(id, TagValue::fired(v));
                    injected.push((id, prev));
                }
            }
        }
        injected
    }

    fn restore<E: UserEvent>(
        event: &mut Event<E>,
        injected: smallvec::SmallVec<[(BindId, Option<TagValue>); 4]>,
    ) {
        for (id, prev) in injected {
            match prev {
                Some(tv) => {
                    event.variables.insert(id, tv);
                }
                None => {
                    event.variables.remove(&id);
                }
            }
        }
    }
}

/// The bool-literal leaves of a composite pattern in position order —
/// `Some(b)` for a literal, `None` for a bind or `_` — or `None` when the
/// pattern is not composite or has any other refutable leaf (another
/// literal, a slice, a type test): such an atom pools no coverage.
fn composite_literal_vector(
    sp: &StructPatternNode,
) -> Option<smallvec::SmallVec<[Option<bool>; 8]>> {
    fn leaves(
        sp: &StructPatternNode,
        out: &mut smallvec::SmallVec<[Option<bool>; 8]>,
    ) -> bool {
        match sp {
            StructPatternNode::Bind(_) | StructPatternNode::Ignore => {
                out.push(None);
                true
            }
            StructPatternNode::Literal(Value::Bool(b)) => {
                out.push(Some(*b));
                true
            }
            StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds } => {
                binds.iter().all(|p| leaves(p, out))
            }
            StructPatternNode::Struct { all: _, binds } => {
                let mut order: smallvec::SmallVec<
                    [&(ArcStr, usize, StructPatternNode); 8],
                > = binds.iter().collect();
                order.sort_by_key(|(_, i, _)| *i);
                order.iter().all(|(_, _, p)| leaves(p, out))
            }
            StructPatternNode::Variant { tag: _, all: _, binds } => {
                binds.iter().all(|p| leaves(p, out))
            }
            _ => false,
        }
    }
    match sp {
        StructPatternNode::Slice { kind: SliceKind::Tuple, .. }
        | StructPatternNode::Struct { .. }
        | StructPatternNode::Variant { .. } => {
            let mut out = smallvec::SmallVec::new();
            if leaves(sp, &mut out) && out.iter().any(|l| l.is_some()) {
                Some(out)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// The head a composite pattern tests: what groups arms in the
/// literal pool and selects the scrutinee member they cover.
#[derive(PartialEq, Eq)]
enum Shape {
    Tuple(usize),
    Variant(ArcStr, usize),
    Struct(smallvec::SmallVec<[ArcStr; 8]>),
}

fn shape_of(sp: &StructPatternNode) -> Option<Shape> {
    match sp {
        StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds } => {
            Some(Shape::Tuple(binds.len()))
        }
        StructPatternNode::Variant { tag, all: _, binds } => {
            Some(Shape::Variant(tag.clone(), binds.len()))
        }
        StructPatternNode::Struct { all: _, binds } => {
            let mut names: smallvec::SmallVec<[ArcStr; 8]> =
                binds.iter().map(|(n, _, _)| n.clone()).collect();
            names.sort();
            Some(Shape::Struct(names))
        }
        _ => None,
    }
}

/// The scrutinee's member of this shape — the type a complete literal
/// ladder covers — through references and unions.
fn scrutinee_member(
    env: &crate::env::Env,
    scrut: &Type,
    shape: &Shape,
) -> Result<Option<Type>> {
    fn walk(
        env: &crate::env::Env,
        t: &Type,
        shape: &Shape,
        depth: usize,
    ) -> Result<Option<Type>> {
        if depth > 8 {
            return Ok(None);
        }
        let hit = t.with_deref(|t| match t {
            Some(Type::Tuple(ts)) => {
                Ok(matches!(shape, Shape::Tuple(n) if *n == ts.len())
                    .then(|| Type::Tuple(ts.clone())))
            }
            Some(Type::Variant(tag, ts)) => Ok(matches!(
                shape,
                Shape::Variant(st, n) if st == tag && *n == ts.len()
            )
            .then(|| Type::Variant(tag.clone(), ts.clone()))),
            Some(Type::Struct(fs)) => {
                let same = match shape {
                    Shape::Struct(names) => {
                        names.len() == fs.len()
                            && names.iter().zip(fs.iter()).all(|(n, (f, _))| n == f)
                    }
                    _ => false,
                };
                Ok(same.then(|| Type::Struct(fs.clone())))
            }
            Some(Type::Set(ms)) => {
                for m in ms.iter() {
                    if let Some(hit) = walk(env, m, shape, depth + 1)? {
                        return Ok(Some(hit));
                    }
                }
                Ok(None)
            }
            Some(r @ Type::Ref(_)) => walk(env, &r.lookup_ref(env)?, shape, depth + 1),
            _ => Ok(None),
        })?;
        Ok(hit)
    }
    walk(env, scrut, shape, 0)
}

/// Composite arms whose only refutable leaves are bool literals pool
/// coverage per position: same-shaped arms cover the scrutinee's member
/// of that shape once their literal vectors cover every assignment of
/// the tested positions (`(true, true) | (true, false) | (false, _)`
/// covers `(bool, bool)`).
#[derive(Default)]
struct LiteralPool {
    groups: Vec<(Shape, Vec<smallvec::SmallVec<[Option<bool>; 8]>>, bool)>,
}

impl LiteralPool {
    /// True when this arm completes its shape's coverage.
    fn insert(
        &mut self,
        shape: Shape,
        vec: smallvec::SmallVec<[Option<bool>; 8]>,
    ) -> bool {
        let idx = match self.groups.iter().position(|(g, _, _)| *g == shape) {
            Some(i) => i,
            None => {
                self.groups.push((shape, Vec::new(), false));
                self.groups.len() - 1
            }
        };
        let (_, vecs, done) = &mut self.groups[idx];
        if *done || vecs.iter().any(|v| v.len() != vec.len()) {
            return false;
        }
        vecs.push(vec);
        let positions: smallvec::SmallVec<[usize; 8]> =
            (0..vecs[0].len()).filter(|&i| vecs.iter().any(|v| v[i].is_some())).collect();
        if positions.len() > 10 {
            return false;
        }
        let complete = (0..1usize << positions.len()).all(|assign| {
            vecs.iter().any(|v| {
                positions.iter().enumerate().all(|(bit, &pos)| {
                    v[pos].is_none_or(|b| b == ((assign >> bit) & 1 == 1))
                })
            })
        });
        *done = complete;
        complete
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Select<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.arg.image_len()
            + varint_len(self.arms.len() as u64)
            + self.arms.iter().map(|(p, n)| p.image_len() + n.image_len()).sum::<usize>()
            + self.typ.encoded_len()
            + self.spec.encoded_len()
            + 1
    }

    fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
        put_tag(NodeTag::Select, buf);
        self.arg.image_encode(buf)?;
        encode_varint(self.arms.len() as u64, buf);
        for (p, n) in &self.arms {
            p.image_encode(buf)?;
            n.image_encode(buf)?;
        }
        self.typ.encode(buf)?;
        self.spec.encode(buf)?;
        self.tail_dispatch_select.load(Ordering::Relaxed).encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.arm_facts.is_none() {
            let scrut = self.arg.node.typ().clone();
            self.arm_facts = Some(LazyArmFacts::build(ctx, &scrut, &mut self.arms));
        }
        let Self {
            selected,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_dispatch_select,
            consulted_guard_mask,
            resident,
            slept,
            arm_facts,
        } = self;
        let LazyArmFacts { tracked, sleep_on_deselect } =
            arm_facts.as_mut().expect("arm facts built above");
        let sleep_on_deselect: &[bool] = sleep_on_deselect;
        let woke = slept.take() && ctx.frame_depth == 0;
        // Per-arm guard production tags; `None` = unguarded. Only guards
        // the chain consults contribute fires or bottomness.
        let mut guard_tags: smallvec::SmallVec<[Option<Tag>; 8]> =
            smallvec::SmallVec::with_capacity(arms.len());
        let arg_prod = arg.update(ctx, event);
        tracked.observe(ctx, event);
        let bottomed = arg.tag.is_bottom();
        let arg_up = !bottomed;
        // Arm binds carry the scrutinee's production tag; firing comes
        // from the selection/emission rules, never from the binds.
        let bind_tag = arg_prod;
        macro_rules! bind {
            ($i:expr) => {
                bind!($i, bind_tag)
            };
            ($i:expr, $tag:expr) => {{
                if let Some(arg) = arg.value.as_ref() {
                    arms[$i].0.bind_event(ctx, event, arg, $tag);
                }
            }};
        }
        // Guards are live nodes and tick every cycle, even under a
        // tainted scrutinee; only the bind is gated on a value view.
        for (pat, _) in arms.iter_mut() {
            let bind_guard = arg_up && pat.guard.is_some();
            if bind_guard {
                if let Some(arg) = arg.value.as_ref() {
                    pat.bind_event(ctx, event, arg, bind_tag);
                }
            }
            guard_tags.push(pat.update(ctx, event));
            if bind_guard {
                pat.unbind_event(event);
            }
        }
        // Any guard fire drives a re-match; whether it affects the
        // emission is decided by the consulted set below.
        let pat_up = guard_tags.iter().any(|t| t.is_some_and(|t| t.triggers()));
        // A bottom scrutinee bottoms the select; it consults no guards.
        if bottomed {
            return if arg_prod.triggers() {
                resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                resident.ride()
            };
        }
        if crate::dbgenv::graphix_dbg_select() {
            eprintln!(
                "SELECT[{}] upd init={} fd={} arg_up={arg_up} pat_up={pat_up} sel={:?} argc={:?} vars={}",
                self.spec.pos,
                event.init,
                ctx.frame_depth,
                selected.get(),
                arg.value.as_ref(),
                event.variables.len()
            );
        }
        let tail = tail_dispatch_select.load(Ordering::Relaxed) && ctx.frame_depth > 0;
        // Inside frames selection is value-driven: a jump-rebound loop
        // variable arrives STALE, so a triggers-only driver would spin.
        let arg_trig = arg_prod.triggers() || (ctx.frame_depth > 0 && arg_up);
        enum ChainOut {
            Quiet,
            Taken(Option<usize>),
            Undet,
        }
        let route_unselected_present = ctx.frame_depth == 0
            && selected.get().is_none()
            && arg.value.is_some()
            && !arg.tag.is_bottom();
        let chain = if !arg_trig && !pat_up && !route_unselected_present && !woke {
            ChainOut::Quiet
        } else {
            match arg.value.as_ref() {
                None => ChainOut::Taken(None),
                Some(v) => {
                    let mut mask = 0u64;
                    let mut out = ChainOut::Taken(None);
                    for (i, (pat, _)) in arms.iter().enumerate() {
                        use super::pattern::ArmMatch;
                        match pat.arm_match(&ctx.env, v) {
                            ArmMatch::NoStruct => (),
                            ArmMatch::GuardFalse => {
                                if i < 64 {
                                    mask |= 1 << i;
                                }
                            }
                            // Undecidable: the chain stops and the
                            // selection holds.
                            ArmMatch::GuardBottom => {
                                if i < 64 {
                                    mask |= 1 << i;
                                }
                                out = ChainOut::Undet;
                                break;
                            }
                            ArmMatch::Matched => {
                                if pat.guard.is_some() && i < 64 {
                                    mask |= 1 << i;
                                }
                                out = ChainOut::Taken(Some(i));
                                break;
                            }
                        }
                    }
                    *consulted_guard_mask = mask;
                    out
                }
            }
        };
        let EmissionPlanes { sound: own_sound, anyfire: own_anyfire, consulted_bottom } =
            emission_planes(&guard_tags, arg_prod, bottomed, *consulted_guard_mask);
        // Sound only: a bottom must never upgrade a stale result to FIRED.
        if own_sound && tail_dispatch_select.load(Ordering::Relaxed) {
            ctx.tail_scrut_fired = true;
        }
        // A consulted-guard bottom makes the emission bottom whatever
        // else fired; otherwise a sound consumed fire emits the arm's
        // current value FIRED, and quiet rides.
        macro_rules! emit {
            ($t:expr, $v:expr) => {{
                let t: Tag = $t;
                if consulted_bottom {
                    if t.triggers() || own_anyfire {
                        Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    } else {
                        None
                    }
                } else if t.is_bottom() {
                    if t.triggers() || own_sound {
                        Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    } else {
                        None
                    }
                } else if t.is_fired() || own_sound {
                    $v.map(|v| TagValue::tagged(v, Tag::FIRED))
                } else {
                    $v.map(|v| TagValue::tagged(v, Tag::STALE))
                }
            }};
        }
        let out = match chain {
            ChainOut::Quiet => selected.get().and_then(|i| {
                let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx, event);
                emit!(t, v)
            }),
            ChainOut::Undet => {
                let t = if own_anyfire { Tag::FRESH_BOTTOM } else { Tag::STALE_BOTTOM };
                return resident.set(TagValue::tagged(Value::Null, t));
            }
            ChainOut::Taken(sel) => match (sel, selected.get()) {
                (Some(i), Some(j)) if i == j => {
                    if crate::dbgenv::graphix_dbg_select() {
                        eprintln!(
                            "SELECT[{}] same-arm i={i} arg={:?}",
                            self.spec.pos,
                            arg.value.as_ref()
                        );
                    }
                    if arg_up {
                        bind!(i);
                    }
                    let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx, event);
                    emit!(t, v)
                }
                (Some(i), Some(_) | None) => {
                    if crate::dbgenv::graphix_dbg_select() {
                        eprintln!(
                            "SELECT[{}] BECOMING-SELECTED {:?} -> {i} fd={} init={}",
                            self.spec.pos,
                            selected.get(),
                            ctx.frame_depth,
                            event.init
                        );
                    }
                    if let Some(j) = selected.get() {
                        if sleep_on_deselect[j] {
                            deselect_sleep(&mut arms[j].1, ctx);
                        }
                        tracked.refresh_arm(&ctx.env, j, &arms[j].0, &arms[j].1);
                    }
                    selected.set(Some(i));
                    // The wake bind is part of the arm's init view: a
                    // guard-flip wake binds FIRED so interior call sites
                    // dispatch; a stale first consult binds a past value.
                    let wake_tag = if tail {
                        bind_tag
                    } else if arg_prod.triggers() {
                        arg_prod
                    } else if route_unselected_present && !pat_up {
                        Tag::STALE
                    } else {
                        Tag::FIRED
                    };
                    bind!(i, wake_tag);
                    // A slept arm resumes under the wake view; a
                    // skip-sleep arm was never updated, so this is its
                    // birth and gets `init` only.
                    let (init, wake) = (event.init, event.wake_init);
                    event.init = true;
                    if sleep_on_deselect[i] {
                        event.wake_init = true;
                    }
                    let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx, event);
                    event.init = init;
                    event.wake_init = wake;
                    emit!(t, v)
                }
                (None, Some(j)) => {
                    if sleep_on_deselect[j] {
                        deselect_sleep(&mut arms[j].1, ctx);
                    }
                    tracked.refresh_arm(&ctx.env, j, &arms[j].0, &arms[j].1);
                    selected.set(None);
                    None
                }
                (None, None) => None,
            },
        };
        match out {
            Some(tv) => resident.set(tv),
            None => resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_dispatch_select: _,
            consulted_guard_mask: _,
            resident: _,
            slept: _,
            arm_facts: _,
        } = self;
        arg.node.delete(ctx);
        for (pat, arm) in arms {
            arm.delete(ctx);
            pat.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_dispatch_select: _,
            consulted_guard_mask: _,
            resident: _,
            slept,
            arm_facts: _,
        } = self;
        slept.set();
        arg.sleep(ctx);
        for (pat, arg) in arms {
            arg.sleep(ctx);
            if let Some(n) = &mut pat.guard {
                n.sleep(ctx)
            }
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The selection is semantic state, not a replay cache: a frame
        // pass that re-derives the same selection stays quiet.
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_dispatch_select: _,
            consulted_guard_mask: _,
            resident: _,
            slept: _,
            arm_facts: _,
        } = self;
        arg.reset_replay(ctx);
        for (pat, arg) in arms {
            arg.reset_replay(ctx);
            if let Some(n) = &mut pat.guard {
                n.reset_replay(ctx)
            }
        }
    }

    fn refs(&self, refs: &mut Refs) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            tail_dispatch_select: _,
            consulted_guard_mask: _,
            resident: _,
            slept: _,
            arm_facts: _,
        } = self;
        arg.node.refs(refs);
        for (pat, arm) in arms {
            arm.refs(refs);
            pat.structure_predicate.ids(&mut |id| {
                refs.bound.insert(id);
            });
            if let Some(n) = &pat.guard {
                n.node.refs(refs);
            }
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        // An arm's coverage atoms: each or-alternative paired with its
        // member of the arm's predicate (the inferred Set is one member
        // per alternative, in order); any other arm is one atom.
        fn arm_atoms<'a>(
            sp: &'a StructPatternNode,
            typ: &'a Type,
            out: &mut smallvec::SmallVec<[(&'a StructPatternNode, Type); 4]>,
        ) {
            match sp {
                StructPatternNode::Or { alts } => {
                    let ts = typ.with_deref(|t| match t {
                        Some(Type::Set(ts)) if ts.len() == alts.len() => Some(ts.clone()),
                        _ => None,
                    });
                    for (i, a) in alts.iter().enumerate() {
                        let t = match &ts {
                            Some(ts) => ts[i].clone(),
                            None => typ.clone(),
                        };
                        out.push((a, t));
                    }
                }
                _ => out.push((sp, typ.clone())),
            }
        }
        self.arg.node.typecheck0(ctx)?;
        // A partial struct pattern infers only its named fields;
        // complete each inferred predicate against the typed scrutinee
        // before coverage or dispatch reads it.
        if let ExprKind::Select(se) = &self.spec.kind {
            let scrut = self.arg.node.typ().clone();
            for ((pat, _), (spec_pat, _)) in self.arms.iter_mut().zip(se.arms.iter()) {
                if !pat.explicit_type_predicate {
                    if let Some(t) = spec_pat
                        .structure_predicate
                        .complete_type_predicate(&ctx.env, &pat.type_predicate, &scrut)?
                    {
                        pat.structure_predicate.realign(&ctx.env, &t)?;
                        pat.type_predicate = t;
                    }
                }
            }
        }
        let mut rtypes: LPooled<Vec<&Type>> = LPooled::take();
        let mut mtypes: LPooled<Vec<Type>> = LPooled::take();
        let mut itypes: LPooled<Vec<&Type>> = LPooled::take();
        let mut saw_true = false;
        let mut saw_false = false;
        // An unguarded inferred-irrefutable arm makes the select
        // exhaustive; its fresh-tvar predicate must stay out of the
        // coverage unions, which `check_contains` would greedily bind.
        let mut wildcard = false;
        // `(k, exact, predicate)`: an unguarded slice arm matching every
        // array of length == k or >= k. If the pool's lengths cover the
        // naturals it covers each member every pool predicate contains.
        let mut slice_pool: smallvec::SmallVec<[(usize, bool, Type); 8]> =
            smallvec::SmallVec::new();
        let mut literal_pool = LiteralPool::default();
        let (mut guarded_slice, mut refutable_slice) = (false, false);
        for (pat, _) in self.arms.iter() {
            let inferred_irrefutable = !pat.explicit_type_predicate
                && pat.structure_predicate.matches_anything();
            match &pat.guard {
                Some(_) => {
                    guarded_slice |= pat.structure_predicate.is_array_slice();
                }
                None => {
                    if inferred_irrefutable {
                        wildcard = true;
                    } else {
                        let mut atoms: smallvec::SmallVec<
                            [(&StructPatternNode, Type); 4],
                        > = smallvec::SmallVec::new();
                        arm_atoms(
                            &pat.structure_predicate,
                            &pat.type_predicate,
                            &mut atoms,
                        );
                        for (sp, at) in atoms.iter() {
                            if !sp.is_refutable() {
                                mtypes.push(at.clone())
                            } else if let StructPatternNode::Literal(Value::Bool(b)) = sp
                            {
                                saw_true |= *b;
                                saw_false |= !*b;
                                if saw_true && saw_false {
                                    mtypes.push(Type::Primitive(Typ::Bool.into()));
                                }
                            } else if let Some(vec) = composite_literal_vector(sp)
                                && let Some(shape) = shape_of(sp)
                            {
                                if literal_pool.insert(shape, vec)
                                    && let Some(shape) = shape_of(sp)
                                    && let Some(m) = scrutinee_member(
                                        &ctx.env,
                                        self.arg.node.typ(),
                                        &shape,
                                    )?
                                {
                                    mtypes.push(m);
                                }
                            } else if let Some((k, exact)) = sp.array_len_coverage() {
                                slice_pool.push((k, exact, at.clone()));
                            } else if sp.is_array_slice() {
                                refutable_slice = true;
                            }
                        }
                    }
                }
            }
            if !inferred_irrefutable {
                itypes.push(&pat.type_predicate);
            }
        }
        let itype = Type::union(&ctx.env, &itypes)?;
        drop(itypes);
        if wildcard {
            // Narrow an under-constrained scrutinee against the informative
            // arm predicates, except a union scrutinee: its open members
            // are what the arms discriminate and must stay free.
            let union_scrut = match self.arg.node.typ().deref_cloned() {
                Some(Type::Set(_)) => true,
                Some(t @ Type::Ref(_)) => matches!(t.lookup_ref(&ctx.env)?, Type::Set(_)),
                _ => false,
            };
            if itype != Type::Primitive(BitFlags::empty()) && !union_scrut {
                let _ = itype.contains(&ctx.env, &self.arg.node.typ())?;
            }
        } else {
            itype.check_contains(&ctx.env, &self.arg.node.typ()).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!("missing match cases {e}")
                })
            })?;
            // The slice pool resolves after the itype check narrows the
            // scrutinee and before the mtype check joins its coverage.
            let mut slice_note = String::new();
            if !slice_pool.is_empty() {
                let rest =
                    slice_pool.iter().filter(|(_, e, _)| !e).map(|(k, _, _)| *k).min();
                match rest {
                    None => {
                        slice_note = " (the slice arms cover finitely many \
                                      lengths — an array or list scrutinee also needs a \
                                      rest pattern or a wildcard)"
                            .into()
                    }
                    Some(rest) => {
                        let hole = (0..rest)
                            .find(|n| !slice_pool.iter().any(|(k, e, _)| *e && k == n));
                        match hole {
                            Some(n) => {
                                slice_note = format!(
                                    " (the slice arms leave array length {n} \
                                     uncovered)"
                                )
                            }
                            None => {
                                let mut members: smallvec::SmallVec<[Type; 4]> =
                                    smallvec::SmallVec::new();
                                array_members(
                                    &ctx.env,
                                    self.arg.node.typ(),
                                    &mut members,
                                    0,
                                )?;
                                for m in members {
                                    let mut all = true;
                                    for (_, _, p) in slice_pool.iter() {
                                        if !p.contains(&ctx.env, &m)? {
                                            all = false;
                                            break;
                                        }
                                    }
                                    if all {
                                        mtypes.push(m);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if refutable_slice {
                slice_note.push_str(
                    " (a slice arm with refutable element patterns — literals, \
                     variants, nested slices — cannot establish length coverage)",
                );
            }
            if guarded_slice {
                slice_note
                    .push_str(" (a guarded slice arm cannot establish length coverage)");
            }
            let mtype =
                Type::union(&ctx.env, &mtypes.iter().collect::<LPooled<Vec<_>>>())?;
            let scrut = self.arg.node.typ().clone();
            mtype.check_contains(&ctx.env, &scrut).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    if mtype == Type::Primitive(BitFlags::empty()) {
                        anyhow!(
                            "missing match cases: no unguarded arm irrefutably \
                             covers {scrut}{slice_note}"
                        )
                    } else {
                        anyhow!("missing match cases {e}{slice_note}")
                    }
                })
            })?;
        }
        let mut ntype = self.arg.node.typ().clone().normalize();
        for (pat, n) in self.arms.iter_mut() {
            // Alias the arm's binds against the scrutinee minus every
            // earlier unguarded irrefutable arm. The `any_as_tvar` view
            // keeps a `_` slot from short-circuiting the walk.
            ntype.contains(&ctx.env, &pat.type_predicate.any_as_tvar())?;
            // The guard typechecks after the narrowing so it sees the
            // arm's binds at their settled type; it must be bool.
            if let Some(guard) = &mut pat.guard {
                guard.node.typecheck0(ctx)?;
                let bt = Type::Primitive(Typ::Bool.into());
                wrap!(guard.node, bt.check_contains(&ctx.env, guard.node.typ()))?;
            }
            wrap!(n, n.typecheck0(ctx))?;
            rtypes.push(n.typ());
            if !pat.structure_predicate.is_refutable() && pat.guard.is_none() {
                ntype = ntype.diff(&ctx.env, &pat.type_predicate)?;
            }
        }
        self.typ = Type::union(&ctx.env, &rtypes)?;
        drop(rtypes);
        // The dead-arm walk: `atype` is what can still reach each arm.
        // Array members subtract length-precisely, and a slice arm whose
        // every length is already matched is dead whatever its guard.
        let mut atype = self.arg.node.typ().clone().normalize();
        let mut members: smallvec::SmallVec<[Type; 4]> = smallvec::SmallVec::new();
        array_members(&ctx.env, &atype, &mut members, 0)?;
        struct Cov {
            m: Type,
            exacts: smallvec::SmallVec<[usize; 8]>,
            rest: Option<usize>,
            done: bool,
        }
        let mut covered: smallvec::SmallVec<[Cov; 4]> = members
            .drain(..)
            .map(|m| Cov {
                m,
                exacts: smallvec::SmallVec::new(),
                rest: None,
                done: false,
            })
            .collect();
        let (mut saw_t, mut saw_f) = (false, false);
        let mut dead_pool = LiteralPool::default();
        for (pat, _) in self.arms.iter() {
            if atype == Type::Primitive(BitFlags::empty()) {
                bail!(
                    "unreachable arm: the earlier arms already cover the whole \
                     scrutinee, unused match cases"
                )
            }
            if !&pat.type_predicate.could_match(&ctx.env, &atype)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    if pat.explicit_type_predicate && pat.type_predicate.has_bottom() {
                        bail!(
                            "pattern {} will never match {}, unused match cases (`_` in a \
                             type is bottom, the type of never(); a type test that admits \
                             any parameter is spelled with Any, e.g. Error<Any>)",
                            pat.type_predicate,
                            atype
                        )
                    }
                    bail!(
                        "pattern {} will never match {}, unused match cases",
                        pat.type_predicate,
                        atype
                    )
                })?
            }
            let mut atoms: smallvec::SmallVec<[(&StructPatternNode, Type); 4]> =
                smallvec::SmallVec::new();
            arm_atoms(&pat.structure_predicate, &pat.type_predicate, &mut atoms);
            let or_arm = atoms.len() > 1;
            for (sp, at) in atoms.iter() {
                if or_arm && !at.could_match(&ctx.env, &atype)? {
                    format_with_flags(PrintFlag::DerefTVars, || {
                        bail!(
                            "unreachable or-pattern alternative: {at} will never \
                             match {atype}, unused match cases"
                        )
                    })?
                }
                if let Some((k, exact)) = sp.array_len_range() {
                    let mut any = false;
                    let mut all = true;
                    for c in covered.iter() {
                        if at.could_match(&ctx.env, &c.m)? {
                            any = true;
                            all &= range_covered(k, exact, &c.exacts, c.rest);
                        }
                    }
                    if any && all {
                        if or_arm {
                            bail!(
                                "unreachable or-pattern alternative: every array \
                                 length it can match is covered by earlier arms, \
                                 unused match cases"
                            )
                        }
                        bail!(
                            "unreachable arm: every array length this slice pattern \
                             can match is covered by earlier arms, unused match cases"
                        )
                    }
                    if pat.guard.is_none() && sp.array_len_coverage().is_some() {
                        for c in covered.iter_mut() {
                            if !c.done && at.contains(&ctx.env, &c.m)? {
                                if exact {
                                    if !c.exacts.contains(&k) {
                                        c.exacts.push(k)
                                    }
                                } else {
                                    c.rest = Some(c.rest.map_or(k, |r| r.min(k)))
                                }
                                if lens_complete(&c.exacts, c.rest) {
                                    c.done = true;
                                    atype = atype.diff(&ctx.env, &c.m)?;
                                }
                            }
                        }
                    }
                }
                if pat.guard.is_none()
                    && let StructPatternNode::Literal(Value::Bool(b)) = sp
                {
                    saw_t |= *b;
                    saw_f |= !*b;
                    if saw_t && saw_f {
                        atype =
                            atype.diff(&ctx.env, &Type::Primitive(Typ::Bool.into()))?;
                    }
                }
                if pat.guard.is_none()
                    && let Some(vec) = composite_literal_vector(sp)
                    && let Some(shape) = shape_of(sp)
                    && dead_pool.insert(shape, vec)
                    && let Some(shape) = shape_of(sp)
                    && let Some(m) =
                        scrutinee_member(&ctx.env, self.arg.node.typ(), &shape)?
                {
                    atype = atype.diff(&ctx.env, &m)?;
                }
                if !sp.is_refutable() && pat.guard.is_none() {
                    atype = atype.diff(&ctx.env, at)?;
                }
            }
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.arg.node.typecheck1(ctx)?;
        for (pat, n) in self.arms.iter_mut() {
            if let Some(guard) = &mut pat.guard {
                guard.node.typecheck1(ctx)?;
            }
            wrap!(n, n.typecheck1(ctx))?;
        }
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Select(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_select_node(cx, self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Reached only when no enclosing region fused this select whole.
        // The scrutinee, each guard and each arm body get their own
        // region passes; constant bodies are skipped.
        crate::fusion::fuse(&mut self.arg.node, ctx)?;
        for (pat, body) in self.arms.iter_mut() {
            if let Some(g) = &mut pat.guard {
                if !matches!(g.node.view(), NodeView::Constant(_)) {
                    crate::fusion::fuse(&mut g.node, ctx)?;
                }
            }
            if !matches!(body.view(), NodeView::Constant(_)) {
                crate::fusion::fuse(body, ctx)?;
            }
        }
        Ok(None)
    }
}
