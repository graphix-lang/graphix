use super::{
    Held, WakeBit,
    compiler::compile,
    pattern::{ArmMatch, PatternNode, SliceKind, StructPatternNode},
    wake::TrackedFires,
};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent, bailat,
    env::Env,
    expr::{At, Expr, ExprId, ExprKind, Pattern, union_members},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag, tag_len},
    },
    typ::Type,
    wrap,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::{Typ, Value};
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::sync::atomic::{AtomicBool, Ordering};
use triomphe::Arc;

atomic_id!(SelectId);

#[derive(Debug)]
pub struct Select<R: Rt, E: UserEvent> {
    /// The selected arm. Semantic state: survives sleep and
    /// `reset_replay`.
    selected: Option<usize>,
    pub arg: Held<R, E>,
    pub arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
    pub typ: Type,
    pub(crate) spec: Expr,
    /// In a frame, a tail re-selection rides the arm's tag instead of
    /// firing.
    pub(crate) tail_dispatch_select: AtomicBool,
    /// Bit i = arm i's guard was consulted by the last re-match. Quiet
    /// cycles read it so a standing bottom on a consulted guard keeps
    /// the select bottom.
    consulted_guard_mask: ArmMask,
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
    /// Per arm: the shallow discriminator of an inferred predicate.
    shallow: Vec<Option<Type>>,
}

impl LazyArmFacts {
    fn build<R: Rt, E: UserEvent>(
        ctx: &ExecCtx<R, E>,
        scrut: &Type,
        arms: &[(PatternNode<R, E>, Node<R, E>)],
    ) -> Self {
        LazyArmFacts {
            tracked: TrackedFires::new(&ctx.env, arms.len(), |i, r| {
                arm_refs(&arms[i], r)
            }),
            sleep_on_deselect: arms
                .iter()
                .map(|(_, n)| crate::analysis::arm_sleeps_on_deselect(ctx, n))
                .collect(),
            shallow: arms
                .iter()
                .map(|(pat, _)| pat.shallow_discriminant(&ctx.env, scrut))
                .collect(),
        }
    }
}

/// One bit per arm; inline up to 64 arms.
#[derive(Debug, Default)]
struct ArmMask(SmallVec<[u64; 1]>);

impl ArmMask {
    fn clear(&mut self, arms: usize) {
        self.0.clear();
        self.0.resize(arms.div_ceil(64), 0);
    }

    fn set(&mut self, i: usize) {
        self.0[i / 64] |= 1 << (i % 64);
    }

    fn get(&self, i: usize) -> bool {
        self.0.get(i / 64).is_some_and(|w| w & (1 << (i % 64)) != 0)
    }
}

/// What this cycle's consumed inputs say about the emission: the
/// scrutinee's production and the guards the last re-match consulted.
struct EmissionPlanes {
    /// A consulted guard's production fired.
    guard_fire: bool,
    /// A non-bottom fire was consumed.
    sound: bool,
    /// Any consumed input fired, bottom or not.
    anyfire: bool,
    /// A consulted guard's current channel is bottom.
    consulted_bottom: bool,
}

impl EmissionPlanes {
    /// `arg_prod` is a non-bottom scrutinee production.
    fn new(guard_tags: &[Option<Tag>], arg_prod: Tag, mask: &ArmMask) -> Self {
        let mut planes = EmissionPlanes {
            guard_fire: false,
            sound: arg_prod.triggers(),
            anyfire: arg_prod.triggers(),
            consulted_bottom: false,
        };
        let consulted = guard_tags.iter().enumerate().filter(|(i, _)| mask.get(*i));
        for t in consulted.filter_map(|(_, t)| *t) {
            if t.triggers() {
                planes.guard_fire = true;
                planes.anyfire = true;
                planes.sound |= !t.is_bottom();
            }
            planes.consulted_bottom |= t.is_bottom();
        }
        planes
    }

    /// The taken arm's production `(t, v)` as the select's: a value
    /// fires when the arm or a sound consumed input fired; a bottom arm
    /// sets the select bottom, fresh on the same condition.
    fn emit(&self, t: Tag, v: Option<Value>) -> TagValue {
        match v {
            Some(v) if t.is_fired() || self.sound => TagValue::fired(v),
            Some(v) => TagValue::stale(v),
            None if t.triggers() || self.sound => {
                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
            }
            None => TagValue::tagged(Value::Null, Tag::STALE_BOTTOM),
        }
    }
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
    fn new(
        arg: Held<R, E>,
        arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
        typ: Type,
        spec: Expr,
        tail_dispatch_select: bool,
    ) -> Node<R, E> {
        Node::new(Self {
            selected: None,
            arg,
            arms,
            typ,
            spec,
            tail_dispatch_select: AtomicBool::new(tail_dispatch_select),
            consulted_guard_mask: ArmMask::default(),
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            arm_facts: None,
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let arg = Held::image_decode(ctx, buf)?;
        let n = decode_varint(buf)? as usize;
        let mut arms = Vec::with_capacity(n.min(buf.len()));
        for _ in 0..n {
            let pat = PatternNode::image_decode(ctx, buf)?;
            let body = decode_node(ctx, buf)?;
            arms.push((pat, body));
        }
        let typ = Type::decode(buf)?;
        let spec = Expr::decode(buf)?;
        let tail_dispatch_select = bool::decode(buf)?;
        Ok(Self::new(arg, arms, typ, spec, tail_dispatch_select))
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
        let inputs = scrutinee_inputs(&ctx.env, &arg.node);
        let arms = arms
            .iter()
            .map(|(pat, body)| {
                let scope = scope.append_block("sel", SelectId::new().0);
                let pat = PatternNode::compile(
                    ctx,
                    flags,
                    pat,
                    &scope,
                    top_id,
                    body.pos,
                    body.ori.clone(),
                )
                .at(body)?;
                pat.structure_predicate
                    .ids(&mut |id| ctx.env.mark_pattern_bind(id, inputs.clone()));
                let n = compile(ctx, flags, body.clone(), &scope, top_id)?;
                Ok((pat, n))
            })
            .collect::<Result<Vec<_>>>()
            .at(&spec)?;
        Ok(Self::new(arg, arms, Type::empty_tvar(), spec, false))
    }
}

impl<R: Rt, E: UserEvent> Select<R, E> {
    /// Exhaustiveness: the unguarded arms must cover `scrut`, by an arm
    /// that matches anything or by the union of what the irrefutable
    /// atoms, bool literals, literal pools and slice ladders cover.
    fn check_coverage(&self, ctx: &ExecCtx<R, E>, scrut: &Type) -> Result<()> {
        let env = &ctx.env;
        let mut mtypes: LPooled<Vec<Type>> = LPooled::take();
        let mut itypes: LPooled<Vec<&Type>> = LPooled::take();
        let (mut saw_true, mut saw_false) = (false, false);
        // An unguarded arm that matches anything makes the select
        // exhaustive; its fresh-tvar predicate must stay out of the
        // coverage unions, which `check_contains` would greedily bind.
        let mut wildcard = false;
        let mut pooled: SmallVec<[&StructPatternNode; 8]> = SmallVec::new();
        // `(k, exact, predicate)`: an unguarded slice atom matching every
        // array of length == k or >= k.
        let mut slices: SmallVec<[(usize, bool, Type); 8]> = SmallVec::new();
        let (mut guarded_slice, mut refutable_slice) = (false, false);
        for (pat, _) in self.arms.iter() {
            let wild = pat.matches_every(env, scrut)?;
            match &pat.guard {
                Some(_) => guarded_slice |= pat.structure_predicate.is_array_slice(),
                None if wild => wildcard = true,
                None => {
                    for (sp, at) in pat.atoms() {
                        if !sp.is_refutable() {
                            mtypes.push(at)
                        } else if let StructPatternNode::Literal(Value::Bool(b)) = sp {
                            saw_true |= *b;
                            saw_false |= !*b;
                            if saw_true && saw_false {
                                mtypes.push(Type::Primitive(Typ::Bool.into()));
                            }
                        } else if Shape::of(sp).is_some() {
                            pooled.push(sp)
                        } else if let Some((k, exact)) = sp.array_len_coverage() {
                            slices.push((k, exact, at));
                        } else if sp.is_array_slice() {
                            refutable_slice = true;
                        }
                    }
                }
            }
            if !wild {
                itypes.push(&pat.type_predicate);
            }
        }
        let itype = Type::union(env, &itypes)?;
        drop(itypes);
        if wildcard {
            // Narrow an under-constrained scrutinee against the informative
            // arm predicates, except a union scrutinee: its open members
            // are what the arms discriminate and must stay free.
            let union_scrut = match scrut.deref_cloned() {
                Some(Type::Set(_)) => true,
                Some(t @ Type::Ref(_)) => matches!(t.lookup_ref(env)?, Type::Set(_)),
                _ => false,
            };
            if itype != Type::Primitive(BitFlags::empty()) && !union_scrut {
                let _ = itype.contains(env, scrut)?;
            }
            return Ok(());
        }
        itype.check_contains(env, scrut).map_err(|e| {
            format_with_flags(PrintFlag::DerefTVars, || {
                anyhow!("missing match cases {e}")
            })
        })?;
        // The pools and ladders read the scrutinee as the itype check
        // settled it.
        let mut pool = LiteralPool::default();
        for sp in pooled {
            if let Some(m) = pool.claim(env, scrut, sp)? {
                mtypes.push(m)
            }
        }
        // The first array member the slice arms leave open: its rest
        // bound and least uncovered length.
        let mut open: Option<(Option<usize>, Option<usize>)> = None;
        if !slices.is_empty() {
            for mut ladder in Ladder::of(env, scrut)? {
                for (k, exact, at) in slices.iter() {
                    if at.contains_with_flags(BitFlags::empty(), env, &ladder.member)? {
                        ladder.claim(*k, *exact);
                    }
                }
                if ladder.complete() {
                    mtypes.push(ladder.member)
                } else if open.is_none() {
                    open = Some((ladder.rest, ladder.gap()))
                }
            }
        }
        let mtype = Type::union(env, &mtypes.iter().collect::<LPooled<Vec<_>>>())?;
        mtype.check_contains(env, scrut).map_err(|e| {
            format_with_flags(PrintFlag::DerefTVars, || {
                let gap = match open {
                    None => format_compact!(""),
                    Some((None, _)) => format_compact!(
                        " (the slice arms cover finitely many lengths — an array \
                         or list scrutinee also needs a rest pattern or a wildcard)"
                    ),
                    Some((Some(_), n)) => format_compact!(
                        " (the slice arms leave array length {} uncovered)",
                        n.unwrap_or(0)
                    ),
                };
                let refutable = if refutable_slice {
                    " (a slice arm with refutable element patterns — literals, \
                     variants, nested slices — cannot establish length coverage)"
                } else {
                    ""
                };
                let guarded = if guarded_slice {
                    " (a guarded slice arm cannot establish length coverage)"
                } else {
                    ""
                };
                if mtype == Type::Primitive(BitFlags::empty()) {
                    anyhow!(
                        "missing match cases: no unguarded arm irrefutably covers \
                         {scrut}{gap}{refutable}{guarded}"
                    )
                } else {
                    anyhow!("missing match cases {e}{gap}{refutable}{guarded}")
                }
            })
        })
    }

    /// Dead arms: an arm, or an or-alternative, that nothing reaching it
    /// can match. `atype` is what can still reach each arm; array
    /// members subtract length-precisely, and a slice arm whose every
    /// length is already matched is dead whatever its guard.
    fn check_dead_arms(&self, ctx: &ExecCtx<R, E>, scrut: &Type) -> Result<()> {
        let env = &ctx.env;
        let mut atype = scrut.normalize();
        let mut ladders = Ladder::of(env, &atype)?;
        let (mut saw_t, mut saw_f) = (false, false);
        let mut pool = LiteralPool::default();
        for (pat, body) in self.arms.iter() {
            let site = body.spec();
            if atype == Type::Primitive(BitFlags::empty()) {
                bailat!(
                    site,
                    "unreachable arm: the earlier arms already cover the whole \
                     scrutinee, unused match cases"
                )
            }
            if !pat.type_predicate.could_match(env, &atype)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    if pat.explicit_type_predicate && pat.type_predicate.has_bottom() {
                        bailat!(
                            site,
                            "pattern {} will never match {}, unused match cases (`_` in a \
                             type is bottom, the type of never(); a type test that admits \
                             any parameter is spelled with Any, e.g. Error<Any>)",
                            pat.type_predicate,
                            atype
                        )
                    }
                    bailat!(
                        site,
                        "pattern {} will never match {}, unused match cases",
                        pat.type_predicate,
                        atype
                    )
                })?
            }
            let atoms = pat.atoms();
            let or_arm = atoms.len() > 1;
            let unguarded = pat.guard.is_none();
            for (sp, at) in atoms.iter() {
                if or_arm && !at.could_match(env, &atype)? {
                    format_with_flags(PrintFlag::DerefTVars, || {
                        bailat!(
                            site,
                            "unreachable or-pattern alternative: {at} will never \
                             match {atype}, unused match cases"
                        )
                    })?
                }
                if let Some((k, exact)) = sp.array_len_range() {
                    let (mut any, mut all) = (false, true);
                    for l in ladders.iter() {
                        if at.could_match(env, &l.member)? {
                            any = true;
                            all &= l.covers(k, exact);
                        }
                    }
                    if any && all {
                        if or_arm {
                            bailat!(
                                site,
                                "unreachable or-pattern alternative: every array \
                                 length it can match is covered by earlier arms, \
                                 unused match cases"
                            )
                        }
                        bailat!(
                            site,
                            "unreachable arm: every array length this slice pattern \
                             can match is covered by earlier arms, unused match cases"
                        )
                    }
                    if unguarded && sp.array_len_coverage().is_some() {
                        for l in ladders.iter_mut() {
                            if !l.complete()
                                && at.contains(env, &l.member)?
                                && l.claim(k, exact)
                            {
                                atype = atype.diff(env, &l.member)?;
                            }
                        }
                    }
                }
                if !unguarded {
                    continue;
                }
                if let StructPatternNode::Literal(Value::Bool(b)) = sp {
                    saw_t |= *b;
                    saw_f |= !*b;
                    if saw_t && saw_f {
                        atype = atype.diff(env, &Type::Primitive(Typ::Bool.into()))?;
                    }
                }
                if let Some(m) = pool.claim(env, scrut, sp)? {
                    atype = atype.diff(env, &m)?;
                }
                if !sp.is_refutable() {
                    atype = atype.diff(env, at)?;
                }
            }
        }
        Ok(())
    }
}

/// The inputs whose fires reach a scrutinee, for the pattern binds it
/// delivers: its triggering free refs (a level under a sample's right
/// side is banked, never fired through), with a pattern bind among
/// them standing for its own inputs.
fn scrutinee_inputs<R: Rt, E: UserEvent>(env: &Env, arg: &Node<R, E>) -> Arc<[BindId]> {
    let mut r = Refs::default();
    arg.refs(&mut r);
    let mut out: LPooled<IntSet<BindId>> = LPooled::take();
    for id in r.triggering.difference(&r.bound) {
        match env.pattern_inputs(*id) {
            Some(inputs) => out.extend(inputs.iter().copied()),
            None => {
                out.insert(*id);
            }
        }
    }
    Arc::from_iter(out.drain())
}

/// The slice-length ladder over one array or list member of a
/// scrutinee: the exact lengths and the least `rest..` bound the slice
/// arms claim for it.
struct Ladder {
    member: Type,
    exacts: SmallVec<[usize; 8]>,
    rest: Option<usize>,
}

impl Ladder {
    /// One ladder per array or list member of `scrut`.
    fn of(env: &Env, scrut: &Type) -> Result<SmallVec<[Ladder; 4]>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, scrut, &mut members)?;
        Ok(members
            .into_iter()
            .filter(|m| matches!(m, Type::Array(_) | Type::List(_)))
            .map(|member| Ladder { member, exacts: SmallVec::new(), rest: None })
            .collect())
    }

    /// Do the claimed lengths cover every length? That needs a rest
    /// bound with every length below it exact.
    fn complete(&self) -> bool {
        self.gap().is_none()
    }

    /// The least length no claim covers, `None` when they cover all;
    /// with no rest bound every length past the exacts is a gap.
    fn gap(&self) -> Option<usize> {
        let r = self.rest.unwrap_or(usize::MAX);
        (0..r).find(|n| !self.exacts.contains(n))
    }

    /// Is the length range `== k` (`exact`) / `>= k` already claimed?
    fn covers(&self, k: usize, exact: bool) -> bool {
        let covered =
            |n: usize| self.rest.is_some_and(|r| n >= r) || self.exacts.contains(&n);
        match (exact, self.rest) {
            (true, _) => covered(k),
            (false, None) => false,
            (false, Some(r)) => (k..r.max(k)).all(covered),
        }
    }

    /// Claim `== k` (`exact`) or `>= k`; true when this claim completes
    /// the ladder.
    fn claim(&mut self, k: usize, exact: bool) -> bool {
        if self.complete() {
            return false;
        }
        if !exact {
            self.rest = Some(self.rest.map_or(k, |r| r.min(k)));
        } else if !self.exacts.contains(&k) {
            self.exacts.push(k)
        }
        self.complete()
    }
}

/// Deselect arm `j`: sleep it unless it is pure, then refresh its
/// tracked refs. Under `deselecting_arm` a recursive-edge callee inside
/// the arm is deleted, not retained (`CallSite::sleep`), so unreached
/// activations are shed.
fn deselect<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    tracked: &mut TrackedFires,
    j: usize,
    (pat, body): &mut (PatternNode<R, E>, Node<R, E>),
    sleep: bool,
) {
    if sleep {
        let saved = ctx.deselecting_arm;
        ctx.deselecting_arm = true;
        body.sleep(ctx);
        ctx.deselecting_arm = saved;
    }
    tracked.refresh(&ctx.env, j, |r| {
        body.refs(r);
        bound_by(pat, r)
    });
}

/// An arm's free reads: its body's refs less its pattern's binds.
fn arm_refs<R: Rt, E: UserEvent>(
    (pat, body): &(PatternNode<R, E>, Node<R, E>),
    r: &mut Refs,
) {
    body.refs(r);
    bound_by(pat, r)
}

fn bound_by<R: Rt, E: UserEvent>(pat: &PatternNode<R, E>, r: &mut Refs) {
    pat.structure_predicate.ids(&mut |id| {
        r.bound.insert(id);
    });
}

/// A refutable head the literal pool reads at one position of a
/// composite pattern.
#[derive(Clone, PartialEq, Eq)]
enum Head {
    Bool(bool),
    /// A variant's tag and arity; its payload matches anything.
    Tag(ArcStr, usize),
}

/// One position of a composite pattern: the head the arm demands there
/// (`None` = any value) and the heads the scrutinee member admits there
/// (`None` = not a finite set of heads).
struct Leaf {
    head: Option<Head>,
    domain: Option<SmallVec<[Head; 4]>>,
}

/// The heads `t` admits when it is a finite set of them: `bool`, or a
/// union of variants.
fn heads(env: &Env, t: &Type) -> Result<Option<SmallVec<[Head; 4]>>> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    union_members(env, t, &mut members)?;
    let is_bool = |m: &Type| matches!(m, Type::Primitive(p) if *p == Typ::Bool);
    if !members.is_empty() && members.iter().all(is_bool) {
        return Ok(Some(SmallVec::from_iter([Head::Bool(true), Head::Bool(false)])));
    }
    let tag = |m: &Type| match m {
        Type::Variant(tag, ts, _) => Some(Head::Tag(tag.clone(), ts.len())),
        _ => None,
    };
    Ok(if members.is_empty() { None } else { members.iter().map(tag).collect() })
}

/// The pooled positions of composite pattern `sp` against `m`, the
/// scrutinee member of its shape, in position order; `None` when the
/// pattern has a refutable part the pool does not read (another
/// literal, a slice, a type test, a variant with a refutable payload)
/// or tests no head.
fn pooled_leaves(
    env: &Env,
    sp: &StructPatternNode,
    m: &Type,
) -> Result<Option<SmallVec<[Leaf; 8]>>> {
    fn leaf(
        env: &Env,
        sp: &StructPatternNode,
        t: &Type,
        out: &mut SmallVec<[Leaf; 8]>,
    ) -> Result<bool> {
        let head = match sp {
            StructPatternNode::Bind(_) | StructPatternNode::Ignore => None,
            StructPatternNode::Literal(Value::Bool(b)) => Some(Head::Bool(*b)),
            StructPatternNode::Variant { tag, all: _, binds }
                if binds.iter().all(|p| p.matches_anything()) =>
            {
                Some(Head::Tag(tag.clone(), binds.len()))
            }
            StructPatternNode::Slice { kind: SliceKind::Tuple, .. }
            | StructPatternNode::Struct { .. } => {
                return crate::stack::ensure_sufficient(|| children(env, sp, t, out));
            }
            _ => return Ok(false),
        };
        out.push(Leaf { head, domain: heads(env, t)? });
        Ok(true)
    }
    fn children(
        env: &Env,
        sp: &StructPatternNode,
        t: &Type,
        out: &mut SmallVec<[Leaf; 8]>,
    ) -> Result<bool> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, t, &mut members)?;
        let [m] = &members[..] else { return Ok(false) };
        match (sp, m) {
            (
                StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds },
                Type::Tuple(ts),
            )
            | (
                StructPatternNode::Variant { tag: _, all: _, binds },
                Type::Variant(_, ts, _),
            ) if ts.len() == binds.len() => {
                for (p, t) in binds.iter().zip(ts.iter()) {
                    if !leaf(env, p, t, out)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (StructPatternNode::Struct { all: _, binds }, Type::Struct(fs)) => {
                let mut order: SmallVec<[&(ArcStr, usize, StructPatternNode); 8]> =
                    binds.iter().collect();
                order.sort_by_key(|(_, i, _)| *i);
                for (name, _, p) in order {
                    let Some((_, t, _)) = fs.iter().find(|(n, _, _)| n == name) else {
                        return Ok(false);
                    };
                    if !leaf(env, p, t, out)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }
    let mut out = SmallVec::new();
    let pooled = children(env, sp, m, &mut out)? && out.iter().any(|l| l.head.is_some());
    Ok(pooled.then_some(out))
}

/// The head a composite pattern tests: what groups arms in the
/// literal pool and selects the scrutinee member they cover.
#[derive(PartialEq, Eq)]
enum Shape {
    Tuple(usize),
    Variant(ArcStr, usize),
    Struct(SmallVec<[ArcStr; 8]>),
}

impl Shape {
    fn of(sp: &StructPatternNode) -> Option<Shape> {
        match sp {
            StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds } => {
                Some(Shape::Tuple(binds.len()))
            }
            StructPatternNode::Variant { tag, all: _, binds } => {
                Some(Shape::Variant(tag.clone(), binds.len()))
            }
            StructPatternNode::Struct { all: _, binds } => {
                let mut names: SmallVec<[ArcStr; 8]> =
                    binds.iter().map(|(n, _, _)| n.clone()).collect();
                names.sort();
                Some(Shape::Struct(names))
            }
            _ => None,
        }
    }

    fn fits(&self, t: &Type) -> bool {
        match (self, t) {
            (Shape::Tuple(n), Type::Tuple(ts)) => *n == ts.len(),
            (Shape::Variant(tag, n), Type::Variant(t, ts, _)) => {
                tag == t && *n == ts.len()
            }
            (Shape::Struct(names), Type::Struct(fs)) => {
                names.len() == fs.len()
                    && names.iter().zip(fs.iter()).all(|(n, (f, _, _))| n == f)
            }
            _ => false,
        }
    }

    /// The scrutinee's member of this shape: the type a complete pool
    /// group covers.
    fn member(&self, env: &Env, scrut: &Type) -> Result<Option<Type>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, scrut, &mut members)?;
        Ok(members.into_iter().find(|m| self.fits(m)))
    }
}

/// A pool group's combinations are enumerated up to this many; a group
/// over more positions never completes.
const MAX_POOL_COMBINATIONS: usize = 1024;

/// Composite arms whose only refutable parts are bool literals and
/// variant heads pool coverage per position: same-shaped arms cover the
/// scrutinee's member of that shape once some arm matches every
/// combination of heads the member admits (`(true, `A) | (true, `B) |
/// (false, _)` covers `(bool, [`A, `B])`).
#[derive(Default)]
struct LiteralPool {
    groups: Vec<PoolGroup>,
}

struct PoolGroup {
    shape: Shape,
    arms: Vec<SmallVec<[Leaf; 8]>>,
    done: bool,
}

impl LiteralPool {
    /// Pool the unguarded atom `sp`: the scrutinee member whose coverage
    /// it completes, if it does.
    fn claim(
        &mut self,
        env: &Env,
        scrut: &Type,
        sp: &StructPatternNode,
    ) -> Result<Option<Type>> {
        let Some(shape) = Shape::of(sp) else { return Ok(None) };
        let Some(m) = shape.member(env, scrut)? else { return Ok(None) };
        let Some(leaves) = pooled_leaves(env, sp, &m)? else { return Ok(None) };
        let i = match self.groups.iter().position(|g| g.shape == shape) {
            Some(i) => i,
            None => {
                self.groups.push(PoolGroup { shape, arms: Vec::new(), done: false });
                self.groups.len() - 1
            }
        };
        let g = &mut self.groups[i];
        if g.done || g.arms.iter().any(|a| a.len() != leaves.len()) {
            return Ok(None);
        }
        g.arms.push(leaves);
        g.done = g.complete();
        Ok(g.done.then_some(m))
    }
}

impl PoolGroup {
    fn complete(&self) -> bool {
        let mut positions: SmallVec<[(usize, &[Head]); 8]> = SmallVec::new();
        let mut total = 1usize;
        for i in 0..self.arms[0].len() {
            if self.arms.iter().all(|a| a[i].head.is_none()) {
                continue;
            }
            let Some(domain) = self.arms.iter().find_map(|a| a[i].domain.as_deref())
            else {
                return false;
            };
            total = total.saturating_mul(domain.len());
            positions.push((i, domain));
        }
        if total > MAX_POOL_COMBINATIONS {
            return false;
        }
        (0..total).all(|mut c| {
            let mut combo: SmallVec<[&Head; 8]> = SmallVec::new();
            for (_, domain) in positions.iter() {
                combo.push(&domain[c % domain.len()]);
                c /= domain.len();
            }
            self.arms.iter().any(|a| {
                positions
                    .iter()
                    .zip(combo.iter())
                    .all(|((i, _), h)| a[*i].head.as_ref().is_none_or(|x| x == *h))
            })
        })
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

    /// The selection, the consulted mask and the tracker exist only once
    /// a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.selected.is_some() || self.arm_facts.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
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
            self.arm_facts = Some(LazyArmFacts::build(ctx, &scrut, &self.arms));
        }
        let Self {
            selected,
            arg,
            arms,
            typ: _,
            spec,
            tail_dispatch_select,
            consulted_guard_mask,
            resident,
            slept,
            arm_facts,
        } = self;
        let LazyArmFacts { tracked, sleep_on_deselect, shallow } =
            arm_facts.as_mut().expect("arm facts built above");
        let woke = slept.take() && ctx.frame_depth == 0;
        // Per-arm guard production tags; `None` = unguarded. Only guards
        // the chain consults contribute fires or bottomness.
        let mut guard_tags: SmallVec<[Option<Tag>; 8]> =
            SmallVec::with_capacity(arms.len());
        let arg_prod = arg.update(ctx, event);
        tracked.observe(ctx, event);
        // XCR claude for eric: kept per cycle: unbound, a guard's binds read the
        // store as Standing, which an init view reads FIRED where the delivered
        // entry reads STALE, so skipping quiet cycles changes guard firing. The
        // taken arm's rebind carries its own tag; shapes are sealed shallow tests.
        // Guards are live nodes and tick every cycle, even under a
        // tainted scrutinee. The bind is delivered only to an arm whose
        // shape admits the value: the checker narrowed the binds by that
        // shape, and a guard fused to the narrowing must never see the
        // value an earlier arm claimed.
        for ((pat, _), shallow) in arms.iter_mut().zip(shallow.iter()) {
            let bound = match arg.value.as_ref() {
                Some(v)
                    if !arg.tag.is_bottom()
                        && pat.guard.is_some()
                        && pat.shape_matches(&ctx.env, shallow.as_ref(), v) =>
                {
                    pat.bind_event(ctx, event, v, arg_prod);
                    true
                }
                _ => false,
            };
            guard_tags.push(pat.update(ctx, event));
            if bound {
                pat.unbind_event(event);
            }
        }
        // Any guard fire drives a re-match; whether it affects the
        // emission is decided by the consulted set below.
        let pat_up = guard_tags.iter().any(|t| t.is_some_and(|t| t.triggers()));
        // A bottom scrutinee bottoms the select; it consults no guards.
        if arg.tag.is_bottom() {
            return resident.set_bottom(arg_prod.triggers());
        }
        let v = arg.value.as_ref().expect("Held keeps every non-bottom production");
        if crate::dbgenv::graphix_dbg_select() {
            eprintln!(
                "SELECT[{}] upd init={} fd={} pat_up={pat_up} sel={selected:?} argc={v:?} vars={}",
                spec.pos,
                event.init,
                ctx.frame_depth,
                event.variables.len()
            );
        }
        let tail = tail_dispatch_select.load(Ordering::Relaxed) && ctx.frame_depth > 0;
        // Inside frames selection is value-driven: a jump-rebound loop
        // variable arrives STALE, so a triggers-only driver would spin.
        let arg_trig = arg_prod.triggers() || ctx.frame_depth > 0;
        enum ChainOut {
            Quiet(usize),
            Taken(Option<usize>),
            Undet,
        }
        let chain = match *selected {
            Some(i) if !(arg_trig || pat_up || woke) => ChainOut::Quiet(i),
            _ => {
                consulted_guard_mask.clear(arms.len());
                let mut out = ChainOut::Taken(None);
                for (i, ((pat, _), shallow)) in
                    arms.iter().zip(shallow.iter()).enumerate()
                {
                    match pat.arm_match(&ctx.env, shallow.as_ref(), v) {
                        ArmMatch::NoStruct => (),
                        ArmMatch::GuardFalse => consulted_guard_mask.set(i),
                        ArmMatch::GuardBottom => {
                            consulted_guard_mask.set(i);
                            out = ChainOut::Undet;
                            break;
                        }
                        ArmMatch::Matched => {
                            if pat.guard.is_some() {
                                consulted_guard_mask.set(i);
                            }
                            out = ChainOut::Taken(Some(i));
                            break;
                        }
                    }
                }
                out
            }
        };
        let planes = EmissionPlanes::new(&guard_tags, arg_prod, consulted_guard_mask);
        // Sound only: a bottom must never upgrade a stale result to FIRED.
        if planes.sound && tail_dispatch_select.load(Ordering::Relaxed) {
            ctx.tail_scrut_fired = true;
        }
        // A selection stays undecidable while a consulted guard stands
        // bottom: no arm is evaluated.
        let chain = match chain {
            ChainOut::Quiet(_) if planes.consulted_bottom => ChainOut::Undet,
            chain => chain,
        };
        let tv = match chain {
            ChainOut::Undet => return resident.set_bottom(planes.anyfire),
            ChainOut::Quiet(i) => {
                let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx, event);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) if *selected == Some(i) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!("SELECT[{}] same-arm i={i} arg={v:?}", spec.pos);
                }
                arms[i].0.bind_event(ctx, event, v, arg_prod);
                let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx, event);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!(
                        "SELECT[{}] BECOMING-SELECTED {selected:?} -> {i} fd={} init={}",
                        spec.pos, ctx.frame_depth, event.init
                    );
                }
                if let Some(j) = selected.replace(i) {
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect[j]);
                }
                // A stale scrutinee binds stale, a past event at a first consult
                // or a wake alike; only a consulted guard's flip binds FIRED, and
                // under an init view a guard's fire is its birth, not a flip.
                let bind_tag = if tail || arg_prod.triggers() {
                    arg_prod
                } else if planes.guard_fire && !event.init && !woke {
                    Tag::FIRED
                } else {
                    Tag::STALE
                };
                arms[i].0.bind_event(ctx, event, v, bind_tag);
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
                planes.emit(t, v)
            }
            // No arm matches: the select has no value.
            ChainOut::Taken(None) => {
                if let Some(j) = selected.take() {
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect[j]);
                }
                return resident.set_bottom(false);
            }
        };
        resident.set(tv)
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
        for (pat, body) in arms {
            body.sleep(ctx);
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
        for (pat, body) in arms {
            body.reset_replay(ctx);
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

    // XCR claude for eric: not the typecheck: GRAPHIX_PROFILE puts this select's
    // compile at 9 ms at depth 14, while `graphix fmt` (parse + reparse) takes
    // 9.7 s (as a let: 0.05 s). The arm parser (patternexp.rs `pattern`) tries
    // the arm as a `T as` type first, the let path does not; parse-print's.
    // XCR claude for eric: split into completion + narrowing here and
    // `check_coverage` / `check_dead_arms`, which read one `LiteralPool::claim`
    // and one `Ladder`. Two passes remain: coverage must not bind the arms' tvars
    // (it runs before narrowing), the dead-arm walk runs after and may.
    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.arg.node.typecheck0(ctx)?;
        // A partial struct pattern infers only its named fields;
        // complete each inferred predicate against the typed scrutinee
        // before coverage or dispatch reads it.
        if let ExprKind::Select(se) = &self.spec.kind {
            let scrut = self.arg.node.typ().clone();
            for ((pat, body), (spec_pat, _)) in self.arms.iter_mut().zip(se.arms.iter()) {
                if !pat.explicit_type_predicate {
                    let t = spec_pat
                        .structure_predicate
                        .complete_type_predicate(&ctx.env, &pat.type_predicate, &scrut)
                        .at(body.spec())?;
                    if let Some(t) = t {
                        pat.structure_predicate.realign(&ctx.env, &t)?;
                        pat.type_predicate = t;
                    }
                }
            }
        }
        let scrut = self.arg.node.typ().clone();
        self.check_coverage(ctx, &scrut)?;
        let mut rtypes: LPooled<Vec<&Type>> = LPooled::take();
        let mut ntype = scrut.normalize();
        for (pat, n) in self.arms.iter_mut() {
            // Alias the arm's binds against the scrutinee minus every
            // earlier unguarded irrefutable atom. The `any_as_tvar` view
            // keeps a `_` slot from short-circuiting the walk.
            let narrowed = pat.type_predicate.any_as_tvar();
            ntype.contains(&ctx.env, &narrowed)?;
            pat.bind_captures(&ctx.env, &narrowed)?;
            // The guard typechecks after the narrowing so it sees the
            // arm's binds at their settled type; it must be bool.
            if let Some(guard) = &mut pat.guard {
                guard.node.typecheck0(ctx)?;
                let bt = Type::Primitive(Typ::Bool.into());
                wrap!(guard.node, bt.check_contains(&ctx.env, guard.node.typ()))?;
            }
            wrap!(n, n.typecheck0(ctx))?;
            rtypes.push(n.typ());
            if pat.guard.is_none() {
                for (sp, at) in pat.atoms() {
                    if !sp.is_refutable() {
                        ntype = ntype.diff(&ctx.env, &at)?;
                    }
                }
            }
        }
        self.typ = Type::union(&ctx.env, &rtypes)?;
        drop(rtypes);
        self.check_dead_arms(ctx, &scrut)
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
