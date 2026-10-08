use super::{
    Held, WakeBit,
    compiler::compile,
    pattern::{ArmMatch, PatternNode, SliceKind, StructPatternNode},
    wake::TrackedFires,
};
use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent, View, bailat,
    env::Env,
    expr::{At, Expr, ExprId, ExprKind, Pattern, union_members},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    typ::{AbstractId, Type},
    wrap,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::{Typ, Value};
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// Where an error in a pattern is placed: the pattern's text when it was
/// parsed, else its arm's body.
fn pattern_site(pat: &Pattern, body: &Expr) -> Expr {
    match (pat.pos.get(), pat.end.get()) {
        (Some(pos), Some(end)) => {
            let mut site = ExprKind::NoOp.to_expr(pos).ending(end);
            site.ori = body.ori.clone();
            site
        }
        _ => body.clone(),
    }
}
#[derive(Debug)]
pub struct Select<R: Rt, E: UserEvent> {
    /// The selected arm. Semantic state: survives sleep.
    selected: Option<usize>,
    pub arg: Held<R, E>,
    pub arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
    pub typ: Type,
    pub(crate) spec: Expr,
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
    /// A value no arm matched was logged: once per select.
    logged_no_match: bool,
}

impl LazyArmFacts {
    fn build<R: Rt, E: UserEvent>(
        ctx: &ExecCtx<'_, R, E>,
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
            logged_no_match: false,
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
    /// A sound fire was a real event, not only a wake's constants.
    real: bool,
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
            real: arg_prod.triggers() && !arg_prod.is_wake(),
            anyfire: arg_prod.triggers(),
            consulted_bottom: false,
        };
        let consulted = guard_tags.iter().enumerate().filter(|(i, _)| mask.get(*i));
        for t in consulted.filter_map(|(_, t)| *t) {
            if t.triggers() {
                planes.guard_fire = true;
                planes.anyfire = true;
                planes.sound |= !t.is_bottom();
                planes.real |= !t.is_bottom() && !t.is_wake();
            }
            planes.consulted_bottom |= t.is_bottom();
        }
        planes
    }

    /// The taken arm's production `(t, v)` as the select's: a value
    /// fires when the arm or a sound consumed input fired, a real event
    /// when one of those was; a bottom arm sets the select bottom, fresh
    /// on the same condition.
    fn emit(&self, t: Tag, v: Option<Value>) -> TagValue {
        match v {
            Some(v) if t.is_fired() || self.sound => {
                let real = self.real || (t.is_fired() && !t.is_wake());
                TagValue::tagged(v, if real { Tag::FIRED } else { Tag::WAKE_FIRED })
            }
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
    ctx: &mut ExecCtx<'_, R, E>,
) -> (Tag, Option<Value>) {
    let injected = tracked.deliver(ctx, i);
    let tv = arm.update(ctx);
    let t = tv.tag();
    let v = if t.is_bottom() { None } else { Some(tv.value_cloned()) };
    TrackedFires::restore(ctx.event, injected);
    (t, v)
}

impl<R: Rt, E: UserEvent> Select<R, E> {
    fn new(
        arg: Held<R, E>,
        arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
        typ: Type,
        spec: Expr,
    ) -> Node<R, E> {
        Node::new(Self {
            selected: None,
            arg,
            arms,
            typ,
            spec,
            consulted_guard_mask: ArmMask::default(),
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            arm_facts: None,
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let arg = Held::image_decode(ctx, buf)?;
        let n = crate::image::count_decode(buf)?;
        let mut arms = Vec::with_capacity(n);
        for _ in 0..n {
            let pat = PatternNode::image_decode(ctx, buf)?;
            let body = decode_node(ctx, buf)?;
            arms.push((pat, body));
        }
        let typ = Type::decode(buf)?;
        let spec = Expr::decode(buf)?;
        Ok(Self::new(arg, arms, typ, spec))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
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
                let scope = scope.append_block("sel", body.id.inner());
                let pat = PatternNode::compile(
                    ctx,
                    flags,
                    pat,
                    &scope,
                    top_id,
                    body.pos,
                    body.ori.clone(),
                )
                .at(&pattern_site(pat, body))?;
                pat.structure_predicate
                    .ids(&mut |id| ctx.env.mark_pattern_bind(id, inputs.clone()));
                let n = compile(ctx, flags, body.clone(), &scope, top_id)?;
                Ok((pat, n))
            })
            .collect::<Result<Vec<_>>>()
            .at(&spec)?;
        Ok(Self::new(arg, arms, Type::empty_tvar(), spec))
    }
}

/// Do two atoms test the same thing: equal literals and heads at every
/// position, a bind and `_` alike?
fn same_test(a: &StructPatternNode, b: &StructPatternNode) -> bool {
    use StructPatternNode as P;
    let all = |x: &[P], y: &[P]| {
        x.len() == y.len() && x.iter().zip(y).all(|(x, y)| same_test(x, y))
    };
    crate::stack::ensure_sufficient(|| match (a, b) {
        (P::Ignore | P::Bind(_), P::Ignore | P::Bind(_)) => true,
        (P::Literal(x), P::Literal(y)) => x == y,
        (P::Slice { kind: k0, binds: b0, .. }, P::Slice { kind: k1, binds: b1, .. }) => {
            k0 == k1 && all(b0, b1)
        }
        (
            P::SlicePrefix { list: l0, prefix: p0, .. },
            P::SlicePrefix { list: l1, prefix: p1, .. },
        ) => l0 == l1 && all(p0, p1),
        (P::SliceSuffix { suffix: s0, .. }, P::SliceSuffix { suffix: s1, .. }) => {
            all(s0, s1)
        }
        (P::Struct { binds: b0, .. }, P::Struct { binds: b1, .. }) => {
            b0.len() == b1.len()
                && b0
                    .iter()
                    .zip(b1.iter())
                    .all(|((n0, _, p0), (n1, _, p1))| n0 == n1 && same_test(p0, p1))
        }
        (
            P::Variant { tag: t0, binds: b0, .. },
            P::Variant { tag: t1, binds: b1, .. },
        ) => t0 == t1 && all(b0, b1),
        (P::Abstract { id: i0, bind: x0, .. }, P::Abstract { id: i1, bind: x1, .. }) => {
            i0 == i1 && same_test(x0, x1)
        }
        (P::Or { alts: a0 }, P::Or { alts: a1 }) => all(a0, a1),
        _ => false,
    })
}

/// A refutable atom that tests what an earlier unguarded one does, under
/// the same type test, matches nothing new.
fn type_test<R: Rt, E: UserEvent>(p: &PatternNode<R, E>) -> Option<&Type> {
    p.explicit_type_predicate.then_some(&p.type_predicate)
}

fn check_repeats<R: Rt, E: UserEvent>(
    env: &Env,
    arms: &[(PatternNode<R, E>, Node<R, E>)],
) -> Result<()> {
    for (i, (pat, body)) in arms.iter().enumerate() {
        for (sp, at) in pat.atoms() {
            if sp.covers(env, &at, !pat.explicit_type_predicate) {
                continue;
            }
            let repeat =
                arms[..i].iter().filter(|(p, _)| p.guard.is_none()).any(|(p, _)| {
                    type_test(p) == type_test(pat)
                        && p.atoms().iter().any(|(e, _)| same_test(e, sp))
                });
            if repeat {
                bailat!(
                    body.spec(),
                    "unreachable arm: an earlier arm matches the same values, unused match \
                     cases"
                )
            }
        }
    }
    Ok(())
}

/// What reaches each arm of a select, walked arm by arm: the scrutinee
/// less what the unguarded arms before it cover (irrefutable atoms,
/// `null`, a true/false pair, a completed literal pool, a completed
/// slice ladder). It narrows an arm's binds, an arm that can match none
/// of it is dead, and the select is exhaustive when nothing reaches past
/// the last arm.
struct Reach {
    /// What still reaches the next arm.
    atype: Type,
    ladders: SmallVec<[Ladder; 4]>,
    pool: LiteralPool,
    bools: (bool, bool),
    /// What the unguarded arms cover, as containment reads it.
    covered: SmallVec<[Type; 8]>,
    /// An unguarded arm matches every value.
    wildcard: bool,
    guarded_slice: bool,
    refutable_slice: bool,
}

impl Reach {
    /// The walk over `scrut`. An under-constrained scrutinee first
    /// settles against the arms' informative predicates, except a union
    /// scrutinee's open members, which are what the arms discriminate.
    fn new<R: Rt, E: UserEvent>(
        env: &Env,
        scrut: &Type,
        arms: &[(PatternNode<R, E>, Node<R, E>)],
    ) -> Result<Self> {
        let mut itypes: LPooled<Vec<&Type>> = LPooled::take();
        let mut wildcard = false;
        for (pat, _) in arms.iter() {
            match pat.matches_every(env, scrut)? {
                true => wildcard |= pat.guard.is_none(),
                false => itypes.push(&pat.type_predicate),
            }
        }
        let itype = Type::union_exact(env, &itypes)?;
        drop(itypes);
        let union_scrut = match scrut.deref_cloned() {
            Some(Type::Set(_)) => true,
            Some(t @ Type::Ref(_)) => matches!(t.lookup_ref(env)?, Type::Set(_)),
            _ => false,
        };
        // XCR claude for eric: [bug] A guarded arm before an unguarded one over a payload
        // that holds null refuses the select as non-exhaustive: `select v0 { `C(x) if
        // false => 0, `C(v1) => 2, `Some => 5 }` over `[`C([string, null]), `Some]` fails
        // here with "[`C('_: string), `Some] does not contain [`C([null, string]),
        // `Some]", so the unguarded arm's bind is typed string, not [string, null]. It is
        // accepted without the guarded arm, and over `C(i64)`. Found by graphix-fuzz
        // gen-check while fixing the generator; off the CR campaign's topic, so filed
        // rather than fixed. probe: design/review-2026-10-05/repro/coverage-guarded-nullable-01.gx
        // (coverage-guarded-nullable-01)
        // 2026-10-08 claude: the check moved from check_coverage, where it refused, to
        // Reach::new, where it only settles the scrutinee; the arms' walk decides.
        // 2026-10-08 claude: with that, the probe is accepted and `v1` holds the null
        // (2 in both engines); pinned by lang::select::coverage_guarded_nullable_payload.
        let informative = itype != Type::Primitive(BitFlags::empty());
        if informative && !(wildcard && union_scrut) && scrut.has_unbound() {
            let _ = itype.contains(env, scrut);
        }
        let atype = scrut.normalize();
        let ladders = Ladder::of(env, &atype)?;
        Ok(Self {
            atype,
            ladders,
            pool: LiteralPool::default(),
            bools: (false, false),
            covered: SmallVec::new(),
            wildcard,
            guarded_slice: false,
            refutable_slice: false,
        })
    }

    /// Take away what `t` covers from what reaches the arms after.
    fn cover(&mut self, env: &Env, t: Type) -> Result<()> {
        self.atype = self.atype.diff(env, &t)?;
        self.covered.push(t);
        Ok(())
    }

    /// The arm `pat`, whose body is `site`, against what reaches it
    /// (refused when it is dead and `checking`), then less what it
    /// covers.
    fn arm<R: Rt, E: UserEvent>(
        &mut self,
        env: &Env,
        scrut: &Type,
        pat: &PatternNode<R, E>,
        site: &Expr,
        checking: bool,
    ) -> Result<()> {
        let empty = Type::Primitive(BitFlags::empty());
        if checking && self.atype == empty {
            bailat!(
                site,
                "unreachable arm: the earlier arms already cover the whole scrutinee, \
                 unused match cases"
            )
        }
        if checking && !pat.type_predicate.could_match(env, &self.atype)? {
            let (tp, atype) = (&pat.type_predicate, &self.atype);
            format_with_flags(PrintFlag::DerefTVars, || {
                if pat.explicit_type_predicate && tp.has_bottom() {
                    bailat!(
                        site,
                        "pattern {tp} will never match {atype}, unused match cases (`_` in \
                         a type is bottom, the type of never(); a type test that admits any \
                         parameter is spelled with Any, e.g. Error<Any>)"
                    )
                }
                bailat!(site, "pattern {tp} will never match {atype}, unused match cases")
            })?
        }
        let wild = pat.matches_every(env, scrut)?;
        let unguarded = pat.guard.is_none();
        if !unguarded {
            self.guarded_slice |= pat.structure_predicate.is_array_slice();
        }
        let atoms = pat.atoms();
        let or_arm = atoms.len() > 1;
        let dead = |what: &str| -> Result<()> {
            match or_arm {
                true => bailat!(
                    site,
                    "unreachable or-pattern alternative: {what}, unused match cases"
                ),
                false => bailat!(site, "unreachable arm: {what}, unused match cases"),
            }
        };
        let probe = BitFlags::empty();
        for (sp, at) in atoms.iter() {
            if checking && or_arm && !at.could_match(env, &self.atype)? {
                let atype = &self.atype;
                format_with_flags(PrintFlag::DerefTVars, || {
                    bailat!(
                        site,
                        "unreachable or-pattern alternative: {at} will never match \
                         {atype}, unused match cases"
                    )
                })?
            }
            if let Some((k, exact)) = sp.array_len_range() {
                let (mut any, mut all) = (false, true);
                for l in self.ladders.iter() {
                    if at.could_match(env, &l.member)? {
                        any = true;
                        all &= l.covers(k, exact);
                    }
                }
                if checking && any && all {
                    dead(
                        "every array length this slice pattern can match is covered by earlier arms",
                    )?
                }
                let explicit = pat.explicit_type_predicate.then_some(at);
                if unguarded && sp.array_len_coverage(env, explicit).is_some() {
                    let mut done: SmallVec<[Type; 2]> = SmallVec::new();
                    for l in self.ladders.iter_mut() {
                        if !l.complete()
                            && at.contains_with_flags(probe, env, &l.member)?
                            && l.claim(k, exact)
                        {
                            done.push(l.member.clone());
                        }
                    }
                    for m in done {
                        self.cover(env, m)?
                    }
                }
            }
            if !unguarded {
                continue;
            }
            if let StructPatternNode::Literal(v) = sp {
                match v {
                    Value::Bool(b) => {
                        self.bools.0 |= *b;
                        self.bools.1 |= !*b;
                        if self.bools == (true, true) {
                            self.cover(env, Type::Primitive(Typ::Bool.into()))?
                        }
                    }
                    Value::Null => self.cover(env, Type::Primitive(Typ::Null.into()))?,
                    _ => (),
                }
            }
            let explicit = pat.explicit_type_predicate.then_some(&pat.type_predicate);
            match self.pool.claim(env, scrut, sp, explicit)? {
                Claim::Dead if checking => dead(
                    "every combination of heads it matches is covered by earlier arms",
                )?,
                Claim::Completes(m) => self.cover(env, m)?,
                Claim::Dead | Claim::Open | Claim::Unpooled => (),
            }
            if sp.covers(env, at, !pat.explicit_type_predicate) {
                match wild {
                    true => self.atype = self.atype.diff(env, at)?,
                    false => self.cover(env, at.clone())?,
                }
            } else {
                // under a written union, each member the atom covers
                if pat.explicit_type_predicate {
                    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                    union_members(env, at, &mut members)?;
                    if members.len() > 1 {
                        for m in members {
                            if sp.covers(env, &m, false) {
                                self.cover(env, m)?
                            }
                        }
                    }
                }
                if sp.is_array_slice() && sp.array_len_coverage(env, explicit).is_none() {
                    self.refutable_slice = true;
                }
            }
        }
        Ok(())
    }

    /// Nothing reaches past the last arm: an unguarded arm matches
    /// anything, or what the arms cover leaves no case.
    fn exhausted(&self, env: &Env, scrut: &Type) -> Result<()> {
        let empty = Type::Primitive(BitFlags::empty());
        if self.wildcard || self.atype == empty {
            return Ok(());
        }
        let covered: LPooled<Vec<&Type>> = self.covered.iter().collect();
        if Type::union_exact(env, &covered)?.contains(env, scrut)? {
            return Ok(());
        }
        let open = self.ladders.iter().find(|l| !l.complete()).map(|l| (l.rest, l.gap()));
        format_with_flags(PrintFlag::DerefTVars, || {
            let gap = match open {
                None => format_compact!(""),
                Some((None, _)) => format_compact!(
                    " (the slice arms cover finitely many lengths — an array or list \
                     scrutinee also needs a rest pattern or a wildcard)"
                ),
                Some((Some(_), n)) => format_compact!(
                    " (the slice arms leave array length {} uncovered)",
                    n.unwrap_or(0)
                ),
            };
            let refutable = match self.refutable_slice {
                true => {
                    " (a slice arm with refutable element patterns — literals, variants, \
                     nested slices — cannot establish length coverage)"
                }
                false => "",
            };
            let guarded = match self.guarded_slice {
                true => " (a guarded slice arm cannot establish length coverage)",
                false => "",
            };
            Err(anyhow!(
                "missing match cases: no arm covers {}{gap}{refutable}{guarded}",
                self.atype
            ))
        })
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
/// tracked refs. Under [`super::deselecting_arm`] a recursive-edge callee inside
/// the arm is deleted, not retained (`CallSite::sleep`), so unreached
/// activations are shed.
fn deselect<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    tracked: &mut TrackedFires,
    j: usize,
    arm: &mut (PatternNode<R, E>, Node<R, E>),
    sleep: bool,
) {
    if sleep {
        super::deselecting_arm(true, || arm.1.sleep(ctx));
    }
    tracked.refresh(&ctx.env, j, |r| arm_refs(arm, r));
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

/// One position of a composite pattern, keyed by its path into the
/// scrutinee member: the heads the arm admits there (`None` = any
/// value, which also covers every position under the path) and the
/// heads the member admits there (`None` = not a finite set of heads).
struct Leaf {
    path: SmallVec<[u32; 4]>,
    heads: Option<SmallVec<[Head; 2]>>,
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

/// The head a pattern tests at one position, when it tests only a head:
/// a bool literal or a variant whose payload matches anything.
fn head_of(sp: &StructPatternNode) -> Option<Head> {
    match sp {
        StructPatternNode::Literal(Value::Bool(b)) => Some(Head::Bool(*b)),
        StructPatternNode::Variant { tag, all: _, binds }
            if binds.iter().all(|p| p.matches_anything()) =>
        {
            Some(Head::Tag(tag.clone(), binds.len()))
        }
        _ => None,
    }
}

/// The pooled positions of composite pattern `sp` against `m`, the
/// scrutinee member of its shape; `None` when the pattern has a
/// refutable part the pool does not read (another literal, a slice, a
/// type test, a variant with a refutable payload) or tests no head.
fn pooled_leaves(
    env: &Env,
    sp: &StructPatternNode,
    m: &Type,
) -> Result<Option<SmallVec<[Leaf; 8]>>> {
    fn leaf(
        env: &Env,
        sp: &StructPatternNode,
        t: &Type,
        path: &mut SmallVec<[u32; 4]>,
        out: &mut SmallVec<[Leaf; 8]>,
    ) -> Result<bool> {
        let admitted = match sp {
            StructPatternNode::Bind(_) | StructPatternNode::Ignore => None,
            StructPatternNode::Slice { kind: SliceKind::Tuple, .. }
            | StructPatternNode::Struct { .. } => {
                return crate::stack::ensure_sufficient(|| {
                    children(env, sp, t, path, out)
                });
            }
            StructPatternNode::Or { alts } => {
                match alts.iter().map(head_of).collect::<Option<SmallVec<[Head; 2]>>>() {
                    Some(hs) => Some(hs),
                    None => return Ok(false),
                }
            }
            sp => match head_of(sp) {
                Some(h) => Some(SmallVec::from_iter([h])),
                None => return Ok(false),
            },
        };
        out.push(Leaf { path: path.clone(), heads: admitted, domain: heads(env, t)? });
        Ok(true)
    }
    fn children(
        env: &Env,
        sp: &StructPatternNode,
        t: &Type,
        path: &mut SmallVec<[u32; 4]>,
        out: &mut SmallVec<[Leaf; 8]>,
    ) -> Result<bool> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, t, &mut members)?;
        let [m] = &members[..] else { return Ok(false) };
        let mut at = |i: usize, p: &StructPatternNode, t: &Type| -> Result<bool> {
            path.push(i as u32);
            let r = leaf(env, p, t, path, out);
            path.pop();
            r
        };
        match (sp, m) {
            (
                StructPatternNode::Slice { kind: SliceKind::Tuple, all: _, binds },
                Type::Tuple(ts),
            )
            | (
                StructPatternNode::Variant { tag: _, all: _, binds },
                Type::Variant(_, ts, _),
            ) if ts.len() == binds.len() => {
                for (i, (p, t)) in binds.iter().zip(ts.iter()).enumerate() {
                    if !at(i, p, t)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (
                StructPatternNode::Abstract { id, bind, rep, .. },
                Type::Abstract { id: t, .. },
            ) if id == t => at(0, bind, rep),
            (StructPatternNode::Struct { all: _, binds }, Type::Struct(fs)) => {
                for (name, _, p) in binds.iter() {
                    let Some(i) = fs.iter().position(|(n, _, _)| n == name) else {
                        return Ok(false);
                    };
                    if !at(i, p, &fs[i].1)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }
    let mut out = SmallVec::new();
    let pooled = children(env, sp, m, &mut SmallVec::new(), &mut out)?
        && out.iter().any(|l| l.heads.is_some());
    Ok(pooled.then_some(out))
}

/// The head a composite pattern tests: what groups arms in the
/// literal pool and selects the scrutinee member they cover.
#[derive(PartialEq, Eq)]
enum Shape {
    Tuple(usize),
    Variant(ArcStr, usize),
    Struct(SmallVec<[ArcStr; 8]>),
    Abstract(AbstractId),
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
            StructPatternNode::Abstract { id, .. } => Some(Shape::Abstract(*id)),
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
            (Shape::Abstract(id), Type::Abstract { id: t, .. }) => id == t,
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

/// What pooling an atom decided.
enum Claim {
    /// The pool does not read it.
    Unpooled,
    /// Its group does not cover its member yet.
    Open,
    /// It completes its group: the member it covers.
    Completes(Type),
    /// Every combination of heads it matches, earlier arms match.
    Dead,
}

impl LiteralPool {
    /// Pool the unguarded atom `sp`; `explicit`, its arm's written type
    /// test.
    fn claim(
        &mut self,
        env: &Env,
        scrut: &Type,
        sp: &StructPatternNode,
        explicit: Option<&Type>,
    ) -> Result<Claim> {
        let Some(shape) = Shape::of(sp) else { return Ok(Claim::Unpooled) };
        let Some(m) = shape.member(env, scrut)? else { return Ok(Claim::Unpooled) };
        // under a type test that does not hold the member, the atom covers
        // none of it
        if let Some(t) = explicit
            && !t.contains_with_flags(BitFlags::empty(), env, &m)?
        {
            return Ok(Claim::Unpooled);
        }
        let Some(leaves) = pooled_leaves(env, sp, &m)? else {
            return Ok(Claim::Unpooled);
        };
        let i = match self.groups.iter().position(|g| g.shape == shape) {
            Some(i) => i,
            None => {
                self.groups.push(PoolGroup { shape, arms: Vec::new(), done: false });
                self.groups.len() - 1
            }
        };
        let g = &mut self.groups[i];
        if g.done {
            return Ok(Claim::Open);
        }
        if !g.arms.is_empty() && g.covers(&leaves) {
            return Ok(Claim::Dead);
        }
        g.arms.push(leaves);
        g.done = g.covers_all();
        Ok(match g.done {
            true => Claim::Completes(m),
            false => Claim::Open,
        })
    }
}

/// Does `arm` match head `h` at `path`: a leaf there admitting it, or
/// any-value leaf at a path above it.
fn admits(arm: &[Leaf], path: &[u32], h: &Head) -> bool {
    arm.iter().any(|l| match &l.heads {
        None => path.starts_with(&l.path),
        Some(hs) => l.path[..] == *path && hs.contains(h),
    })
}

impl PoolGroup {
    /// The positions some arm (or `extra`) tests a head at, with the
    /// heads the member admits there; `None` when one is not a finite
    /// set, or there are too many combinations.
    fn positions<'a>(
        &'a self,
        extra: Option<&'a [Leaf]>,
    ) -> Option<SmallVec<[(&'a [u32], &'a [Head]); 8]>> {
        let mut out: SmallVec<[(&[u32], &[Head]); 8]> = SmallVec::new();
        let mut total = 1usize;
        let arms = self.arms.iter().map(|a| &a[..]).chain(extra);
        for l in arms.flat_map(|a| a.iter()).filter(|l| l.heads.is_some()) {
            if out.iter().any(|(p, _)| *p == &l.path[..]) {
                continue;
            }
            let domain = l.domain.as_deref()?;
            total = total.saturating_mul(domain.len());
            out.push((&l.path[..], domain));
        }
        (total <= MAX_POOL_COMBINATIONS).then_some(out)
    }

    /// Every combination of heads `positions` names, as head references.
    fn combinations<'a>(
        positions: &'a [(&'a [u32], &'a [Head])],
    ) -> impl Iterator<Item = SmallVec<[&'a Head; 8]>> + 'a {
        let total = positions.iter().map(|(_, d)| d.len()).product::<usize>();
        (0..total).map(move |mut c| {
            positions
                .iter()
                .map(|(_, domain)| {
                    let h = &domain[c % domain.len()];
                    c /= domain.len();
                    h
                })
                .collect()
        })
    }

    fn matches(arm: &[Leaf], positions: &[(&[u32], &[Head])], combo: &[&Head]) -> bool {
        positions.iter().zip(combo.iter()).all(|((p, _), h)| admits(arm, p, h))
    }

    /// Some arm matches every combination the member admits.
    fn covers_all(&self) -> bool {
        let Some(positions) = self.positions(None) else { return false };
        Self::combinations(&positions)
            .all(|c| self.arms.iter().any(|a| Self::matches(a, &positions, &c)))
    }

    /// Earlier arms match every combination `arm` matches.
    fn covers(&self, arm: &[Leaf]) -> bool {
        let Some(positions) = self.positions(Some(arm)) else { return false };
        Self::combinations(&positions)
            .filter(|c| Self::matches(arm, &positions, c))
            .all(|c| self.arms.iter().any(|a| Self::matches(a, &positions, &c)))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Select<R, E> {
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
        self.spec.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
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
            consulted_guard_mask,
            resident,
            slept,
            arm_facts,
        } = self;
        let LazyArmFacts { tracked, sleep_on_deselect, shallow, logged_no_match } =
            arm_facts.as_mut().expect("arm facts built above");
        let woke = slept.take();
        // Per-arm guard production tags; `None` = unguarded. Only guards
        // the chain consults contribute fires or bottomness.
        let mut guard_tags: SmallVec<[Option<Tag>; 32]> =
            SmallVec::with_capacity(arms.len());
        let arg_prod = arg.update(ctx);
        tracked.observe(ctx);
        // Guards are live nodes and tick every cycle, even under a
        // tainted scrutinee. The bind is delivered only to an arm whose
        // shape admits the value: the checker narrowed the binds by that
        // shape, and a guard fused to the narrowing must never see the
        // value an earlier arm claimed.
        for ((pat, _), shallow) in arms.iter_mut().zip(shallow.iter()) {
            let saved = match arg.value.as_ref() {
                Some(v)
                    if !arg.tag.is_bottom()
                        && pat.guard.is_some()
                        && pat.shape_matches(&ctx.env, shallow.as_ref(), v) =>
                {
                    Some(pat.bind_tentative(ctx, v, arg_prod))
                }
                _ => None,
            };
            guard_tags.push(pat.update(ctx));
            if let Some(saved) = saved {
                pat.retract(ctx, saved);
            }
        }
        // Any guard fire drives a re-match; whether it affects the
        // emission is decided by the consulted set below.
        let pat_up = guard_tags.iter().any(|t| t.is_some_and(|t| t.triggers()));
        // A bottom scrutinee bottoms the select; it consults no guards.
        // A window with no arm (a bottom scrutinee, an undecidable guard)
        // pauses the selected arm as a deselect does, so it resumes with
        // catch-up; a wake that lands in it waits for the next arm.
        if arg.tag.is_bottom() {
            if woke {
                slept.set();
            }
            if let Some(j) = selected.take() {
                deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect[j]);
            }
            return resident.set_bottom(arg_prod.triggers());
        }
        let v = arg.value.as_ref().expect("Held keeps every non-bottom production");
        if crate::dbgenv::graphix_dbg_select() {
            eprintln!(
                "SELECT[{}] upd init={} pat_up={pat_up} sel={selected:?} argc={v:?} vars={}",
                spec.pos,
                ctx.event.init(),
                ctx.event.variables.len()
            );
        }
        enum ChainOut {
            Quiet(usize),
            Taken(Option<usize>),
            Undet,
        }
        let chain = match *selected {
            Some(i) if !(arg_prod.triggers() || pat_up || woke) => ChainOut::Quiet(i),
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
        // A selection stays undecidable while a consulted guard stands
        // bottom: no arm is evaluated.
        let chain = match chain {
            ChainOut::Quiet(_) if planes.consulted_bottom => ChainOut::Undet,
            chain => chain,
        };
        let tv = match chain {
            ChainOut::Undet => {
                if woke {
                    slept.set();
                }
                if let Some(j) = selected.take() {
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect[j]);
                }
                return resident.set_bottom(planes.anyfire);
            }
            ChainOut::Quiet(i) => {
                let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) if *selected == Some(i) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!("SELECT[{}] same-arm i={i} arg={v:?}", spec.pos);
                }
                arms[i].0.bind_event(ctx, v, arg_prod);
                let (t, v) = evaluate_arm(tracked, &mut arms[i].1, i, ctx);
                planes.emit(t, v)
            }
            ChainOut::Taken(Some(i)) => {
                if crate::dbgenv::graphix_dbg_select() {
                    eprintln!(
                        "SELECT[{}] BECOMING-SELECTED {selected:?} -> {i} init={}",
                        spec.pos,
                        ctx.event.init()
                    );
                }
                if let Some(j) = selected.replace(i) {
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect[j]);
                }
                // A stale scrutinee binds stale, a past event at a first consult
                // or a wake alike; only a consulted guard's flip binds FIRED, and
                // under an init view a guard's fire is its birth, not a flip.
                let bind_tag = if arg_prod.triggers() {
                    arg_prod
                } else if planes.guard_fire && !ctx.event.init() && !woke {
                    Tag::FIRED
                } else {
                    Tag::STALE
                };
                arms[i].0.bind_event(ctx, v, bind_tag);
                // An arm that sleeps on deselect enters under the wake
                // view, its first selection included; a pure
                // non-recursive arm, with nothing to pause, enters under
                // `init` alone.
                let view = if sleep_on_deselect[i] { View::Wake } else { View::Birth };
                let (t, v) =
                    ctx.under(view, |ctx| evaluate_arm(tracked, &mut arms[i].1, i, ctx));
                planes.emit(t, v)
            }
            // No arm matches: the select has no value.
            ChainOut::Taken(None) => {
                // exhaustiveness makes this a checker bug: say so, once
                if !std::mem::replace(logged_no_match, true) {
                    crate::node::error::report_coverage_hole(&format_args!(
                        "{spec}: no arm matches {v}"
                    ));
                }
                if let Some(j) = selected.take() {
                    deselect(ctx, tracked, j, &mut arms[j], sleep_on_deselect[j]);
                }
                return resident.set_bottom(false);
            }
        };
        resident.set(tv)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
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

    fn refs(&self, refs: &mut Refs) {
        let Self {
            selected: _,
            arg,
            arms,
            typ: _,
            spec: _,
            consulted_guard_mask: _,
            resident: _,
            slept: _,
            arm_facts: _,
        } = self;
        arg.node.refs(refs);
        for arm in arms {
            arm_refs(arm, refs);
            if let Some(n) = &arm.0.guard {
                n.node.refs(refs);
            }
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        match types.aux(self.spec.id) {
            Some(rows) => self.typecheck0_rows(ctx, types, &rows),
            None => self.typecheck0_with(
                ctx,
                &mut |n, ctx| n.typecheck0_instance(ctx, types),
                false,
            ),
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
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

impl<R: Rt, E: UserEvent> Select<R, E> {
    /// What the check derived for the arms ([`super::lambda::DefTable`]):
    /// each arm's completed predicate, then the types of its pattern's
    /// binds in [`StructPatternNode::ids`] order.
    pub(crate) fn aux_types(&self, env: &Env) -> Box<[Type]> {
        let mut out = Vec::new();
        for (pat, _) in self.arms.iter() {
            out.push(pat.type_predicate.clone());
            pat.structure_predicate.ids(&mut |id| {
                out.push(env.by_id.get(&id).map_or(Type::Bottom, |b| b.typ.clone()))
            });
        }
        out.into_boxed_slice()
    }

    /// An instance's typecheck from the rows its definition's check
    /// recorded: the arms' predicates and binds are read, not narrowed.
    fn typecheck0_rows(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
        rows: &[Type],
    ) -> Result<()> {
        wrap!(self.arg.node, self.arg.node.typecheck0_instance(ctx, types))?;
        let mut rows = rows.iter();
        for (pat, n) in self.arms.iter_mut() {
            let tp = rows.next().ok_or_else(|| anyhow!("BUG: a select row short"))?;
            // the predicate's cells are its binds' (an inferred predicate
            // is built from them), bound by position before it is replaced
            pat.type_predicate.take_row(tp);
            match pat.explicit_type_predicate {
                // the type test reads the names it writes at run time, where
                // the body's typedefs are gone: resolve them here
                true => {
                    pat.type_predicate.seed_refs(&ctx.env);
                }
                false => {
                    pat.structure_predicate.realign(&ctx.env, tp).at(n.spec())?;
                    pat.type_predicate = tp.clone();
                }
            }
            let mut ids: SmallVec<[BindId; 4]> = SmallVec::new();
            pat.structure_predicate.ids(&mut |id| ids.push(id));
            for id in ids {
                let row =
                    rows.next().ok_or_else(|| anyhow!("BUG: a select row short"))?;
                if let Some(b) = ctx.env.by_id.get(&id) {
                    b.typ.take_row(row);
                }
            }
            if let Some(guard) = &mut pat.guard {
                wrap!(guard.node, guard.node.typecheck0_instance(ctx, types))?;
            }
            wrap!(n, n.typecheck0_instance(ctx, types))?;
        }
        let rtypes: LPooled<Vec<&Type>> =
            self.arms.iter().map(|(_, n)| n.typ()).collect();
        self.typ = Type::union_of_alternatives(&ctx.env, &rtypes)?;
        Ok(())
    }

    /// `checking`: the definition's check, which also judges coverage,
    /// dead arms and guards; an instance does only what they decide.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        checking: bool,
    ) -> Result<()> {
        wrap!(self.arg.node, child(&mut self.arg.node, ctx))?;
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
                        .at(&pattern_site(spec_pat, body.spec()))?;
                    if let Some(t) = t {
                        pat.structure_predicate.realign(&ctx.env, &t)?;
                        pat.type_predicate = t;
                    }
                }
            }
        }
        let scrut = self.arg.node.typ().clone();
        let mut reach = Reach::new(&ctx.env, &scrut, &self.arms)?;
        let mut rtypes: LPooled<Vec<&Type>> = LPooled::take();
        for (pat, n) in self.arms.iter_mut() {
            // The arm's binds alias against what reaches it. The
            // `any_as_tvar` view keeps a `_` slot from short-circuiting the
            // walk.
            let reaching = reach.atype.clone();
            let narrowed = pat.type_predicate.any_as_tvar();
            reaching.contains(&ctx.env, &narrowed)?;
            pat.bind_narrowed(&ctx.env, &narrowed, checking).at(n.spec())?;
            // a runtime test can't tell apart two types with one runtime form
            if checking
                && let Ok(rest) = reaching.diff(&ctx.env, &pat.type_predicate)
                && let Some((a, b)) = pat.type_predicate.rep_collision(&ctx.env, &rest)
            {
                let (a, b) = (a.resolve_tvars(), b.resolve_tvars());
                return Err(format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!(
                        "this pattern can't tell {a} from {b}: both have the same \
                         runtime form; wrap them in distinct variants"
                    )
                }))
                .at(n.spec());
            }
            // The guard typechecks after the narrowing so it sees the
            // arm's binds at their settled type; it must be bool.
            if let Some(guard) = &mut pat.guard {
                wrap!(guard.node, child(&mut guard.node, ctx))?;
                if checking {
                    let bt = Type::Primitive(Typ::Bool.into());
                    wrap!(guard.node, bt.check_contains(&ctx.env, guard.node.typ()))?;
                }
            }
            wrap!(n, child(n, ctx))?;
            rtypes.push(n.typ());
            reach.arm(&ctx.env, &scrut, pat, n.spec(), checking)?;
        }
        self.typ = Type::union_of_alternatives(&ctx.env, &rtypes)?;
        drop(rtypes);
        match checking {
            true => {
                check_repeats(&ctx.env, &self.arms)?;
                reach.exhausted(&ctx.env, &scrut).at(&self.spec)
            }
            false => Ok(()),
        }
    }
}
