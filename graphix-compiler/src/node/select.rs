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
    pub(crate) spec: Arc<Expr>,
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
            // CR claude for claude: [bug/medium] The tracker is built from each arm's
            // refs at the first update, and a dynamic call site's refs are only its
            // function node and arguments. So a variable captured by the closure the
            // site will dispatch (in1 in `|x| count(in1) + x`) is untracked until the
            // arm has been selected and deselected once (`deselect` refreshes it). A
            // fire of that variable while a never-selected arm sleeps is lost at the
            // arm's first selection. The static twin `g(0)` catches it up, and so does
            // the same dynamic arm at its second selection. The outcome depends on
            // dispatch kind and history, which breaks the wake catch-up invariant (each
            // event influences the arm exactly once). Fix: make the tracked set cover
            // what a dynamic site can reach before dispatch, e.g. by tracking the free
            // refs of the lambda value currently delivered to the site's function node
            // (re-read when that value changes). Pin the static/dynamic first-selection
            // pair. x-engine-firing-05 (function value bottom while the capture fires)
            // has the same root. probe:
            // design/review-2026-10-12/repro/x-engine-firing-03.sh (fuzz-schedule form:
            // x-engine-firing-01.gx) (x-engine-firing-03)
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
        spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
        Ok(Self::new(arg, arms, typ, spec))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
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

// CR claude for claude: [readability/low] The doc comment that states this function's
// rule (a refutable atom that tests what an earlier unguarded one does, under the same
// type test, matches nothing new) sits on `type_test` above, a one-line accessor for
// the arm's written type test. `check_repeats` has no doc. A reader of `type_test` gets
// a dead-arm rule, and the refusal is not explained where it is made. Move the comment
// onto `check_repeats`. Either give `type_test` no comment (its body says what it does)
// or inline it into its one caller here. (c-select-seq-03)
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
/// the last arm. Quadratic in the arm count (each arm re-walks what still
/// reaches; 4000 tag arms check in about 2 s): member sets indexed by
/// head would make it linear.
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
        // an arm that matches anything before narrowing steers only the
        // settle: narrowing may bind its cells to one member
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
            wildcard: false,
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
        self.wildcard |= wild && unguarded;
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
                // CR claude for eric: [bug/low] A slice arm claims its length on the
                // ladder only when array_len_coverage holds, which means every element
                // pattern is irrefutable. Element literals and variant heads are never
                // pooled per length. `[] => .., [true] => .., [false] => .., [_, _, ..]
                // => ..` over Array<bool> is refused with "the slice arms leave array
                // length 1 uncovered", and so are `[(true, x)]` and `[(false, x)]`.
                // CLAUDE.md says bool literals and variant heads pool per position
                // inside composite patterns, and says nothing about this exception.
                // Choose one: (a) pool each exact-length slice arm as a fixed-arity
                // group keyed by (kind, len), the way Shape::Tuple is, and claim that
                // length on the ladder when its group completes; or (b) state the limit
                // in CLAUDE.md's Sets and coverage section. probe:
                // design/review-2026-10-12/repro/x-typecheck-patterns-07.gx
                // (x-typecheck-patterns-07)
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
        // CR claude for claude: [readability/low] The hint comes from the first
        // incomplete ladder, even one no slice arm ever claimed. With only type-test
        // arms (`Array<i64> as x`, `Array<string> as y` over Array<[i64, string]>), the
        // refusal still says "the slice arms cover finitely many lengths — ... also
        // needs a rest pattern". Over [Array<i64>, Array<string>], where the `[x, ..]`
        // arm was narrowed to Array<i64>, it asks for a rest pattern the select already
        // has. Fix: give the hint only for a ladder that some arm claimed (non-empty
        // exacts or a rest), and name that ladder's member in it, e.g. "the slice arms
        // cover finitely many lengths of Array<string>". probe:
        // design/review-2026-10-12/repro/x-typecheck-patterns-08.gx
        // (x-typecheck-patterns-08)
        // CR claude for claude: [bug/low] `open` takes the first incomplete ladder, but
        // a ladder exists for every array or list member of the scrutinee, including
        // one a type-test arm (`Array<i64> as a`) covers whole. So a select with no
        // slice arm at all, missing only `null`, is refused with '(the slice arms cover
        // finitely many lengths — ... needs a rest pattern or a wildcard)', which sends
        // the user after patterns they never wrote. Fix: consider only a ladder some
        // arm claimed (exacts or a rest) whose member the uncovered remainder
        // `self.atype` can still match, and otherwise add no gap text. Probe:
        // design/review-2026-10-12/repro/book-core-11.gx. Also reported as
        // (x-rerun-5-07). (book-core-11)
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
            // CR claude for claude: [bug/low] Shape::Struct holds only the pattern's
            // named fields, and Shape::fits needs them to equal the member's full field
            // list. So a partial struct pattern (`{a: true, ..}`) finds no member and
            // LiteralPool::claim returns Unpooled. `{a: true, ..}` and `{a: false, ..}`
            // over {a: bool, b: i64} are refused with "missing match cases", although
            // CLAUDE.md says bool literals pool per position inside composite patterns.
            // The same arms with every field named are accepted, and so are variant
            // heads in the partial pattern. Fix: key a struct group by the scrutinee
            // member it fits, where fits means the pattern's names are a subset of the
            // member's fields. Then arms naming different subsets share one group.
            // pooled_leaves::children already places each named field by its index in
            // the member. probe:
            // design/review-2026-10-12/repro/x-typecheck-patterns-06.gx
            // (x-typecheck-patterns-06)
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
            // CR claude for claude: [bug/medium] c-select-seq-06 still reproduces for
            // partial struct patterns: they count for exhaustiveness only. fe898ea9 says
            // one walk (Reach) decides narrowing, dead arms and exhaustiveness. For partial
            // struct patterns the three still disagree. `Shape::of` keys a struct pattern
            // by its bound field names (select.rs), and `Shape::fits` (886-890) requires
            // those names to equal the member's full field list. So `{d: `A, ..}` over `{c:
            // string, d: [`A, `B]}` never pools (Claim::Unpooled), and `Reach.atype` keeps
            // the struct member. `Reach::exhausted` still passes through its second test,
            // `union_exact(covered).contains(scrut)`, which the arms' `sp.covers` atoms
            // satisfy. Net effect: the select is exhaustive, but a later bind is not
            // narrowed, and an arm after full coverage is not reported dead. This is the
            // struct case the old c-select-seq-06 repro lists. Writing every field (`{d:
            // `A, c}`) works. Failure: `type S = {c: string, d: [`A, `B]}; let s: [i64, S]
            // = 3; select s { {d: `A, ..} => 10, {d: `B, ..} => 20, n => n + 1 }` is
            // refused with "cannot compute [i64, S] + i64": n is not narrowed. `select s {
            // {d: `A, ..} => 10, {d: `B, ..} => 20 }` over `s: S` is accepted as
            // exhaustive, yet appending `_ => 0` is accepted too, not refused as
            // unreachable (the tuple analogue is refused). Over `[S, null]`, `v => select v
            // { null => 0 }` after the two arms is refused: "no arm covers S". Fix: Make
            // Shape::Struct for a partial pattern fit any member that has its named fields:
            // pool against the member and add Ignore leaves for the unnamed fields.
            // Alternatively, have `Reach::cover` subtract what `covered` proves, so
            // exhaustiveness, narrowing and dead arms read one set. Add a
            // lang::select::coverage_* pin with a partial struct pool followed by a bind.
            // Unverified: the reviewer's report, not checked by a verifier. (x-rerun-2-04)
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.arg.node);
        for (pat, body) in self.arms.iter() {
            if let Some(g) = &pat.guard {
                f(&g.node)
            }
            f(body)
        }
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.arg.node);
        for (pat, body) in self.arms.iter_mut() {
            if let Some(g) = &mut pat.guard {
                f(&mut g.node)
            }
            f(body)
        }
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
        // CR claude for claude: [perf/low] guard_tags spills to the heap on every
        // update of a select with more than 32 arms. Then every arm's pat.update runs,
        // which returns None for an unguarded arm, and EmissionPlanes::new walks the
        // result again. All of this happens on quiet cycles too, so a large keymap or
        // state-machine select pays O(arms) plus an allocation per event. An idle
        // 1600-arm unguarded select costs about 107 us per unrelated cycle against 22
        // us at 100 arms (probe: design/review-2026-10-12/repro/x-perf-runtime-05.py
        // with `s`). Precompute the guarded arms' indices in LazyArmFacts and tick only
        // those. Record their tags sparsely or as bitsets beside consulted_guard_mask,
        // so an all-unguarded select does no guard work and allocates nothing.
        // (x-perf-runtime-05)
        let mut guard_tags: SmallVec<[Option<Tag>; 32]> =
            SmallVec::with_capacity(arms.len());
        let arg_prod = arg.update(ctx);
        tracked.observe(ctx);
        // Guards are live nodes and tick every cycle, even under a
        // tainted scrutinee. The bind is delivered only to an arm whose
        // shape admits the value: the checker narrowed the binds by that
        // shape, and a guard fused to the narrowing must never see the
        // value an earlier arm claimed.
        // CR claude for eric: [bug/high] This loop binds the scrutinee into every
        // guarded arm whose own shape admits the value, including arms below one that
        // already claimed it with a literal or a true guard. So a guard the chain never
        // consults still runs over that value, and its effects happen. Examples:
        // `select x { -1 => .., n if check(n)? => .., _ => .. }` raises to the
        // enclosing catch each time x is -1, and `select x { 0 => .., n if 10 / n > 5
        // => .., _ => .. }` logs "attempt to divide by zero". The design's own Ruling
        // 1a example (`[x, y] => .., [x, y, tail..] if x / y == 0 => ..` over a pair
        // with y = 0) logs every cycle too. The kernel's guard prologue in
        // `emit_select_chain` does the same deliberately, so both engines agree
        // (graphix-fuzz check AGREE), and the comment above this loop states an
        // invariant that does not hold. Ruling needed: a guard below the claim point
        // either keeps ticking with no bind delivered, which needs a claimed flag that
        // follows the consult chain here and a not-yet-claimed mask on the prologue's
        // binds in the kernel, or its effects are declared part of the semantics and
        // design/activation_state.md says so. probe:
        // design/review-2026-10-12/repro/c-select-seq-01.gx (and c-select-seq-01b.gx
        // with --no-fusion). (c-select-seq-01)
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
                // CR claude for claude: [bug/medium] An arm's tracked inputs come from
                // `TrackedFires::new` at the select's first update and are refreshed
                // only in `deselect`. `Deref::refs` names its target only after the
                // Deref's first update. So an arm selected for the first time reads
                // `*r`'s target live without consuming that target's fire bit. The bit
                // stays pending, and the other arm gets a phantom FIRED catch-up when
                // it wakes. In the probe, cnt ticks at the wake although the false arm
                // already read b. Fix: refresh the arm's tracked refs
                // (`tracked.refresh(&ctx.env, i, ..)`) right after its first evaluation
                // here, and do the same in seq_machine, which builds its tracker the
                // same way. A moving reference that changes target later needs a
                // refresh when a Deref's address moves, or its reads keyed under the
                // reference's own cell. probe:
                // design/review-2026-10-12/repro/c-bind-06.gx (c-bind-06)
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
        // CR claude for claude: [bug/low] Every arm's partial struct pattern is
        // completed against the whole scrutinee type, before Reach narrows anything.
        // StructurePattern::unique_completion therefore counts members that an earlier
        // unguarded arm has already consumed. Example: `select v { {x, y} => x, s@ {x,
        // ..} => x + 10 }` over `[{x: i64, y: string}, {x: i64, z: f64}]` is refused
        // with "matches more than one member", although only the second member reaches
        // arm 2. The same happens in nested positions (`` `A({x, ..}) ``, `Q(s@ {x,
        // ..})`), where the suggested `T as` annotation cannot be written. Fix:
        // complete each arm against `reach.atype` as the walk reaches it. Reach::new
        // reads every arm's type_predicate, so one way is to keep this pass, defer an
        // arm whose completion is ambiguous here, and complete it again inside the arm
        // loop against what reaches it. After that, reword the ambiguity rule in
        // book/src/core/select.md to say "members that reach the arm". probe:
        // design/review-2026-10-12/repro/t-parser-b-03.gx (t-parser-b-03)
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
            // CR claude for claude: [bug/high] When what reaches an arm is a union of
            // several members with the same shape, this containment binds an inferred
            // predicate's open cells to ONE member. Example: `(x, y)` over `[(i64,
            // string), (string, i64)]` becomes `(i64, string)`. `Reach::new` has
            // already counted the arm as a wildcard (`matches_every` over the unbound
            // predicate), so the check accepts the select, but the runtime type test
            // admits only that member. The other member's values match no arm in both
            // engines ("no arm matches ... a coverage hole in the check"), and an
            // enclosing `array::map` produces nothing. The struct form `{x, y}` fails
            // the same way. The variant form `` `A(x, y) `` and abstract and slice
            // patterns are refused instead, with a wrong "missing match cases" message.
            // Fix: narrow an inferred predicate whose structure matches anything to the
            // union of the reaching members its shape covers, not to one unified
            // member. Then compute the wildcard from the narrowed predicate in
            // `Reach::arm`, so coverage and the runtime test read the same type. probe:
            // design/review-2026-10-12/repro/x-typecheck-patterns-02.gx
            // (x-typecheck-patterns-02)
            // CR claude for claude: [bug/high] Over a scrutinee that is a union of
            // collections or tuples, this contains binds a slice's element cells, or a
            // tuple's position cells, to the first reaching member. Those cells are
            // shared with the arm's run-time type predicate, so the arm then rejects
            // every other member. `[x]` over [Array<i64>, Array<string>] sends ["s"] to
            // the next arm in both engines (`[_]` matches it). `[h, rest..]`/`[]`, the
            // List analogue, and `(a, b)` with a null arm are refused as
            // non-exhaustive, because Reach::arm's ladder claim and coverage see only
            // the first member. Fix: bind each element/position cell to the union of
            // its parts over every reaching member the pattern's shape matches, keep
            // the run-time predicate admitting all of them, and make Reach::arm claim
            // every matching member's ladder. Same cause as the capture finding
            // c-pattern-03. probe: design/review-2026-10-12/repro/c-pattern-04.gx
            // (c-pattern-04)
            // CR claude for claude: [bug/high] When `reaching` is a union, `Set ⊇
            // narrowed` commits the predicate's cells to the first member that admits
            // them (`set_commit`). So the arm's binds are typed from one member while
            // its run-time test admits every member. Over `p: [(i64, string), (string,
            // i64)]`, `q@ (_, _) => q.0` gives q the type (i64, string). `let r: i64 =
            // g(("s", 1))` then checks, the node-walk yields "s" and the JIT yields 0
            // (probe: design/review-2026-10-12/repro/c-pattern-03.gx; with `q.0 + 1`,
            // graphix-fuzz check reports a DIVERGENCE). The same commit hits a cell
            // inside the arm's own predicate: `(a, _) => a` over that union passes the
            // exhaustiveness check, but its run-time predicate becomes (i64, _), and
            // ("s", 1) reaches "no arm matches: a coverage hole in the check". Fix:
            // narrow per reaching member. For each member the predicate could match,
            // compute that member's part on a copy, then bind each capture and each
            // predicate cell to the union of those parts, instead of letting one
            // `contains` over the Set commit to a single member. (c-pattern-03)
            reaching.contains(&ctx.env, &narrowed)?;
            pat.bind_narrowed(&ctx.env, &narrowed, checking).at(n.spec())?;
            // a runtime test can't tell apart two types with one runtime form
            // CR claude for claude: [bug/high] x-unsafe-03/c-pattern-02 still open:
            // nested constructor patterns ignore the one-runtime-form rule. Select
            // checks one-runtime-form collisions only for an arm's whole predicate
            // against the rest of the scrutinee (select.rs). The nested check in
            // StructPatternNode::compile (pattern.rs) runs only when `!cx.inferred`. A
            // constructor pattern nested in a tuple, struct field, slice element, List
            // element or variant payload therefore tests only the shape: over `U =
            // [(string, string), `S(i64)]`, `[`S(n)]` matches the tuple ("S", "x") and
            // binds `n: i64` to the string "x". Commit 613864c3 closed x-unsafe-03 by
            // hardening buffer::encode against the bad payload; the root cause was left
            // in place. At top level the same test correctly takes the other arm.
            // Failure: For p = ("S", "x"), every nested position takes the `S arm: the
            // output is ("S x", "S x", "S x", "S x", "S x") instead of "other" five
            // times. Passing n to `|n: i64| n * 2` makes the JIT panic the runtime:
            // "kernel param `n`: runtime String(\"x\") does not match the compiled
            // Scalar(I64) slot". The node-walk logs an arith error and bottoms. Fix:
            // Run the rep_collision test for every nested constructor position, not
            // only the top-level predicate. Either drop the `!cx.inferred` restriction
            // in pattern.rs, so an inferred nested predicate is completed against its
            // position's type and checked, or have Reach walk sub-positions. Pin each
            // position kind. Unverified: the reviewer's report, not checked by a
            // verifier. (x-rerun-8-01)
            // CR claude for eric: [bug/medium] The one-runtime-form check runs only in
            // the definition's check, and Type::rep_collision reads an open cell as
            // Open::Benign. An instance never re-checks (typecheck0_instance), so a
            // generic select escapes the rule. `|x: 'a| select x { string as s => .., v
            // => .. }` applied to `A takes the string arm. `|x: [(string, i64), 'a]|
            // select x { (s, n) => .., v => .. }` applied to `A(41) destructures the
            // variant as a tuple. Both engines give these wrong answers, and the check
            // accepts them. The comparisons refuse the same types at the definition
            // (Open::Unknown through PendingSettle::Discernible), so the two halves of
            // the rule disagree. A fix needs a rule: either refuse an open member here
            // as the comparisons do ("annotate it"), or give the select's open cells a
            // deferred constraint, like the Discernible bound, so each call checks its
            // substitution against the arm's predicate. The second also has to cross
            // interface vals. probe:
            // design/review-2026-10-12/repro/x-typecheck-patterns-04.gx
            // (x-typecheck-patterns-04)
            // CR claude for claude: [bug/high] The checker accepts a type test that
            // names a reference over an `Any` scrutinee. In `let b: Any = n; select b {
            // &mut string as q => *q <- "boom", _ => .. }`, any u64 passes the test and
            // becomes a writable reference to whatever variable has that id. The probe
            // writes a string into `x: i64`. The JIT then panics in kernel.rs ("runtime
            // String does not match the compiled Scalar(I64) slot") and the runtime
            // dies. The node-walk keeps running with an i64 binding that holds a
            // string. The cause is that is_a's ByRef arm only checks for a U64/V64, and
            // this rep_collision check never fires when the rest is `Any`, because
            // rep_head(Any) is None and the walk runs under Open::Benign. The same arm
            // over `[u64, &mut string]` is correctly refused. Fix: refuse an explicit
            // predicate that holds a reference (Type::holds_ref) wherever the reaching
            // type can hold `Any` at that position, with the "same runtime form"
            // wording. Keep allowing a reference test over a scrutinee whose type
            // already pins the reference (the byref_pattern pin and the `[&mut T,
            // null]` optional-argument idiom), so do not refuse every reference
            // predicate the way pattern.rs refuses functions. Pin it beside
            // unsound_acceptances_are_refused, with both the top-level form and the
            // form nested in a tuple. Same root as x-invalid-states-01. probe:
            // design/review-2026-10-12/repro/x-fix-types-03.gx (x-fix-types-03)
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
                // CR claude for claude: [readability/low] This refusal is about the
                // arm's pattern but is placed at the arm's body, so the CLI caret and
                // the LSP diagnostic point at the body (`1`) and not at `&i64 as r`.
                // The `pat.bind_narrowed(..).at(n.spec())` above has the same problem.
                // Fix: zip the arms with `se.arms`, as the predicate-completion loop
                // above does, and place both errors with `.at(&pattern_site(spec_pat,
                // n.spec()))`. probe:
                // design/review-2026-10-12/repro/x-typecheck-generics-09.gx
                // (x-typecheck-generics-09)
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
            // CR claude for claude: [readability/low] The site passed here is the arm's
            // body, so every 'unreachable arm' and 'pattern ... will never match'
            // refusal points at the body. In `select a { [x, tl..] => 1, [init.., y] =>
            // 2, [] => 0 }` the error is at the `2`, and the LSP underlines that
            // instead of the dead `[init.., y]`. The parsed expr::Pattern already
            // records its `pos`/`end`. Fix: carry that span into PatternNode and use it
            // as the error site for the dead-arm and never-match refusals, falling back
            // to the body when the span is NOWHERE (an AST from a pack or an image).
            // (book-core-17)
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
