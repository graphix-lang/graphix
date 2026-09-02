//! Source A — fixture/seed mutation.
//!
//! Parse a seed expression, apply 1..=N structural mutations to its AST,
//! pretty-print back to text, and hand it to the differential oracle.
//! Three mutations, all type-blind (the oracle's compile step is the
//! validity filter — see the mutate-deep loop):
//!   - **transplant**: replace a random subtree with a subtree from a
//!     donor seed. High structural novelty — the "mash up two fixtures to
//!     create a novel interaction" idea. Type-blind, so most results
//!     don't typecheck, but the survivors are exactly the interaction-
//!     rich programs where bugs live.
//!   - **swap_binop**: change a binary operator within its class
//!     (arith/checked/cmp/bool). Type-preserving.
//!   - **perturb_literal**: change a numeric/bool literal toward an edge
//!     value (0, ±1, MIN/MAX, inf, NaN). Exercises overflow / float edges.
//!
//! Determinism: a seeded xorshift RNG, so any run replays from its seed.

use graphix_compiler::expr::{
    ApplyExpr, BindExpr, CatchExpr, Expr, ExprKind, Origin, SelectExpr, StructExpr,
    StructWithExpr, StructurePattern,
    parser::{self, parse_one},
};
use netidx::utils::Either;
use netidx_value::Value;
use triomphe::Arc;

/// Deterministic, replayable xorshift64 RNG.
pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Self {
        Rng(seed | 1)
    }

    pub fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }

    pub fn below(&mut self, n: usize) -> usize {
        if n == 0 { 0 } else { (self.next_u64() % n as u64) as usize }
    }

    pub fn pick<'a, T>(&mut self, xs: &'a [T]) -> &'a T {
        &xs[self.below(xs.len())]
    }
}

fn aslice(v: Vec<Expr>) -> Arc<[Expr]> {
    Arc::from_iter(v)
}

// ── canonical child enumeration ──
// `collect_preorder` and `replace_at` MUST visit children in the same
// order so a preorder index means the same node to both. Both follow the
// field order below; the complex sub-structs descend into their `Expr`
// fields (Lambda arg defaults are intentionally skipped — rare, fiddly).

fn collect_preorder(e: &Expr, out: &mut Vec<Expr>) {
    out.push(e.clone());
    for_each_child(e, &mut |c| collect_preorder(c, out));
}

/// The canonical preorder as a flat clone list — index `i` here is the
/// same node [`replace`] addresses. Shared with `typemorph`'s site
/// enumeration.
pub(crate) fn preorder(e: &Expr) -> Vec<Expr> {
    let mut out = Vec::new();
    collect_preorder(e, &mut out);
    out
}

pub(crate) fn for_each_child(e: &Expr, f: &mut impl FnMut(&Expr)) {
    use ExprKind::*;
    match &e.kind {
        NoOp
        | Constant(_)
        | Use { .. }
        | Ref { .. }
        | TypeDef(_)
        | Trait(_)
        | Impl(_)
        | Module { .. } => {}
        ExplicitParens(x)
        | Qop(x)
        | OrNever(x)
        | ByRef(x)
        | Deref(x)
        | Neg(x)
        | Not { expr: x }
        | Construct { arg: x, .. }
        | TypeCast { expr: x, .. } => f(x),
        Do { exprs }
        | StringInterpolate { args: exprs }
        | Any { args: exprs }
        | Never { args: exprs, .. }
        | Array { args: exprs }
        | List { args: exprs }
        | Tuple { args: exprs }
        | Variant { args: exprs, .. } => {
            for c in exprs.iter() {
                f(c);
            }
        }
        Bind(b) => f(&b.value),
        Connect { value, .. } => f(value),
        StructRef { source, .. } | TupleRef { source, .. } => f(source),
        ArrayRef { source, i } => {
            f(source);
            f(i);
        }
        ArraySlice { source, start, end } => {
            f(source);
            if let Some(s) = start {
                f(s);
            }
            if let Some(en) = end {
                f(en);
            }
        }
        MapRef { source, key } => {
            f(source);
            f(key);
        }
        Map { args } => {
            for (k, v) in args.iter() {
                f(k);
                f(v);
            }
        }
        Struct(s) => {
            for (_, v) in s.args.iter() {
                f(v);
            }
        }
        StructWith(sw) => {
            f(&sw.source);
            for (_, v) in sw.replace.iter() {
                f(v);
            }
        }
        Apply(a) => {
            f(&a.function);
            for (_, v) in a.args.iter() {
                f(v);
            }
        }
        Select(s) => {
            f(&s.arg);
            for (_, body) in s.arms.iter() {
                f(body);
            }
        }
        Catch(c) => f(&c.handler),
        Lambda(l) => {
            if let Either::Left(body) = &l.body {
                f(body);
            }
        }
        Eq { lhs, rhs }
        | Ne { lhs, rhs }
        | Lt { lhs, rhs }
        | Gt { lhs, rhs }
        | Lte { lhs, rhs }
        | Gte { lhs, rhs }
        | And { lhs, rhs }
        | Or { lhs, rhs }
        | Add { lhs, rhs }
        | CheckedAdd { lhs, rhs }
        | Sub { lhs, rhs }
        | CheckedSub { lhs, rhs }
        | Mul { lhs, rhs }
        | CheckedMul { lhs, rhs }
        | Div { lhs, rhs }
        | CheckedDiv { lhs, rhs }
        | Mod { lhs, rhs }
        | CheckedMod { lhs, rhs }
        | Sample { lhs, rhs } => {
            f(lhs);
            f(rhs);
        }
    }
}

/// Total number of nodes (preorder), so a target index can be chosen.
fn count(e: &Expr) -> usize {
    let mut n = 1;
    for_each_child(e, &mut |c| n += count(c));
    n
}

/// Rebuild `e` with the node at preorder index `target` replaced by
/// `repl`. `ctr` threads the running preorder index.
fn replace_at(e: &Expr, target: usize, ctr: &mut usize, repl: &Expr) -> Expr {
    let here = *ctr;
    *ctr += 1;
    if here == target {
        return repl.clone();
    }
    // Direct recursive calls via macros — closures can't both hold the
    // `&mut ctr` borrow. Deref coercion lets `r!`/`ra!` accept either
    // `&Expr` or `&Arc<Expr>`.
    macro_rules! r {
        ($c:expr) => {
            replace_at($c, target, ctr, repl)
        };
    }
    macro_rules! ra {
        ($c:expr) => {
            Arc::new(replace_at($c, target, ctr, repl))
        };
    }
    use ExprKind::*;
    let kind = match &e.kind {
        NoOp
        | Constant(_)
        | Use { .. }
        | Ref { .. }
        | TypeDef(_)
        | Trait(_)
        | Impl(_)
        | Module { .. } => e.kind.clone(),
        ExplicitParens(x) => ExplicitParens(ra!(x)),
        Qop(x) => Qop(ra!(x)),
        OrNever(x) => OrNever(ra!(x)),
        ByRef(x) => ByRef(ra!(x)),
        Deref(x) => Deref(ra!(x)),
        Neg(x) => Neg(ra!(x)),
        Not { expr } => Not { expr: ra!(expr) },
        TypeCast { expr, typ } => TypeCast { expr: ra!(expr), typ: typ.clone() },
        Construct { name, arg } => Construct { name: name.clone(), arg: ra!(arg) },
        Do { exprs } => Do { exprs: aslice(exprs.iter().map(|c| r!(c)).collect()) },
        StringInterpolate { args } => {
            StringInterpolate { args: aslice(args.iter().map(|c| r!(c)).collect()) }
        }
        Any { args } => Any { args: aslice(args.iter().map(|c| r!(c)).collect()) },
        Never { typ, args } => {
            Never { typ: typ.clone(), args: aslice(args.iter().map(|c| r!(c)).collect()) }
        }
        Array { args } => Array { args: aslice(args.iter().map(|c| r!(c)).collect()) },
        List { args } => List { args: aslice(args.iter().map(|c| r!(c)).collect()) },
        Tuple { args } => Tuple { args: aslice(args.iter().map(|c| r!(c)).collect()) },
        Variant { tag, args } => Variant {
            tag: tag.clone(),
            args: aslice(args.iter().map(|c| r!(c)).collect()),
        },
        Bind(b) => Bind(Arc::new(BindExpr {
            rec: b.rec,
            pattern: b.pattern.clone(),
            typ: b.typ.clone(),
            value: r!(&b.value),
        })),
        Connect { name, value, deref } => {
            Connect { name: name.clone(), value: ra!(value), deref: *deref }
        }
        StructRef { source, field } => {
            StructRef { source: ra!(source), field: field.clone() }
        }
        TupleRef { source, field } => TupleRef { source: ra!(source), field: *field },
        ArrayRef { source, i } => ArrayRef { source: ra!(source), i: ra!(i) },
        ArraySlice { source, start, end } => ArraySlice {
            source: ra!(source),
            start: start.as_ref().map(|s| ra!(s)),
            end: end.as_ref().map(|s| ra!(s)),
        },
        MapRef { source, key } => MapRef { source: ra!(source), key: ra!(key) },
        Map { args } => Map {
            args: args.iter().map(|(k, v)| (r!(k), r!(v))).collect::<Vec<_>>().into(),
        },
        Struct(s) => Struct(StructExpr {
            args: s
                .args
                .iter()
                .map(|(n, v)| (n.clone(), r!(v)))
                .collect::<Vec<_>>()
                .into(),
        }),
        StructWith(sw) => StructWith(StructWithExpr {
            source: ra!(&sw.source),
            replace: sw
                .replace
                .iter()
                .map(|(n, v)| (n.clone(), r!(v)))
                .collect::<Vec<_>>()
                .into(),
        }),
        Apply(a) => Apply(ApplyExpr {
            function: ra!(&a.function),
            args: a
                .args
                .iter()
                .map(|(n, v)| (n.clone(), r!(v)))
                .collect::<Vec<_>>()
                .into(),
        }),
        Select(s) => Select(SelectExpr {
            arg: ra!(&s.arg),
            arms: s
                .arms
                .iter()
                .map(|(p, b)| (p.clone(), r!(b)))
                .collect::<Vec<_>>()
                .into(),
        }),
        Catch(c) => Catch(Arc::new(CatchExpr {
            bind: c.bind.clone(),
            constraint: c.constraint.clone(),
            handler: ra!(&c.handler),
        })),
        Lambda(l) => {
            let mut nl = (**l).clone();
            if let Either::Left(body) = &l.body {
                nl.body = Either::Left(r!(body));
            }
            Lambda(Arc::new(nl))
        }
        Eq { lhs, rhs } => Eq { lhs: ra!(lhs), rhs: ra!(rhs) },
        Ne { lhs, rhs } => Ne { lhs: ra!(lhs), rhs: ra!(rhs) },
        Lt { lhs, rhs } => Lt { lhs: ra!(lhs), rhs: ra!(rhs) },
        Gt { lhs, rhs } => Gt { lhs: ra!(lhs), rhs: ra!(rhs) },
        Lte { lhs, rhs } => Lte { lhs: ra!(lhs), rhs: ra!(rhs) },
        Gte { lhs, rhs } => Gte { lhs: ra!(lhs), rhs: ra!(rhs) },
        And { lhs, rhs } => And { lhs: ra!(lhs), rhs: ra!(rhs) },
        Or { lhs, rhs } => Or { lhs: ra!(lhs), rhs: ra!(rhs) },
        Add { lhs, rhs } => Add { lhs: ra!(lhs), rhs: ra!(rhs) },
        CheckedAdd { lhs, rhs } => CheckedAdd { lhs: ra!(lhs), rhs: ra!(rhs) },
        Sub { lhs, rhs } => Sub { lhs: ra!(lhs), rhs: ra!(rhs) },
        CheckedSub { lhs, rhs } => CheckedSub { lhs: ra!(lhs), rhs: ra!(rhs) },
        Mul { lhs, rhs } => Mul { lhs: ra!(lhs), rhs: ra!(rhs) },
        CheckedMul { lhs, rhs } => CheckedMul { lhs: ra!(lhs), rhs: ra!(rhs) },
        Div { lhs, rhs } => Div { lhs: ra!(lhs), rhs: ra!(rhs) },
        CheckedDiv { lhs, rhs } => CheckedDiv { lhs: ra!(lhs), rhs: ra!(rhs) },
        Mod { lhs, rhs } => Mod { lhs: ra!(lhs), rhs: ra!(rhs) },
        CheckedMod { lhs, rhs } => CheckedMod { lhs: ra!(lhs), rhs: ra!(rhs) },
        Sample { lhs, rhs } => Sample { lhs: ra!(lhs), rhs: ra!(rhs) },
    };
    Expr::new(kind, e.pos)
}

fn binop_kind(name: &str, lhs: Arc<Expr>, rhs: Arc<Expr>) -> ExprKind {
    use ExprKind::*;
    match name {
        "Add" => Add { lhs, rhs },
        "Sub" => Sub { lhs, rhs },
        "Mul" => Mul { lhs, rhs },
        "Div" => Div { lhs, rhs },
        "Mod" => Mod { lhs, rhs },
        "CheckedAdd" => CheckedAdd { lhs, rhs },
        "CheckedSub" => CheckedSub { lhs, rhs },
        "CheckedMul" => CheckedMul { lhs, rhs },
        "CheckedDiv" => CheckedDiv { lhs, rhs },
        "CheckedMod" => CheckedMod { lhs, rhs },
        "Eq" => Eq { lhs, rhs },
        "Ne" => Ne { lhs, rhs },
        "Lt" => Lt { lhs, rhs },
        "Gt" => Gt { lhs, rhs },
        "Lte" => Lte { lhs, rhs },
        "Gte" => Gte { lhs, rhs },
        "And" => And { lhs, rhs },
        "Or" => Or { lhs, rhs },
        _ => unreachable!(),
    }
}

const ARITH: &[&str] = &["Add", "Sub", "Mul", "Div", "Mod"];
const CHECKED: &[&str] =
    &["CheckedAdd", "CheckedSub", "CheckedMul", "CheckedDiv", "CheckedMod"];
const CMP: &[&str] = &["Eq", "Ne", "Lt", "Gt", "Lte", "Gte"];
const BOOLOP: &[&str] = &["And", "Or"];

/// If `e` is a binop, return a same-class swap with its operands.
fn try_swap_binop(e: &Expr, rng: &mut Rng) -> Option<ExprKind> {
    use ExprKind::*;
    let (class, lhs, rhs) = match &e.kind {
        Add { lhs, rhs }
        | Sub { lhs, rhs }
        | Mul { lhs, rhs }
        | Div { lhs, rhs }
        | Mod { lhs, rhs } => (ARITH, lhs, rhs),
        CheckedAdd { lhs, rhs }
        | CheckedSub { lhs, rhs }
        | CheckedMul { lhs, rhs }
        | CheckedDiv { lhs, rhs }
        | CheckedMod { lhs, rhs } => (CHECKED, lhs, rhs),
        Eq { lhs, rhs }
        | Ne { lhs, rhs }
        | Lt { lhs, rhs }
        | Gt { lhs, rhs }
        | Lte { lhs, rhs }
        | Gte { lhs, rhs } => (CMP, lhs, rhs),
        And { lhs, rhs } | Or { lhs, rhs } => (BOOLOP, lhs, rhs),
        _ => return None,
    };
    let op = *rng.pick(class);
    Some(binop_kind(op, lhs.clone(), rhs.clone()))
}

/// If `e` is a numeric/bool constant, return an edge-value perturbation.
fn try_perturb_literal(e: &Expr, rng: &mut Rng) -> Option<ExprKind> {
    let v = match &e.kind {
        ExprKind::Constant(v) => v,
        _ => return None,
    };
    let nv = match v {
        Value::I64(_) => {
            Value::I64(*rng.pick(&[0, 1, -1, i64::MAX, i64::MIN, 2, 100, -100]))
        }
        Value::U64(_) => Value::U64(*rng.pick(&[0u64, 1, u64::MAX, 2, 100])),
        Value::I32(_) => Value::I32(*rng.pick(&[0i32, 1, -1, i32::MAX, i32::MIN])),
        Value::U8(_) => Value::U8(*rng.pick(&[0u8, 1, 255, 100, 200])),
        Value::F64(_) => Value::F64(*rng.pick(&[
            0.0,
            1.0,
            -1.0,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::NAN,
            f64::MAX,
            5e-324,
        ])),
        Value::F32(_) => {
            Value::F32(*rng.pick(&[0.0f32, 1.0, -1.0, f32::INFINITY, f32::NAN, f32::MAX]))
        }
        Value::Bool(b) => Value::Bool(!b),
        _ => return None,
    };
    Some(ExprKind::Constant(nv))
}

/// If `e` is a block with ≥2 simple (`let name = …`) binds, rename a
/// later one to an earlier one's name — a targeted shadow-creating
/// mutation. The 2026-07 audit's bug classes were all name-vs-identity
/// confusions (shadowed lambda names, colliding locals); this steers
/// seed programs into exactly those shapes. Type-blind (a rebind at a
/// different type is legal); the oracle's compile step filters.
fn try_shadow_rename(e: &Expr, rng: &mut Rng) -> Option<ExprKind> {
    let exprs = match &e.kind {
        ExprKind::Do { exprs } => exprs,
        _ => return None,
    };
    let binds: Vec<usize> = exprs
        .iter()
        .enumerate()
        .filter_map(|(i, ex)| match &ex.kind {
            ExprKind::Bind(b) if matches!(&b.pattern, StructurePattern::Bind(_)) => {
                Some(i)
            }
            _ => None,
        })
        .collect();
    if binds.len() < 2 {
        return None;
    }
    let i = rng.below(binds.len() - 1);
    let j = i + 1 + rng.below(binds.len() - 1 - i);
    let name = match &exprs[binds[i]].kind {
        ExprKind::Bind(b) => match &b.pattern {
            StructurePattern::Bind(n) => n.clone(),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    let new_exprs: Vec<Expr> = exprs
        .iter()
        .enumerate()
        .map(|(k, ex)| match &ex.kind {
            ExprKind::Bind(b) if k == binds[j] => Expr::new(
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: b.rec,
                    pattern: StructurePattern::Bind(name.clone()),
                    typ: b.typ.clone(),
                    value: b.value.clone(),
                })),
                ex.pos,
            ),
            _ => ex.clone(),
        })
        .collect();
    Some(ExprKind::Do { exprs: aslice(new_exprs) })
}

/// If `e` is a lambda (or a bind of one), strip a type annotation: a
/// random param constraint, the return type, or the bind's own
/// annotation. An unannotated lambda is per-call-site polymorphic, so
/// stripping a typed seed creates monomorphization pressure (the
/// audit's bug-2 class: two instantiations of one lambda in one
/// region). Type-blind; the oracle filters.
fn try_strip_annotation(e: &Expr, rng: &mut Rng) -> Option<ExprKind> {
    match &e.kind {
        ExprKind::Bind(b) if b.typ.is_some() && rng.below(2) == 0 => {
            Some(ExprKind::Bind(Arc::new(BindExpr {
                rec: b.rec,
                pattern: b.pattern.clone(),
                typ: None,
                value: b.value.clone(),
            })))
        }
        ExprKind::Lambda(l) => {
            let annotated: Vec<usize> = l
                .args
                .iter()
                .enumerate()
                .filter_map(|(i, a)| a.constraint.is_some().then_some(i))
                .collect();
            let strip_ret = l.rtype.is_some();
            if annotated.is_empty() && !strip_ret {
                return None;
            }
            let mut nl = (**l).clone();
            if strip_ret && (annotated.is_empty() || rng.below(2) == 0) {
                nl.rtype = None;
            } else {
                let i = annotated[rng.below(annotated.len())];
                let mut args: Vec<_> = l.args.iter().cloned().collect();
                args[i].constraint = None;
                nl.args = Arc::from_iter(args);
            }
            Some(ExprKind::Lambda(Arc::new(nl)))
        }
        _ => None,
    }
}

/// Apply one random mutation to `prog`, drawing transplant donors from
/// `donor_nodes` (a flat preorder pool of subtrees from the seed corpus).
pub fn mutate_once(prog: &Expr, donor_nodes: &[Expr], rng: &mut Rng) -> Expr {
    let nodes = {
        let mut v = Vec::new();
        collect_preorder(prog, &mut v);
        v
    };
    let total = nodes.len();
    // Try a few targets to find one a local mutation applies to; fall
    // back to transplant (which applies anywhere).
    for _ in 0..4 {
        let target = rng.below(total);
        let node = &nodes[target];
        let kind = match rng.below(5) {
            0 => try_swap_binop(node, rng),
            1 => try_perturb_literal(node, rng),
            2 => try_shadow_rename(node, rng),
            3 => try_strip_annotation(node, rng),
            _ => None,
        };
        if let Some(k) = kind {
            let repl = Expr::new(k, node.pos);
            let mut ctr = 0;
            return replace_at(prog, target, &mut ctr, &repl);
        }
    }
    // Transplant: replace a random subtree with a donor subtree.
    let target = rng.below(total);
    let donor = if donor_nodes.is_empty() {
        nodes[rng.below(total)].clone()
    } else {
        rng.pick(donor_nodes).clone()
    };
    let mut ctr = 0;
    replace_at(prog, target, &mut ctr, &donor)
}

/// Parse `seed`, apply 1..=`max_muts` mutations, return the mutated
/// program as text. Each intermediate must still PARSE (syntactic
/// validity); type validity is the oracle's job. `None` if the seed
/// itself doesn't parse.
pub fn mutate_program(
    seed: &str,
    donor_nodes: &[Expr],
    rng: &mut Rng,
    max_muts: usize,
) -> Option<String> {
    let mut expr = parse_one(seed).ok()?;
    let n = 1 + rng.below(max_muts);
    let mut applied = 0;
    let mut tries = 0;
    while applied < n && tries < n * 4 {
        tries += 1;
        let cand = mutate_once(&expr, donor_nodes, rng);
        let text = cand.to_string();
        // Keep only syntactically-valid intermediates; if a mutation
        // produces unparseable text, retry from the current expr (the
        // mutate-deep "cross the valley" loop).
        if parse_one(&text).is_ok() {
            expr = cand;
            applied += 1;
        }
    }
    if applied == 0 {
        return None;
    }
    Some(expr.to_string())
}

/// Wrapper-aware mutation (Phase 3): split any schedule header off
/// FIRST — the AST round-trip drops comments, so parsing the raw
/// wrapper would silently strip the schedule and every mutant of a
/// reactive seed would degenerate to single-burst. The body mutates
/// through [`mutate_program`] as before; when the seed carries a
/// schedule, an M3 SCHEDULE op fires with its own probability —
/// sometimes alongside a body mutation, sometimes alone (values
/// pushed toward edges, epochs dropped/duplicated/swapped/extended).
/// Headerless seeds behave exactly as before.
pub fn mutate_wrapper(
    seed: &str,
    donor_nodes: &[Expr],
    rng: &mut Rng,
    max_muts: usize,
) -> Option<String> {
    let (mut sched, body) = crate::schedule::Schedule::parse(seed).ok()?;
    // The callable header is wrapper DATA like the schedule's — split
    // it off before the AST round-trip and reattach verbatim.
    let (cspec, body_owned) = crate::callable::CallSpec::parse(body).ok()?;
    // File sections split off before the AST round-trip (which drops
    // comment lines — the markers are wrapper DATA, like the schedule
    // header) and ride through unchanged: only the MAIN body mutates.
    // Mutating module internals is a possible future M-op.
    let (body, files) = crate::files::split(&body_owned).ok()?;
    let sched_op = !sched.epochs.is_empty() && rng.below(100) < 40;
    let body_only_keep = sched_op && rng.below(100) < 50;
    let new_body = if body_only_keep {
        body.trim().to_string()
    } else {
        mutate_program(body, donor_nodes, rng, max_muts)?
    };
    if sched_op {
        mutate_schedule(&mut sched, rng);
    }
    let text = sched.render(&crate::files::render(&new_body, &files));
    Some(match &cspec {
        Some(c) => c.render(&text),
        None => text,
    })
}

/// One M3 schedule op. Epoch structure stays valid by construction
/// (never an empty epoch; dropping the last epoch yields the empty
/// schedule, which renders headerless). Caps are left alone — they're
/// the trace budgets, and shrinking them is the minimizer's business,
/// not a bug-finding move.
fn mutate_schedule(s: &mut crate::schedule::Schedule, rng: &mut Rng) {
    use netidx::publisher::Value;
    let n = s.epochs.len();
    match rng.below(5) {
        // Perturb one injection toward an edge value.
        0 => {
            let i = rng.below(n);
            let m = s.epochs[i].len();
            let v = &mut s.epochs[i][rng.below(m)].1;
            *v = match &*v {
                Value::I64(_) => Value::I64([0, 1, -1, i64::MAX, i64::MIN][rng.below(5)]),
                Value::F64(_) => Value::F64(
                    [
                        0.0,
                        -0.0,
                        1.0,
                        f64::NAN,
                        f64::INFINITY,
                        f64::NEG_INFINITY,
                        f64::MIN_POSITIVE,
                    ][rng.below(7)],
                ),
                Value::Bool(b) => Value::Bool(!*b),
                other => other.clone(),
            };
        }
        // Drop an epoch.
        1 => {
            s.epochs.remove(rng.below(n));
        }
        // Duplicate an epoch in place (same values twice — the
        // classic same-length-source / unchanged-selection firing
        // provocation).
        2 => {
            let i = rng.below(n);
            let ep = s.epochs[i].clone();
            s.epochs.insert(i, ep);
        }
        // Swap adjacent epochs.
        3 => {
            if n >= 2 {
                let i = rng.below(n - 1);
                s.epochs.swap(i, i + 1);
            }
        }
        // Append a perturbed copy of the last epoch.
        _ => {
            if let Some(last) = s.epochs.last().cloned() {
                s.epochs.push(last);
                let i = s.epochs.len() - 1;
                let m = s.epochs[i].len();
                let v = &mut s.epochs[i][rng.below(m)].1;
                *v = match &*v {
                    Value::I64(x) => Value::I64(x.wrapping_add(1)),
                    Value::F64(x) => Value::F64(*x + 1.0),
                    Value::Bool(b) => Value::Bool(!*b),
                    other => other.clone(),
                };
            }
        }
    }
}

/// Build the transplant donor pool: every subtree of every seed.
/// AST shape signature for ring admission: an order-independent hash
/// over (node-kind discriminant, child arity) pairs, plus the node
/// count and whether the tree contains a construct worth breeding from
/// (a lambda, select, or application). Splits any schedule header /
/// file sections off first, exactly like [`mutate_wrapper`]. `None` =
/// unparseable (never admitted).
pub fn shape_stats(prog: &str) -> Option<(u64, usize, bool)> {
    let (_, body) = crate::schedule::Schedule::parse(prog).ok()?;
    let (body, _) = crate::files::split(body).ok()?;
    let e = parse_one(&body).ok()?;
    let mut sig = 0u64;
    let mut nodes = 0usize;
    let mut interesting = false;
    fn walk(e: &Expr, sig: &mut u64, nodes: &mut usize, interesting: &mut bool) {
        use std::hash::{Hash, Hasher};
        *nodes += 1;
        if matches!(
            &e.kind,
            ExprKind::Lambda(_) | ExprKind::Select { .. } | ExprKind::Apply(_)
        ) {
            *interesting = true;
        }
        let mut arity = 0usize;
        for_each_child(e, &mut |_| arity += 1);
        let mut h = ahash::AHasher::default();
        std::mem::discriminant(&e.kind).hash(&mut h);
        arity.hash(&mut h);
        // Multiset sum: order-independent, so a pure statement shuffle
        // isn't "novel" — only new construct/arity combinations are.
        *sig = sig.wrapping_add(h.finish());
        for_each_child(e, &mut |c| walk(c, sig, nodes, interesting));
    }
    walk(&e, &mut sig, &mut nodes, &mut interesting);
    Some((sig, nodes, interesting))
}

pub fn donor_pool(seeds: &[&str]) -> Vec<Expr> {
    let mut pool = Vec::new();
    for s in seeds {
        if let Ok(e) = parse_one(s) {
            collect_preorder(&e, &mut pool);
        }
    }
    pool
}

// ── minimization support (used by the typed-AST reducer in `lib`) ──

/// Parse an expression, or `None` if it doesn't parse.
pub fn parse(s: &str) -> Option<Expr> {
    parse_one(s).ok()
}

/// Parse a top-level item SEQUENCE — a `.gx` module section, which is
/// a run of statements rather than the single expression `parse` takes.
/// Returned as a `Do` so one set of reduction machinery serves both;
/// render it back with [`render_items`], not `to_string`.
pub fn parse_items(s: &str) -> Option<Expr> {
    let items = parser::parse(Origin::from_str(s)).ok()?;
    let pos = items.first()?.pos;
    Some(Expr::new(ExprKind::Do { exprs: items }, pos))
}

/// Render a [`parse_items`] `Do` back to module-section text: the items
/// bare and semicolon-separated, NOT wrapped in the block braces
/// `to_string` would emit.
pub fn render_items(e: &Expr) -> String {
    match &e.kind {
        ExprKind::Do { exprs } => {
            exprs.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(";\n")
        }
        _ => e.to_string(),
    }
}

/// Total node count (preorder), so a reduction target can be chosen.
pub fn node_count(e: &Expr) -> usize {
    count(e)
}

/// Preorder subtree sizes: `sizes(e)[i]` is the node count rooted at
/// preorder index `i`, so `i..i + sizes[i]` is that node's extent. The
/// reducer needs it to tell INDEPENDENT targets apart — two reductions
/// compose only if neither sits inside the other.
pub fn sizes(e: &Expr) -> Vec<usize> {
    let mut out = Vec::new();
    size_at(e, &mut out);
    out
}

fn size_at(e: &Expr, out: &mut Vec<usize>) -> usize {
    let here = out.len();
    out.push(0);
    let mut n = 1;
    for_each_child(e, &mut |c| n += size_at(c, out));
    out[here] = n;
    n
}

/// Replace the node at preorder index `target` with `repl`.
pub fn replace(prog: &Expr, target: usize, repl: &Expr) -> Expr {
    let mut ctr = 0;
    replace_at(prog, target, &mut ctr, repl)
}

/// Candidate replacements for EVERY preorder target, in one walk —
/// `out[i]` is the list for the node at index `i`: each of its direct
/// children (hoist a sub-expression up), plus a few minimal constants
/// (collapse a whole computation to a literal). Type-blind — the
/// reducer keeps any candidate that still parses, still typechecks, and
/// reproduces the same divergence, so the oracle filters them.
pub fn reductions_all(prog: &Expr) -> Vec<Vec<Expr>> {
    let mut nodes = Vec::new();
    collect_preorder(prog, &mut nodes);
    nodes
        .iter()
        .map(|node| {
            let mut out = Vec::new();
            for_each_child(node, &mut |c| out.push(c.clone()));
            for v in [Value::I64(0), Value::F64(0.0), Value::Bool(true), Value::Null] {
                out.push(Expr::new(ExprKind::Constant(v), node.pos));
            }
            out
        })
        .collect()
}

/// Every droppable block statement: `(block's preorder index, position
/// in the block, the statement's OWN preorder index)`. Blocks with one
/// statement are skipped — dropping it empties the block.
///
/// The statement drop is the operator that does the work on generated
/// programs. They are long runs of interdependent `let`s, and the only
/// thing [`reductions_all`] can do to such a run is hoist one child of
/// the block, collapsing the whole run to a single statement — a
/// candidate that essentially never survives. Dropping one statement at
/// a time walks the run down instead. Keying each drop by the
/// STATEMENT's index (not the block's) is what lets a whole round of
/// them apply at once: statements are disjoint, so N drops found in one
/// scan compose, where N edits keyed at the shared block would all
/// claim the same subtree.
pub fn statements(e: &Expr) -> Vec<(usize, usize, usize)> {
    let mut out = Vec::new();
    let mut ctr = 0;
    statements_at(e, &mut ctr, &mut out);
    out
}

fn statements_at(e: &Expr, ctr: &mut usize, out: &mut Vec<(usize, usize, usize)>) {
    let here = *ctr;
    *ctr += 1;
    if let ExprKind::Do { exprs } = &e.kind
        && exprs.len() >= 2
    {
        // `for_each_child` visits a block's statements in order, so
        // their preorder indices run consecutively from `here + 1`.
        let mut idx = here + 1;
        for (pos, c) in exprs.iter().enumerate() {
            out.push((here, pos, idx));
            idx += count(c);
        }
    }
    for_each_child(e, &mut |c| statements_at(c, ctr, out));
}

/// Drop the statement at `pos` from the block at preorder index `at`.
/// Returns `prog` unchanged if that node isn't a block (the caller's
/// parse check still guards a drop that empties one).
pub fn drop_statement(prog: &Expr, at: usize, pos: usize) -> Expr {
    let mut nodes = Vec::new();
    collect_preorder(prog, &mut nodes);
    let Some(ExprKind::Do { exprs }) = nodes.get(at).map(|e| &e.kind) else {
        return prog.clone();
    };
    let kept: Vec<Expr> = exprs
        .iter()
        .enumerate()
        .filter(|(k, _)| *k != pos)
        .map(|(_, e)| e.clone())
        .collect();
    let block = Expr::new(ExprKind::Do { exprs: aslice(kept) }, nodes[at].pos);
    replace(prog, at, &block)
}

#[cfg(test)]
mod test {
    use super::*;

    const PROG: &str =
        "{let a = i64:1; let b = {let c = i64:2; c + a}; let d = [a, b]; a + b}";

    fn preorder(e: &Expr) -> Vec<Expr> {
        let mut v = Vec::new();
        collect_preorder(e, &mut v);
        v
    }

    /// The reducer's whole disjointness argument rests on `sizes[i]`
    /// being the extent of node `i` in the SAME preorder `replace` and
    /// `statements` index by — a drift here silently composes
    /// overlapping reductions.
    #[test]
    fn sizes_are_preorder_extents() {
        let e = parse(PROG).unwrap();
        let nodes = preorder(&e);
        let sz = sizes(&e);
        assert_eq!(sz.len(), nodes.len());
        for (i, n) in nodes.iter().enumerate() {
            let sub = preorder(n);
            assert_eq!(sz[i], sub.len(), "size at {i}");
            for (k, s) in sub.iter().enumerate() {
                assert_eq!(s.to_string(), nodes[i + k].to_string(), "extent {i}+{k}");
            }
        }
    }

    #[test]
    fn statement_targets_agree_with_drops() {
        let e = parse(PROG).unwrap();
        let nodes = preorder(&e);
        let stmts = statements(&e);
        assert_eq!(stmts.len(), 4 + 2); // outer block, inner block
        for (block, pos, stmt) in stmts {
            let ExprKind::Do { exprs } = &nodes[block].kind else {
                panic!("statement {stmt}'s block {block} is not a block")
            };
            assert_eq!(exprs[pos].to_string(), nodes[stmt].to_string());
            let mut want: Vec<String> = exprs.iter().map(|e| e.to_string()).collect();
            want.remove(pos);
            // Dropping a statement can't move its own block: the
            // block's children come after it in preorder.
            let after = preorder(&drop_statement(&e, block, pos));
            let ExprKind::Do { exprs } = &after[block].kind else { panic!() };
            let got: Vec<String> = exprs.iter().map(|e| e.to_string()).collect();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn items_round_trip() {
        let src = "let a: i64 = i64:1;\nlet b = |x: i64| -> i64 x + a;\nb(a)";
        let e = parse_items(src).unwrap();
        assert!(matches!(&e.kind, ExprKind::Do { exprs } if exprs.len() == 3));
        assert!(parse_items(&render_items(&e)).is_some());
        // A section is rendered BARE — block braces would not be a
        // module file, and the section text is spliced back verbatim.
        assert!(!render_items(&e).starts_with('{'));
        let dropped = render_items(&drop_statement(&e, 0, 1));
        assert!(!dropped.contains("|x: i64|"));
        assert!(parse_items(&dropped).is_some());
    }

    #[test]
    fn reductions_cover_every_node() {
        let e = parse(PROG).unwrap();
        assert_eq!(reductions_all(&e).len(), node_count(&e));
    }
}
