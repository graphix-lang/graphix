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
    parser::parse_one, ApplyExpr, BindExpr, Expr, ExprKind, SelectExpr, StructExpr,
    StructWithExpr, TryCatchExpr,
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
        if n == 0 {
            0
        } else {
            (self.next_u64() % n as u64) as usize
        }
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

fn for_each_child(e: &Expr, f: &mut impl FnMut(&Expr)) {
    use ExprKind::*;
    match &e.kind {
        NoOp | Constant(_) | Use { .. } | Ref { .. } | TypeDef(_) | Module { .. } => {}
        ExplicitParens(x)
        | Qop(x)
        | OrNever(x)
        | ByRef(x)
        | Deref(x)
        | Not { expr: x }
        | TypeCast { expr: x, .. } => f(x),
        Do { exprs }
        | StringInterpolate { args: exprs }
        | Any { args: exprs }
        | Array { args: exprs }
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
        TryCatch(tc) => {
            for c in tc.exprs.iter() {
                f(c);
            }
            f(&tc.handler);
        }
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
        NoOp | Constant(_) | Use { .. } | Ref { .. } | TypeDef(_) | Module { .. } => {
            e.kind.clone()
        }
        ExplicitParens(x) => ExplicitParens(ra!(x)),
        Qop(x) => Qop(ra!(x)),
        OrNever(x) => OrNever(ra!(x)),
        ByRef(x) => ByRef(ra!(x)),
        Deref(x) => Deref(ra!(x)),
        Not { expr } => Not { expr: ra!(expr) },
        TypeCast { expr, typ } => TypeCast { expr: ra!(expr), typ: typ.clone() },
        Do { exprs } => Do { exprs: aslice(exprs.iter().map(|c| r!(c)).collect()) },
        StringInterpolate { args } => {
            StringInterpolate { args: aslice(args.iter().map(|c| r!(c)).collect()) }
        }
        Any { args } => Any { args: aslice(args.iter().map(|c| r!(c)).collect()) },
        Array { args } => Array { args: aslice(args.iter().map(|c| r!(c)).collect()) },
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
        TryCatch(tc) => TryCatch(Arc::new(TryCatchExpr {
            bind: tc.bind.clone(),
            constraint: tc.constraint.clone(),
            exprs: aslice(tc.exprs.iter().map(|c| r!(c)).collect()),
            handler: ra!(&tc.handler),
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
        let kind = match rng.below(3) {
            0 => try_swap_binop(node, rng),
            1 => try_perturb_literal(node, rng),
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

/// Build the transplant donor pool: every subtree of every seed.
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

/// Total node count (preorder), so a reduction target can be chosen.
pub fn node_count(e: &Expr) -> usize {
    count(e)
}

/// Replace the node at preorder index `target` with `repl`.
pub fn replace(prog: &Expr, target: usize, repl: &Expr) -> Expr {
    let mut ctr = 0;
    replace_at(prog, target, &mut ctr, repl)
}

/// Candidate reductions for the node at preorder index `target`:
/// each of its direct children (hoist a sub-expression up), plus a few
/// minimal constants (collapse a whole computation to a literal). The
/// reducer keeps any candidate that still parses, still typechecks, and
/// reproduces the same divergence — so these are type-blind here and the
/// oracle filters them.
pub fn reductions(prog: &Expr, target: usize) -> Vec<Expr> {
    let mut nodes = Vec::new();
    collect_preorder(prog, &mut nodes);
    if target >= nodes.len() {
        return Vec::new();
    }
    let node = &nodes[target];
    let mut out = Vec::new();
    for_each_child(node, &mut |c| out.push(c.clone()));
    for v in [Value::I64(0), Value::F64(0.0), Value::Bool(true), Value::Null] {
        out.push(Expr::new(ExprKind::Constant(v), node.pos));
    }
    out
}
