//! Seed mutation: parse a seed, apply 1..=N type-blind structural
//! mutations to its AST (transplant a donor subtree, swap a binary
//! operator within its class, push a literal toward an edge value,
//! create a shadowing rename, strip a type annotation), print it back
//! and hand it to the oracle, whose compile step is the validity filter.
//! The RNG is a seeded xorshift, so any run replays from its seed.

use graphix_compiler::expr::{
    BindExpr, Expr, ExprKind, Origin, StructurePattern,
    parser::{self, parse_one},
};
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

fn collect_preorder(e: &Expr, out: &mut Vec<Expr>) {
    out.push(e.clone());
    e.for_each_child(&mut |c| collect_preorder(c, out));
}

/// The canonical preorder as a flat clone list — index `i` here is the
/// same node [`replace`] addresses. Shared with `typemorph`'s site
/// enumeration.
pub(crate) fn preorder(e: &Expr) -> Vec<Expr> {
    let mut out = Vec::new();
    collect_preorder(e, &mut out);
    out
}

/// Total number of nodes (preorder), so a target index can be chosen.
fn count(e: &Expr) -> usize {
    let mut n = 1;
    e.for_each_child(&mut |c| n += count(c));
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
    e.map_children(&mut |c| replace_at(c, target, ctr, repl))
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

/// If `e` is a block with ≥2 simple binds, rename a later one to an
/// earlier one's name, creating a shadow. Type-blind.
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

/// If `e` is a lambda (or a bind of one), strip one type annotation:
/// a param constraint, the return type, or the bind's annotation.
/// Type-blind.
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
    let target = rng.below(total);
    let donor = if donor_nodes.is_empty() {
        nodes[rng.below(total)].clone()
    } else {
        rng.pick(donor_nodes).clone()
    };
    let mut ctr = 0;
    replace_at(prog, target, &mut ctr, &donor)
}

/// Parse `seed`, apply 1..=`max_muts` mutations and return the mutated
/// text. Every intermediate must still parse; type validity is the
/// oracle's job. `None` if the seed itself doesn't parse.
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

/// Wrapper-aware mutation: the schedule header, callable header and file
/// sections are split off before the AST round-trip (which drops
/// comments) and reattached verbatim; only the main body mutates. A
/// scheduled seed may also get a schedule op, alone or alongside.
pub fn mutate_wrapper(
    seed: &str,
    donor_nodes: &[Expr],
    rng: &mut Rng,
    max_muts: usize,
) -> Option<String> {
    let (mut sched, body) = crate::schedule::Schedule::parse(seed).ok()?;
    let (cspec, body_owned) = crate::callable::CallSpec::parse(body).ok()?;
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

/// One schedule op. Epoch structure stays valid by construction; caps
/// are left alone (shrinking them is the minimizer's business).
fn mutate_schedule(s: &mut crate::schedule::Schedule, rng: &mut Rng) {
    use netidx::publisher::Value;
    let n = s.epochs.len();
    match rng.below(5) {
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
        1 => {
            s.epochs.remove(rng.below(n));
        }
        // duplicate an epoch in place
        2 => {
            let i = rng.below(n);
            let ep = s.epochs[i].clone();
            s.epochs.insert(i, ep);
        }
        3 => {
            if n >= 2 {
                let i = rng.below(n - 1);
                s.epochs.swap(i, i + 1);
            }
        }
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

/// AST shape signature for ring admission: an order-independent hash
/// over (node kind, child arity) pairs, the node count, and whether the
/// tree contains a lambda, select or application. Headers and file
/// sections are split off first. `None` = unparseable.
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
        e.for_each_child(&mut |_| arity += 1);
        let mut h = ahash::AHasher::default();
        std::mem::discriminant(&e.kind).hash(&mut h);
        arity.hash(&mut h);
        // order-independent, so a statement shuffle is not novel
        *sig = sig.wrapping_add(h.finish());
        e.for_each_child(&mut |c| walk(c, sig, nodes, interesting));
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
    e.for_each_child(&mut |c| n += size_at(c, out));
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
            node.for_each_child(&mut |c| out.push(c.clone()));
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
///
/// Drops are keyed by the STATEMENT's preorder index, not the block's,
/// so a whole round of them composes: statements are disjoint.
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
        let mut idx = here + 1;
        for (pos, c) in exprs.iter().enumerate() {
            out.push((here, pos, idx));
            idx += count(c);
        }
    }
    e.for_each_child(&mut |c| statements_at(c, ctr, out));
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

    /// `sizes[i]` must be the extent of node `i` in the same preorder
    /// `replace` and `statements` index by.
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
        // a section renders bare: no block braces
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
