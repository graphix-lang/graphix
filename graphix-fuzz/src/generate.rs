//! Source C — type-directed program generation from scratch.
//!
//! Unlike Source A (mutating existing seeds), this builds valid programs
//! out of nothing, reaching shapes no fixture contains — "test cases no
//! one would think of." It's purely mechanical (no API), so it can run
//! for as long as you let it.
//!
//! The generator is TYPE-DIRECTED: `gen_typed(ctx, ty, …)` emits a graphix
//! expression *of type `ty`*, recursively, choosing only constructions
//! that produce `ty` from in-scope bindings + literals. This guarantees
//! type-correctness by construction (the killer constraint — a random
//! parse-valid graphix program typechecks ~never). It emits TEXT (not the
//! AST), tracking types as it goes; the oracle compiles from text.
//!
//! `let`-bindings introduce scoped variables that later expressions
//! reference — producing the internal data dependencies that stress
//! fusion's region/dataflow analysis (fusion *is* dependency-subgraph
//! analysis). Only pure, deterministic constructs are generated (no
//! rand/time/net/fs), so the oracle's comparison is sound.
//!
//! V1 covers: i64/f64/u8/bool/string scalars, arithmetic, comparison,
//! boolean ops, tuples, arrays, and `select`. Composite accessors, HOFs,
//! structs, and value-shape types are follow-ups.

use crate::mutate::Rng;

#[derive(Debug, Clone, PartialEq)]
pub enum GenType {
    I64,
    F64,
    U8,
    Bool,
    Str,
    Tuple(Vec<GenType>),
    Array(Box<GenType>),
}

impl GenType {
    #[allow(dead_code)]
    fn is_numeric(&self) -> bool {
        matches!(self, GenType::I64 | GenType::F64 | GenType::U8)
    }
}

struct GenCtx {
    /// In-scope bindings: (name, type). Later expressions ref these.
    vars: Vec<(String, GenType)>,
    next: usize,
}

impl GenCtx {
    fn new() -> Self {
        GenCtx { vars: Vec::new(), next: 0 }
    }

    fn fresh(&mut self) -> String {
        let n = format!("v{}", self.next);
        self.next += 1;
        n
    }

    fn vars_of(&self, ty: &GenType) -> Vec<&str> {
        self.vars.iter().filter(|(_, t)| t == ty).map(|(n, _)| n.as_str()).collect()
    }
}

fn pick<'a>(rng: &mut Rng, xs: &[&'a str]) -> &'a str {
    xs[rng.below(xs.len())]
}

fn numeric_type(rng: &mut Rng) -> GenType {
    match rng.below(3) {
        0 => GenType::I64,
        1 => GenType::F64,
        _ => GenType::U8,
    }
}

fn random_type(rng: &mut Rng, depth: usize) -> GenType {
    if depth == 0 {
        return match rng.below(5) {
            0 => GenType::I64,
            1 => GenType::F64,
            2 => GenType::U8,
            3 => GenType::Bool,
            _ => GenType::Str,
        };
    }
    match rng.below(8) {
        0 | 1 => GenType::I64,
        2 => GenType::F64,
        3 => GenType::U8,
        4 => GenType::Bool,
        5 => GenType::Str,
        6 => {
            let n = 2 + rng.below(2);
            GenType::Tuple((0..n).map(|_| random_type(rng, depth - 1)).collect())
        }
        _ => GenType::Array(Box::new(random_type(rng, depth - 1))),
    }
}

fn literal(rng: &mut Rng, ty: &GenType) -> String {
    match ty {
        GenType::I64 => {
            let v = [0i64, 1, -1, 2, 42, 100, -100, 7][rng.below(8)];
            format!("i64:{v}")
        }
        GenType::U8 => format!("u8:{}", [0u8, 1, 2, 100, 255][rng.below(5)]),
        GenType::F64 => {
            let v = ["0.0", "1.0", "-1.0", "3.14", "2.5", "0.1"][rng.below(6)];
            format!("f64:{v}")
        }
        GenType::Bool => {
            if rng.below(2) == 0 {
                "true".into()
            } else {
                "false".into()
            }
        }
        GenType::Str => {
            let v = ["a", "hello", "", "xyz", "graphix"][rng.below(5)];
            format!("\"{v}\"")
        }
        GenType::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| literal(rng, e)).collect();
            format!("({})", parts.join(", "))
        }
        GenType::Array(elem) => {
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| literal(rng, elem)).collect();
            format!("[{}]", parts.join(", "))
        }
    }
}

/// Emit an expression of exactly `ty`, recursing up to `depth`.
fn gen_typed(ctx: &GenCtx, rng: &mut Rng, ty: &GenType, depth: usize) -> String {
    // Base case: a literal, or an in-scope ref of this type. Bias toward
    // refs when available (they create dataflow).
    let recurse = depth > 0 && rng.below(3) != 0;
    if !recurse {
        let vars = ctx.vars_of(ty);
        if !vars.is_empty() && rng.below(2) == 0 {
            return vars[rng.below(vars.len())].to_string();
        }
        return literal(rng, ty);
    }
    let d = depth - 1;
    match ty {
        GenType::I64 | GenType::F64 | GenType::U8 => {
            // Unary minus (a real `Neg` node, exercising `ineg`/`fneg`). Only
            // for signed/float — `-u8` is a compile error (Part B's
            // signed/float/decimal constraint). Parenthesize the operand so
            // `-(i64:5)` stays a `Neg` rather than re-parsing as a negative
            // literal.
            if !matches!(ty, GenType::U8) && rng.below(6) == 0 {
                return format!("(-({}))", gen_typed(ctx, rng, ty, d));
            }
            // Bias toward +/-/* over //% — a generated `/0` or `%0` drops to
            // bottom (Timeout in all modes), which is slow to check. Div/mod
            // are still ~25% of ops, so the div0/overflow paths get exercised.
            let op = pick(rng, &["+", "+", "-", "-", "*", "*", "/", "%"]);
            format!(
                "({} {} {})",
                gen_typed(ctx, rng, ty, d),
                op,
                gen_typed(ctx, rng, ty, d)
            )
        }
        GenType::Bool => match rng.below(3) {
            0 => {
                let nt = numeric_type(rng);
                let op = pick(rng, &["<", ">", "<=", ">=", "==", "!="]);
                format!(
                    "({} {} {})",
                    gen_typed(ctx, rng, &nt, d),
                    op,
                    gen_typed(ctx, rng, &nt, d)
                )
            }
            1 => {
                let op = pick(rng, &["&&", "||"]);
                format!(
                    "({} {} {})",
                    gen_typed(ctx, rng, &GenType::Bool, d),
                    op,
                    gen_typed(ctx, rng, &GenType::Bool, d)
                )
            }
            _ => format!("(!{})", gen_typed(ctx, rng, &GenType::Bool, d)),
        },
        GenType::Str => literal(rng, ty),
        GenType::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(|e| gen_typed(ctx, rng, e, d)).collect();
            format!("({})", parts.join(", "))
        }
        GenType::Array(elem) => {
            let n = 1 + rng.below(3);
            let parts: Vec<_> = (0..n).map(|_| gen_typed(ctx, rng, elem, d)).collect();
            format!("[{}]", parts.join(", "))
        }
    }
}

/// Maybe wrap an expression of `ty` in a `select` over a numeric
/// scrutinee with a literal arm + catch-all (both arms produce `ty`).
fn maybe_select(
    ctx: &GenCtx,
    rng: &mut Rng,
    ty: &GenType,
    depth: usize,
) -> Option<String> {
    if depth == 0 || rng.below(4) != 0 {
        return None;
    }
    let scrut = gen_typed(ctx, rng, &GenType::I64, depth - 1);
    let lit = [0i64, 1, 2, 42][rng.below(4)];
    let arm = gen_typed(ctx, rng, ty, depth - 1);
    let dflt = gen_typed(ctx, rng, ty, depth - 1);
    Some(format!("select {scrut} {{ {lit} => {arm}, _ => {dflt} }}"))
}

/// Generate one complete program (a graphix expression). With a few
/// `let`-bindings whose values reference earlier bindings, plus a tail.
pub fn gen_program(rng: &mut Rng) -> String {
    let mut ctx = GenCtx::new();
    let mut stmts = Vec::new();
    let nlets = rng.below(4); // 0..=3 bindings
    for _ in 0..nlets {
        let ty = random_type(rng, 2);
        let name = ctx.fresh();
        let val = maybe_select(&ctx, rng, &ty, 3)
            .unwrap_or_else(|| gen_typed(&ctx, rng, &ty, 3));
        stmts.push(format!("let {name} = {val}"));
        ctx.vars.push((name, ty));
    }
    let tail_ty = random_type(rng, 2);
    let tail = maybe_select(&ctx, rng, &tail_ty, 3)
        .unwrap_or_else(|| gen_typed(&ctx, rng, &tail_ty, 3));
    if stmts.is_empty() { tail } else { format!("{{ {}; {} }}", stmts.join("; "), tail) }
}
