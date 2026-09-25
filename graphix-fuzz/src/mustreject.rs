//! Must-reject mutation (`design/must_reject.md`): take a program the
//! checker accepts and its checked types, apply ONE mutation a rule says
//! the checker must refuse, and require the refusal where the mutation
//! or its rigid consumer is. Families 1 (monomorphic reuse), 2 (rigid
//! variables), 3 (a shared variable at a call), 4 (widening into a rigid
//! consumer), 5 (variant widening), 6 (retyping a let) and 7 (labels).

use crate::{mutate, typemorph};
use ahash::AHashMap;
use arcstr::ArcStr;
use graphix_compiler::{
    SourcePosition,
    expr::{
        ApplyExpr, BinOp, BindExpr, Expr, ExprKind, LambdaExpr, ModPath, Name, Origin,
        Pattern, SelectExpr, Source, StructurePattern,
    },
    ide::ExprTypeSite,
    typ::{FnType, Type},
};
use netidx_core::utils::Either;
use netidx_value::Typ;
use triomphe::Arc;

/// A must-reject family.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Family {
    MonoReuse,
    RigidVar,
    SharedVar,
    WidenConsumer,
    VariantWiden,
    Retype,
    LabelUnknown,
    LabelMissing,
    LabelDefault,
}

impl std::fmt::Display for Family {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Family::MonoReuse => "mono-reuse",
            Family::RigidVar => "rigid-var",
            Family::SharedVar => "shared-var",
            Family::LabelUnknown => "label-unknown",
            Family::LabelMissing => "label-missing",
            Family::LabelDefault => "label-default",
            Family::WidenConsumer => "widen-consumer",
            Family::VariantWiden => "variant-widen",
            Family::Retype => "retype",
        })
    }
}

/// A span of the body, `[start, end)`.
pub type Span = (SourcePosition, SourcePosition);

/// One mutant: its body, and the spans a right refusal falls in (the
/// mutation or its rigid consumer).
pub struct RejectProbe {
    pub family: Family,
    pub site: usize,
    pub body: String,
    pub expect: Vec<Span>,
}

impl RejectProbe {
    pub fn id(&self) -> String {
        format!("{}#{}", self.family, self.site)
    }

    /// Whether a refusal at `pos` (body coordinates) is where the
    /// mutation's argument says it must be.
    pub fn right_site(&self, pos: SourcePosition) -> bool {
        let key = |p: SourcePosition| (p.line, p.column);
        self.expect.iter().any(|(s, e)| key(*s) <= key(pos) && key(pos) < key(*e))
    }
}

/// The base's checked types by body span; nodes sharing a span share an
/// entry.
pub struct TypeMap(AHashMap<(i32, i32, i32, i32), Entry>);

#[derive(Default)]
struct Entry {
    types: Vec<Type>,
    /// A node here had a type its uses decided (`ExprTypeSite::cell`).
    cell: bool,
}

impl TypeMap {
    /// From a check's sites in the subject's module `module`, whose first
    /// line carries `body_col` characters before the body.
    pub fn new(sites: &[ExprTypeSite], module: &str, body_col: usize) -> Self {
        let mut map: AHashMap<(i32, i32, i32, i32), Entry> = AHashMap::new();
        for s in sites.iter().filter(|s| in_module(&s.ori, module)) {
            let Some(end) = s.end else { continue };
            let (a, b) = (to_body(s.pos, body_col), to_body(end, body_col));
            let e = map.entry((a.line, a.column, b.line, b.column)).or_default();
            e.types.push(s.typ.clone());
            e.cell |= s.cell;
        }
        TypeMap(map)
    }

    fn entry(&self, e: &Expr) -> Option<&Entry> {
        let end = e.end.get()?;
        self.0.get(&(e.pos.line, e.pos.column, end.line, end.column))
    }

    fn of(&self, e: &Expr) -> &[Type] {
        self.entry(e).map_or(&[], |x| &x.types[..])
    }

    fn cell(&self, e: &Expr) -> bool {
        self.entry(e).is_some_and(|x| x.cell)
    }
}

/// Whether `ori` is the VFS module `module` (a module's origin is its
/// name, the root program's its whole text).
pub fn in_module(ori: &Origin, module: &str) -> bool {
    matches!(&ori.source, Source::Internal(m) if m == module)
}

/// A module-file position as a body position.
pub fn to_body(p: SourcePosition, body_col: usize) -> SourcePosition {
    match p.line {
        1 => SourcePosition { line: 1, column: p.column - body_col as i32 },
        _ => p,
    }
}

fn span(e: &Expr) -> Option<Span> {
    e.end.get().map(|end| (e.pos, end))
}

/// A single primitive type, the only kind whose disjointness is taken on
/// sight (two different ones never contain each other).
fn primitive(t: &Type) -> Option<Typ> {
    t.with_deref(|d| match d {
        Some(Type::Primitive(p)) if p.len() == 1 => p.iter().next(),
        _ => None,
    })
}

/// A function type's first parameter, when it is a single primitive.
fn first_param(t: &Type) -> Option<Typ> {
    t.with_deref(|d| match d {
        Some(Type::Fn(ft)) => ft.args.first().and_then(|a| primitive(&a.typ)),
        _ => None,
    })
}

/// The probe for mutant `cand`, its expected spans read off the mutant as
/// printed and reparsed (the spans the checker reports), or `None` when
/// the print does not read back as the mutant.
fn finish(
    family: Family,
    site: usize,
    cand: &Expr,
    expect: impl FnOnce(&Expr, &[Expr]) -> Vec<Option<Span>>,
) -> Option<RejectProbe> {
    let body = cand.to_string();
    let back = mutate::parse(&body).filter(|b| b == cand)?;
    let pre = mutate::preorder(&back);
    let expect = expect(&back, &pre).into_iter().flatten().collect();
    Some(RejectProbe { family, site, body, expect })
}

/// Statement `i` of a block.
fn nth_stmt(root: &Expr, i: usize) -> Option<&Expr> {
    match &root.kind {
        ExprKind::Block { exprs } => exprs.get(i),
        _ => None,
    }
}

/// Every mutant of `body` the families take, up to `cap` per family.
pub fn probes(body: &str, types: &TypeMap, cap: usize) -> Vec<RejectProbe> {
    let Some(root) = mutate::parse(body) else { return Vec::new() };
    let pre = mutate::preorder(&root);
    let mut out = Vec::new();
    mono_reuse(&root, &pre, types, cap, &mut out);
    rigid_var(&root, cap, &mut out);
    shared_var(&root, &pre, types, cap, &mut out);
    labels(&root, &pre, types, cap, &mut out);
    widen_consumer(&root, &pre, types, cap, &mut out);
    widen_through_let(&root, &pre, types, cap, &mut out);
    variant_widen(&root, &pre, types, cap, &mut out);
    retype(&root, &pre, types, cap, &mut out);
    out
}

/// `e` widened by `u`: `select (i64:1 == i64:1) { true => e, false => u }`,
/// typed the union of the two whatever the scrutinee's value.
fn widen(e: &Expr, u: Expr) -> Expr {
    let one =
        || Arc::new(ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos());
    ExprKind::Select(SelectExpr {
        arg: Arc::new(ExprKind::Eq { lhs: one(), rhs: one() }.to_expr_nopos()),
        arms: Arc::from_iter([(bool_arm(true), e.clone()), (bool_arm(false), u)]),
    })
    .to_expr_nopos()
}

/// A literal whose type `t` neither contains nor is contained by: a
/// primitive `t` lacks, or a string beside anything that is not a
/// primitive. `None` for a type that could hold anything.
fn disjoint_literal(t: &Type) -> Option<Expr> {
    use netidx_value::Value;
    let prim = t.with_deref(|d| match d {
        Some(Type::Primitive(p)) => Some(Some(*p)),
        Some(Type::Struct(_) | Type::Tuple(_) | Type::Array(_) | Type::Map { .. }) => {
            Some(None)
        }
        _ => None,
    })?;
    let v = match prim {
        None => Value::String(ArcStr::from("tm")),
        Some(p) => {
            [
                (Typ::String, Value::String(ArcStr::from("tm"))),
                (Typ::Bool, Value::Bool(true)),
                (Typ::I64, Value::I64(7)),
            ]
            .into_iter()
            .find(|(k, _)| !p.contains(*k))?
            .1
        }
    };
    Some(ExprKind::Constant(v).to_expr_nopos())
}

/// Whether a type has no cell anywhere, so it cannot bend to a widened
/// argument.
fn concrete(t: &Type) -> bool {
    t.resolve_tvars().tvar_free()
}

fn arith_or_compare(op: BinOp) -> bool {
    !matches!(op, BinOp::And | BinOp::Or | BinOp::Sample | BinOp::StrictSample)
}

/// The preorder indices of a call's arguments, `i` the call's.
fn arg_indices(i: usize, sizes: &[usize], n: usize) -> Vec<usize> {
    let mut idx = i + 1 + sizes[i + 1];
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        out.push(idx);
        idx += sizes[idx];
    }
    out
}

/// A call's callee type, when the map shows a function.
fn callee_type(
    types: &TypeMap,
    ap: &graphix_compiler::expr::ApplyExpr,
) -> Option<Arc<FnType>> {
    types.of(&ap.function).first().and_then(|t| {
        t.with_deref(|d| match d {
            Some(Type::Fn(ft)) => Some(ft.clone()),
            _ => None,
        })
    })
}

fn call(function: Arc<Expr>, args: Vec<(Option<ArcStr>, Expr)>) -> Expr {
    ExprKind::Apply(ApplyExpr { args: Arc::from_iter(args), function }).to_expr_nopos()
}

/// Family 2. A lambda whose parameter `x` is annotated by a declared
/// variable `'a` gets a first statement comparing `x` with a literal: a
/// def's declared variables are rigid in its body, so `'a` cannot become
/// the literal's type. Right site: the definition.
fn rigid_var(root: &Expr, cap: usize, out: &mut Vec<RejectProbe>) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let mut offset = 1usize;
    let mut taken = 0usize;
    for (si, stmt) in stmts.iter().enumerate() {
        let at = offset;
        offset += sizes[at];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let ExprKind::Lambda(l) = &b.value.kind else { continue };
        let Either::Left(body) = &l.body else { continue };
        let Some(x) = l.args.iter().find_map(|a| match (&a.pattern, &a.constraint) {
            (StructurePattern::Bind(x), Some(Type::TVar(_))) if a.labeled.is_none() => {
                Some(x)
            }
            _ => None,
        }) else {
            continue;
        };
        let one = ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos();
        let x_ref =
            ExprKind::Ref { name: ModPath::from([x.name.as_str()]) }.to_expr_nopos();
        let test =
            ExprKind::Eq { lhs: Arc::new(x_ref), rhs: Arc::new(one) }.to_expr_nopos();
        let body = ExprKind::Block {
            exprs: Arc::from_iter([
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: false,
                    pattern: StructurePattern::Bind(Name::from("tm__0")),
                    typ: None,
                    value: test,
                }))
                .to_expr_nopos(),
                body.clone(),
            ]),
        }
        .to_expr_nopos();
        let lambda = ExprKind::Lambda(Arc::new(LambdaExpr {
            body: Either::Left(body),
            ..(**l).clone()
        }))
        .to_expr_nopos();
        let cand = mutate::replace(root, at + 1, &lambda);
        out.extend(finish(Family::RigidVar, at, &cand, |back, _| {
            vec![nth_stmt(back, si).and_then(span)]
        }));
        taken += 1;
    }
}

/// Family 3. A call whose callee takes two positionals of one variable,
/// the first argument a concrete primitive, gets a second argument of a
/// type the first's does not relate to: with no argument containing the
/// other the widest-argument rule refuses, and a variable a callback
/// holds keeps the first (`callsite.rs::Widening`). Right site: the call.
fn shared_var(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    let mut taken = 0usize;
    for (i, e) in pre.iter().enumerate() {
        if taken >= cap {
            break;
        }
        let ExprKind::Apply(ap) = &e.kind else { continue };
        if ap.args.iter().any(|(l, _)| l.is_some()) {
            continue;
        }
        let Some(ft) = callee_type(types, ap) else { continue };
        let params: Vec<&Type> =
            ft.args.iter().filter(|a| a.is_positional()).map(|a| &a.typ).collect();
        if params.len() != ap.args.len() {
            continue;
        }
        let same = |a: &Type, b: &Type| matches!((a, b), (Type::TVar(x), Type::TVar(y)) if x.same_cell(y));
        let pair = (0..params.len()).find_map(|a| {
            ((a + 1)..params.len()).find(|b| same(params[a], params[*b])).map(|b| (a, b))
        });
        let Some((a, b)) = pair else { continue };
        let Some(t) = types.of(&ap.args[a].1).first() else { continue };
        if !concrete(t) || primitive(t).is_none() {
            continue;
        }
        let Some(u) = disjoint_literal(t) else { continue };
        let at = arg_indices(i, &sizes, ap.args.len())[b];
        let cand = mutate::replace(root, at, &u);
        out.extend(finish(Family::SharedVar, i, &cand, |_, pre| {
            vec![pre.get(i).and_then(span)]
        }));
        taken += 1;
    }
}

/// Family 7. At a call whose callee's labels the map shows: a label the
/// callee lacks, or a required label dropped (the call must name every
/// required label and no other). And a labeled lambda passed as a value
/// where the expected function type omits a defaulted label or keeps it
/// optional loses the default: the value then requires what the expected
/// type lets a caller omit (`fntyp.rs::align`). Right site: the call, or
/// for the default, the definition and every statement using the lambda.
fn labels(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    let (mut unknown, mut dropped) = (0usize, 0usize);
    for (i, e) in pre.iter().enumerate() {
        let ExprKind::Apply(ap) = &e.kind else { continue };
        let Some(ft) = callee_type(types, ap) else { continue };
        if !ft.args.iter().any(|a| a.label().is_some()) {
            continue;
        }
        if unknown < cap {
            let mut args = ap.args.to_vec();
            let lit = ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos();
            args.insert(0, (Some(ArcStr::from("tm__0")), lit));
            let cand = mutate::replace(root, i, &call(ap.function.clone(), args));
            out.extend(finish(Family::LabelUnknown, i, &cand, |_, pre| {
                vec![pre.get(i).and_then(span)]
            }));
            unknown += 1;
        }
        let required = ap.args.iter().position(|(l, _)| {
            l.as_ref().is_some_and(|l| {
                ft.args.iter().any(|a| a.label() == Some(l) && !a.has_default())
            })
        });
        if dropped < cap
            && let Some(j) = required
        {
            let mut args = ap.args.to_vec();
            args.remove(j);
            let cand = mutate::replace(root, i, &call(ap.function.clone(), args));
            out.extend(finish(Family::LabelMissing, i, &cand, |_, pre| {
                vec![pre.get(i).and_then(span)]
            }));
            dropped += 1;
        }
    }
    labels_default(root, pre, types, &sizes, cap, out);
}

/// The default half of family 7 (see [`labels`]).
fn labels_default(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    sizes: &[usize],
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let mut offset = 1usize;
    let mut taken = 0usize;
    for (si, stmt) in stmts.iter().enumerate() {
        let at = offset;
        offset += sizes[at];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(f), ExprKind::Lambda(l)) =
            (&b.pattern, &b.value.kind)
        else {
            continue;
        };
        let (mut calls, mut refs) = (Vec::new(), Vec::new());
        let mut idx = offset;
        for later in &stmts[si + 1..] {
            uses_reached(later, f, true, &mut idx, &mut calls, &mut refs);
            if typemorph::binds(later, f) {
                break;
            }
        }
        // a value use of `f` as an argument, and the parameter it meets
        let expected = pre.iter().enumerate().find_map(|(i, n)| {
            let ExprKind::Apply(ap) = &n.kind else { return None };
            let ft = callee_type(types, ap)?;
            let idxs = arg_indices(i, sizes, ap.args.len());
            let mut positional = ft.args.iter().filter(|a| a.is_positional());
            ap.args.iter().zip(idxs).find_map(|((label, _), ix)| {
                let param = match label {
                    Some(lb) => ft.args.iter().find(|a| a.label() == Some(lb)),
                    None => positional.next(),
                }?;
                refs.contains(&ix).then(|| param.typ.clone())
            })
        });
        let Some(expected) = expected else { continue };
        let expected = expected.with_deref(|d| match d {
            Some(Type::Fn(ft)) => Some(ft.clone()),
            _ => None,
        });
        let Some(expected) = expected else { continue };
        // a defaulted label the expected type omits or keeps optional
        let Some(k) = l.args.iter().position(|a| {
            matches!((&a.labeled, &a.pattern), (Some(Some(_)), StructurePattern::Bind(n))
                if expected.args.iter().find(|e| e.label().map(|x| &**x) == Some(n.name.as_str()))
                    .is_none_or(|e| e.has_default()))
        }) else {
            continue;
        };
        let mut args = l.args.to_vec();
        args[k].labeled = Some(None);
        let lambda = ExprKind::Lambda(Arc::new(LambdaExpr {
            args: Arc::from_iter(args),
            ..(**l).clone()
        }))
        .to_expr_nopos();
        let cand = mutate::replace(root, at + 1, &lambda);
        let mut sites = vec![si];
        sites.extend(stmts.iter().enumerate().skip(si + 1).filter(|(_, st)| {
            st.fold(false, &mut |m, n| m || matches!(&n.kind, ExprKind::Ref { name } if name.to_string() == **f))
        }).map(|(k, _)| k));
        out.extend(finish(Family::LabelDefault, at, &cand, |back, _| {
            sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
        }));
        taken += 1;
    }
}

/// Family 4. A value directly under a rigid consumer is widened by a
/// literal of a type its own does not relate to: an arithmetic or
/// comparison operand (exactly one type, each containing the other), a
/// field read's source (a union with a non-struct is refused), or an
/// argument whose parameter is concretely typed (containment).
fn widen_consumer(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    let mut taken = 0usize;
    for (i, e) in pre.iter().enumerate() {
        if taken >= cap {
            break;
        }
        let target: Option<(usize, &Expr)> = match &e.kind {
            k if BinOp::of(k).is_some_and(|(op, _, _)| arith_or_compare(op)) => {
                BinOp::of(k).map(|(_, lhs, _)| (i + 1, &**lhs))
            }
            ExprKind::StructRef { source, .. } => {
                let is_struct = types.of(source).first().is_some_and(|t| {
                    t.with_deref(|d| matches!(d, Some(Type::Struct(_))))
                });
                is_struct.then(|| (i + 1, &**source))
            }
            ExprKind::Apply(ap) => {
                // the first argument whose parameter is a concrete primitive
                let ft = types.of(&ap.function).first().and_then(|t| {
                    t.with_deref(|d| match d {
                        Some(Type::Fn(ft)) => Some(ft.clone()),
                        _ => None,
                    })
                });
                let Some(ft) = ft else { continue };
                let mut idx = i + 1 + sizes[i + 1];
                let mut found = None;
                let mut positional = ft.args.iter().filter(|a| a.label().is_none());
                for (label, arg) in ap.args.iter() {
                    let param = match label {
                        Some(l) => ft.args.iter().find(|a| a.label() == Some(l)),
                        None => positional.next(),
                    };
                    if found.is_none()
                        && let Some(p) = param
                        && concrete(&p.typ)
                        && p.typ.with_deref(|d| matches!(d, Some(Type::Primitive(_))))
                    {
                        found = Some((idx, arg));
                    }
                    idx += sizes[idx];
                }
                found
            }
            _ => None,
        };
        let Some((at, value)) = target else { continue };
        let Some(t) = types.of(value).first() else { continue };
        if !concrete(t) {
            continue;
        }
        let Some(u) = disjoint_literal(t) else { continue };
        let cand = mutate::replace(root, at, &widen(value, u));
        // the consumer keeps its index: what changed comes after it
        out.extend(finish(Family::WidenConsumer, i, &cand, |_, pre| {
            vec![pre.get(i).and_then(span)]
        }));
        taken += 1;
    }
}

/// Family 6. An unannotated `let v = e` over a primitive gets a writer
/// of a type `e`'s does not relate to (a later writer must fit the type
/// the initializer gave), or, where a later use pins `v` by a literal
/// (`v + 1`), an initializer of such a type (the use then conflicts).
fn retype(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let mut offset = 1usize;
    let mut taken = 0usize;
    for (si, stmt) in stmts.iter().enumerate() {
        let at = offset;
        offset += sizes[at];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(v), None, false) = (&b.pattern, &b.typ, b.rec) else {
            continue;
        };
        // a let over ⊥ (its type a cell the first use decides) takes a
        // writer's type rather than refusing it
        if matches!(b.value.kind, ExprKind::Lambda(_)) || types.cell(stmt) {
            continue;
        }
        let Some(t) = types.of(&b.value).first() else { continue };
        if !concrete(t) || !t.with_deref(|d| matches!(d, Some(Type::Primitive(_)))) {
            continue;
        }
        let Some(u) = disjoint_literal(t) else { continue };
        // a writer right after the let
        let write = ExprKind::Connect {
            name: ModPath::from([v.name.as_str()]),
            value: Arc::new(u.clone()),
            deref: false,
        }
        .to_expr_nopos();
        let mut with_writer: Vec<Expr> = stmts.to_vec();
        with_writer.insert(si + 1, write);
        let cand = ExprKind::Block { exprs: Arc::from_iter(with_writer) }.to_expr_nopos();
        out.extend(finish(Family::Retype, at, &cand, |back, _| {
            vec![nth_stmt(back, si).and_then(span), nth_stmt(back, si + 1).and_then(span)]
        }));
        taken += 1;
        // the initializer retyped, where a reached use is pinned by a literal
        let (mut calls, mut refs) = (Vec::new(), Vec::new());
        let mut idx = offset;
        for later in &stmts[si + 1..] {
            uses_reached(later, v, true, &mut idx, &mut calls, &mut refs);
            if typemorph::binds(later, v) {
                break;
            }
        }
        let pinned = pre.iter().enumerate().find_map(|(j, n)| {
            let (op, lhs, rhs) = BinOp::of(&n.kind)?;
            let is_v = |e: &Expr| matches!(&e.kind, ExprKind::Ref { name } if name.to_string() == **v);
            let lit = |e: &Expr| matches!(e.kind, ExprKind::Constant(_));
            let use_at = match (is_v(lhs), is_v(rhs)) {
                (true, false) if lit(rhs) => j + 1,
                (false, true) if lit(lhs) => j + 1 + sizes[j + 1],
                _ => return None,
            };
            (arith_or_compare(op) && refs.contains(&use_at)).then_some(j)
        });
        if let Some(j) = pinned {
            let cand = mutate::replace(root, at + 1, &u);
            let sites = affected_stmts(stmts, si, v);
            out.extend(finish(Family::Retype, j, &cand, |back, _| {
                sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
            }));
        }
    }
}

/// The statement `si` (a `let v = ..`) and every later one, up to a
/// rebinding of `v`, that reads or writes `v` or a `let` built from it: a
/// change to `v`'s type meets all of them, and the checker reports
/// whichever it reaches first.
fn affected_stmts(stmts: &[Expr], si: usize, v: &str) -> Vec<usize> {
    let mut affected: Vec<&str> = vec![v];
    let mut sites = vec![si];
    for (k, later) in stmts.iter().enumerate().skip(si + 1) {
        let mentions = later.fold(false, &mut |m, n| {
            m || matches!(&n.kind, ExprKind::Ref { name } | ExprKind::Connect { name, .. }
                if affected.iter().any(|a| name.to_string() == *a))
        });
        if mentions {
            sites.push(k);
            if let ExprKind::Bind(lb) = &later.kind
                && let StructurePattern::Bind(w) = &lb.pattern
            {
                affected.push(w.name.as_str());
            }
        }
        if typemorph::binds(later, v) {
            break;
        }
    }
    sites
}

/// Family 4 through a `let` (the hop): an unannotated `let w = e` over a
/// concrete type whose variable a reached use puts directly under an
/// arithmetic or comparison operator or a field read gets `e` widened.
/// An unannotated `let` takes its initializer's type, so `w` carries the
/// union to the consumer. Right site: the let and every statement `w`'s
/// new type meets.
fn widen_through_let(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let mut offset = 1usize;
    let mut taken = 0usize;
    for (si, stmt) in stmts.iter().enumerate() {
        let at = offset;
        offset += sizes[at];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(w), None, false) = (&b.pattern, &b.typ, b.rec) else {
            continue;
        };
        if matches!(b.value.kind, ExprKind::Lambda(_)) || types.cell(stmt) {
            continue;
        }
        let Some(t) = types.of(&b.value).first() else { continue };
        if !concrete(t) {
            continue;
        }
        let Some(u) = disjoint_literal(t) else { continue };
        let (mut calls, mut refs) = (Vec::new(), Vec::new());
        let mut idx = offset;
        for later in &stmts[si + 1..] {
            uses_reached(later, w, true, &mut idx, &mut calls, &mut refs);
            if typemorph::binds(later, w) {
                break;
            }
        }
        let struct_t = t.with_deref(|d| matches!(d, Some(Type::Struct(_))));
        let consumed = pre.iter().enumerate().any(|(j, n)| match &n.kind {
            k if BinOp::of(k).is_some_and(|(op, _, _)| arith_or_compare(op)) => {
                refs.contains(&(j + 1)) || refs.contains(&(j + 1 + sizes[j + 1]))
            }
            ExprKind::StructRef { .. } => struct_t && refs.contains(&(j + 1)),
            _ => false,
        });
        if !consumed {
            continue;
        }
        let cand = mutate::replace(root, at + 1, &widen(&b.value, u));
        let sites = affected_stmts(stmts, si, w);
        out.extend(finish(Family::WidenConsumer, at, &cand, |back, _| {
            sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
        }));
        taken += 1;
    }
}

/// Family 1. A generalized lambda (`let f = |..| ..`) passed as a VALUE
/// at two uses whose instances take different primitive first parameters
/// is wrapped so the binding is not generalized. A call instantiates its
/// callee whatever the binding, but a value reference to a binding that
/// is not generalized holds the binding's own cells, so the two uses
/// meet in one instance and cannot both hold.
fn mono_reuse(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let mut offset = 1usize;
    let mut taken = 0usize;
    for (si, stmt) in stmts.iter().enumerate() {
        let at = offset;
        offset += sizes[at];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(f), ExprKind::Lambda(_), false) =
            (&b.pattern, &b.value.kind, b.rec)
        else {
            continue;
        };
        let (mut calls, mut refs) = (Vec::new(), Vec::new());
        let mut stmt_of: Vec<usize> = Vec::new();
        let mut idx = offset;
        for (j, later) in stmts.iter().enumerate().skip(si + 1) {
            let before = refs.len();
            uses_reached(later, f, true, &mut idx, &mut calls, &mut refs);
            stmt_of.extend(std::iter::repeat_n(j, refs.len() - before));
            if typemorph::binds(later, f) {
                break;
            }
        }
        // a callee's reference sits right after its call in preorder
        let values: Vec<(usize, usize)> = refs
            .iter()
            .zip(stmt_of.iter())
            .filter(|(r, _)| !calls.iter().any(|c| c + 1 == **r))
            .map(|(r, j)| (*r, *j))
            .collect();
        let firsts: Vec<(Typ, usize)> = values
            .iter()
            .filter_map(|(r, j)| {
                Some((types.of(&pre[*r]).first().and_then(first_param)?, *j))
            })
            .collect();
        let Some(((_, ja), (_, jb))) = firsts.iter().enumerate().find_map(|(n, a)| {
            firsts[n + 1..].iter().find(|b| b.0 != a.0).map(|b| (a, b))
        }) else {
            continue;
        };
        let wrapped = ExprKind::Block {
            exprs: Arc::from_iter([
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: false,
                    pattern: StructurePattern::Bind(Name::from("tm__0")),
                    typ: None,
                    value: ExprKind::Constant(netidx_value::Value::Null).to_expr_nopos(),
                }))
                .to_expr_nopos(),
                b.value.clone(),
            ]),
        }
        .to_expr_nopos();
        let cand = mutate::replace(root, at + 1, &wrapped);
        let (ja, jb) = (*ja, *jb);
        let probe = finish(Family::MonoReuse, at, &cand, |back, _| {
            [si, ja, jb].into_iter().map(|j| nth_stmt(back, j).and_then(span)).collect()
        });
        out.extend(probe);
        taken += 1;
    }
}

/// The calls of `f` and the references to `f` (preorder indices from
/// `idx`) that `f`'s binding reaches under `e`; a call's own callee is
/// among the references.
fn uses_reached(
    e: &Expr,
    f: &str,
    reached: bool,
    idx: &mut usize,
    calls: &mut Vec<usize>,
    refs: &mut Vec<usize>,
) {
    let at = *idx;
    *idx += 1;
    if reached {
        match &e.kind {
            ExprKind::Apply(ap) if matches!(&ap.function.kind, ExprKind::Ref { name } if name.to_string() == f) => {
                calls.push(at)
            }
            ExprKind::Ref { name } if name.to_string() == f => refs.push(at),
            _ => (),
        }
    }
    match &e.kind {
        ExprKind::Block { exprs } => {
            let mut reached = reached;
            for st in exprs.iter() {
                uses_reached(st, f, reached, idx, calls, refs);
                reached &= !typemorph::binds(st, f);
            }
        }
        _ => {
            let reached = reached && !typemorph::binds(e, f);
            e.for_each_child(&mut |c| uses_reached(c, f, reached, idx, calls, refs));
        }
    }
}

/// Family 5. A select over a set of variants with no catch-all and no
/// type-test arm gets a scrutinee widened by a tag it never names: the
/// new member is not covered.
fn variant_widen(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let mut taken = 0usize;
    for (i, e) in pre.iter().enumerate() {
        if taken >= cap {
            break;
        }
        let ExprKind::Select(SelectExpr { arg, arms }) = &e.kind else { continue };
        let covers_all = arms.iter().any(|(p, _)| {
            p.type_predicate.is_some()
                || matches!(
                    p.structure_predicate,
                    StructurePattern::Bind(_) | StructurePattern::Ignore
                )
        });
        if covers_all {
            continue;
        }
        let Some(t) = types.of(arg).first() else { continue };
        if !only_variants(t) {
            continue;
        }
        let fresh =
            ExprKind::Variant { tag: ArcStr::from("Tm__1"), args: Arc::from_iter([]) };
        let widened = widen(arg, fresh.to_expr_nopos());
        let cand = mutate::replace(root, i + 1, &widened);
        // the select keeps its preorder index: its scrutinee comes after it
        let probe = finish(Family::VariantWiden, i, &cand, |_, pre| {
            vec![pre.get(i).and_then(span)]
        });
        out.extend(probe);
        taken += 1;
    }
}

/// Whether every member of `t` is a variant (a bare one, or a set of
/// them).
fn only_variants(t: &Type) -> bool {
    let variant = |m: &Type| m.with_deref(|d| matches!(d, Some(Type::Variant(..))));
    t.with_deref(|d| match d {
        Some(Type::Variant(..)) => true,
        Some(Type::Set(ts)) => !ts.is_empty() && ts.iter().all(variant),
        _ => false,
    })
}

fn bool_arm(b: bool) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Literal(netidx_value::Value::Bool(b)),
        guard: None,
    }
}
