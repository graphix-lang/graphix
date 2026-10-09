//! Must-reject mutation (`design/must_reject.md`): take a program the
//! checker accepts and its checked types, apply ONE mutation a rule says
//! the checker must refuse, and require the refusal where the mutation
//! or its rigid consumer is. Families 1 (monomorphic reuse), 2 (rigid
//! variables), 3 (a shared variable at a call), 4 (widening into a rigid
//! consumer), 5 (variant widening), 6 (retyping a let), 7 (labels), 8
//! (a function bound), 9 (writable references) and 10 (one runtime
//! form).

use crate::{mutate, typemorph};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use graphix_compiler::{
    SourcePosition,
    expr::{
        ApplyExpr, ArgKind, BinOp, BindExpr, Expr, ExprKind, LambdaBody, LambdaExpr,
        ModPath, Name, Origin, Pattern, SelectExpr, Source, StructurePattern, WrittenAt,
        parser,
    },
    ide::ExprTypeSite,
    typ::{FnArgType, FnType, Mutability, Type},
};
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
    FunctionBound,
    RefWrite,
    RefWiden,
    SameForm,
}

impl Family {
    pub const ALL: [Family; 13] = [
        Family::MonoReuse,
        Family::RigidVar,
        Family::SharedVar,
        Family::LabelUnknown,
        Family::LabelMissing,
        Family::LabelDefault,
        Family::WidenConsumer,
        Family::VariantWiden,
        Family::Retype,
        Family::FunctionBound,
        Family::RefWrite,
        Family::RefWiden,
        Family::SameForm,
    ];
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
            Family::FunctionBound => "function-bound",
            Family::RefWrite => "ref-write",
            Family::RefWiden => "ref-widen",
            Family::SameForm => "same-form",
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
    /// The expected sites' shapes ([`shape`]): what an accepted mutant
    /// slipped past, so one family's leaks split by what they reached.
    pub reached: String,
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
    let expect: Vec<Span> = expect(&back, &pre).into_iter().flatten().collect();
    let mut shapes: Vec<&str> = expect
        .iter()
        .filter_map(|sp| pre.iter().find(|e| span(e) == Some(*sp)).map(shape))
        .collect();
    shapes.dedup();
    let reached = shapes.join(", ");
    Some(RejectProbe { family, site, body, expect, reached })
}

/// An expected site by its kind: the operator, or what it is.
fn shape(e: &Expr) -> &'static str {
    match &e.kind {
        k if let Some((op, ..)) = BinOp::of(k) => op.token(),
        ExprKind::Apply(_) => "call",
        ExprKind::StructRef { .. } => "field read",
        ExprKind::Select(_) => "select",
        ExprKind::Bind(_) => "let",
        ExprKind::Connect { .. } => "write",
        ExprKind::Lambda(_) => "lambda",
        _ => "expression",
    }
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
    function_bound(&root, &pre, types, cap, &mut out);
    references(&root, types, cap, &mut out);
    same_form(&root, &pre, cap, &mut out);
    out
}

/// Family 10. A select with an arm `string as x` gets its scrutinee
/// widened by `` `TmSame `` (and a final `_ => never()` arm when it has
/// none, so the select stays exhaustive): a bare variant and a string have
/// one runtime form, so no arm may tell them apart. Right site: the select.
fn same_form(root: &Expr, pre: &[Expr], cap: usize, out: &mut Vec<RejectProbe>) {
    let mut taken = 0usize;
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        let ExprKind::Select(se) = &e.kind else { continue };
        let tests_string = se.arms.iter().any(|(p, _)| {
            matches!(&p.type_predicate, Some(Type::Primitive(t)) if t.contains(Typ::String))
        });
        if !tests_string || binds_outward(&se.arg) {
            continue;
        }
        let tag =
            ExprKind::Variant { tag: ArcStr::from("TmSame"), args: Arc::from_iter([]) }
                .to_expr_nopos();
        let mut arms = se.arms.to_vec();
        let wild = arms.last().is_some_and(|(p, _)| {
            p.guard.is_none()
                && p.type_predicate.is_none()
                && matches!(p.structure_predicate, StructurePattern::Ignore)
        });
        if !wild {
            let ignore = Pattern {
                type_predicate: None,
                structure_predicate: StructurePattern::Ignore,
                guard: None,
                pos: WrittenAt::NOWHERE,
                end: WrittenAt::NOWHERE,
            };
            let never =
                ExprKind::Never { typ: None, args: Arc::from_iter([]) }.to_expr_nopos();
            arms.push((ignore, never));
        }
        let sel = ExprKind::Select(SelectExpr {
            arg: Arc::new(widen(&se.arg, tag)),
            arms: Arc::from_iter(arms),
        })
        .to_expr(e.pos);
        let cand = mutate::replace(root, i, &sel);
        out.extend(finish(Family::SameForm, i, &cand, |_, pre| {
            vec![pre.get(i).and_then(span)]
        }));
        taken += 1;
    }
}

/// Family 9. A statement `let r = &mut x` over a binding `x`. (a) When a
/// later statement writes `*r <- ..`, the `&mut` becomes `&`: a write
/// needs a writable reference. Right site: the write. (b) When the map
/// shows `x` a concrete primitive, the let is annotated `&mut Any`: a
/// writable reference is invariant. Right site: the let.
fn references(root: &Expr, types: &TypeMap, cap: usize, out: &mut Vec<RejectProbe>) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let (mut writes, mut widens) = (0usize, 0usize);
    for (si, stmt) in stmts.iter().enumerate() {
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(r), None) = (&b.pattern, &b.typ) else { continue };
        let ExprKind::ByRef(Mutability::Mut, x) = &b.value.kind else { continue };
        if !matches!(x.kind, ExprKind::Ref { .. }) {
            continue;
        }
        let rebind = |typ: Option<Type>, value: Expr| {
            let mut stmts = stmts.to_vec();
            stmts[si] =
                ExprKind::Bind(Arc::new(BindExpr { typ, value, ..(**b).clone() }))
                    .to_expr(stmt.pos);
            ExprKind::Block { exprs: Arc::from_iter(stmts) }.to_expr_nopos()
        };
        let later = &stmts[si + 1..];
        let writer = later.iter().position(|s| {
            matches!(&s.kind, ExprKind::Connect { name, deref: true, .. }
                if name.to_string() == *r.name)
        });
        if let Some(w) = writer
            && writes < cap
            && !later[..w].iter().any(|s| typemorph::binds_after(s, &r.name))
        {
            let shared =
                ExprKind::ByRef(Mutability::Shared, x.clone()).to_expr(b.value.pos);
            let cand = rebind(None, shared);
            out.extend(finish(Family::RefWrite, si, &cand, |back, _| {
                vec![nth_stmt(back, si + 1 + w).and_then(span)]
            }));
            writes += 1;
        }
        let pinned =
            types.of(x).first().is_some_and(|t| concrete(t) && primitive(t).is_some());
        if pinned && widens < cap {
            let any = Type::ByRef(Mutability::Mut, Arc::new(Type::Any));
            let cand = rebind(Some(any), b.value.clone());
            out.extend(finish(Family::RefWiden, si, &cand, |back, _| {
                vec![nth_stmt(back, si).and_then(span)]
            }));
            widens += 1;
        }
    }
}

/// Does `e` bind a name its surroundings see: a dynamic module outside
/// any block or lambda of its own? Under a select arm the name would be
/// scoped.
fn binds_outward(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::Module { .. } => true,
        ExprKind::Block { .. } | ExprKind::Lambda(_) => false,
        _ => {
            let mut any = false;
            e.for_each_child(&mut |c| any = any || binds_outward(c));
            any
        }
    }
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

fn literal(e: &Expr) -> bool {
    matches!(e.kind, ExprKind::Constant(_))
}

/// A comparison holds two operands of one type, so it refuses a widened
/// operand only when the other side cannot take the union too: a
/// literal. The other side may derive from the widened value (`v < v`,
/// a capture of it) or be an open cell the comparison binds.
fn comparison(op: BinOp) -> bool {
    use BinOp::*;
    matches!(op, Eq | Ne | Lt | Gt | Lte | Gte)
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

/// A block's statements, each with its index and its preorder index.
fn statements<'a>(root: &'a Expr, sizes: &[usize]) -> Vec<(usize, usize, &'a Expr)> {
    let ExprKind::Block { exprs } = &root.kind else { return Vec::new() };
    let mut at = 1;
    exprs
        .iter()
        .enumerate()
        .map(|(si, e)| {
            let s = (si, at, e);
            at += sizes[at];
            s
        })
        .collect()
}

/// The uses of `f` its binding at statement `si` reaches in the
/// statements after it, up to a rebinding: the calls, and the references
/// (a call's callee among them) with the statement each is in, by
/// preorder index.
fn reached_uses(
    stmts: &[(usize, usize, &Expr)],
    si: usize,
    f: &str,
) -> (Vec<usize>, Vec<(usize, usize)>) {
    let (mut calls, mut refs) = (Vec::new(), Vec::new());
    for &(k, at, later) in stmts.iter().skip(si + 1) {
        let mut idx = at;
        let mut here = Vec::new();
        uses_reached(later, f, true, &mut idx, &mut calls, &mut here);
        refs.extend(here.into_iter().map(|r| (r, k)));
        if typemorph::binds_after(later, f) {
            break;
        }
    }
    (calls, refs)
}

/// Each argument of `ap` with its preorder index (`i` is the call's)
/// and the parameter of `ft` it meets.
fn paired<'a, 'b>(
    ft: &'a FnType,
    ap: &'b ApplyExpr,
    i: usize,
    sizes: &[usize],
) -> Vec<(usize, &'b Expr, Option<&'a FnArgType>)> {
    let mut positional = ft.args.iter().filter(|a| a.is_positional());
    ap.args
        .iter()
        .zip(arg_indices(i, sizes, ap.args.len()))
        .map(|((label, arg), at)| {
            let param = match label {
                Some(l) => ft.args.iter().find(|a| a.label() == Some(l)),
                None => positional.next(),
            };
            (at, arg, param)
        })
        .collect()
}

/// `0..n` in an order that spreads its first picks over the range (the
/// bit-reversal permutation): a family takes its first `cap` sites, and
/// in preorder they cluster at a long subject's start.
fn spread(n: usize) -> impl Iterator<Item = usize> {
    let bits = usize::BITS - n.saturating_sub(1).leading_zeros();
    (0..(1usize << bits))
        .map(
            move |k| if bits == 0 { k } else { k.reverse_bits() >> (usize::BITS - bits) },
        )
        .filter(move |k| *k < n)
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

/// A first statement that fixes one of a lambda's declared variables,
/// as its value and annotation: `x == 1` for a parameter `x: 'a`, or
/// under `bottom` `x` annotated `_`; `f(1)` for a parameter `f: fn(x:
/// 'a) -> ..` whose `'a` is not its own quantifier.
fn rigid_probe(l: &LambdaExpr, bottom: bool) -> Option<(Expr, Option<Type>)> {
    let one = || ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos();
    let name_ref = |x: &Name| {
        Arc::new(ExprKind::Ref { name: ModPath::from([x.name.as_str()]) }.to_expr_nopos())
    };
    l.args.iter().filter(|a| !a.kind.is_labeled()).find_map(|a| {
        let StructurePattern::Bind(x) = &a.pattern else { return None };
        match a.constraint.as_ref()? {
            Type::TVar(_) if bottom => Some(((*name_ref(x)).clone(), Some(Type::Bottom))),
            Type::TVar(_) => Some((
                ExprKind::Eq { lhs: name_ref(x), rhs: Arc::new(one()) }.to_expr_nopos(),
                None,
            )),
            Type::Fn(ft)
                if ft.vargs.is_none()
                    && ft.args.len() == 1
                    && ft.args[0].is_positional()
                    && matches!(&ft.args[0].typ, Type::TVar(tv)
                        if !ft.quantifiers.contains(&tv.name)) =>
            {
                Some((call(name_ref(x), vec![(None, one())]), None))
            }
            _ => None,
        }
    })
}

/// Family 2. A lambda with a parameter over a declared variable `'a`
/// gets a first statement fixing `'a` to `i64` or to `_`
/// ([`rigid_probe`], alternately): a def's declared variables are rigid
/// in its body, so `'a` can become neither. Right site: the definition.
fn rigid_var(root: &Expr, cap: usize, out: &mut Vec<RejectProbe>) {
    let sizes = mutate::sizes(root);
    let stmts = statements(root, &sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let ExprKind::Lambda(l) = &b.value.kind else { continue };
        let LambdaBody::Expr(body) = &l.body else { continue };
        let Some((test, typ)) = rigid_probe(l, taken % 2 == 1) else { continue };
        let body = ExprKind::Block {
            exprs: Arc::from_iter([
                ExprKind::Bind(Arc::new(BindExpr {
                    rec: false,
                    pattern: StructurePattern::Bind(Name::from("tm__0")),
                    typ,
                    value: test,
                }))
                .to_expr_nopos(),
                body.clone(),
            ]),
        }
        .to_expr_nopos();
        let lambda = ExprKind::Lambda(Arc::new(LambdaExpr {
            body: LambdaBody::Expr(body),
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
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
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

/// Family 8. A call argument whose parameter is `'a: Function` (a builtin
/// that wraps a function) replaced by a literal, or by a function with a
/// defaulted label: the bound admits only a function type a generated call
/// passes every argument of (`Type::function_holds`). Right site: the call.
fn function_bound(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let bounded = |t: &Type| match t {
        Type::TVar(tv) => {
            tv.cell_constraints().iter().any(|c| matches!(c, Type::Function))
        }
        _ => false,
    };
    let sizes = mutate::sizes(root);
    let mut taken = 0usize;
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        let ExprKind::Apply(ap) = &e.kind else { continue };
        let Some(ft) = callee_type(types, ap) else { continue };
        let found = paired(&ft, ap, i, &sizes).into_iter().find(|(_, arg, param)| {
            !binds_outward(arg) && param.is_some_and(|p| bounded(&p.typ))
        });
        let Some((at, _, _)) = found else { continue };
        let lit = ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos();
        let defaulted = parser::parse_one("|#d: i64 = 0| d").expect("a lambda");
        for arg in [lit, defaulted] {
            let cand = mutate::replace(root, at, &arg);
            out.extend(finish(Family::FunctionBound, i, &cand, |_, pre| {
                vec![pre.get(i).and_then(span)]
            }));
        }
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
    for i in spread(pre.len()) {
        let e = &pre[i];
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
        // an argument that binds outward takes a name the rest reads
        let required = ap.args.iter().position(|(l, e)| {
            !binds_outward(e)
                && l.as_ref().is_some_and(|l| {
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
    let stmts = statements(root, sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(f), ExprKind::Lambda(l)) =
            (&b.pattern, &b.value.kind)
        else {
            continue;
        };
        let (_, refs) = reached_uses(&stmts, si, f);
        // a value use of `f` as an argument, and the parameter it meets
        let expected = pre.iter().enumerate().find_map(|(i, n)| {
            let ExprKind::Apply(ap) = &n.kind else { return None };
            let ft = callee_type(types, ap)?;
            paired(&ft, ap, i, sizes).into_iter().find_map(|(ix, _, param)| {
                refs.iter()
                    .any(|(r, _)| *r == ix)
                    .then(|| param.map(|p| p.typ.clone()))?
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
            matches!((&a.kind, &a.pattern), (ArgKind::Defaulted(_), StructurePattern::Bind(n))
                if expected.args.iter().find(|e| e.label().map(|x| &**x) == Some(n.name.as_str()))
                    .is_none_or(|e| e.has_default()))
        }) else {
            continue;
        };
        let mut args = l.args.to_vec();
        args[k].kind = ArgKind::Labeled;
        let lambda = ExprKind::Lambda(Arc::new(LambdaExpr {
            args: Arc::from_iter(args),
            ..(**l).clone()
        }))
        .to_expr_nopos();
        let cand = mutate::replace(root, at + 1, &lambda);
        let mut sites = vec![si];
        sites.extend(stmts.iter().skip(si + 1).filter(|(_, _, st)| {
            st.fold(false, &mut |m, n| m || matches!(&n.kind, ExprKind::Ref { name } if name.to_string() == **f))
        }).map(|(k, _, _)| *k));
        out.extend(finish(Family::LabelDefault, at, &cand, |back, _| {
            sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
        }));
        taken += 1;
    }
}

/// Family 4. A value directly under a rigid consumer is widened by a
/// literal of a type its own does not relate to: an arithmetic operand
/// or a comparison operand opposite a literal (exactly one type, each
/// containing the other), a field read's source (a union with a non-struct is refused), or an
/// argument whose parameter is concretely typed (containment) and not
/// inferred, by a literal outside the parameter's type.
fn widen_consumer(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    // a lambda with an unannotated parameter: the parameter may be a cell
    // shared with the environment (`|y| z <- y` over `let z = never()`),
    // which a call widens rather than refuses
    let inferred: AHashSet<&str> = pre
        .iter()
        .filter_map(|e| match &e.kind {
            ExprKind::Bind(b) => match (&b.pattern, &typemorph::unparen(&b.value).kind) {
                (StructurePattern::Bind(f), ExprKind::Lambda(l))
                    if l.args.iter().any(|a| a.constraint.is_none()) =>
                {
                    Some(f.as_str())
                }
                _ => None,
            },
            _ => None,
        })
        .collect();
    let mut taken = 0usize;
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        // the consumer's own type, where it is wider than the value's (a
        // parameter): the widening literal must lie outside it
        let target: Option<(usize, &Expr, Option<Type>)> = match &e.kind {
            k if let Some((op, lhs, rhs)) = BinOp::of(k)
                && arith_or_compare(op) =>
            {
                match comparison(op) {
                    false => Some((i + 1, &**lhs, None)),
                    true if literal(rhs) => Some((i + 1, &**lhs, None)),
                    true if literal(lhs) => Some((i + 1 + sizes[i + 1], &**rhs, None)),
                    true => None,
                }
            }
            ExprKind::StructRef { source, .. } => {
                let is_struct = types.of(source).first().is_some_and(|t| {
                    t.with_deref(|d| matches!(d, Some(Type::Struct(_))))
                });
                is_struct.then(|| (i + 1, &**source, None))
            }
            // a callee whose type its uses decided (a monomorphic value's
            // cell) has the parameter this very call gave it
            ExprKind::Apply(ap) if types.cell(&ap.function) => None,
            ExprKind::Apply(ap)
                if matches!(&typemorph::unparen(&ap.function).kind,
                    ExprKind::Ref { name } if inferred.contains(name.to_string().as_str())) =>
            {
                None
            }
            ExprKind::Apply(ap) => {
                // the first argument whose parameter is a concrete primitive
                let Some(ft) = callee_type(types, ap) else { continue };
                paired(&ft, ap, i, &sizes).into_iter().find_map(|(at, arg, param)| {
                    let p = param?;
                    (concrete(&p.typ)
                        && p.typ.with_deref(|d| matches!(d, Some(Type::Primitive(_)))))
                    .then(|| (at, arg, Some(p.typ.clone())))
                })
            }
            _ => None,
        };
        let Some((at, value, consumer)) = target else { continue };
        if binds_outward(value) {
            continue;
        }
        let Some(t) = types.of(value).first() else { continue };
        if !concrete(t) {
            continue;
        }
        let Some(u) = disjoint_literal(consumer.as_ref().unwrap_or(t)) else { continue };
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
    let ExprKind::Block { exprs: block } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let stmts = statements(root, &sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
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
        let mut with_writer: Vec<Expr> = block.to_vec();
        with_writer.insert(si + 1, write);
        let cand = ExprKind::Block { exprs: Arc::from_iter(with_writer) }.to_expr_nopos();
        out.extend(finish(Family::Retype, at, &cand, |back, _| {
            vec![nth_stmt(back, si).and_then(span), nth_stmt(back, si + 1).and_then(span)]
        }));
        taken += 1;
        // the initializer retyped, where a reached use is pinned by a literal
        let (_, refs) = reached_uses(&stmts, si, v);
        let pinned = pre.iter().enumerate().find_map(|(j, n)| {
            let (op, lhs, rhs) = BinOp::of(&n.kind)?;
            let is_v = |e: &Expr| matches!(&e.kind, ExprKind::Ref { name } if name.to_string() == **v);
            let lit = |e: &Expr| matches!(e.kind, ExprKind::Constant(_));
            let use_at = match (is_v(lhs), is_v(rhs)) {
                (true, false) if lit(rhs) => j + 1,
                (false, true) if lit(lhs) => j + 1 + sizes[j + 1],
                _ => return None,
            };
            (arith_or_compare(op) && refs.iter().any(|(r, _)| *r == use_at)).then_some(j)
        });
        if let Some(j) = pinned {
            let cand = mutate::replace(root, at + 1, &u);
            let sites = affected_stmts(block, si, v);
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
            if let ExprKind::Bind(lb) = &later.kind {
                lb.pattern.with_names(&mut |n| affected.push(n.as_str()));
            }
        }
        if typemorph::binds_after(later, v) {
            break;
        }
    }
    sites
}

/// Family 4 through a `let` (the hop): an unannotated `let w = e` over a
/// concrete type whose variable a reached use puts directly under an
/// arithmetic operator, a comparison opposite a literal, or a field read
/// gets `e` widened.
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
    let ExprKind::Block { exprs: block } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let stmts = statements(root, &sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
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
        let (_, refs) = reached_uses(&stmts, si, w);
        let refs: Vec<usize> = refs.into_iter().map(|(r, _)| r).collect();
        let struct_t = t.with_deref(|d| matches!(d, Some(Type::Struct(_))));
        let consumed = pre.iter().enumerate().any(|(j, n)| match &n.kind {
            k if let Some((op, lhs, rhs)) = BinOp::of(k)
                && arith_or_compare(op) =>
            {
                let (l, r) =
                    (refs.contains(&(j + 1)), refs.contains(&(j + 1 + sizes[j + 1])));
                match comparison(op) {
                    true => (l && literal(rhs)) || (r && literal(lhs)),
                    false => l || r,
                }
            }
            ExprKind::StructRef { .. } => struct_t && refs.contains(&(j + 1)),
            _ => false,
        });
        if !consumed {
            continue;
        }
        if binds_outward(&b.value) {
            continue;
        }
        let cand = mutate::replace(root, at + 1, &widen(&b.value, u));
        let sites = affected_stmts(block, si, w);
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
    let sizes = mutate::sizes(root);
    let stmts = statements(root, &sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(f), ExprKind::Lambda(_), false) =
            (&b.pattern, &b.value.kind, b.rec)
        else {
            continue;
        };
        let (calls, refs) = reached_uses(&stmts, si, f);
        // a callee's reference sits right after its call in preorder
        let values: Vec<(usize, usize)> = refs
            .into_iter()
            .filter(|(r, _)| !calls.iter().any(|c| c + 1 == *r))
            .collect();
        let firsts: Vec<(Typ, usize)> = values
            .iter()
            .filter_map(|(r, j)| {
                Some((types.of(&pre[*r]).first().and_then(first_param)?, *j))
            })
            .collect();
        let Some((_, (_, jb))) = firsts.iter().enumerate().find_map(|(n, a)| {
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
        let jb = *jb;
        // every value use shares the wrapped binding's cells, so the
        // checker refuses at whichever reached use conflicts first
        let mut sites = vec![si];
        sites.extend(values.iter().map(|(_, j)| *j).filter(|j| *j <= jb));
        sites.dedup();
        let probe = finish(Family::MonoReuse, at, &cand, |back, _| {
            sites.iter().map(|j| nth_stmt(back, *j).and_then(span)).collect()
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
    let is_f =
        |n: &Expr| matches!(&n.kind, ExprKind::Ref { name } if name.to_string() == f);
    typemorph::for_each_reached(e, f, reached, idx, &mut |at, n| match &n.kind {
        ExprKind::Apply(ap) if is_f(&ap.function) => calls.push(at),
        _ if is_f(n) => refs.push(at),
        _ => (),
    });
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
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        let ExprKind::Select(SelectExpr { arg, arms }) = &e.kind else { continue };
        fn catches_any(p: &StructurePattern) -> bool {
            match p {
                StructurePattern::Bind(_) | StructurePattern::Ignore => true,
                StructurePattern::Or(alts) => alts.iter().any(catches_any),
                _ => false,
            }
        }
        let covers_all = arms.iter().any(|(p, _)| {
            p.type_predicate.is_some() || catches_any(&p.structure_predicate)
        });
        if covers_all {
            continue;
        }
        let Some(t) = types.of(arg).first() else { continue };
        if !only_variants(t) || binds_outward(arg) {
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
        structure_predicate: StructurePattern::literal(netidx_value::Value::Bool(b)),
        guard: None,
        pos: WrittenAt::NOWHERE,
        end: WrittenAt::NOWHERE,
    }
}
