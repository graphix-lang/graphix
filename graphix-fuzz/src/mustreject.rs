//! Must-reject mutation (`design/must_reject.md`): take a program the
//! checker accepts and its checked types, apply ONE mutation a rule says
//! the checker must refuse, and require the refusal where the mutation
//! or its rigid consumer is. Families 1 (monomorphic reuse) and 5
//! (variant widening).

use crate::{mutate, typemorph};
use ahash::AHashMap;
use arcstr::ArcStr;
use graphix_compiler::{
    SourcePosition,
    expr::{
        BindExpr, Expr, ExprKind, Name, Origin, Pattern, SelectExpr, Source,
        StructurePattern,
    },
    ide::ExprTypeSite,
    typ::Type,
};
use netidx_value::Typ;
use triomphe::Arc;

/// A must-reject family.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Family {
    MonoReuse,
    VariantWiden,
}

impl std::fmt::Display for Family {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Family::MonoReuse => "mono-reuse",
            Family::VariantWiden => "variant-widen",
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
pub struct TypeMap(AHashMap<(i32, i32, i32, i32), Vec<Type>>);

impl TypeMap {
    /// From a check's sites in the subject's module `module`, whose first
    /// line carries `body_col` characters before the body.
    pub fn new(sites: &[ExprTypeSite], module: &str, body_col: usize) -> Self {
        let mut map: AHashMap<(i32, i32, i32, i32), Vec<Type>> = AHashMap::new();
        for s in sites.iter().filter(|s| in_module(&s.ori, module)) {
            let Some(end) = s.end else { continue };
            let (a, b) = (to_body(s.pos, body_col), to_body(end, body_col));
            map.entry((a.line, a.column, b.line, b.column))
                .or_default()
                .push(s.typ.clone());
        }
        TypeMap(map)
    }

    fn of(&self, e: &Expr) -> &[Type] {
        match e.end.get() {
            None => &[],
            Some(end) => self
                .0
                .get(&(e.pos.line, e.pos.column, end.line, end.column))
                .map_or(&[], |v| &v[..]),
        }
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
    variant_widen(&root, &pre, types, cap, &mut out);
    out
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
        let widened = ExprKind::Select(SelectExpr {
            arg: Arc::new(
                ExprKind::Eq {
                    lhs: Arc::new(
                        ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos(),
                    ),
                    rhs: Arc::new(
                        ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos(),
                    ),
                }
                .to_expr_nopos(),
            ),
            arms: Arc::from_iter([
                (bool_arm(true), (**arg).clone()),
                (
                    bool_arm(false),
                    ExprKind::Variant {
                        tag: ArcStr::from("Tm__1"),
                        args: Arc::from_iter([]),
                    }
                    .to_expr_nopos(),
                ),
            ]),
        })
        .to_expr_nopos();
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
