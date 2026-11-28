//! Must-reject mutation (`design/must_reject.md`): take a program the
//! checker accepts and its checked types, apply ONE mutation a rule says
//! the checker must refuse, and require the refusal where the mutation
//! or its rigid consumer is. Families 1 (monomorphic reuse), 2 (rigid
//! variables), 3 (a bounded variable at a call), 4 (widening into a rigid
//! consumer), 5 (variant widening), 6 (retyping a let), 7 (labels), 8
//! (a function bound), 9 (writable references), 10 (one runtime form)
//! and 11 (an abstract type's parameter).

use crate::{mutate, typemorph};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::{
    SourcePosition,
    env::Env,
    expr::{
        ApplyExpr, Arg, ArgKind, BinOp, BindExpr, Expr, ExprKind, LambdaBody, LambdaExpr,
        ModPath, Name, Origin, Pattern, SelectExpr, Source, StructurePattern, WrittenAt,
        parser,
    },
    ide::ExprTypeSite,
    typ::{Bound, FnArgType, FnType, Mutability, TVar, Type},
};
use netidx_value::Typ;
use poolshark::local::LPooled;
use triomphe::Arc;

/// A must-reject family.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Family {
    MonoReuse,
    RigidVar,
    SharedVar,
    WidenConsumer,
    WidenHop,
    VariantWiden,
    Retype,
    LabelUnknown,
    LabelMissing,
    LabelDefault,
    FunctionBound,
    RefWrite,
    RefWiden,
    SameForm,
    AbstractParam,
}

impl Family {
    pub const ALL: [Family; 15] = [
        Family::MonoReuse,
        Family::RigidVar,
        Family::SharedVar,
        Family::LabelUnknown,
        Family::LabelMissing,
        Family::LabelDefault,
        Family::WidenConsumer,
        Family::WidenHop,
        Family::VariantWiden,
        Family::Retype,
        Family::FunctionBound,
        Family::RefWrite,
        Family::RefWiden,
        Family::SameForm,
        Family::AbstractParam,
    ];
}

impl Family {
    /// Whether `head`, a refusal's message, is one this family's rule
    /// gives: a refusal at the right site for another reason (a dead arm
    /// the mutation added) would hide the rule's regression.
    pub fn refuses_with(self, head: &str) -> bool {
        let says: &[&str] = match self {
            Family::MonoReuse => &["does not contain", "and no value is both"],
            Family::RigidVar => &[
                "unbound",
                "which the signature does not declare",
                "but the declared variable",
            ],
            // a variable a select tests takes what the select's arms do
            Family::SharedVar => &["does not contain", "missing match cases"],
            Family::AbstractParam => &["does not contain"],
            // a call of a function value the check types says "no argument"
            Family::LabelUnknown => &["unknown labeled argument", "no argument #"],
            Family::LabelMissing => &["missing required argument"],
            Family::LabelDefault => {
                &["missing required argument", "does not contain fn(", "is required"]
            }
            Family::WidenConsumer | Family::WidenHop => &[
                "does not contain",
                "missing match cases",
                "cannot hold",
                "cannot compute",
                "cannot compare",
                "cannot order",
                "expected struct not",
                "is not a struct",
                "must be one type",
                "has no field",
                "the members' fields differ",
                "not decided at this select",
                "not decided at this let",
                "not decided at this parameter",
                "matches more than one member",
            ],
            Family::VariantWiden => &["missing match cases"],
            Family::Retype => &[
                "missing match cases",
                "cannot hold",
                "cannot compute",
                "cannot compare",
                "cannot order",
                "does not contain",
                "will never match",
                "refuses",
            ],
            Family::FunctionBound => &["must be Function here"],
            Family::RefWrite => &["read-only reference", "does not contain &"],
            Family::RefWiden => &["does not contain", "missing match cases"],
            Family::SameForm => &["the same runtime form"],
        };
        says.iter().any(|s| head.contains(s))
    }
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
            Family::WidenHop => "widen-hop",
            Family::VariantWiden => "variant-widen",
            Family::Retype => "retype",
            Family::FunctionBound => "function-bound",
            Family::RefWrite => "ref-write",
            Family::RefWiden => "ref-widen",
            Family::SameForm => "same-form",
            Family::AbstractParam => "abstract-param",
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
    /// The mutant's ordinal among its family's at its site.
    pub nth: usize,
}

impl RejectProbe {
    pub fn id(&self) -> String {
        match self.nth {
            0 => format!("{}#{}", self.family, self.site),
            n => format!("{}#{}.{n}", self.family, self.site),
        }
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
    /// A reference here names a function whose declared type leaves part
    /// of its result to its uses ([`open_return`]).
    open_return: bool,
    /// The declared type of the binding a reference here names: a
    /// generic function's signature, where `types` holds the instance.
    declared: Option<Type>,
}

impl TypeMap {
    /// From a check's sites in the subject's module `module`, and the
    /// check's `env`, which holds each reference's binding.
    pub fn new(sites: &[ExprTypeSite], env: &Env, module: &str) -> Self {
        let mut map: AHashMap<(i32, i32, i32, i32), Entry> = AHashMap::new();
        for s in sites.iter().filter(|s| in_module(&s.ori, module)) {
            let Some(end) = s.end else { continue };
            let (a, b) = (to_body(s.pos), to_body(end));
            let e = map.entry((a.line, a.column, b.line, b.column)).or_default();
            e.types.push(s.typ.clone());
            e.cell |= s.cell;
            let bound = s.bind.and_then(|id| env.by_id.get(&id));
            e.open_return |= bound.is_some_and(|b| open_return(&b.typ));
            if e.declared.is_none() {
                e.declared = bound.map(|b| b.typ.clone())
            }
        }
        TypeMap(map)
    }

    fn entry(&self, e: &Expr) -> Option<&Entry> {
        let end = e.end.get()?;
        self.0.get(&(e.pos.line, e.pos.column, end.line, end.column))
    }

    pub(crate) fn of(&self, e: &Expr) -> &[Type] {
        self.entry(e).map_or(&[], |x| &x.types[..])
    }

    fn cell(&self, e: &Expr) -> bool {
        self.entry(e).is_some_and(|x| x.cell)
    }

    fn open_return(&self, f: &Expr) -> bool {
        self.entry(typemorph::unparen(f)).is_some_and(|x| x.open_return)
    }

    /// The declared function type of the binding the callee `f` names.
    fn declared(&self, f: &Expr) -> Option<Arc<FnType>> {
        let t = self.entry(typemorph::unparen(f))?.declared.as_ref()?;
        t.with_deref(|d| match d {
            Some(Type::Fn(ft)) => Some(ft.clone()),
            _ => None,
        })
    }
}

/// Whether `t` is a function type whose return holds an open cell that no
/// parameter holds, nor the bound of one a parameter holds, nor anything
/// bounds from below (an abstract's parameter, which its payload does):
/// no argument decides it, so a call's uses do (`list::nil`'s element, a
/// type-directed read's `'b: Concrete` target).
fn open_return(t: &Type) -> bool {
    t.with_deref(|d| match d {
        Some(Type::Fn(ft)) => {
            let mut held: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            for p in ft.args.iter().map(|a| &a.typ).chain(&ft.vargs) {
                open_cells(p, true, &mut held)
            }
            let mut ret: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            open_cells(&ft.rtype, false, &mut ret);
            ret.iter().any(|(c, tv)| !held.contains_key(c) && tv.cell_lower().is_empty())
        }
        _ => false,
    })
}

/// The open cells `t` holds, by identity, bindings followed; with
/// `bounds`, those each one's bound holds too.
fn open_cells(t: &Type, bounds: bool, out: &mut AHashMap<usize, TVar>) {
    match t {
        Type::TVar(tv) => match tv.open_cell() {
            Some(c) => {
                if out.insert(c.cell_addr(), c.clone()).is_none() && bounds {
                    for b in c.cell_above() {
                        open_cells(&b, bounds, out)
                    }
                }
            }
            None => {
                if let Some(b) = tv.binding() {
                    open_cells(&b, bounds, out)
                }
            }
        },
        t => t.for_each_child(&mut |c| open_cells(c, bounds, out)),
    }
}

/// Whether `ori` is the VFS module named `module` (its origin is the
/// module's path).
pub fn in_module(ori: &Origin, module: &str) -> bool {
    matches!(&ori.source, Source::Vfs(m) if netidx_core::path::Path::basename(m) == Some(module))
}

/// A module-file position as a body position: the body starts on the
/// module's second line.
pub fn to_body(p: SourcePosition) -> SourcePosition {
    SourcePosition { line: p.line - 1, column: p.column }
}

/// Where each expression of the module `module` that the refusal `e`
/// passed through starts, read off the frames as they print (an error's
/// chain gives a frame's text, not the frame).
pub fn passed(e: &anyhow::Error, module: &str) -> Vec<SourcePosition> {
    let frame = |text: String| -> Option<SourcePosition> {
        let (line, rest) = text.strip_prefix("at: line: ")?.split_once(", column: ")?;
        let digits = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
        let (column, rest) = rest.split_at(digits);
        let rest = rest.strip_prefix(" in module ")?;
        let path = rest.split_once(',').map_or(rest, |(path, _)| path);
        (netidx_core::path::Path::basename(path) == Some(module)).then_some(
            SourcePosition { line: line.parse().ok()?, column: column.parse().ok()? },
        )
    };
    e.chain().filter_map(|c| frame(c.to_string())).collect()
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

/// A function type's first positional parameter, the one a call's first
/// positional argument types.
fn first_param(t: &Type) -> Option<Type> {
    t.with_deref(|d| match d {
        Some(Type::Fn(ft)) => {
            ft.args.iter().find(|a| a.is_positional()).map(|a| a.typ.clone())
        }
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
    Some(RejectProbe { family, site, body, expect, reached, nth: 0 })
}

/// An expected site by its kind: the operator, or what it is.
fn shape(e: &Expr) -> &'static str {
    match &e.kind {
        ExprKind::BinOp { op, .. } => op.token(),
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
/// `modules` names the subject's own file modules.
pub fn probes(
    body: &str,
    types: &TypeMap,
    modules: &[&str],
    cap: usize,
) -> Vec<RejectProbe> {
    if typemorph::holds_synthetic(body) {
        return Vec::new();
    }
    let Some(root) = mutate::parse_body(body) else { return Vec::new() };
    let pre = mutate::preorder(&root);
    let mut out = Vec::new();
    mono_reuse(&root, &pre, types, cap, &mut out);
    rigid_var(&root, cap, &mut out);
    shared_var(&root, &pre, types, cap, &mut out);
    labels(&root, &pre, types, cap, &mut out);
    widen_consumer(&root, &pre, types, modules, cap, &mut out);
    widen_through_let(&root, &pre, types, cap, &mut out);
    variant_widen(&root, &pre, types, cap, &mut out);
    retype(&root, &pre, types, cap, &mut out);
    function_bound(&root, &pre, types, cap, &mut out);
    references(&root, types, cap, &mut out);
    same_form(&root, &pre, types, cap, &mut out);
    abstract_param(&root, &pre, types, cap, &mut out);
    // at most `cap` of each family's, each id its own
    let mut kept: Vec<RejectProbe> = Vec::with_capacity(out.len());
    for mut p in out {
        let family = kept.iter().filter(|k| k.family == p.family);
        if family.clone().count() < cap {
            p.nth = family.filter(|k| k.site == p.site).count();
            kept.push(p);
        }
    }
    kept
}

/// Family 10. A select over a concrete scrutinee `s` of type `T` becomes
/// `select <&s or &u> { &T as r => select *r { arms }, _ => never() }`,
/// `u` a literal of a type disjoint from `T`: two references have one
/// runtime form, so no arm may tell them apart. Right site: the outer
/// select.
fn same_form(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    const R: &str = "tm__ref";
    let mut taken = 0usize;
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        let ExprKind::Select(se) = &e.kind else { continue };
        let Some(t) = types.of(&se.arg).first() else { continue };
        // a type test over a function is refused by its own rule first
        let holds_fn = holds(t, &|d| matches!(d, Type::Fn(_)));
        if !concrete(t) || opaque(t) || holds_fn || typemorph::leaks_binds(&se.arg) {
            continue;
        }
        let Some(u) = disjoint_literal(t) else { continue };
        let by_ref =
            |e: Expr| ExprKind::ByRef(Mutability::Shared, Arc::new(e)).to_expr_nopos();
        let deref = ExprKind::Deref(Arc::new(
            ExprKind::Ref { name: ModPath::from([R]) }.to_expr_nopos(),
        ))
        .to_expr_nopos();
        let inner =
            ExprKind::Select(SelectExpr { arg: Arc::new(deref), arms: se.arms.clone() })
                .to_expr_nopos();
        let test = Pattern {
            type_predicate: Some(Type::ByRef(
                Mutability::Shared,
                Arc::new(t.resolve_tvars()),
            )),
            structure_predicate: StructurePattern::Bind(Name::from(R)),
            guard: None,
            pos: WrittenAt::NOWHERE,
            end: WrittenAt::NOWHERE,
        };
        let ignore = Pattern {
            type_predicate: None,
            structure_predicate: StructurePattern::Ignore,
            guard: None,
            pos: WrittenAt::NOWHERE,
            end: WrittenAt::NOWHERE,
        };
        let never =
            ExprKind::Never { typ: None, args: Arc::from_iter([]) }.to_expr_nopos();
        let sel = ExprKind::Select(SelectExpr {
            arg: Arc::new(widen(&by_ref((*se.arg).clone()), by_ref(u))),
            arms: Arc::from_iter([(test, inner), (ignore, never)]),
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
/// needs a writable reference. Right site: the write, or a statement
/// before it that uses `r` (a call taking `&mut`). (b) When the map
/// shows `x` a concrete primitive and a later statement computes with
/// `x`, the let is annotated `&mut Any`: a writable reference is
/// invariant, so `x` is `Any`, which arithmetic refuses. Right site: the
/// let, a statement that uses `r`, or one from `x`'s own let on that reads
/// `x` or a binding built from it: `x` is an `Any` from where it is bound.
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
            // a use of `r` before the write may need it writable too
            // CR claude for claude: [bug/low] The sites of `&mut x` made `&x` are the
            // write and the statements before it that name `r`, but an alias carries
            // the read-only reference too. In `let p = r; let y = f(p)`, with `f`
            // taking `&mut i64`, the refusal "&mut i64 does not contain &i64" lands at
            // `f(p)` and is filed as MISPLACED. The generator puts its `&mut`
            // references in tuples. Fix: follow the lets built from `r` up to the
            // write, as `affected_stmts` does for retype. probe:
            // design/review-2026-11-16/repro/fuzz-mutate-ref-write-alias.gx
            // (fuzz-mutate-05)
            let uses = (0..w).filter(|&k| {
                later[k].fold(false, &mut |m, n| {
                    m || matches!(&n.kind, ExprKind::Ref { name } if name.to_string() == *r.name)
                })
            });
            let sites: Vec<usize> = uses.chain([w]).collect();
            out.extend(finish(Family::RefWrite, si, &cand, |back, _| {
                sites.iter().map(|k| nth_stmt(back, si + 1 + k).and_then(span)).collect()
            }));
            writes += 1;
        }
        let pinned =
            types.of(x).first().is_some_and(|t| concrete(t) && primitive(t).is_some());
        let ExprKind::Ref { name: referent } = &x.kind else { continue };
        let referent = referent.to_string();
        let is_x = |e: &Expr| matches!(&e.kind, ExprKind::Ref { name } if name.to_string() == referent);
        // an arithmetic use the referent's own binding reaches
        let mut computes = false;
        for s in later {
            typemorph::for_each_reached(s, &referent, true, &mut 0, &mut |_, n| {
                computes |= matches!(&n.kind, ExprKind::BinOp { op, lhs, rhs }
                    if arith_or_compare(*op) && !comparison(*op) && (is_x(lhs) || is_x(rhs)))
            });
            if typemorph::binds_after(s, &referent) {
                break;
            }
        }
        // a referent whose uses decide its type: the annotation would decide it
        if pinned && computes && !open_init(types, stmts, si, x) && widens < cap {
            let any = Type::ByRef(Mutability::Mut, Arc::new(Type::Any));
            let cand = rebind(Some(any), b.value.clone());
            // the reference is a `&mut Any` to whatever takes it, too
            let bound = earlier_let(stmts, si, x).map_or(si, |(j, _)| j);
            let mut sites = affected_stmts(stmts, bound, &referent);
            sites.extend(affected_stmts(stmts, si, &r.name));
            sites.sort_unstable();
            sites.dedup();
            out.extend(finish(Family::RefWiden, si, &cand, |back, _| {
                sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
            }));
            widens += 1;
        }
    }
}

/// Family 11. A statement `let v = e` the map types `B<i64>`, a
/// generated module's parameterized abstract type: the let is annotated
/// `m::B<string>`. A constructed value's parameter holds its payload's
/// type, and an abstract type's parameters are invariant, whatever its
/// rep does with them. The map does not name the module:
/// it is the one whose `mkb`/`unb` the body reaches, or with several,
/// the one whose `mkb` the let calls. A value with a leaf its uses typed
/// is skipped: the annotation would decide it. Right site: the let and
/// every statement that reads its name, or the let of a binding its value
/// reads and every statement that reads that binding (one decided after
/// this let meets the annotation where its own value arrives, or where a
/// later use asks another type of it or of the annotated name).
fn abstract_param(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let ExprKind::Block { exprs: stmts } = &root.kind else { return };
    let ctor_module = |e: &Expr| match &e.kind {
        ExprKind::Ref { name } => {
            let name = name.to_string();
            ["::mkb", "::unb"].iter().find_map(|s| name.strip_suffix(s).map(String::from))
        }
        _ => None,
    };
    let mut modules: Vec<String> = pre.iter().filter_map(ctor_module).collect();
    modules.sort();
    modules.dedup();
    let mut n = 0usize;
    for (si, stmt) in stmts.iter().enumerate() {
        if n >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(_), None) = (&b.pattern, &b.typ) else { continue };
        let boxed_i64 = types.of(&b.value).first().is_some_and(|t| {
            t.to_string() == "B<i64>" && matches!(t, Type::Abstract { .. })
        });
        // an annotation decides a parameter an open initializer left open
        if !boxed_i64 || open_init(types, stmts, si, &b.value) {
            continue;
        }
        let module = match &modules[..] {
            [m] => m.clone(),
            _ => match &b.value.kind {
                ExprKind::Apply(a) => match ctor_module(&a.function) {
                    Some(m) => m,
                    None => continue,
                },
                _ => continue,
            },
        };
        let Ok(widened) = parser::parse_type(&format!("{module}::B<string>")) else {
            continue;
        };
        let StructurePattern::Bind(own) = &b.pattern else { continue };
        let mut sites = affected_stmts(stmts, si, &own.name);
        // the bindings the value reads, and those theirs read: one a `?`
        // or a select still decides takes the refusal at its own let
        let mut read: Vec<(usize, &Expr)> = vec![(si, &b.value)];
        let mut met: Vec<usize> = Vec::new();
        while let Some((before, value)) = read.pop() {
            let mut found: Vec<(usize, &BindExpr)> = Vec::new();
            value.fold((), &mut |(), n| found.extend(earlier_let(stmts, before, n)));
            for (j, from) in found {
                if let StructurePattern::Bind(name) = &from.pattern
                    && !met.contains(&j)
                {
                    met.push(j);
                    sites.extend(affected_stmts(stmts, j, &name.name));
                    read.push((j, &from.value))
                }
            }
        }
        sites.sort_unstable();
        sites.dedup();
        let mut stmts = stmts.to_vec();
        stmts[si] =
            ExprKind::Bind(Arc::new(BindExpr { typ: Some(widened), ..(**b).clone() }))
                .to_expr(stmt.pos);
        let cand = ExprKind::Block { exprs: Arc::from_iter(stmts) }.to_expr_nopos();
        out.extend(finish(Family::AbstractParam, si, &cand, |back, _| {
            sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
        }));
        n += 1;
    }
}

/// `e` widened by `u`: `select (i64:1 == i64:1) { true => e, false => u }`,
/// typed the union of the two whatever the scrutinee's value.
fn widen(e: &Expr, u: Expr) -> Expr {
    let one =
        || Arc::new(ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos());
    ExprKind::Select(SelectExpr {
        arg: Arc::new(
            ExprKind::BinOp { op: BinOp::Eq, lhs: one(), rhs: one() }.to_expr_nopos(),
        ),
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
/// Whether `t` is a set of primitives or names a class of them (`Number`,
/// `Int`, ..): a bound only those primitives meet.
// CR claude for claude: [readability/low] The first two lines of this doc ("Whether a
// type has no cell anywhere, so it cannot bend to a widened argument.") describe a
// deleted function, not primitive_class: delete them. The module doc's family list also
// stops at 11, while this file builds 13 (rigid-pattern), 14 (undecided-part) and 15
// (variable-union). Found by reading; not run. (fuzz-mutate-07)
// 2026-11-19 claude: Family 15 (variable-union) is deleted with its rule (Eric ruled
// 11-19: drop the variable-union rule); the list now stops at 14.
// 2026-11-22 claude: Families 13 (rigid-pattern) and 14 (undecided-part) are deleted
// with their rules (patterns inside a declared variable were cut 11-17; a part nothing
// decides is bottom at a check's end): the module doc's list, ending at 11, is now the
// whole list. The stale first two doc lines above primitive_class still stand.
fn primitive_class(t: &Type) -> bool {
    const CLASSES: [&str; 6] = ["Number", "Real", "Float", "Int", "Uint", "Sint"];
    match t {
        Type::Primitive(_) => true,
        Type::Ref(r) => {
            r.params.is_empty()
                && netidx_core::path::Path::basename(&r.name.0)
                    .is_some_and(|n| CLASSES.contains(&n))
        }
        _ => false,
    }
}

fn concrete(t: &Type) -> bool {
    t.resolve_tvars().tvar_free()
}

/// Whether `t` holds `Any`, an abstract type or a type name anywhere: a
/// part whose runtime form the map does not show.
fn opaque(t: &Type) -> bool {
    holds(t, &|d| matches!(d, Type::Any | Type::Abstract { .. } | Type::Ref(_)))
}

/// Whether the written type `t` names the variable `name`.
fn names(t: &Type, name: &str) -> bool {
    let mut found = matches!(t, Type::TVar(tv) if &*tv.name == name);
    t.for_each_child(&mut |c| found |= names(c, name));
    found
}

/// Whether `t` or a part of it satisfies `p`; a function's parts are not
/// walked.
fn holds(t: &Type, p: &impl Fn(&Type) -> bool) -> bool {
    t.with_deref(|d| match d {
        None => false,
        Some(d) if p(d) => true,
        Some(Type::Set(ts) | Type::Tuple(ts)) => ts.iter().any(|t| holds(t, p)),
        Some(Type::Struct(fs)) => fs.iter().any(|(_, t, _)| holds(t, p)),
        Some(Type::Variant(_, ts, _)) => ts.iter().any(|t| holds(t, p)),
        Some(Type::Array(t) | Type::List(t) | Type::Error(t) | Type::ByRef(_, t)) => {
            holds(t, p)
        }
        Some(Type::Map(m)) => holds(&m.key, p) || holds(&m.value, p),
        Some(_) => false,
    })
}

/// Whether `value`, the initializer of a statement at `si` in `block`, is
/// over ⊥: its type a cell its uses decide, so a writer or an annotation
/// decides it rather than being refused. A call's return is decided by
/// its callee and its arguments, open through an open argument, a callee
/// whose type returns a cell no parameter holds ([`open_return`]), or a
/// callee bound to a lambda with no return type whose body is open or
/// reads an open let (a generalized return, fresh at each call; the
/// captures are typemorph's [`typemorph::open_lets`]); an unannotated let
/// read by name shares its initializer's cell; a block is its last
/// expression; an operator, a negation, a `$`/`?` or a read of a part
/// is open through an operand that is, a constructor through a part, and
/// a `$`/`?` also over a value that is only an error.
fn open_init(types: &TypeMap, block: &[Expr], si: usize, value: &Expr) -> bool {
    if typemorph::open_value(value) {
        return true;
    }
    let open = |e: &Expr| open_init(types, block, si, e);
    match &typemorph::unparen(value).kind {
        ExprKind::Apply(ap) => {
            types.open_return(&ap.function)
                || payload_read(&ap.function)
                || ap.args.iter().any(|(_, a)| open(a))
                || earlier_let(block, si, &ap.function).is_some_and(|(j, d)| {
                    match &typemorph::unparen(&d.value).kind {
                        ExprKind::Lambda(l) => {
                            l.rtype.is_none()
                                && (matches!(&l.body, LambdaBody::Expr(b)
                                    if open_body(types, d, b))
                                    || matches!(&d.pattern, StructurePattern::Bind(n)
                                        if typemorph::open_lets(&block[..=j])
                                            .contains_key(n.name.as_str())))
                        }
                        _ => false,
                    }
                })
        }
        // CR claude for claude: [bug/medium] earlier_let finds the last top-level `let`
        // of a name before the statement, whatever binds that name in between (a select
        // arm, a lambda parameter, an inner let), so open_init judges the wrong
        // binding. Over `let x = 1; let n = never(); let v = select n { x => x }` it
        // calls `v` decided, retype adds `v <- "tm"`, which decides `n`, and the
        // refusal at `n + 1` is filed as MISPLACED. With an annotated top-level `x`,
        // variant-widen widens an arm bind over `n` and is refused by the
        // undecided-part rule, which is filed as WRONG REFUSAL. Generated programs
        // reuse top-level names as arm binds (`bool as v20` beside `let v20`). Fix:
        // resolve the name through the binding in force at the reference
        // (`typemorph::for_each_reached` tracks it), and fall back to `types.cell` when
        // that binding is not the top-level let. probe:
        // design/review-2026-11-16/repro/fuzz-mutate-open-init-shadowed-name.sh
        // (fuzz-mutate-02)
        // 2026-11-22 claude: The undecided-part rule this names is gone (a part nothing
        // decides is bottom at a check's end), so the WRONG REFUSAL half no longer
        // arises; the MISPLACED half (earlier_let judging the wrong binding) stands.
        ExprKind::Ref { .. } => match earlier_let(block, si, value) {
            Some((j, d)) => d.typ.is_none() && open_init(types, block, j, &d.value),
            None => types.cell(value),
        },
        ExprKind::Block { exprs } => {
            exprs.last().is_some_and(|l| open_init(types, exprs, exprs.len() - 1, l))
        }
        ExprKind::Constant(_)
        | ExprKind::Lambda(_)
        | ExprKind::TypeCast { .. }
        | ExprKind::StringInterpolate { .. }
        | ExprKind::Not { .. } => false,
        ExprKind::BinOp { op, rhs, .. } if sample(*op) => open(rhs),
        ExprKind::BinOp { op, lhs, rhs } if arith_or_compare(*op) && !comparison(*op) => {
            open(lhs) || open(rhs)
        }
        // a comparison or a logical operator is a bool
        ExprKind::BinOp { .. } => false,
        // a value that is only what the operator takes off is ⊥
        ExprKind::OrNever(x) | ExprKind::Qop { arg: x, .. } => {
            open(x) || !types.of(x).first().is_some_and(succeeds)
        }
        ExprKind::Neg(x)
        | ExprKind::StructRef { source: x, .. }
        | ExprKind::TupleRef { source: x, .. }
        | ExprKind::ArrayRef { source: x, .. }
        | ExprKind::ArraySlice { source: x, .. }
        | ExprKind::MapRef { source: x, .. } => open(x),
        ExprKind::Array { args }
        | ExprKind::List { args }
        | ExprKind::Tuple { args }
        | ExprKind::Variant { args, .. } => args.iter().any(open),
        ExprKind::Struct(st) => st.args.iter().any(|(_, a)| open(a)),
        ExprKind::Select(se) => {
            !se.arms.is_empty() && se.arms.iter().all(|(_, b)| open(b))
        }
        _ => types.cell(value),
    }
}

/// Whether a definition's body `b` returns what its uses decide: it is
/// open, or a `let rec`'s every branch is open or calls itself again.
fn open_body(types: &TypeMap, d: &BindExpr, b: &Expr) -> bool {
    let StructurePattern::Bind(n) = &d.pattern else { return false };
    let recurses = |e: &Expr| {
        d.rec
            && matches!(&typemorph::unparen(e).kind, ExprKind::Apply(ap)
                if matches!(&typemorph::unparen(&ap.function).kind,
                    ExprKind::Ref { name } if name.to_string() == *n.name))
    };
    match &typemorph::unparen(b).kind {
        ExprKind::Select(se) => {
            !se.arms.is_empty()
                && se.arms.iter().all(|(_, a)| recurses(a) || open_init(types, &[], 0, a))
        }
        _ => open_init(types, &[], 0, b),
    }
}

/// Whether `f` is a generated module's `unb`, which reads a constructed
/// abstract's payload back at its parameter: the payload only bounds the
/// parameter from below, so the uses decide the result's type.
fn payload_read(f: &Expr) -> bool {
    matches!(&typemorph::unparen(f).kind, ExprKind::Ref { name }
        if netidx_core::path::Path::basename(&name.0) == Some("unb"))
}

/// Whether `x?` or `x$` over a value of `t` may produce: they take off
/// its errors, or its null where it holds no error.
fn succeeds(t: &Type) -> bool {
    fn holds_error(t: &Type) -> bool {
        t.with_deref(|d| match d {
            Some(Type::Error(_)) => true,
            Some(Type::Set(ts)) => ts.iter().any(holds_error),
            Some(Type::Primitive(p)) => p.iter().any(|k| k == Typ::Error),
            _ => false,
        })
    }
    fn left(t: &Type, null: bool) -> bool {
        t.with_deref(|d| match d {
            None | Some(Type::Bottom | Type::Error(_)) => false,
            Some(Type::Set(ts)) => ts.iter().any(|t| left(t, null)),
            Some(Type::Primitive(p)) => {
                p.iter().any(|k| k != Typ::Error && (null || k != Typ::Null))
            }
            Some(_) => true,
        })
    }
    left(t, holds_error(t))
}

/// The last `let` of the name `r` reads among the statements of `block`
/// before `si`, with its index.
fn earlier_let<'a>(
    block: &'a [Expr],
    si: usize,
    r: &Expr,
) -> Option<(usize, &'a BindExpr)> {
    let ExprKind::Ref { name } = &typemorph::unparen(r).kind else { return None };
    let name = name.to_string();
    block[..si.min(block.len())].iter().enumerate().rev().find_map(|(j, s)| match &s.kind {
        ExprKind::Bind(d) if matches!(&d.pattern, StructurePattern::Bind(n) if *n.name == name) => {
            Some((j, &**d))
        }
        _ => None,
    })
}

/// A structure that matches any value: a bind, `_`, or an or-pattern
/// holding one.
fn catches_any(p: &StructurePattern) -> bool {
    match p {
        StructurePattern::Bind(_) | StructurePattern::Ignore => true,
        StructurePattern::Or(alts) => alts.iter().any(catches_any),
        _ => false,
    }
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
    !matches!(op, BinOp::And | BinOp::Or) && !sample(op)
}

fn sample(op: BinOp) -> bool {
    matches!(op, BinOp::Sample | BinOp::StrictSample | BinOp::BankedSample)
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

/// [`reached_uses`] of `f` and of every name an unannotated `let g = f`
/// forwards it under, transitively (once `f` is not generalized an alias
/// shares its cells), in preorder; and the names it travels under.
fn forwarded_uses<'a>(
    stmts: &[(usize, usize, &'a Expr)],
    si: usize,
    f: &'a str,
) -> (Vec<usize>, Vec<(usize, usize)>, Vec<&'a str>) {
    let (mut calls, mut refs, mut names) = (Vec::new(), Vec::new(), Vec::new());
    let mut todo = vec![(si, f)];
    while let Some((si, f)) = todo.pop() {
        names.push(f);
        let (c, r) = reached_uses(stmts, si, f);
        for &(_, k) in &r {
            let st: &'a Expr = stmts[k].2;
            if let ExprKind::Bind(b) = &st.kind
                && let (StructurePattern::Bind(g), None, false) =
                    (&b.pattern, &b.typ, b.rec)
                && matches!(&typemorph::unparen(&b.value).kind,
                    ExprKind::Ref { name } if name.to_string() == f)
            {
                todo.push((k, g.as_str()));
            }
        }
        calls.extend(c);
        refs.extend(r);
    }
    calls.sort_unstable();
    refs.sort_unstable();
    (calls, refs, names)
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
/// as its value and annotation: `x + 1` for a parameter `x: 'a`, or
/// under `RigidProbe::Bottom` `x` annotated `_`, or under
/// `RigidProbe::Needs` `x + x` where the header bounds `'a` by `Any`, a
/// predicate that does not say `Number`; `f(1)` for a parameter `f:
/// fn(x: 'a) -> ..` whose `'a` is not its own quantifier.
fn rigid_probe(l: &LambdaExpr, mode: RigidProbe) -> Option<(Expr, Option<Type>)> {
    let one = || ExprKind::Constant(netidx_value::Value::I64(1)).to_expr_nopos();
    let name_ref = |x: &Name| {
        Arc::new(ExprKind::Ref { name: ModPath::from([x.name.as_str()]) }.to_expr_nopos())
    };
    l.args.iter().filter(|a| !a.kind.is_labeled()).find_map(|a| {
        let StructurePattern::Bind(x) = &a.pattern else { return None };
        match a.constraint.as_ref()? {
            Type::TVar(tv) if mode == RigidProbe::Needs => {
                let any = l
                    .constraints
                    .iter()
                    .find(|(c, _)| c.name == tv.name)
                    .is_none_or(|(_, b)| matches!(b, Type::Any));
                any.then(|| {
                    (
                        ExprKind::BinOp {
                            op: BinOp::Add,
                            lhs: name_ref(x),
                            rhs: name_ref(x),
                        }
                        .to_expr_nopos(),
                        None,
                    )
                })
            }
            Type::TVar(_) if mode == RigidProbe::Bottom => {
                Some(((*name_ref(x)).clone(), Some(Type::Bottom)))
            }
            Type::TVar(_) => Some((
                ExprKind::BinOp {
                    op: BinOp::Add,
                    lhs: name_ref(x),
                    rhs: Arc::new(one()),
                }
                .to_expr_nopos(),
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

/// What [`rigid_probe`] does to a declared variable.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RigidProbe {
    Added,
    Bottom,
    Needs,
}

/// Family 2. A lambda with a parameter over a declared variable `'a`
/// gets a first statement fixing `'a` to `i64` or to `_`, or asking more
/// of one bounded `Any` or written bare than that says ([`rigid_probe`],
/// in turn): a def's declared variables are rigid in its body, so `'a`
/// can become neither, and its body adds no bound to it. Right site: the
/// definition.
fn rigid_var(root: &Expr, cap: usize, out: &mut Vec<RejectProbe>) {
    with_first_statement(root, cap, Family::RigidVar, out, |l, taken| {
        rigid_probe(
            l,
            match taken % 3 {
                0 => RigidProbe::Added,
                1 => RigidProbe::Bottom,
                _ => RigidProbe::Needs,
            },
        )
    })
}

/// Each top-level `let` of a lambda `probe` gives a first statement for
/// (its value and annotation, bound to `tm__0`), as a `family` mutant,
/// up to `cap`; `probe` is told how many were taken.
fn with_first_statement(
    root: &Expr,
    cap: usize,
    family: Family,
    out: &mut Vec<RejectProbe>,
    probe: impl Fn(&LambdaExpr, usize) -> Option<(Expr, Option<Type>)>,
) {
    let sizes = mutate::sizes(root);
    let stmts = mutate::top_statements(root, &sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let ExprKind::Lambda(l) = &b.value.kind else { continue };
        let LambdaBody::Expr(body) = &l.body else { continue };
        let Some((test, typ)) = probe(l, taken) else { continue };
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
        out.extend(finish(family, at, &cand, |back, _| {
            vec![nth_stmt(back, si).and_then(span)]
        }));
        taken += 1;
    }
}

/// Family 3. A call whose callee declares a positional parameter over a
/// variable its header bounds by a class of primitives (`'a: Number`),
/// the argument there a concrete primitive, gets that argument replaced
/// by a literal the bound does not hold: a variable is within its bound
/// at every call, however its arguments widen it. Where two parameters
/// are of one such variable the second is replaced, the first then giving
/// the variable a type. A call where another argument of the variable has
/// a type its uses decide (`open_init`) is skipped. Right site: the call.
fn shared_var(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    let block: &[Expr] = match &root.kind {
        ExprKind::Block { exprs } => exprs,
        _ => &[],
    };
    let stmts = mutate::top_statements(root, &sizes);
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
        let Some(ft) = types.declared(&ap.function) else { continue };
        let params: Vec<&Type> =
            ft.args.iter().filter(|a| a.is_positional()).map(|a| &a.typ).collect();
        if params.len() != ap.args.len() {
            continue;
        }
        let same = |a: &Type, b: &Type| matches!((a, b), (Type::TVar(x), Type::TVar(y)) if x.same_cell(y));
        let bound = |t: &Type| match t {
            Type::TVar(tv) if !tv.is_bound() => {
                tv.cell_above().iter().find(|c| primitive_class(c)).cloned()
            }
            _ => None,
        };
        let bounded = |k: &usize| bound(params[*k]).is_some();
        let second = (0..params.len()).find_map(|a| {
            ((a + 1)..params.len()).find(|b| same(params[a], params[*b]) && bounded(b))
        });
        let Some(replaced) = second.or_else(|| (0..params.len()).find(bounded)) else {
            continue;
        };
        let si = stmts
            .iter()
            .find(|(_, at, _)| (*at..*at + sizes[*at]).contains(&i))
            .map_or(0, |(si, ..)| *si);
        // every other argument of the variable must be decided
        let decided = (0..params.len())
            .filter(|c| *c != replaced && same(params[replaced], params[*c]))
            .all(|c| {
                let arg = &ap.args[c].1;
                !open_init(types, block, si, arg)
                    && types.of(arg).first().is_some_and(|t| concrete(t))
            });
        if !decided {
            continue;
        }
        let u = match bound(params[replaced]) {
            Some(b @ Type::Primitive(_)) => match disjoint_literal(&b) {
                Some(u) => u,
                None => continue,
            },
            _ => ExprKind::Constant(netidx_value::Value::String(ArcStr::from("tm")))
                .to_expr_nopos(),
        };
        let at = arg_indices(i, &sizes, ap.args.len())[replaced];
        let cand = mutate::replace(root, at, &u);
        out.extend(finish(Family::SharedVar, i, &cand, |_, pre| {
            vec![pre.get(i).and_then(span)]
        }));
        taken += 1;
    }
}

/// Whether an argument of the call is no value: such a call is never
/// made, and a seq, which gathers a call's arguments into one value and
/// selects over it, checks nothing of the others.
fn never_made(types: &TypeMap, ap: &ApplyExpr) -> bool {
    ap.args.iter().any(|(_, a)| {
        matches!(typemorph::unparen(a).kind, ExprKind::Never { .. })
            || types.of(a).first().is_some_and(|t| matches!(t, Type::Bottom))
    })
}

/// Family 8. A call argument whose parameter is `'a: Function` (a builtin
/// that wraps a function) replaced by a literal, or by a function with a
/// defaulted label: the bound admits only a function type a generated call
/// passes every argument of (`Type::function_holds`). Right site: the call,
/// one that is made ([`never_made`]).
fn function_bound(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let bounded = |t: &Type| match t {
        Type::TVar(tv) => {
            tv.cell_above().iter().any(|c| matches!(c, Type::Bound(Bound::Function)))
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
        if never_made(types, ap) {
            continue;
        }
        let ft = types.declared(&ap.function).or_else(|| callee_type(types, ap));
        let Some(ft) = ft else { continue };
        let found = paired(&ft, ap, i, &sizes).into_iter().find(|(_, arg, param)| {
            !typemorph::leaks_binds(arg) && param.is_some_and(|p| bounded(&p.typ))
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
/// type lets a caller omit (`fntyp.rs::align`). The expected type is one
/// the callee declares: a parameter typed by a variable, or by nothing,
/// takes the argument's own type and expects nothing of it. Right site:
/// the call, or for the default, the definition and every statement
/// using the lambda.
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
            !typemorph::leaks_binds(e)
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
    let stmts = mutate::top_statements(root, sizes);
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
        let (_, refs, names) = forwarded_uses(&stmts, si, f);
        // a value use of `f` as an argument, and the parameter it meets
        let expected = pre.iter().enumerate().find_map(|(i, n)| {
            let ExprKind::Apply(ap) = &n.kind else { return None };
            // a callee whose type its uses decided (`let g = never()`) has
            // the parameter this very call gave it
            if types.cell(&ap.function) {
                return None;
            }
            let ft = callee_type(types, ap)?;
            // the callee declares a function type there: a variable or an
            // untyped parameter takes the argument's own type, and expects
            // nothing of it
            let declared = types.declared(&ap.function)?;
            let written = |p: &FnArgType| {
                let k = ft.args.iter().position(|a| std::ptr::eq(a, p))?;
                let t = &declared.args.get(k)?.typ;
                t.with_deref(|d| matches!(d, Some(Type::Fn(_)))).then_some(())
            };
            paired(&ft, ap, i, sizes).into_iter().find_map(|(ix, _, param)| {
                refs.iter().any(|(r, _)| *r == ix).then(|| {
                    param.filter(|p| written(p).is_some()).map(|p| p.typ.clone())
                })?
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
        // CR claude for claude: [bug/low] The right sites are the definition and the
        // statements naming `f` or a plain alias, but a struct or array holding `f`
        // carries the now-required label too. `let s = {g: f}; let p = s.g(2)` is
        // refused "missing required argument a" at `s.g(2)`, which is filed as
        // MISPLACED. Fix: take the sites from `affected_stmts` over the root block's
        // statements, which follows the lets built from `f` and what reads them, as
        // retype and widen-hop already do. probe:
        // design/review-2026-11-16/repro/fuzz-mutate-label-default-holder.gx
        // (fuzz-mutate-04)
        let mut sites = vec![si];
        sites.extend(stmts.iter().skip(si + 1).filter(|(_, _, st)| {
            st.fold(false, &mut |m, n| m || matches!(&n.kind, ExprKind::Ref { name } if names.contains(&name.to_string().as_str())))
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
    modules: &[&str],
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    let block: &[Expr] = match &root.kind {
        ExprKind::Block { exprs } => exprs,
        _ => &[],
    };
    // A callee's parameter may be a cell shared with the environment
    // (`|y| z <- y` over `let z = never()`), which a call widens rather
    // than refuses. A call is a consumer only through a package's module
    // path (never the subject's own modules), a builtin the body does not
    // rebind, or a let-bound lambda literal whose every parameter is
    // annotated with no type variable (a call widens a variable too); any
    // other name the body binds, by any form, may be such a lambda (an
    // alias, a destructured element, an import).
    let mut untyped: AHashSet<&str> = AHashSet::default();
    let mut binds_any = false;
    for e in pre {
        if let ExprKind::Bind(b) = &e.kind
            && let StructurePattern::Bind(_) = &b.pattern
            && let ExprKind::Lambda(l) = &typemorph::unparen(&b.value).kind
            && l.args.iter().all(|a| a.constraint.as_ref().is_some_and(Type::tvar_free))
        {
            continue;
        }
        typemorph::each_bound(e, &mut |n| match n {
            Some(n) => {
                untyped.insert(n);
            }
            None => binds_any = true,
        });
    }
    let consumer_callee = |f: &Expr| match &typemorph::unparen(f).kind {
        ExprKind::Ref { name } => {
            let name = name.to_string();
            match name.split_once("::") {
                Some((m, _)) => !modules.contains(&m),
                None => !binds_any && !untyped.contains(name.as_str()),
            }
        }
        _ => false,
    };
    let mut taken = 0usize;
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        // the consumer's own type, where it is wider than the value's (a
        // parameter): the widening literal must lie outside it
        let mut beside: Vec<usize> = Vec::new();
        let target: Option<(usize, &Expr, Option<Type>)> = match &e.kind {
            // a comparison is no consumer: the widened operand's type
            // holds the other's
            ExprKind::BinOp { op, lhs, rhs }
                if arith_or_compare(*op) && !comparison(*op) =>
            {
                // the operands are one type: the other's written binding
                // meets the literal's too
                beside = written_readers(block, rhs);
                Some((i + 1, &**lhs, None))
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
            ExprKind::Apply(ap) if !consumer_callee(&ap.function) => None,
            ExprKind::Apply(ap) if never_made(types, ap) => None,
            ExprKind::Apply(ap) => {
                // the first argument whose parameter is declared a concrete
                // primitive: one over a variable widens with its argument
                let ft = types.declared(&ap.function).or_else(|| callee_type(types, ap));
                let Some(ft) = ft else { continue };
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
        if typemorph::leaks_binds(value) {
            continue;
        }
        let Some(t) = types.of(value).first() else { continue };
        if !concrete(t) {
            continue;
        }
        let Some(u) = disjoint_literal(consumer.as_ref().unwrap_or(t)) else { continue };
        let cand = mutate::replace(root, at, &widen(value, u));
        // the consumer keeps its index: what changed comes after it
        out.extend(finish(Family::WidenConsumer, i, &cand, |back, pre| {
            let beside = beside.iter().map(|k| nth_stmt(back, *k).and_then(span));
            [pre.get(i).and_then(span)].into_iter().chain(beside).collect()
        }));
        taken += 1;
    }
}

/// Family 6. An unannotated `let v = e` over a primitive, where a later
/// use pins `v` by a literal (`v + 1`, `v == 1`), gets an initializer of
/// a type `e`'s does not relate to: the use then conflicts. Where the
/// use is arithmetic it gets a writer of such a type too: the write
/// widens `v` to hold both, and arithmetic takes one numeric type (a
/// comparison takes the wider `v`).
fn retype(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let ExprKind::Block { exprs: block } = &root.kind else { return };
    let sizes = mutate::sizes(root);
    let stmts = mutate::top_statements(root, &sizes);
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
        if matches!(b.value.kind, ExprKind::Lambda(_))
            || open_init(types, block, si, &b.value)
        {
            continue;
        }
        let Some(t) = types.of(&b.value).first() else { continue };
        if !concrete(t) || !t.with_deref(|d| matches!(d, Some(Type::Primitive(_)))) {
            continue;
        }
        let Some(u) = disjoint_literal(t) else { continue };
        let (_, refs) = reached_uses(&stmts, si, v);
        let pinned = pre.iter().enumerate().find_map(|(j, n)| {
            let ExprKind::BinOp { op, lhs, rhs } = &n.kind else { return None };
            let op = *op;
            let is_v = |e: &Expr| matches!(&e.kind, ExprKind::Ref { name } if name.to_string() == **v);
            let lit = |e: &Expr| matches!(e.kind, ExprKind::Constant(_));
            let use_at = match (is_v(lhs), is_v(rhs)) {
                (true, false) if lit(rhs) => j + 1,
                (false, true) if lit(lhs) => j + 1 + sizes[j + 1],
                _ => return None,
            };
            (arith_or_compare(op) && refs.iter().any(|(r, _)| *r == use_at))
                .then_some((j, op))
        });
        let Some((j, op)) = pinned else { continue };
        // a write widens `v` to hold the new initializer too, and a
        // comparison takes the wider `v`
        if comparison(op) && writes(&block[si + 1..], v) {
            continue;
        }
        // a writer right after the let
        if !comparison(op) {
            let write = ExprKind::Connect {
                name: ModPath::from([v.name.as_str()]),
                at: WrittenAt::NOWHERE,
                value: Arc::new(u.clone()),
                deref: false,
            }
            .to_expr_nopos();
            let mut with_writer: Vec<Expr> = block.to_vec();
            with_writer.insert(si + 1, write);
            let sites = affected_stmts(&with_writer, si, v);
            let cand =
                ExprKind::Block { exprs: Arc::from_iter(with_writer) }.to_expr_nopos();
            out.extend(finish(Family::Retype, at, &cand, |back, _| {
                sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
            }));
        }
        taken += 1;
        // the initializer retyped
        let cand = mutate::replace(root, at + 1, &u);
        let sites = affected_stmts(block, si, v);
        out.extend(finish(Family::Retype, j, &cand, |back, _| {
            sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
        }));
    }
}

/// Whether a statement of `stmts` writes `v`: a connect to it, or a `&mut`
/// of it.
fn writes(stmts: &[Expr], v: &str) -> bool {
    let is_v =
        |e: &Expr| matches!(&e.kind, ExprKind::Ref { name } if name.to_string() == v);
    stmts.iter().any(|s| {
        s.fold(false, &mut |m, n| {
            m || match &n.kind {
                ExprKind::Connect { name, deref: false, .. } => name.to_string() == v,
                ExprKind::ByRef(Mutability::Mut, x) => is_v(x),
                _ => false,
            }
        })
    })
}

/// The statement `si` (a `let v = ..`) and every later one that reads or
/// writes `v` (until a statement binds the name to a value not built from
/// it; one that may, a module, hides nothing for sure), a `let` built
/// from it, or what
/// a connect anywhere in such a statement relates to it (its target, and a
/// cell it reads, which the connect decides): a change to `v`'s type meets
/// all of them, and the checker reports whichever it reaches first.
fn affected_stmts(stmts: &[Expr], si: usize, v: &str) -> Vec<usize> {
    let mut affected: Vec<CompactString> = vec![v.into()];
    let mut sites = vec![si];
    for (k, later) in stmts.iter().enumerate().skip(si + 1) {
        let mentions = later.fold(false, &mut |m, n| {
            m || matches!(&n.kind, ExprKind::Ref { name } | ExprKind::Connect { name, .. }
                if affected.iter().any(|a| name.to_string() == **a))
        });
        // a let or a use binds for the statements after it (a lambda's
        // parameter and an arm's name bind inside their own expression):
        // a name it binds to a value not built from these is another's
        if !mentions && matches!(later.kind, ExprKind::Bind(_) | ExprKind::Use { .. }) {
            typemorph::each_bound(later, &mut |b| {
                if let Some(b) = b {
                    affected.retain(|a| **a != *b)
                }
            });
            if affected.is_empty() {
                break;
            }
        }
        if mentions {
            sites.push(k);
            if let ExprKind::Bind(lb) = &later.kind {
                lb.pattern.with_names(&mut |n| affected.push(n.as_str().into()))
            }
            // a connect anywhere in the statement (an `on` block's, an arm's)
            let mut related: Vec<CompactString> = Vec::new();
            later.fold((), &mut |(), n| {
                let ExprKind::Connect { name, value, .. } = &n.kind else { return };
                let mut names: Vec<CompactString> = vec![name.to_string().into()];
                value.fold((), &mut |(), n| {
                    if let ExprKind::Ref { name } = &n.kind {
                        names.push(name.to_string().into())
                    }
                });
                if names.iter().any(|n| affected.contains(n)) {
                    related.extend(names)
                }
            });
            affected.extend(related)
        }
    }
    sites
}

/// The statements of `block` that name a binding something writes (a
/// connect's target, a `&mut`'s referent) which `e` reads. Such a binding
/// is one cell: a type that meets it in `e` reaches every statement that
/// reads it, and the checker reports whichever refuses first.
fn written_readers(block: &[Expr], e: &Expr) -> Vec<usize> {
    let read = |n: &Expr| match &n.kind {
        ExprKind::Ref { name } => Some(name.to_string()),
        _ => None,
    };
    let mut written: Vec<String> = Vec::new();
    for s in block {
        s.fold((), &mut |(), n| match &n.kind {
            ExprKind::Connect { name, .. } => written.push(name.to_string()),
            ExprKind::ByRef(Mutability::Mut, x) => written.extend(read(x)),
            _ => (),
        })
    }
    let reads = e.fold(Vec::new(), &mut |mut names: Vec<String>, n| {
        names.extend(read(n).filter(|x| written.contains(x)));
        names
    });
    let names = |s: &Expr| {
        s.fold(false, &mut |m, n| {
            m || read(n).is_some_and(|x| reads.contains(&x))
                || matches!(&n.kind, ExprKind::Connect { name, .. }
                    if reads.contains(&name.to_string()))
        })
    };
    (0..block.len()).filter(|k| names(&block[*k])).collect()
}

/// Family 4 through a `let` (the hop, its own id): an unannotated `let w = e` over a
/// concrete type whose variable a reached use puts directly under an
/// arithmetic operator or a field read gets `e` widened.
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
    let stmts = mutate::top_statements(root, &sizes);
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
        if matches!(b.value.kind, ExprKind::Lambda(_))
            || open_init(types, block, si, &b.value)
        {
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
            ExprKind::BinOp { op, .. } if arith_or_compare(*op) && !comparison(*op) => {
                refs.contains(&(j + 1)) || refs.contains(&(j + 1 + sizes[j + 1]))
            }
            ExprKind::StructRef { .. } => struct_t && refs.contains(&(j + 1)),
            _ => false,
        });
        if !consumed {
            continue;
        }
        if typemorph::leaks_binds(&b.value) {
            continue;
        }
        let cand = mutate::replace(root, at + 1, &widen(&b.value, u));
        let mut sites = affected_stmts(block, si, w);
        let beside: Vec<usize> =
            sites.iter().flat_map(|k| written_readers(block, &block[*k])).collect();
        sites.extend(beside);
        sites.sort_unstable();
        sites.dedup();
        out.extend(finish(Family::WidenHop, at, &cand, |back, _| {
            sites.iter().map(|k| nth_stmt(back, *k).and_then(span)).collect()
        }));
        taken += 1;
    }
}

/// Family 1. A generalized lambda (`let f = |..| ..`) used at two uses
/// whose instances take different primitive first parameters is routed
/// through a binding something writes, `let f = { let tm__0 = |..| ..; let
/// tm__1 = &mut tm__0; tm__0 }`: a written binding is one instance, which
/// `f` forwards, so the two uses meet in it and cannot both hold; an alias
/// `let g = f` forwards the binding, and its value uses are among them.
fn mono_reuse(
    root: &Expr,
    pre: &[Expr],
    types: &TypeMap,
    cap: usize,
    out: &mut Vec<RejectProbe>,
) {
    let sizes = mutate::sizes(root);
    let stmts = mutate::top_statements(root, &sizes);
    let mut taken = 0usize;
    for k in spread(stmts.len()) {
        let (si, at, stmt) = stmts[k];
        if taken >= cap {
            break;
        }
        let ExprKind::Bind(b) = &stmt.kind else { continue };
        let (StructurePattern::Bind(f), ExprKind::Lambda(l), false) =
            (&b.pattern, &b.value.kind, b.rec)
        else {
            continue;
        };
        // in a written binding an unannotated parameter or return is a cell
        // outside the definition, so meeting a declared variable is its
        // escape, refused first
        if !l.constraints.is_empty()
            && (l.rtype.is_none() || l.args.iter().any(|a| a.constraint.is_none()))
        {
            continue;
        }
        // the first parameter narrowed to one type is its declared variable
        // no more: a body that asks it to be one type with whatever else
        // holds the variable is then refused, at the definition
        let positional = |a: &&Arg| matches!(a.kind, ArgKind::Positional);
        let first = l.args.iter().find(positional).and_then(|a| a.constraint.as_ref());
        let shared = l.constraints.iter().any(|(tv, _)| {
            let named = |t: &Type| names(t, &tv.name);
            first.is_some_and(named)
                && l.args
                    .iter()
                    .filter_map(|a| a.constraint.as_ref())
                    .chain(&l.rtype)
                    .filter(|t| named(t))
                    .count()
                    > 1
        });
        if shared {
            continue;
        }
        let (calls, values, _) = forwarded_uses(&stmts, si, f);
        // a call's argument types its parameter only where the definition
        // left the parameter open
        let open_param = types.of(&b.value).first().is_some_and(|t| {
            t.with_deref(|d| match d {
                Some(Type::Fn(ft)) => {
                    ft.args.iter().find(|a| a.is_positional()).is_some_and(
                        |a| matches!(&a.typ, Type::TVar(tv) if !tv.is_bound()),
                    )
                }
                _ => false,
            })
        });
        // a use's first parameter: a value's from its type, a call's (its
        // callee's reference sits right after it in preorder) from its
        // first positional argument
        let uses: Vec<(Type, usize)> = values
            .iter()
            .filter_map(|(r, j)| {
                let first = match calls.iter().find(|c| *c + 1 == *r) {
                    None => types.of(&pre[*r]).first().and_then(first_param)?,
                    Some(_) if !open_param => return None,
                    Some(c) => match &pre[*c].kind {
                        ExprKind::Apply(ap) => ap
                            .args
                            .iter()
                            .find(|(label, _)| label.is_none())
                            .and_then(|(_, a)| types.of(a).first().cloned())?,
                        _ => return None,
                    },
                };
                // a read nothing passes a value through (a plain alias's)
                // decides nothing of the parameter
                let unused = first.with_deref(|d| matches!(d, Some(Type::Bottom)));
                (!unused).then_some((first, *j))
            })
            .collect();
        // a decided type of many values may be the one the others fit
        if uses.iter().any(|(t, _)| concrete(t) && primitive(t).is_none()) {
            continue;
        }
        let firsts: Vec<(Typ, usize)> =
            uses.iter().filter_map(|(t, j)| Some((primitive(t)?, *j))).collect();
        let Some(((ta, _), (_, jb))) = firsts.iter().enumerate().find_map(|(n, a)| {
            firsts[n + 1..].iter().find(|b| b.0 != a.0).map(|b| (a, b))
        }) else {
            continue;
        };
        let bind = |name: &str, value: Expr| {
            ExprKind::Bind(Arc::new(BindExpr {
                rec: false,
                pattern: StructurePattern::Bind(Name::from(name)),
                typ: None,
                value,
            }))
            .to_expr_nopos()
        };
        let held = ExprKind::Ref { name: ModPath::from(["tm__0"]) }.to_expr_nopos();
        let writer =
            ExprKind::ByRef(Mutability::Mut, Arc::new(held.clone())).to_expr_nopos();
        // the one instance is a cell every value of it flows into: a value
        // taking only the first use's type, written there, is what the
        // other use's argument cannot pass
        let mut narrow = l.as_ref().clone();
        let mut args: Vec<Arg> = narrow.args.iter().cloned().collect();
        let Some(first) = args.iter_mut().find(|a| matches!(a.kind, ArgKind::Positional))
        else {
            continue;
        };
        first.constraint = Some(Type::Primitive((*ta).into()));
        narrow.args = Arc::from(args);
        let narrowed = ExprKind::Lambda(Arc::new(narrow)).to_expr_nopos();
        let write = ExprKind::Connect {
            name: ModPath::from(["tm__1"]),
            at: Default::default(),
            value: Arc::new(narrowed),
            deref: true,
        }
        .to_expr_nopos();
        let wrapped = ExprKind::Block {
            exprs: Arc::from_iter([
                bind("tm__0", b.value.clone()),
                bind("tm__1", writer),
                write,
                held,
            ]),
        }
        .to_expr_nopos();
        let cand = mutate::replace(root, at + 1, &wrapped);
        let jb = *jb;
        // every value use shares the wrapped binding's cells, so the
        // checker refuses at whichever reached use conflicts first
        let mut sites = vec![si];
        // CR claude for claude: [bug/medium] mono_reuse's right sites (the definition,
        // and the statements naming `f` or a plain alias up to the second use) miss two
        // ways the one instance reaches past a use of `f`. A use whose call holds a
        // cell its uses decide (`f(n)` over `let n = never()`, `array::map(xs, f)` over
        // `let xs = []`) gives that cell the instance's type, so the refusal lands at
        // the cell's later reader. A holder (`let s = {g: f}`, `let s = [f]`) shares
        // the instance, so `s.g("str")` refuses before the second use. Both are filed
        // as MISPLACED. Fix: skip a use whose call has an argument `open_init` calls
        // open, as `shared_var` does, and take the sites from `affected_stmts(block,
        // si, f)`, which follows the lets built from `f`. probe:
        // design/review-2026-11-16/repro/fuzz-mutate-mono-reuse-sites.sh
        // (fuzz-mutate-01)
        // 2026-11-24 claude: Soak nov24b's twenty mono-reuse flips (MISPLACED 'a value
        // here is used as v32 and as i64', one LEAK) were not these two ways: the let
        // around the family's block generalized the written binding again (bind.rs
        // look-ahead, fixed with pin
        // lang::quantified::a_written_local_a_block_gives_is_one_instance). With that
        // fixed all twenty replay clean under the sites as they stand; the two ways
        // this CR names are not re-tested here.
        sites.extend(values.iter().map(|(_, j)| *j).filter(|j| *j <= jb));
        sites.sort_unstable();
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
    let block = match &root.kind {
        ExprKind::Block { exprs } => &exprs[..],
        _ => &[],
    };
    let sizes = mutate::sizes(root);
    let stmts = mutate::top_statements(root, &sizes);
    let mut taken = 0usize;
    for i in spread(pre.len()) {
        if taken >= cap {
            break;
        }
        let e = &pre[i];
        let ExprKind::Select(SelectExpr { arg, arms }) = &e.kind else { continue };
        let covers_all = arms.iter().any(|(p, _)| {
            p.type_predicate.is_some() || catches_any(&p.structure_predicate)
        });
        if covers_all {
            continue;
        }
        let Some(t) = types.of(arg).first() else { continue };
        // a scrutinee its arms type (a ⊥ let, a raise) takes the fresh tag
        // as its type rather than refusing it
        let si = stmts.iter().rposition(|(_, at, _)| *at <= i).unwrap_or(0);
        if !only_variants(t)
            || typemorph::leaks_binds(arg)
            || open_init(types, block, si, arg)
        {
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
    plain_arm(StructurePattern::literal(netidx_value::Value::Bool(b)))
}

/// An arm's pattern of `structure_predicate` alone.
fn plain_arm(structure_predicate: StructurePattern) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate,
        guard: None,
        pos: WrittenAt::NOWHERE,
        end: WrittenAt::NOWHERE,
    }
}
