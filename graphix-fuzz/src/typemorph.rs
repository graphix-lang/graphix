//! Metamorphic typecheck probes: take a program the checker accepts,
//! apply an acceptance-preserving transform, and check acceptance
//! again. A flip is a typechecker finding the differential oracle
//! cannot see.
//!
//! Transforms are `Expr -> Expr` on the parsed body, printed back
//! through the pretty printer; a candidate that fails to re-parse is
//! dropped and counted (`noparse`). Site indices are [`crate::mutate`]
//! preorder indices and transforms are deterministic in the body text,
//! so a `(kind, site)` id re-derives the same candidate in a fresh
//! process. Grades: parens-wrap is sound (a flip is a compiler bug);
//! the rest are expected-preserving (a flip files for triage).

use crate::mutate;
use arcstr::ArcStr;
use graphix_compiler::{
    expr::{
        ApplyExpr, ArgKind, BindExpr, Expr, ExprKind, LambdaBody, LambdaExpr, ModPath,
        Name, SeqTrigger, StructurePattern, TypeDefBody, TypeDefExpr,
    },
    typ::{TVar, Type, TypeRef},
};
use netidx_core::path::Path;
use std::collections::HashSet;
use triomphe::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TmKind {
    ParensWrap,
    BlockWrap,
    LetExtract,
    LetInline,
    StmtPermute,
    AliasSwap,
    LabelPermute,
    DefaultMaterialize,
    DefaultElide,
}

impl std::fmt::Display for TmKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TmKind::ParensWrap => "parens-wrap",
            TmKind::BlockWrap => "block-wrap",
            TmKind::LetExtract => "let-extract",
            TmKind::LetInline => "let-inline",
            TmKind::StmtPermute => "stmt-permute",
            TmKind::AliasSwap => "alias-swap",
            TmKind::LabelPermute => "label-permute",
            TmKind::DefaultMaterialize => "default-materialize",
            TmKind::DefaultElide => "default-elide",
        };
        write!(f, "{s}")
    }
}

/// One transformed candidate: the body text to substitute for the
/// subject's body, addressed by the deterministic `(kind, site)` pair.
pub struct TmProbe {
    pub kind: TmKind,
    pub site: usize,
    pub body: String,
}

impl TmProbe {
    pub fn id(&self) -> String {
        format!("{}#{}", self.kind, self.site)
    }
}

/// Reserved fresh names. Binding names must start alphabetic, so the
/// reserve marker is the interior double underscore; subjects already
/// containing it are skipped.
const VAL: &str = "tm__0";
const TYP: &str = "Tm__0";

/// Enumerate up to `cap` candidates PER TRANSFORM over `body`.
/// Returns the probes and the count of candidates dropped because
/// their printed form failed to re-parse.
pub fn probes(body: &str, cap: usize) -> (Vec<TmProbe>, usize) {
    let mut out: Vec<TmProbe> = Vec::new();
    let mut noparse = 0usize;
    // `mutate::replace` drops attributes on the rebuilt path, so an
    // attr-bearing body could flip on attribute loss rather than typing.
    if body.contains("tm__") || body.contains("Tm__") || body.contains("#[") {
        return (out, 0);
    }
    let Some(root) = mutate::parse(body) else {
        return (out, 0);
    };
    let pre = mutate::preorder(&root);
    let sizes = mutate::sizes(&root);
    // a candidate is its printed text only when that text reads back as
    // the candidate: a print that parses as another program probes that one
    let push = |out: &mut Vec<TmProbe>, noparse: &mut usize, kind, site, cand: &Expr| {
        let text = cand.to_string();
        if mutate::parse(&text).is_some_and(|back| back == *cand) {
            out.push(TmProbe { kind, site, body: text });
        } else {
            *noparse += 1;
        }
    };
    // parens-wrap: `e` -> `(e)`
    {
        let sites: Vec<usize> =
            (0..pre.len()).filter(|&i| value_pos(&pre[i].kind)).collect();
        for i in sample(&sites, cap) {
            let repl = ExprKind::ExplicitParens(Arc::new(pre[i].clone())).to_expr_nopos();
            let cand = mutate::replace(&root, i, &repl);
            push(&mut out, &mut noparse, TmKind::ParensWrap, i, &cand);
        }
    }
    // block-wrap: `e` -> `{ let tm__0 = e; tm__0 }`; not on lambda
    // literals (let-extract's probe) or blocks, nor on `never()`: a `let`
    // over ⊥ is typed by its writers, an open cell where `never()` is ⊥;
    // nor under `&`, where `e` may be a place's root
    {
        let under_ref = |i: usize| {
            (0..i).any(|j| matches!(pre[j].kind, ExprKind::ByRef(..)) && i < j + sizes[j])
        };
        let sites: Vec<usize> = (0..pre.len())
            .filter(|&i| {
                !under_ref(i)
                    && value_pos(&pre[i].kind)
                    && !matches!(
                        unparen(&pre[i]).kind,
                        ExprKind::Lambda(_)
                            | ExprKind::Block { .. }
                            | ExprKind::Never { .. }
                    )
                    && !leaks_binds(&pre[i])
            })
            .collect();
        for i in sample(&sites, cap) {
            let bind = ExprKind::Bind(Arc::new(BindExpr {
                rec: false,
                pattern: StructurePattern::Bind(Name::from(VAL)),
                typ: None,
                value: pre[i].clone(),
            }))
            .to_expr_nopos();
            let r = ExprKind::Ref { name: mp(VAL) }.to_expr_nopos();
            let repl =
                ExprKind::Block { exprs: Arc::from_iter([bind, r]) }.to_expr_nopos();
            let cand = mutate::replace(&root, i, &repl);
            push(&mut out, &mut noparse, TmKind::BlockWrap, i, &cand);
        }
    }
    // label-permute: rotate a call's labeled arguments; labels bind by
    // name, so the order is never observable
    {
        let sites: Vec<usize> = (0..pre.len())
            .filter(|&i| match &pre[i].kind {
                ExprKind::Apply(ap) => {
                    ap.args.iter().filter(|(l, _)| l.is_some()).count() > 1
                }
                _ => false,
            })
            .collect();
        for i in sample(&sites, cap) {
            let ExprKind::Apply(ap) = &pre[i].kind else { continue };
            let n = ap.args.iter().filter(|(l, _)| l.is_some()).count();
            let mut args: Vec<_> = ap.args.to_vec();
            args[..n].rotate_left(1);
            let repl = ExprKind::Apply(ApplyExpr {
                args: Arc::from_iter(args),
                function: ap.function.clone(),
            })
            .to_expr_nopos();
            let cand = mutate::replace(&root, i, &repl);
            push(&mut out, &mut noparse, TmKind::LabelPermute, i, &cand);
        }
    }
    let ExprKind::Block { exprs: stmts } = &root.kind else {
        return (out, noparse);
    };
    let stmts: Vec<Expr> = stmts.to_vec();
    let offsets: Vec<usize> = {
        let mut off = 1usize;
        let mut v = Vec::with_capacity(stmts.len());
        for _ in 0..stmts.len() {
            v.push(off);
            off += sizes[*v.last().expect("just pushed")];
        }
        v
    };
    // let-extract: `f(.., |x| body)` -> `let tm__0 = |x| body; f(.., tm__0)`.
    // Only at Apply sites reachable from the statement root through
    // non-scoping nodes, else the lambda's captures would be stranded;
    // nor in a statement that binds names inside itself, which the
    // hoisted lambda may read.
    {
        let mut found: Vec<(usize, usize)> = Vec::new();
        for (si, stmt) in stmts.iter().enumerate() {
            let inner_binds = match &stmt.kind {
                ExprKind::Bind(b) => leaks_binds(&b.value),
                _ => leaks_binds(stmt),
            };
            if inner_binds {
                continue;
            }
            let mut idx = offsets[si];
            find_lambda_args(stmt, &mut idx, false, &mut |gi| found.push((si, gi)));
        }
        for (si, gi) in sample(&found, cap) {
            let r = ExprKind::Ref { name: mp(VAL) }.to_expr_nopos();
            let replaced = mutate::replace(&root, gi, &r);
            let ExprKind::Block { exprs } = &replaced.kind else { continue };
            let bind = ExprKind::Bind(Arc::new(BindExpr {
                rec: false,
                pattern: StructurePattern::Bind(Name::from(VAL)),
                typ: None,
                value: pre[gi].clone(),
            }))
            .to_expr_nopos();
            let mut v: Vec<Expr> = exprs.to_vec();
            v.insert(si, bind);
            let cand = ExprKind::Block { exprs: Arc::from_iter(v) }.to_expr_nopos();
            push(&mut out, &mut noparse, TmKind::LetExtract, gi, &cand);
        }
    }
    // let-inline: substitute a single-use, unannotated, non-shadowed
    // `let x = e` into its one later use
    {
        let mut done = 0usize;
        for si in 0..stmts.len() {
            if done >= cap {
                break;
            }
            let ExprKind::Bind(b) = &stmts[si].kind else { continue };
            // a `let` over `never()` is an open cell, the value inlined ⊥;
            // a value that binds names binds them for the statements between
            if b.rec
                || b.typ.is_some()
                || matches!(b.value.kind, ExprKind::Never { .. })
                || leaks_binds(&b.value)
            {
                continue;
            }
            let StructurePattern::Bind(name) = &b.pattern else { continue };
            let nm = name.to_string();
            let vrefs = stmt_names(&stmts[si]).refs;
            let mut uses = 0usize;
            let mut ok = true;
            // a later binder of the name shadows the use; a later binder
            // of a name the value references, at any depth, captures it
            // CR claude for claude: [bug] let-inline moves the value's check from its
            // `let` to its one use, past every read in between. But a let over ⊥ takes
            // its type from its first reader, the rule stmt-permute's `open` set
            // encodes. When the value and another reader of such a let are checked in a
            // different order after the inline, the other reader decides the cell and
            // the moved value is refused. This files a false let-inline typeflip. The
            // other reader can be any statement up to the use, including the use's own
            // statement before the use (`let x = k(g); h(g) + x`). Skip the inline when
            // the value reads an open let that any statement from si+1 through the use
            // also reads, using the same openness test as stmt-permute. probe:
            // design/review-2026-10-05/repro/fuzz-mutate-07.gx (`graphix-fuzz
            // typemorph` on it reports let-inline#3). (fuzz-mutate-07)
            for later in &stmts[si + 1..] {
                later.fold((), &mut |(), n| match &n.kind {
                    ExprKind::Ref { name } if name.to_string() == nm => uses += 1,
                    ExprKind::Connect { name, .. } if name.to_string() == nm => {
                        ok = false
                    }
                    _ if binds(n, &nm) || vrefs.iter().any(|r| binds(n, r)) => ok = false,
                    _ => (),
                });
            }
            if !ok || uses != 1 || stmts.len() < 3 {
                continue;
            }
            let start = offsets[si] + sizes[offsets[si]];
            let Some(gi) = (start..pre.len()).find(
                |&i| matches!(&pre[i].kind, ExprKind::Ref { name } if name.to_string() == nm),
            ) else {
                continue;
            };
            let under = |k: fn(&ExprKind) -> bool| {
                (0..gi).any(|j| k(&pre[j].kind) && gi < j + sizes[j])
            };
            // a name under `&` may be a place root; a seq body refuses a `catch`
            // CR claude for claude: [bug] let-inline can move a value that raises (`x?`,
            // or a call whose callee raises) to a use that sits after a `catch` in an
            // enclosing block, so the raise reaches that handler instead of its old
            // one; a handler typed for other errors then refuses the mutant by language
            // rule, a false typeflip. The guard below covers only the reverse move (a
            // value holding a `catch` into a seq body). Skip the inline when the value
            // can raise and a `catch` statement stands between the `let` and the use in
            // a block enclosing the use. probe:
            // design/review-2026-10-05/repro/fuzz-mutate-10.gx (fuzz-mutate-10)
            let holds_catch = b
                .value
                .fold(false, &mut |a, n| a || matches!(n.kind, ExprKind::Catch(_)));
            if under(|k| matches!(k, ExprKind::ByRef(..)))
                || (holds_catch && under(|k| matches!(k, ExprKind::Seq { .. })))
            {
                continue;
            }
            let value =
                ExprKind::ExplicitParens(Arc::new(b.value.clone())).to_expr_nopos();
            let replaced = mutate::replace(&root, gi, &value);
            let ExprKind::Block { exprs } = &replaced.kind else { continue };
            let v: Vec<Expr> = exprs
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != si)
                .map(|(_, e)| e.clone())
                .collect();
            let cand = ExprKind::Block { exprs: Arc::from_iter(v) }.to_expr_nopos();
            push(&mut out, &mut noparse, TmKind::LetInline, si, &cand);
            done += 1;
        }
    }
    // stmt-permute: swap adjacent independent statements
    {
        // a let over ⊥ takes its type from its first use, so two
        // readers of one do not commute
        // CR claude for claude: [bug] `open` only holds lets whose value is a bare
        // `never()`. The first reader also decides the type of a let over `(never())`,
        // a block ending in `never()`, or a select whose only arm is `never()`, and
        // `let xs = []` flips the same way through its element cell. stmt-permute still
        // swaps two readers of these, so the reader moved to the front decides the cell
        // and the other is refused: a false stmt-permute flip filed for triage.
        // let-inline's guard (line 235) uses the same bare test, and the pin
        // readers_of_a_bottom_let_do_not_commute covers only the bare form. Fix: decide
        // openness from the subject's checked types (`ExprTypeSite::cell`, or a let
        // type that holds a cell), or at least look through parens, block tails and
        // all-`never()` selects. probe:
        // design/review-2026-10-05/repro/fuzz-mutate-08.gx (graphix-fuzz typemorph on
        // it prints a stmt-permute#1 TYPEFLIP). (fuzz-mutate-08)
        let open: HashSet<String> = stmts
            .iter()
            .filter_map(|st| match &st.kind {
                ExprKind::Bind(b)
                    if b.typ.is_none()
                        && matches!(b.value.kind, ExprKind::Never { .. }) =>
                {
                    match &b.pattern {
                        StructurePattern::Bind(n) => Some(n.to_string()),
                        _ => None,
                    }
                }
                _ => None,
            })
            .collect();
        let mut sites = Vec::new();
        for i in 0..stmts.len().saturating_sub(1) {
            if permutable(&stmts[i], &stmts[i + 1], &open) {
                sites.push(i);
            }
        }
        for i in sample(&sites, cap) {
            let mut v = stmts.clone();
            v.swap(i, i + 1);
            let cand = ExprKind::Block { exprs: Arc::from_iter(v) }.to_expr_nopos();
            push(&mut out, &mut noparse, TmKind::StmtPermute, i, &cand);
        }
    }
    // default-materialize / default-elide: write an omitted default out
    // at a call, or drop an explicit argument that is the default. Only
    // a default that reads no name moves (elsewhere a name could be
    // captured), and only to a call the lambda's own binding reaches.
    {
        let mut defaults: Vec<(usize, ArcStr, ArcStr, Expr)> = Vec::new();
        for (si, stmt) in stmts.iter().enumerate() {
            let ExprKind::Bind(b) = &stmt.kind else { continue };
            let (StructurePattern::Bind(f), ExprKind::Lambda(l)) =
                (&b.pattern, &b.value.kind)
            else {
                continue;
            };
            for a in l.args.iter() {
                if let (ArgKind::Defaulted(d), StructurePattern::Bind(label)) =
                    (&a.kind, &a.pattern)
                    && reads_no_name(d)
                {
                    defaults.push((si, f.name.clone(), label.name.clone(), d.clone()));
                }
            }
        }
        let (mut materialize, mut elide): (Vec<(usize, Expr)>, Vec<(usize, Expr)>) =
            (Vec::new(), Vec::new());
        for (si, f, label, d) in &defaults {
            let mut calls: Vec<usize> = Vec::new();
            for (j, st) in stmts.iter().enumerate().skip(si + 1) {
                let mut idx = offsets[j];
                calls_reached(st, f, true, &mut idx, &mut calls);
                if binds_after(st, f) {
                    break;
                }
            }
            for i in calls {
                let ExprKind::Apply(ap) = &pre[i].kind else { continue };
                let given =
                    ap.args.iter().position(|(l, _)| l.as_deref() == Some(&**label));
                let mut args: Vec<_> = ap.args.to_vec();
                let list = match given {
                    None => {
                        args.insert(0, (Some(label.clone()), d.clone()));
                        &mut materialize
                    }
                    Some(j) if args[j].1 == *d => {
                        args.remove(j);
                        &mut elide
                    }
                    Some(_) => continue,
                };
                let repl = ExprKind::Apply(ApplyExpr {
                    args: Arc::from_iter(args),
                    function: ap.function.clone(),
                })
                .to_expr_nopos();
                list.push((i, mutate::replace(&root, i, &repl)));
            }
        }
        for (kind, cands) in
            [(TmKind::DefaultMaterialize, materialize), (TmKind::DefaultElide, elide)]
        {
            for (i, cand) in cands.into_iter().take(cap) {
                push(&mut out, &mut noparse, kind, i, &cand);
            }
        }
    }
    // alias-swap: hoist a bind's annotation into `type Tm__0 = T`
    {
        let mut done = 0usize;
        for si in 0..stmts.len() {
            if done >= cap {
                break;
            }
            let ExprKind::Bind(b) = &stmts[si].kind else { continue };
            let Some(t) = &b.typ else { continue };
            let td = ExprKind::TypeDef(TypeDefExpr {
                name: Name::from(TYP),
                params: Arc::from_iter(std::iter::empty::<(TVar, Option<Type>)>()),
                body: TypeDefBody::Alias(t.clone()),
            })
            .to_expr_nopos();
            let nb = ExprKind::Bind(Arc::new(BindExpr {
                rec: b.rec,
                pattern: b.pattern.clone(),
                typ: Some(Type::Ref(TypeRef::synthetic(
                    ModPath::root(),
                    mp(TYP),
                    Arc::from_iter(std::iter::empty::<Type>()),
                ))),
                value: b.value.clone(),
            }))
            .to_expr_nopos();
            let mut v = stmts.clone();
            v[si] = nb;
            v.insert(si, td);
            let cand = ExprKind::Block { exprs: Arc::from_iter(v) }.to_expr_nopos();
            push(&mut out, &mut noparse, TmKind::AliasSwap, si, &cand);
            done += 1;
        }
    }
    (out, noparse)
}

fn mp(s: &str) -> ModPath {
    [s].into_iter().collect()
}

/// A node a value can stand at — statement forms wrapped in parens or
/// a block are nonsense, not probes.
fn value_pos(k: &ExprKind) -> bool {
    !matches!(
        k,
        ExprKind::NoOp
            | ExprKind::Bind(_)
            | ExprKind::Use { .. }
            | ExprKind::Module { .. }
            | ExprKind::TypeDef(_)
            | ExprKind::Trait(_)
            | ExprKind::Impl(_)
            | ExprKind::Connect { .. }
            | ExprKind::Catch(_)
            | ExprKind::Until(_)
            | ExprKind::TryWith(_)
    )
}

/// Deterministic spread of up to `cap` sites (stride over the list —
/// first sites cluster at the top of small bodies otherwise).
fn sample<T: Copy>(sites: &[T], cap: usize) -> Vec<T> {
    if sites.len() <= cap {
        return sites.to_vec();
    }
    let step = (sites.len() / cap).max(1);
    sites.iter().copied().step_by(step).take(cap).collect()
}

/// Lambda literals in ARGUMENT position of an Apply reachable from the
/// statement root without crossing a scope-introducing node. `idx`
/// enters as the node's own preorder index and tracks
/// `Expr::for_each_child`'s exact order, so the reported index is
/// the lambda node in [`mutate::replace`]'s address space.
fn find_lambda_args(e: &Expr, idx: &mut usize, blocked: bool, f: &mut impl FnMut(usize)) {
    let at_apply = !blocked && matches!(e.kind, ExprKind::Apply(_));
    let blocked = blocked
        || matches!(
            e.kind,
            ExprKind::Lambda(_)
                | ExprKind::Select(_)
                | ExprKind::Catch(_)
                | ExprKind::Block { .. }
                | ExprKind::Seq { .. }
                | ExprKind::TryWith(_)
        );
    *idx += 1;
    e.for_each_child(&mut |c| {
        if at_apply
            && let ExprKind::Lambda(l) = &c.kind
            && !reads_param_type(l)
        {
            f(*idx);
        }
        find_lambda_args(c, idx, blocked, f);
    });
}

/// Whether the body needs an unannotated parameter's type before it
/// checks: a select over a value the parameter's type decides (a type
/// test binds it, coverage reads it), or a field read or a `with` update
/// on it. Only the call supplies that type, so a `let` of the
/// `e` under any parentheses.
// CR claude for claude: [readability] The doc above (489-493) is the head of
// reads_param_type's doc spliced onto the tail of unparen's own, and reads_param_type
// keeps only its last line (501); mustreject.rs:197-198 is widen's doc sitting on
// binds_outward, and widen (mustreject.rs:214) has none. Move each block back above its
// function. The comment at line 83 is also wrong: mutate::replace rebuilds the path
// with `with_kind`, which keeps `dec`. Attributes are lost where a transform rebuilds
// its target from the kind (`to_expr_nopos`) and moved where it relocates a node, and
// the reparse check sees neither because Expr equality ignores `dec`; that is the
// reason to skip `#[`. (fuzz-mutate-17)
pub(crate) fn unparen(mut e: &Expr) -> &Expr {
    while let ExprKind::ExplicitParens(inner) = &e.kind {
        e = inner;
    }
    e
}

/// lambda is refused by language rule, not by an ordering bug.
fn reads_param_type(l: &LambdaExpr) -> bool {
    let mut untyped: HashSet<&str> = HashSet::new();
    for a in l.args.iter().filter(|a| a.constraint.is_none()) {
        a.pattern.with_names(&mut |n| {
            untyped.insert(n.as_str());
        });
    }
    let LambdaBody::Expr(body) = &l.body else { return false };
    let is_param = |e: &Expr| matches!(&unparen(e).kind, ExprKind::Ref { name } if untyped.contains(name.to_string().as_str()));
    let mentions_param = |e: &Expr| e.fold(false, &mut |found, n| found || is_param(n));
    body.fold(false, &mut |found, n| {
        found
            || match &n.kind {
                ExprKind::Select(s) => mentions_param(&s.arg),
                ExprKind::StructRef { source, .. }
                | ExprKind::TupleRef { source, .. } => is_param(source),
                ExprKind::StructWith(w) => is_param(&w.source),
                // CR claude for claude: [bug] Like the field read above, a call through
                // an unannotated parameter (`|g| g(2)`) or a deref of one (`|r| *r +
                // 1`) needs the parameter's type before the body checks. A `let` of
                // either lambda is refused at its definition ("type must be known,
                // annotations needed" / "expected reference"). Both fall through to
                // `false` here, so let-extract hoists them and typemorph files a
                // typeflip for a language rule. Flips dedup by kind and head, so that
                // class then also hides any real let-extract flip with the same head.
                // Add `ExprKind::Apply(ap) => is_param(&ap.function)` and
                // `ExprKind::Deref(x) => is_param(x)`, and pin both shapes in
                // extract_skips_param_type_reads. probe: graphix-fuzz typemorph
                // design/review-2026-10-05/repro/fuzz-mutate-09.gx (the generators
                // never emit these shapes; hand-run typemorph and mutants can).
                // (fuzz-mutate-09)
                _ => false,
            }
    })
}

struct StmtNames {
    /// `None` = the statement binds through a pattern this analysis
    /// doesn't enumerate — treat as unknown and disqualify.
    bound: Option<Vec<ArcStr>>,
    refs: HashSet<String>,
    connects: bool,
}

fn stmt_names(e: &Expr) -> StmtNames {
    let bound = match &e.kind {
        // a bind whose value leaks further names binds more than its
        // pattern says
        ExprKind::Bind(b) if leaks_binds(&b.value) => None,
        ExprKind::Bind(b) => match &b.pattern {
            StructurePattern::Bind(n) => Some(vec![n.name.clone()]),
            _ => None,
        },
        _ if leaks_binds(e) => None,
        _ => Some(Vec::new()),
    };
    let mut refs = HashSet::new();
    let mut connects = false;
    e.fold((), &mut |(), n| match &n.kind {
        ExprKind::Ref { name } => {
            // the full spelling and the leading segment: `dr0::f` depends
            // on whichever sibling binds `dr0`
            let s = name.to_string();
            for sep in ["::", "/"] {
                if let Some((first, _)) = s.split_once(sep) {
                    let first = first.trim_start_matches('/');
                    if !first.is_empty() {
                        refs.insert(first.to_string());
                    }
                }
            }
            refs.insert(s);
        }
        ExprKind::Connect { .. } => connects = true,
        _ => (),
    });
    StmtNames { bound, refs, connects }
}

fn permutable(a: &Expr, b: &Expr, open: &HashSet<String>) -> bool {
    let kind_ok = |e: &Expr| {
        !matches!(
            e.kind,
            ExprKind::NoOp
                | ExprKind::Use { .. }
                | ExprKind::Module { .. }
                | ExprKind::TypeDef(_)
                | ExprKind::Trait(_)
                | ExprKind::Impl(_)
                | ExprKind::Connect { .. }
                | ExprKind::Catch(_)
        )
    };
    if !kind_ok(a) || !kind_ok(b) {
        return false;
    }
    let na = stmt_names(a);
    let nb = stmt_names(b);
    let (Some(ba), Some(bb)) = (&na.bound, &nb.bound) else {
        return false;
    };
    !na.connects
        && !nb.connects
        && ba.iter().all(|n| {
            let n = n.to_string();
            !nb.refs.contains(&n) && !bb.iter().any(|m| m.as_str() == n)
        })
        && bb.iter().all(|n| !na.refs.contains(&n.to_string()))
        && !na.refs.iter().any(|r| open.contains(r) && nb.refs.contains(r))
}

/// Does this subtree introduce names the enclosing statement list can
/// read? A declaration does, and a dynamic module wherever it stands; an
/// interior `Do` or `Lambda` contains its own.
fn leaks_binds(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::TypeDef(_)
        | ExprKind::Use { .. }
        | ExprKind::Module { .. }
        | ExprKind::Trait(_)
        | ExprKind::Impl(_)
        | ExprKind::Catch(_) => true,
        ExprKind::Block { .. } | ExprKind::Lambda(_) => false,
        ExprKind::Select(s) => leaks_binds(&s.arg),
        _ => {
            let mut found = false;
            e.for_each_child(&mut |c| found = found || leaks_binds(c));
            found
        }
    }
}

/// Visit, with its preorder index (from `idx`), every node under `e`
/// that the binding of `f` in force at `e` reaches: the statements of a
/// block, a seq body or a `try` lose it after one rebinds `f`
/// ([`binds_after`]), and a form that binds `f` itself (a lambda
/// parameter, a select arm, a catch) hides all of its children.
pub(crate) fn for_each_reached(
    e: &Expr,
    f: &str,
    reached: bool,
    idx: &mut usize,
    visit: &mut impl FnMut(usize, &Expr),
) {
    if reached {
        visit(*idx, e);
    }
    *idx += 1;
    fn stmts(
        stmts: &[Expr],
        f: &str,
        mut reached: bool,
        idx: &mut usize,
        visit: &mut impl FnMut(usize, &Expr),
    ) {
        for st in stmts {
            for_each_reached(st, f, reached, idx, visit);
            reached &= !binds_after(st, f);
        }
    }
    let inner = reached && !binds(e, f);
    match &e.kind {
        ExprKind::Block { exprs } => stmts(exprs, f, reached, idx, visit),
        ExprKind::Seq { kind, trigger, abort, body } => {
            let heads = trigger
                .iter()
                .map(|t| t.expr())
                .chain(abort.iter().map(|a| &**a))
                .chain(kind.flush().into_iter().map(|a| &**a));
            for h in heads {
                for_each_reached(h, f, inner, idx, visit);
            }
            stmts(body, f, inner, idx, visit)
        }
        ExprKind::TryWith(t) => {
            stmts(&t.body, f, inner, idx, visit);
            stmts(&t.handler, f, inner, idx, visit)
        }
        _ => e.for_each_child(&mut |c| for_each_reached(c, f, inner, idx, visit)),
    }
}

/// The preorder indices of the calls of `f` under `e` that the binding
/// of `f` in force at `e` reaches ([`for_each_reached`]).
pub(crate) fn calls_reached(
    e: &Expr,
    f: &str,
    reached: bool,
    idx: &mut usize,
    out: &mut Vec<usize>,
) {
    for_each_reached(e, f, reached, idx, &mut |at, n| {
        if let ExprKind::Apply(ap) = &n.kind
            && matches!(&ap.function.kind, ExprKind::Ref { name } if name.to_string() == f)
        {
            out.push(at)
        }
    });
}

/// Whether an expression reads no name at all (a literal, an operator
/// over literals), so it means the same thing anywhere.
fn reads_no_name(e: &Expr) -> bool {
    !e.fold(false, &mut |found, n| found || matches!(n.kind, ExprKind::Ref { .. }))
}

/// Whether `n` itself introduces `name`. A `use` binds each item's
/// rename or last segment, a glob anything; a form whose names this does
/// not enumerate (`mod`, traits and impls) binds anything.
pub(crate) fn binds(n: &Expr, name: &str) -> bool {
    match &n.kind {
        ExprKind::Bind(b) => pattern_binds(&b.pattern, name),
        ExprKind::Lambda(l) => l.args.iter().any(|a| pattern_binds(&a.pattern, name)),
        ExprKind::Select(s) => {
            s.arms.iter().any(|(p, _)| pattern_binds(&p.structure_predicate, name))
        }
        ExprKind::Catch(c) => &*c.bind == name,
        ExprKind::TryWith(t) => &*t.bind == name,
        ExprKind::Seq { trigger: Some(SeqTrigger::Bind(b)), .. } => {
            pattern_binds(&b.pattern, name)
        }
        ExprKind::Use { names, .. } => names.iter().any(|u| {
            u.is_glob()
                || match &u.rename {
                    Some(r) => &**r == name,
                    None => Path::basename(&u.path.0) == Some(name),
                }
        }),
        ExprKind::Module { .. } | ExprKind::Trait(_) | ExprKind::Impl(_) => true,
        _ => false,
    }
}

/// Whether statement `st` binds `name` for the statements after it: by
/// its own form, or by a dynamic module nested where it leaks out
/// (`let a = mod m dynamic { .. }` binds `m` too; see [`leaks_binds`]).
pub(crate) fn binds_after(st: &Expr, name: &str) -> bool {
    fn leaks(e: &Expr, name: &str) -> bool {
        let mut found = false;
        let mut child = |c: &Expr| {
            let declares = matches!(c.kind, ExprKind::Module { .. });
            found = found || (declares && binds(c, name)) || leaks(c, name)
        };
        match &e.kind {
            ExprKind::Block { .. } | ExprKind::Lambda(_) => (),
            ExprKind::Select(s) => child(&s.arg),
            _ => e.for_each_child(&mut child),
        }
        found
    }
    binds(st, name) || leaks(st, name)
}

/// Does the pattern bind `name` anywhere? Conservative: an
/// unrecognized pattern form claims it does.
fn pattern_binds(p: &StructurePattern, name: &str) -> bool {
    let all_binds = |all: &Option<Name>, binds: &Arc<[StructurePattern]>| {
        all.as_ref().is_some_and(|a| &**a == name)
            || binds.iter().any(|p| pattern_binds(p, name))
    };
    match p {
        StructurePattern::Ignore | StructurePattern::Literal(_) => false,
        StructurePattern::Bind(n) => &**n == name,
        StructurePattern::Slice { list: _, all, binds } => all_binds(all, binds),
        StructurePattern::SlicePrefix { list: _, all, prefix, tail } => {
            all_binds(all, prefix) || tail.as_ref().is_some_and(|t| &**t == name)
        }
        StructurePattern::SliceSuffix { all, head, suffix } => {
            all_binds(all, suffix) || head.as_ref().is_some_and(|h| &**h == name)
        }
        StructurePattern::Tuple { all, binds } => all_binds(all, binds),
        StructurePattern::Variant { all, binds, .. } => all_binds(all, binds),
        _ => true,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const BODY: &str = "{ let a = i64:1; let b = array::map([i64:1], |x| x + i64:2); let c = a + i64:3; c }";

    #[test]
    fn probes_generate_and_reparse() {
        let (probes, noparse) = probes(BODY, 3);
        assert!(noparse == 0, "printer failed to round-trip {noparse} candidates");
        assert!(
            probes.iter().any(|p| p.kind == TmKind::LetExtract),
            "the map callback must yield a let-extract site"
        );
        assert!(probes.iter().any(|p| p.kind == TmKind::ParensWrap));
        for p in &probes {
            assert!(
                mutate::parse(&p.body).is_some(),
                "{}: candidate does not reparse: {}",
                p.id(),
                p.body
            );
        }
        let extract = probes.iter().find(|p| p.kind == TmKind::LetExtract).unwrap();
        assert!(extract.body.contains("tm__0"), "{}", extract.body);
    }

    #[test]
    fn probes_deterministic() {
        let (a, _) = probes(BODY, 3);
        let (b, _) = probes(BODY, 3);
        let a: Vec<_> = a.iter().map(|p| (p.id(), p.body.clone())).collect();
        let b: Vec<_> = b.iter().map(|p| (p.id(), p.body.clone())).collect();
        assert_eq!(a, b);
    }

    #[test]
    fn inline_respects_shadowing() {
        // `a` is used twice — no inline site.
        let body = "{ let a = i64:1; let b = a + a; b + b }";
        let (probes, _) = probes(body, 8);
        assert!(
            probes.iter().all(|p| p.kind != TmKind::LetInline),
            "double use must not inline"
        );
    }

    #[test]
    fn inline_does_not_capture() {
        // (body, the let whose one use sits where a name its value reads
        // is rebound)
        for (body, kept) in [
            (
                "{ let v6 = &f64:3.14; let v7 = [v6, v6]; let z = { let v6: f64 = f64:0.1; *(v7[0]$) }; z }",
                "let v7",
            ),
            (
                "{ let a = i64:1; let b = a + i64:1; let z = select i64:2 { a => b + a }; z }",
                "let b",
            ),
            (
                "{ let a = i64:1; let b = a + i64:1; let f = |a: i64| b + a; f(i64:3) }",
                "let b",
            ),
        ] {
            let (probes, _) = probes(body, 8);
            assert!(
                probes
                    .iter()
                    .all(|p| p.kind != TmKind::LetInline || p.body.contains(kept)),
                "`{kept}` inlined into a scope that rebinds its reads: {body}"
            );
        }
    }

    #[test]
    fn inline_keeps_places_seq_bodies_and_binders() {
        // (body, the let that must stay: its use is a place root, its
        // value holds a catch and its use sits in a seq body, or its value
        // binds a name a statement before the use reads)
        for (body, kept) in [
            ("{ let a = [10, 20]; let r = &a[1]; let t = *r; t }", "let a"),
            (
                "{ let v = { catch(e) 1; (in0 %? in0)? }; let t = in0; seq t { v } }",
                "let v",
            ),
        ] {
            let (probes, _) = probes(body, 8);
            assert!(
                probes
                    .iter()
                    .all(|p| p.kind != TmKind::LetInline || p.body.contains(kept)),
                "`{kept}` inlined: {body}"
            );
        }
    }

    #[test]
    fn block_wrap_keeps_place_roots() {
        let (probes, _) = probes("{ let a = [10, 20]; let r = &a[1]; *r }", 16);
        assert!(
            probes
                .iter()
                .all(|p| p.kind != TmKind::BlockWrap || p.body.contains("&a[1]")),
            "a place root wrapped"
        );
    }

    #[test]
    fn never_moves_through_no_let() {
        let body =
            "{ let n = never(); let a = [((1, never()), 10), ((100, 4), 20)]; (a, n) }";
        let (probes, _) = probes(body, 16);
        for p in &probes {
            match p.kind {
                TmKind::BlockWrap => {
                    assert!(!p.body.contains("tm__0 = never()"), "wrapped: {}", p.body)
                }
                TmKind::LetInline => {
                    assert!(p.body.contains("let n = never()"), "inlined: {}", p.body)
                }
                _ => (),
            }
        }
        assert!(probes.iter().any(|p| p.kind == TmKind::BlockWrap), "other sites wrap");
    }

    fn bodies(body: &str, kind: TmKind) -> Vec<String> {
        let (probes, noparse) = probes(body, 8);
        assert_eq!(noparse, 0, "{body}");
        probes.into_iter().filter(|p| p.kind == kind).map(|p| p.body).collect()
    }

    #[test]
    fn label_transforms() {
        let f = "let f = |#a: i64 = 3, #b: string, x: i64| -> i64 a + x";
        let permuted =
            bodies(&format!("{{ {f}; f(#a: 1, #b: \"s\", 2) }}"), TmKind::LabelPermute);
        assert!(
            permuted.iter().any(|b| b.contains("f(#b: \"s\", #a: 1, 2)")),
            "{permuted:?}"
        );
        let made =
            bodies(&format!("{{ {f}; f(#b: \"s\", 4) }}"), TmKind::DefaultMaterialize);
        assert!(made.iter().any(|b| b.contains("f(#a: 3, #b: \"s\", 4)")), "{made:?}");
        let elided =
            bodies(&format!("{{ {f}; f(#a: 3, #b: \"s\", 4) }}"), TmKind::DefaultElide);
        assert!(elided.iter().any(|b| b.contains("f(#b: \"s\", 4)")), "{elided:?}");
    }

    #[test]
    fn defaults_move_only_where_they_mean_the_same() {
        // the default reads a name
        let reads = "{ let k = 3; let f = |#a: i64 = k, x: i64| -> i64 a + x; f(4) }";
        assert!(bodies(reads, TmKind::DefaultMaterialize).is_empty());
        // `f` is rebound after its definition
        let shadowed = "{ let f = |#a: i64 = 3, x: i64| -> i64 a + x; let f = |x: i64| -> i64 x; f(4) }";
        assert!(bodies(shadowed, TmKind::DefaultMaterialize).is_empty());
        // a lambda parameter named `f` hides only its own body's calls
        let nested = "{ let f = |#a: i64 = 3, x: i64| -> i64 a + x; \
                      let g = |f: i64| f + 1; let z = f(4); g(z) }";
        let made = bodies(nested, TmKind::DefaultMaterialize);
        assert!(made.iter().any(|b| b.contains("f(#a: 3, 4)")), "{made:?}");
        let hidden = "{ let f = |#a: i64 = 3, x: i64| -> i64 a + x; \
                      let g = |f: fn(x: i64) -> i64| f(4); g(|x| x) }";
        assert!(bodies(hidden, TmKind::DefaultMaterialize).is_empty());
    }

    #[test]
    fn inline_keeps_the_value_whole() {
        // `~` binds loosest: unparenthesized, `.. - v1` with `v1 = in0 ~ e`
        // prints as `(.. - in0) ~ e`, another program
        let body =
            "{ let v0 = in0 ~ i64:1; let v1 = in0 ~ (i64:7 * v0); (v0 - v0) - v1 }";
        let (probes, noparse) = probes(body, 8);
        assert_eq!(noparse, 0);
        let inlined: Vec<_> =
            probes.iter().filter(|p| p.kind == TmKind::LetInline).collect();
        assert!(!inlined.is_empty(), "v1 has one use");
        for p in inlined {
            let back = mutate::parse(&p.body).expect("a probe parses");
            assert_eq!(back.to_string(), p.body, "a probe reads back as printed");
        }
        let bodies: Vec<&str> = probes.iter().map(|p| p.body.as_str()).collect();
        assert!(bodies.iter().any(|b| b.contains("- (in0 ~")), "{bodies:?}");
    }

    #[test]
    fn guard_refs_are_dependencies() {
        // `m`'s only use is inside a select guard: a dependency for
        // stmt-permute and a single use for let-inline
        let body = "{ let m = i64:1; let rec f = |n: i64| -> i64 \
                    select n { i64:0 if m == i64:0 => i64:1, i64:0 => i64:2, _ => f(n - i64:1) }; \
                    f(i64:2) }";
        let (probes, _) = probes(body, 8);
        assert!(
            probes.iter().all(|p| p.kind != TmKind::StmtPermute || p.site != 0),
            "must not swap a def past a guard-only use"
        );
        let inlined: Vec<_> = probes
            .iter()
            .filter(|p| p.kind == TmKind::LetInline && p.site == 0)
            .collect();
        assert_eq!(inlined.len(), 1, "one inline of the guard use");
        assert!(
            inlined[0].body.contains("(1) == 0") && !inlined[0].body.contains("let m"),
            "the guard use takes the value: {}",
            inlined[0].body
        );
    }

    #[test]
    fn a_dynamic_module_binds_its_name() {
        // `let v = mod m dynamic { .. }` binds `m` for the statements after
        // it (sep29b)
        let body = "{ let src = \"let f = |x: i64| -> i64 x\"; \
                    let v = mod m dynamic { sandbox whitelist [core]; \
                    sig { val f: fn(x: i64) -> i64 }; source src }; \
                    let r = m::f(i64:1); r }";
        let (probes, _) = probes(body, 8);
        assert!(
            probes.iter().all(|p| p.kind != TmKind::StmtPermute || p.site != 1),
            "must not move a use of `m` above its module"
        );
    }

    #[test]
    fn readers_of_a_bottom_let_do_not_commute() {
        let body = "{ let g = never(); let a: Array<fn(?#x: i64) -> i64> = [g]; \
                    let b: Array<fn(#x: i64) -> i64> = [g]; array::len(a) + array::len(b) }";
        let (probes, _) = probes(body, 8);
        assert!(
            probes.iter().all(|p| p.kind != TmKind::StmtPermute || p.site != 1),
            "the first reader of `g` decides its type"
        );
    }

    #[test]
    fn extract_skips_param_type_reads() {
        for body in [
            "{ let k = i64:0; array::map([k], |x| select x { i64 as n => n, _ => i64:0 }) }",
            "{ let k = u8:1; map::filter({\"k\" => k}, |kv| str::len(kv.0) > i64:1) }",
            "{ let k = i64:1; array::map([{a: k}], |r| (r).a) }",
            "{ let k = u8:1; array::map([{n: k, y: k}], |r| { r with y: k }) }",
            "{ let k = u8:1; array::fold([true], k, |acc, x| select (acc %? acc) { error as _ => acc, u8 as n => n }) }",
            "{ let k = 1; array::map([((1, 2), k)], |(pt, n)| pt.0 + n) }",
        ] {
            let (probes, _) = probes(body, 8);
            assert!(
                probes.iter().all(|p| p.kind != TmKind::LetExtract),
                "the callback reads its parameter's type: {body}"
            );
        }
        let (probes, _) =
            probes("{ let k = i64:1; array::map([k], |x: i64| select x { n => n }) }", 8);
        assert!(
            probes.iter().any(|p| p.kind == TmKind::LetExtract),
            "an annotated parameter is extractable"
        );
    }

    #[test]
    fn extract_does_not_cross_scopes() {
        for body in [
            // the callback lambda sits inside another lambda's body
            "{ let f = |y: i64| array::map([i64:1], |x| x + y); f(i64:1) }",
        ] {
            let (probes, _) = probes(body, 8);
            assert!(
                probes.iter().all(|p| p.kind != TmKind::LetExtract),
                "must not extract out of the callback's scope: {body}"
            );
        }
    }
}
