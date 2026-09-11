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
        BindExpr, Expr, ExprKind, ModPath, StructurePattern, TypeDefBody, TypeDefExpr,
    },
    typ::{TVar, Type, TypeRef},
};
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
    let push = |out: &mut Vec<TmProbe>, noparse: &mut usize, kind, site, cand: &Expr| {
        let text = cand.to_string();
        if mutate::parse(&text).is_some() {
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
    // literals (let-extract's probe) or blocks
    {
        let sites: Vec<usize> = (0..pre.len())
            .filter(|&i| {
                value_pos(&pre[i].kind)
                    && !matches!(pre[i].kind, ExprKind::Lambda(_) | ExprKind::Do { .. })
                    && !leaks_binds(&pre[i])
            })
            .collect();
        for i in sample(&sites, cap) {
            let bind = ExprKind::Bind(Arc::new(BindExpr {
                rec: false,
                pattern: StructurePattern::Bind(ArcStr::from(VAL)),
                typ: None,
                value: pre[i].clone(),
            }))
            .to_expr_nopos();
            let r = ExprKind::Ref { name: mp(VAL) }.to_expr_nopos();
            let repl = ExprKind::Do { exprs: Arc::from_iter([bind, r]) }.to_expr_nopos();
            let cand = mutate::replace(&root, i, &repl);
            push(&mut out, &mut noparse, TmKind::BlockWrap, i, &cand);
        }
    }
    let ExprKind::Do { exprs: stmts } = &root.kind else {
        return (out, noparse);
    };
    let stmts: Vec<Expr> = stmts.to_vec();
    let sizes = mutate::sizes(&root);
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
    // non-scoping nodes, else the lambda's captures would be stranded.
    {
        let mut found: Vec<(usize, usize)> = Vec::new();
        for (si, stmt) in stmts.iter().enumerate() {
            let mut idx = offsets[si];
            find_lambda_args(stmt, &mut idx, false, &mut |gi| found.push((si, gi)));
        }
        for (si, gi) in sample(&found, cap) {
            let r = ExprKind::Ref { name: mp(VAL) }.to_expr_nopos();
            let replaced = mutate::replace(&root, gi, &r);
            let ExprKind::Do { exprs } = &replaced.kind else { continue };
            let bind = ExprKind::Bind(Arc::new(BindExpr {
                rec: false,
                pattern: StructurePattern::Bind(ArcStr::from(VAL)),
                typ: None,
                value: pre[gi].clone(),
            }))
            .to_expr_nopos();
            let mut v: Vec<Expr> = exprs.to_vec();
            v.insert(si, bind);
            let cand = ExprKind::Do { exprs: Arc::from_iter(v) }.to_expr_nopos();
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
            if b.rec || b.typ.is_some() {
                continue;
            }
            let StructurePattern::Bind(name) = &b.pattern else { continue };
            let nm = name.to_string();
            let vrefs = stmt_names(&stmts[si]).refs;
            let mut uses = 0usize;
            let mut ok = true;
            for later in &stmts[si + 1..] {
                later.fold((), &mut |(), n| match &n.kind {
                    ExprKind::Ref { name } if name.to_string() == nm => uses += 1,
                    ExprKind::Connect { name, .. } if name.to_string() == nm => {
                        ok = false
                    }
                    ExprKind::Bind(lb) if pattern_binds(&lb.pattern, &nm) => ok = false,
                    ExprKind::Lambda(l)
                        if l.args.iter().any(|a| pattern_binds(&a.pattern, &nm)) =>
                    {
                        ok = false
                    }
                    _ => (),
                });
                // a later top-level bind shadowing a name the value
                // references would capture the moved expression
                if let ExprKind::Bind(lb) = &later.kind
                    && let StructurePattern::Bind(ln) = &lb.pattern
                    && vrefs.contains(&ln.to_string())
                {
                    ok = false;
                }
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
            let replaced = mutate::replace(&root, gi, &b.value);
            let ExprKind::Do { exprs } = &replaced.kind else { continue };
            let v: Vec<Expr> = exprs
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != si)
                .map(|(_, e)| e.clone())
                .collect();
            let cand = ExprKind::Do { exprs: Arc::from_iter(v) }.to_expr_nopos();
            push(&mut out, &mut noparse, TmKind::LetInline, si, &cand);
            done += 1;
        }
    }
    // stmt-permute: swap adjacent independent statements
    {
        let mut sites = Vec::new();
        for i in 0..stmts.len().saturating_sub(1) {
            if permutable(&stmts[i], &stmts[i + 1]) {
                sites.push(i);
            }
        }
        for i in sample(&sites, cap) {
            let mut v = stmts.clone();
            v.swap(i, i + 1);
            let cand = ExprKind::Do { exprs: Arc::from_iter(v) }.to_expr_nopos();
            push(&mut out, &mut noparse, TmKind::StmtPermute, i, &cand);
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
                name: ArcStr::from(TYP),
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
            let cand = ExprKind::Do { exprs: Arc::from_iter(v) }.to_expr_nopos();
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
                | ExprKind::Do { .. }
                | ExprKind::Seq { .. }
                | ExprKind::TryWith(_)
        );
    *idx += 1;
    e.for_each_child(&mut |c| {
        if at_apply && matches!(c.kind, ExprKind::Lambda(_)) {
            f(*idx);
        }
        find_lambda_args(c, idx, blocked, f);
    });
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
            StructurePattern::Bind(n) => Some(vec![n.clone()]),
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

fn permutable(a: &Expr, b: &Expr) -> bool {
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
}

/// Does this subtree introduce names the enclosing statement list can
/// read? A statement binds whatever its subtree binds; an interior `Do`
/// or `Lambda` contains its own binds, and only a select's scrutinee leaks.
fn leaks_binds(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::Bind(_)
        | ExprKind::TypeDef(_)
        | ExprKind::Use { .. }
        | ExprKind::Module { .. }
        | ExprKind::Trait(_)
        | ExprKind::Impl(_)
        | ExprKind::Catch(_) => true,
        ExprKind::Do { .. } | ExprKind::Lambda(_) => false,
        ExprKind::Select(s) => leaks_binds(&s.arg),
        _ => {
            let mut found = false;
            e.for_each_child(&mut |c| found = found || leaks_binds(c));
            found
        }
    }
}

/// Does the pattern bind `name` anywhere? Conservative: an
/// unrecognized pattern form claims it does.
fn pattern_binds(p: &StructurePattern, name: &str) -> bool {
    let all_binds = |all: &Option<ArcStr>, binds: &Arc<[StructurePattern]>| {
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
            inlined[0].body.contains("i64:1 == i64:0")
                && !inlined[0].body.contains("let m"),
            "the guard use takes the value: {}",
            inlined[0].body
        );
    }

    #[test]
    fn extract_does_not_cross_scopes() {
        // the callback lambda sits inside another lambda's body
        let body = "{ let f = |y: i64| array::map([i64:1], |x| x + y); f(i64:1) }";
        let (probes, _) = probes(body, 8);
        assert!(
            probes.iter().all(|p| p.kind != TmKind::LetExtract),
            "must not extract across a lambda boundary"
        );
    }
}
