//! AST-to-AST lowering of `seq` (`design/seq_blocks.md` §7).
//!
//! Straight-line only: lets, connects, expression steps, `until`, one
//! catch at the top. Each step is its own arm. Presence-select + a free
//! read of `pc` is the issue atom.

use super::{
    ApplyExpr, Arg, BindExpr, CatchExpr, Expr, ExprKind, LambdaExpr, ModPath, Pattern,
    SelectExpr, StructurePattern,
};
use crate::{
    expr::ErrorContext,
    typ::{TVar, Type},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use netidx_core::{path::Path, utils::Either};
use netidx_value::Value;
use triomphe::Arc;

pub fn desugar(spec: &Expr) -> Result<Expr> {
    let ExprKind::Seq { trigger, body } = &spec.kind else {
        panic!("desugar_seq on a non-seq");
    };
    let pos = spec.pos;
    let id = spec.id.inner();
    if body.is_empty() {
        return Err(anyhow!("a seq block must contain at least one step")
            .context(ErrorContext(spec.clone())));
    }
    let mut catch: Option<Arc<CatchExpr>> = None;
    let mut steps: Vec<&Expr> = Vec::new();
    for e in body.iter() {
        match &e.kind {
            ExprKind::Catch(c) if steps.is_empty() => {
                if catch.is_some() {
                    return Err(anyhow!("a seq block has one catch, at the top")
                        .context(ErrorContext(e.clone())));
                }
                catch = Some(c.clone());
            }
            ExprKind::Catch(_) => {
                return Err(anyhow!("a seq catch must be the first statement")
                    .context(ErrorContext(e.clone())));
            }
            ExprKind::NoOp => (),
            ExprKind::Until(_) | ExprKind::Bind(_) | _ => steps.push(e),
        }
    }
    if steps.is_empty() {
        return Err(anyhow!("a seq block must contain at least one step")
            .context(ErrorContext(spec.clone())));
    }
    let n = steps.len();
    let pc = format_compact!("seqpc{id}");
    let idle = format_compact!("seqidle{id}");
    let result = format_compact!("seqr{id}");
    let trig_cell = format_compact!("seqt{id}");

    let mut names: AHashMap<ArcStr, ArcStr> = AHashMap::new();
    let mut cells: Vec<(ArcStr, SourcePosition)> = Vec::new();
    if let Some(t) = trigger.as_ref() {
        if let Some(n) = simple_ref_name(t) {
            names.insert(n.clone(), ArcStr::from(trig_cell.as_str()));
            cells.push((ArcStr::from(trig_cell.as_str()), t.pos));
        }
    }
    for (i, e) in steps.iter().enumerate() {
        if let ExprKind::Bind(b) = &e.kind {
            if b.rec {
                return Err(anyhow!("let rec is not a seq step")
                    .context(ErrorContext((*e).clone())));
            }
            let mut ns: Vec<ArcStr> = Vec::new();
            b.pattern.with_names(&mut |n| ns.push(n.clone()));
            for n in ns {
                let cell = ArcStr::from(format_compact!("seqc{id}_{i}_{n}").as_str());
                names.insert(n, cell.clone());
                cells.push((cell, e.pos));
            }
        }
    }

    let pc_typ = pc_type(n);
    let mut prelude: Vec<Expr> = Vec::new();
    prelude.push(let_bind(pos, pc.as_str(), Some(pc_typ), variant(pos, "Idle")));
    prelude.push(let_bind(pos, idle.as_str(), None, idle_of(pos, pc.as_str())));
    prelude.push(let_bind(pos, result.as_str(), None, never(pos)));
    for (cell, cpos) in &cells {
        prelude.push(let_bind(*cpos, cell.as_str(), None, never(*cpos)));
    }

    let trig_expr = match trigger {
        Some(t) => (**t).clone(),
        None => ExprKind::Constant(Value::Bool(true)).to_expr(pos),
    };
    let t_name = format_compact!("seqgo{id}");
    let filter = apply_filter(pos, trig_expr, lambda_idle(pos, idle.as_str()));
    let mut start: Vec<Expr> = Vec::new();
    start.push(let_bind(pos, t_name.as_str(), None, filter));
    start.push(connect(
        pos,
        pc.as_str(),
        sample(pos, r#ref(pos, t_name.as_str()), variant(pos, "S0")),
    ));
    if names.values().any(|c| c.as_str() == trig_cell.as_str()) {
        start.push(connect(pos, trig_cell.as_str(), r#ref(pos, t_name.as_str())));
    }

    let mut visible: AHashMap<ArcStr, ArcStr> = AHashMap::new();
    if let Some(t) = trigger {
        if let Some(n) = simple_ref_name(t) {
            visible.insert(n, ArcStr::from(trig_cell.as_str()));
        }
    }
    let err_bind =
        catch.as_ref().map(|c| c.bind.clone()).unwrap_or_else(|| ArcStr::from("e"));
    let may_throw = steps.iter().any(|e| expr_may_throw(e))
        || catch.as_ref().is_some_and(|c| expr_may_throw(&c.handler));
    let catch_node = if catch.is_some() || may_throw {
        let reset = connect(
            pos,
            pc.as_str(),
            sample(pos, r#ref(pos, err_bind.as_str()), variant(pos, "Idle")),
        );
        let mut handler_map = visible.clone();
        handler_map.remove(&err_bind);
        let user = catch.as_ref().map(|c| rewrite(&c.handler, &handler_map));
        let mut handler_body = Vec::new();
        if let Some(h) = user {
            handler_body.push(h);
        }
        handler_body.push(reset);
        if may_throw {
            handler_body.push(qop(pos, r#ref(pos, err_bind.as_str())));
        }
        Some(
            ExprKind::Catch(Arc::new(CatchExpr {
                bind: err_bind,
                constraint: catch.as_ref().and_then(|c| c.constraint.clone()),
                handler: Arc::new(block(pos, handler_body)),
            }))
            .to_expr(pos),
        )
    } else {
        None
    };

    let vname = format_compact!("seqv{id}");
    let mut arms: Vec<(Pattern, Expr)> = Vec::new();
    arms.push((pat_variant("Idle"), never(pos)));
    for (i, step) in steps.iter().enumerate() {
        let tag = format_compact!("S{i}");
        let next = if i + 1 == n {
            ArcStr::from("Idle")
        } else {
            ArcStr::from(format_compact!("S{}", i + 1).as_str())
        };
        let last = i + 1 == n;
        let mut this_let: AHashMap<ArcStr, ArcStr> = AHashMap::new();
        if let ExprKind::Bind(b) = &step.kind {
            b.pattern.with_names(&mut |n| {
                if let Some(cell) = names.get(n) {
                    this_let.insert(n.clone(), cell.clone());
                }
            });
        }
        let arm = step_arm(
            step,
            pc.as_str(),
            next.as_str(),
            last,
            result.as_str(),
            vname.as_str(),
            &visible,
            &this_let,
        )?;
        arms.push((pat_variant(&tag), arm));
        visible.extend(this_let);
    }

    let machine = select(pos, r#ref(pos, pc.as_str()), arms);
    let mut body_exprs = prelude;
    if let Some(c) = catch_node {
        body_exprs.push(c);
    }
    body_exprs.extend(start);
    body_exprs.push(machine);
    body_exprs.push(r#ref(pos, result.as_str()));
    Ok(block(pos, body_exprs))
}

fn step_arm(
    step: &Expr,
    pc: &str,
    next: &str,
    last: bool,
    result: &str,
    vname: &str,
    visible: &AHashMap<ArcStr, ArcStr>,
    this_let: &AHashMap<ArcStr, ArcStr>,
) -> Result<Expr> {
    let pos = step.pos;
    let trans = connect(pos, pc, sample(pos, r#ref(pos, pc), variant(pos, next)));
    match &step.kind {
        ExprKind::Until(e) => {
            let e = rewrite(e, visible);
            Ok(select(
                pos,
                e,
                vec![
                    (pat_lit(Value::Bool(true)), trans),
                    (pat_lit(Value::Bool(false)), never(pos)),
                ],
            ))
        }
        ExprKind::Bind(b) => {
            let value = rewrite(&b.value, visible);
            let mut body =
                vec![let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, vname))];
            b.pattern.with_names(&mut |n| {
                let cell = this_let.get(n).cloned().unwrap_or_else(|| n.clone());
                body.push(connect(
                    pos,
                    cell.as_str(),
                    sample(pos, r#ref(pos, pc), r#ref(pos, n.as_str())),
                ));
            });
            if last {
                body.push(connect(
                    pos,
                    result,
                    sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
                ));
            }
            body.push(trans);
            Ok(select(pos, value, vec![(pat_bind(vname), block(pos, body))]))
        }
        ExprKind::Connect { name, value, deref } => {
            let value = rewrite(value, visible);
            let target = rewrite_path(name, visible);
            let mut body = vec![connect_path(
                pos,
                target,
                *deref,
                sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
            )];
            if last {
                body.push(connect(
                    pos,
                    result,
                    sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
                ));
            }
            body.push(trans);
            Ok(select(pos, value, vec![(pat_bind(vname), block(pos, body))]))
        }
        _ => {
            let e = rewrite(step, visible);
            let mut body = Vec::new();
            if last {
                body.push(connect(
                    pos,
                    result,
                    sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
                ));
            }
            body.push(trans);
            Ok(select(pos, e, vec![(pat_bind(vname), block(pos, body))]))
        }
    }
}

fn expr_may_throw(e: &Expr) -> bool {
    e.fold(false, &mut |acc, n| acc || matches!(n.kind, ExprKind::Qop(_)))
}

fn pc_type(n_steps: usize) -> Type {
    let mut mem = Vec::with_capacity(n_steps + 1);
    mem.push(Type::Variant(ArcStr::from("Idle"), Arc::from(Vec::<Type>::new())));
    for i in 0..n_steps {
        mem.push(Type::Variant(
            ArcStr::from(format_compact!("S{i}").as_str()),
            Arc::from(Vec::<Type>::new()),
        ));
    }
    Type::Set(Arc::from(mem))
}

fn idle_of(pos: SourcePosition, pc: &str) -> Expr {
    select(
        pos,
        r#ref(pos, pc),
        vec![
            (pat_variant("Idle"), ExprKind::Constant(Value::Bool(true)).to_expr(pos)),
            (pat_wild(), ExprKind::Constant(Value::Bool(false)).to_expr(pos)),
        ],
    )
}

fn lambda_idle(pos: SourcePosition, idle: &str) -> Expr {
    let x = ArcStr::from("x");
    ExprKind::Lambda(Arc::new(LambdaExpr {
        args: Arc::from(vec![Arg {
            labeled: None,
            pattern: StructurePattern::Bind(x.clone()),
            constraint: None,
            pos,
        }]),
        vargs: None,
        rtype: None,
        constraints: Arc::from(Vec::<(TVar, Type)>::new()),
        throws: None,
        body: Either::Left(sample(pos, r#ref(pos, "x"), r#ref(pos, idle))),
    }))
    .to_expr(pos)
}

fn apply_filter(pos: SourcePosition, trig: Expr, pred: Expr) -> Expr {
    ExprKind::Apply(ApplyExpr {
        function: Arc::new(r#ref(pos, "filter")),
        args: Arc::from(vec![(None, trig), (None, pred)]),
    })
    .to_expr(pos)
}

fn simple_ref_name(e: &Expr) -> Option<ArcStr> {
    match &e.kind {
        ExprKind::Ref { name } => simple_name(name),
        _ => None,
    }
}

fn simple_name(p: &ModPath) -> Option<ArcStr> {
    if Path::levels(&p.0) == 1 {
        Path::parts(&p.0).next().map(ArcStr::from)
    } else {
        None
    }
}

fn rewrite_path(p: &ModPath, map: &AHashMap<ArcStr, ArcStr>) -> ModPath {
    match simple_name(p) {
        Some(n) => match map.get(&n) {
            Some(cell) => ModPath::from([cell.as_str()]),
            None => p.clone(),
        },
        None => p.clone(),
    }
}

fn rewrite(e: &Expr, map: &AHashMap<ArcStr, ArcStr>) -> Expr {
    if map.is_empty() {
        return e.clone();
    }
    let kind = match &e.kind {
        ExprKind::Ref { name } => ExprKind::Ref { name: rewrite_path(name, map) },
        ExprKind::Connect { name, value, deref } => ExprKind::Connect {
            name: rewrite_path(name, map),
            value: Arc::new(rewrite(value, map)),
            deref: *deref,
        },
        ExprKind::Until(x) => ExprKind::Until(Arc::new(rewrite(x, map))),
        ExprKind::Seq { trigger, body } => {
            let trigger = trigger.as_ref().map(|t| Arc::new(rewrite(t, map)));
            let mut inner = map.clone();
            let mut out = Vec::with_capacity(body.len());
            for x in body.iter() {
                out.push(rewrite(x, &inner));
                if let ExprKind::Bind(b) = &x.kind {
                    b.pattern.with_names(&mut |n| {
                        inner.remove(n);
                    });
                }
            }
            ExprKind::Seq { trigger, body: Arc::from(out) }
        }
        ExprKind::ExplicitParens(x) => {
            ExprKind::ExplicitParens(Arc::new(rewrite(x, map)))
        }
        ExprKind::Qop(x) => ExprKind::Qop(Arc::new(rewrite(x, map))),
        ExprKind::OrNever(x) => ExprKind::OrNever(Arc::new(rewrite(x, map))),
        ExprKind::ByRef(x) => ExprKind::ByRef(Arc::new(rewrite(x, map))),
        ExprKind::Deref(x) => ExprKind::Deref(Arc::new(rewrite(x, map))),
        ExprKind::Neg(x) => ExprKind::Neg(Arc::new(rewrite(x, map))),
        ExprKind::Not { expr } => ExprKind::Not { expr: Arc::new(rewrite(expr, map)) },
        ExprKind::TypeCast { expr, typ } => {
            ExprKind::TypeCast { expr: Arc::new(rewrite(expr, map)), typ: typ.clone() }
        }
        ExprKind::Construct { name, arg } => {
            ExprKind::Construct { name: name.clone(), arg: Arc::new(rewrite(arg, map)) }
        }
        ExprKind::Do { exprs } => {
            let mut inner = map.clone();
            let mut out = Vec::with_capacity(exprs.len());
            for x in exprs.iter() {
                out.push(rewrite(x, &inner));
                if let ExprKind::Bind(b) = &x.kind {
                    b.pattern.with_names(&mut |n| {
                        inner.remove(n);
                    });
                }
            }
            ExprKind::Do { exprs: Arc::from(out) }
        }
        ExprKind::StringInterpolate { args } => ExprKind::StringInterpolate {
            args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))),
        },
        ExprKind::Any { args } => {
            ExprKind::Any { args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))) }
        }
        ExprKind::Never { typ, args } => ExprKind::Never {
            typ: typ.clone(),
            args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))),
        },
        ExprKind::Array { args } => {
            ExprKind::Array { args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))) }
        }
        ExprKind::List { args } => {
            ExprKind::List { args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))) }
        }
        ExprKind::Tuple { args } => {
            ExprKind::Tuple { args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))) }
        }
        ExprKind::Variant { tag, args } => ExprKind::Variant {
            tag: tag.clone(),
            args: Arc::from_iter(args.iter().map(|x| rewrite(x, map))),
        },
        ExprKind::Bind(b) => ExprKind::Bind(Arc::new(BindExpr {
            rec: b.rec,
            pattern: b.pattern.clone(),
            typ: b.typ.clone(),
            value: rewrite(&b.value, map),
        })),
        ExprKind::StructRef { source, field } => ExprKind::StructRef {
            source: Arc::new(rewrite(source, map)),
            field: field.clone(),
        },
        ExprKind::TupleRef { source, field } => {
            ExprKind::TupleRef { source: Arc::new(rewrite(source, map)), field: *field }
        }
        ExprKind::ArrayRef { source, i } => ExprKind::ArrayRef {
            source: Arc::new(rewrite(source, map)),
            i: Arc::new(rewrite(i, map)),
        },
        ExprKind::ArraySlice { source, start, end } => ExprKind::ArraySlice {
            source: Arc::new(rewrite(source, map)),
            start: start.as_ref().map(|s| Arc::new(rewrite(s, map))),
            end: end.as_ref().map(|s| Arc::new(rewrite(s, map))),
        },
        ExprKind::MapRef { source, key } => ExprKind::MapRef {
            source: Arc::new(rewrite(source, map)),
            key: Arc::new(rewrite(key, map)),
        },
        ExprKind::Map { args } => ExprKind::Map {
            args: Arc::from_iter(
                args.iter().map(|(k, v)| (rewrite(k, map), rewrite(v, map))),
            ),
        },
        ExprKind::Struct(s) => ExprKind::Struct(super::StructExpr {
            args: Arc::from_iter(
                s.args.iter().map(|(n, v)| (n.clone(), rewrite(v, map))),
            ),
        }),
        ExprKind::StructWith(sw) => ExprKind::StructWith(super::StructWithExpr {
            source: Arc::new(rewrite(&sw.source, map)),
            replace: Arc::from_iter(
                sw.replace.iter().map(|(n, v)| (n.clone(), rewrite(v, map))),
            ),
        }),
        ExprKind::Apply(a) => ExprKind::Apply(ApplyExpr {
            function: Arc::new(rewrite(&a.function, map)),
            args: Arc::from_iter(
                a.args.iter().map(|(n, v)| (n.clone(), rewrite(v, map))),
            ),
        }),
        ExprKind::Select(s) => ExprKind::Select(SelectExpr {
            arg: Arc::new(rewrite(&s.arg, map)),
            arms: Arc::from_iter(s.arms.iter().map(|(p, b)| {
                let mut inner = map.clone();
                p.structure_predicate.with_names(&mut |n| {
                    inner.remove(n);
                });
                let guard = p.guard.as_ref().map(|g| rewrite(g, &inner));
                let mut p = p.clone();
                p.guard = guard;
                (p, rewrite(b, &inner))
            })),
        }),
        ExprKind::Catch(c) => {
            let mut inner = map.clone();
            inner.remove(&c.bind);
            ExprKind::Catch(Arc::new(CatchExpr {
                bind: c.bind.clone(),
                constraint: c.constraint.clone(),
                handler: Arc::new(rewrite(&c.handler, &inner)),
            }))
        }
        ExprKind::Lambda(l) => {
            let mut inner = map.clone();
            for a in l.args.iter() {
                a.pattern.with_names(&mut |n| {
                    inner.remove(n);
                });
            }
            let args: Vec<Arg> = l
                .args
                .iter()
                .map(|a| Arg {
                    labeled: match &a.labeled {
                        Some(Some(d)) => Some(Some(rewrite(d, map))),
                        other => other.clone(),
                    },
                    pattern: a.pattern.clone(),
                    constraint: a.constraint.clone(),
                    pos: a.pos,
                })
                .collect();
            let body = match &l.body {
                Either::Left(b) => Either::Left(rewrite(b, &inner)),
                Either::Right(s) => Either::Right(s.clone()),
            };
            ExprKind::Lambda(Arc::new(LambdaExpr {
                args: Arc::from(args),
                vargs: l.vargs.clone(),
                rtype: l.rtype.clone(),
                constraints: l.constraints.clone(),
                throws: l.throws.clone(),
                body,
            }))
        }
        ExprKind::Eq { lhs, rhs } => ExprKind::Eq {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Ne { lhs, rhs } => ExprKind::Ne {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Lt { lhs, rhs } => ExprKind::Lt {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Gt { lhs, rhs } => ExprKind::Gt {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Lte { lhs, rhs } => ExprKind::Lte {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Gte { lhs, rhs } => ExprKind::Gte {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::And { lhs, rhs } => ExprKind::And {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Or { lhs, rhs } => ExprKind::Or {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Add { lhs, rhs } => ExprKind::Add {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::CheckedAdd { lhs, rhs } => ExprKind::CheckedAdd {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Sub { lhs, rhs } => ExprKind::Sub {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::CheckedSub { lhs, rhs } => ExprKind::CheckedSub {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Mul { lhs, rhs } => ExprKind::Mul {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::CheckedMul { lhs, rhs } => ExprKind::CheckedMul {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Div { lhs, rhs } => ExprKind::Div {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::CheckedDiv { lhs, rhs } => ExprKind::CheckedDiv {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Mod { lhs, rhs } => ExprKind::Mod {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::CheckedMod { lhs, rhs } => ExprKind::CheckedMod {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::Sample { lhs, rhs } => ExprKind::Sample {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::StrictSample { lhs, rhs } => ExprKind::StrictSample {
            lhs: Arc::new(rewrite(lhs, map)),
            rhs: Arc::new(rewrite(rhs, map)),
        },
        ExprKind::NoOp
        | ExprKind::Constant(_)
        | ExprKind::Use { .. }
        | ExprKind::TypeDef(_)
        | ExprKind::Trait(_)
        | ExprKind::Impl(_)
        | ExprKind::Module { .. } => e.kind.clone(),
    };
    Expr {
        id: super::ExprId::new(),
        ori: e.ori.clone(),
        pos: e.pos,
        kind,
        dec: e.dec.clone(),
    }
}

fn let_bind(pos: SourcePosition, name: &str, typ: Option<Type>, value: Expr) -> Expr {
    ExprKind::Bind(Arc::new(BindExpr {
        rec: false,
        pattern: StructurePattern::Bind(ArcStr::from(name)),
        typ,
        value,
    }))
    .to_expr(pos)
}

fn let_pat(
    pos: SourcePosition,
    pattern: StructurePattern,
    typ: Option<Type>,
    value: Expr,
) -> Expr {
    ExprKind::Bind(Arc::new(BindExpr { rec: false, pattern, typ, value })).to_expr(pos)
}

fn connect(pos: SourcePosition, name: &str, value: Expr) -> Expr {
    connect_path(pos, ModPath::from([name]), false, value)
}

fn connect_path(pos: SourcePosition, name: ModPath, deref: bool, value: Expr) -> Expr {
    ExprKind::Connect { name, value: Arc::new(value), deref }.to_expr(pos)
}

fn r#ref(pos: SourcePosition, name: &str) -> Expr {
    ExprKind::Ref { name: ModPath::from([name]) }.to_expr(pos)
}

fn never(pos: SourcePosition) -> Expr {
    ExprKind::Never { typ: None, args: Arc::from(Vec::<Expr>::new()) }.to_expr(pos)
}

fn variant(pos: SourcePosition, tag: &str) -> Expr {
    ExprKind::Variant { tag: ArcStr::from(tag), args: Arc::from(Vec::<Expr>::new()) }
        .to_expr(pos)
}

fn sample(pos: SourcePosition, lhs: Expr, rhs: Expr) -> Expr {
    ExprKind::Sample { lhs: Arc::new(lhs), rhs: Arc::new(rhs) }.to_expr(pos)
}

fn qop(pos: SourcePosition, e: Expr) -> Expr {
    ExprKind::Qop(Arc::new(e)).to_expr(pos)
}

fn select(pos: SourcePosition, arg: Expr, arms: Vec<(Pattern, Expr)>) -> Expr {
    ExprKind::Select(SelectExpr { arg: Arc::new(arg), arms: Arc::from(arms) })
        .to_expr(pos)
}

fn block(pos: SourcePosition, mut exprs: Vec<Expr>) -> Expr {
    match exprs.len() {
        0 => never(pos),
        1 => exprs.pop().unwrap(),
        _ => ExprKind::Do { exprs: Arc::from(exprs) }.to_expr(pos),
    }
}

fn pat_bind(name: &str) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Bind(ArcStr::from(name)),
        guard: None,
    }
}

fn pat_wild() -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Ignore,
        guard: None,
    }
}

fn pat_lit(v: Value) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Literal(v),
        guard: None,
    }
}

fn pat_variant(tag: &str) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Variant {
            all: None,
            tag: ArcStr::from(tag),
            binds: Arc::from(Vec::<StructurePattern>::new()),
        },
        guard: None,
    }
}
