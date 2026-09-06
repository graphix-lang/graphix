//! AST-to-AST lowering of `seq` (`design/seq_blocks.md` §7).
//!
//! Straight-line only: lets, connects, expression steps, `until`, one
//! catch at the top. Each step is its own arm; calls consume one strict
//! argument snapshot per entry.

use super::{
    ApplyExpr, Arg, BindExpr, CatchExpr, Expr, ExprId, ExprKind, LambdaExpr, ModPath,
    Pattern, SelectExpr, StructurePattern,
};
use crate::{
    env::Env,
    expr::ErrorContext,
    stack::ensure_sufficient,
    typ::{TVar, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use indexmap::IndexMap;
use netidx_core::{path::Path, utils::Either};
use netidx_value::Value;
use poolshark::local::LPooled;
use triomphe::Arc;

type CarriedBinds = IndexMap<(ExprId, ArcStr), (ArcStr, SourcePosition)>;

#[derive(Clone, Copy)]
enum Rewrite<'a> {
    Bindings,
    Captures,
    Issue(&'a str),
}

impl Rewrite<'_> {
    fn deferred(self) -> Self {
        match self {
            Self::Issue(_) => Self::Bindings,
            mode => mode,
        }
    }
}

pub fn desugar(spec: &Expr, env: &Env, scope: &ModPath) -> Result<Expr> {
    match &spec.kind {
        ExprKind::Seq { queued: true, .. } => desugar_queued(spec, env, scope),
        _ => desugar_plain(spec, None),
    }
}

fn desugar_plain(spec: &Expr, abort_clock: Option<&str>) -> Result<Expr> {
    let ExprKind::Seq { trigger, body, .. } = &spec.kind else {
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

    let trigger_bind =
        trigger.as_ref().and_then(|t| simple_ref_name(t).map(|n| (n, t.pos)));
    let mut cells = CarriedBinds::new();
    for e in &steps {
        collect_step_binds(e, &mut cells)?;
    }

    let pc_typ = pc_type(n);
    let mut prelude: Vec<Expr> = Vec::new();
    prelude.push(let_bind(pos, pc.as_str(), Some(pc_typ), variant(pos, "Idle")));
    prelude.push(let_bind(pos, idle.as_str(), None, idle_of(pos, pc.as_str())));
    prelude.push(let_bind(pos, result.as_str(), None, never(pos)));
    if let Some((_, tpos)) = &trigger_bind {
        prelude.push(let_bind(*tpos, trig_cell.as_str(), None, never(*tpos)));
    }
    for (cell, cpos) in cells.values() {
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
    if trigger_bind.is_some() {
        start.push(connect(pos, trig_cell.as_str(), r#ref(pos, t_name.as_str())));
    }

    let mut visible: AHashMap<ArcStr, ArcStr> = AHashMap::new();
    if let Some((n, _)) = trigger_bind {
        visible.insert(n, ArcStr::from(trig_cell.as_str()));
    }
    let err_bind =
        catch.as_ref().map(|c| c.bind.clone()).unwrap_or_else(|| ArcStr::from("e"));
    let catch_node = {
        let reset = connect(pos, pc.as_str(), variant(pos, "Idle"));
        let mut handler_map = visible.clone();
        handler_map.remove(&err_bind);
        let user = catch.as_ref().map(|c| rewrite(&c.handler, &handler_map));
        let mut handler_body = Vec::new();
        if let Some(h) = user {
            handler_body.push(h);
        }
        let mut abort_body = vec![reset];
        if let Some(clock) = abort_clock {
            abort_body.push(connect(pos, clock, boolean(pos, true)));
        }
        handler_body.push(
            ExprKind::Rethrow(Arc::new(r#ref(pos, err_bind.as_str()))).to_expr(pos),
        );
        ExprKind::Catch(Arc::new(CatchExpr {
            bind: err_bind,
            constraint: catch.as_ref().and_then(|c| c.constraint.clone()),
            handler: Arc::new(block(pos, handler_body)),
            seq_abort: Some(Arc::new(block(pos, abort_body))),
        }))
        .to_expr(pos)
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
        let arm = step_arm(
            step,
            pc.as_str(),
            next.as_str(),
            last,
            result.as_str(),
            vname.as_str(),
            &visible,
            &cells,
        )?;
        arms.push((pat_variant(&tag), arm));
        expose_step_binds(step, &cells, &mut visible);
    }

    let machine = select(pos, r#ref(pos, pc.as_str()), arms);
    let mut body_exprs = prelude;
    body_exprs.push(catch_node);
    body_exprs.extend(start);
    body_exprs.push(machine);
    body_exprs.push(r#ref(pos, result.as_str()));
    Ok(block(pos, body_exprs))
}

fn desugar_queued(spec: &Expr, env: &Env, scope: &ModPath) -> Result<Expr> {
    let ExprKind::Seq { trigger, body, .. } = &spec.kind else { unreachable!() };
    let pos = spec.pos;
    let id = spec.id.inner();
    let request = format_compact!("seqqrequest{id}");
    let clock = format_compact!("seqqclock{id}");
    let activation = format_compact!("seqqactivation{id}");
    let input = format_compact!("seqqinput{id}");
    let result = format_compact!("seqqresult{id}");
    let trigger_name = trigger.as_ref().and_then(|t| simple_ref_name(t));
    let body =
        ExprKind::Seq { queued: false, trigger: None, body: body.clone() }.to_expr(pos);
    let captures = body.fold(IndexMap::new(), &mut |mut caps, e| {
        if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
            &e.kind
            && let Ok(Some((_, bind))) = env.lookup_bind(scope, name)
            && env.trait_methods.get(&bind.id).is_none()
        {
            let n = caps.len();
            caps.entry(
                simple_name(name).unwrap_or_else(|| ArcStr::from(name.0.as_ref())),
            )
            .or_insert_with(|| {
                let mut expr = e.clone();
                expr.kind = ExprKind::Ref { name: name.clone() };
                (expr, ArcStr::from(format_compact!("seqqcap{id}_{n}").as_str()), bind.id)
            });
        }
        caps
    });
    let mut names: AHashMap<_, _> =
        captures.iter().map(|(n, (_, c, _))| (n.clone(), c.clone())).collect();
    let written = rewrite(&body, &names).fold(AHashSet::new(), &mut |mut names, e| {
        if let ExprKind::Connect { name, deref: false, .. } = &e.kind
            && let Some(n) = simple_name(name)
        {
            names.insert(n);
        }
        names
    });
    let written: AHashSet<_> = captures
        .values()
        .filter_map(|(_, name, id)| written.contains(name).then_some(*id))
        .collect();
    names.retain(|name, _| !written.contains(&captures[name].2));
    let body = rewrite_with(&body, &names, Rewrite::Captures);
    let used = body.fold(AHashSet::new(), &mut |mut names, e| {
        if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
            &e.kind
            && let Some(n) = simple_name(name)
        {
            names.insert(n);
        }
        names
    });
    let captures: Vec<_> =
        captures.into_iter().filter(|(_, (_, c, _))| used.contains(c)).collect();
    let mut prelude = vec![
        let_bind(pos, clock.as_str(), None, never(pos)),
        let_bind(pos, activation.as_str(), None, boolean(pos, true)),
        let_bind(
            pos,
            request.as_str(),
            None,
            trigger.as_ref().map_or_else(|| boolean(pos, true), |t| (**t).clone()),
        ),
    ];
    let mut args = vec![r#ref(pos, request.as_str())];
    for (name, (expr, _, _)) in &captures {
        args.push(if trigger_name.as_ref() == Some(name) {
            r#ref(pos, request.as_str())
        } else {
            expr.clone()
        });
    }
    for (i, arg) in args.iter_mut().enumerate() {
        let name = format_compact!("seqqseed{id}_{i}");
        prelude.push(let_bind(
            pos,
            name.as_str(),
            None,
            ExprKind::Any {
                args: Arc::from_iter([
                    arg.clone(),
                    sample(pos, r#ref(pos, activation.as_str()), arg.clone()),
                ]),
            }
            .to_expr(pos),
        ));
        *arg = apply_core(
            pos,
            "hold",
            vec![
                (Some(ArcStr::from("clock")), r#ref(pos, name.as_str())),
                (None, r#ref(pos, name.as_str())),
            ],
        );
    }
    let payload = if captures.is_empty() {
        args.pop().unwrap()
    } else {
        ExprKind::Tuple { args: Arc::from(args) }.to_expr(pos)
    };
    prelude.push(let_bind(
        pos,
        input.as_str(),
        None,
        apply_core(
            pos,
            "queue",
            vec![
                (
                    Some(ArcStr::from("clock")),
                    ExprKind::Any {
                        args: Arc::from_iter([
                            r#ref(pos, activation.as_str()),
                            r#ref(pos, clock.as_str()),
                        ]),
                    }
                    .to_expr(pos),
                ),
                (None, sample(pos, r#ref(pos, request.as_str()), payload)),
            ],
        ),
    ));
    for (i, (_, (_, name, _))) in captures.iter().enumerate() {
        prelude.push(let_bind(
            pos,
            name.as_str(),
            None,
            ExprKind::TupleRef {
                source: Arc::new(r#ref(pos, input.as_str())),
                field: i + 1,
            }
            .to_expr(pos),
        ));
    }
    let ExprKind::Seq { body, .. } = &body.kind else { unreachable!() };
    let machine = ExprKind::Seq {
        queued: false,
        trigger: Some(Arc::new(r#ref(pos, input.as_str()))),
        body: body.clone(),
    }
    .to_expr(pos);
    prelude.push(let_bind(
        pos,
        result.as_str(),
        None,
        desugar_plain(&machine, Some(clock.as_str()))?,
    ));
    prelude.push(connect(
        pos,
        clock.as_str(),
        sample(pos, r#ref(pos, result.as_str()), boolean(pos, true)),
    ));
    prelude.push(r#ref(pos, result.as_str()));
    Ok(block(pos, prelude))
}

fn boolean(pos: SourcePosition, value: bool) -> Expr {
    ExprKind::Constant(Value::Bool(value)).to_expr(pos)
}

fn apply_core(
    pos: SourcePosition,
    name: &str,
    args: impl IntoIterator<Item = (Option<ArcStr>, Expr)>,
) -> Expr {
    ExprKind::Apply(ApplyExpr {
        function: Arc::new(
            ExprKind::Ref { name: ModPath::from(["core", name]) }.to_expr(pos),
        ),
        args: Arc::from_iter(args),
    })
    .to_expr(pos)
}

fn step_arm(
    step: &Expr,
    pc: &str,
    next: &str,
    last: bool,
    result: &str,
    vname: &str,
    visible: &AHashMap<ArcStr, ArcStr>,
    cells: &CarriedBinds,
) -> Result<Expr> {
    let pos = step.pos;
    let trans = connect(pos, pc, sample(pos, r#ref(pos, pc), variant(pos, next)));
    match &step.kind {
        ExprKind::Until(e) => {
            let e = guard(rewrite(e, visible));
            Ok(select(
                pos,
                e,
                vec![
                    (pat_lit(Value::Bool(true)), trans),
                    (pat_lit(Value::Bool(false)), never(pos)),
                ],
            ))
        }
        ExprKind::SeqDo { body } => {
            lower_do_stmts(body, pc, next, last, result, vname, visible, cells)
        }
        ExprKind::Bind(b) => {
            let value = issue_expr(&b.value, visible, pc);
            let mut body =
                vec![let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, vname))];
            b.pattern.with_names(&mut |n| {
                let (cell, _) = &cells[&(step.id, n.clone())];
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
            let value = issue_expr(value, visible, pc);
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
            let e = issue_expr(step, visible, pc);
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

fn collect_step_binds(e: &Expr, cells: &mut CarriedBinds) -> Result<()> {
    ensure_sufficient(|| match &e.kind {
        ExprKind::SeqDo { body } => {
            for s in body.iter() {
                collect_step_binds(s, cells)?;
            }
            Ok(())
        }
        ExprKind::Bind(b) => {
            if b.rec {
                return Err(
                    anyhow!("let rec is not a seq step").context(ErrorContext(e.clone()))
                );
            }
            b.pattern.with_names(&mut |n| {
                let cell =
                    ArcStr::from(format_compact!("seqc{}_{n}", e.id.inner()).as_str());
                cells.insert((e.id, n.clone()), (cell, e.pos));
            });
            Ok(())
        }
        _ => Ok(()),
    })
}

fn expose_step_binds(
    e: &Expr,
    cells: &CarriedBinds,
    visible: &mut AHashMap<ArcStr, ArcStr>,
) {
    ensure_sufficient(|| match &e.kind {
        ExprKind::SeqDo { body } => {
            for s in body.iter() {
                expose_step_binds(s, cells, visible);
            }
        }
        ExprKind::Bind(b) => {
            b.pattern.with_names(&mut |n| {
                let (cell, _) = &cells[&(e.id, n.clone())];
                visible.insert(n.clone(), cell.clone());
            });
        }
        _ => (),
    })
}

fn lower_do_stmts(
    stmts: &[Expr],
    pc: &str,
    next: &str,
    last_step: bool,
    result: &str,
    vname: &str,
    visible: &AHashMap<ArcStr, ArcStr>,
    cells: &CarriedBinds,
) -> Result<Expr> {
    let stmts = match stmts {
        [body @ .., Expr { kind: ExprKind::NoOp, .. }] => body,
        _ => stmts,
    };
    let trans = |pos: SourcePosition| {
        connect(pos, pc, sample(pos, r#ref(pos, pc), variant(pos, next)))
    };
    let Some((head, rest)) = stmts.split_first() else {
        panic!("empty do body");
    };
    let pos = head.pos;
    let rest_empty = rest.is_empty();
    let tail = |visible: &AHashMap<ArcStr, ArcStr>| -> Result<Expr> {
        if rest_empty {
            Ok(trans(pos))
        } else {
            lower_do_stmts(rest, pc, next, last_step, result, vname, visible, cells)
        }
    };
    match &head.kind {
        ExprKind::Until(_) => {
            Err(anyhow!("until is not a do statement")
                .context(ErrorContext(head.clone())))
        }
        ExprKind::Catch(_) => Err(anyhow!(
            "a seq catch must be the first statement of the seq, not inside do"
        )
        .context(ErrorContext(head.clone()))),
        ExprKind::SeqDo { body } => {
            let mut flat: Vec<Expr> = body.iter().cloned().collect();
            flat.extend(rest.iter().cloned());
            lower_do_stmts(&flat, pc, next, last_step, result, vname, visible, cells)
        }
        ExprKind::Bind(b) => {
            let value = issue_expr(&b.value, visible, pc);
            let mut vis = visible.clone();
            b.pattern.with_names(&mut |n| {
                vis.remove(n);
            });
            let mut body =
                vec![let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, vname))];
            b.pattern.with_names(&mut |n| {
                let (cell, _) = &cells[&(head.id, n.clone())];
                body.push(connect(
                    pos,
                    cell.as_str(),
                    sample(pos, r#ref(pos, pc), r#ref(pos, n.as_str())),
                ));
            });
            if rest_empty && last_step {
                body.push(connect(
                    pos,
                    result,
                    sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
                ));
            }
            body.push(tail(&vis)?);
            Ok(select(pos, value, vec![(pat_bind(vname), block(pos, body))]))
        }
        ExprKind::Connect { name, value, deref } => {
            let value = issue_expr(value, visible, pc);
            let target = rewrite_path(name, visible);
            let mut body = vec![connect_path(
                pos,
                target,
                *deref,
                sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
            )];
            if rest_empty && last_step {
                body.push(connect(
                    pos,
                    result,
                    sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
                ));
            }
            body.push(tail(visible)?);
            Ok(select(pos, value, vec![(pat_bind(vname), block(pos, body))]))
        }
        _ => {
            let e = issue_expr(head, visible, pc);
            let mut body = Vec::new();
            if rest_empty && last_step {
                body.push(connect(
                    pos,
                    result,
                    sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
                ));
            }
            body.push(tail(visible)?);
            Ok(select(pos, e, vec![(pat_bind(vname), block(pos, body))]))
        }
    }
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
    let name = simple_name(p);
    match map.get(name.as_deref().unwrap_or_else(|| p.0.as_ref())) {
        Some(cell) => ModPath::from([cell.as_str()]),
        None => p.clone(),
    }
}

fn rewrite(e: &Expr, map: &AHashMap<ArcStr, ArcStr>) -> Expr {
    rewrite_with(e, map, Rewrite::Bindings)
}

fn issue_expr(e: &Expr, map: &AHashMap<ArcStr, ArcStr>, pc: &str) -> Expr {
    guard(rewrite_with(e, map, Rewrite::Issue(pc)))
}

fn inline_lambda(mut e: &Expr) -> bool {
    while let ExprKind::ExplicitParens(inner) = &e.kind {
        e = inner;
    }
    matches!(e.kind, ExprKind::Lambda(_))
}

fn issue_call(spec: &Expr, mut call: ApplyExpr, pc: &str) -> Expr {
    if call.args.is_empty() {
        let mut expr = spec.clone();
        expr.kind = ExprKind::Apply(call);
        return expr;
    }
    let pos = spec.pos;
    let input = format_compact!("seqargs{}", spec.id.inner());
    let issued = format_compact!("seqissued{}", spec.id.inner());
    let mut args: LPooled<Vec<Expr>> = LPooled::take();
    args.reserve(call.args.len() + 1);
    args.push(r#ref(pos, pc));
    call.args = Arc::from_iter(call.args.iter().map(|(label, arg)| {
        let value = if inline_lambda(arg) {
            ExprKind::StrictSample {
                lhs: Arc::new(r#ref(arg.pos, pc)),
                rhs: Arc::new(arg.clone()),
            }
            .to_expr(arg.pos)
        } else {
            let field = args.len();
            args.push(arg.clone());
            ExprKind::TupleRef {
                source: Arc::new(r#ref(arg.pos, issued.as_str())),
                field,
            }
            .to_expr(arg.pos)
        };
        (label.clone(), value)
    }));
    let input_tuple = if args.len() == 1 {
        args.pop().unwrap()
    } else {
        ExprKind::Tuple { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
    };
    let mut expr = spec.clone();
    expr.id = ExprId::new();
    expr.kind = ExprKind::Apply(call);
    let ready = apply_core(pos, "once", [(None, r#ref(pos, input.as_str()))]);
    let snapshot = ExprKind::StrictSample {
        lhs: Arc::new(
            ExprKind::Any { args: Arc::from_iter([r#ref(pos, pc), ready]) }.to_expr(pos),
        ),
        rhs: Arc::new(r#ref(pos, input.as_str())),
    }
    .to_expr(pos);
    block(
        pos,
        vec![
            let_bind(pos, input.as_str(), None, input_tuple),
            select(pos, snapshot, vec![(pat_bind(issued.as_str()), expr)]),
        ],
    )
}

fn shadow_step(e: &Expr, map: &mut AHashMap<ArcStr, ArcStr>) {
    match &e.kind {
        ExprKind::Bind(b) => b.pattern.with_names(&mut |n| {
            map.remove(n);
        }),
        ExprKind::SeqDo { body } => body.iter().for_each(|e| shadow_step(e, map)),
        _ => (),
    }
}

fn rewrite_with(e: &Expr, map: &AHashMap<ArcStr, ArcStr>, mode: Rewrite<'_>) -> Expr {
    if map.is_empty() && !matches!(mode, Rewrite::Issue(_)) {
        return e.clone();
    }
    let captures = matches!(mode, Rewrite::Captures);
    let rewrite = |e: &Expr, map: &AHashMap<ArcStr, ArcStr>| rewrite_with(e, map, mode);
    let kind = match &e.kind {
        ExprKind::Until(_) | ExprKind::ByRef(_) if captures => return e.clone(),
        ExprKind::Ref { name } => ExprKind::Ref { name: rewrite_path(name, map) },
        ExprKind::Connect { name, value, deref } => ExprKind::Connect {
            name: if captures && !deref { name.clone() } else { rewrite_path(name, map) },
            value: Arc::new(rewrite(value, map)),
            deref: *deref,
        },
        ExprKind::Until(x) => {
            ExprKind::Until(Arc::new(rewrite_with(x, map, mode.deferred())))
        }
        ExprKind::SeqDo { body } => {
            let mut inner = map.clone();
            let mut out = Vec::with_capacity(body.len());
            for x in body.iter() {
                out.push(rewrite(x, &inner));
                shadow_step(x, &mut inner);
            }
            ExprKind::SeqDo { body: Arc::from(out) }
        }
        ExprKind::Seq { queued, trigger, body } => {
            let trigger = trigger.as_ref().map(|t| Arc::new(rewrite(t, map)));
            let mut inner = map.clone();
            let mut out = Vec::with_capacity(body.len());
            for x in body.iter() {
                out.push(rewrite_with(x, &inner, mode.deferred()));
                shadow_step(x, &mut inner);
            }
            ExprKind::Seq { queued: *queued, trigger, body: Arc::from(out) }
        }
        ExprKind::ExplicitParens(x) => {
            ExprKind::ExplicitParens(Arc::new(rewrite(x, map)))
        }
        ExprKind::Qop(x) => ExprKind::Qop(Arc::new(rewrite(x, map))),
        ExprKind::Rethrow(x) => ExprKind::Rethrow(Arc::new(rewrite(x, map))),
        ExprKind::SeqGuard(x) => ExprKind::SeqGuard(Arc::new(rewrite(x, map))),
        ExprKind::OrNever(x) => ExprKind::OrNever(Arc::new(rewrite(x, map))),
        ExprKind::ByRef(x) => {
            ExprKind::ByRef(Arc::new(rewrite_with(x, map, mode.deferred())))
        }
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
            value: if b.rec {
                let mut inner = map.clone();
                shadow_step(e, &mut inner);
                rewrite(&b.value, &inner)
            } else {
                rewrite(&b.value, map)
            },
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
        ExprKind::Apply(a) => {
            let call = ApplyExpr {
                function: Arc::new(rewrite(&a.function, map)),
                args: Arc::from_iter(
                    a.args.iter().map(|(n, v)| (n.clone(), rewrite(v, map))),
                ),
            };
            if let Rewrite::Issue(pc) = mode {
                return issue_call(e, call, pc);
            }
            ExprKind::Apply(call)
        }
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
                handler: Arc::new(rewrite_with(&c.handler, &inner, mode.deferred())),
                seq_abort: c
                    .seq_abort
                    .as_ref()
                    .map(|e| Arc::new(rewrite_with(e, &inner, mode.deferred()))),
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
                        Some(Some(d)) => {
                            Some(Some(rewrite_with(d, map, mode.deferred())))
                        }
                        other => other.clone(),
                    },
                    pattern: a.pattern.clone(),
                    constraint: a.constraint.clone(),
                    pos: a.pos,
                })
                .collect();
            let body = match &l.body {
                Either::Left(b) => Either::Left(rewrite_with(b, &inner, mode.deferred())),
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

fn guard(e: Expr) -> Expr {
    let pos = e.pos;
    ExprKind::SeqGuard(Arc::new(e)).to_expr(pos)
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
