//! AST-to-AST lowering of `seq` (`design/seq_blocks.md`).
//!
//! Lets, connects, expression steps, `{ … }` blocks, `until`, and
//! `try … with`. `catch` is refused in a seq body. The machine installs
//! one handler that resets and rethrows; each try-body arm carries a
//! generated handler that jumps to the with body. Statements share an
//! arm until one reads what an earlier one wrote (`split_arms`); a call
//! consumes one argument snapshot per entry.

use super::{
    ApplyExpr, Arg, BindExpr, CatchExpr, Expr, ExprId, ExprKind, LambdaExpr, ModPath,
    Pattern, SelectExpr, SeqTrigger, StructurePattern, TryWithExpr, WrittenAt,
};
use crate::{
    env::Env,
    expr::{At, Name, OriginScope},
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
use smallvec::SmallVec;
use triomphe::Arc;

/// A carried cell per let name (and per try's `e`): its generated name,
/// the declaring position, and the let's annotation when the pattern is a
/// plain name.
type CarriedBinds = IndexMap<(ExprId, ArcStr), (ArcStr, SourcePosition, Option<Type>)>;

#[derive(Clone, Copy)]
enum Rewrite<'a> {
    Bindings,
    /// A `seqq` body's reads become the request's captures. An `until`
    /// stays live, except for the trigger's name (the payload): a run
    /// waits on its own request.
    Captures(Option<&'a str>),
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

/// The seq `spec` lowered to its machine, every expression of which
/// comes from `spec`'s source.
pub fn desugar(spec: &Expr, env: &Env, scope: &ModPath) -> Result<Expr> {
    let _ori = OriginScope::enter(spec.ori.clone());
    match &spec.kind {
        ExprKind::Seq { trigger: Some(SeqTrigger::Bind(b)), .. } => desugar_let(spec, b),
        ExprKind::Seq { queued: true, .. } => desugar_queued(spec, env, scope),
        ExprKind::Seq { flush: Some(f), .. } => Err(anyhow!(
            "flush empties a queue, and a seq has none: write seqq, or abort(..)"
        )
        .at(&(**f))),
        _ => desugar_plain(spec, None),
    }
}

/// The variables a `seqq` hands its machine: the credit clock an abort
/// returns a credit on, and the variable a flush is written to.
struct Queue<'a> {
    clock: &'a str,
    flush: &'a str,
}

/// `seq let pat = e { body }` is `{ let pat = e; seq name { body } }`:
/// the trigger is a level named for the body, bound outside the machine
/// so the run costs no step. A pattern that is not one name gets the
/// level a name of its own and is destructured beside it.
fn desugar_let(spec: &Expr, b: &BindExpr) -> Result<Expr> {
    let ExprKind::Seq { queued, abort, flush, body, .. } = &spec.kind else {
        panic!("desugar_let on a non-seq");
    };
    let pos = spec.pos;
    if b.rec {
        return Err(anyhow!("a seq trigger cannot be rec: it has no self to recurse on")
            .at(&spec));
    }
    let BindExpr { pattern, typ, value, .. } = b;
    let name = match pattern {
        StructurePattern::Bind(n) => n.name.clone(),
        _ => ArcStr::from(format_compact!("seqbind{}", spec.id.inner()).as_str()),
    };
    let mut exprs: SmallVec<[Expr; 3]> = SmallVec::new();
    exprs.push(let_bind(pos, &name, typ.clone(), value.clone()));
    if !matches!(pattern, StructurePattern::Bind(_)) {
        exprs.push(
            ExprKind::Bind(Arc::new(BindExpr {
                rec: false,
                pattern: pattern.clone(),
                typ: None,
                value: r#ref(pos, &name),
            }))
            .to_expr(pos),
        );
    }
    exprs.push(
        ExprKind::Seq {
            queued: *queued,
            trigger: Some(SeqTrigger::Expr(Arc::new(r#ref(pos, &name)))),
            abort: abort.clone(),
            flush: flush.clone(),
            body: body.clone(),
        }
        .to_expr(pos),
    );
    Ok(block(pos, exprs))
}

fn desugar_plain(spec: &Expr, queue: Option<&Queue>) -> Result<Expr> {
    let ExprKind::Seq { trigger, abort, flush, body, .. } = &spec.kind else {
        panic!("desugar_seq on a non-seq");
    };
    let pos = spec.pos;
    let id = spec.id.inner();
    if body.is_empty() {
        return Err(anyhow!("a seq block must contain at least one step").at(&spec));
    }
    let mut steps: SmallVec<[&Expr; 8]> = SmallVec::new();
    for e in body.iter() {
        refuse_catch(e)?;
        if !matches!(e.kind, ExprKind::NoOp) {
            steps.push(e);
        }
    }
    if steps.is_empty() {
        return Err(anyhow!("a seq block must contain at least one step").at(&spec));
    }
    let pc = format_compact!("seqpc{id}");
    let idle = format_compact!("seqidle{id}");
    let result = format_compact!("seqr{id}");
    let trig_cell = format_compact!("seqt{id}");

    let trigger_bind = trigger
        .as_ref()
        .and_then(|t| simple_ref_name(t.expr()).map(|n| (n, t.expr().pos)));
    let mut cells = CarriedBinds::new();
    for e in &steps {
        collect_step_binds(e, &mut cells)?;
    }

    let trig_expr = match trigger {
        Some(t) => t.expr().clone(),
        None => ExprKind::Constant(Value::Bool(true)).to_expr(pos),
    };
    let t_name = format_compact!("seqgo{id}");
    let filter = apply_filter(pos, trig_expr, lambda_sampling(pos, idle.as_str()));

    let mut visible: LPooled<AHashMap<ArcStr, ArcStr>> = LPooled::take();
    if let Some((n, _)) = &trigger_bind {
        visible.insert(n.clone(), ArcStr::from(trig_cell.as_str()));
    }
    let aborted = format_compact!("seqab{id}");
    let err_bind = ArcStr::from("e");
    let catch_node = {
        let reset = connect(pos, pc.as_str(), variant(pos, "Idle"));
        let mut abort_body: SmallVec<[Expr; 2]> = SmallVec::new();
        abort_body.push(reset);
        if let Some(q) = queue {
            abort_body.push(connect(pos, q.clock, boolean(pos, true)));
        }
        ExprKind::Catch(Arc::new(CatchExpr {
            bind: err_bind.clone().into(),
            constraint: None,
            handler: Arc::new(
                ExprKind::Rethrow(Arc::new(r#ref(pos, err_bind.as_str()))).to_expr(pos),
            ),
            seq_abort: Some(Arc::new(block(pos, abort_body))),
            seq_capture: None,
            seq_manual: (abort.is_some() || flush.is_some())
                .then(|| Arc::new(r#ref(pos, aborted.as_str()))),
            seq_pc: Some(ArcStr::from(pc.as_str())),
        }))
        .to_expr(pos)
    };

    let vname = format_compact!("seqv{id}");
    let mut machine = Machine {
        pc: pc.as_str(),
        result: result.as_str(),
        vname: vname.as_str(),
        cells: &cells,
        labels: LPooled::take(),
        arms: LPooled::take(),
    };
    let entry = machine.fresh();
    let mut sink = Sink::new();
    sink.push(Write::Result);
    machine.lower_stmts(&steps, entry.clone(), ArcStr::from("Idle"), &sink, &visible)?;
    let Machine { labels, arms: mut body_arms, .. } = machine;

    let mut prelude: LPooled<Vec<Expr>> = LPooled::take();
    prelude.push(let_bind(
        pos,
        pc.as_str(),
        Some(pc_type(&labels)),
        variant(pos, "Idle"),
    ));
    prelude.push(let_bind(pos, idle.as_str(), None, idle_of(pos, pc.as_str())));
    prelude.push(let_bind(pos, result.as_str(), None, never(pos)));
    if let Some((_, tpos)) = &trigger_bind {
        prelude.push(let_bind(*tpos, trig_cell.as_str(), None, never(*tpos)));
    }
    for (cell, cpos, typ) in cells.values() {
        prelude.push(let_bind(*cpos, cell.as_str(), typ.clone(), never(*cpos)));
    }
    if abort.is_some() || flush.is_some() {
        let edge = format_compact!("seqedge{id}");
        let armed = format_compact!("seqarmed{id}");
        let flushed = format_compact!("seqfl{id}");
        prelude.push(let_bind(
            pos,
            edge.as_str(),
            None,
            apply_core(pos, "uniq", [(None, r#ref(pos, idle.as_str()))]),
        ));
        prelude.push(let_bind(pos, armed.as_str(), None, boolean(pos, false)));
        prelude.push(connect(
            pos,
            armed.as_str(),
            ExprKind::Not { expr: Arc::new(r#ref(pos, edge.as_str())) }.to_expr(pos),
        ));
        let mut events: SmallVec<[Expr; 2]> = SmallVec::new();
        if let Some(e) = abort {
            events.push(run_event(&rewrite(e, &visible), edge.as_str(), armed.as_str()));
        }
        if let (Some(e), Some(q)) = (flush, queue) {
            let event = run_event(&rewrite(e, &visible), edge.as_str(), armed.as_str());
            prelude.push(let_bind(e.pos, flushed.as_str(), None, event));
            prelude.push(connect(e.pos, q.flush, r#ref(e.pos, flushed.as_str())));
            events.push(r#ref(e.pos, flushed.as_str()));
        }
        let event = match events.len() {
            1 => events.pop().unwrap(),
            _ => ExprKind::Any { args: Arc::from_iter(events) }.to_expr(pos),
        };
        prelude.push(let_bind(pos, aborted.as_str(), None, event));
    }
    let mut start: SmallVec<[Expr; 3]> = SmallVec::new();
    start.push(let_bind(pos, t_name.as_str(), None, filter));
    start.push(connect(
        pos,
        pc.as_str(),
        sample(pos, r#ref(pos, t_name.as_str()), variant(pos, entry.as_str())),
    ));
    if trigger_bind.is_some() {
        start.push(connect(pos, trig_cell.as_str(), r#ref(pos, t_name.as_str())));
    }

    let arms = std::iter::once((pat_variant("Idle"), never(pos)))
        .chain(body_arms.drain(..).map(|(label, arm)| (pat_variant(&label), arm)));
    let select_pc = select(pos, r#ref(pos, pc.as_str()), arms);
    let mut body_exprs = prelude;
    body_exprs.push(catch_node);
    if abort.is_some() || flush.is_some() {
        body_exprs.push(
            ExprKind::SeqAbort(Arc::new(r#ref(pos, aborted.as_str()))).to_expr(pos),
        );
    }
    body_exprs.extend(start);
    body_exprs.push(select_pc);
    body_exprs.push(r#ref(pos, result.as_str()));
    Ok(block(pos, body_exprs.drain(..)))
}

/// An `abort(..)` or `flush(..)` event: `e` is an initial step, woken
/// when a run starts and asleep between runs, and only its fires after
/// the entry cycle count. What stands at entry, and the fires it missed
/// asleep, belong to no run.
fn run_event(e: &Expr, edge: &str, armed: &str) -> Expr {
    let pos = e.pos;
    let live = select(
        pos,
        r#ref(pos, edge),
        [
            (pat_lit(Value::Bool(true)), never(pos)),
            (pat_lit(Value::Bool(false)), e.clone()),
        ],
    );
    apply_filter(pos, live, lambda_sampling(pos, armed))
}

/// `catch` is refused anywhere in a seq body: an install cannot produce
/// the value the next step waits for. A lambda literal is its own dynamic
/// scope, so its body and defaults are exempt.
fn refuse_catch(e: &Expr) -> Result<()> {
    match find_outside_lambdas(e, |x| matches!(x.kind, ExprKind::Catch(_))) {
        None => Ok(()),
        Some(c) => Err(anyhow!(
            "catch is not allowed inside a seq: `?` aborts the run, \
             `try {{ .. }} with(e) {{ .. }} handles it, and a catch around the \
             seq sees the abort"
        )
        .at(&c)),
    }
}

/// `Expr::fold` over every node that is not inside a lambda literal's
/// body or defaults: the lambda itself is visited, its children are not.
fn fold_outside_lambdas<T>(e: &Expr, init: T, f: &mut impl FnMut(T, &Expr) -> T) -> T {
    crate::stack::ensure_sufficient(|| {
        let mut acc = Some(f(init, e));
        if !matches!(e.kind, ExprKind::Lambda(_)) {
            e.for_each_child(&mut |c| {
                let v = acc.take().unwrap();
                acc = Some(fold_outside_lambdas(c, v, f));
            });
        }
        acc.unwrap()
    })
}

/// The first node satisfying `pred` that is not inside a lambda literal's
/// body or defaults.
fn find_outside_lambdas(e: &Expr, pred: impl Fn(&Expr) -> bool) -> Option<Expr> {
    fold_outside_lambdas(e, None, &mut |found: Option<Expr>, x| {
        found.or_else(|| pred(x).then(|| x.clone()))
    })
}

/// What a statement touches, by a variable's last path segment: the
/// names it reads (taking `&a` counts), the names it writes, the names
/// it takes a reference to, the names it binds with `let`, whether it
/// holds something the analysis cannot see through (a call to a closure
/// over some variable, a read through a reference, a nested seq), and
/// whether it writes through a reference, whose target is unknown.
struct Access {
    reads: LPooled<AHashSet<ArcStr>>,
    writes: LPooled<AHashSet<ArcStr>>,
    refs: LPooled<AHashSet<ArcStr>>,
    binds: LPooled<AHashSet<ArcStr>>,
    opaque: bool,
    deref_write: bool,
}

/// The variable a place expression is rooted at, if it names one.
fn place_root(mut e: &Expr) -> Option<&ModPath> {
    loop {
        match &e.kind {
            ExprKind::Ref { name } => return Some(name),
            ExprKind::StructRef { source, .. }
            | ExprKind::TupleRef { source, .. }
            | ExprKind::ArrayRef { source, .. }
            | ExprKind::ArraySlice { source, .. }
            | ExprKind::MapRef { source, .. } => e = source,
            _ => return None,
        }
    }
}

fn access(e: &Expr) -> Access {
    let base = |p: &ModPath| Path::basename(&p.0).map(ArcStr::from);
    let mut init = Access {
        reads: LPooled::take(),
        writes: LPooled::take(),
        refs: LPooled::take(),
        binds: LPooled::take(),
        opaque: false,
        deref_write: false,
    };
    if let ExprKind::Bind(b) = &e.kind {
        b.pattern.with_names(&mut |n| {
            init.binds.insert(n.clone());
        });
    }
    fold_outside_lambdas(e, init, &mut |mut a, x| {
        match &x.kind {
            ExprKind::Ref { name } => {
                a.reads.extend(base(name));
            }
            ExprKind::ByRef(place) => {
                a.refs.extend(place_root(place).and_then(base));
            }
            ExprKind::Connect { name, deref: false, .. } => {
                a.writes.extend(base(name));
            }
            ExprKind::Connect { deref: true, .. } => a.deref_write = true,
            ExprKind::Apply(_) | ExprKind::Deref(_) | ExprKind::Seq { .. } => {
                a.opaque = true
            }
            _ => (),
        }
        a
    })
}

fn is_try(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::TryWith(_) => true,
        ExprKind::Bind(b) => matches!(b.value.kind, ExprKind::TryWith(_)),
        ExprKind::Connect { value, .. } => matches!(value.kind, ExprKind::TryWith(_)),
        _ => false,
    }
}

/// The end index of each arm of a statement list. `until` and `try`
/// are arms of their own. Other statements share an arm until one
/// reads a variable an earlier statement of the arm wrote, writes it
/// again, or is opaque while such a write is pending: the next arm is
/// the next cycle, when the write has landed. A write through a
/// reference ends its arm. Within an arm a `let` is read through a
/// local binding, so a statement that writes or takes a reference to
/// a name the arm bound starts the next arm, where the name is its
/// carried cell. An arm lowers to nested selects, so a run is cut at
/// the parser's nesting limit.
fn split_arms(stmts: &[&Expr]) -> LPooled<Vec<usize>> {
    let mut ends: LPooled<Vec<usize>> = LPooled::take();
    let mut pending: LPooled<AHashSet<ArcStr>> = LPooled::take();
    let mut bound: LPooled<AHashSet<ArcStr>> = LPooled::take();
    let limit = super::parser::max_nesting();
    let mut start = 0;
    for (i, s) in stmts.iter().enumerate() {
        if matches!(s.kind, ExprKind::Until(_)) || is_try(s) {
            if i > start {
                ends.push(i);
            }
            ends.push(i + 1);
            start = i + 1;
            pending.clear();
            bound.clear();
            continue;
        }
        let a = access(s);
        let conflict = !pending.is_empty()
            && (a.opaque
                || a.reads.iter().chain(a.writes.iter()).any(|n| pending.contains(n)));
        let rebinds = a.writes.iter().chain(a.refs.iter()).any(|n| bound.contains(n));
        if i > start && (conflict || rebinds || i - start >= limit) {
            ends.push(i);
            start = i;
            pending.clear();
            bound.clear();
        }
        pending.extend(a.writes.iter().cloned());
        bound.extend(a.binds.iter().cloned());
        if a.deref_write {
            ends.push(i + 1);
            start = i + 1;
            pending.clear();
            bound.clear();
        }
    }
    if start < stmts.len() {
        ends.push(stmts.len());
    }
    ends
}

/// What the tail of a statement list does with its value, in order:
/// bind a `let`'s pattern and write its carried cells, write a
/// connect's target, publish the block's result.
#[derive(Clone)]
enum Write {
    Let { pattern: StructurePattern, typ: Option<Type>, id: ExprId },
    Connect { name: ModPath, deref: bool },
    Result,
}

type Sink = SmallVec<[Write; 2]>;

/// The arms under construction. Labels `S{k}` are allocated as statements
/// are lowered, not contiguously per statement; the pc type is the set of
/// every label allocated.
struct Machine<'a> {
    pc: &'a str,
    result: &'a str,
    vname: &'a str,
    cells: &'a CarriedBinds,
    labels: LPooled<Vec<ArcStr>>,
    arms: LPooled<Vec<(ArcStr, Expr)>>,
}

impl Machine<'_> {
    fn fresh(&mut self) -> ArcStr {
        let l = ArcStr::from(format_compact!("S{}", self.labels.len()).as_str());
        self.labels.push(l.clone());
        l
    }

    /// Lower `stmts` as consecutive arms (`split_arms`): the first at
    /// `entry`, the last transitioning to `next` after the sink's
    /// writes. A `let`'s names become visible to the arms after it.
    fn lower_stmts(
        &mut self,
        stmts: &[&Expr],
        entry: ArcStr,
        next: ArcStr,
        sink: &Sink,
        visible: &AHashMap<ArcStr, ArcStr>,
    ) -> Result<()> {
        let ends = split_arms(stmts);
        let n = ends.len();
        let mut entries: SmallVec<[ArcStr; 8]> = SmallVec::new();
        entries.push(entry);
        for _ in 1..n {
            let l = self.fresh();
            entries.push(l);
        }
        let mut vis = scope(visible);
        let mut start = 0;
        for (i, &end) in ends.iter().enumerate() {
            let group = &stmts[start..end];
            let next_i = if i + 1 < n { entries[i + 1].clone() } else { next.clone() };
            let none = Sink::new();
            let sink_i = if i + 1 == n { sink } else { &none };
            self.lower_arm(group, entries[i].clone(), next_i, sink_i, &vis)?;
            for stmt in group {
                expose_step_binds(stmt, self.cells, &mut vis);
            }
            start = end;
        }
        Ok(())
    }

    fn lower_arm(
        &mut self,
        group: &[&Expr],
        entry: ArcStr,
        next: ArcStr,
        sink: &Sink,
        visible: &AHashMap<ArcStr, ArcStr>,
    ) -> Result<()> {
        let solo = match group {
            [stmt] => Some(*stmt),
            _ => None,
        };
        ensure_sufficient(|| match solo.map(|s| &s.kind) {
            Some(ExprKind::TryWith(t)) => {
                self.lower_try(solo.unwrap(), t, entry, next, sink, visible)
            }
            Some(ExprKind::Bind(b)) if matches!(b.value.kind, ExprKind::TryWith(_)) => {
                let ExprKind::TryWith(t) = &b.value.kind else { unreachable!() };
                let mut sink = sink.clone();
                sink.insert(
                    0,
                    Write::Let {
                        pattern: b.pattern.clone(),
                        typ: b.typ.clone(),
                        id: solo.unwrap().id,
                    },
                );
                self.lower_try(&b.value, t, entry, next, &sink, visible)
            }
            Some(ExprKind::Connect { name, value, deref })
                if matches!(value.kind, ExprKind::TryWith(_)) =>
            {
                let ExprKind::TryWith(t) = &value.kind else { unreachable!() };
                let mut sink = sink.clone();
                sink.insert(0, Write::Connect { name: name.clone(), deref: *deref });
                self.lower_try(value, t, entry, next, &sink, visible)
            }
            Some(ExprKind::Until(e)) => {
                let arm =
                    until_arm(solo.unwrap(), e, self.pc, next.as_str(), sink, visible)?;
                self.arms.push((entry, arm));
                Ok(())
            }
            _ => {
                let arm = lower_group(
                    group,
                    self.pc,
                    next.as_str(),
                    sink,
                    self.result,
                    self.vname,
                    visible,
                    self.cells,
                )?;
                self.arms.push((entry, arm));
                Ok(())
            }
        })
    }

    /// Each try-body arm carries a generated handler that captures the
    /// first error into the with body's cell (`seq_capture`) and whose
    /// drain action jumps to the with body's entry. Both tails write the
    /// sink and transition to `next`.
    fn lower_try(
        &mut self,
        spec: &Expr,
        t: &TryWithExpr,
        entry: ArcStr,
        next: ArcStr,
        sink: &Sink,
        visible: &AHashMap<ArcStr, ArcStr>,
    ) -> Result<()> {
        let pos = spec.pos;
        let body: SmallVec<[&Expr; 8]> =
            t.body.iter().filter(|e| !matches!(e.kind, ExprKind::NoOp)).collect();
        let handler: SmallVec<[&Expr; 8]> =
            t.handler.iter().filter(|e| !matches!(e.kind, ExprKind::NoOp)).collect();
        let with_entry = self.fresh();
        let e_cell = self.cells[&(spec.id, t.bind.clone())].0.clone();
        let mark = self.arms.len();
        self.lower_stmts(&body, entry, next.clone(), sink, visible)?;
        let caught = ArcStr::from(format_compact!("seqtry{}", spec.id.inner()).as_str());
        for (_, arm) in self.arms[mark..].iter_mut() {
            let jump = ExprKind::Catch(Arc::new(CatchExpr {
                bind: caught.clone().into(),
                constraint: t.constraint.clone(),
                handler: Arc::new(never(pos)),
                seq_abort: Some(Arc::new(connect(
                    pos,
                    self.pc,
                    variant(pos, with_entry.as_str()),
                ))),
                seq_capture: Some(e_cell.clone()),
                seq_manual: None,
                seq_pc: None,
            }))
            .to_expr(pos);
            let inner = std::mem::replace(arm, never(pos));
            *arm = block(pos, [jump, inner]);
        }
        let mut wvis = scope(visible);
        wvis.insert(t.bind.clone(), e_cell);
        self.lower_stmts(&handler, with_entry, next, sink, &wvis)
    }
}

/// The tail writes of a statement list, sampled on the entry event
/// (`pc`) so they land with the transition.
fn sink_writes(
    sink: &Sink,
    pos: SourcePosition,
    pc: &str,
    result: &str,
    vname: &str,
    cells: &CarriedBinds,
    visible: &AHashMap<ArcStr, ArcStr>,
) -> LPooled<Vec<Expr>> {
    let mut out: LPooled<Vec<Expr>> = LPooled::take();
    for w in sink.iter() {
        match w {
            Write::Let { pattern, typ, id } => {
                out.push(let_pat(pos, pattern.clone(), typ.clone(), r#ref(pos, vname)));
                pattern.with_names(&mut |n| {
                    let (cell, _, _) = &cells[&(*id, n.clone())];
                    out.push(connect(
                        pos,
                        cell.as_str(),
                        sample(pos, r#ref(pos, pc), r#ref(pos, n.as_str())),
                    ));
                });
            }
            Write::Connect { name, deref } => out.push(connect_path(
                pos,
                rewrite_path(name, visible),
                *deref,
                sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
            )),
            Write::Result => out.push(connect(
                pos,
                result,
                sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
            )),
        }
    }
    out
}

fn desugar_queued(spec: &Expr, env: &Env, scope: &ModPath) -> Result<Expr> {
    let ExprKind::Seq { trigger, abort, flush, body, .. } = &spec.kind else {
        unreachable!()
    };
    let pos = spec.pos;
    let id = spec.id.inner();
    let flushed = format_compact!("seqqflush{id}");
    let dequeued = format_compact!("seqqrun{id}");
    let request = format_compact!("seqqrequest{id}");
    let clock = format_compact!("seqqclock{id}");
    let activation = format_compact!("seqqactivation{id}");
    let input = format_compact!("seqqinput{id}");
    let result = format_compact!("seqqresult{id}");
    let trigger_name = trigger.as_ref().and_then(|t| simple_ref_name(t.expr()));
    let body = ExprKind::Seq {
        queued: false,
        trigger: None,
        abort: None,
        flush: None,
        body: body.clone(),
    }
    .to_expr(pos);
    let mut captures =
        body.fold(LPooled::<IndexMap<_, _>>::take(), &mut |mut caps, e| {
            if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
                &e.kind
                && let Ok(Some((_, bind))) = env.lookup_bind(scope, name)
                && env.trait_methods.get(&bind.id).is_none()
                && !bind.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
            {
                let n = caps.len();
                caps.entry(
                    simple_name(name).unwrap_or_else(|| ArcStr::from(name.0.as_ref())),
                )
                .or_insert_with(|| {
                    let mut expr = e.clone();
                    expr.kind = ExprKind::Ref { name: name.clone() };
                    (
                        expr,
                        ArcStr::from(format_compact!("seqqcap{id}_{n}").as_str()),
                        bind.id,
                    )
                });
            }
            caps
        });
    let mut names: LPooled<AHashMap<_, _>> =
        captures.iter().map(|(n, (_, c, _))| (n.clone(), c.clone())).collect();
    let written = rewrite(&body, &names).fold(
        LPooled::<AHashSet<_>>::take(),
        &mut |mut names, e| {
            if let ExprKind::Connect { name, deref: false, .. } = &e.kind
                && let Some(n) = simple_name(name)
            {
                names.insert(n);
            }
            names
        },
    );
    let written: LPooled<AHashSet<_>> = captures
        .values()
        .filter_map(|(_, name, id)| written.contains(name).then_some(*id))
        .collect();
    names.retain(|name, _| !written.contains(&captures[name].2));
    let body = rewrite_with(&body, &names, Rewrite::Captures(trigger_name.as_deref()));
    let used = body.fold(LPooled::<AHashSet<_>>::take(), &mut |mut names, e| {
        if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
            &e.kind
            && let Some(n) = simple_name(name)
        {
            names.insert(n);
        }
        names
    });
    let captures: LPooled<Vec<_>> =
        captures.drain(..).filter(|(_, (_, c, _))| used.contains(c)).collect();
    let mut prelude: LPooled<Vec<Expr>> = LPooled::take();
    prelude.extend([
        let_bind(pos, clock.as_str(), None, never(pos)),
        let_bind(pos, activation.as_str(), None, boolean(pos, true)),
        let_bind(
            pos,
            request.as_str(),
            None,
            trigger.as_ref().map_or_else(|| boolean(pos, true), |t| t.expr().clone()),
        ),
    ]);
    if flush.is_some() {
        prelude.push(let_bind(pos, flushed.as_str(), None, never(pos)));
    }
    let mut args: LPooled<Vec<Expr>> = LPooled::take();
    args.push(r#ref(pos, request.as_str()));
    for (name, (expr, _, _)) in captures.iter() {
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
            [
                (Some(ArcStr::from("clock")), r#ref(pos, name.as_str())),
                (None, r#ref(pos, name.as_str())),
            ],
        );
    }
    let payload = if captures.is_empty() {
        args.pop().unwrap()
    } else {
        ExprKind::Tuple { args: Arc::from_iter(args.drain(..)) }.to_expr(pos)
    };
    let mut queue_args: SmallVec<[(Option<ArcStr>, Expr); 3]> = SmallVec::new();
    queue_args.push((
        Some(ArcStr::from("clock")),
        ExprKind::Any {
            args: Arc::from_iter([
                r#ref(pos, activation.as_str()),
                r#ref(pos, clock.as_str()),
            ]),
        }
        .to_expr(pos),
    ));
    if flush.is_some() {
        queue_args.push((Some(ArcStr::from("flush")), r#ref(pos, flushed.as_str())));
    }
    queue_args.push((None, sample(pos, r#ref(pos, request.as_str()), payload)));
    prelude.push(let_bind(
        pos,
        input.as_str(),
        None,
        apply_core(pos, "queue", queue_args),
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
    // an abort or flush event reads everything live but the trigger's
    // name, which is the request this run dequeued
    let run_names: AHashMap<ArcStr, ArcStr> = trigger_name
        .iter()
        .map(|n| (n.clone(), ArcStr::from(dequeued.as_str())))
        .collect();
    if trigger_name.is_some() && (abort.is_some() || flush.is_some()) {
        let input = r#ref(pos, input.as_str());
        let request = if captures.is_empty() {
            input
        } else {
            ExprKind::TupleRef { source: Arc::new(input), field: 0 }.to_expr(pos)
        };
        prelude.push(let_bind(pos, dequeued.as_str(), None, request));
    }
    let machine = ExprKind::Seq {
        queued: false,
        trigger: Some(SeqTrigger::Expr(Arc::new(r#ref(pos, input.as_str())))),
        abort: abort.as_ref().map(|e| Arc::new(rewrite(e, &run_names))),
        flush: flush.as_ref().map(|e| Arc::new(rewrite(e, &run_names))),
        body: body.clone(),
    }
    .to_expr(pos);
    let queue = Queue { clock: clock.as_str(), flush: flushed.as_str() };
    prelude.push(let_bind(
        pos,
        result.as_str(),
        None,
        desugar_plain(&machine, Some(&queue))?,
    ));
    prelude.push(connect(
        pos,
        clock.as_str(),
        sample(pos, r#ref(pos, result.as_str()), boolean(pos, true)),
    ));
    prelude.push(r#ref(pos, result.as_str()));
    Ok(block(pos, prelude.drain(..)))
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

fn until_arm(
    step: &Expr,
    cond: &Expr,
    pc: &str,
    next: &str,
    sink: &Sink,
    visible: &AHashMap<ArcStr, ArcStr>,
) -> Result<Expr> {
    let pos = step.pos;
    if !sink.is_empty() {
        return Err(anyhow!(
            "until has no value: the last statement of a seq, or of a try \
             or with body whose value is used, must be an expression"
        )
        .at(&step));
    }
    let trans = connect(pos, pc, sample(pos, r#ref(pos, pc), variant(pos, next)));
    let e = guard(entry_fire(rewrite(cond, visible), pc));
    Ok(select(
        pos,
        e,
        [(pat_lit(Value::Bool(true)), trans), (pat_lit(Value::Bool(false)), never(pos))],
    ))
}

fn collect_step_binds(e: &Expr, cells: &mut CarriedBinds) -> Result<()> {
    ensure_sufficient(|| match &e.kind {
        ExprKind::TryWith(t) => {
            let cell = ArcStr::from(format_compact!("seqe{}", e.id.inner()).as_str());
            cells.insert((e.id, t.bind.clone()), (cell, e.pos, None));
            for s in t.body.iter().chain(t.handler.iter()) {
                collect_step_binds(s, cells)?;
            }
            Ok(())
        }
        ExprKind::Bind(b) => {
            if b.rec {
                return Err(anyhow!("let rec is not a seq step").at(&e));
            }
            let annotated = matches!(b.pattern, StructurePattern::Bind(_));
            b.pattern.with_names(&mut |n| {
                let cell =
                    ArcStr::from(format_compact!("seqc{}_{n}", e.id.inner()).as_str());
                let typ = if annotated { b.typ.clone() } else { None };
                cells.insert((e.id, n.clone()), (cell, e.pos, typ));
            });
            if matches!(b.value.kind, ExprKind::TryWith(_)) {
                collect_step_binds(&b.value, cells)?;
            }
            Ok(())
        }
        ExprKind::Connect { value, .. } if matches!(value.kind, ExprKind::TryWith(_)) => {
            collect_step_binds(value, cells)
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
        ExprKind::Bind(b) => {
            b.pattern.with_names(&mut |n| {
                let (cell, _, _) = &cells[&(e.id, n.clone())];
                visible.insert(n.clone(), cell.clone());
            });
        }
        _ => (),
    })
}

fn lower_group(
    stmts: &[&Expr],
    pc: &str,
    next: &str,
    sink: &Sink,
    result: &str,
    vname: &str,
    visible: &AHashMap<ArcStr, ArcStr>,
    cells: &CarriedBinds,
) -> Result<Expr> {
    ensure_sufficient(|| {
        lower_group_inner(stmts, pc, next, sink, result, vname, visible, cells)
    })
}

/// One arm. Each statement's completion arm holds the statements after
/// it, so a statement is issued in the cycle the one before it produced
/// in; the last one writes the sink and the transition.
fn lower_group_inner(
    stmts: &[&Expr],
    pc: &str,
    next: &str,
    sink: &Sink,
    result: &str,
    vname: &str,
    visible: &AHashMap<ArcStr, ArcStr>,
    cells: &CarriedBinds,
) -> Result<Expr> {
    let trans = |pos: SourcePosition| {
        connect(pos, pc, sample(pos, r#ref(pos, pc), variant(pos, next)))
    };
    let Some((head, rest)) = stmts.split_first() else {
        panic!("empty arm");
    };
    let head = *head;
    if matches!(head.kind, ExprKind::Until(_)) || is_try(head) {
        unreachable!("until and try are arms of their own");
    }
    let pos = head.pos;
    let rest_empty = rest.is_empty();
    let tail = |visible: &AHashMap<ArcStr, ArcStr>| -> Result<Expr> {
        if rest_empty {
            Ok(trans(pos))
        } else {
            lower_group(rest, pc, next, sink, result, vname, visible, cells)
        }
    };
    let writes = |visible: &AHashMap<ArcStr, ArcStr>| -> LPooled<Vec<Expr>> {
        if rest_empty {
            sink_writes(sink, pos, pc, result, vname, cells, visible)
        } else {
            LPooled::take()
        }
    };
    match &head.kind {
        ExprKind::Bind(b) => {
            let value = stmt_value(&b.value, visible, pc)?;
            let mut vis = scope(visible);
            b.pattern.with_names(&mut |n| {
                vis.remove(n);
            });
            let mut body: LPooled<Vec<Expr>> = LPooled::take();
            body.push(let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, vname)));
            b.pattern.with_names(&mut |n| {
                let (cell, _, _) = &cells[&(head.id, n.clone())];
                body.push(connect(
                    pos,
                    cell.as_str(),
                    sample(pos, r#ref(pos, pc), r#ref(pos, n.as_str())),
                ));
            });
            body.extend(writes(&vis).drain(..));
            body.push(tail(&vis)?);
            Ok(select(pos, value, [(pat_bind(vname), block(pos, body.drain(..)))]))
        }
        ExprKind::Connect { name, value, deref } => {
            let value = stmt_value(value, visible, pc)?;
            let target = rewrite_path(name, visible);
            let mut body: LPooled<Vec<Expr>> = LPooled::take();
            body.push(connect_path(
                pos,
                target,
                *deref,
                sample(pos, r#ref(pos, pc), r#ref(pos, vname)),
            ));
            body.extend(writes(visible).drain(..));
            body.push(tail(visible)?);
            Ok(select(pos, value, [(pat_bind(vname), block(pos, body.drain(..)))]))
        }
        _ => {
            let e = stmt_value(head, visible, pc)?;
            let mut body = writes(visible);
            body.push(tail(visible)?);
            Ok(select(pos, e, [(pat_bind(vname), block(pos, body.drain(..)))]))
        }
    }
}

/// A statement's scrutinee. A block in statement position, or as a
/// let's or a connect's right-hand side, is lowered as a block; anything
/// else is issued.
fn stmt_value(e: &Expr, visible: &AHashMap<ArcStr, ArcStr>, pc: &str) -> Result<Expr> {
    match &e.kind {
        ExprKind::Do { exprs } => lower_block(e, exprs, pc, visible),
        _ => Ok(issue_expr(e, visible, pc)),
    }
}

/// A `{ … }` statement: every statement issued at entry, lets local to
/// the block, connects clocked to the entry, and the value the last
/// statement's once every statement has produced.
fn lower_block(
    spec: &Expr,
    exprs: &[Expr],
    pc: &str,
    visible: &AHashMap<ArcStr, ArcStr>,
) -> Result<Expr> {
    let pos = spec.pos;
    let stmts = match exprs {
        [body @ .., Expr { kind: ExprKind::NoOp, .. }] => body,
        _ => exprs,
    };
    let mut vis = scope(visible);
    let mut body: LPooled<Vec<Expr>> = LPooled::take();
    let mut vals: LPooled<Vec<Expr>> = LPooled::take();
    for (i, s) in stmts.iter().enumerate() {
        if is_try(s) {
            return Err(
                anyhow!("try is a seq statement; write it at the seq level").at(&s)
            );
        }
        let v = format_compact!("seqb{}_{i}", spec.id.inner());
        match &s.kind {
            ExprKind::NoOp => continue,
            ExprKind::Bind(b) => {
                body.push(let_bind(s.pos, &v, None, stmt_value(&b.value, &vis, pc)?));
                body.push(let_pat(
                    s.pos,
                    b.pattern.clone(),
                    b.typ.clone(),
                    r#ref(s.pos, &v),
                ));
                b.pattern.with_names(&mut |n| {
                    vis.remove(n);
                });
            }
            ExprKind::Connect { name, value, deref } => {
                body.push(let_bind(s.pos, &v, None, stmt_value(value, &vis, pc)?));
                body.push(connect_path(
                    s.pos,
                    rewrite_path(name, &vis),
                    *deref,
                    sample(s.pos, r#ref(s.pos, pc), r#ref(s.pos, &v)),
                ));
            }
            _ => body.push(let_bind(s.pos, &v, None, stmt_value(s, &vis, pc)?)),
        }
        vals.push(r#ref(s.pos, &v));
    }
    let value = match vals.len() {
        0 => never(pos),
        1 => vals.pop().unwrap(),
        n => {
            let last = format_compact!("seqb{}", spec.id.inner());
            select(
                pos,
                ExprKind::Tuple { args: Arc::from_iter(vals.drain(..)) }.to_expr(pos),
                [(pat_last(n, &last), r#ref(pos, &last))],
            )
        }
    };
    body.push(value);
    Ok(block(pos, body.drain(..)))
}

fn pc_type(labels: &[ArcStr]) -> Type {
    let idle = ArcStr::from("Idle");
    Type::Set(Arc::from_iter(
        [&idle]
            .into_iter()
            .chain(labels)
            .map(|l| Type::Variant(l.clone(), Arc::from_iter([]), WrittenAt::NOWHERE)),
    ))
}

fn idle_of(pos: SourcePosition, pc: &str) -> Expr {
    select(
        pos,
        r#ref(pos, pc),
        [
            (pat_variant("Idle"), ExprKind::Constant(Value::Bool(true)).to_expr(pos)),
            (pat_wild(), ExprKind::Constant(Value::Bool(false)).to_expr(pos)),
        ],
    )
}

fn lambda_sampling(pos: SourcePosition, level: &str) -> Expr {
    let x = ArcStr::from("x");
    ExprKind::Lambda(Arc::new(LambdaExpr {
        args: Arc::from_iter([Arg {
            labeled: None,
            pattern: StructurePattern::Bind(x.clone().into()),
            constraint: None,
            pos,
        }]),
        vargs: None,
        rtype: None,
        constraints: Arc::from(Vec::<(TVar, Type)>::new()),
        throws: None,
        body: Either::Left(sample(pos, r#ref(pos, "x"), r#ref(pos, level))),
    }))
    .to_expr(pos)
}

fn apply_filter(pos: SourcePosition, trig: Expr, pred: Expr) -> Expr {
    apply_core(pos, "filter", [(None, trig), (None, pred)])
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

/// A copy of `names` for a scope to shadow in.
fn scope(names: &AHashMap<ArcStr, ArcStr>) -> LPooled<AHashMap<ArcStr, ArcStr>> {
    let mut inner: LPooled<AHashMap<ArcStr, ArcStr>> = LPooled::take();
    inner.extend(names.iter().map(|(k, v)| (k.clone(), v.clone())));
    inner
}

/// A step's scrutinee. A step completes on a fired production after entry;
/// a call is re-issued at entry and answers fired, while a level read as
/// it stands is fired at entry here.
fn issue_expr(e: &Expr, map: &AHashMap<ArcStr, ArcStr>, pc: &str) -> Expr {
    let issued = has_call(e);
    let e = rewrite_with(e, map, Rewrite::Issue(pc));
    guard(if issued { e } else { entry_fire(e, pc) })
}

/// `e` fired at entry with its standing value, then tracked: `any(pc
/// ~! e, e)`. A compound `e` is bound once so its nodes are not
/// duplicated (a `?` inside it must raise once).
fn entry_fire(e: Expr, pc: &str) -> Expr {
    let pos = e.pos;
    let at_entry = |v: Expr| {
        ExprKind::StrictSample { lhs: Arc::new(r#ref(pos, pc)), rhs: Arc::new(v) }
            .to_expr(pos)
    };
    let any =
        |a: Expr, b: Expr| ExprKind::Any { args: Arc::from_iter([a, b]) }.to_expr(pos);
    match &e.kind {
        ExprKind::Ref { .. } | ExprKind::Constant(_) => any(at_entry(e.clone()), e),
        _ => {
            let v = format_compact!("seqv{}", e.id.inner());
            block(
                pos,
                [
                    let_bind(pos, &v, None, e),
                    any(at_entry(r#ref(pos, &v)), r#ref(pos, &v)),
                ],
            )
        }
    }
}

/// Whether a step produces later rather than standing as a level: it
/// holds a call, or a nested seq, whose result cell stands from its
/// previous run.
fn has_call(e: &Expr) -> bool {
    find_outside_lambdas(e, |x| {
        matches!(x.kind, ExprKind::Apply(_) | ExprKind::Seq { .. })
    })
    .is_some()
}

fn inline_lambda(mut e: &Expr) -> bool {
    while let ExprKind::ExplicitParens(inner) = &e.kind {
        e = inner;
    }
    matches!(e.kind, ExprKind::Lambda(_))
}

/// One call issued per entry over a snapshot of the arguments taken on
/// the entry event; only the call's own fired production is this
/// invocation's answer. A nullary call is a level read at entry.
fn issue_call(spec: &Expr, mut call: ApplyExpr, pc: &str) -> Expr {
    if call.args.is_empty() {
        let mut expr = spec.clone();
        expr.kind = ExprKind::Apply(call);
        return entry_fire(expr, pc);
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
        [
            let_bind(pos, input.as_str(), None, input_tuple),
            select(pos, snapshot, [(pat_bind(issued.as_str()), guard(expr))]),
        ],
    )
}

fn shadow_step(e: &Expr, map: &mut AHashMap<ArcStr, ArcStr>) {
    match &e.kind {
        ExprKind::Bind(b) => b.pattern.with_names(&mut |n| {
            map.remove(n);
        }),
        _ => (),
    }
}

fn rewrite_with(e: &Expr, map: &AHashMap<ArcStr, ArcStr>, mode: Rewrite<'_>) -> Expr {
    ensure_sufficient(|| rewrite_with_inner(e, map, mode))
}

// XCR codex for eric: [CR18, P2] done: a scope is a pooled copy
// (`scope`), a rewritten sequence goes straight into `Arc::from_iter`,
// and the seqq capture analysis is pooled too.
fn rewrite_with_inner(
    e: &Expr,
    map: &AHashMap<ArcStr, ArcStr>,
    mode: Rewrite<'_>,
) -> Expr {
    if map.is_empty() && !matches!(mode, Rewrite::Issue(_)) {
        return e.clone();
    }
    let captures = matches!(mode, Rewrite::Captures(_));
    let rewrite = |e: &Expr, map: &AHashMap<ArcStr, ArcStr>| rewrite_with(e, map, mode);
    let kind = match &e.kind {
        ExprKind::Until(x) if captures => {
            let Rewrite::Captures(Some(trigger)) = mode else { return e.clone() };
            let Some((name, capture)) = map.get_key_value(trigger) else {
                return e.clone();
            };
            let mut request: LPooled<AHashMap<ArcStr, ArcStr>> = LPooled::take();
            request.insert(name.clone(), capture.clone());
            ExprKind::Until(Arc::new(rewrite(x, &request)))
        }
        ExprKind::ByRef(_) if captures => return e.clone(),
        ExprKind::Ref { name } => ExprKind::Ref { name: rewrite_path(name, map) },
        ExprKind::Connect { name, value, deref } => ExprKind::Connect {
            name: if captures && !deref { name.clone() } else { rewrite_path(name, map) },
            value: Arc::new(rewrite(value, map)),
            deref: *deref,
        },
        ExprKind::Until(x) => {
            ExprKind::Until(Arc::new(rewrite_with(x, map, mode.deferred())))
        }
        ExprKind::TryWith(t) => {
            let stmts = |stmts: &[Expr], mut inner: LPooled<AHashMap<ArcStr, ArcStr>>| {
                Arc::from_iter(stmts.iter().map(|x| {
                    let r = rewrite(x, &inner);
                    shadow_step(x, &mut inner);
                    r
                }))
            };
            let mut with_map = scope(map);
            with_map.remove(&t.bind);
            ExprKind::TryWith(Arc::new(TryWithExpr {
                body: stmts(&t.body, scope(map)),
                bind: t.bind.clone(),
                constraint: t.constraint.clone(),
                handler: stmts(&t.handler, with_map),
            }))
        }
        ExprKind::Seq { queued, trigger, abort, flush, body } => {
            let trigger = trigger.as_ref().map(|t| t.map(|e| rewrite(e, map)));
            let mut inner = scope(map);
            if let Some(SeqTrigger::Bind(b)) = &trigger {
                b.pattern.with_names(&mut |n| {
                    inner.remove(n);
                });
            }
            let abort = abort.as_ref().map(|e| Arc::new(rewrite(e, &inner)));
            let flush = flush.as_ref().map(|e| Arc::new(rewrite(e, &inner)));
            let body = Arc::from_iter(body.iter().map(|x| {
                let r = rewrite_with(x, &inner, mode.deferred());
                shadow_step(x, &mut inner);
                r
            }));
            ExprKind::Seq { queued: *queued, trigger, abort, flush, body }
        }
        ExprKind::Qop(x) => {
            let x = rewrite(x, map);
            match mode {
                // A `?` over a level reads it as it stands at entry, so a
                // carried error raises at every entry.
                Rewrite::Issue(pc) if !has_call(&x) => ExprKind::Qop(Arc::new(
                    ExprKind::StrictSample {
                        lhs: Arc::new(r#ref(x.pos, pc)),
                        rhs: Arc::new(x),
                    }
                    .to_expr(e.pos),
                )),
                _ => ExprKind::Qop(Arc::new(x)),
            }
        }
        ExprKind::ByRef(x) => {
            ExprKind::ByRef(Arc::new(rewrite_with(x, map, mode.deferred())))
        }
        ExprKind::Do { exprs } => {
            let mut inner = scope(map);
            ExprKind::Do {
                exprs: Arc::from_iter(exprs.iter().map(|x| {
                    let r = rewrite(x, &inner);
                    shadow_step(x, &mut inner);
                    r
                })),
            }
        }
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
                let mut inner = scope(map);
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
            let mut inner = scope(map);
            inner.remove(c.bind.as_str());
            ExprKind::Catch(Arc::new(CatchExpr {
                bind: c.bind.clone(),
                constraint: c.constraint.clone(),
                handler: Arc::new(rewrite_with(&c.handler, &inner, mode.deferred())),
                seq_abort: c
                    .seq_abort
                    .as_ref()
                    .map(|e| Arc::new(rewrite_with(e, &inner, mode.deferred()))),
                seq_capture: c.seq_capture.clone(),
                seq_manual: c
                    .seq_manual
                    .as_ref()
                    .map(|e| Arc::new(rewrite_with(e, map, mode.deferred()))),
                seq_pc: c.seq_pc.clone(),
            }))
        }
        ExprKind::Lambda(l) => {
            let mut inner = scope(map);
            for a in l.args.iter() {
                a.pattern.with_names(&mut |n| {
                    inner.remove(n);
                });
            }
            let args = Arc::from_iter(l.args.iter().map(|a| Arg {
                labeled: match &a.labeled {
                    Some(Some(d)) => Some(Some(rewrite_with(d, map, mode.deferred()))),
                    other => other.clone(),
                },
                pattern: a.pattern.clone(),
                constraint: a.constraint.clone(),
                pos: a.pos,
            }));
            let body = match &l.body {
                Either::Left(b) => Either::Left(rewrite_with(b, &inner, mode.deferred())),
                Either::Right(s) => Either::Right(s.clone()),
            };
            ExprKind::Lambda(Arc::new(LambdaExpr {
                args,
                vargs: l.vargs.clone(),
                rtype: l.rtype.clone(),
                constraints: l.constraints.clone(),
                throws: l.throws.clone(),
                body,
            }))
        }
        _ => return e.map_children(&mut |c| rewrite(c, map)),
    };
    Expr {
        id: super::ExprId::new(),
        ori: e.ori.clone(),
        pos: e.pos,
        kind,
        dec: e.dec.clone(),
        str_form: e.str_form,
        end: e.end,
    }
}

fn let_bind(pos: SourcePosition, name: &str, typ: Option<Type>, value: Expr) -> Expr {
    ExprKind::Bind(Arc::new(BindExpr {
        rec: false,
        pattern: StructurePattern::Bind(Name::from(name)),
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

fn select(
    pos: SourcePosition,
    arg: Expr,
    arms: impl IntoIterator<Item = (Pattern, Expr)>,
) -> Expr {
    ExprKind::Select(SelectExpr { arg: Arc::new(arg), arms: Arc::from_iter(arms) })
        .to_expr(pos)
}

fn block(pos: SourcePosition, exprs: impl IntoIterator<Item = Expr>) -> Expr {
    let mut exprs: LPooled<Vec<Expr>> = exprs.into_iter().collect();
    match exprs.len() {
        0 => never(pos),
        1 => exprs.pop().unwrap(),
        _ => ExprKind::Do { exprs: Arc::from_iter(exprs.drain(..)) }.to_expr(pos),
    }
}

fn pat_bind(name: &str) -> Pattern {
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Bind(Name::from(name)),
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

/// `(_, …, name)`: the last of `n` tuple elements.
fn pat_last(n: usize, name: &str) -> Pattern {
    let binds = (1..n)
        .map(|_| StructurePattern::Ignore)
        .chain([StructurePattern::Bind(Name::from(name))]);
    Pattern {
        type_predicate: None,
        structure_predicate: StructurePattern::Tuple {
            all: None,
            binds: Arc::from_iter(binds),
        },
        guard: None,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::parser::{max_nesting, parse_one};

    fn ones(n: usize) -> Vec<Expr> {
        (0..n).map(|_| ExprKind::Constant(Value::I64(1)).to_expr_nopos()).collect()
    }

    fn lower_ones(n: usize) -> Result<Expr> {
        let stmts = ones(n);
        let stmts: Vec<&Expr> = stmts.iter().collect();
        let mut sink = Sink::new();
        sink.push(Write::Result);
        lower_group(
            &stmts,
            "pc",
            "Idle",
            &sink,
            "r",
            "v",
            &AHashMap::new(),
            &CarriedBinds::new(),
        )
    }

    /// Every expression of a lowered machine comes from the seq's
    /// source, whatever the thread built last.
    #[test]
    fn a_machine_has_the_seq_origin() {
        use crate::expr::{Origin, Source, parser::parse};
        let ori = |name: &str| Origin {
            parent: None,
            source: Source::Internal(ArcStr::from(name)),
            text: ArcStr::from("seq { a <- 1; b <- a; let c = f(b); c }"),
        };
        let seq = parse(ori("seq")).expect("parses");
        let _elsewhere = OriginScope::enter(Arc::new(ori("elsewhere")));
        let lowered = desugar(&seq[0], &Env::default(), &ModPath::root()).unwrap();
        let foreign = lowered.fold(0, &mut |n, e| {
            n + (e.ori.source != Source::Internal(ArcStr::from("seq"))) as usize
        });
        assert_eq!(foreign, 0);
    }

    fn arms_of(body: &str) -> Vec<usize> {
        let e = parse_one(&format!("seq {{ {body} }}")).expect("parses");
        let ExprKind::Seq { body, .. } = &e.kind else { panic!("not a seq") };
        let stmts: Vec<&Expr> =
            body.iter().filter(|e| !matches!(e.kind, ExprKind::NoOp)).collect();
        split_arms(&stmts).to_vec()
    }

    #[test]
    fn arms_split_at_a_read_after_write() {
        for (body, ends) in [
            ("a <- x; b <- y; let s = a + b", vec![2, 3]),
            ("a <- x; b <- a", vec![1, 2]),
            ("n <- n + 1; n <- n + 1", vec![1, 2]),
            ("a <- 1; a <- 2; let s = a", vec![1, 2, 3]),
            ("a <- x; f(y)", vec![1, 2]),
            ("f(x); g(y)", vec![2]),
            ("a <- f(x); b <- g(y)", vec![1, 2]),
            ("a <- x; let r = &a; b <- *r", vec![1, 3]),
            ("*r <- 1; b <- 2", vec![1, 2]),
            ("a <- x; until a > 1; b <- 2", vec![1, 2, 3]),
            ("let a = f(); let b = g(a); c <- b; d <- c", vec![3, 4]),
            ("a <- x; let f = |v| v + a; b <- y", vec![3]),
            ("m::a <- x; let s = a", vec![1, 2]),
            ("let x = try { 1 } with(e) { 2 }; a <- x; b <- y", vec![1, 3]),
            ("{ a <- x; b <- y }; let s = a", vec![1, 2]),
            ("let flag = false; flag <- true; until flag", vec![1, 2, 3]),
            ("let x = 1; let a = &x; y <- 2", vec![1, 3]),
            ("let x = 1; f(&x)", vec![1, 2]),
            ("let x = 1; let y = x + 1; z <- y", vec![3]),
        ] {
            assert_eq!(arms_of(body), ends, "{body}");
        }
    }

    #[test]
    fn a_run_over_the_limit_is_cut() {
        let n = max_nesting();
        let stmts = ones(n + 1);
        let stmts: Vec<&Expr> = stmts.iter().collect();
        assert_eq!(split_arms(&stmts).to_vec(), [n, n + 1]);
    }

    #[test]
    fn a_run_at_the_limit_does_not_overflow() {
        let n = max_nesting();
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(move || lower_ones(n).expect("lowers"))
            .expect("spawn")
            .join()
            .expect("arm lowering overflowed the stack");
    }
}
