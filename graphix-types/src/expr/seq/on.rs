//! AST-to-AST lowering of `on e { .. }` (`design/on_blocks.md`): a block
//! clocked to one event, with no machine. Every binding outside the block
//! that its text reads is taken as it stands when the event fires (`k ~!
//! v`), and what reads no variable is fired by the event, so a constant
//! acts once an event. Values latch as they arrive and are released on the
//! clock's pulse.

use super::*;

/// The prefix of a block's clock, `let #onk<id> = e`.
const CLOCK: &str = "#onk";

/// The prefix of a block's sample of a binding it reads, `let
/// #ons<id>_<n>_<v> = #onk<id> ~! v`.
pub(super) const SAMPLE: &str = "#ons";

/// The prefix of a block's read through a reference it samples, `let
/// #ond<id>_<n>_<r> = #onk<id> ~! *#ons<id>_<n>_<r>`.
pub(super) const TARGET: &str = "#ond";

fn refuse(e: &Expr) -> Result<()> {
    let dynamic = |x: &Expr| {
        matches!(&x.kind, ExprKind::Module { value, .. }
            if matches!(**value, ModuleKind::Dynamic { .. }))
    };
    if let Some(m) = find_outside_lambdas(e, dynamic) {
        return Err(anyhow!(
            "a dynamic module is not allowed inside an on block: load it outside the \
             block"
        )
        .at(&m));
    }
    match find_outside_lambdas(e, |x| matches!(x.kind, ExprKind::Catch(_))) {
        None => Ok(()),
        Some(c) => Err(anyhow!(
            "catch is not allowed inside an on block: its handler runs when an error \
             is raised, not at the event. Write it around the block, where a `?` in \
             the block raises to it"
        )
        .at(&c)),
    }
}

/// `v` at the fire of `clock`, and nothing where it has no value.
fn at(clock: &str, v: Expr) -> Expr {
    let pos = v.pos;
    ExprKind::BinOp {
        op: BinOp::StrictSample,
        lhs: Arc::new(r#ref(pos, clock)),
        rhs: Arc::new(v),
    }
    .to_expr(pos)
}

/// Whether anything in `e` fires by itself: a read of a variable, live or
/// not, a nested seq or on block. A call's function is where the call
/// resolves it, and a lambda literal is a value.
fn fires(e: &Expr) -> bool {
    ensure_sufficient(|| match &e.kind {
        ExprKind::Ref { .. } | ExprKind::Seq(_) | ExprKind::Live(_) => true,
        ExprKind::Lambda(_) => false,
        _ => {
            let callee = callee_ref(e);
            let mut found = false;
            e.for_each_child(&mut |c| {
                found = found || (!callee.is_some_and(|k| std::ptr::eq(k, c)) && fires(c))
            });
            found
        }
    })
}

/// A value the event has nothing to fire: a function, or no value.
fn standing(e: &Expr) -> bool {
    inline_lambda(e) || matches!(e.kind, ExprKind::Never { .. })
}

/// `e` fired by `clock` where nothing in it fires.
fn root(e: Expr, clock: &str) -> Expr {
    match fires(&e) || standing(&e) {
        true => e,
        false => at(clock, e),
    }
}

/// `e` with each read through a reference the block samples as that
/// read's own cell: the target's value at the event wherever the read
/// stands, in an arm that wakes between two events and in a lambda the
/// block writes, as a variable's is.
fn through(e: &Expr, targets: &AHashMap<ArcStr, ArcStr>) -> Expr {
    ensure_sufficient(|| {
        let target = match &e.kind {
            ExprKind::Deref(r) => simple_ref_name(r).and_then(|r| targets.get(&r)),
            _ => None,
        };
        match target {
            Some(cell) => renewed(e, ExprKind::Ref { name: ModPath::from([&**cell]) }),
            None => e.map_children(&mut |c| through(c, targets)),
        }
    })
}

/// `e` with what reads no variable fired by `clock`: the arguments of a
/// call none of which fires, so the call is issued at the event, and the
/// value of a connect, a `let`, a select's scrutinee and a block that
/// nothing fires. A read through a reference the block made follows its
/// target, so it is taken at the event. A lambda literal's body, a nested
/// seq and what a `live` reads are their own.
fn clocked(e: &Expr, clock: &str) -> Expr {
    ensure_sufficient(|| match &e.kind {
        ExprKind::Lambda(_)
        | ExprKind::Seq(_)
        | ExprKind::Module { .. }
        | ExprKind::Live(_) => e.clone(),
        ExprKind::Deref(x) => {
            let read = renewed(e, ExprKind::Deref(Arc::new(clocked(x, clock))));
            at(clock, read)
        }
        ExprKind::Apply(a) => {
            let function = match callee_ref(e) {
                Some(f) => f.clone(),
                None => clocked(&a.function, clock),
            };
            let mut args: LPooled<Vec<(Option<ArcStr>, Expr)>> = a
                .args
                .iter()
                .map(|(label, arg)| (label.clone(), clocked(arg, clock)))
                .collect();
            if !args.iter().any(|(_, arg)| fires(arg)) {
                for (_, arg) in args.iter_mut() {
                    if !matches!(arg.kind, ExprKind::Never { .. }) {
                        *arg = at(clock, arg.clone())
                    }
                }
            }
            let call = ApplyExpr {
                function: Arc::new(function),
                args: Arc::from_iter(args.drain(..)),
            };
            renewed(e, ExprKind::Apply(call))
        }
        ExprKind::Bind(b) => {
            let value = root(clocked(&b.value, clock), clock);
            let bind = BindExpr { value, ..(**b).clone() };
            renewed(e, ExprKind::Bind(Arc::new(bind)))
        }
        ExprKind::Connect { name, at: written, value, deref } => {
            let value = Arc::new(root(clocked(value, clock), clock));
            let kind = ExprKind::Connect {
                name: name.clone(),
                at: *written,
                value,
                deref: *deref,
            };
            renewed(e, kind)
        }
        ExprKind::Select(s) => {
            let arms = s.arms.iter().map(|(p, body)| {
                let mut p = p.clone();
                p.guard = p.guard.as_ref().map(|g| clocked(g, clock));
                (p, clocked(body, clock))
            });
            let select = SelectExpr {
                arg: Arc::new(root(clocked(&s.arg, clock), clock)),
                arms: Arc::from_iter(arms),
            };
            renewed(e, ExprKind::Select(select))
        }
        ExprKind::Block { exprs } => {
            let exprs = clocked_stmts(exprs, clock);
            renewed(e, ExprKind::Block { exprs })
        }
        _ => e.map_children(&mut |c| clocked(c, clock)),
    })
}

/// The statements of one block, the last of which is its value.
fn clocked_stmts(stmts: &[Expr], clock: &str) -> Arc<[Expr]> {
    let last = stmts.len().saturating_sub(1);
    Arc::from_iter(stmts.iter().enumerate().map(|(i, x)| {
        let x = clocked(x, clock);
        let value = !matches!(
            x.kind,
            ExprKind::Bind(_)
                | ExprKind::Connect { .. }
                | ExprKind::NoOp
                | ExprKind::Use { .. }
                | ExprKind::TypeDef(_)
        );
        match i == last && value {
            true => root(x, clock),
            false => x,
        }
    }))
}

/// Every name `e` reads as a variable or writes through, by its key.
fn names_read(e: &Expr, out: &mut AHashSet<ArcStr>) {
    e.fold((), &mut |(), x| {
        if let ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. } =
            &x.kind
        {
            out.insert(ArcStr::from(ref_key(name)));
        }
    })
}

pub(super) fn desugar(seq: &Parts, env: &Env, scope: &ModPath) -> Result<Expr> {
    let pos = seq.spec.pos;
    let Some(trigger) = seq.trigger else { bail!("BUG: an on block with no event") };
    if let SeqTrigger::Bind(b) = trigger {
        refuse_rec(seq.spec, b)?
    }
    seq.body.iter().try_for_each(refuse)?;
    let id = seq.spec.id.inner();
    let clock = ArcStr::from(format_compact!("{CLOCK}{id}").as_str());
    // what the block makes itself, the event it is clocked by, and the
    // functions it names are no samples
    let mut own: LPooled<AHashSet<ArcStr>> = fn_names(seq.body, env, scope);
    match trigger {
        SeqTrigger::Bind(b) => b.pattern.with_names(&mut |n| {
            own.insert(n.clone());
        }),
        SeqTrigger::Expr(e) => own.extend(simple_ref_name(e)),
    }
    let mut vars: LPooled<IndexMap<ArcStr, Expr>> = LPooled::take();
    for stmt in seq.body.iter() {
        captured(stmt, false, &mut |r| {
            let (ExprKind::Ref { name } | ExprKind::Connect { name, deref: true, .. }) =
                &r.kind
            else {
                return;
            };
            let key = ref_key(name);
            let outer = !own.contains(key)
                && !vars.contains_key(key)
                && matches!(env.lookup_bind(scope, name),
                    Ok(Some((_, bind))) if !is_function(env, bind));
            if outer {
                let mut read = r.clone();
                read.kind = ExprKind::Ref { name: name.clone() };
                vars.insert(ArcStr::from(key), read);
            }
        })
    }
    let mut map: LPooled<Names> = LPooled::take();
    let mut samples: LPooled<Vec<(ArcStr, Expr)>> = LPooled::take();
    let mut targets: LPooled<AHashMap<ArcStr, ArcStr>> = LPooled::take();
    for (i, (key, read)) in vars.drain(..).enumerate() {
        let cell = written_cell(&format_compact!("{SAMPLE}{id}_{i}"), &key);
        let target = written_cell(&format_compact!("{TARGET}{id}_{i}"), &key);
        targets.insert(cell.clone(), target);
        map.insert(key, Redirect::Entry(cell.clone()));
        samples.push((cell, read))
    }
    let body = rewrite_stmts(seq.body, &map, Rewrite::Bindings);
    let body: LPooled<Vec<Expr>> = body.iter().map(|x| through(x, &targets)).collect();
    let body = clocked_stmts(&body, &clock);
    let body: LPooled<Vec<Expr>> = body.iter().map(strip_live).collect();
    // a name every read of which the block shadows takes no sample
    let mut used: LPooled<AHashSet<ArcStr>> = LPooled::take();
    body.iter().for_each(|x| names_read(x, &mut used));
    let mut out: LPooled<Vec<Expr>> = LPooled::take();
    out.push(let_bind(pos, &clock, None, trigger.expr().clone()));
    if let SeqTrigger::Bind(b) = trigger {
        out.push(let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, &clock)))
    }
    for (cell, read) in samples.drain(..) {
        let target = &targets[&cell];
        let through = used.contains(target);
        if used.contains(&cell) || through {
            let pos = read.pos;
            out.push(let_bind(pos, &cell, None, at(&clock, read)));
            if through {
                let read = ExprKind::Deref(Arc::new(r#ref(pos, &cell))).to_expr(pos);
                out.push(let_bind(pos, target, None, at(&clock, read)))
            }
        }
    }
    out.extend(body.iter().cloned());
    Ok(block(pos, out.drain(..)))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{PrintFlag, expr::parser::parse_one};

    fn lowered(src: &str) -> String {
        let e = parse_one(src).unwrap_or_else(|e| panic!("{src}: {e:#}"));
        let l = desugar(
            &Parts {
                spec: &e,
                trigger: match &e.kind {
                    ExprKind::Seq(s) => s.trigger.as_ref(),
                    _ => panic!("{src} is no on block"),
                },
                abort: None,
                flush: None,
                body: match &e.kind {
                    ExprKind::Seq(s) => &s.body,
                    _ => unreachable!(),
                },
            },
            &Env::default(),
            &ModPath::root(),
        )
        .unwrap_or_else(|e| panic!("{src}: {e:#}"));
        // the clock without the block's id
        let clock = format_compact!("{CLOCK}{}", e.id.inner());
        crate::format_with_flags(PrintFlag::Lowered, || l.to_string())
            .replace(clock.as_str(), CLOCK)
    }

    fn refused(src: &str) -> String {
        let e = parse_one(src).unwrap_or_else(|e| panic!("{src}: {e:#}"));
        let err = crate::expr::seq::desugar(&e, &Env::default(), &ModPath::root())
            .expect_err(src);
        format!("{err:#}")
    }

    /// A connect, a `let`, a select's scrutinee and a block that read no
    /// variable are fired by the event.
    #[test]
    fn what_reads_no_variable_is_fired_by_the_event() {
        let l = lowered("on k { x <- 0; let n = 5; select true { _ => y <- n }; `Stop }");
        assert!(l.contains("x <- #onk ~! 0"), "{l}");
        assert!(l.contains("let n = #onk ~! 5"), "{l}");
        assert!(l.contains("select #onk ~! true"), "{l}");
        assert!(l.contains("y <- n"), "{l}");
        assert!(l.ends_with("#onk ~! `Stop }"), "{l}");
    }

    /// A call none of whose arguments fires is issued by the event: each
    /// argument is, and its result needs no second sample.
    #[test]
    fn a_call_over_constants_is_issued_by_the_event() {
        let l = lowered("on k { x <- f(1, \"a\"); g(x) }");
        assert!(l.contains("x <- f(#onk ~! 1, #onk ~! \"a\")"), "{l}");
        assert!(l.ends_with("g(x) }"), "{l}");
    }

    /// A function is a value the event does not fire, and its body is its
    /// own; a nested block of either kind is its own too.
    #[test]
    fn a_function_and_a_nested_block_are_left_alone() {
        let l = lowered(
            "on k { let f = |x| x + 1; seq t { a <- 1 }; on j { b <- 2 }; f(3) }",
        );
        assert!(l.contains("let f = |x| x + 1"), "{l}");
        assert!(l.contains("seq t { a <- 1 }"), "{l}");
        assert!(l.contains("on j { b <- 2 }"), "{l}");
        assert!(l.ends_with("f(#onk ~! 3) }"), "{l}");
    }

    /// `on let` names the event's value, and `live` is read where it stands.
    #[test]
    fn the_event_is_named_and_live_is_stripped() {
        let l = lowered("on let (a, b) = e { x <- a; y <- live(b) }");
        assert!(
            l.starts_with("{ let #onk = e; let (a, b) = #onk; x <- a; y <- b }"),
            "{l}"
        );
    }

    #[test]
    fn a_catch_in_the_block_is_refused() {
        let e = refused("on k { catch(e) x <- e; f(k)? }");
        assert!(e.contains("catch is not allowed inside an on block"), "{e}");
    }
}
