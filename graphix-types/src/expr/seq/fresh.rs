//! AST-to-AST lowering of `fresh e { .. }` (`design/fresh_blocks.md`): a
//! block that starts over at each fire of its event and runs live
//! between. It is a call of the block as a function of the event,
//! `(|#frv| { .. })(e)`, marked [`ExprKind::Fresh`]: the call holds no
//! instance until its argument first fires, and builds a new one, born,
//! at each fire.

use super::*;

/// The prefix of the parameter that takes a block's event, `#frv<id>`.
const EVENT: &str = "#frv";

pub(super) fn desugar(seq: &Parts) -> Result<Expr> {
    let pos = seq.spec.pos;
    let Some(trigger) = seq.trigger else { bail!("BUG: a fresh block with no event") };
    let event = format_compact!("{EVENT}{}", seq.spec.id.inner());
    let mut body: LPooled<Vec<Expr>> = LPooled::take();
    if let SeqTrigger::Bind(b) = trigger {
        refuse_rec(seq.spec, b)?;
        body.push(let_pat(pos, b.pattern.clone(), b.typ.clone(), r#ref(pos, &event)))
    }
    body.extend(seq.body.iter().cloned());
    let block = ExprKind::Block { exprs: Arc::from_iter(body.drain(..)) }.to_expr(pos);
    let lambda = ExprKind::Lambda(Arc::new(LambdaExpr {
        vargs_comments: Default::default(),
        below: Default::default(),
        args: Arc::from_iter([Arg {
            comments: Default::default(),
            kind: ArgKind::Positional,
            pattern: StructurePattern::Bind(Name::from(event.as_str())),
            constraint: None,
            pos: WrittenAt(pos),
        }]),
        vargs: None,
        rtype: None,
        constraints: Arc::from(Vec::<(TVar, Type)>::new()),
        throws: None,
        body: LambdaBody::Expr(block),
    }))
    .to_expr(pos);
    let call = ExprKind::Apply(ApplyExpr {
        args: Arc::from_iter([(None, trigger.expr().clone())]),
        function: Arc::new(lambda),
    })
    .to_expr(pos);
    Ok(ExprKind::Fresh(Arc::new(call)).to_expr(pos))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{PrintFlag, expr::parser::parse_one};

    fn lowered(src: &str) -> String {
        let e = parse_one(src).unwrap_or_else(|e| panic!("{src}: {e:#}"));
        let l = crate::expr::seq::desugar(&e, &Env::default(), &ModPath::root())
            .unwrap_or_else(|e| panic!("{src}: {e:#}"));
        let event = format_compact!("{EVENT}{}", e.id.inner());
        crate::format_with_flags(PrintFlag::Lowered, || l.to_string())
            .replace(event.as_str(), EVENT)
    }

    /// The block is a function of its event, called with it; nothing in it
    /// is rewritten.
    #[test]
    fn a_fresh_block_is_a_call_of_its_body() {
        let l = lowered("fresh k { n <- n + 1; count(k) }");
        assert_eq!(l, "(|#frv| { n <- n + 1; count(k) })(k)");
    }

    /// `fresh let` names the event's value in the body.
    #[test]
    fn the_event_is_named() {
        let l = lowered("fresh let (a, b) = e { a + b }");
        assert_eq!(l, "(|#frv| { let (a, b) = #frv; a + b })(e)");
    }
}
