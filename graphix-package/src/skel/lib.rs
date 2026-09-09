use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, Effect, Event, ExecCtx, Node, Rt, Scope, TagValue, TagView,
    UserEvent, effects::EffectKind, expr::ExprId, typ::FnType,
};
use graphix_derive::defpackage;
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};
use netidx_value::Value;
use std::boxed::Box;

#[derive(Debug, Default)]
struct ExampleBuiltin {
    // The result slot `update` lends to the caller.
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ExampleBuiltin {
    const NAME: &str = "{{name}}_example";
    // Override to `Sync` only if every output lands on the same cycle
    // as the input that triggered it.
    const EFFECT: Effect = Effect::Async;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved_typ: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(ExampleBuiltin::default()))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ExampleBuiltin {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        // Every awake arg produces every cycle; match the view exhaustively.
        match from[0].update(ctx, event).view() {
            TagView::Fired(tv) => {
                let v = tv.with_value(|v| match v {
                    Value::Error(_) => Value::Bool(true),
                    _ => Value::Bool(false),
                });
                self.out.set(TagValue::fired(v))
            }
            TagView::Stale(_) => self.out.ride(),
            // A consumed bottom bottoms the invocation; the result slot keeps
            // its history for later stale re-surfacing.
            TagView::FreshBottom => TagValue::bottom_null(true),
            TagView::StaleBottom => TagValue::bottom_null(false),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Default)]
struct ExampleCachedEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ExampleCachedEv {
    const NAME: &str = "{{name}}_example_cached";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(false));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => {
                    res = Some(Value::Bool(true));
                }
                Some(_) => (),
            }
        }
        res
    }
}

type ExampleCached = CachedArgs<ExampleCachedEv>;

defpackage! {
    builtins => [
        ExampleBuiltin,
        ExampleCached,
    ]
}
