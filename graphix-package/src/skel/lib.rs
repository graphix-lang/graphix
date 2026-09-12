use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, Effect, Event, ExecCtx, Node, Rt, Scope, TagValue, TagView,
    UserEvent, effects::EffectKind, expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_derive::defpackage;
use graphix_package_core::{CachedArgs, CachedVals, EvalCached, unit_image_state};
use netidx_core::pack::PackError;
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

    // Restore what `image_encode` wrote; `from` is the argument list as
    // `init` saw it. The result slot is never imaged.
    fn image_decode(
        _ctx: &mut ExecCtx<R, E>,
        _from: &[Node<R, E>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(ExampleBuiltin::default()))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ExampleBuiltin {
    // The image is written before any cycle runs: encode exactly the state
    // `init` built (bind ids, generated nodes, configuration), keeping
    // `image_len` in lockstep with `image_encode`.
    fn image_len(&self) -> usize {
        0
    }

    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

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

// A payload with no state; one whose fields are all `Pack` derives
// `netidx_derive::Pack` and uses `pack_image_state!` instead.
unit_image_state!(ExampleCachedEv);

type ExampleCached = CachedArgs<ExampleCachedEv>;

defpackage! {
    builtins => [
        ExampleBuiltin,
        ExampleCached,
    ]
}
