use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, FastCall, Node, Rt, Scope, TagValue,
    TagView, UserEvent, expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_derive::defpackage;
use graphix_package_core::{CachedArgs, CachedVals, EvalCached, fast_eval, unit_image_state};
use netidx_core::pack::PackError;
use netidx_value::Value;
use std::boxed::Box;

#[derive(Debug, Default)]
struct ExampleBuiltin {
    // The result slot `update` lends to the caller.
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ExampleBuiltin {
    const NAME: &str = "{{ident}}_example";
    // What a builtin's output depends on (graphix_compiler::effects):
    // - `Stateless(fast)`: its arguments alone, in the cycle they arrive; a
    //   `FastCall` lets the JIT call it inside a fused kernel;
    // - `Sync`: the same cycle, but it keeps state across invocations, or
    //   depends on which arguments arrived;
    // - `Async`: it may answer later, on its own, or never.
    const EFFECT: Effect = Effect::Stateless(None);

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(ExampleBuiltin::default()))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ExampleBuiltin {
    // The image is written before any cycle runs: encode exactly the state
    // `init` built (bind ids, generated nodes, configuration).
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        // Every awake arg produces every cycle; match the view exhaustively.
        match from[0].update(ctx).view() {
            TagView::Fired(tv) => {
                let v = tv.with_value(|v| match v {
                    Value::Error(_) => Value::Bool(true),
                    _ => Value::Bool(false),
                });
                self.out.set(TagValue::fired(v))
            }
            TagView::Stale(_) => self.out.ride(),
            // A bottom input sets the result bottom: it never rides the
            // value from before.
            TagView::FreshBottom => self.out.set_bottom(true),
            TagView::StaleBottom => self.out.set_bottom(false),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

// The one implementation the interpreter (through `fast_eval`) and the
// JIT (through the `FastCall`) share: it sees only present arguments.
fn example_any(args: &[Value]) -> Option<Value> {
    Some(Value::Bool(args.iter().any(|v| matches!(v, Value::Bool(true)))))
}

#[derive(Debug, Default)]
struct ExampleCachedEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ExampleCachedEv {
    const NAME: &str = "{{ident}}_example_cached";
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(example_any)));

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, example_any, from)
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
