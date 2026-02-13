use anyhow::Result;
use graphix_compiler::{
    expr::ExprId, typ::FnType, Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
};
use graphix_derive::defpackage;
use graphix_package_core::{CachedArgs, CachedVals, EvalCached};
use netidx::publisher::Value;
use std::boxed::Box;

#[derive(Debug)]
struct ExampleBuiltin;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ExampleBuiltin {
    const NAME: &str = "{{name}}_example";
    deftype!("fn(Any) -> bool");

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(ExampleBuiltin))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ExampleBuiltin {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|v| match v {
            Value::Error(_) => Value::Bool(true),
            _ => Value::Bool(false),
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Default)]
struct ExampleCachedEv;

impl EvalCached for ExampleCachedEv {
    const NAME: &str = "{{name}}_example_cached";
    deftype!("fn(@args: bool) -> bool");

    fn eval(&mut self, from: &CachedVals) -> Option<Value> {
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

type ExampleCachedBuiltin = CachedArgs<ExampleCachedEv>;

defpackage! {
    builtins => [
        ExampleBuiltin,
        ExampleCachedBuiltin,
    ]
}
