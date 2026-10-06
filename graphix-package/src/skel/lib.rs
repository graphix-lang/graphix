use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, Node, Rt, Scope, TagValue, TagView,
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
    // CR claude for claude: [doc-drift] The comment above sends a same-cycle builtin to
    // Sync, but effects.rs and book/src/packages/creating.md reserve Sync for builtins
    // that keep cross-invocation state; a pure builtin is Stateless, the only class
    // that fuses. Both examples are pure (this one computes core's is_err,
    // ExampleCachedEv an any-true) yet stay Async. So packages scaffolded from here
    // start with builtins that node-walk and fail `#[native]` where they are called.
    // Line 4 imports effects::EffectKind, which nothing uses, so every new package
    // compiles with a warning. Mark the examples Stateless (the cached one with a
    // FastCall through fast_eval), describe the three classes as creating.md does, and
    // drop the import. (package-14)
    const EFFECT: Effect = Effect::Async;

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
            // CR claude for claude: [doc-drift] The template every new package starts
            // from presents the bottom ride as policy: a bottom input returns
            // `bottom_null` and keeps the pre-bottom result in `out` "for later stale
            // re-surfacing" through `Stale(_) => self.out.ride()`, where
            // tval.rs:201-205 and CLAUDE.md say a bottom input sets the resident
            // (`self.out.set_bottom(..)`) and never rides it. Its pure
            // `ExampleCachedEv` keeps the default `Effect::Async`, never shows
            // `Stateless`, `FastCall` or `fast_eval`, and has a dead `None => return
            // None` arm (CachedArgs never calls eval with a missing slot);
            // `effects::EffectKind` is imported and unused, a warning in every new
            // package. The book's fast-call example (book/src/packages/creating.md:178)
            // calls `fast_eval(my_len, from)` without `ctx`. "Not replayable, so it
            // must not be `Sync`" (sys/src/lib.rs:499, sys/src/dirs_mod.rs:18,
            // args/src/lib.rs:144) gives a reason that no longer applies: nothing
            // replays a builtin. (x-builtin-effects-20)
            // A consumed bottom bottoms the invocation; the result slot keeps
            // its history for later stale re-surfacing.
            TagView::FreshBottom => TagValue::bottom_null(true),
            TagView::StaleBottom => TagValue::bottom_null(false),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

#[derive(Debug, Default)]
struct ExampleCachedEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ExampleCachedEv {
    const NAME: &str = "{{name}}_example_cached";

    fn eval(&mut self, _ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
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
