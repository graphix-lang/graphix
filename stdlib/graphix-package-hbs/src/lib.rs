#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    CompileCtx, ExecCtx, FastCall, Rt, UserEvent, deref_typ,
    effects::Effect,
    errf,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, FastMemo, fast_eval, is_struct,
};
use graphix_package_json::value_to_json;
use handlebars::Handlebars;
use netidx::publisher::Typ;
use netidx_value::Value;
use std::cell::RefCell;

fn is_null_type(t: &Type) -> bool {
    matches!(t, Type::Primitive(flags) if flags.iter().count() == 1 && flags.contains(Typ::Null))
}

fn register_partials(
    registry: &mut Handlebars<'static>,
    partials: &Value,
) -> std::result::Result<(), String> {
    match partials {
        Value::Null => Ok(()),
        Value::Array(arr) if is_struct(arr) => {
            for field in arr.iter() {
                if let Value::Array(pair) = field {
                    if let (Value::String(name), Value::String(tmpl)) =
                        (&pair[0], &pair[1])
                    {
                        registry
                            .register_partial(name.as_str(), tmpl.as_str())
                            .map_err(|e| format!("{e}"))?;
                    } else {
                        return Err(format!(
                            "partial values must be strings, got {}",
                            &pair[1]
                        ));
                    }
                }
            }
            Ok(())
        }
        Value::Map(m) => {
            for (k, v) in m.into_iter() {
                match v {
                    Value::String(tmpl) => {
                        registry
                            .register_partial(&format!("{k}"), tmpl.as_str())
                            .map_err(|e| format!("{e}"))?;
                    }
                    _ => return Err(format!("partial values must be strings, got {v}")),
                }
            }
            Ok(())
        }
        v => Err(format!("partials must be a struct, map, or null, got {v}")),
    }
}

thread_local! {
    static TEMPLATES: RefCell<FastMemo<(ArcStr, bool, Value), Handlebars<'static>>> =
        RefCell::new(FastMemo::new(16));
}

fn build_registry(
    strict: bool,
    partials: &Value,
    template: &str,
) -> Result<Handlebars<'static>> {
    let mut registry = Handlebars::new();
    registry.set_strict_mode(strict);
    register_partials(&mut registry, partials).map_err(|e| anyhow!("{e}"))?;
    registry.register_template_string("main", template).map_err(|e| anyhow!("{e}"))?;
    Ok(registry)
}

fn fc_render(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Bool(strict), partials, Value::String(template), data] => {
            let json_data = match value_to_json(data) {
                Ok(j) => j,
                Err(e) => return Some(errf!("HbsErr", "{e}")),
            };
            let key = (template.clone(), *strict, partials.clone());
            Some(TEMPLATES.with(|c| {
                c.borrow_mut()
                    .with(
                        &key,
                        || build_registry(*strict, partials, template),
                        // CR claude for eric: [bug] Handlebars::render has no depth
                        // bound. handlebars only refuses a partial that includes itself
                        // while it is the current template, so these all recurse until
                        // the worker's stack overflows, and the whole process aborts
                        // (exit 134) instead of returning `HbsErr`: a cycle through two
                        // partials, an inline partial that includes itself, or a
                        // self-include after any block (`{{#if n}}x{{/if}}{{> p}}`).
                        // The template string alone is enough to trigger it, and
                        // templates are run-time values. A cycle check over #partials
                        // in build_registry would not close it: inline partials live in
                        // the template, and `{{> (lookup this "pn")}}` computes the
                        // name from data. probe:
                        // design/review-2026-10-05/repro/x-panics-08.gx (x-panics-08)
                        |registry| match registry.render("main", &json_data) {
                            Ok(s) => Value::String(ArcStr::from(s.as_str())),
                            Err(e) => errf!("HbsErr", "{e}"),
                        },
                    )
                    .unwrap_or_else(|e| errf!("HbsErr", "{e}"))
            }))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct HbsRenderEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HbsRenderEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_render)));
    const NAME: &str = "hbs_render";

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [graphix_compiler::Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    // CR claude for eric: [bug] This hook refuses a #partials that is not a struct, map
    // or null, and data that is not a struct or map. The check (--check, the LSP) never
    // runs typecheck1, and the signature's 'a and 'b admit anything, so the editor
    // shows no error and the build refuses; graphix-fuzz check reports this as a
    // type-system bug. sys::net::call's typecheck1 (graphix-package-sys/src/net.rs:487)
    // and sys::net::rpc's validate_spec (net.rs:890) refuse the same way, although
    // design/tvar_constraints.md:205 says these hooks never refuse. Through a dynamic
    // call, a run-time bind only logs the refusal ("did not elaborate"): hbs::render
    // then renders 42 as "v=42", and sys::net::rpc with #spec {x: 5} reaches the
    // unreachable!() at net.rs:1149 and kills the runtime. Move each rule to where the
    // check sees it (a bound on the signature's variable, the open item at
    // design/parallel_compile.md:408), or check it at run time and return an error
    // value. probe: design/review-2026-10-05/repro/x-builtin-effects-08.sh
    // (x-builtin-effects-08)
    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [graphix_compiler::Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        if let Some(partials_arg) = resolved.args.get(1) {
            deref_typ!("struct, map, or null", ctx, &partials_arg.typ,
                Some(Type::Struct(_)) => Ok(()),
                Some(Type::Map { .. }) => Ok(()),
                Some(t @ Type::Primitive(_)) => {
                    if is_null_type(t) { Ok(()) }
                    else { bail!("hbs::render #partials must be a struct, map, or null") }
                },
                None => Ok(()) // unresolved = using default
            )?;
        }
        if let Some(data_arg) = resolved.args.get(3) {
            deref_typ!("struct or map", ctx, &data_arg.typ,
                Some(Type::Struct(_)) => Ok(()),
                Some(Type::Map { .. }) => Ok(())
            )?;
        }
        Ok(())
    }

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_render, from)
    }
}

type HbsRender = CachedArgs<HbsRenderEv>;

graphix_package_core::unit_image_state!(HbsRenderEv);

graphix_derive::defpackage! {
    builtins => [
        HbsRender,
    ],
}
