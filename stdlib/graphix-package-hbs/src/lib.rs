#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    ExecCtx, FastFn, PrintFlag, Rt, UserEvent, deref_typ,
    effects::EffectKind,
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "hbs_render";
    const FASTCALL: Option<FastFn> = Some(fc_render);

    fn typecheck0(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [graphix_compiler::Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
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

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_render, from)
    }
}

type HbsRender = CachedArgs<HbsRenderEv>;

graphix_derive::defpackage! {
    builtins => [
        HbsRender,
    ],
}
