#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::ArcStr;
use graphix_compiler::{deref_typ, errf, typ::Type, ExecCtx, PrintFlag, Rt, TypecheckPhase, UserEvent};
use graphix_package_core::{is_struct, CachedArgs, CachedVals, EvalCached};
use graphix_package_json::value_to_json;
use handlebars::Handlebars;
use netidx::publisher::Typ;
use netidx_value::Value;

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

#[derive(Debug)]
struct HbsRenderEv {
    registry: Handlebars<'static>,
    last_template: Option<ArcStr>,
    last_strict: bool,
    last_partials: Option<Value>,
}

impl Default for HbsRenderEv {
    fn default() -> Self {
        Self {
            registry: Handlebars::new(),
            last_template: None,
            last_strict: false,
            last_partials: None,
        }
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for HbsRenderEv {
    const NAME: &str = "hbs_render";
    const NEEDS_CALLSITE: bool = true;

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [graphix_compiler::Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
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
        }
    }

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let strict = cached.get::<bool>(0)?;
        let partials = cached.0.get(1)?.clone();
        let template = match cached.0.get(2)?.as_ref()? {
            Value::String(s) => s.clone(),
            _ => return Some(errf!("HbsErr", "template must be a string")),
        };
        let data = cached.0.get(3)?.as_ref()?;
        // rebuild registry if template, strict, or partials changed
        let template_changed =
            self.last_template.as_ref().map_or(true, |prev| prev != &template);
        let strict_changed = self.last_strict != strict;
        let partials_changed = self.last_partials != partials;
        if template_changed || strict_changed || partials_changed {
            self.registry = Handlebars::new();
            self.registry.set_strict_mode(strict);
            if let Some(ref p) = partials {
                if let Err(e) = register_partials(&mut self.registry, p) {
                    return Some(errf!("HbsErr", "{e}"));
                }
            }
            match self.registry.register_template_string("main", template.as_str()) {
                Ok(()) => (),
                Err(e) => return Some(errf!("HbsErr", "{e}")),
            }
            self.last_template = Some(template);
            self.last_strict = strict;
            self.last_partials = partials;
        }
        let json_data = match value_to_json(data) {
            Ok(j) => j,
            Err(e) => return Some(errf!("HbsErr", "{e}")),
        };
        match self.registry.render("main", &json_data) {
            Ok(s) => Some(Value::String(ArcStr::from(s.as_str()))),
            Err(e) => Some(errf!("HbsErr", "{e}")),
        }
    }
}

type HbsRender = CachedArgs<HbsRenderEv>;

graphix_derive::defpackage! {
    builtins => [
        HbsRender,
    ],
}
