#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_package_core::{FastMemo, is_struct};
use graphix_package_json::value_to_json;
use handlebars::Handlebars;
use netidx_value::Value;
use std::cell::RefCell;

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
                            .register_partial(
                                &graphix_package_core::map_key(k),
                                tmpl.as_str(),
                            )
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
            let is_map = match data {
                Value::Map(_) => true,
                Value::Array(a) => is_struct(a),
                _ => false,
            };
            if !is_map {
                return Some(errf!(
                    "HbsErr",
                    "the data must be a struct or a map, not {data}"
                ));
            }
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
                        // 2026-10-07 claude: open. handlebars 6.4 has no recursion limit and no hook on a
                        // partial's render to count depth through; an inline or data-named partial
                        // escapes any guard set from here. Needs an upstream limit or a vendored patch.
                        // 2026-10-08 claude: re-addressed: handlebars 6.4 has no
                        // recursion limit and no per-partial hook, and inline or
                        // data-named partials escape any guard from here. Options: vendor
                        // a patched handlebars with a depth bound, or upstream one; a
                        // dependency-policy call.
                        // 2026-10-08 claude: re-addressed: handlebars 6.4 has no
                        // recursion limit and no per-partial hook, and inline or
                        // data-named partials escape any guard from here. Options: vendor
                        // a patched handlebars with a depth bound, or upstream one; a
                        // dependency-policy call.
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

graphix_package_core::fast_builtin!(HbsRender, HbsRenderEv, "hbs_render", fc_render);

graphix_derive::defpackage! {
    builtins => [
        HbsRender,
    ],
}
