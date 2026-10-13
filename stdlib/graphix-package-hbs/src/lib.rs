#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    errf,
    typ::{FnType, Type},
};
use graphix_package_core::{FastMemo, Shape, shape};
use graphix_package_json::value_to_json;
use handlebars::Handlebars;
use netidx_value::Value;
use std::cell::RefCell;

/// Register each entry of `partials`, a struct or a map of templates by
/// name as its type `typ` says, or null.
fn register_partials(
    registry: &mut Handlebars<'static>,
    env: &Env,
    typ: Option<&Type>,
    partials: &Value,
) -> std::result::Result<(), String> {
    match shape(env, typ, partials) {
        _ if *partials == Value::Null => Ok(()),
        Shape::Object(entries, _) => {
            for (name, tmpl, _) in entries.iter() {
                let Value::String(tmpl) = tmpl else {
                    return Err(format!("partial values must be strings, got {tmpl}"));
                };
                registry
                    .register_partial(name.as_str(), tmpl.as_str())
                    .map_err(|e| format!("{e}"))?;
            }
            Ok(())
        }
        Shape::Seq(_) | Shape::Leaf => {
            Err(format!("partials must be a struct, map, or null, got {partials}"))
        }
    }
}

thread_local! {
    static TEMPLATES: RefCell<FastMemo<(ArcStr, bool, Value, Option<Type>), Handlebars<'static>>> =
        RefCell::new(FastMemo::new(16));
}

fn build_registry(
    strict: bool,
    env: &Env,
    ft: &FnType,
    partials: &Value,
    template: &str,
) -> Result<Handlebars<'static>> {
    let mut registry = Handlebars::new();
    registry.set_strict_mode(strict);
    register_partials(&mut registry, env, ft.args.get(1).map(|a| &a.typ), partials)
        .map_err(|e| anyhow!("{e}"))?;
    registry.register_template_string("main", template).map_err(|e| anyhow!("{e}"))?;
    Ok(registry)
}

fn fc_render(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    match args {
        [Value::Bool(strict), partials, Value::String(template), data] => {
            let data_type = ft.args.get(3).map(|a| &a.typ);
            // CR claude for claude: [perf/low] This shape() call is made only to test
            // for an object, and value_to_json calls shape() again on the same data at
            // once. Every render therefore builds each field's (ArcStr, Value,
            // Option<Type>) entry twice, plus a String and an ArcStr per map key. Fix:
            // call value_to_json once and refuse unless the result is a
            // serde_json::Value::Object. to_json returns Object exactly when shape is
            // Object, so the error case is unchanged. (small-pkgs-11)
            if !matches!(shape(env, data_type, data), Shape::Object(..)) {
                return Some(errf!(
                    "HbsErr",
                    "the data must be a struct or a map, not {data}"
                ));
            }
            let json_data = match value_to_json(env, data_type, data) {
                Ok(j) => j,
                Err(e) => return Some(errf!("HbsErr", "{e}")),
            };
            let partials_type = ft.args.get(1).map(|a| a.typ.clone());
            let key = (template.clone(), *strict, partials.clone(), partials_type);
            Some(TEMPLATES.with(|c| {
                c.borrow_mut()
                    .with(
                        &key,
                        || build_registry(*strict, env, ft, partials, template),
                        // CR claude for claude: [bug/medium] x-panics-08 still
                        // reproduces: hbs::render with mutually recursive partials
                        // aborts the process. fc_render calls `registry.render("main",
                        // &json_data)` (lib.rs) with no bound on partial recursion.
                        // handlebars refuses only a partial that includes itself while
                        // it is the current template. A two-partial cycle, an inline
                        // self-include or a self-include under a block recurses until
                        // the runtime worker's stack overflows: an uncatchable abort of
                        // the whole program. Commit 5f6364bc re-addressed this CR to
                        // Eric, but no CR or pin mentioning it remains in the tree, so
                        // it has dropped out of tracking. Failure: The program below
                        // aborts: "thread 'tokio-rt-worker' has overflowed its stack /
                        // fatal runtime error: stack overflow, aborting", SIGABRT, in
                        // both engines. A template or partials map that comes from data
                        // (a config file, a request) can take down any server that
                        // renders it. Fix: Before rendering, walk the registered
                        // templates' ASTs for `{{> name}}` references, inline partials
                        // included, and refuse a cycle with `HbsErr`. Alternatively,
                        // register partials through a wrapper that tracks depth and
                        // fails past a fixed bound. Pin the three shapes from the
                        // original repro. Unverified: the reviewer's report, not
                        // checked by a verifier. (x-rerun-8-05)
                        // CR claude for claude: [bug/high] Partials that include each
                        // other without a data bound, such as {a: "x{{> b}}", b: "y{{>
                        // a}}"}, make registry.render recurse until the stack
                        // overflows, and the whole process aborts. handlebars refuses
                        // only a partial that includes itself directly
                        // (expand_partial's is_current_template check), and it has no
                        // depth limit. Templates and partials are often loaded from
                        // files, so the input can come from outside the program.
                        // Refusing every static cycle in build_registry would be wrong:
                        // a cycle under a block (node -> kid -> node inside {{#each
                        // kids}}) is ordinary tree rendering, and it works today. A fix
                        // could refuse, in build_registry, only a cycle of `{{> name}}`
                        // references that are not under any block, returning HbsErr. It
                        // could also run the render under a stack guard
                        // (crate::stack::ensure_sufficient or a stacker::grow with a
                        // fixed budget), so deep data-bounded recursion is contained
                        // too. probe: design/review-2026-10-12/repro/small-pkgs-01.gx
                        // (x-security-04 is the same defect). Also reported as
                        // (x-panics-01), (x-security-07). (small-pkgs-01)
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

graphix_package_core::typed_fast_builtin!(
    HbsRender,
    HbsRenderEv,
    "hbs_render",
    fc_render
);

graphix_derive::defpackage! {
    builtins => [
        HbsRender,
    ],
}
