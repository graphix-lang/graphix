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
use graphix_package_json::JsonView;
use handlebars::{
    Context, Handlebars, Helper, HelperDef, HelperResult, Output, RenderContext,
    RenderErrorReason, Renderable,
    template::{Template, TemplateElement},
};
use netidx_value::Value;
use std::{
    cell::{Cell, RefCell},
    mem,
};

/// Register each entry of `partials`, a struct or a map of templates by
/// name as its type `typ` says, or null.
fn register_partials(
    registry: &mut Handlebars<'static>,
    env: &Env,
    typ: Option<&Type>,
    partials: &Value,
) -> std::result::Result<(), String> {
    if *partials == Value::Null {
        return Ok(());
    }
    match shape(env, typ, partials).map_err(|e| format!("{e}"))? {
        Shape::Object(entries, _) => {
            for (name, tmpl, _) in entries.iter() {
                let Value::String(tmpl) = tmpl else {
                    return Err(format!("partial values must be strings, got {tmpl}"));
                };
                registry
                    .register_partial(name.as_str(), shallow(tmpl)?)
                    .map_err(|e| format!("{e}"))?;
            }
            Ok(())
        }
        Shape::Seq(_) | Shape::Leaf(_) => {
            Err(format!("partials must be a struct, map, or null, got {partials}"))
        }
    }
}

thread_local! {
    static TEMPLATES: RefCell<FastMemo<(ArcStr, bool, Value, Option<Type>), Handlebars<'static>>> =
        RefCell::new(FastMemo::new(16));
}

/// How deep blocks and partial expansions may nest while a template
/// renders, each a level of handlebars' recursion. Nothing else bounds a
/// cycle of partials (handlebars refuses only a partial naming itself),
/// and one would recurse until the stack overflows.
const MAX_RENDER_DEPTH: usize = 128;

/// How deep a template's or partial's text may nest its blocks: building
/// and dropping a template recurses once per level.
const MAX_BLOCK_DEPTH: usize = 64;

/// The block helper every block and partial expansion is wrapped in: it
/// counts the levels in progress and refuses one past
/// [`MAX_RENDER_DEPTH`].
const DEPTH_HELPER: &str = "__gx_render_depth";

thread_local! {
    static RENDER_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// `text`, refused when its blocks (`{{#..}}`, `{{^..}}`, closed by
/// `{{/..}}`) nest deeper than [`MAX_BLOCK_DEPTH`].
// XCR claude for claude: [bug/medium] Only partial expansions are bounded
// (MAX_PARTIAL_DEPTH); block nesting is not. Handlebars compiles iteratively, but the
// template clone here, bound_partials, Template's drop and render all recurse per
// level. 3000 nested `{{#if}}` blocks abort the process with a stack overflow, even
// when the condition is false and render enters only the outer block. So do two
// partials that include each other with 12 nested blocks each, before the 128-partial
// cap answers. Scan each template's and partial's text for block depth before compiling
// it, and answer HbsErr past a cap. Then count blocks as well as partials in the depth
// helper (or lower MAX_PARTIAL_DEPTH) so cap × depth fits the stack. probe:
// design/review-2026-10-19/repro/x-panics-hbs-nesting-stack-overflow.gx (x-panics-03)
// 2026-10-19 claude: A template's or partial's text whose blocks nest past
// MAX_BLOCK_DEPTH (64) is refused before it is built (shallow), and the render-depth
// helper now wraps every block as well as every partial, so MAX_RENDER_DEPTH (128)
// bounds blocks and partials together. Pin: lib_tests::hbs::hbs_block_nesting_refused
// (mutual partials of 12 blocks, 5000 nested blocks, 60 nested blocks rendering);
// hbs_partial_cycle_refused's message is now 'templates nest more than 128 deep'.
fn shallow(text: &str) -> std::result::Result<&str, String> {
    let (mut depth, mut deepest) = (0usize, 0usize);
    for (i, _) in text.match_indices("{{") {
        match text[i + 2..].trim_start_matches('~').as_bytes().first() {
            Some(b'#' | b'^') => {
                depth += 1;
                deepest = deepest.max(depth);
            }
            Some(b'/') => depth = depth.saturating_sub(1),
            _ => (),
        }
    }
    match deepest > MAX_BLOCK_DEPTH {
        true => Err(format!("blocks nest more than {MAX_BLOCK_DEPTH} deep")),
        false => Ok(text),
    }
}

struct RenderDepth;

impl HelperDef for RenderDepth {
    fn call<'reg: 'rc, 'rc>(
        &self,
        h: &Helper<'rc>,
        r: &'reg Handlebars<'reg>,
        ctx: &'rc Context,
        rc: &mut RenderContext<'reg, 'rc>,
        out: &mut dyn Output,
    ) -> HelperResult {
        let depth = RENDER_DEPTH.with(|d| d.replace(d.get() + 1)) + 1;
        let res = match h.template() {
            _ if depth > MAX_RENDER_DEPTH => Err(RenderErrorReason::Other(format!(
                "templates nest more than {MAX_RENDER_DEPTH} deep"
            ))
            .into()),
            Some(t) => t.render(r, ctx, rc, out),
            None => Ok(()),
        };
        RENDER_DEPTH.with(|d| d.set(depth - 1));
        res
    }
}

/// Wrap every block and partial expansion in `t`, nested templates and
/// inline partials included, in a [`DEPTH_HELPER`] block like `block`.
fn bound_nesting(t: &mut Template, block: &TemplateElement) {
    let Template { elements, mapping, .. } = t;
    for (i, el) in elements.iter_mut().enumerate() {
        match el {
            TemplateElement::PartialExpression(_) => (),
            TemplateElement::PartialBlock(d) | TemplateElement::DecoratorBlock(d) => {
                if let Some(t) = &mut d.template {
                    bound_nesting(t, block)
                }
            }
            TemplateElement::HelperBlock(h) => {
                for t in [&mut h.template, &mut h.inverse].into_iter().flatten() {
                    bound_nesting(t, block)
                }
            }
            _ => continue,
        }
        if let TemplateElement::DecoratorBlock(_) = el {
            continue;
        }
        let mut wrapper = block.clone();
        if let TemplateElement::HelperBlock(h) = &mut wrapper {
            let mut inner = Template::default();
            inner
                .elements
                .push(mem::replace(el, TemplateElement::Comment(String::new())));
            inner.mapping.extend(mapping.get(i).cloned());
            h.template = Some(inner);
        }
        *el = wrapper;
    }
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
    registry
        .register_template_string("main", shallow(template).map_err(|e| anyhow!("{e}"))?)
        .map_err(|e| anyhow!("{e}"))?;
    let block =
        Template::compile(&format!("{{{{#{DEPTH_HELPER}}}}}{{{{/{DEPTH_HELPER}}}}}"))?
            .elements
            .pop()
            .ok_or_else(|| anyhow!("the partial depth helper did not compile"))?;
    let names: Vec<String> = registry.get_templates().keys().cloned().collect();
    for name in names {
        if let Some(mut t) = registry.get_template(&name).cloned() {
            bound_nesting(&mut t, &block);
            registry.register_template(&name, t);
        }
    }
    registry.register_helper(DEPTH_HELPER, Box::new(RenderDepth));
    Ok(registry)
}

fn fc_render(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    match args {
        [Value::Bool(strict), partials, Value::String(template), data] => {
            let data_type = ft.args.get(3).map(|a| &a.typ);
            // handlebars renders from a JSON tree, built here once
            let view = JsonView::new(env, data_type, data);
            let json_data = match serde_json::to_value(view) {
                Ok(j @ handlebars::JsonValue::Object(_)) => j,
                Ok(_) => {
                    return Some(errf!(
                        "HbsErr",
                        "the data must be a struct or a map, not {data}"
                    ));
                }
                Err(e) => return Some(errf!("HbsErr", "{e}")),
            };
            let partials_type = ft.args.get(1).map(|a| a.typ.clone());
            let key = (template.clone(), *strict, partials.clone(), partials_type);
            Some(TEMPLATES.with(|c| {
                c.borrow_mut()
                    .with(
                        &key,
                        || build_registry(*strict, env, ft, partials, template),
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
