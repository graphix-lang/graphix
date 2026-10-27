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
    template::{Parameter, Template, TemplateElement},
};
use netidx_value::Value;
use std::{
    cell::{Cell, RefCell},
    iter, mem, slice,
};

/// Register each entry of `partials`, a struct or a map of templates by
/// name as its type `typ` says, or null.
fn register_partials(
    registry: &mut Handlebars<'static>,
    block: &TemplateElement,
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
                register(registry, block, name.as_str(), tmpl)?;
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

/// How deep a template's or partial's text may nest its blocks and
/// subexpressions: rendering, cloning and dropping a template recurse once
/// per level.
const MAX_BLOCK_DEPTH: usize = 64;

/// The stack a template's compile takes beyond what its nesting does.
const STACK_BASE: usize = 256 * 1024;

/// The stack a template's compile may take per bracket in its text, at
/// most one per level it nests: twice what the deepest per-bracket shape,
/// a run of subexpressions, measures.
const STACK_PER_BRACKET: usize = 4 * 1024;

/// The block helper every block and partial expansion is wrapped in: it
/// counts the levels in progress and refuses one past
/// [`MAX_RENDER_DEPTH`].
const DEPTH_HELPER: &str = "__gx_render_depth";

thread_local! {
    static RENDER_DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// `text` compiled as the template `name`, refused when its blocks and
/// subexpressions nest deeper than [`MAX_BLOCK_DEPTH`]. Parsing, building
/// and dropping a template recurse once per bracket a level opens, so all
/// three run on a stack sized by the brackets in `text`.
// XCR claude for claude: [bug/medium] Only block tags are counted here, so
// subexpressions nested inside one tag (`{{lookup (lookup (lookup ... "x") "x") "x"}}`)
// are unbounded. At about 1000 levels hbs::render overflows the worker's stack and
// aborts the process instead of answering HbsErr. Count parenthesis depth within a tag
// (outside string literals) against MAX_BLOCK_DEPTH as well. probe:
// design/review-2026-10-26/repro/x-rerun-5-hbs-subexpr-overflow.gx (x-rerun-5-05)
// 2026-10-26 claude: Fixed with small-pkgs-01: the depth is measured on the parsed
// Template, where a subexpression (in a helper's name, params or hash) is a level as a
// block is, against the same MAX_BLOCK_DEPTH. The repro now answers HbsErr('blocks and
// subexpressions nest more than 64 deep'). Pin: graphix-tests
// lib_tests::hbs::hbs_text_nesting_refused (sub(64) renders, sub(65) and sub(5000) are
// refused).
// XCR claude for claude: [bug/high] shallow undercounts nesting wherever handlebars
// reads a `{{/..}}` as text. Case 1: `unescaped` skips one `{{` after a backslash, but
// the grammar's escape takes `\{{{{` whole, so `{{#if a}}\{{{{/if}}` repeated nests.
// Case 2: `tag_len` ends a tag at the first `}}`, but a path key `[..]` may hold `}}`,
// so `{{#if a}}{{x [}}{{/if}}]}}` repeated nests too. 5000 repeats overflow the stack
// and abort the process; 100 render past the 64 bound. Mirroring the grammar by hand
// will keep missing cases. Instead, compile under `stacker::grow` sized by the text
// length, measure the depth on the parsed `Template`, refuse past MAX_BLOCK_DEPTH
// (dropping the refused template inside the grown stack), and register the parsed
// templates. probe:
// design/review-2026-10-26/repro/small-pkgs-hbs-escaped-closer-abort.sh (small-pkgs-01)
// 2026-10-26 claude: shallow and its hand-mirrored scanner (unescaped, after, tag_len)
// are gone. hbs::compile parses each template and partial with
// Template::compile_with_name under stacker::maybe_grow, sized at 4 KiB per '{', '[' or
// '(' in the text (every level of handlebars' recursion opens one; the deepest shape, a
// run of subexpressions, measured about 2.2 KiB per bracket in dev and release),
// measures the depth on the parsed Template (nests_within: each block template, inverse
// and subexpression a level), drops a refused template on the grown stack, and
// registers the parsed template directly (register). The message is now 'blocks and
// subexpressions nest more than 64 deep'. A text of a million brackets reserves 4 GiB
// of address space, which a host refusing the mapping answers with a stacker panic.
// Pin: graphix-tests lib_tests::hbs::hbs_text_nesting_refused (escaped and keyed at 100
// and 5000); with the old scan it aborts with a stack overflow, and without maybe_grow
// too.
fn compile(name: &str, text: &str) -> std::result::Result<Template, String> {
    let brackets = text.bytes().filter(|b| matches!(b, b'{' | b'[' | b'(')).count();
    let stack = STACK_BASE + brackets * STACK_PER_BRACKET;
    stacker::maybe_grow(stack, stack, || {
        let t = Template::compile_with_name(text, name.to_owned())
            .map_err(|e| format!("{e}"))?;
        match nests_within(&t.elements, MAX_BLOCK_DEPTH) {
            true => Ok(t),
            false => Err(format!(
                "blocks and subexpressions nest more than {MAX_BLOCK_DEPTH} deep"
            )),
        }
    })
}

/// Whether the blocks and subexpressions in `elements` nest at most
/// `budget` deep: a block's template and inverse, and a subexpression,
/// are each a level.
fn nests_within(elements: &[TemplateElement], budget: usize) -> bool {
    use TemplateElement::*;
    let below =
        |elements: &[TemplateElement]| budget > 0 && nests_within(elements, budget - 1);
    elements.iter().all(|el| {
        let (name, params, hash, templates) = match el {
            Expression(h) | HtmlExpression(h) | HelperBlock(h) => {
                (&h.name, &h.params, &h.hash, [h.template.as_ref(), h.inverse.as_ref()])
            }
            DecoratorExpression(d)
            | DecoratorBlock(d)
            | PartialExpression(d)
            | PartialBlock(d) => {
                (&d.name, &d.params, &d.hash, [d.template.as_ref(), None])
            }
            _ => return true,
        };
        iter::once(name).chain(params).chain(hash.values()).all(|p| match p {
            Parameter::Subexpression(s) => below(slice::from_ref(&*s.element)),
            _ => true,
        }) && templates.into_iter().flatten().all(|t| below(&t.elements))
    })
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

/// Register `text` as the template `name`, compiled by [`compile`] with
/// every block and partial expansion bounded by [`bound_nesting`].
fn register(
    registry: &mut Handlebars<'static>,
    block: &TemplateElement,
    name: &str,
    text: &str,
) -> std::result::Result<(), String> {
    let mut t = compile(name, text)?;
    bound_nesting(&mut t, block);
    registry.register_template(name, t);
    Ok(())
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
    let block =
        Template::compile(&format!("{{{{#{DEPTH_HELPER}}}}}{{{{/{DEPTH_HELPER}}}}}"))?
            .elements
            .pop()
            .ok_or_else(|| anyhow!("the partial depth helper did not compile"))?;
    let partials_type = ft.args.get(1).map(|a| &a.typ);
    register_partials(&mut registry, &block, env, partials_type, partials)
        .and_then(|()| register(&mut registry, &block, "main", template))
        .map_err(|e| anyhow!("{e}"))?;
    registry.register_helper(DEPTH_HELPER, Box::new(RenderDepth));
    Ok(registry)
}

// CR claude for claude: [bug/low] The refusal messages here and in register_partials
// ("the data must be a struct or a map, not {data}", "partials must be ... got
// {partials}", "partial values must be strings, got {tmpl}") print the Value with its
// Rust Display. A tuple or variant therefore reaches the program as
// `[abstract:EnwukQtE2EoTn2Uo4FoXw40A, i64:1, i64:2]`, and a function as `abstract:`.
// Format them with graphix_types::typ::Naked, or TVal over the argument's type from ft,
// as str::sprintf does. probe:
// design/review-2026-10-26/repro/gx-stdlib-hbs-marker-leak.gx (gx-stdlib-02)
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
