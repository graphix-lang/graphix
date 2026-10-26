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

/// `text`, refused when its blocks nest deeper than [`MAX_BLOCK_DEPTH`]
/// as handlebars builds them: a block opens at `{{#..}}` and closes at
/// `{{/..}}`, and each `{{else ..}}` or `{{^ ..}}` naming a helper (a chain
/// link) nests one level more until its block closes. What the parser does
/// not read as a tag (an escaped `{{`, a comment, a raw block's text, a
/// string literal in a tag) opens and closes nothing.
// CR claude for claude: [bug/medium] Only block tags are counted here, so
// subexpressions nested inside one tag (`{{lookup (lookup (lookup ... "x") "x") "x"}}`)
// are unbounded. At about 1000 levels hbs::render overflows the worker's stack and
// aborts the process instead of answering HbsErr. Count parenthesis depth within a tag
// (outside string literals) against MAX_BLOCK_DEPTH as well. probe:
// design/review-2026-10-26/repro/x-rerun-5-hbs-subexpr-overflow.gx (x-rerun-5-05)
// CR claude for claude: [bug/high] shallow undercounts nesting wherever handlebars
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
fn shallow(text: &str) -> std::result::Result<&str, String> {
    let refused = || Err(format!("blocks nest more than {MAX_BLOCK_DEPTH} deep"));
    fn pad(s: &str) -> &str {
        s.trim_start_matches(|c: char| c == '~' || c.is_whitespace())
    }
    let mut links = [0usize; MAX_BLOCK_DEPTH + 1];
    let (mut blocks, mut depth) = (0usize, 0usize);
    let mut rest = text;
    while let Some(i) = unescaped(rest, "{{") {
        let tag = &rest[i..];
        rest = if let Some(raw) = tag.strip_prefix("{{{{") {
            let text = after(raw, "}}}}");
            after(&text[unescaped(text, "{{{{").unwrap_or(text.len())..], "}}}}")
        } else if let Some(comment) = tag.strip_prefix("{{!--")
            && comment.contains("--}}")
        {
            after(comment, "--}}")
        } else if let Some(comment) = tag.strip_prefix("{{!") {
            after(comment, "}}")
        } else {
            let body = pad(&tag[2..]);
            let invert = body.strip_prefix('^').or_else(|| body.strip_prefix("else"));
            match (body.as_bytes().first(), invert) {
                (Some(b'#'), _) => {
                    depth += 1;
                    if depth > MAX_BLOCK_DEPTH {
                        return refused();
                    }
                    links[blocks] = 0;
                    blocks += 1;
                }
                (Some(b'/'), _) if blocks > 0 => {
                    blocks -= 1;
                    depth -= 1 + links[blocks];
                }
                (_, Some(invert)) if blocks > 0 && !pad(invert).starts_with("}}") => {
                    depth += 1;
                    if depth > MAX_BLOCK_DEPTH {
                        return refused();
                    }
                    links[blocks - 1] += 1;
                }
                _ => (),
            }
            &tag[tag_len(tag)..]
        };
    }
    Ok(text)
}

/// Where in `s` the first `pat` that is not escaped (one backslash before
/// it) starts.
fn unescaped(s: &str, pat: &str) -> Option<usize> {
    let mut from = 0;
    while let Some(i) = s[from..].find(pat).map(|i| from + i) {
        let before = &s[..i];
        match before.ends_with('\\') && !before.ends_with("\\\\") {
            true => from = i + pat.len(),
            false => return Some(i),
        }
    }
    None
}

/// `s` past the first `pat`, or empty when there is none.
fn after<'a>(s: &'a str, pat: &str) -> &'a str {
    s.find(pat).map_or("", |i| &s[i + pat.len()..])
}

/// The length of the tag `tag` starts with, through its closing `}}`, a
/// string literal in it read whole.
fn tag_len(tag: &str) -> usize {
    let b = tag.as_bytes();
    let mut i = 2;
    while i < b.len() {
        match b[i] {
            b'}' if b.get(i + 1) == Some(&b'}') => return i + 2,
            q @ (b'"' | b'\'') if !b[i - 1].is_ascii_alphanumeric() => {
                i += 1;
                while i < b.len() && b[i] != q {
                    i += if b[i] == b'\\' { 2 } else { 1 };
                }
                i += 1;
            }
            _ => i += 1,
        }
    }
    b.len()
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
