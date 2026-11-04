#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    errf,
    typ::{FnType, NakedPrefix, Type},
};
use graphix_package_core::{FastMemo, Form, Shape, shape};
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
    match shape(env, typ, partials, Form::Template).map_err(|e| format!("{e}"))?.0 {
        Shape::Object(entries, _) => {
            for (name, tmpl, _) in entries.iter() {
                let Value::String(tmpl) = tmpl else {
                    return Err(format!(
                        "partial values must be strings, got {}",
                        NakedPrefix(tmpl)
                    ));
                };
                register(registry, block, name.as_str(), tmpl)?;
            }
            Ok(())
        }
        Shape::Seq(_) | Shape::Pairs(_) | Shape::Leaf(_) => Err(format!(
            "partials must be a struct, map, or null, got {}",
            NakedPrefix(partials)
        )),
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
fn compile(name: &str, text: &str) -> std::result::Result<Template, String> {
    // handlebars' compile is quadratic in the text (UPSTREAM.md): a text
    // surely too deep is refused before it
    if surely_deeper(text, MAX_BLOCK_DEPTH) {
        return Err(format!(
            "blocks and subexpressions nest more than {MAX_BLOCK_DEPTH} deep"
        ));
    }
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

/// Whether `text`'s template, as handlebars would build it, surely nests its
/// blocks and subexpressions deeper than [`nests_within`] admits for `max`: an
/// element (a tag, or text that is not blank) or a subexpression stands more
/// than `max` levels in. The scan reads handlebars' grammar (its
/// `grammar.pest`: escapes, comments, string literals and path keys in a tag)
/// and counts only levels the built template has, a block's (`{{#`) and each
/// `else` chain link's; where it cannot be sure what the grammar reads (a raw
/// block, a triple-stash tag, `else` run into a name, a quote in a path key)
/// it answers false, and the compile measures.
fn surely_deeper(text: &str, max: usize) -> bool {
    let b = text.as_bytes();
    let at = |i: usize, s: &[u8]| b.get(i..).is_some_and(|t| t.starts_with(s));
    let symbol = |c: u8| c.is_ascii_alphanumeric() || b"-_$:".contains(&c) || c >= 0x80;
    // the chain links each open block has taken, innermost last
    let mut links: Vec<usize> = Vec::new();
    let mut depth = 0usize;
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'\\' {
            let run = b[i..].iter().take_while(|c| **c == b'\\').count();
            if !at(i + run, b"{{") {
                i += run;
                continue;
            }
            if run == 1 {
                // an escaped mustache is text: `\{{`, and a second `{{`
                if depth > max {
                    return true;
                }
                i += 3 + if at(i + 3, b"{{") { 2 } else { 0 };
                continue;
            }
            // a run of two or more is text, and the mustache after it a tag
            i += run;
        }
        if !at(i, b"{{") {
            if depth > max && !b[i].is_ascii_whitespace() {
                return true;
            }
            i += 1;
            continue;
        }
        if at(i, b"{{{{") {
            return false;
        }
        let mut j = i + 2;
        if at(j, b"~") {
            j += 1;
        }
        if at(j, b"!") {
            let close: &[u8] = if at(j, b"!--") { b"--}}" } else { b"}}" };
            let Some(end) = b[j..].windows(close.len()).position(|w| w == close) else {
                return false;
            };
            if depth > max {
                return true;
            }
            i = j + end + close.len();
            continue;
        }
        enum Kind {
            Open,
            Close,
            Chain,
            Else,
            Plain,
        }
        let kind = match b.get(j) {
            Some(b'{') | None => return false,
            Some(b'#') => Kind::Open,
            Some(b'/') => Kind::Close,
            Some(b'^') => {
                let rest = b[j + 1..].iter().position(|c| !c.is_ascii_whitespace());
                match rest.map(|k| b[j + 1 + k]) {
                    Some(b'~' | b'}') | None => Kind::Else,
                    Some(_) => Kind::Chain,
                }
            }
            Some(_) if at(j, b"else") => match b.get(j + 4) {
                Some(c) if symbol(*c) => return false,
                _ => {
                    let rest = b[j + 4..].iter().position(|c| !c.is_ascii_whitespace());
                    match rest.map(|k| b[j + 4 + k]) {
                        Some(b'~' | b'}') | None => Kind::Else,
                        Some(_) => Kind::Chain,
                    }
                }
            },
            Some(_) => Kind::Plain,
        };
        // the tag's end, and how deep its subexpressions nest
        let (mut parens, mut deepest) = (0usize, 0usize);
        let mut k = j;
        let end = loop {
            match b.get(k) {
                None => return false,
                Some(b'}') if at(k, b"}}") => break k + 2,
                Some(q @ (b'"' | b'\'')) => {
                    k += 1;
                    loop {
                        match b.get(k) {
                            None => return false,
                            Some(b'\\') => k += 2,
                            Some(c) if c == q => break,
                            Some(_) => k += 1,
                        }
                    }
                    k += 1;
                }
                Some(b'[') => {
                    let Some(close) = b[k..].iter().position(|c| *c == b']') else {
                        return false;
                    };
                    if b[k..k + close].iter().any(|c| *c == b'"' || *c == b'\'') {
                        return false;
                    }
                    k += close + 1;
                }
                Some(b'(') => {
                    parens += 1;
                    deepest = deepest.max(parens);
                    k += 1;
                }
                Some(b')') => {
                    parens = parens.saturating_sub(1);
                    k += 1;
                }
                Some(_) => k += 1,
            }
        };
        if !matches!(kind, Kind::Close | Kind::Else) && depth + deepest > max {
            return true;
        }
        match kind {
            Kind::Open => {
                links.push(0);
                depth += 1;
            }
            Kind::Chain => {
                let Some(l) = links.last_mut() else { return false };
                *l += 1;
                depth += 1;
            }
            Kind::Close => {
                let Some(l) = links.pop() else { return false };
                depth = depth.saturating_sub(1 + l);
            }
            Kind::Else | Kind::Plain => (),
        }
        i = end;
    }
    false
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

fn fc_render(env: &Env, ft: &FnType, args: &[Value]) -> Option<Value> {
    match args {
        [Value::Bool(strict), partials, Value::String(template), data] => {
            let data_type = ft.args.get(3).map(|a| &a.typ);
            // handlebars renders from a JSON tree, built here once
            let view = JsonView::new(env, data_type, data, Form::Template);
            let json_data = match serde_json::to_value(view) {
                Ok(j @ handlebars::JsonValue::Object(_)) => j,
                Ok(_) => {
                    return Some(errf!(
                        "HbsErr",
                        "the data must be a struct or a map, not {}",
                        NakedPrefix(data)
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
    fc_render; cached
);

graphix_derive::defpackage! {
    builtins => [
        HbsRender,
    ],
}

#[cfg(test)]
mod test {
    use super::*;

    /// Whether handlebars' build of `text` nests within `max`, `None` where
    /// it does not compile.
    fn built_within(text: &str, max: usize) -> Option<bool> {
        let t = Template::compile_with_name(text, "t".to_owned()).ok()?;
        Some(nests_within(&t.elements, max))
    }

    /// The scan refuses only what the built template refuses.
    fn agrees(text: &str, max: usize) {
        if surely_deeper(text, max) {
            assert_eq!(built_within(text, max), Some(false), "max {max}: {text}");
        }
    }

    #[test]
    fn nested_blocks_and_chains_are_counted_exactly() {
        let nest =
            |k: usize| format!("{}x{}", "{{#if a}}".repeat(k), "{{/if}}".repeat(k));
        let chain =
            |k: usize| format!("{{{{#if a}}}}x{}{{{{/if}}}}", "{{else if a}}y".repeat(k));
        let sub = |k: usize| {
            format!("{{{{x {}a{}}}}}", "(lookup ".repeat(k), " \"b\")".repeat(k))
        };
        for k in 0..8 {
            for max in 0..6 {
                for text in [nest(k), chain(k), sub(k)] {
                    let refused = built_within(&text, max) == Some(false);
                    assert_eq!(surely_deeper(&text, max), refused, "max {max}: {text}");
                }
            }
        }
        assert!(!surely_deeper(&nest(64), MAX_BLOCK_DEPTH));
        assert!(surely_deeper(&nest(65), MAX_BLOCK_DEPTH));
        assert!(surely_deeper(&nest(5000), MAX_BLOCK_DEPTH));
    }

    #[test]
    fn what_the_grammar_reads_as_text_counts_no_level() {
        let three = "{{#if a}}{{#if a}}{{#if a}}";
        for text in [
            format!("{{{{!-- {three} --}}}}x"),
            format!("{{{{! {three} }}}}x"),
            format!("{{{{lookup this \"{three}\"}}}}"),
            format!("{{{{lookup this '{three}'}}}}"),
            format!("{{{{x.[{three}]}}}}"),
            format!("\\{three}x"),
            "\\{{{{#if a}}\\{{{{#if a}}\\{{{{#if a}}x".to_owned(),
            "{{#if a}}{{else}}{{else}}x{{/if}}".to_owned(),
            "{{#if a}}{{^}}x{{/if}}".to_owned(),
        ] {
            assert!(!surely_deeper(&text, 2), "{text}");
            agrees(&text, 2);
        }
    }

    /// A deep text the scan leaves to the compile (it reads no raw block) is
    /// refused by it, on a stack grown for it, not overflowed.
    #[test]
    fn a_deep_text_the_scan_leaves_is_refused_by_the_compile() {
        let k = 5000;
        let text = format!(
            "{{{{{{{{raw}}}}}}}}{{{{{{{{/raw}}}}}}}}{}x{}",
            "{{#if a}}".repeat(k),
            "{{/if}}".repeat(k)
        );
        assert!(!surely_deeper(&text, MAX_BLOCK_DEPTH));
        let refused = compile("t", &text).err();
        assert!(
            refused.as_deref().is_some_and(|e| e.contains("nest more than 64 deep")),
            "{refused:?}"
        );
    }

    /// A balanced template of `depth` levels at most, from the pieces the
    /// scan reads, drawn by `next`.
    fn template(next: &mut impl FnMut(usize) -> usize, depth: usize, out: &mut String) {
        for _ in 0..next(4) {
            match next(if depth == 0 { 10 } else { 14 }) {
                0 => out.push('x'),
                1 => out.push_str(" \n"),
                2 => out.push_str("{{x}}"),
                3 => {
                    let k = next(4);
                    out.push_str(&format!(
                        "{{{{~x {}a{}~}}}}",
                        "(lookup ".repeat(k),
                        " \"b\")".repeat(k)
                    ))
                }
                4 => out.push_str("{{!-- {{#if a}}{{/if}} }} --}}"),
                5 => out.push_str("{{! {{#if a}} }}"),
                6 => out.push_str("{{lookup this \"}}{{#if a}}\" 'x}}'}}"),
                7 => out.push_str("{{x.[}}{{#if a}}]}}"),
                8 => out.push_str("\\{{#if a}}"),
                9 => out.push_str("\\\\{{x}}"),
                10 | 11 => {
                    out.push_str("{{~#if a}}");
                    template(next, depth - 1, out);
                    for _ in 0..next(3) {
                        out.push_str("{{else if b~}}");
                        template(next, depth - 1, out);
                    }
                    if next(2) == 0 {
                        out.push_str("{{else}}");
                        template(next, depth - 1, out);
                    }
                    out.push_str("{{/if}}");
                }
                12 => {
                    out.push_str("{{#> p}}");
                    template(next, depth - 1, out);
                    out.push_str("{{/p}}");
                }
                _ => {
                    out.push_str("{{#*inline \"q\"}}");
                    template(next, depth - 1, out);
                    out.push_str("{{/inline}}");
                }
            }
        }
    }

    #[test]
    fn generated_templates_are_refused_only_where_the_build_is() {
        let mut state: u64 = 0x9e3779b97f4a7c15;
        let mut next = |n: usize| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state % n as u64) as usize
        };
        let (mut refused, mut compiled) = (0, 0);
        for _ in 0..2000 {
            let mut text = String::new();
            template(&mut next, 5, &mut text);
            if built_within(&text, 0).is_none() {
                continue;
            }
            compiled += 1;
            for max in 0..5 {
                agrees(&text, max);
                refused += surely_deeper(&text, max) as usize;
            }
        }
        assert!(
            compiled > 1500 && refused > 1000,
            "{compiled} compiled, {refused} refused"
        );
    }
}
