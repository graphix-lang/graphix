use anyhow::Result;
use arcstr::literal;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::subscriber::Value;

run!(hbs_basic, r#"{
    let r = hbs::render("Hello {{name}}!", {name: "world"})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Hello world!")
}; FuseExpect::Jit);

run!(hbs_struct_fields, r#"{
    let r = hbs::render("{{first}} {{last}}", {first: "Alice", last: "Smith"})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Alice Smith")
}; FuseExpect::Jit);

run!(hbs_nested, r#"{
    let r = hbs::render("{{person.name}} is {{person.age}}", {person: {name: "Bob", age: 30}})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Bob is 30")
}; FuseExpect::Jit);

run!(hbs_each, r#"{
    let r = hbs::render("{{#each items}}{{this}} {{/each}}", {items: ["a", "b", "c"]})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a b c ")
}; FuseExpect::Jit);

run!(hbs_if, r#"{
    let r = hbs::render("{{#if show}}visible{{else}}hidden{{/if}}", {show: true})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "visible")
}; FuseExpect::Jit);

run!(hbs_if_false, r#"{
    let r = hbs::render("{{#if show}}visible{{else}}hidden{{/if}}", {show: false})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hidden")
}; FuseExpect::Jit);

run!(hbs_partials, r#"{
    let r = hbs::render(
        #partials: {header: "<h1>{{title}}</h1>"},
        "{{> header}} body",
        {title: "Hi"}
    )$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "<h1>Hi</h1> body")
}; FuseExpect::Jit);

run!(hbs_strict_missing, r#"{
    let r = hbs::render(#strict: true, "{{missing}}", {x: 1});
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

run!(hbs_invalid_template, r#"{
    let r = hbs::render("{{#if}}", {x: 1});
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

run!(hbs_numeric_data, r#"{
    let r = hbs::render("count: {{n}}", {n: 42})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "count: 42")
}; FuseExpect::Jit);

run!(hbs_non_strict_missing, r#"{
    let r = hbs::render("hello {{missing}}", {x: 1})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello ")
}; FuseExpect::Jit);

// One thread-local template memo serves every site: a site whose
// template VARIES per call rebuilds on the miss and renders the right
// one.
run!(
    hbs_template_varies,
    r#"{
    let r = |t: string| { let s = #[native] hbs::render(t, {name: "x"})$; s };
    array::map(["a {{name}}", "b {{name}}", "a {{name}}"], r)
}"#,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                a.len() == 3
                    && a.iter()
                        .zip(["a x", "b x", "a x"])
                        .all(|(v, e)| matches!(v, Value::String(s) if &**s == e))
            }
            _ => false,
        }
    }
);

// The data renders as its type says: an array of pairs iterates as pairs
// in any order, and a List as a flat sequence.
run!(
    hbs_renders_by_type,
    r#"{
    let t = "{{#each p}}{{this.[0]}}={{this.[1]}};{{/each}}{{#each l}}<{{this}}>{{/each}}";
    let sorted: Array<(string, i64)> = [("a", 1), ("b", 2)];
    let unsorted: Array<(string, i64)> = [("b", 1), ("a", 2)];
    let l = [<"x", "y">];
    (hbs::render(t, {p: sorted, l})$, hbs::render(t, {p: unsorted, l})$)
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            a.len() == 2
                && a[0] == Value::String(literal!("a=1;b=2;<x><y>"))
                && a[1] == Value::String(literal!("b=1;a=2;<x><y>"))
        }
        _ => false,
    }
);

// Partials that expand inside one another without end are refused at a
// depth bound, whatever the shape of the cycle; a data-bounded recursion
// renders.
run!(hbs_partial_cycle_refused, r#"{
    let cycle = hbs::render(#partials: {a: "x{{> b}}", b: "y{{> a}}"}, "{{> a}}", {x: 1});
    let inline = hbs::render("{{#*inline \"p\"}}{{> p}}{{/inline}}{{> p}}", {x: 1});
    let block = hbs::render(#partials: {a: "{{#if x}}{{> a}}{{/if}}"}, "{{> a}}", {x: 1});
    "[cycle]|[inline]|[block]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.matches("templates nest more than 128 deep").count() == 3)
}; FuseExpect::Jit);

// Blocks count toward the bound as partials do, and a template whose
// text nests its blocks past a cap is refused before it is built: neither
// overflows the stack (x-panics-03).
run!(hbs_block_nesting_refused, r#"{
    let nest = |k: i64, inner: string| str::concat(
        str::join(#sep: "", array::init(k, |i| "{{#if a}}")),
        inner,
        str::join(#sep: "", array::init(k, |i| "{{/if}}"))
    );
    let p = nest(12, "{{> q}}");
    let q = nest(12, "{{> p}}");
    let mutual = hbs::render(#partials: {p, q}, "{{> p}}", {a: true});
    let deep = hbs::render(nest(5000, "x"), {a: false});
    let fine = hbs::render(nest(60, "x"), {a: true});
    let repeat = |k: i64, s: string| str::join(#sep: "", array::init(k, |i| s));
    let chain = hbs::render(
        str::concat("{{#if a}}x", repeat(5000, "{{else if a}}y"), "{{/if}}"),
        {a: false}
    );
    let commented = hbs::render(
        str::concat(repeat(5000, "{{#if a}}{{!-- {{/if}} --}}"), "x", repeat(5000, "{{/if}}")),
        {a: false}
    );
    let quoted = hbs::render(
        str::concat(repeat(5000, "{{#if a}}{{lookup this \"{{/if}}\"}}"), "x", repeat(5000, "{{/if}}")),
        {a: false}
    );
    let short = hbs::render(
        str::concat("{{#if a}}x", repeat(30, "{{else if a}}y"), "{{else if b}}z{{else}}w{{/if}}"),
        {a: false, b: true}
    );
    "[mutual]|[deep]|[fine]|[chain]|[commented]|[quoted]|[short]"
}"#, |v: Result<&Value>| {
    let nests = |s: &str| s.contains("blocks and subexpressions nest more than 64 deep");
    matches!(v, Ok(Value::String(s)) if matches!(s.split('|').collect::<Vec<_>>()[..],
        [mutual, deep, "x", chain, commented, quoted, "z"]
            if mutual.contains("templates nest more than 128 deep")
            && nests(deep) && nests(chain) && nests(commented) && nests(quoted)))
}; FuseExpect::Jit);

// The bound is measured on the template handlebars builds, so text it
// reads as no tag (`\{{{{` escapes whole, a path key `[..]` may hold
// `}}`) closes nothing, and subexpressions nested in one tag count as
// blocks do. Compiling a deep text answers rather than overflowing the
// stack (small-pkgs-01, x-rerun-5-05).
run!(hbs_text_nesting_refused, r##"{
    let repeat = |k: i64, s: string| str::join(#sep: "", array::init(k, |i| s));
    let nest = |k: i64, open: string, inner: string, close: string|
        str::concat(repeat(k, open), inner, repeat(k, close));
    let escaped = |k: i64|
        hbs::render(nest(k, r"{{#if a}}\{{{{/if}}", "x", "{{/if}}"), {a: true});
    let keyed = |k: i64|
        hbs::render(nest(k, r"{{#if a}}{{x [}}{{/if}}]}}", "x", "{{/if}}"), {a: true});
    let sub = |k: i64| hbs::render(
        str::concat("{{lookup ", nest(k, "(lookup ", "a", r#" "x")"#), r#" "x"}}"#),
        {a: 1}
    );
    let fine = sub(64);
    "[escaped(100)]|[escaped(5000)]|[keyed(100)]|[keyed(5000)]|[sub(65)]|[sub(5000)]|<[fine]>"
}"##, |v: Result<&Value>| {
    let nests = |s: &str| s.contains("blocks and subexpressions nest more than 64 deep");
    matches!(v, Ok(Value::String(s)) if matches!(s.split('|').collect::<Vec<_>>()[..],
        [e1, e2, k1, k2, s1, s2, "<>"]
            if [e1, e2, k1, k2, s1, s2].into_iter().all(nests)))
}; FuseExpect::Jit);

run!(hbs_partial_tree_renders, r#"{
    type Node = {name: string, kids: Array<Node>};
    let leaf = |name: string| -> Node {name, kids: []};
    let tree: Node = {name: "a", kids: [{name: "b", kids: [leaf("c")]}, leaf("d")]};
    hbs::render(#partials: {node: "{{name}}({{#each kids}}{{> node}}{{/each}})"}, "{{> node}}", tree)$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a(b(c())d())")
}; FuseExpect::Jit);

// A refusal names the value as Graphix prints it, never by its runtime
// form (gx-stdlib-02).
run!(hbs_refusals_print_values, r#"{
    let tuple = hbs::render("x", (1, 2));
    let variant = hbs::render(#partials: `A(1), "x", {a: 1});
    let partial = hbs::render(#partials: {p: (1, "q")}, "x", {a: 1});
    "[tuple]|[variant]|[partial]"
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == concat!(
        "error:`HbsErr(\"the data must be a struct or a map, not (1, 2)\")",
        "|error:`HbsErr(\"partials must be a struct, map, or null, got `A(1)\")",
        "|error:`HbsErr(\"partial values must be strings, got (1, \\\"q\\\")\")"
    ))
}; FuseExpect::Jit);

// The partials are data, as the data is: the check refuses a reference or
// a function there (gx-stdlib-05).
run!(hbs_partials_reference_refused, r#"{
    let p = "P";
    hbs::render(#partials: &p, "hi {{a}}", {a: 1})
}"#, refused("&string is not data"); FuseExpect::None);

run!(hbs_partials_function_refused, r#"
    hbs::render(#partials: {p: |x| x}, "hi {{a}}", {a: 1})
"#, refused("is not data: data holds no function and no reference"); FuseExpect::None);
