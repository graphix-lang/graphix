use anyhow::Result;
use arcstr::literal;
use graphix_package_core::{run, testing::FuseExpect};
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
    matches!(v, Ok(Value::String(s)) if s.matches("partials nest more than 128 deep").count() == 3)
}; FuseExpect::Jit);

run!(hbs_partial_tree_renders, r#"{
    type Node = {name: string, kids: Array<Node>};
    let leaf = |name: string| -> Node {name, kids: []};
    let tree: Node = {name: "a", kids: [{name: "b", kids: [leaf("c")]}, leaf("d")]};
    hbs::render(#partials: {node: "{{name}}({{#each kids}}{{> node}}{{/each}})"}, "{{> node}}", tree)$
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a(b(c())d())")
}; FuseExpect::Jit);
