use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

run!(hbs_basic, r#"{
    let r = hbs::render("Hello {{name}}!", {name: "world"})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Hello world!")
});

run!(hbs_struct_fields, r#"{
    let r = hbs::render("{{first}} {{last}}", {first: "Alice", last: "Smith"})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Alice Smith")
});

run!(hbs_nested, r#"{
    let r = hbs::render("{{person.name}} is {{person.age}}", {person: {name: "Bob", age: 30}})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Bob is 30")
});

run!(hbs_each, r#"{
    let r = hbs::render("{{#each items}}{{this}} {{/each}}", {items: ["a", "b", "c"]})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a b c ")
});

run!(hbs_if, r#"{
    let r = hbs::render("{{#if show}}visible{{else}}hidden{{/if}}", {show: true})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "visible")
});

run!(hbs_if_false, r#"{
    let r = hbs::render("{{#if show}}visible{{else}}hidden{{/if}}", {show: false})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hidden")
});

run!(hbs_partials, r#"{
    let r = hbs::render(
        #partials: {header: "<h1>{{title}}</h1>"},
        "{{> header}} body",
        {title: "Hi"}
    )$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "<h1>Hi</h1> body")
});

run!(hbs_strict_missing, r#"{
    let r = hbs::render(#strict: true, "{{missing}}", {x: 1});
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(hbs_invalid_template, r#"{
    let r = hbs::render("{{#if}}", {x: 1});
    is_err(r)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

run!(hbs_numeric_data, r#"{
    let r = hbs::render("count: {{n}}", {n: 42})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "count: 42")
});

run!(hbs_non_strict_missing, r#"{
    let r = hbs::render("hello {{missing}}", {x: 1})$;
    r
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello ")
});
