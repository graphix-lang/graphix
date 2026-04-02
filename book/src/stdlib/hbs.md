# hbs

The `hbs` module provides [Handlebars](https://handlebarsjs.com/) template
rendering.

```graphix
/// Render a Handlebars template with the given data context.
/// Use #partials to register named partial templates (as a struct or map).
/// Use #strict to error on missing variables instead of rendering empty strings.
val render: fn(?#strict: bool, ?#partials: 'a, string, 'b) -> Result<string, `HbsErr(string)>;
```

## Example

```graphix
use hbs;

let greeting = hbs::render(
    "Hello, {{name}}! You have {{count}} messages.",
    {name: "Alice", count: 5}
)?;

// with partials
let page = hbs::render(
    #partials: {header: "<h1>{{title}}</h1>"},
    "{{> header}}{{body}}",
    {title: "Welcome", body: "Content here"}
)?;
```
