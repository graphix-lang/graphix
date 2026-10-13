# hbs

The `hbs` module provides [Handlebars](https://handlebarsjs.com/) template
rendering.

```graphix
{{#include ../../../stdlib/graphix-package-hbs/src/graphix/mod.gxi}}
```

## Example

```graphix

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
