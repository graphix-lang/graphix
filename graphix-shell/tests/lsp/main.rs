//! End-to-end LSP tests: the real server and backend, driven as an
//! editor would.

mod harness;

use harness::Client;

const MAIN: &str = "\
mod util;
use util::{Shape, area};

type Point = { x: f64, y: f64 };
let x = 1;
let f = |x: i64| -> i64 {
    let y = x + 1;
    y * 2
};
let z = f(x) + util::bump(3);
let p: Point = { x: 1.0, y: 2.0 };
let s: Shape = `Circle(2.0);
let total = area(s) + p.x;
let names = array::map([1, 2, 3], |n| n + z);
println(\"[total] [names]\")
";

const UTIL: &str = "\
type Shape = [`Circle(f64), `Square(f64)];
let bump = |n: i64| -> i64 n + 1;
let area = |s: Shape| -> f64 select s {
    `Circle(r) => r * r * 3.14,
    `Square(w) => w * w
};
";

fn two_files() -> Client {
    let mut c = Client::start(&[("main.gx", MAIN), ("util.gx", UTIL)]);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    c
}

fn hover_is(c: &mut Client, marker: &str, expected: &str) {
    let got = c.hover("main.gx", marker);
    let sig = got.as_deref().and_then(|h| h.lines().nth(1));
    assert_eq!(sig, Some(expected), "hover at `{marker}`: {got:?}");
}

#[test]
fn hover() {
    let mut c = two_files();
    hover_is(&mut c, "let |x = 1", "x: i64");
    hover_is(&mut c, "let y = |x + 1", "x: i64");
    hover_is(&mut c, "|y * 2", "y: i64");
    hover_is(&mut c, "|f(x)", "f: fn(x: i64) -> i64");
    hover_is(&mut c, "util::|bump", "util::bump: fn(n: i64) -> i64");
    hover_is(&mut c, "let p: |Point", "type Point = { x: f64, y: f64 }");
    hover_is(&mut c, "|n + z", "n: i64");
    hover_is(&mut c, "[|total]", "total: f64");
    hover_is(&mut c, "let |z", "z: i64");
}

#[test]
fn hover_never_answers_for_another_name() {
    let mut c = two_files();
    assert_eq!(
        c.hover("main.gx", "mod |util").as_deref(),
        Some("```graphix\nmod util\n```")
    );
    let field = c.hover("main.gx", "p.|x");
    assert!(!field.as_deref().unwrap_or("").contains("i64"), "{field:?}");
}

#[test]
fn definition() {
    let mut c = two_files();
    let def = |c: &mut Client, m: &str| c.definition("main.gx", m);
    assert_eq!(def(&mut c, "f(|x)"), Some(c.site("main.gx", "let |x = 1")));
    assert_eq!(def(&mut c, "let y = |x + 1"), Some(c.site("main.gx", "|x: i64")));
    assert_eq!(def(&mut c, "|x: i64"), Some(c.site("main.gx", "|x: i64")));
    assert_eq!(def(&mut c, "|y * 2"), Some(c.site("main.gx", "let |y")));
    assert_eq!(def(&mut c, "util::|bump"), Some(c.site("util.gx", "let |bump")));
    assert_eq!(def(&mut c, "|area(s)"), Some(c.site("util.gx", "let |area")));
    assert_eq!(def(&mut c, "let s: |Shape"), Some(c.site("util.gx", "type |Shape")));
    assert_eq!(def(&mut c, "mod |util"), Some(("util.gx".into(), 0, 0)));
    assert_eq!(def(&mut c, "use |util"), Some(("util.gx".into(), 0, 0)));
    assert_ne!(def(&mut c, "p.|x"), Some(c.site("main.gx", "let |x = 1")));
}

const TRAITS: &str = "\
type Counter = Abstract<i64>;
trait Show {
    val show: fn(self) -> string;
    val twice: fn(self) -> string = |s| \"[Show::show(s)] [Show::show(s)]\"
};
impl Show for Counter { let show = |c| \"[c.0]\" };
let c = Counter(1);
let t = Show::twice(c);
Show::show(c)
";

// A trait answers like the module its methods are in; a method like its
// own binding.
#[test]
fn traits_and_methods_answer() {
    let mut c = Client::start(&[("main.gx", TRAITS)]);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    let def = |c: &mut Client, m: &str| c.definition("main.gx", m);
    assert_eq!(def(&mut c, "Show::|show(c)"), Some(c.site("main.gx", "val |show")));
    assert_eq!(def(&mut c, "|Show::show(c)"), Some(c.site("main.gx", "trait |Show")));
    assert_eq!(def(&mut c, "impl |Show for"), Some(c.site("main.gx", "trait |Show")));
    let hover = c.hover("main.gx", "trait |Show").expect("hover at a trait");
    assert!(hover.contains("val show: fn(self) -> string"), "{hover}");
    let show = c.references("main.gx", "trait |Show");
    for m in ["trait |Show", "impl |Show for", "|Show::show(c)", "|Show::twice(c)"] {
        assert!(show.contains(&c.site("main.gx", m)), "{m} missing from {show:?}");
    }
}

const GENERIC_IMPLS: &str = "\
type Counter<'a> = Abstract<'a>;
trait Show {
    val show: fn(self) -> string
};
impl<'a> Show for Counter<'a> { let show = |c| \"x\" };
impl<'a>
Show for Array<'a> { let show = |c| \"y\" };
Show::show(Counter(1))
";

// A generic impl's trait name is found at its name, whatever space or line
// break follows the parameters.
#[test]
fn a_generic_impl_names_its_trait_where_written() {
    let mut c = Client::start(&[("main.gx", GENERIC_IMPLS)]);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    let show = c.references("main.gx", "trait |Show");
    for m in ["impl<'a> |Show for Counter", "\n|Show for Array"] {
        assert!(show.contains(&c.site("main.gx", m)), "{m} missing from {show:?}");
    }
    assert_eq!(
        c.definition("main.gx", "impl<'a> Show| for Counter"),
        Some(c.site("main.gx", "trait |Show"))
    );
}

#[test]
fn stdlib_definitions_are_not_in_the_document() {
    let mut c = two_files();
    for m in ["array::|map", "|println"] {
        let d = c.definition("main.gx", m);
        assert!(d.as_ref().is_none_or(|(f, _, _)| f != "main.gx"), "{m}: {d:?}");
        let r = c.references("main.gx", m);
        // a reference stands on the name it refers to
        assert_eq!(r, vec![c.site("main.gx", m)]);
    }
}

#[test]
fn references() {
    let mut c = two_files();
    let top_x = vec![c.site("main.gx", "let |x = 1"), c.site("main.gx", "f(|x)")];
    assert_eq!(c.references("main.gx", "f(|x)"), top_x);
    assert_eq!(c.references("main.gx", "let |x = 1"), top_x);
    let param_x = vec![c.site("main.gx", "|x: i64"), c.site("main.gx", "let y = |x + 1")];
    assert_eq!(c.references("main.gx", "let y = |x + 1"), param_x);
    assert_eq!(c.references("main.gx", "|x: i64"), param_x);
    let y = vec![c.site("main.gx", "let |y"), c.site("main.gx", "|y * 2")];
    assert_eq!(c.references("main.gx", "|y * 2"), y);
    let n = vec![c.site("main.gx", "|n| n + z"), c.site("main.gx", "|n + z")];
    assert_eq!(c.references("main.gx", "|n + z"), n);
    let bump = vec![c.site("main.gx", "util::|bump"), c.site("util.gx", "let |bump")];
    assert_eq!(c.references("main.gx", "util::|bump"), bump);
    let shape = vec![
        c.site("main.gx", "use util::{|Shape"),
        c.site("main.gx", "let s: |Shape"),
        c.site("util.gx", "type |Shape"),
        c.site("util.gx", "s: ^Shape| -> f64"),
    ];
    assert_eq!(c.references("main.gx", "let s: |Shape"), shape);
}

#[test]
fn completion() {
    let mut c = two_files();
    let at = |c: &mut Client, line: &str| {
        c.replace("main.gx", "let total", &format!("{line}\nlet total"));
        let got = c.completions("main.gx", &format!("{line}|"));
        c.edit("main.gx", MAIN);
        got
    };
    assert_eq!(at(&mut c, "let q = util::"), ["area", "bump"]);
    assert_eq!(at(&mut c, "let q = util::bu"), ["bump"]);
    assert_eq!(at(&mut c, "let q = are"), ["area"]);
    // declared after the cursor: not in scope there
    assert_eq!(at(&mut c, "let q = nam"), Vec::<String>::new());
    assert_eq!(at(&mut c, "let q = println(#"), ["#dest"]);
    assert_eq!(at(&mut c, "let q = p."), ["x", "y"]);
    c.replace("main.gx", "    y * 2", "    let w = y\n    y * 2");
    assert_eq!(c.completions("main.gx", "let w = y|"), ["y"]);
}

/// Inside a `use`, completion offers what the module under the cursor
/// exports, however deep the tree, and nothing else.
#[test]
fn a_use_completes_with_the_modules_exports() {
    let main = "mod shapes;\nuse shapes::round::{Circle, area};\narea({ r: 1.0 })\n";
    let round = "use super::sides;\ntype Circle = { r: f64 };\n\
                 let area = |c: Circle| -> f64 c.r * c.r * 3.14 + cast<f64>(sides)$;\n";
    let mut c = Client::start(&[
        ("main.gx", main),
        ("shapes.gx", "let sides = 4;\nmod round;\n"),
        ("shapes/round.gx", round),
    ]);
    c.open("main.gx");
    c.open("shapes/round.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    let at = |c: &mut Client, file: &str, original: &str, from: &str, to: &str| {
        c.replace(file, from, to);
        let got = c.completions(file, &format!("{to}|"));
        c.edit(file, original);
        got
    };
    let head = "use shapes::round::{Circle, area};";
    assert_eq!(
        at(&mut c, "main.gx", main, head, "use shapes::round::{"),
        ["Circle", "area", "self"]
    );
    assert_eq!(
        at(&mut c, "main.gx", main, head, "use shapes::round::{Circle, a"),
        ["area"]
    );
    assert_eq!(at(&mut c, "main.gx", main, head, "use shapes::"), ["round", "sides"]);
    assert_eq!(
        at(&mut c, "main.gx", main, head, "use shapes::{\n  sides,\n  round::{\n    Ci"),
        ["Circle"]
    );
    let top = at(&mut c, "main.gx", main, head, "use sha");
    assert_eq!(top, ["shapes"]);
    let up = at(&mut c, "shapes/round.gx", round, "use super::sides;", "use super::");
    assert_eq!(up, ["round", "sides"]);
    let std = at(&mut c, "main.gx", main, head, "use sys::time::");
    assert!(
        std.contains(&"timer".to_string()) && !std.contains(&"println".to_string()),
        "{std:?}"
    );
}

#[test]
fn symbols_show_resolved_types() {
    let mut c = two_files();
    let syms = c.symbols("main.gx");
    let detail =
        |n: &str| syms.iter().find(|(name, _)| name == n).and_then(|(_, d)| d.clone());
    assert_eq!(detail("z").as_deref(), Some("i64"));
    assert_eq!(detail("total").as_deref(), Some("f64"));
    assert!(syms.iter().any(|(n, _)| n == "Point"), "{syms:?}");
}

#[test]
fn an_unsaved_root_file_is_checked() {
    let mut c = two_files();
    c.replace("main.gx", "let z = f(x)", "let z = f(true)");
    let d = c.diagnostics("main.gx");
    assert_eq!(d.len(), 1, "{d:?}");
    assert_eq!((d[0].0, d[0].1), {
        let p = c.at("main.gx", "f(|true)");
        (p.line, p.character)
    });
    c.edit("main.gx", MAIN);
    assert_eq!(c.diagnostics("main.gx"), vec![]);
}

#[test]
fn an_error_is_reported_where_it_arose() {
    let mut c = Client::start(&[(
        "a.gx",
        "let g = |a: i64| {\n  let q = a + 1;\n  let r = missing + q;\n  r\n};\ng(1)\n",
    )]);
    c.open("a.gx");
    let p = c.at("a.gx", "|missing");
    assert_eq!(
        c.diagnostics("a.gx"),
        [(p.line, p.character, "missing not defined".into())]
    );
}

#[test]
fn a_diagnostic_underlines_the_expression() {
    let mut c = Client::start(&[(
        "a.gx",
        "let f = |n: i64| n + 1;\nlet bad = f(str::len(\"four\") == 4);\nbad\n",
    )]);
    c.open("a.gx");
    assert_eq!(c.underlined("a.gx"), ["str::len(\"four\") == 4"]);
}

/// A lambda's own refusals (duplicate parameters, a variadic) are placed
/// at the lambda.
#[test]
fn a_lambda_refusal_is_reported_at_the_lambda() {
    for (src, lambda) in [
        ("let f = |x, x| x;\nf(1, 2)\n", "|x, x| x"),
        ("let f = |@args: i64| args;\nf(1)\n", "|@args: i64| args"),
    ] {
        let mut c = Client::start(&[("a.gx", src)]);
        c.open("a.gx");
        assert_eq!(c.underlined("a.gx"), [lambda], "{src}");
    }
}

#[test]
fn a_dead_arm_is_reported_at_its_pattern() {
    let mut c = Client::start(&[(
        "a.gx",
        "let a = [1, 2];\nselect a { [x, tl..] => 1, [init.., y] => 2, [] => 0 }\n",
    )]);
    c.open("a.gx");
    assert_eq!(c.underlined("a.gx"), ["[init.., y]"]);
}

#[test]
fn a_runtime_form_refusal_is_reported_at_its_pattern() {
    let mut c = Client::start(&[(
        "a.gx",
        "let s = \"a\";\nlet v: [&i64, &string] = &s;\nselect v { &i64 as r => 1, &string as q => 2 }\n",
    )]);
    c.open("a.gx");
    assert_eq!(c.underlined("a.gx"), ["&i64 as r"]);
}

#[test]
fn a_constructor_error_is_reported_at_the_constructor() {
    let mut c = Client::start(&[("a.gx", "type C = i64;\nlet c = C(5);\nc\n")]);
    c.open("a.gx");
    assert_eq!(c.underlined("a.gx"), ["C(5)"]);
}

/// A package under development declares builtins this binary was not
/// built with: they are warnings, and everything else is still checked.
#[test]
fn an_unknown_builtin_is_a_warning() {
    let mut c = Client::start(&[
        ("Cargo.toml", "[package]\nname = \"graphix-package-demo\"\n"),
        (
            "src/graphix/mod.gxi",
            "val ping: fn(n: i64) -> i64;\nval twice: fn(n: i64) -> i64;\nmod sub;\n",
        ),
        // `package::` names a package this binary never registered
        (
            "src/graphix/sub.gx",
            "use package::ping;\nlet thrice = |n: i64| -> i64 ping(n) * 3\n",
        ),
        (
            "src/graphix/mod.gx",
            "let ping = |n: i64| -> i64 'demo_ping;\n\
             let twice = |n: i64| -> i64 ping(n) + ping(n);\n",
        ),
    ]);
    let f = "src/graphix/mod.gx";
    c.open(f);
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    let w = c.warnings(f);
    assert_eq!(w.len(), 1, "{w:?}");
    assert_eq!(w[0].0, "'demo_ping");
    assert!(w[0].1.starts_with("unknown builtin function demo_ping"), "{}", w[0].1);
    let h = c.hover(f, "|ping(n) +").unwrap();
    assert!(h.contains("ping: fn(n: i64) -> i64"), "{h}");
    c.replace(f, "ping(n) + ping(n)", "ping(n) + ping(\"no\")");
    assert_eq!(c.files_with_diagnostics(), [f]);
    assert_eq!(c.warnings(f).len(), 1, "a failed check keeps the last warnings");
}

#[test]
fn an_uncaught_error_is_a_warning() {
    let mut c = Client::start(&[("a.gx", "let n = cast<i64>(\"1\")?;\nn + 1\n")]);
    c.open("a.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    let w = c.warnings("a.gx");
    assert_eq!(
        w,
        [("cast<i64>(\"1\")?".into(), "error raised by ? will not be caught".into())]
    );
    c.edit("a.gx", "let n = cast<i64>(\"1\")$;\nn + 1\n");
    assert_eq!(c.warnings("a.gx"), vec![]);
}

/// An import nothing looks a name up through is a warning at the name; a
/// module's interface imports count, and so does a use through `as`.
#[test]
fn an_unused_import_is_a_warning() {
    let mut c = Client::start(&[
        ("a.gx", "mod m;\nuse array::{len, map as amap};\nlen(m::xs)\n"),
        ("m.gx", "let xs = [1, 2]"),
        ("m.gxi", "use str::len;\nuse array::len as alen;\nval xs: Array<i64>;\n"),
    ]);
    c.open("a.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    assert_eq!(c.warnings("a.gx"), [("amap".into(), "unused import `amap`".into())]);
    let mut m = c.warnings("m.gxi");
    m.sort();
    assert_eq!(
        m,
        [
            ("alen".into(), "unused import `alen`".into()),
            ("len".into(), "unused import `len`".into())
        ]
    );
    c.edit(
        "a.gx",
        "mod m;\nuse array::{len, map as amap};\namap(m::xs, |x| x + len(m::xs))\n",
    );
    assert_eq!(c.warnings("a.gx"), vec![]);
}

#[test]
fn a_field_shows_its_type() {
    let mut c = two_files();
    hover_is(&mut c, "p.|x", "x: f64");
}

/// A binding is offered once it is in scope: after its statement, or
/// inside it for a `let rec`.
#[test]
fn completion_offers_only_what_is_in_scope() {
    let mut c = Client::start(&[(
        "main.gx",
        "let x = 1;\n\
         let f = |a: i64| a + 1;\n\
         let rec r = |n: i64| -> i64 select n { 0 => 0, n => r(n - 1) };\n\
         let later = 2;\n\
         later\n",
    )]);
    c.open("main.gx");
    let got = c.completions("main.gx", "a + |1");
    let has = |n: &str| got.iter().any(|g| g == n);
    assert!(has("a") && has("x"), "{got:?}");
    assert!(!has("f") && !has("r") && !has("later"), "{got:?}");
    let got = c.completions("main.gx", "n => |r(");
    assert!(got.iter().any(|g| g == "r"), "{got:?}");
}

#[test]
fn a_block_does_not_leak_its_scope() {
    let mut c = two_files();
    c.replace("main.gx", "let z = f(x)", "let w = \nlet z = f(x)");
    let after = c.completions("main.gx", "let w = |");
    assert!(
        after.contains(&"f".to_string()) && !after.contains(&"y".to_string()),
        "{after:?}"
    );
    c.edit("main.gx", MAIN);
    c.replace("main.gx", "    y * 2", "    let w = \n    y * 2");
    let inside = c.completions("main.gx", "    let w = |");
    assert!(inside.contains(&"y".to_string()), "{inside:?}");
}

#[test]
fn an_unsaved_module_is_checked() {
    let mut c = two_files();
    c.open("util.gx");
    c.replace("util.gx", "n + 1", "n + \"no\"");
    assert_eq!(c.files_with_diagnostics(), ["util.gx"]);
    c.edit("util.gx", UTIL);
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
}

#[test]
fn a_parse_error_is_reported_where_it_is() {
    let mut c = two_files();
    c.replace("main.gx", "let total", "let q = ;\nlet total");
    let d = c.diagnostics("main.gx");
    assert_eq!(d.len(), 1, "{d:?}");
    assert_eq!(d[0].0, c.at("main.gx", "|let q").line);
}

#[test]
fn queries_answer_while_the_buffer_is_broken() {
    let mut c = two_files();
    c.replace("main.gx", "let total", "let q = ;\nlet total");
    hover_is(&mut c, "|f(x)", "f: fn(x: i64) -> i64");
    assert_eq!(c.definition("main.gx", "|y * 2"), Some(c.site("main.gx", "let |y")));
}

/// A reference on its declaration's line goes to the declaration, not
/// to the interface `val` the declaration itself goes to.
#[test]
fn a_reference_beside_its_declaration_is_a_reference() {
    let mut c = Client::start(&[
        ("main.gx", "mod api;\napi::quad(2)\n"),
        ("api.gxi", "val double: fn(n: i64) -> i64;\nval quad: fn(n: i64) -> i64;\n"),
        (
            "api.gx",
            "let double = |n: i64| -> i64 n * 2; \
             let quad = |n: i64| -> i64 double(double(n));\n",
        ),
    ]);
    c.open("api.gx");
    let decl = c.site("api.gx", "let |double");
    assert_eq!(c.definition("api.gx", "|double(double"), Some(decl));
    let val = c.site("api.gxi", "val |double");
    assert_eq!(c.definition("api.gx", "let |double"), Some(val));
}

#[test]
fn an_interface_val_and_its_implementation_are_one_symbol() {
    let mut c = Client::start(&[
        ("main.gx", "mod api;\napi::double(2)\n"),
        ("api.gxi", "/// twice n\nval double: fn(n: i64) -> i64;\n"),
        ("api.gx", "let double = |n: i64| -> i64 n * 2;\nlet four = double(2)\n"),
    ]);
    for f in ["main.gx", "api.gxi", "api.gx"] {
        c.open(f);
    }
    let val = c.site("api.gxi", "val |double");
    let imp = c.site("api.gx", "let |double");
    assert_eq!(c.definition("main.gx", "api::|double"), Some(val.clone()));
    assert_eq!(c.definition("api.gxi", "val |double"), Some(imp.clone()));
    let all = vec![
        imp.clone(),
        c.site("api.gx", "|double(2)"),
        val,
        c.site("main.gx", "api::|double"),
    ];
    assert_eq!(c.references("api.gx", "let |double"), all);
    assert_eq!(c.references("main.gx", "api::|double"), all);
    let h = c.hover("main.gx", "api::|double").unwrap();
    assert!(h.contains("twice n"), "{h}");
}

#[test]
fn a_use_names_what_it_imports() {
    let mut c = two_files();
    hover_is(&mut c, "use util::{Shape, |area}", "area: fn(s: Shape) -> f64");
    assert_eq!(
        c.definition("main.gx", "use util::{|Shape"),
        Some(c.site("util.gx", "type |Shape"))
    );
    let area = vec![
        c.site("main.gx", "use util::{Shape, |area}"),
        c.site("main.gx", "|area(s)"),
        c.site("util.gx", "let |area"),
    ];
    assert_eq!(c.references("main.gx", "|area(s)"), area);
    // a path's segment before its last names the module of its prefix
    let util = vec![
        c.site("main.gx", "mod |util"),
        c.site("main.gx", "use |util"),
        c.site("main.gx", "|util::bump"),
    ];
    assert_eq!(c.references("main.gx", "mod |util"), util);
}

#[test]
fn a_malformed_request_is_refused_and_the_server_lives() {
    let mut c = two_files();
    let refused = c.raw_request("textDocument/hover", serde_json::json!({"nope": 1}));
    assert!(refused.is_some());
    assert!(c.raw_request("graphix/unknown", serde_json::Value::Null).is_some());
    hover_is(&mut c, "|f(x)", "f: fn(x: i64) -> i64");
}

#[test]
fn every_declared_name_is_a_site() {
    let src = "\
use array::{
    fold,
    map
};
type Pair = { left: i64, right: i64 };
let (a, b) = (1, 2);
let { left, right: r } = { left: a, right: b };
let pick = |#scale: i64, p: Pair| select p {
    { left: 0, right } => right * scale,
    whole@ { left, .. } => left + whole.right
};
pick(#scale: r, { left, right: b }) + fold(map([a], |x| x), 0, |acc, x| acc + x)
";
    let mut c = Client::start(&[("a.gx", src)]);
    c.open("a.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    let mut is = |decl: &str, sig: &str, uses: &[&str]| {
        let h = c.hover("a.gx", decl);
        assert_eq!(h.as_deref().and_then(|h| h.lines().nth(1)), Some(sig), "{decl}");
        let mut sites = vec![c.site("a.gx", decl)];
        sites.extend(uses.iter().map(|u| c.site("a.gx", u)));
        sites.sort();
        assert_eq!(c.references("a.gx", decl), sites, "{decl}");
        for u in uses {
            assert_eq!(c.definition("a.gx", u), Some(c.site("a.gx", decl)), "{u}");
        }
    };
    is("let (a, |b)", "b: i64", &["right: |b }", "right: |b })"]);
    is("let { |left,", "left: i64", &["pick(#scale: r, { |left"]);
    is("right: |r }", "r: i64", &["#scale: |r,"]);
    is("|#^scale: i64", "scale: i64", &["right * |scale"]);
    is("{ left: 0, |right }", "right: i64", &["=> |right * scale"]);
    is("|whole@", "whole: { left: i64, right: i64 }", &["left + |whole"]);
    is("|^acc, x| acc", "acc: i64", &["|acc + x"]);
    assert!(c.hover("a.gx", "    |map\n").unwrap().contains("map: fn("));
    assert!(c.hover("a.gx", "|array::{").unwrap().contains("mod array"));
}

#[test]
fn a_save_redraws_the_project_graph() {
    let mut c = Client::start(&[("main.gx", "let a = 1"), ("helper.gx", "let h = 1")]);
    c.open("main.gx");
    c.open("helper.gx");
    c.edit("main.gx", "mod helper;\nhelper::h + 1");
    c.save("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    assert_eq!(
        c.definition("main.gx", "helper::|h"),
        Some(c.site("helper.gx", "let |h"))
    );
    c.edit("helper.gx", "let h = \"one\"");
    assert_eq!(c.files_with_diagnostics(), ["main.gx"]);
}

/// Saving an open file re-checks only what reads it from disk: not
/// another project, and not its own (which reads the buffer).
#[test]
fn a_save_rechecks_only_what_it_changes() {
    let mut c = Client::start(&[
        ("a.gx", "let a: string = 1;\na\n"),
        ("b.gx", "let b: string = 1;\nb\n"),
    ]);
    c.open("a.gx");
    c.open("b.gx");
    let (a, b) = (c.publishes("a.gx"), c.publishes("b.gx"));
    assert!(a > 0 && b > 0);
    c.save("b.gx");
    assert_eq!((c.publishes("a.gx"), c.publishes("b.gx")), (a, b));
    // a module no one has open is read again from disk
    let mut c = Client::start(&[
        ("main.gx", "mod util;\nlet x: string = util::v;\nx\n"),
        ("util.gx", "let v = 1;\n"),
    ]);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), ["main.gx"]);
    c.disk_changed("util.gx", "let v = \"one\";\n");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
}

/// A check that fails however it fails, a checker panic included,
/// leaves the server checking every other file.
#[test]
fn a_failed_check_leaves_the_server_running() {
    let mut c = Client::start(&[
        ("bad.gx", "let g = |a: i64, f: fn() -> i64| -> i64 'array_group;\n0\n"),
        ("main.gx", "let x = 1;\nx + 1\n"),
    ]);
    c.open("bad.gx");
    assert_eq!(c.diagnostics("bad.gx").len(), 1);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), ["bad.gx"]);
    hover_is(&mut c, "|x + 1", "x: i64");
}

#[test]
fn closing_a_file_clears_its_diagnostics() {
    let mut c = Client::start(&[("a.gx", "let a = 1"), ("b.gx", "let b = 1")]);
    c.open("b.gx");
    c.edit("b.gx", "let b: string = 1");
    assert_eq!(c.files_with_diagnostics(), ["b.gx"]);
    c.close("b.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
}

/// A package root's interface declares types and uses the impl sees.
#[test]
fn a_package_root_is_checked_with_its_interface() {
    let mut c = Client::start(&[
        ("Cargo.toml", "[package]\nname = \"graphix-package-demo\"\n"),
        (
            "src/graphix/mod.gxi",
            "mod shapes;\nuse self::shapes::Shape;\ntype Dir = [`Up, `Down];\n\
             val flip: fn(d: Dir) -> Dir;\nval unit: fn() -> Shape;\n",
        ),
        (
            "src/graphix/mod.gx",
            "let flip = |d: Dir| -> Dir select d { `Up => `Down, `Down => `Up };\n\
             let unit = || -> Shape `Circle(1.0);\n",
        ),
        ("src/graphix/shapes.gxi", "type Shape = [`Circle(f64)];\n"),
        ("src/graphix/shapes.gx", "let unused = 0\n"),
    ]);
    c.open("src/graphix/mod.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    assert_eq!(
        c.definition("src/graphix/mod.gx", "d: ^Dir| -> Dir"),
        Some(c.site("src/graphix/mod.gxi", "type |Dir")),
    );
}

/// The stdlib's own packages check clean over the copy compiled in.
#[test]
fn stdlib_packages_check() {
    let stdlib = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../stdlib");
    let mut checked = 0;
    for entry in std::fs::read_dir(stdlib).unwrap() {
        let entry = entry.unwrap();
        let name = entry.file_name().into_string().unwrap();
        // a package is a crate; a directory a sync tool left behind is not
        if !name.starts_with("graphix-package-")
            || !entry.path().join("Cargo.toml").exists()
        {
            continue;
        }
        let mut c = Client::start_in_repo(&format!("stdlib/{name}"));
        c.open("src/graphix/mod.gx");
        assert_eq!(c.files_with_diagnostics(), Vec::<String>::new(), "{name}");
        // only a finished check answers this; a clean check publishes nothing
        assert!(!c.symbols("src/graphix/mod.gx").is_empty(), "{name}: no symbols");
        checked += 1;
    }
    assert!(checked >= 19, "{checked}");
}

#[test]
fn a_place_field_shows_its_type() {
    let src = "\
type P = { a: { b: i64 } };
let s: P = { a: { b: 1 } };
let r = &s.a.b;
*r
";
    let mut c = Client::start(&[("main.gx", src)]);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    hover_is(&mut c, "&s.a.|b", "b: i64");
    hover_is(&mut c, "&s.|a", "a: { b: i64 }");
}

/// A file two projects share stands with what either finds on it, and
/// keeps one's error when the other's root closes.
#[test]
fn a_file_two_projects_share_keeps_what_either_finds() {
    let tool = "mod util;\nutil::g\n";
    let mut c = Client::start(&[
        ("util.gx", "let g = 1 + \"no\";\n"),
        ("tool_a.gx", tool),
        ("tool_b.gx", tool),
    ]);
    c.open("tool_a.gx");
    c.open("tool_b.gx");
    assert_eq!(c.diagnostics("util.gx").len(), 1, "both roots' error, once");
    c.close("tool_b.gx");
    assert_eq!(c.diagnostics("util.gx").len(), 1, "tool_a still fails on it");
}

/// A root that a save makes a module is retired, and what its own checks
/// left on its files goes with it.
#[test]
fn a_root_that_becomes_a_module_is_retired() {
    let mut c = Client::start(&[
        ("main.gx", "let cfg = 1;\ncfg\n"),
        ("helper.gx", "use super::cfg;\nlet h = cfg + 1;\n"),
    ]);
    c.open("helper.gx");
    c.open("main.gx");
    assert_eq!(c.diagnostics("helper.gx").len(), 1, "helper.gx alone has no `super`");
    c.edit("main.gx", "let cfg = 1;\nmod helper;\nhelper::h\n");
    c.save("main.gx");
    assert_eq!(c.diagnostics("helper.gx"), vec![]);
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
}

/// A file a watcher reports changed on disk rescans the project graph as
/// a save does.
#[test]
fn a_watched_change_rescans() {
    let mut c = Client::start(&[
        ("main.gx", "let cfg = 1;\ncfg\n"),
        ("helper.gx", "use super::cfg;\nlet h = cfg + 1;\n"),
    ]);
    c.open("helper.gx");
    assert_eq!(c.diagnostics("helper.gx").len(), 1);
    c.disk_changed("main.gx", "let cfg = 1;\nmod helper;\nhelper::h\n");
    assert_eq!(c.diagnostics("helper.gx"), vec![]);
}

/// An interface written before its implementation has its syntax
/// checked, and a clean one publishes nothing.
/// A parse error's message says what went wrong; its squiggle shows
/// where, so no position or caret line rides in the text.
#[test]
fn a_parse_error_is_published_without_its_snippet() {
    let mut c = Client::start(&[("main.gx", "let x = 1;\nlet y = ;\nx\n")]);
    c.open("main.gx");
    let d = c.diagnostics("main.gx");
    assert_eq!(d.len(), 1, "{d:?}");
    let (line, _, msg) = &d[0];
    assert_eq!(*line, 1, "{msg}");
    assert!(!msg.contains('^') && !msg.contains("Parse error at"), "{msg}");
}

#[test]
fn a_lone_interface_reports_its_parse_error() {
    let mut c = Client::start(&[("api.gxi", "val x: i64 = ;\n")]);
    c.open("api.gxi");
    assert_eq!(c.diagnostics("api.gxi").len(), 1);
    let mut c = Client::start(&[("api.gxi", "val x: i64;\n")]);
    c.open("api.gxi");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
}

/// An interface is checked with its implementation, never as a program
/// of its own.
#[test]
fn an_interface_alone_is_checked_with_its_implementation() {
    let mut c = Client::start(&[
        ("api.gx", "let double = |n: i64| -> i64 n * 2;\n"),
        ("api.gxi", "/// twice n\nval double: fn(n: i64) -> i64;\n"),
    ]);
    c.open("api.gxi");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    // and a mismatch is reported on the implementation
    let mut c = Client::start(&[
        ("api.gx", "let double = |n: i64| -> i64 n * 2;\n"),
        ("api.gxi", "/// twice n\nval double: fn(n: i64) -> string;\n"),
    ]);
    c.open("api.gxi");
    assert_eq!(c.files_with_diagnostics(), vec!["api.gx".to_string()]);
}

#[test]
fn a_shebang_script_checks() {
    let mut c =
        Client::start(&[("main.gx", "#!/usr/bin/env graphix\nlet x = 1;\nx + 1\n")]);
    c.open("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    hover_is(&mut c, "|x + 1", "x: i64");
}

/// A path's segment before its last names the module of its prefix.
#[test]
fn a_path_segment_names_its_module() {
    let mut c = two_files();
    let h = c.hover("main.gx", "|util::bump").unwrap();
    assert!(h.contains("mod "), "{h}");
    assert_eq!(c.definition("main.gx", "|util::bump"), Some(("util.gx".into(), 0, 0)));
}

/// A symbol's range spans its declaration, so a cursor inside a body is
/// inside the symbol.
#[test]
fn a_symbol_spans_its_declaration() {
    let mut c = Client::start(&[("util.gx", UTIL)]);
    c.open("util.gx");
    let spans = c.symbol_spans("util.gx");
    let bump = spans.iter().find(|(n, _)| n == "bump").unwrap();
    assert_eq!(bump.1, "let bump = |n: i64| -> i64 n + 1");
}

/// Columns are UTF-16 units: a name after non-ASCII and astral text on
/// its line is found where the client counts it.
#[test]
fn columns_count_utf16_units() {
    let mut c = Client::start(&[("main.gx", "let s = \"é𝒜\"; let t = s;\nt\n")]);
    c.open("main.gx");
    assert_eq!(c.definition("main.gx", "let t = |s"), Some(c.site("main.gx", "let |s")));
    hover_is(&mut c, "let |t", "t: string");
}

#[test]
fn an_interface_mismatch_is_reported_where_it_is() {
    let mut c = Client::start(&[
        ("main.gx", "mod api;\napi::x\n"),
        ("api.gxi", "val x: i64;\nval y: i64;\n"),
        ("api.gx", "let x = \"s\"\n"),
    ]);
    for f in ["main.gx", "api.gxi", "api.gx"] {
        c.open(f);
    }
    let d = c.diagnostics("api.gx");
    assert_eq!(d.len(), 1, "{d:?}");
    assert_eq!(d[0].0, c.at("api.gx", "|let x").line);
    assert!(d[0].2.contains("val x"), "{d:?}");
    c.edit("api.gx", "let x = 1\n");
    let d = c.diagnostics("api.gxi");
    assert_eq!(d.len(), 1, "{d:?}");
    assert_eq!(d[0].0, c.at("api.gxi", "|val y").line);
}

#[test]
fn a_lone_module_is_checked_against_its_interface() {
    let mut c =
        Client::start(&[("api.gxi", "val x: string;\n"), ("api.gx", "let x = 1\n")]);
    c.open("api.gx");
    let d = c.diagnostics("api.gx");
    assert_eq!(d.len(), 1, "{d:?}");
    assert!(d[0].2.contains("val x"), "{d:?}");
    c.edit("api.gx", "let x = \"s\"\n");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
}
