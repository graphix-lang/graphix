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

#[test]
fn stdlib_definitions_are_not_in_the_document() {
    let mut c = two_files();
    for m in ["array::|map", "|println"] {
        let d = c.definition("main.gx", m);
        assert!(d.as_ref().is_none_or(|(f, _, _)| f != "main.gx"), "{m}: {d:?}");
        let r = c.references("main.gx", m);
        assert_eq!(r, vec![c.site("main.gx", &m.replace("array::|", "|array::"))]);
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
    let bump = vec![c.site("main.gx", "|util::bump"), c.site("util.gx", "let |bump")];
    assert_eq!(c.references("main.gx", "util::|bump"), bump);
    let shape = vec![
        c.site("main.gx", "use util::{|Shape"),
        c.site("main.gx", "let s: |Shape"),
        c.site("util.gx", "type |Shape"),
        c.site("util.gx", "s: |Shape| -> f64"),
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
    assert_eq!(at(&mut c, "let q = nam"), ["names"]);
    assert_eq!(at(&mut c, "let q = println(#"), ["#dest"]);
    assert_eq!(at(&mut c, "let q = p."), ["x", "y"]);
    c.replace("main.gx", "    y * 2", "    let w = y\n    y * 2");
    assert_eq!(c.completions("main.gx", "let w = y|"), ["y"]);
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
        c.site("main.gx", "|api::double"),
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
    let util = vec![c.site("main.gx", "mod |util"), c.site("main.gx", "use |util")];
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
fn a_save_redraws_the_project_graph() {
    let mut c = Client::start(&[("main.gx", "let a = 1"), ("helper.gx", "let h = 1")]);
    c.open("main.gx");
    c.open("helper.gx");
    c.edit("main.gx", "mod helper;\nhelper::h + 1");
    c.save("main.gx");
    assert_eq!(c.files_with_diagnostics(), Vec::<String>::new());
    assert_eq!(c.definition("main.gx", "helper::|h"), Some(c.site("helper.gx", "let |h")));
    c.edit("helper.gx", "let h = \"one\"");
    assert_eq!(c.files_with_diagnostics(), ["main.gx"]);
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
        c.definition("src/graphix/mod.gx", "d: |Dir| -> Dir"),
        Some(c.site("src/graphix/mod.gxi", "type |Dir")),
    );
}

/// The stdlib's own packages check clean over the copy compiled in.
#[test]
fn stdlib_packages_check() {
    let stdlib = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../stdlib");
    let mut checked = 0;
    for entry in std::fs::read_dir(stdlib).unwrap() {
        let name = entry.unwrap().file_name().into_string().unwrap();
        if !name.starts_with("graphix-package-") {
            continue;
        }
        let mut c = Client::start_in_repo(&format!("stdlib/{name}"));
        c.open("src/graphix/mod.gx");
        assert_eq!(c.files_with_diagnostics(), Vec::<String>::new(), "{name}");
        checked += 1;
    }
    assert!(checked >= 20, "{checked}");
}
