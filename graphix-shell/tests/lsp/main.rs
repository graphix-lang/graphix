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
    assert_eq!(at(&mut c, "let q = nam"), ["names"]);
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
    let mut at = |c: &mut Client, file: &str, original: &str, from: &str, to: &str| {
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

#[test]
fn a_field_shows_its_type() {
    let mut c = two_files();
    hover_is(&mut c, "p.|x", "x: f64");
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
