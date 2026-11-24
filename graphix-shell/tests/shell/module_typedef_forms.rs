//! A `let` annotated with a typedef its module declares above it has the
//! same type wherever the module is declared: at the script's top, in a
//! block, one level down or two. A module the root script declares is no
//! exception, in what it refuses or accepts and in what it prints.

use anyhow::Result;
use graphix_compiler::expr::Load;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::fs;

async fn check(files: &[(&str, &str)]) -> Result<Option<String>> {
    let dir = tempfile::tempdir()?;
    for (name, text) in files {
        let path = dir.path().join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, text)?;
    }
    let checked = ShellBuilder::<NoExt>::default()
        .cache(CacheMode::Off)
        .mode(Mode::Check(Load::File(dir.path().join("main.gx"))))
        .build()?
        .check()
        .await;
    Ok(checked.err().map(|e| format!("{e:#}")))
}

const STRUCT_MOD: &str =
    "type H = {h: i64};\nlet s0: H = {h: 1};\nlet z: string = s0;\nz\n";

#[tokio::test(flavor = "multi_thread")]
async fn a_typedef_annotation_prints_alike_wherever_its_module_is_declared() -> Result<()>
{
    let layouts: [(&str, Vec<(&str, &str)>); 4] = [
        ("script", vec![("main.gx", STRUCT_MOD)]),
        ("root module", vec![("main.gx", "mod m;\nm::z\n"), ("m.gx", STRUCT_MOD)]),
        ("block module", vec![("main.gx", "{ mod m; m::z }\n"), ("m.gx", STRUCT_MOD)]),
        (
            "nested module",
            vec![
                ("main.gx", "mod a;\na::m::z\n"),
                ("a.gx", "mod m;\n"),
                ("a/m.gx", STRUCT_MOD),
            ],
        ),
    ];
    for (name, files) in layouts {
        let e = check(&files).await?.expect("a string was given a struct");
        assert!(e.contains("string does not contain H"), "{name}: {e}");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_pattern_over_a_typedef_declared_above_is_accepted_in_a_root_module()
-> Result<()> {
    let m = "type P = (i64, i64);\nlet (a, b): P = (1, 2);\na\n";
    let e = check(&[("main.gx", "mod m;\nm::a\n"), ("m.gx", m)]).await?;
    assert!(e.is_none(), "{e:?}");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_pattern_over_a_typedef_declared_below_checks_in_a_root_module() -> Result<()> {
    let m = "let (a, b): P = (1, 2);\ntype P = (i64, i64);\na\n";
    let e = check(&[("main.gx", "mod m;\nm::a\n"), ("m.gx", m)]).await?;
    assert!(e.is_none(), "{e:?}");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_function_held_through_a_typedef_is_judged_alike_in_a_root_module() -> Result<()>
{
    let m = "type Opt<'a> = ['a, null];\nlet i = 0;\nlet j = 0;\n\
             let o1: Opt<fn(?#d: 'z, x: i64) -> string> =\n    \
             select j { 0 => |#d = 1, x: i64| cast<string>(d)$, _ => null };\n\
             let o2: Opt<fn(?#d: 'w, x: i64) -> string> =\n    \
             select j { 0 => |#d = \"q\", x: i64| cast<string>(d)$, _ => null };\n\
             let o = select i { 0 => o1, _ => o2 };\n\
             let r = select o { null as _ => \"n\", f => f(7) };\nr\n";
    for files in [vec![("main.gx", "mod m;\nm::r\n"), ("m.gx", m)], vec![("main.gx", m)]]
    {
        let e = check(&files).await?;
        assert!(e.is_none(), "{e:?}");
    }
    let wrong = "type Opt<'a> = ['a, null];\nlet i = 0;\nlet j = 0;\n\
             let o1: Opt<fn(?#d: 'z, x: i64) -> i64> =\n    \
             select j { 0 => |#d = 1, x: i64| d + x, _ => null };\n\
             let o2: Opt<fn(?#d: 'w, x: i64) -> i64> =\n    \
             select j { 0 => |#d = \"q\", x: i64| str::len(d) + x, _ => null };\n\
             let o = select i { 0 => o1, _ => o2 };\n\
             let r = select o { null as _ => 0, f => f(#d: \"s\", 7) };\nr\n";
    let e = check(&[("main.gx", "mod m;\nm::r\n"), ("m.gx", wrong)])
        .await?
        .expect("a default of another type passed through a value was accepted");
    assert!(e.contains("i64 does not contain string"), "{e}");
    Ok(())
}
