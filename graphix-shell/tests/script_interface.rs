//! A script with a `.gxi` beside it runs as a script, and its interface
//! is checked over its top-level names: a `val` the script does not bind
//! as declared, or an impl it does not implement, is refused by the run
//! as by `--check`.

use anyhow::Result;
use graphix_compiler::expr::Load;
use graphix_package::MainThreadHandle;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::fs;

async fn verdicts(gx: &str, gxi: &str) -> Result<[Option<String>; 2]> {
    let dir = tempfile::tempdir()?;
    let file = dir.path().join("prog.gx");
    fs::write(&file, gx)?;
    fs::write(dir.path().join("prog.gxi"), gxi)?;
    let shell =
        |mode| ShellBuilder::<NoExt>::default().cache(CacheMode::Off).mode(mode).build();
    let check = shell(Mode::Check(Load::File(file.clone())))?.check().await;
    let run = shell(Mode::Script(Load::File(file.clone())))?
        .run(MainThreadHandle::new().0)
        .await
        .map(|_| ());
    Ok([check, run].map(|r| r.err().map(|e| format!("{e:#}"))))
}

#[tokio::test(flavor = "multi_thread")]
async fn val_declared_otherwise_is_refused() -> Result<()> {
    let [check, run] = verdicts(
        "let f = |x: i64| -> string \"[x]\";\nf(1)\n",
        "val f: fn(x: i64) -> i64;\n",
    )
    .await?;
    for e in [check, run] {
        let e = e.expect("an implementation that does not match was accepted");
        assert!(e.contains("val f is declared fn(x: i64) -> i64 but implemented"), "{e}");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn missing_items_are_refused() -> Result<()> {
    let [check, run] = verdicts(
        "let f = |x: i64| x + 1;\nf(1)\n",
        "val f: fn(x: i64) -> i64;\nval g: fn(x: i64) -> i64;\n\
         trait Sh { val sh: fn(self) -> string };\nimpl Sh for i64;\n",
    )
    .await?;
    for e in [check, run] {
        let e = e.expect("a missing val was accepted");
        assert!(
            e.contains("val g: fn(x: i64) -> i64 is missing an implementation"),
            "{e}"
        );
    }
    let [check, run] = verdicts(
        "let x = 1;\nx\n",
        "trait Sh { val sh: fn(self) -> string };\nimpl Sh for i64;\n",
    )
    .await?;
    for e in [check, run] {
        let e = e.expect("a missing impl was accepted");
        assert!(e.contains("impl Sh for i64 is missing an implementation"), "{e}");
    }
    Ok(())
}

// A script's own definition of a type or trait its interface defines is
// refused, as a module's is, and so is an impl that contradicts it.
#[tokio::test(flavor = "multi_thread")]
async fn contradicting_definitions_are_refused() -> Result<()> {
    let cases = [
        (
            "type T = string;\nlet x: T = \"a\";\nx\n",
            "type T = i64;\nval x: T;\n",
            "T is defined by its interface; delete this definition",
        ),
        (
            "trait Show { val show: fn(self) -> string };\n\
             impl Show for i64 { let show = |x| \"s[x]\" };\nShow::show(3)\n",
            "trait Show { val show: fn(self) -> i64 };\nimpl Show for i64;\n",
            "trait Show is declared by its interface; delete this definition",
        ),
        (
            "impl Show for i64 { let show = |x| \"int [x]\" };\n\
             impl<'a: Show> Show for Array<'a> { let show = |xs| \"arr\" };\nShow::show([1])\n",
            "trait Show { val show: fn(self) -> string };\n\
             impl<'a> Show for Array<'a>;\n",
            "the implementation requires 'a: Show, which the signature does not declare",
        ),
    ];
    for (gx, gxi, refusal) in cases {
        let [check, run] = verdicts(gx, gxi).await?;
        for e in [check, run] {
            let e = e.expect("a contradicting definition was accepted");
            assert!(e.contains(refusal), "{e}");
        }
    }
    Ok(())
}

// A script's interface names only what it imports or declares itself,
// as a module's does.
#[tokio::test(flavor = "multi_thread")]
async fn interface_names_resolve_in_the_interface() -> Result<()> {
    let cases = [
        ("type Foo = i64;\nlet v: Foo = 1;\nv\n", "val v: Foo;\n"),
        (
            "use str::Escape;\nlet v: [Escape, null] = null;\nv\n",
            "type W = [Escape, null];\nval v: W;\n",
        ),
    ];
    for (gx, gxi) in cases {
        for e in verdicts(gx, gxi).await? {
            let e = e.expect("a name only the script gives was accepted");
            assert!(e.contains("undefined type"), "{e}");
        }
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn matching_interface_checks() -> Result<()> {
    let dir = tempfile::tempdir()?;
    let file = dir.path().join("prog.gx");
    fs::write(
        &file,
        "impl Sh for i64 { let sh = |i| \"i[i]\" };\nlet t: T = 41;\nSh::sh(t)\n",
    )?;
    fs::write(
        dir.path().join("prog.gxi"),
        "type T = i64;\ntrait Sh { val sh: fn(self) -> string };\nimpl Sh for i64;\nval t: T;\n",
    )?;
    ShellBuilder::<NoExt>::default()
        .cache(CacheMode::Off)
        .mode(Mode::Check(Load::File(file)))
        .build()?
        .check()
        .await
}
