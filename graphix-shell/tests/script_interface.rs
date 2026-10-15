//! A script with a `.gxi` beside it runs as a script, and its interface
//! is checked over its top-level names: a `val` the script does not bind
//! as declared, or an impl it does not implement, is refused by the run
//! as by `--check`.

use anyhow::Result;
use graphix_compiler::expr::Source;
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
    let check = shell(Mode::Check(Source::File(file.clone())))?.check().await;
    let run = shell(Mode::Script(Source::File(file.clone())))?
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

// A script's own type, trait or impl that contradicts its interface is
// refused, as a module's is.
#[tokio::test(flavor = "multi_thread")]
async fn contradicting_definitions_are_refused() -> Result<()> {
    let cases = [
        (
            "type T = string;\nlet x: T = \"a\";\nx\n",
            "type T = i64;\nval x: T;\n",
            "signature mismatch in T, expected type T = i64, found type T = string",
        ),
        (
            "trait Show { val show: fn(self) -> string };\n\
             impl Show for i64 { let show = |x| \"s[x]\" };\nShow::show(3)\n",
            "trait Show { val show: fn(self) -> i64 };\nimpl Show for i64;\n",
            "trait Show is declared by the interface as",
        ),
        (
            "trait Show { val show: fn(self) -> string };\n\
             impl Show for i64 { let show = |x| \"int [x]\" };\n\
             impl<'a> Show for Array<'a> { let show = |xs| \"arr\" };\nShow::show([1])\n",
            "trait Show { val show: fn(self) -> string };\n\
             impl<'a: Show> Show for Array<'a>;\n",
            "missing constraint 'a: Show in implementation",
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
        .mode(Mode::Check(Source::File(file)))
        .build()?
        .check()
        .await
}
