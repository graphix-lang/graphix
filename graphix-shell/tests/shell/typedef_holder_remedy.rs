//! A val whose variable is given one type all its uses share, through a
//! binding of a typedef's type, is refused: one holding a function a call
//! returned, where a lambda in its place checks, and one holding the cell
//! under a reference, where a type that holds every value checks.

use anyhow::Result;
use graphix_compiler::expr::Load;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::fs;

async fn check(m: &str) -> Result<Option<String>> {
    let dir = tempfile::tempdir()?;
    let main = dir.path().join("main.gx");
    fs::write(&main, "mod m;\n(m::f(1), m::f(\"s\"))\n")?;
    fs::write(dir.path().join("m.gx"), m)?;
    fs::write(dir.path().join("m.gxi"), "val f: fn(x: 'a) -> 'a;\n")?;
    let checked = ShellBuilder::<NoExt>::default()
        .cache(CacheMode::Off)
        .mode(Mode::Check(Load::File(main)))
        .build()?
        .check()
        .await;
    Ok(checked.err().map(|e| format!("{e:#}")))
}

async fn refused(m: &str, remedy: &str) -> Result<()> {
    let e = check(m).await?.expect("a val sharing a cell was accepted");
    assert!(e.contains(remedy), "{e}");
    Ok(())
}

async fn accepted(m: &str) -> Result<()> {
    let e = check(m).await?;
    assert!(e.is_none(), "{e:?}");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn typedef_holding_a_call_result_names_a_lambda() -> Result<()> {
    refused(
        "type H<'b> = {h: fn(x: 'b) -> 'b};\nlet mk = || |x| x;\n\
         let s0: H<'b> = {h: mk()};\nlet f = |x| s0.h(x)\n",
        "the implementation writes it to a binding outside its let, which has one type \
         all uses share: give that binding a type that holds every 'a, or declare the \
         signature at one type",
    )
    .await?;
    accepted(
        "type H<'b> = {h: fn(x: 'b) -> 'b};\nlet s0: H<'b> = {h: |x| x};\n\
         let f = |x| s0.h(x)\n",
    )
    .await
}

#[tokio::test(flavor = "multi_thread")]
async fn typedef_holding_the_cell_in_data_names_its_type() -> Result<()> {
    let m = |at: &str| {
        format!(
            "type R<'b> = {{g: fn(x: i64) -> i64, r: &mut 'b}};\nlet y = never();\n\
             let s0: R<{at}> = {{g: |x| x, r: &mut y}};\n\
             let f = |x| {{ let r = s0.r; *r <- x; x }}\n"
        )
    };
    refused(
        &m("'b"),
        "the implementation writes it to a binding outside its let, which has one type \
         all uses share: give that binding a type that holds every 'a, or declare the \
         signature at one type",
    )
    .await?;
    accepted(&m("Any")).await
}
