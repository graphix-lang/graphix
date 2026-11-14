//! A val's refusal tells what holds the cell by the typedef's expansion: a
//! parameter that stands only in a function type a call returned is one
//! instance, told to put a lambda there; one under a reference is data, told
//! to give the holder a type.

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
        "that of s0, a binding outside its let holding a function that is one \
         instance, as a call's result is: declare the val at one type, or write a \
         lambda over variables of its own in that function's place",
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
        "that of s0, a binding outside its let: declare the val at one type, or give \
         s0 a type that holds every 'a",
    )
    .await?;
    accepted(&m("Any")).await
}
