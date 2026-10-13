use ahash::AHashMap;
use anyhow::Result;
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef};
use graphix_package_gui::widgets::{self, GuiW};
use graphix_rt::NoExt;
use netidx::publisher::Value;
use std::time::Duration;
use tokio::sync::mpsc;

const REG: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

async fn drain(
    gx: &graphix_rt::GXHandle<NoExt>,
    rt: &tokio::runtime::Handle,
    widget: &mut GuiW<NoExt>,
    rx: &mut testing::Events,
    errors: &mut Vec<String>,
) -> Result<()> {
    let deliver = async |id, v: Value| {
        match tokio::task::block_in_place(|| widget.handle_update(rt, id, &v)) {
            Ok(c) => Ok(c),
            Err(e) => {
                errors.push(format!("{e:?}"));
                Ok(false)
            }
        }
    };
    testing::drain_idle(gx, rx, deliver).await.map(|_| ())
}

fn count(w: &GuiW<NoExt>) -> usize {
    let mut n = 0;
    w.for_each_child(&mut |_| n += 1);
    n
}

#[tokio::test(flavor = "multi_thread")]
async fn a_refused_new_child_empties_its_siblings() -> Result<()> {
    let code = "use gui::{row::row, text::text};\n\
                let n = 1;\n\
                let bad = 0.0;\n\
                let items = select n { 1 => [text(&\"one\")], _ => [text(&\"one\"), text(#size: &bad, &\"two\")] };\n\
                let result = row(&items)";
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let id = compiled.exprs[0].id;
    let init = testing::next_update(
        &mut rx,
        id,
        tokio::time::Instant::now() + Duration::from_secs(5),
    )
    .await?;
    let mut widget = widgets::compile(gx.clone(), init).await?;
    let rt = tokio::runtime::Handle::current();
    println!("children at start: {}", count(&widget));
    let mut errors = Vec::new();
    drain(&gx, &rt, &mut widget, &mut rx, &mut errors).await?;
    let n = testing::find_bind_id(&compiled.env, "test::n")?;
    let bad = testing::find_bind_id(&compiled.env, "test::bad")?;
    gx.set(n, Value::I64(2))?;
    drain(&gx, &rt, &mut widget, &mut rx, &mut errors).await?;
    let after_refusal = count(&widget);
    println!("children after the refused child: {after_refusal}");
    gx.set(bad, Value::F64(16.0))?;
    drain(&gx, &rt, &mut widget, &mut rx, &mut errors).await?;
    let after_fix = count(&widget);
    println!("children after the size is fixed: {after_fix}");
    println!("errors: {errors:#?}");
    assert!(after_refusal >= 1, "the row lost its kept child \"one\"");
    Ok(())
}
