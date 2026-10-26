use super::TEST_REGISTER;
use crate::{
    types::WindowSizeV,
    window::{Changed, ResolvedWindow},
};
use anyhow::Result;
use graphix_package_core::testing::{self, Fixture, find_bind_id};
use graphix_rt::NoExt;
use iced_core::Size;
use netidx::publisher::Value;

const APP: &str = r#"
use gui::{window, text::text};
let app = {size: {width: 800.0, height: 600.0}, theme: `Dark, icon: "", title: "t", ticks: 0};
let tick: i64 = never();
let grow: i64 = never();
app <- tick ~ {app with ticks: tick};
app <- grow ~ {app with size: {width: 900.0, height: 700.0}};
let result = window(
  #title: &app.title,
  #size: &mut app.size,
  #theme: &app.theme,
  #icon: &app.icon,
  &text(&"hi")
)
"#;

/// Deliver every update to `w` until the runtime is idle: what changed,
/// and how many updates were the window's own properties.
async fn drain(
    f: &mut Fixture,
    w: &mut ResolvedWindow<NoExt>,
) -> Result<(Vec<Changed>, usize)> {
    let ids = [w.title.r.id, w.size.r.id, w.theme.r.id, w.icon.r.id];
    let (mut changes, mut delivered) = (Vec::new(), 0);
    testing::drain_idle(&f.gx, &mut f.rx, async |id, v: Value| {
        delivered += ids.contains(&id) as usize;
        changes.extend(w.update(id, &v)?);
        Ok(false)
    })
    .await?;
    Ok((changes, delivered))
}

fn set(f: &Fixture, name: &str, v: i64) -> Result<()> {
    f.gx.set(find_bind_id(&f.compiled.env, name)?, Value::I64(v))
}

/// A field reference delivers again at every write of its struct; the
/// window takes only what changed, and never asks again for the size it
/// shows, the size a user's resize wrote back included.
#[tokio::test(flavor = "current_thread")]
async fn a_property_delivered_unchanged_changes_nothing() -> Result<()> {
    let (mut f, v) = Fixture::new(APP, TEST_REGISTER, |_| ()).await?;
    let mut w = ResolvedWindow::compile(f.gx.clone(), v).await?;
    drain(&mut f, &mut w).await?;
    set(&f, "test::tick", 1)?;
    let (changes, delivered) = drain(&mut f, &mut w).await?;
    assert_eq!(delivered, 4, "every property delivered again");
    assert_eq!(changes, []);
    let resized = WindowSizeV::new(Size::new(1000., 700.))?;
    w.size.set_deref(resized)?;
    assert_eq!(drain(&mut f, &mut w).await?.0, []);
    assert_eq!(w.size.t, Some(resized));
    set(&f, "test::tick", 2)?;
    assert_eq!(drain(&mut f, &mut w).await?.0, []);
    set(&f, "test::grow", 1)?;
    let grown = WindowSizeV::new(Size::new(900., 700.))?;
    assert_eq!(drain(&mut f, &mut w).await?.0, [Changed::Size(grown)]);
    Ok(())
}
