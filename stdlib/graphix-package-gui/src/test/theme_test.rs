use super::{TEST_REGISTER, wait_for_update};
use crate::types::ThemeV;
use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing;
use netidx::publisher::FromValue;
use netidx_core::path::Path;
use tokio::sync::mpsc;

#[tokio::test(flavor = "current_thread")]
async fn custom_theme_decodes() -> Result<()> {
    let code = r#"
use gui::color;
use gui::style::{button_style, rule_style, stylesheet};
let result = `Custom(stylesheet(
  #palette: {
    background: color(#r: 0.1, #g: 0.2, #b: 0.3)$,
    text: color(#r: 0.9)$,
    primary: color(#g: 0.5)$,
    success: color(#b: 0.5)$,
    danger: color(#r: 1.0)$,
    warning: color(#g: 1.0)$
  },
  #button: button_style(#background: color(#r: 0.25)$, #border_width: 2.0),
  #rule: rule_style(#width: 3.0)
))
"#;
    let (tx, mut rx) = mpsc::channel(100);
    let vfs = AHashMap::from_iter([(
        Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx =
        testing::init_with_resolvers(tx, TEST_REGISTER, vec![VfsResolver::new(vfs)])
            .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let v = wait_for_update(&mut rx, compiled.exprs[0].id).await?;
    let ThemeV(theme) = ThemeV::from_value(v)?;
    let p = theme.palette();
    assert_eq!((p.background.r, p.background.g, p.background.b), (0.1, 0.2, 0.3));
    assert_eq!((p.text.r, p.danger.r, p.warning.g), (0.9, 1.0, 1.0));
    let overrides = theme.overrides.context("stylesheet overrides")?;
    let button = overrides.button.context("button style")?;
    assert_eq!(button.background.map(|c| c.0.r), Some(0.25));
    assert_eq!(button.border_width, Some(2.0));
    assert!(button.text_color.is_none() && button.border_radius.is_none());
    assert_eq!(overrides.rule.and_then(|r| r.width), Some(3.0));
    assert!(overrides.slider.is_none() && overrides.toggler.is_none());
    Ok(())
}
