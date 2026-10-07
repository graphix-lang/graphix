use super::TEST_REGISTER;
use crate::types::ThemeV;
use anyhow::{Context, Result};
use graphix_package_core::testing;
use netidx::publisher::FromValue;

#[tokio::test(flavor = "current_thread")]
async fn custom_theme_decodes() -> Result<()> {
    let code = r#"{
use gui::color;
use gui::style::{button_style, rule_style, stylesheet};
`Custom(stylesheet(
  #palette: {
    background: color(#r: 0.1, #g: 0.2, #b: 0.3)$,
    text: color(#r: 0.9)$,
    primary: color(#g: 0.5)$,
    success: color(#b: 0.5)$,
    danger: color(#r: 1.0)$,
    warning: color(#g: 1.0)$
  },
  #button: button_style(#background: color(#r: 0.25)$, #border_width: 2.0),
  #rule: rule_style(#fill_percent: 3.0)
))
}"#;
    let (v, _ctx) = testing::eval(code, TEST_REGISTER).await?;
    let ThemeV(theme) = ThemeV::from_value(v)?;
    let p = theme.palette();
    assert_eq!((p.background.r, p.background.g, p.background.b), (0.1, 0.2, 0.3));
    assert_eq!((p.text.r, p.danger.r, p.warning.g), (0.9, 1.0, 1.0));
    let overrides = theme.overrides.context("stylesheet overrides")?;
    let button = overrides.button.context("button style")?;
    assert_eq!(button.background.map(|c| c.0.r), Some(0.25));
    assert_eq!(button.border_width, Some(2.0));
    assert!(button.text_color.is_none() && button.border_radius.is_none());
    assert_eq!(overrides.rule.and_then(|r| r.fill_percent), Some(3.0));
    assert!(overrides.slider.is_none() && overrides.toggler.is_none());
    Ok(())
}
