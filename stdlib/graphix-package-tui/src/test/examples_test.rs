//! The book's TUI examples run: each renders through a few cycles, and
//! the interactive ones answer a scripted key sequence.

use super::{ch, key};
use crate::testing::TuiTestHarness;
use anyhow::{Context, Result, bail};
use crossterm::event::{Event, KeyCode};
use ratatui::style::Color;
use std::{
    fs,
    path::PathBuf,
    time::{Duration, Instant},
};

const VIEWPORT: (u16, u16) = (80, 24);

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../book/src/examples/tui")
}

async fn harness(name: &str) -> Result<TuiTestHarness> {
    let path = examples_dir().join(format!("{name}.gx"));
    let src = fs::read_to_string(&path).with_context(|| path.display().to_string())?;
    let code = format!("let result = {{\n{src}\n}}");
    TuiTestHarness::with_register(&code, crate::TEST_REGISTER, VIEWPORT.0, VIEWPORT.1)
        .await
        .with_context(|| format!("example {name}"))
}

fn shows(h: &mut TuiTestHarness, text: &str) -> Result<bool> {
    Ok(h.render_lines()?.iter().any(|l| l.contains(text)))
}

/// How long an example may take to draw its first frame: some wait on a
/// timer or a netidx listing.
const FIRST_FRAME: Duration = Duration::from_secs(10);

#[tokio::test(flavor = "multi_thread")]
async fn every_example_renders() -> Result<()> {
    let mut names = vec![];
    for e in fs::read_dir(examples_dir())? {
        let p = e?.path();
        if p.extension().is_some_and(|x| x == "gx") {
            names.push(p.file_stem().unwrap().to_string_lossy().into_owned())
        }
    }
    names.sort();
    assert!(names.len() >= 60, "examples dir looks wrong: {} files", names.len());
    let mut failures = vec![];
    for name in names {
        let r: Result<()> = async {
            let mut h = harness(&name).await?;
            h.drain().await?;
            h.render()?;
            for _ in 0..3 {
                if !h.next_update(Duration::from_millis(10)).await? {
                    break;
                }
                h.render()?;
            }
            let deadline = Instant::now() + FIRST_FRAME;
            while h.render_lines()?.iter().all(|l| l.trim().is_empty()) {
                let left = deadline.saturating_duration_since(Instant::now());
                if left.is_zero() || !h.next_update(left).await? {
                    bail!("draws nothing")
                }
                h.render()?;
            }
            Ok(())
        }
        .await;
        if let Err(e) = r {
            failures.push(format!("{name}: {e:#}"))
        }
    }
    if !failures.is_empty() {
        bail!("{} example(s) failed:\n{}", failures.len(), failures.join("\n"))
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn scroll_paragraph_pages() -> Result<()> {
    let mut h = harness("scroll_paragraph").await?;
    h.drain().await?;
    assert!(shows(&mut h, "Line 1")?);
    h.dispatch_event(key(KeyCode::Down)).await?;
    assert!(!shows(&mut h, "Line 1 ")? && shows(&mut h, "Line 2")?);
    h.dispatch_event(key(KeyCode::End)).await?;
    assert!(shows(&mut h, "Line 100")?);
    h.dispatch_event(key(KeyCode::PageUp)).await?;
    assert!(shows(&mut h, "Line 90")?);
    h.dispatch_event(key(KeyCode::PageDown)).await?;
    assert!(shows(&mut h, "Line 100")? && !shows(&mut h, "Line 90")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn scroll_list_follows_the_selection() -> Result<()> {
    let mut h = harness("scroll_list").await?;
    h.drain().await?;
    h.dispatch_events((0..23).map(|_| key(KeyCode::Down))).await?;
    assert!(shows(&mut h, "Item 1 ")? && shows(&mut h, "Item 24")?);
    h.dispatch_event(key(KeyCode::Down)).await?;
    assert!(!shows(&mut h, "Item 1 ")? && shows(&mut h, "Item 25")?);
    assert_eq!(h.render()?[(0, VIEWPORT.1 - 1)].bg, Color::Yellow);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn list_interactive_selects() -> Result<()> {
    let mut h = harness("list_interactive").await?;
    h.drain().await?;
    h.dispatch_events([key(KeyCode::Down), key(KeyCode::Down)]).await?;
    assert!(shows(&mut h, "▶ Cherry")?);
    h.dispatch_event(key(KeyCode::End)).await?;
    assert!(shows(&mut h, "▶ Grape")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn overlay_modal_opens_and_closes() -> Result<()> {
    let mut h = harness("overlay_modal").await?;
    h.drain().await?;
    assert!(!shows(&mut h, "modal, Esc closes")?);
    h.dispatch_event(ch('o')).await?;
    assert!(shows(&mut h, "modal, Esc closes")?);
    h.dispatch_event(key(KeyCode::Esc)).await?;
    assert!(!shows(&mut h, "modal, Esc closes")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn input_basic_counts() -> Result<()> {
    let mut h = harness("input_basic").await?;
    h.drain().await?;
    h.dispatch_events([key(KeyCode::Up), key(KeyCode::Up), key(KeyCode::Up)]).await?;
    h.dispatch_event(key(KeyCode::Down)).await?;
    assert_eq!(h.render_lines()?[0].trim(), "2");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn input_conditional_gates_the_editor() -> Result<()> {
    let mut h = harness("input_conditional").await?;
    h.drain().await?;
    h.dispatch_event(ch('x')).await?;
    assert!(shows(&mut h, "press e to edit: ")?);
    h.dispatch_events([ch('e'), ch('h'), ch('e')]).await?;
    assert!(shows(&mut h, "editing: he")?);
    h.dispatch_event(key(KeyCode::Esc)).await?;
    assert!(shows(&mut h, "press e to edit: he")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn input_line_edit_saves() -> Result<()> {
    let mut h = harness("input_line_edit").await?;
    h.drain().await?;
    h.dispatch_events("hi".chars().map(|c| ch(c))).await?;
    h.dispatch_event(key(KeyCode::Enter)).await?;
    assert!(shows(&mut h, "saved: hi")?);
    h.dispatch_event(Event::Paste("pasted".into())).await?;
    h.dispatch_event(key(KeyCode::Enter)).await?;
    assert!(shows(&mut h, "saved: pasted")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn input_form_submits() -> Result<()> {
    let mut h = harness("input_form").await?;
    h.drain().await?;
    h.dispatch_event(ch('f')).await?;
    assert!(shows(&mut h, "Your name")?);
    h.dispatch_events("Ada".chars().map(|c| ch(c))).await?;
    h.dispatch_event(key(KeyCode::Tab)).await?;
    h.dispatch_events("Lovelace".chars().map(|c| ch(c))).await?;
    h.dispatch_event(key(KeyCode::Enter)).await?;
    assert!(shows(&mut h, "hello Ada Lovelace")?);
    assert!(!shows(&mut h, "Your name")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn tabs_navigation_moves() -> Result<()> {
    let mut h = harness("tabs_navigation").await?;
    h.drain().await?;
    assert!(h.render_lines()?[2].starts_with("│Overview"));
    h.dispatch_event(key(KeyCode::Right)).await?;
    assert!(h.render_lines()?[2].starts_with("│Items"));
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn block_focus_tabs() -> Result<()> {
    let mut h = harness("block_focus").await?;
    h.drain().await?;
    let borders = |h: &mut TuiTestHarness| -> Result<(Color, Color)> {
        let buf = h.render()?;
        Ok((buf[(0, 0)].fg, buf[(VIEWPORT.0 / 2, 0)].fg))
    };
    assert_eq!(borders(&mut h)?, (Color::Red, Color::Yellow));
    h.dispatch_event(key(KeyCode::Tab)).await?;
    assert_eq!(borders(&mut h)?, (Color::Yellow, Color::Red));
    h.dispatch_event(key(KeyCode::Tab)).await?;
    assert_eq!(borders(&mut h)?, (Color::Red, Color::Yellow));
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn layout_nested_focus_scrolls_to_the_last_line() -> Result<()> {
    let mut h = harness("layout_nested_focus").await?;
    h.drain().await?;
    assert!(shows(&mut h, "1 I've got")?);
    h.dispatch_event(key(KeyCode::Tab)).await?;
    h.dispatch_events((0..60).map(|_| key(KeyCode::Down))).await?;
    let lines = h.render_lines()?;
    let at = |n: usize| lines.iter().position(|l| l.contains(&format!("│{n} I've got")));
    assert!(at(50).is_some() && at(49).is_none(), "{lines:#?}");
    Ok(())
}
