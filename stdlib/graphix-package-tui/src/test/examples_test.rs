//! The book's TUI examples run: each renders through a few cycles, and
//! the interactive ones answer a scripted key sequence.

use crate::testing::TuiTestHarness;
use anyhow::{Context, Result, bail};
use crossterm::event::{Event, KeyCode, KeyEvent};
use graphix_package_core::testing::PackageRef;
use std::{fs, path::PathBuf, time::Duration};

const VIEWPORT: (u16, u16) = (80, 24);

/// What the examples use: tui's own register and `rand`.
const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_rand::P,
    &crate::P,
];

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../book/src/examples/tui")
}

async fn harness(name: &str) -> Result<TuiTestHarness> {
    let path = examples_dir().join(format!("{name}.gx"));
    let src = fs::read_to_string(&path).with_context(|| path.display().to_string())?;
    let code = format!("let result = {{\n{src}\n}}");
    TuiTestHarness::with_register(&code, REGISTER, VIEWPORT.0, VIEWPORT.1)
        .await
        .with_context(|| format!("example {name}"))
}

fn key(code: KeyCode) -> Event {
    Event::Key(KeyEvent::from(code))
}

fn shows(h: &mut TuiTestHarness, text: &str) -> Result<bool> {
    Ok(h.render_lines()?.iter().any(|l| l.contains(text)))
}

#[tokio::test(flavor = "multi_thread")]
// CR claude for claude: [test-gap/low] [test-gap] An example whose root never produces
// renders an all-blank buffer without error, so it passes here. examples_compile.rs
// already builds every example, so the render is the only thing this test adds, and it
// checks nothing about it. Require at least one non-blank cell after the drain, and
// list explicitly any example that legitimately starts blank. Found by reading; not
// run. (tests-tui-09)
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
    // CR claude for claude: [test-gap/low] [test-gap] The assertion after this key
    // holds before the key is sent. At position 89 (after PageUp) the view already
    // shows Line 90..Line 100, so `shows("Line 100")` cannot see PageDown. Add
    // `!shows(&mut h, "Line 90")?`, since position 99 shows only Line 100.
    // scroll_list_follows_the_selection's "Item 16" check has the same problem: that
    // item is visible at scroll 0. Found by reading; not run. (tests-tui-03)
    h.dispatch_event(key(KeyCode::PageDown)).await?;
    assert!(shows(&mut h, "Line 100")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn scroll_list_follows_the_selection() -> Result<()> {
    let mut h = harness("scroll_list").await?;
    h.drain().await?;
    assert!(shows(&mut h, "Item 1")?);
    h.dispatch_events((0..15).map(|_| key(KeyCode::Down))).await?;
    assert!(shows(&mut h, "Item 16")?);
    assert!(!shows(&mut h, "Item 1 ")?);
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
    h.dispatch_event(key(KeyCode::Char('o'))).await?;
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
    h.dispatch_event(key(KeyCode::Char('x'))).await?;
    assert!(shows(&mut h, "press e to edit: ")?);
    h.dispatch_events([
        key(KeyCode::Char('e')),
        key(KeyCode::Char('h')),
        key(KeyCode::Char('e')),
    ])
    .await?;
    assert!(shows(&mut h, "editing: he")?);
    h.dispatch_event(key(KeyCode::Esc)).await?;
    assert!(shows(&mut h, "press e to edit: he")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn input_line_edit_saves() -> Result<()> {
    let mut h = harness("input_line_edit").await?;
    h.drain().await?;
    h.dispatch_events("hi".chars().map(|c| key(KeyCode::Char(c)))).await?;
    h.dispatch_event(key(KeyCode::Enter)).await?;
    assert!(shows(&mut h, "saved: hi")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn input_form_submits() -> Result<()> {
    let mut h = harness("input_form").await?;
    h.drain().await?;
    h.dispatch_event(key(KeyCode::Char('f'))).await?;
    assert!(shows(&mut h, "Your name")?);
    h.dispatch_events("Ada".chars().map(|c| key(KeyCode::Char(c)))).await?;
    h.dispatch_event(key(KeyCode::Tab)).await?;
    h.dispatch_events("Lovelace".chars().map(|c| key(KeyCode::Char(c)))).await?;
    h.dispatch_event(key(KeyCode::Enter)).await?;
    assert!(shows(&mut h, "hello Ada Lovelace")?);
    assert!(!shows(&mut h, "Your name")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn tabs_navigation_moves() -> Result<()> {
    let mut h = harness("tabs_navigation").await?;
    h.drain().await?;
    assert!(shows(&mut h, "Overview")?);
    h.dispatch_event(key(KeyCode::Right)).await?;
    // CR claude for claude: [test-gap/low] [test-gap] This assertion is vacuous:
    // "Items" is a tab title in the tab bar, so it is on screen before Right is
    // pressed. Check the body row instead. Inside the block, row 2 should read
    // "│Overview" before the key and "│Items" after it. Found by reading; not run.
    // (tests-tui-02)
    // CR claude for claude: [test-gap/medium] This cannot fail. "Items" is a tab title
    // and is always in the tab bar, so the test passes with the Right arm deleted.
    // block_focus_tabs below has the same problem: "Block 2" is a pane title, and two
    // Tabs return focus to pane 0. Assert on the content row under the tab bar here. In
    // block_focus_tabs, press Tab once and compare the two panes' border cell fg via
    // `h.render()?`. Found by reading; not run. (examples-tui-04)
    assert!(shows(&mut h, "Items")?);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn block_focus_tabs() -> Result<()> {
    let mut h = harness("block_focus").await?;
    h.drain().await?;
    h.dispatch_event(key(KeyCode::Tab)).await?;
    h.dispatch_event(key(KeyCode::Tab)).await?;
    // CR claude for claude: [test-gap/medium] [test-gap] This test is vacuous. "Block
    // 2" is pane 2's title, so it is on screen whatever the focus, and two Tabs take
    // block_focus.gx's `focused` from 0 to 1 and back to 0. The only visible sign of
    // focus is the border colour. Send one Tab, check that pane 2's border cell
    // (buffer[(40, 0)].fg) is Red and pane 1's is Yellow, then send a second Tab and
    // check that they swap back. Found by reading; not run. (tests-tui-01)
    assert!(shows(&mut h, "Block 2")?);
    Ok(())
}
