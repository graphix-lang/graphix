//! Tests for the data_table widget.
//!
//! Uses virtual tables (non-absolute paths) so no netidx subscriptions
//! are triggered. Tests verify the data model via DataTableSnapshot.

use super::*;
use anyhow::Result;

/// Create harness and drain initial updates so reactive params are applied.
async fn dt(code: &str) -> Result<GuiTestHarness> {
    let mut h = GuiTestHarness::new(code).await?;
    // Drain until the data table reaches a steady state: the snapshot
    // doesn't change across two consecutive drain cycles. This handles
    // multi-cycle reactive evaluation without depending on timing.
    let mut prev = h.dt_snapshot();
    for _ in 0..20 {
        h.drain().await?;
        let cur = h.dt_snapshot();
        if cur == prev {
            break;
        }
        prev = cur;
    }
    Ok(h)
}

// ── Data model tests ───────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn basic_structure() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1", "r2"], columns: [("c0", v64:0), ("c1", v64:0)] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    // Default sort ascending — columns and rows sorted
    assert_eq!(snap.col_names, vec!["c0", "c1"]);
    assert_eq!(snap.row_basenames, vec!["r0", "r1", "r2"]);
    assert!(!snap.is_value_mode);
    assert_eq!(snap.grid.len(), 3); // 3 rows
    assert_eq!(snap.grid[0].len(), 2); // 2 cols per row
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn empty_table() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: [], columns: [] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(snap.col_names.is_empty());
    assert!(snap.row_basenames.is_empty());
    assert!(snap.grid.is_empty());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn value_mode() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["a", "b"], columns: [] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(snap.is_value_mode);
    assert!(snap.col_names.is_empty());
    assert_eq!(snap.row_basenames, vec!["a", "b"]);
    assert_eq!(snap.grid.len(), 2);
    assert_eq!(snap.grid[0].len(), 1); // value mode: 1 col per row
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_ascending_numeric() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["3", "1", "2"], columns: [("b", v64:0), ("a", v64:0)] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["1", "2", "3"]);
    assert_eq!(snap.col_names, vec!["a", "b"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_ascending_lexicographic() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["cherry", "apple", "banana"], columns: [("z", v64:0), ("a", v64:0)] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["apple", "banana", "cherry"]);
    assert_eq!(snap.col_names, vec!["a", "z"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_disabled_preserves_order() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["z", "a", "m"], columns: [("c", v64:0), ("a", v64:0)] };
let result = data_table(#sort_mode: &`Disabled, #table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["z", "a", "m"]);
    assert_eq!(snap.col_names, vec!["c", "a"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn column_filter_include_preserves_order() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("a", v64:0), ("b", v64:0), ("c", v64:0)] };
let result = data_table(
    #column_filter: &`Include(["c", "a"]),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    // Include with SortMode::None preserves the Include array order
    assert_eq!(snap.col_names, vec!["c", "a"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn column_filter_exclude() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("a", v64:0), ("b", v64:0), ("c", v64:0)] };
let result = data_table(
    #column_filter: &`Exclude(["b"]),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.col_names, vec!["a", "c"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn row_filter_include_preserves_order() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["a", "b", "c"], columns: [("c0", v64:0)] };
let result = data_table(
    #row_filter: &`Include(["c", "a"]),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["c", "a"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn row_filter_keep_range() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["a", "b", "c", "d", "e"], columns: [("c0", v64:0)] };
let result = data_table(
    #row_filter: &`KeepRange({ start: 1, end: 3 }),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    // KeepRange(1,3) on resolver order, then sorted ascending
    assert_eq!(snap.row_basenames.len(), 2);
    assert!(snap.row_basenames.contains(&"b".to_string()));
    assert!(snap.row_basenames.contains(&"c".to_string()));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn hidden_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("visible", v64:0), ("hidden", v64:0)] };
let result = data_table(
    #column_types: &{
        "hidden" => { typ: `Hidden, display_name: null, default_value: &null, on_resize: &null, width: &null }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.col_names, vec!["visible"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn default_value_uniform() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => { typ: `Text({ on_edit: null }), display_name: null, default_value: &"DEF", on_resize: &null, width: &null }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.grid[0][0], "DEF");
    assert_eq!(snap.grid[1][0], "DEF");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn default_value_per_row() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0)] };
let a = "val_a";
let b = "val_b";
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => a, "r1" => b },
            on_resize: &null, width: &null
        }
    },
    #sort_mode: &`Disabled,
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.grid[0][0], "val_a");
    assert_eq!(snap.grid[1][0], "val_b");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn virtual_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1"], columns: [("real", v64:0)] };
let a = "calc_a";
let b = "calc_b";
let result = data_table(
    #column_types: &{
        "virtual" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => a, "r1" => b },
            on_resize: &null, width: &null
        }
    },
    #sort_mode: &`Disabled,
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(snap.col_names.contains(&"real".to_string()));
    assert!(snap.col_names.contains(&"virtual".to_string()));
    // Find virtual column index
    let vi = snap.col_names.iter().position(|n| n == "virtual").unwrap();
    assert_eq!(snap.grid[0][vi], "calc_a");
    assert_eq!(snap.grid[1][vi], "calc_b");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn selection_initial_empty() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let sel = [];
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0)] };
let result = data_table(#selection: &sel, #table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(snap.selection.is_empty());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn selection_from_graphix() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let sel = ["r0/c0"];
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0)] };
let result = data_table(#selection: &sel, #table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.selection, vec!["r0/c0"]);
    Ok(())
}

// ── Callback tests via InteractionHarness ──────────────────────────

/// Test on_select by directly invoking handle_cell_click, bypassing pixel layout.
#[tokio::test(flavor = "current_thread")]
async fn on_select_fires_on_click() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let sel = [];
let last_clicked = "";
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0)] };
let result = data_table(
    #selection: &sel,
    #on_select: |#path: string| {
        sel <- [path];
        last_clicked <- path
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::last_clicked").await?;
    h.drain().await?;

    // Simulate clicking cell (row 0, col "c0") via the widget trait method
    h.widget.handle_cell_click(0, "c0".to_string());
    h.drain().await?;

    let clicked = h.get_watched("test::last_clicked");
    assert_eq!(
        clicked,
        Some(&Value::String(arcstr::literal!("r0/c0"))),
        "on_select should have fired with the cell path",
    );
    Ok(())
}

/// Test on_activate by directly invoking handle_cell_click on the name column.
#[tokio::test(flavor = "current_thread")]
async fn on_activate_fires_on_name_click() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let activated = "";
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0)] };
let result = data_table(
    #on_activate: |#path: string| activated <- path,
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::activated").await?;
    h.drain().await?;

    // Simulate clicking the name column for row 0
    h.widget.handle_cell_click(0, "name".to_string());
    h.drain().await?;

    let activated = h.get_watched("test::activated");
    // Name-column click sends the row path itself (not row/name).
    assert_eq!(
        activated,
        Some(&Value::String(arcstr::literal!("r0"))),
        "on_activate should have fired with the row path",
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn on_header_click_fires() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let clicked_col = "";
let tbl = { rows: ["r0"], columns: [("c0", v64:0), ("c1", v64:0)] };
let result = data_table(
    #on_header_click: |#column: string| clicked_col <- column,
    #table: &tbl
)
"#;
    let mut h = InteractionHarness::with_viewport(
        code,
        iced_core::Size::new(400.0, 200.0),
    ).await?;
    let _ = h.inner.watch("test::clicked_col").await?;
    h.inner.drain().await?;
    // Populate cached_col_widths.
    let _ = h.view();

    // Click on the c1 header: left side of the c1 cell (button text
    // sits just past the container padding), header y is above row 0.
    let bounds = h.inner.dt().dt_cell_bounds(0, "c1").expect("c1 visible");
    let p = iced_core::Point::new(bounds.x + 15.0, 10.0);
    let msgs = h.click(p);
    h.inner.dispatch_calls(&msgs).await?;

    let col = h.inner.get_watched("test::clicked_col");
    assert_eq!(
        col,
        Some(&Value::String(arcstr::literal!("c1"))),
        "on_header_click should have fired with the column name",
    );
    Ok(())
}

// ── Sort by column data tests ──────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_ascending() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1", "r2"], columns: [("real", v64:0)] };
let p0 = "3";
let p1 = "1";
let p2 = "2";
let result = data_table(
    #sort_mode: &`Column({ name: "priority", direction: `Ascending }),
    #column_types: &{
        "priority" => {
            typ: `Text({ on_edit: null }),
            display_name: "Priority",
            default_value: &{"r0" => p0, "r1" => p1, "r2" => p2 },
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    // Sorted ascending by priority values: r1(1) < r2(2) < r0(3)
    assert_eq!(snap.row_basenames, vec!["r1", "r2", "r0"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_descending() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1", "r2"], columns: [("real", v64:0)] };
let s0 = "10";
let s1 = "30";
let s2 = "20";
let result = data_table(
    #sort_mode: &`Column({ name: "score", direction: `Descending }),
    #column_types: &{
        "score" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => s0, "r1" => s1, "r2" => s2 },
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    // Sorted descending by score: r1(30) > r2(20) > r0(10)
    assert_eq!(snap.row_basenames, vec!["r1", "r2", "r0"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_lexicographic() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1", "r2"], columns: [("data", v64:0)] };
let l0 = "cherry";
let l1 = "apple";
let l2 = "banana";
let result = data_table(
    #sort_mode: &`Column({ name: "label", direction: `Ascending }),
    #column_types: &{
        "label" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => l0, "r1" => l1, "r2" => l2 },
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    // Sorted ascending: apple(r1) < banana(r2) < cherry(r0)
    assert_eq!(snap.row_basenames, vec!["r1", "r2", "r0"]);
    Ok(())
}

// ── Sparkline decimation unit test ─────────────────────────────────

#[test]
fn sparkline_decimation() {
    use crate::widgets::data_table::decimate_sparkline;
    use std::collections::VecDeque;
    use std::time::{Duration, Instant};

    let base = Instant::now();
    let mut history: VecDeque<(Instant, f64)> = (0..100)
        .map(|i| {
            let t = base + Duration::from_millis(i * 10);
            let v = (i as f64 * 0.1).sin() * 100.0;
            (t, v)
        })
        .collect();

    assert_eq!(history.len(), 100);
    decimate_sparkline(&mut history);
    assert_eq!(history.len(), 50);

    // Decimate again
    decimate_sparkline(&mut history);
    assert_eq!(history.len(), 25);

    // Values should still be within the original range
    for (_, v) in &history {
        assert!(*v >= -100.0 && *v <= 100.0);
    }

    // Times should be monotonically increasing
    let times: Vec<_> = history.iter().map(|(t, _)| *t).collect();
    for w in times.windows(2) {
        assert!(w[1] >= w[0], "times not monotonic");
    }
}

// ── Editable column callbacks ──────────────────────────────────────

/// 1: Text column on_edit fires through cell-edit lifecycle (begin → input → submit).
#[tokio::test(flavor = "current_thread")]
async fn on_edit_text_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
let edit = |#path: string, #value: string| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: edit }),
            display_name: null,
            default_value: &"old",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    h.widget.handle_cell_edit(0, "c0".to_string());
    h.widget.handle_cell_edit_input("new".to_string());
    h.widget.handle_cell_edit_submit();
    h.drain().await?;
    let log = h.get_watched("test::log");
    assert_eq!(
        log,
        Some(&Value::String(arcstr::literal!("r0/c0=new"))),
        "on_edit text submit should have fired with the new value",
    );
    Ok(())
}

/// 2: Cancelling a text edit must not fire the on_edit callback.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_text_cancel() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
let edit = |#path: string, #value: string| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: edit }),
            display_name: null,
            default_value: &"x",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    h.widget.handle_cell_edit(0, "c0".to_string());
    h.widget.handle_cell_edit_input("never-applied".to_string());
    h.widget.handle_cell_edit_cancel();
    h.drain().await?;
    let log = h.get_watched("test::log");
    assert_eq!(
        log,
        Some(&Value::String(arcstr::literal!(""))),
        "cancel must not invoke on_edit, got: {log:?}",
    );
    Ok(())
}

/// 3: Toggle column on_edit fires when the cell's iced Toggler is
/// clicked. Drives the full pipeline: pixel click → Toggler::on_toggle
/// closure → Message::Call → runtime dispatch → graphix log update.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_toggle_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
let toggled = |#path: string, #value: bool| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Toggle({ on_edit: toggled }),
            display_name: null,
            default_value: &"false",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = InteractionHarness::with_viewport(
        code, iced_core::Size::new(500.0, 200.0),
    ).await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
    // Toggler sits at the left of the cell after the 5px container padding.
    let p = iced_core::Point::new(bounds.x + 15.0, bounds.center().y);
    let msgs = h.click(p);
    expect_call_with_args(&msgs, |args| {
        let v: Vec<_> = args.iter().collect();
        matches!(v.as_slice(),
            [Value::String(p), Value::Bool(true)]
            if p == &arcstr::literal!("r0/c0"))
    });
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(
        h.inner.get_watched("test::log"),
        Some(&Value::String(arcstr::literal!("r0/c0=true"))),
    );
    Ok(())
}

/// 4: Combo column on_edit fires with the chosen choice **id** (not
/// label) when a PickList option is selected. Drives the full pipeline:
/// open-menu click → option click → Message::Call → runtime dispatch.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_combo_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
let pick = |#path: string, #value: string| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Combo({
                choices: [{id: "a", label: "Alpha"}, {id: "b", label: "Bravo"}],
                on_edit: pick
            }),
            display_name: null,
            default_value: &"a",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = InteractionHarness::with_viewport(
        code, iced_core::Size::new(500.0, 300.0),
    ).await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
    // Step 1: click the PickList to open the menu overlay. No Call yet.
    let open_msgs = h.click(bounds.center());
    assert!(
        !open_msgs.iter().any(|m| matches!(m, Message::Call(_, _))),
        "opening the pick list should not fire on_edit; got {open_msgs:?}",
    );
    // Step 2: click the "Bravo" option in the overlay.
    // PickList's overlay opens below the cell; with the default text
    // size (13) and menu padding, each option is ~22px tall. Click
    // roughly in the middle of the second option.
    let option_h = 22.0_f32;
    let p = iced_core::Point::new(
        bounds.center().x,
        bounds.y + bounds.height + option_h * 1.5,
    );
    let pick_msgs = h.click(p);
    expect_call_with_args(&pick_msgs, |args| {
        let v: Vec<_> = args.iter().collect();
        matches!(v.as_slice(),
            [Value::String(p), Value::String(id)]
            if p == &arcstr::literal!("r0/c0") && id == &arcstr::literal!("b"))
    });
    h.inner.dispatch_calls(&pick_msgs).await?;
    assert_eq!(
        h.inner.get_watched("test::log"),
        Some(&Value::String(arcstr::literal!("r0/c0=b"))),
    );
    Ok(())
}

/// 5: Spin column on_edit fires with the new f64 value when the +
/// button is clicked.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_spin_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
let bumped = |#path: string, #value: f64| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Spin({ min: 0.0, max: 10.0, increment: 1.0, on_edit: bumped }),
            display_name: null,
            default_value: &"5.0",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = InteractionHarness::with_viewport(
        code, iced_core::Size::new(500.0, 200.0),
    ).await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
    // Spin cell is a left-aligned Row [−, label, +] (shrink-sized
    // within the cell's 5px-padded content area). Scan from right to
    // left so the first click that fires a Call lands on the + button
    // (rightmost). This rules out a bug that swaps the + and - button
    // wiring — a minus-click would never be the first hit.
    let only_call = |msgs: &[Message]| -> Option<ValArray> {
        msgs.iter().find_map(|m| match m {
            Message::Call(_, args) => Some(args.clone()),
            _ => None,
        })
    };
    let mut hit_args: Option<ValArray> = None;
    for offset in (10..=70).rev().step_by(2) {
        let p = iced_core::Point::new(
            bounds.x + offset as f32,
            bounds.center().y,
        );
        let msgs = h.click(p);
        if let Some(args) = only_call(&msgs) {
            hit_args = Some(args);
            // Dispatch so the runtime updates the graphix log.
            h.inner.dispatch_calls(&msgs).await?;
            break;
        }
    }
    let args = hit_args.expect("no click position produced a Call on the spin cell");
    // The rightmost button is the +. Verify it carries F64(6.0), not
    // F64(4.0) — catches inc/dec swap bugs in render_cell.
    let v: Vec<_> = args.iter().collect();
    assert!(
        matches!(v.as_slice(),
            [Value::String(p), Value::F64(x)]
            if p == &arcstr::literal!("r0/c0") && (*x - 6.0).abs() < 1e-9),
        "rightmost spin button should be + (val 6.0), got args: {:?}", args,
    );
    assert_eq!(
        h.inner.get_watched("test::log"),
        Some(&Value::String(arcstr::literal!("r0/c0=6"))),
    );
    Ok(())
}

/// 6: Button column on_click fires with (path, label) when the cell's
/// iced Button is clicked.
#[tokio::test(flavor = "current_thread")]
async fn on_click_button_column() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
let pressed = |#path: string, #value: Any| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Button({ on_click: pressed }),
            display_name: null,
            default_value: &"Run",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = InteractionHarness::with_viewport(
        code, iced_core::Size::new(500.0, 200.0),
    ).await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
    // Button text "Run" sits at the left after the 5px padding.
    let p = iced_core::Point::new(bounds.x + 15.0, bounds.center().y);
    let msgs = h.click(p);
    expect_call_with_args(&msgs, |args| {
        let v: Vec<_> = args.iter().collect();
        matches!(v.as_slice(),
            [Value::String(p), Value::String(label)]
            if p == &arcstr::literal!("r0/c0") && label == &arcstr::literal!("Run"))
    });
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(
        h.inner.get_watched("test::log"),
        Some(&Value::String(arcstr::literal!("r0/c0=Run"))),
    );
    Ok(())
}

// ── Subscriptions and live updates ─────────────────────────────────

/// 7: on_update fires when a subscribed cell receives a value over netidx.
/// Test ctx has its own internal resolver, so publish/subscribe round-trips
/// in-process. Skip `list_table` and pass a literal table with absolute paths.
#[tokio::test(flavor = "current_thread")]
async fn on_update_fires_for_subscribed_cell() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = "";
sys::net::publish("/local/dt7/r0/c0", v64:42);
let tbl = { rows: ["/local/dt7/r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    // Multiple drains: subscribe + publisher discovery + initial update +
    // callback dispatch is several reactive cycles.
    for _ in 0..15 {
        h.drain().await?;
        if matches!(h.get_watched("test::log"), Some(Value::String(s)) if !s.is_empty()) {
            break;
        }
    }
    let log = h.get_watched("test::log");
    assert!(
        matches!(log, Some(Value::String(s))
            if s.contains("/local/dt7/r0/c0") && s.contains("42")),
        "on_update should have received published value, got: {log:?}",
    );
    Ok(())
}

// ── Default-value reactivity ───────────────────────────────────────

/// 8: A per-row default value backed by a graphix variable updates the
/// rendered cell when the variable changes. Drives the change via a
/// direct `Ref::set` rather than a callable to keep the test focused on
/// the per-row ref propagation path.
#[tokio::test(flavor = "current_thread")]
async fn default_value_per_row_ref_updates() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let a = "v1";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => a},
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    assert_eq!(h.dt_snapshot().grid[0][0], "v1");
    let bid = find_bind_id(&h.compiled.env, "test::a")?;
    let mut a_ref = h.gx.compile_ref(bid).await?;
    a_ref.set(Value::String(arcstr::literal!("v1b")))?;
    for _ in 0..5 {
        h.drain().await?;
        if h.dt_snapshot().grid[0][0] == "v1b" { break; }
    }
    assert_eq!(h.dt_snapshot().grid[0][0], "v1b");
    Ok(())
}

/// 8b: A default_value ref that reads from a nested Map via map::get +
/// opt::or_default continues to reflect later updates to the
/// underlying map, not just the first update. This mirrors
/// book/src/examples/gui/data_table_calculated.gx where `data` is
/// Map<string, Map<string, i64>> updated via `data <- ...` from a
/// callable and the virtual column default_value is
/// `&opt::or_default(map::get(data, "sum"), {})`.
#[tokio::test(flavor = "current_thread")]
async fn default_value_reactive_via_connect() -> Result<()> {
    // Mirror the data_table_calculated.gx pattern: absolute row
    // paths (so the rows ARE subscribed via netidx) and a virtual
    // `sum` column whose default_value reads from a let-bound
    // Map<string, Map<string, i64>> that is updated via `<-` connect.
    // The bug only reproduces with absolute (subscribed) row paths —
    // the cell-update guard at handle_update suppresses default
    // writes when the row has any subscriptions.
    let code = r#"
use gui; use gui::data_table; use sys; use map; use opt;
sys::net::publish("/local/dt8b/r0/c0", v64:0);
sys::net::publish("/local/dt8b/r1/c0", v64:0);
let data: Map<string, Map<string, i64>> = {};
let push = |row: string, sum: i64| {
    let sums = opt::or_default(map::get(data, "sum"), {});
    data <- sum ~ map::insert(data, "sum", map::insert(sums, row, sum))
};
let tbl = {
    rows: ["/local/dt8b/r0", "/local/dt8b/r1"],
    columns: [("c0", v64:0)]
};
let result = data_table(
    #column_types: &{
        "sum" => {
            typ: `Text({ on_edit: null }),
            display_name: "A + B",
            default_value: &opt::or_default(map::get(data, "sum"), {}),
            on_resize: &null,
            width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    // Find "sum" column index (table has c0 and sum).
    let snap = h.dt_snapshot();
    let sum_col = snap
        .col_names
        .iter()
        .position(|n| n == "sum")
        .expect("sum column present");
    let r0 = snap.row_basenames.iter().position(|n| n == "r0").unwrap();
    let r1 = snap.row_basenames.iter().position(|n| n == "r1").unwrap();
    // Initially data is empty so no per-row defaults exist.
    assert_eq!(snap.grid[r0][sum_col], "");
    // First push: r0 -> 5. Should show "5".
    let push_id = h.compile_named_callable("test::push").await?;
    h.call_callback(
        push_id,
        ValArray::from_iter([
            Value::String(arcstr::literal!("r0")),
            Value::I64(5),
        ]),
    )
    .await?;
    for _ in 0..5 {
        h.drain().await?;
        if h.dt_snapshot().grid[r0][sum_col] == "5" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[r0][sum_col], "5", "first update");
    // Second push: r0 -> 9. This is the critical case — does the
    // grid reflect the second update, or does it remain stuck at 5?
    h.call_callback(
        push_id,
        ValArray::from_iter([
            Value::String(arcstr::literal!("r0")),
            Value::I64(9),
        ]),
    )
    .await?;
    for _ in 0..5 {
        h.drain().await?;
        if h.dt_snapshot().grid[r0][sum_col] == "9" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[r0][sum_col], "9", "second update");
    // Third push on a different row: r1 -> 3. r0 should stay at 9,
    // r1 should become 3.
    h.call_callback(
        push_id,
        ValArray::from_iter([
            Value::String(arcstr::literal!("r1")),
            Value::I64(3),
        ]),
    )
    .await?;
    for _ in 0..5 {
        h.drain().await?;
        if h.dt_snapshot().grid[r1][sum_col] == "3" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[r0][sum_col], "9", "r0 preserved");
    assert_eq!(h.dt_snapshot().grid[r1][sum_col], "3", "r1 update");
    // Stress: fire multiple pushes back-to-back without draining
    // between, then drain once. The grid should reflect the latest
    // values, which is what would happen in a GUI event loop where
    // many subscription updates can arrive between renders.
    for v in [10i64, 20, 30, 40] {
        h.gx.call(
            push_id,
            ValArray::from_iter([
                Value::String(arcstr::literal!("r0")),
                Value::I64(v),
            ]),
        )?;
    }
    for _ in 0..10 {
        h.drain().await?;
        if h.dt_snapshot().grid[r0][sum_col] == "40" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[r0][sum_col], "40", "rapid updates");
    Ok(())
}

/// 9: A uniform string default propagates to every grid cell.
#[tokio::test(flavor = "current_thread")]
async fn default_value_uniform_string() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1", "r2"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &"UNI",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    for row in &snap.grid {
        assert_eq!(row[0], "UNI");
    }
    Ok(())
}

// ── Width refs and resize ──────────────────────────────────────────

/// 10: A column-width ref controls `dt_ref_width` for that column.
#[tokio::test(flavor = "current_thread")]
async fn column_width_ref_controlled() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let w = 120.0;
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &null,
            on_resize: &null,
            width: &w
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    assert_eq!(h.dt().dt_ref_width("c0"), Some(120.0));
    Ok(())
}

/// 11: Dragging a column resize handle returns an `(on_resize_cb, new_width)`
/// pair from `handle_mouse_move_resize`; firing the callable through the
/// runtime updates a graphix-side log.
#[tokio::test(flavor = "current_thread")]
async fn on_resize_fires_on_drag() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let log = 0.0;
let on_w = |new_w: f64| log <- new_w;
let w = 100.0;
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &"x",
            on_resize: &on_w,
            width: &w
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    let idx = h.dt().dt_meta_col_idx("c0").expect("c0 visible");
    h.widget.handle_column_resize_start(idx, 100.0);
    let result = h.widget.handle_mouse_move_resize(180.0);
    let (cb_id, new_w) = result.expect("on_resize callback returned");
    h.widget.handle_column_resize_end();
    h.call_callback(cb_id, ValArray::from_iter([Value::F64(new_w)])).await?;
    let log = h.get_watched("test::log");
    assert!(
        matches!(log, Some(Value::F64(f)) if *f > 100.0),
        "on_resize should have logged the new width, got: {log:?}",
    );
    Ok(())
}

// ── Filter and sort variants ───────────────────────────────────────

/// 12: A regex IncludeMatch row filter keeps only rows matching the patterns.
#[tokio::test(flavor = "current_thread")]
async fn row_filter_regex_include() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["abc", "abd", "xyz"], columns: [("c0", v64:0)] };
let result = data_table(
    #row_filter: &`IncludeMatch(["^ab"]),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["abc", "abd"]);
    Ok(())
}

/// 13: A regex ExcludeMatch row filter drops matching rows.
#[tokio::test(flavor = "current_thread")]
async fn row_filter_regex_exclude() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["abc", "xyz", "xab"], columns: [("c0", v64:0)] };
let result = data_table(
    #row_filter: &`ExcludeMatch(["^x"]),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["abc"]);
    Ok(())
}

/// 14: External sort mode disables built-in sorting (graphix manages
/// row order via filters).
#[tokio::test(flavor = "current_thread")]
async fn sort_external_preserves_order() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["zeta", "alpha", "mu"], columns: [("c0", v64:0)] };
let result = data_table(
    #sort_mode: &`External({"c0" => `Ascending}),
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["zeta", "alpha", "mu"]);
    Ok(())
}

/// 15: A Hidden column type is removed from the visible col_names even
/// when other columns are present.
#[tokio::test(flavor = "current_thread")]
async fn hidden_column_absent() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("a", v64:0), ("secret", v64:0), ("b", v64:0)] };
let result = data_table(
    #column_types: &{
        "secret" => { typ: `Hidden, display_name: null, default_value: &null, on_resize: &null, width: &null }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(!snap.col_names.contains(&"secret".to_string()),
        "hidden column must not appear in col_names, got: {:?}", snap.col_names);
    assert_eq!(snap.col_names, vec!["a", "b"]);
    Ok(())
}

// ── Sparkline accumulation and decimation ──────────────────────────

/// 17: A Sparkline column accumulates published values arriving over
/// the netidx subscription path. Uses three one-shot timers spaced by
/// tens of milliseconds: a repeating timer would keep `drain()` spinning
/// forever because batches would always arrive within its reset window.
/// Each timer fires once, publishes a distinct counter value, and then
/// stops producing events — so drain terminates after the last tick.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_accumulates() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys; use sys::time;
let c = 0;
let t1 = time::timer(duration:20.ms, false);
let t2 = time::timer(duration:80.ms, false);
let t3 = time::timer(duration:150.ms, false);
c <- t1 ~ 1;
c <- t2 ~ 2;
c <- t3 ~ 3;
sys::net::publish("/local/dt17/r0/load", c);
let tbl = { rows: ["/local/dt17/r0"], columns: [("load", v64:0)] };
let result = data_table(
    #column_types: &{
        "load" => {
            typ: `Sparkline({ history_seconds: 60.0 }),
            display_name: null,
            default_value: &null,
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    // Give the timers (last fires at 150ms) plus publisher/subscriber
    // round-trips time to land. The cap here is wall-clock bounded: a
    // few hundred ms even if all iterations run.
    for _ in 0..8 {
        h.drain().await?;
        if h.dt().dt_sparkline_len("r0", "load").unwrap_or(0) >= 3 {
            break;
        }
    }
    let len = h.dt().dt_sparkline_len("r0", "load");
    // Subscriber race conditions (resolver lookup + connection happen
    // asynchronously) can sometimes cause the first one or two timer
    // ticks to arrive before the subscriber is ready, so require only
    // >= 2 here. The point is to demonstrate that subscription-driven
    // accumulation flows through the sparkline history.
    assert!(
        len.unwrap_or(0) >= 2,
        "sparkline should have accumulated >= 2 subscribed points, got: {len:?}",
    );
    Ok(())
}

/// 18: A Sparkline column whose default_value parses as f64 seeds the
/// history with at least one initial point (via push_defaults_to_sparklines).
#[tokio::test(flavor = "current_thread")]
async fn sparkline_default_value_seeds_history() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("anchor", v64:0)] };
let result = data_table(
    #column_types: &{
        "spark" => {
            typ: `Sparkline({ history_seconds: 60.0 }),
            display_name: null,
            default_value: &"7.5",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let len = h.dt().dt_sparkline_len("r0", "spark");
    assert!(
        len.unwrap_or(0) >= 1,
        "default value should seed sparkline history, got: {len:?}",
    );
    Ok(())
}

/// 19: Decimation caps stored sparkline points at `MAX_SPARKLINE_POINTS`
/// (currently 512) regardless of how many are pushed.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_decimation_caps_length() -> Result<()> {
    use std::time::{Duration, Instant};
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("anchor", v64:0)] };
let result = data_table(
    #column_types: &{
        "load" => {
            typ: `Sparkline({ history_seconds: 60.0 }),
            display_name: null,
            default_value: &"0.0",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let dt_w = h.dt();
    let base = Instant::now();
    for i in 0..2000_u64 {
        dt_w.dt_push_sparkline(
            "r0", "load",
            base + Duration::from_micros(i),
            i as f64,
        );
    }
    let len = dt_w.dt_sparkline_len("r0", "load").unwrap_or(0);
    assert!(len <= 512, "decimation should cap len <= 512, got {len}");
    // Push a smaller burst on top — must still respect the cap.
    for i in 2000..2500_u64 {
        dt_w.dt_push_sparkline(
            "r0", "load",
            base + Duration::from_micros(i),
            i as f64,
        );
    }
    let len = dt_w.dt_sparkline_len("r0", "load").unwrap_or(0);
    assert!(len <= 512, "after second burst, len <= 512, got {len}");
    Ok(())
}

/// 20: Decimation preserves the extreme values (min/max) of the input
/// stream, demonstrating that the policy keeps the locally-most-deviant
/// point of each merged pair.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_decimation_preserves_extremes() -> Result<()> {
    use std::time::{Duration, Instant};
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("anchor", v64:0)] };
let result = data_table(
    #column_types: &{
        "load" => {
            typ: `Sparkline({ history_seconds: 60.0 }),
            display_name: null,
            default_value: &"0.0",
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    let dt_w = h.dt();
    let base = Instant::now();
    // Push a monotonic ramp, well beyond the cap so decimation runs.
    for i in 0..1024_u64 {
        dt_w.dt_push_sparkline(
            "r0", "load",
            base + Duration::from_micros(i),
            i as f64,
        );
    }
    let vals = dt_w.dt_sparkline_values("r0", "load")
        .expect("sparkline values present");
    let min = vals.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = vals.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    // The minimum should still be very close to 0 and the max close to
    // 1023 — decimation keeps the more-deviant point of each pair.
    assert!(min <= 1.0, "min should be near 0 after decimation, got {min}");
    assert!(max >= 1022.0, "max should be near 1023 after decimation, got {max}");
    Ok(())
}

// ── Coverage: keyboard nav, name-col isolation, double-click resize ─

/// 21: Arrow keys move selection, Enter fires on_activate.
///
/// Plumbs selection through graphix — on_select writes to `sel`, which
/// flows back into the widget's `#selection` ref. Without this, the
/// widget's `selection` stays empty and each keystroke restarts from
/// the default (row 0, first data col).
#[tokio::test(flavor = "current_thread")]
async fn keyboard_nav_arrows_and_enter() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let sel = [];
let selected = "";
let activated = "";
let tbl = { rows: ["r0", "r1"], columns: [("c0", v64:0), ("c1", v64:0)] };
let result = data_table(
    #selection: &sel,
    #on_select: |#path: string| {
        sel <- [path];
        selected <- path
    },
    #on_activate: |#path: string| activated <- path,
    #table: &tbl
)
"#;
    let mut h = InteractionHarness::with_viewport(
        code, iced_core::Size::new(500.0, 200.0),
    ).await?;
    let _ = h.watch("test::selected").await?;
    let _ = h.watch("test::activated").await?;
    h.inner.drain().await?;
    let _ = h.view();
    // KeyboardArea only processes key events when focused. Focus is
    // granted by a mouse click inside its bounds.
    let msgs = h.click(iced_core::Point::new(100.0, 40.0));
    h.inner.dispatch_calls(&msgs).await?;

    // handle_table_key starts at (row 0, cur_col=name_offset=1 → "c0").
    // ArrowRight advances to column "c1".
    let msgs = h.press_key(iced_core::keyboard::key::Named::ArrowRight);
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::selected"),
        Some(&Value::String(arcstr::literal!("r0/c1"))));

    // ArrowDown moves to row 1, keeping col "c1".
    let msgs = h.press_key(iced_core::keyboard::key::Named::ArrowDown);
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::selected"),
        Some(&Value::String(arcstr::literal!("r1/c1"))));

    // Enter fires on_activate with the row path.
    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::activated"),
        Some(&Value::String(arcstr::literal!("r1"))));
    Ok(())
}

/// 22: Clicking the name column fires on_activate but NOT on_select.
#[tokio::test(flavor = "current_thread")]
async fn name_click_activate_only() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let sel_log = "";
let act_log = "";
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let result = data_table(
    #on_select: |#path: string| sel_log <- path,
    #on_activate: |#path: string| act_log <- path,
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::sel_log").await?;
    let _ = h.watch("test::act_log").await?;
    h.drain().await?;
    h.widget.handle_cell_click(0, "name".to_string());
    h.drain().await?;
    assert_eq!(h.get_watched("test::act_log"),
        Some(&Value::String(arcstr::literal!("r0"))));
    assert_eq!(h.get_watched("test::sel_log"),
        Some(&Value::String(arcstr::literal!(""))),
        "on_select must not fire for the name column");
    Ok(())
}

/// 23: A second click on a resize handle within 400ms triggers
/// auto-fit via `auto_fit_all_columns`, which writes to `user_widths`.
#[tokio::test(flavor = "current_thread")]
async fn resize_handle_double_click_autofits() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0"], columns: [("c0", v64:0)] };
let long = "wider than MIN_COL_WIDTH default";
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &long,
            on_resize: &null, width: &null
        }
    },
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.view();
    let idx = h.dt().dt_meta_col_idx("c0").expect("c0 visible");
    // Baseline: no user width yet (cell was auto-sized in view()).
    assert_eq!(h.dt().dt_user_width("c0"), None);
    // First call: starts a resize drag.
    h.widget.handle_column_resize_start(idx, 100.0);
    // Second call within 400ms on the same handle: triggers auto-fit.
    h.widget.handle_column_resize_start(idx, 100.0);
    let w = h.dt().dt_user_width("c0").expect("auto-fit writes user_widths");
    // MIN_COL_WIDTH = 80; the long content must exceed it.
    assert!(w > 80.0, "auto-fit width must exceed MIN_COL_WIDTH, got {w}");
    Ok(())
}
