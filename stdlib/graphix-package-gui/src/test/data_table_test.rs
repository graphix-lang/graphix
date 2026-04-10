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
        "hidden" => { typ: `Hidden, display_name: null, default_value: &null, max_width: null }
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
        "c0" => { typ: `Text({ on_edit: null }), display_name: null, default_value: &"DEF", max_width: null }
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
let result = data_table(
    #column_types: &{
        "c0" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => "val_a", "r1" => "val_b" },
            max_width: null
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
let result = data_table(
    #column_types: &{
        "virtual" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => "calc_a", "r1" => "calc_b" },
            max_width: null
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
    assert!(
        clicked.is_some() && *clicked.unwrap() != Value::String(arcstr::literal!("")),
        "on_select should have fired, got: {:?}",
        clicked
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
    assert!(
        activated.is_some() && *activated.unwrap() != Value::String(arcstr::literal!("")),
        "on_activate should have fired, got: {:?}",
        activated
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

    // Click in the header row area (top, in a data column)
    let msgs = h.click(iced_core::Point::new(200.0, 10.0));
    h.inner.dispatch_calls(&msgs).await?;

    let col = h.inner.get_watched("test::clicked_col");
    assert!(
        col.is_some() && *col.unwrap() != Value::String(arcstr::literal!("")),
        "on_header_click should have fired, got: {:?}",
        col
    );
    Ok(())
}

// ── Sort by column data tests ──────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_ascending() -> Result<()> {
    let code = r#"
use gui; use gui::data_table; use sys;
let tbl = { rows: ["r0", "r1", "r2"], columns: [("real", v64:0)] };
let result = data_table(
    #sort_mode: &`Column({ name: "priority", direction: `Ascending }),
    #column_types: &{
        "priority" => {
            typ: `Text({ on_edit: null }),
            display_name: "Priority",
            default_value: &{"r0" => "3", "r1" => "1", "r2" => "2" },
            max_width: null
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
let result = data_table(
    #sort_mode: &`Column({ name: "score", direction: `Descending }),
    #column_types: &{
        "score" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => "10", "r1" => "30", "r2" => "20" },
            max_width: null
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
let result = data_table(
    #sort_mode: &`Column({ name: "label", direction: `Ascending }),
    #column_types: &{
        "label" => {
            typ: `Text({ on_edit: null }),
            display_name: null,
            default_value: &{"r0" => "cherry", "r1" => "apple", "r2" => "banana" },
            max_width: null
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
