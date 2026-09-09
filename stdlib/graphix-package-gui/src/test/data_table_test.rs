//! Tests for the data_table widget, mostly over virtual tables
//! (non-absolute paths, no subscriptions) via `DataTableSnapshot`.

use super::*;
use anyhow::Result;

/// Harness drained until the snapshot is steady.
async fn dt(code: &str) -> Result<GuiTestHarness> {
    let mut h = GuiTestHarness::new(code).await?;
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

#[tokio::test(flavor = "current_thread")]
async fn basic_structure() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1", "r2"], columns: ["c0", "c1"] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
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
use gui::*; use gui::data_table::{self, *}; use sys::*;
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
use gui::*; use gui::data_table::{self, *}; use sys::*;
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
async fn default_preserves_table_order() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["z", "a", "m"], columns: ["c", "a"] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["z", "a", "m"]);
    assert_eq!(snap.col_names, vec!["c", "a"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn default_value_uniform() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }), display_name: null, source: &"DEF", on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.grid[0][0], "DEF");
    assert_eq!(snap.grid[1][0], "DEF");
    Ok(())
}

/// `` `Netidx(placeholder) `` shows the placeholder while a cell has no
/// subscription value; a null placeholder shows blank.
#[tokio::test(flavor = "current_thread")]
async fn netidx_source_placeholder() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: [
        { name: "loading", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &`Netidx("…"),
            on_resize: &null, width: &null },
        { name: "blank", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &`Netidx(null),
            on_resize: &null, width: &null }
    ] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    let loading_idx = snap.col_names.iter().position(|n| n == "loading").unwrap();
    let blank_idx = snap.col_names.iter().position(|n| n == "blank").unwrap();
    assert_eq!(snap.grid[0][loading_idx], "…");
    assert_eq!(snap.grid[1][loading_idx], "…");
    assert_eq!(snap.grid[0][blank_idx], "");
    assert_eq!(snap.grid[1][blank_idx], "");
    Ok(())
}

/// `` `Netidx(map) `` is a per-row fallback; with no absolute rows every
/// cell renders from the map.
#[tokio::test(flavor = "current_thread")]
async fn netidx_source_map_fallback() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let m = {"r0" => "alpha", "r1" => "beta"};
let tbl = { rows: ["r0", "r1"], columns: [
        { name: "score", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &`Netidx(m),
            on_resize: &null, width: &null }
    ] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    let i = snap.col_names.iter().position(|n| n == "score").unwrap();
    assert_eq!(snap.grid[0][i], "alpha");
    assert_eq!(snap.grid[1][i], "beta");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn default_value_per_row() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let a = "val_a";
let b = "val_b";
let tbl = { rows: ["r0", "r1"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &{"r0" => a, "r1" => b },
            on_resize: &null, width: &null }
    ] };
let result = data_table(
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
use gui::*; use gui::data_table::{self, *}; use sys::*;
let a = "calc_a";
let b = "calc_b";
let tbl = { rows: ["r0", "r1"], columns: [
        "real",
        { name: "virtual", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &{"r0" => a, "r1" => b },
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(snap.col_names.contains(&"real".to_string()));
    assert!(snap.col_names.contains(&"virtual".to_string()));
    let vi = snap.col_names.iter().position(|n| n == "virtual").unwrap();
    assert_eq!(snap.grid[0][vi], "calc_a");
    assert_eq!(snap.grid[1][vi], "calc_b");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn selection_initial_empty() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let sel = [];
let tbl = { rows: ["r0", "r1"], columns: ["c0"] };
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
use gui::*; use gui::data_table::{self, *}; use sys::*;
let sel = ["r0/c0"];
let tbl = { rows: ["r0", "r1"], columns: ["c0"] };
let result = data_table(#selection: &sel, #table: &tbl)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.selection, vec!["r0/c0"]);
    Ok(())
}

/// on_select via `handle_cell_click`, bypassing pixel layout.
#[tokio::test(flavor = "current_thread")]
async fn on_select_fires_on_click() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let sel = [];
let last_clicked = "";
let tbl = { rows: ["r0", "r1"], columns: ["c0"] };
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

    h.dt_mut().handle_cell_click(0, "c0".into());
    h.drain().await?;

    let clicked = h.get_watched("test::last_clicked");
    assert_eq!(
        clicked,
        Some(&Value::String(arcstr::literal!("r0/c0"))),
        "on_select should have fired with the cell path",
    );
    Ok(())
}

/// on_activate via `handle_cell_click` on the name column.
#[tokio::test(flavor = "current_thread")]
async fn on_activate_fires_on_name_click() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let activated = "";
let tbl = { rows: ["r0", "r1"], columns: ["c0"] };
let result = data_table(
    #on_activate: |#path: string| activated <- path,
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::activated").await?;
    h.drain().await?;

    h.dt_mut().handle_cell_click(0, "name".into());
    h.drain().await?;

    let activated = h.get_watched("test::activated");
    // A name-column click sends the row path itself.
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
use gui::*; use gui::data_table::{self, *}; use sys::*;
let clicked_col = "";
let tbl = { rows: ["r0"], columns: ["c0", "c1"] };
let result = data_table(
    #on_header_click: |#column: string| clicked_col <- column,
    #table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(400.0, 200.0))
            .await?;
    let _ = h.inner.watch("test::clicked_col").await?;
    h.inner.drain().await?;
    let _ = h.view();
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

/// A header click rewrites `sort_by` and the header indicator follows
/// the cycle absent → Ascending → Descending → absent.
#[tokio::test(flavor = "current_thread")]
async fn header_click_cycles_sort_state() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use array::*;
let tbl = { rows: ["r0"], columns: ["c0", "c1"] };
let sort_by: Array<SortBy> = [];
let result = data_table(
    #sort_by: &sort_by,
    #on_header_click: |#column: string|
        sort_by <- column ~ {
            let matches = array::filter(sort_by, |sb: SortBy| sb.column == column);
            let n = array::len(matches);
            let m0 = matches[0]$;
            let dir: SortDirection = m0.direction;
            select n {
                0 => array::push(sort_by, { column: column, direction: `Ascending }),
                _ => select dir {
                    `Ascending => array::map(sort_by, |s: SortBy| -> SortBy
                        select s.column == column {
                            true => { s with direction: `Descending },
                            false => s
                        }),
                    `Descending => array::filter(sort_by, |s: SortBy| s.column != column)
                }
            }
        },
    #table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(400.0, 200.0))
            .await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c1").expect("c1 visible");
    let click = iced_core::Point::new(bounds.x + 15.0, 10.0);

    // Click 1: absent → Ascending.
    let msgs = h.click(click);
    h.inner.dispatch_calls(&msgs).await?;
    h.inner.drain().await?;
    let _ = h.view();
    assert_eq!(
        h.inner.dt().dt_sort_indicator("c1").as_deref(),
        Some(" ▲"),
        "first click should leave c1 sorted ascending",
    );

    // Click 2: Ascending → Descending.
    let msgs = h.click(click);
    h.inner.dispatch_calls(&msgs).await?;
    h.inner.drain().await?;
    let _ = h.view();
    assert_eq!(
        h.inner.dt().dt_sort_indicator("c1").as_deref(),
        Some(" ▼"),
        "second click should flip c1 to descending",
    );

    // Click 3: Descending → absent.
    let msgs = h.click(click);
    h.inner.dispatch_calls(&msgs).await?;
    h.inner.drain().await?;
    let _ = h.view();
    assert_eq!(
        h.inner.dt().dt_sort_indicator("c1"),
        None,
        "third click should remove c1 from sort_by",
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_ascending() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let p0 = "3";
let p1 = "1";
let p2 = "2";
let tbl = { rows: ["r0", "r1", "r2"], columns: [
        "real",
        { name: "priority", typ: `Text({ on_edit: null }),
            display_name: "Priority",
            source: &{"r0" => p0, "r1" => p1, "r2" => p2 },
            on_resize: &null, width: &null }
    ] };
let result = data_table(
    #sort_by: &[{ column: "priority", direction: `Ascending }],
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["r1", "r2", "r0"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_descending() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let s0 = "10";
let s1 = "30";
let s2 = "20";
let tbl = { rows: ["r0", "r1", "r2"], columns: [
        "real",
        { name: "score", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &{"r0" => s0, "r1" => s1, "r2" => s2 },
            on_resize: &null, width: &null }
    ] };
let result = data_table(
    #sort_by: &[{ column: "score", direction: `Descending }],
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["r1", "r2", "r0"]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sort_by_virtual_column_lexicographic() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let l0 = "cherry";
let l1 = "apple";
let l2 = "banana";
let tbl = { rows: ["r0", "r1", "r2"], columns: [
        "data",
        { name: "label", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &{"r0" => l0, "r1" => l1, "r2" => l2 },
            on_resize: &null, width: &null }
    ] };
let result = data_table(
    #sort_by: &[{ column: "label", direction: `Ascending }],
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames, vec!["r1", "r2", "r0"]);
    Ok(())
}

/// No sort_by means no indicator on any header.
#[tokio::test(flavor = "current_thread")]
async fn sort_indicator_absent_by_default() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: ["name", "env"] };
let result = data_table(#table: &tbl)
"#;
    let h = dt(code).await?;
    assert_eq!(h.dt().dt_sort_indicator("name"), None);
    assert_eq!(h.dt().dt_sort_indicator("env"), None);
    Ok(())
}

/// A single-column sort renders the arrow with no priority digit.
#[tokio::test(flavor = "current_thread")]
async fn sort_indicator_single_column() -> Result<()> {
    let ascending = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: ["name", "env"] };
let result = data_table(
    #sort_by: &[{ column: "name", direction: `Ascending }],
    #table: &tbl
)
"#;
    let h = dt(ascending).await?;
    assert_eq!(h.dt().dt_sort_indicator("name").as_deref(), Some(" ▲"));
    assert_eq!(h.dt().dt_sort_indicator("env"), None);

    let descending = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: ["name", "env"] };
let result = data_table(
    #sort_by: &[{ column: "env", direction: `Descending }],
    #table: &tbl
)
"#;
    let h = dt(descending).await?;
    assert_eq!(h.dt().dt_sort_indicator("name"), None);
    assert_eq!(h.dt().dt_sort_indicator("env").as_deref(), Some(" ▼"));
    Ok(())
}

/// A multi-column sort adds a 1-based subscript priority digit.
#[tokio::test(flavor = "current_thread")]
async fn sort_indicator_multi_column_shows_priority() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: ["name", "env", "score"] };
let result = data_table(
    #sort_by: &[
        { column: "env", direction: `Ascending },
        { column: "name", direction: `Descending },
        { column: "score", direction: `Ascending }
    ],
    #table: &tbl
)
"#;
    let h = dt(code).await?;
    assert_eq!(h.dt().dt_sort_indicator("env").as_deref(), Some(" ▲₁"));
    assert_eq!(h.dt().dt_sort_indicator("name").as_deref(), Some(" ▼₂"));
    assert_eq!(h.dt().dt_sort_indicator("score").as_deref(), Some(" ▲₃"));
    Ok(())
}

/// An update to a subscribed sort column re-sorts the rows on the next
/// `before_view`.
#[tokio::test(flavor = "current_thread")]
async fn sort_by_subscribed_column_reorders_on_update() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let v0 = f64:30.0;
let v1 = f64:10.0;
let v2 = f64:20.0;
sys::net::publish("/local/dt_sort_live/r0/cpu", v0);
sys::net::publish("/local/dt_sort_live/r1/cpu", v1);
sys::net::publish("/local/dt_sort_live/r2/cpu", v2);
let tbl = {
    rows: ["/local/dt_sort_live/r0", "/local/dt_sort_live/r1", "/local/dt_sort_live/r2"],
    columns: ["cpu"]
};
let result = data_table(
    #sort_by: &[{ column: "cpu", direction: `Ascending }],
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    for _ in 0..15 {
        h.drain().await?;
        h.before_view();
        let snap = h.dt_snapshot();
        if snap.row_basenames == vec!["r1", "r2", "r0"] {
            break;
        }
    }
    let snap = h.dt_snapshot();
    assert_eq!(
        snap.row_basenames,
        vec!["r1", "r2", "r0"],
        "initial ascending sort: r1(10) < r2(20) < r0(30)"
    );

    let bid = find_bind_id(&h.compiled.env, "test::v1")?;
    let mut v1_ref = h.gx.compile_ref(bid).await?;
    v1_ref.set(Value::F64(100.0))?;
    for _ in 0..15 {
        h.drain().await?;
        h.before_view();
        let snap = h.dt_snapshot();
        if snap.row_basenames == vec!["r2", "r0", "r1"] {
            break;
        }
    }
    let snap = h.dt_snapshot();
    assert_eq!(
        snap.row_basenames,
        vec!["r2", "r0", "r1"],
        "after bumping r1's cpu to 100: r2(20) < r0(30) < r1(100)"
    );
    Ok(())
}

/// Rows that scroll into view through a resort get live subscriptions.
#[tokio::test(flavor = "current_thread")]
async fn sort_subscribes_newly_visible_rows() -> Result<()> {
    // cpu = row index; the window is 30 rows, so a descending sort shows
    // mostly rows outside the initial subscription set.
    let n_rows: usize = 40;
    let mut publishes = String::new();
    let mut rows = String::new();
    for i in 0..n_rows {
        publishes.push_str(&format!(
            "sys::net::publish(\"/local/dt_sub_resort/r{i}/cpu\", v64:{i});\n"
        ));
        if i > 0 {
            rows.push_str(", ");
        }
        rows.push_str(&format!("\"/local/dt_sub_resort/r{i}\""));
    }
    let code = format!(
        r#"
use gui::*; use gui::data_table::{{self, *}}; use sys::*;
{publishes}
let tbl = {{ rows: [{rows}], columns: ["cpu"] }};
let result = data_table(
    #sort_by: &[{{ column: "cpu", direction: `Descending }}],
    #table: &tbl
)
"#
    );
    let mut h = dt(&code).await?;
    let top = format!("r{}", n_rows - 1);
    for _ in 0..20 {
        h.drain().await?;
        h.before_view();
        let snap = h.dt_snapshot();
        if snap.row_basenames.first().map(|s| s.as_str()) == Some(top.as_str())
            && !snap
                .grid
                .first()
                .and_then(|r| r.first())
                .map(|s| s.is_empty())
                .unwrap_or(true)
        {
            break;
        }
    }
    let snap = h.dt_snapshot();
    assert_eq!(snap.row_basenames[0], top, "top row after desc sort");
    for vi in 0..30 {
        let row = &snap.row_basenames[vi];
        let val = &snap.grid[vi][0];
        assert!(
            !val.is_empty(),
            "row {row} (visible position {vi}) has empty cpu cell after resort \
             — subs didn't follow the new visible set"
        );
    }
    Ok(())
}

/// With more rows than fit the window, off-screen rows still sort
/// correctly through column and direction changes (alpha asc → alpha
/// desc → beta desc → beta asc → alpha asc; `beta[i] = (i*7+13) % n`
/// makes every ordering distinct).
#[tokio::test(flavor = "current_thread")]
async fn sort_by_change_resorts_offscreen_rows() -> Result<()> {
    let n_rows: usize = 100;
    let beta_of = |i: usize| (i * 7 + 13) % n_rows;
    let mut publishes = String::new();
    let mut rows = String::new();
    for i in 0..n_rows {
        publishes.push_str(&format!(
            "sys::net::publish(\"/local/dt_sort_chg/r{i}/alpha\", v64:{i});\n"
        ));
        publishes.push_str(&format!(
            "sys::net::publish(\"/local/dt_sort_chg/r{i}/beta\", v64:{});\n",
            beta_of(i)
        ));
        if i > 0 {
            rows.push_str(", ");
        }
        rows.push_str(&format!("\"/local/dt_sort_chg/r{i}\""));
    }
    let code = format!(
        r#"
use gui::*; use gui::data_table::{{self, *}}; use sys::*;
{publishes}
let sort_col = "alpha";
let sort_dir: SortDirection = `Ascending;
let sort_by: Array<SortBy> = [{{ column: sort_col, direction: sort_dir }}];
let tbl = {{ rows: [{rows}], columns: ["alpha", "beta"] }};
let result = data_table(
    #sort_by: &sort_by,
    #table: &tbl
)
"#
    );
    let mut h = dt(&code).await?;

    let alpha_asc: Vec<String> = (0..n_rows).map(|i| format!("r{i}")).collect();
    let alpha_desc: Vec<String> = alpha_asc.iter().rev().cloned().collect();
    let mut beta_pairs: Vec<(usize, usize)> =
        (0..n_rows).map(|i| (i, beta_of(i))).collect();
    beta_pairs.sort_by_key(|(_, b)| *b);
    let beta_asc: Vec<String> = beta_pairs.iter().map(|(i, _)| format!("r{i}")).collect();
    let beta_desc: Vec<String> = beta_asc.iter().rev().cloned().collect();

    async fn await_order(
        h: &mut GuiTestHarness,
        expected: &[String],
        why: &str,
    ) -> Result<()> {
        for _ in 0..40 {
            h.drain().await?;
            h.before_view();
            if h.dt_snapshot().row_basenames == expected {
                return Ok(());
            }
        }
        let got = h.dt_snapshot().row_basenames;
        anyhow::bail!(
            "{why}: row order didn't converge\n  expected first 10: {:?}\n  got first 10:      {:?}",
            &expected[..10.min(expected.len())],
            &got[..10.min(got.len())],
        )
    }

    await_order(&mut h, &alpha_asc, "initial alpha asc").await?;

    let col_bid = find_bind_id(&h.compiled.env, "test::sort_col")?;
    let mut col_ref = h.gx.compile_ref(col_bid).await?;
    let dir_bid = find_bind_id(&h.compiled.env, "test::sort_dir")?;
    let mut dir_ref = h.gx.compile_ref(dir_bid).await?;

    dir_ref.set(Value::String(arcstr::literal!("Descending")))?;
    await_order(&mut h, &alpha_desc, "alpha desc after direction flip").await?;

    col_ref.set(Value::String(arcstr::literal!("beta")))?;
    await_order(&mut h, &beta_desc, "beta desc after column switch").await?;

    dir_ref.set(Value::String(arcstr::literal!("Ascending")))?;
    await_order(&mut h, &beta_asc, "beta asc after direction flip").await?;

    col_ref.set(Value::String(arcstr::literal!("alpha")))?;
    await_order(&mut h, &alpha_asc, "alpha asc after returning to alpha").await?;

    Ok(())
}

#[test]
fn sparkline_decimation() {
    use crate::widgets::data_table::decimate_sparkline;
    use std::{
        collections::VecDeque,
        time::{Duration, Instant},
    };

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

    decimate_sparkline(&mut history);
    assert_eq!(history.len(), 25);

    for (_, v) in &history {
        assert!(*v >= -100.0 && *v <= 100.0);
    }

    let times: Vec<_> = history.iter().map(|(t, _)| *t).collect();
    for w in times.windows(2) {
        assert!(w[1] >= w[0], "times not monotonic");
    }
}

/// Text column on_edit fires through begin → input → submit; an
/// unparseable buffer commits as a string.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_text_column() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let edit = |#path: string, #value: Any| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: edit }),
            display_name: null,
            source: &"old",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    h.dt_mut().handle_cell_edit(0, "c0".into());
    h.dt_mut().handle_cell_edit_input("new".into());
    h.dt_mut().handle_cell_edit_submit();
    h.drain().await?;
    let log = h.get_watched("test::log");
    assert_eq!(
        log,
        Some(&Value::String(arcstr::literal!("r0/c0=new"))),
        "on_edit text submit should have fired with the new value",
    );
    Ok(())
}

/// A numeric edit buffer commits as an i64, not a string.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_text_column_parses_number() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let edit = |#path: string, #value: Any| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: edit }),
            display_name: null,
            source: &"1",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    h.dt_mut().handle_cell_edit(0, "c0".into());
    h.dt_mut().handle_cell_edit_input("42".into());
    h.dt_mut().handle_cell_edit_submit();
    h.drain().await?;
    let log = h.get_watched("test::log");
    assert_eq!(
        log,
        Some(&Value::String(arcstr::literal!("r0/c0=42"))),
        "numeric edit should commit typed i64, got: {log:?}",
    );
    Ok(())
}

/// Cancelling a text edit does not fire on_edit.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_text_cancel() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let edit = |#path: string, #value: Any| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: edit }),
            display_name: null,
            source: &"x",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    h.dt_mut().handle_cell_edit(0, "c0".into());
    h.dt_mut().handle_cell_edit_input("never-applied".into());
    h.dt_mut().handle_cell_edit_cancel();
    h.drain().await?;
    let log = h.get_watched("test::log");
    assert_eq!(
        log,
        Some(&Value::String(arcstr::literal!(""))),
        "cancel must not invoke on_edit, got: {log:?}",
    );
    Ok(())
}

/// Clicking a Toggle cell fires on_edit through the full pixel path.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_toggle_column() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let toggled = |#path: string, #value: bool| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Toggle({ on_edit: toggled }),
            display_name: null,
            source: &"false",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(500.0, 200.0))
            .await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
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

/// Selecting a Combo option fires on_edit with the choice id, not its
/// label.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_combo_column() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let pick = |#path: string, #value: string| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Combo({
                choices: [{id: "a", label: "Alpha"}, {id: "b", label: "Bravo"}],
                on_edit: pick
            }),
            display_name: null,
            source: &"a",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(500.0, 300.0))
            .await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
    let open_msgs = h.click(bounds.center());
    assert!(
        !open_msgs.iter().any(|m| matches!(m, Message::Call(_, _))),
        "opening the pick list should not fire on_edit; got {open_msgs:?}",
    );
    // The overlay opens below the cell; each option is ~22px tall.
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

/// Clicking a Spin cell's + button fires on_edit with the incremented
/// value.
#[tokio::test(flavor = "current_thread")]
async fn on_edit_spin_column() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let bumped = |#path: string, #value: f64| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Spin({ min: 0.0, max: 10.0, increment: 1.0, on_edit: bumped }),
            display_name: null,
            source: &"5.0",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(500.0, 200.0))
            .await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
    // The cell is a Row [−, label, +]; scanning from the right, the
    // first Call is the + button.
    let only_call = |msgs: &[Message]| -> Option<ValArray> {
        msgs.iter().find_map(|m| match m {
            Message::Call(_, args) => Some(args.clone()),
            _ => None,
        })
    };
    let mut hit_args: Option<ValArray> = None;
    for offset in (10..=70).rev().step_by(2) {
        let p = iced_core::Point::new(bounds.x + offset as f32, bounds.center().y);
        let msgs = h.click(p);
        if let Some(args) = only_call(&msgs) {
            hit_args = Some(args);
            h.inner.dispatch_calls(&msgs).await?;
            break;
        }
    }
    let args = hit_args.expect("no click position produced a Call on the spin cell");
    let v: Vec<_> = args.iter().collect();
    assert!(
        matches!(v.as_slice(),
            [Value::String(p), Value::F64(x)]
            if p == &arcstr::literal!("r0/c0") && (*x - 6.0).abs() < 1e-9),
        "rightmost spin button should be + (val 6.0), got args: {:?}",
        args,
    );
    assert_eq!(
        h.inner.get_watched("test::log"),
        Some(&Value::String(arcstr::literal!("r0/c0=6"))),
    );
    Ok(())
}

/// Clicking a Button cell fires on_click with (path, label).
#[tokio::test(flavor = "current_thread")]
async fn on_click_button_column() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
let pressed = |#path: string, #value: Any| log <- "[path]=[value]";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Button({ on_click: pressed }),
            display_name: null,
            source: &"Run",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(500.0, 200.0))
            .await?;
    let _ = h.inner.watch("test::log").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "c0").expect("c0 visible");
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

/// on_update fires when a subscribed cell receives a netidx value.
#[tokio::test(flavor = "current_thread")]
async fn on_update_fires_for_subscribed_cell() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
sys::net::publish("/local/dt7/r0/c0", v64:42);
let tbl = { rows: ["/local/dt7/r0"], columns: ["c0"] };
let result = data_table(
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
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

/// A per-row default backed by a graphix variable follows the variable.
#[tokio::test(flavor = "current_thread")]
async fn default_value_per_row_ref_updates() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let a = "v1";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &{"r0" => a},
            on_resize: &null, width: &null }
    ] };
let result = data_table(
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
        if h.dt_snapshot().grid[0][0] == "v1b" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[0][0], "v1b");
    Ok(())
}

/// `columns: []` plus virtual columns renders in Table mode, not Value
/// mode.
#[tokio::test(flavor = "current_thread")]
async fn virtual_columns_prevent_value_mode() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1"], columns: [
        { name: "region", typ: `Text({ on_edit: null }),
            display_name: "Region",
            source: &"prod",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let snap = h.dt_snapshot();
    assert!(!snap.is_value_mode, "should render as Table, not Value");
    assert_eq!(
        snap.col_names.iter().map(|s| s.as_str()).collect::<Vec<_>>(),
        vec!["region"]
    );
    Ok(())
}

/// A default_value ref over a connect-updated nested Map reflects every
/// update, not just the first (the data_table_calculated example).
#[tokio::test(flavor = "current_thread")]
async fn default_value_reactive_via_connect() -> Result<()> {
    // Absolute (subscribed) row paths are required to reproduce.
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use map::*; use opt;
sys::net::publish("/local/dt8b/r0/c0", v64:0);
sys::net::publish("/local/dt8b/r1/c0", v64:0);
let data: Map<string, Map<string, i64>> = {};
let push = |row: string, sum: i64| {
    let sums = opt::or_default(map::get(data, "sum"), {});
    data <- sum ~ map::insert(data, "sum", map::insert(sums, row, sum))
};
let tbl = {
    rows: ["/local/dt8b/r0", "/local/dt8b/r1"],
    columns: [
        "c0",
        { name: "sum", typ: `Text({ on_edit: null }),
            display_name: "A + B",
            source: &opt::or_default(map::get(data, "sum"), {}),
            on_resize: &null,
            width: &null }
    ]
};
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let snap = h.dt_snapshot();
    let sum_col =
        snap.col_names.iter().position(|n| n == "sum").expect("sum column present");
    let r0 = snap.row_basenames.iter().position(|n| n == "r0").unwrap();
    let r1 = snap.row_basenames.iter().position(|n| n == "r1").unwrap();
    assert_eq!(snap.grid[r0][sum_col], "");
    let push_id = h.compile_named_callable("test::push").await?;
    h.call_callback(
        push_id,
        ValArray::from_iter([Value::String(arcstr::literal!("r0")), Value::I64(5)]),
    )
    .await?;
    for _ in 0..5 {
        h.drain().await?;
        if h.dt_snapshot().grid[r0][sum_col] == "5" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[r0][sum_col], "5", "first update");
    h.call_callback(
        push_id,
        ValArray::from_iter([Value::String(arcstr::literal!("r0")), Value::I64(9)]),
    )
    .await?;
    for _ in 0..5 {
        h.drain().await?;
        if h.dt_snapshot().grid[r0][sum_col] == "9" {
            break;
        }
    }
    assert_eq!(h.dt_snapshot().grid[r0][sum_col], "9", "second update");
    h.call_callback(
        push_id,
        ValArray::from_iter([Value::String(arcstr::literal!("r1")), Value::I64(3)]),
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
    // Several pushes between drains must land the latest value.
    for v in [10i64, 20, 30, 40] {
        h.gx.call(
            push_id,
            ValArray::from_iter([Value::String(arcstr::literal!("r0")), Value::I64(v)]),
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

/// A uniform string default propagates to every grid cell.
#[tokio::test(flavor = "current_thread")]
async fn default_value_uniform_string() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1", "r2"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &"UNI",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
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

/// A column-width ref controls `dt_ref_width` for that column.
#[tokio::test(flavor = "current_thread")]
async fn column_width_ref_controlled() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let w = 120.0;
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &`Netidx(null),
            on_resize: &null,
            width: &w }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    assert_eq!(h.dt().dt_ref_width("c0"), Some(120.0));
    Ok(())
}

/// A resize drag yields `(on_resize_cb, new_width)` from
/// `handle_mouse_move_resize`.
#[tokio::test(flavor = "current_thread")]
async fn on_resize_fires_on_drag() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = 0.0;
let on_w = |new_w: f64| log <- new_w;
let w = 100.0;
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &"x",
            on_resize: &on_w,
            width: &w }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.watch("test::log").await?;
    h.drain().await?;
    let idx = h.dt().dt_meta_col_idx("c0").expect("c0 visible");
    h.dt_mut().handle_column_resize_start(idx, 100.0);
    // The first move only seeds the baseline.
    assert!(h.dt_mut().handle_mouse_move_resize(100.0).is_none());
    let result = h.dt_mut().handle_mouse_move_resize(180.0);
    let (cb_id, new_w) = result.expect("on_resize callback returned");
    h.dt_mut().handle_column_resize_end();
    h.call_callback(cb_id, ValArray::from_iter([Value::F64(new_w)])).await?;
    let log = h.get_watched("test::log");
    assert!(
        matches!(log, Some(Value::F64(f)) if *f > 100.0),
        "on_resize should have logged the new width, got: {log:?}",
    );
    Ok(())
}

/// A Sparkline column accumulates values published over netidx. The
/// timers are one-shot (a repeating timer keeps `drain()` spinning)
/// and spaced past subscriber setup.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_accumulates() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let c = 0;
let t1 = time::timer(duration:300.ms, false);
let t2 = time::timer(duration:450.ms, false);
let t3 = time::timer(duration:600.ms, false);
c <- t1 ~ 1;
c <- t2 ~ 2;
c <- t3 ~ 3;
sys::net::publish("/local/dt17/r0/load", c);
let tbl = { rows: ["/local/dt17/r0"], columns: [
        { name: "load", typ: `Sparkline({ history_seconds: 60.0, min: null, max: null }),
            display_name: null,
            source: &`Netidx(null),
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    h.wait_until(
        |h| {
            let vs = h.dt().dt_sparkline_values("r0", "load").unwrap_or_default();
            vs.contains(&1.0) && vs.contains(&2.0) && vs.contains(&3.0)
        },
        std::time::Duration::from_secs(2),
        "sparkline to receive all three timer-driven values",
    )
    .await?;
    // The initial value may or may not have been seen; the three timer
    // values must be.
    let vs = h.dt().dt_sparkline_values("r0", "load").unwrap();
    for expected in [1.0, 2.0, 3.0] {
        assert!(
            vs.contains(&expected),
            "sparkline missing timer value {expected}, got: {vs:?}",
        );
    }
    Ok(())
}

/// A Sparkline default that parses as f64 seeds the history.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_default_value_seeds_history() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0"], columns: [
        "anchor",
        { name: "spark", typ: `Sparkline({ history_seconds: 60.0, min: null, max: null }),
            display_name: null,
            source: &"7.5",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
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

/// Decimation caps stored sparkline points at `MAX_SPARKLINE_POINTS`.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_decimation_caps_length() -> Result<()> {
    use std::time::{Duration, Instant};
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0"], columns: [
        "anchor",
        { name: "load", typ: `Sparkline({ history_seconds: 60.0, min: null, max: null }),
            display_name: null,
            source: &"0.0",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let dt_w = h.dt();
    let base = Instant::now();
    for i in 0..2000_u64 {
        dt_w.dt_push_sparkline("r0", "load", base + Duration::from_micros(i), i as f64);
    }
    let len = dt_w.dt_sparkline_len("r0", "load").unwrap_or(0);
    assert!(len <= 512, "decimation should cap len <= 512, got {len}");
    for i in 2000..2500_u64 {
        dt_w.dt_push_sparkline("r0", "load", base + Duration::from_micros(i), i as f64);
    }
    let len = dt_w.dt_sparkline_len("r0", "load").unwrap_or(0);
    assert!(len <= 512, "after second burst, len <= 512, got {len}");
    Ok(())
}

/// Decimation preserves the extremes of the input.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_decimation_preserves_extremes() -> Result<()> {
    use std::time::{Duration, Instant};
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0"], columns: [
        "anchor",
        { name: "load", typ: `Sparkline({ history_seconds: 60.0, min: null, max: null }),
            display_name: null,
            source: &"0.0",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let dt_w = h.dt();
    let base = Instant::now();
    for i in 0..1024_u64 {
        dt_w.dt_push_sparkline("r0", "load", base + Duration::from_micros(i), i as f64);
    }
    let vals = dt_w.dt_sparkline_values("r0", "load").expect("sparkline values present");
    let min = vals.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = vals.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    assert!(min <= 1.0, "min should be near 0 after decimation, got {min}");
    assert!(max >= 1022.0, "max should be near 1023 after decimation, got {max}");
    Ok(())
}

/// Arrow keys move the selection and Enter fires on_activate.
/// on_select must feed `#selection` back or every key restarts from
/// the default cell.
#[tokio::test(flavor = "current_thread")]
async fn keyboard_nav_arrows_and_enter() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let sel = [];
let selected = "";
let activated = "";
let tbl = { rows: ["r0", "r1"], columns: ["c0", "c1"] };
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
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(500.0, 200.0))
            .await?;
    let _ = h.watch("test::selected").await?;
    let _ = h.watch("test::activated").await?;
    h.inner.drain().await?;
    let _ = h.view();
    // A click focuses the KeyboardArea.
    let msgs = h.click(iced_core::Point::new(100.0, 40.0));
    h.inner.dispatch_calls(&msgs).await?;

    let msgs = h.press_key(iced_core::keyboard::key::Named::ArrowRight);
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(
        h.get_watched("test::selected"),
        Some(&Value::String(arcstr::literal!("r0/c1")))
    );

    let msgs = h.press_key(iced_core::keyboard::key::Named::ArrowDown);
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(
        h.get_watched("test::selected"),
        Some(&Value::String(arcstr::literal!("r1/c1")))
    );

    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(
        h.get_watched("test::activated"),
        Some(&Value::String(arcstr::literal!("r1")))
    );
    Ok(())
}

/// Clicking the name column fires on_activate but not on_select.
#[tokio::test(flavor = "current_thread")]
async fn name_click_activate_only() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let sel_log = "";
let act_log = "";
let tbl = { rows: ["r0"], columns: ["c0"] };
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
    h.dt_mut().handle_cell_click(0, "name".into());
    h.drain().await?;
    assert_eq!(
        h.get_watched("test::act_log"),
        Some(&Value::String(arcstr::literal!("r0")))
    );
    assert_eq!(
        h.get_watched("test::sel_log"),
        Some(&Value::String(arcstr::literal!(""))),
        "on_select must not fire for the name column"
    );
    Ok(())
}

/// A second click on a resize handle within 400ms auto-fits, writing
/// `user_widths`.
#[tokio::test(flavor = "current_thread")]
async fn resize_handle_double_click_autofits() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let long = "wider than MIN_COL_WIDTH default";
let tbl = { rows: ["r0"], columns: [
        { name: "c0", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &long,
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    let _ = h.view();
    let idx = h.dt().dt_meta_col_idx("c0").expect("c0 visible");
    assert_eq!(h.dt().dt_user_width("c0"), None);
    h.dt_mut().handle_column_resize_start(idx, 100.0);
    h.dt_mut().handle_column_resize_start(idx, 100.0);
    let w = h.dt().dt_user_width("c0").expect("auto-fit writes user_widths");
    assert!(w > 80.0, "auto-fit width must exceed MIN_COL_WIDTH, got {w}");
    Ok(())
}

/// Viewport metrics update on every layout pass, including a pure
/// window resize.
#[tokio::test(flavor = "current_thread")]
async fn viewport_metrics_update_on_resize() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r0", "r1", "r2", "r3", "r4"], columns: ["c0", "c1", "c2"] };
let result = data_table(#table: &tbl)
"#;
    use iced_core::Size;
    let mut h = InteractionHarness::with_viewport(code, Size::new(800.0, 400.0)).await?;
    for _ in 0..20 {
        h.drain().await?;
    }
    let _ = h.view();
    let (w0, h0, rows0, cols0) = h.inner.dt().dt_viewport_metrics();
    assert!((w0 - 800.0).abs() < 0.5, "initial viewport_width ~800, got {w0}");
    assert!((h0 - 400.0).abs() < 0.5, "initial viewport_height ~400, got {h0}");
    assert!(rows0 > 1, "rows_in_view > 1 at 400px tall, got {rows0}");
    assert!(cols0 > 1, "cols_in_view > 1 at 800px wide, got {cols0}");

    h.resize(Size::new(200.0, 80.0));
    let (w1, h1, rows1, cols1) = h.inner.dt().dt_viewport_metrics();
    assert!((w1 - 200.0).abs() < 0.5, "post-shrink viewport_width ~200, got {w1}");
    assert!((h1 - 80.0).abs() < 0.5, "post-shrink viewport_height ~80, got {h1}");
    assert!(rows1 < rows0, "rows_in_view shrank: before {rows0}, after {rows1}");
    assert!(cols1 < cols0, "cols_in_view shrank: before {cols0}, after {cols1}");

    h.resize(Size::new(1200.0, 600.0));
    let (w2, h2, _rows2, _cols2) = h.inner.dt().dt_viewport_metrics();
    assert!((w2 - 1200.0).abs() < 0.5, "post-grow viewport_width ~1200, got {w2}");
    assert!((h2 - 600.0).abs() < 0.5, "post-grow viewport_height ~600, got {h2}");
    Ok(())
}

/// `col_at_offset` maps a scroll offset through the prefix sum of the
/// actual column widths.
#[tokio::test(flavor = "current_thread")]
async fn horizontal_scroll_variable_width() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let tbl = { rows: ["r"], columns: [
        { name: "a", typ: `Text({ on_edit: null }),
            display_name: null, source: &"a",
            on_resize: &null, width: &null },
        { name: "b", typ: `Text({ on_edit: null }),
            display_name: null, source: &"b",
            on_resize: &null, width: &null },
        { name: "c", typ: `Text({ on_edit: null }),
            display_name: null, source: &"c",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let h = dt(code).await?;
    let w = h.dt();
    w.dt_set_cached_width("\0__rowname__", 80.0);
    w.dt_set_cached_width("a", 60.0);
    w.dt_set_cached_width("b", 200.0);
    w.dt_set_cached_width("c", 140.0);
    assert_eq!(w.col_at_offset_for_test(0.0), 0, "ox=0 → first_col=0");
    // Midpoint boundaries: col b at 80 + 30 = 110, col c at 80 + 60 +
    // 100 = 240.
    assert_eq!(w.col_at_offset_for_test(100.0), 0, "ox=100 still in col a");
    assert_eq!(w.col_at_offset_for_test(200.0), 1, "ox=200 lands on col b");
    assert_eq!(w.col_at_offset_for_test(300.0), 2, "ox=300 lands on col c");
    Ok(())
}
/// A non-finite or non-positive sparkline `history_seconds` does not
/// panic on a live update.
#[tokio::test(flavor = "current_thread")]
async fn sparkline_history_seconds_rejects_negative() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let c = 0;
c <- time::timer(duration:100.ms, false) ~ 5;
sys::net::publish("/local/dt_hs/r0/load", c);
let tbl = { rows: ["/local/dt_hs/r0"], columns: [
        { name: "load", typ: `Sparkline({ history_seconds: -1.0, min: null, max: null }),
            display_name: null,
            source: &`Netidx(null),
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    h.wait_until(
        |h| {
            let vs = h.dt().dt_sparkline_values("r0", "load").unwrap_or_default();
            vs.contains(&5.0)
        },
        std::time::Duration::from_secs(2),
        "sparkline update to land without panicking on invalid history_seconds",
    )
    .await?;
    Ok(())
}

/// A Button column passes the raw `Value` to on_click, not its display
/// string.
#[tokio::test(flavor = "current_thread")]
async fn button_column_passes_typed_raw_value() -> Result<()> {
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let pressed_val: Any = null;
let pressed = |#path: string, #value: Any| {
    pressed_val <- value;
    null
};
let tbl = { rows: ["r0"], columns: [
        { name: "go", typ: `Button({ on_click: pressed }),
            display_name: null,
            source: &{"r0" => i64:7},
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h =
        InteractionHarness::with_viewport(code, iced_core::Size::new(500.0, 200.0))
            .await?;
    let _ = h.inner.watch("test::pressed_val").await?;
    h.inner.drain().await?;
    let _ = h.view();
    let bounds = h.inner.dt().dt_cell_bounds(0, "go").expect("go col visible");
    let p = iced_core::Point::new(bounds.x + 15.0, bounds.center().y);
    let msgs = h.click(p);
    expect_call_with_args(&msgs, |args| {
        let v: Vec<_> = args.iter().collect();
        matches!(
            v.as_slice(),
            [Value::String(p), Value::I64(7)]
            if p == &arcstr::literal!("r0/go"),
        )
    });
    h.inner.dispatch_calls(&msgs).await?;
    assert_eq!(
        h.inner.get_watched("test::pressed_val"),
        Some(&Value::I64(7)),
        "callback must receive the typed Value, not a string",
    );
    Ok(())
}
/// A virtual column never subscribes to `row_path/virtual_col`.
#[tokio::test(flavor = "current_thread")]
async fn virtual_col_does_not_create_subscription() -> Result<()> {
    // A publication at the virtual column's would-be path must not
    // shadow its default.
    let code = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
sys::net::publish("/local/dt_virt/r0/real", "real-val");
sys::net::publish("/local/dt_virt/r0/ghost", "from-publisher");
let tbl = { rows: ["/local/dt_virt/r0"], columns: [
        "real",
        { name: "ghost", typ: `Text({ on_edit: null }),
            display_name: null,
            source: &"from-default",
            on_resize: &null, width: &null }
    ] };
let result = data_table(
#table: &tbl
)
"#;
    let mut h = dt(code).await?;
    h.wait_until(
        |h| {
            let snap = h.dt_snapshot();
            let real_i = snap.col_names.iter().position(|n| n == "real");
            real_i.map(|i| snap.grid[0][i] == "real-val").unwrap_or(false)
        },
        std::time::Duration::from_secs(2),
        "real column value to arrive",
    )
    .await?;
    let snap = h.dt_snapshot();
    let ghost_i =
        snap.col_names.iter().position(|n| n == "ghost").expect("ghost column present");
    assert_eq!(
        snap.grid[0][ghost_i], "from-default",
        "virtual column must use its default, not a coincident publication",
    );
    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    h.drain().await?;
    let snap = h.dt_snapshot();
    assert_eq!(
        snap.grid[0][ghost_i], "from-default",
        "virtual column default must persist — no late subscription override",
    );
    Ok(())
}
