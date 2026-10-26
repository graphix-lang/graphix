// Title: a time-series view panned or zoomed past the end of chrono's range panics the GUI
// Run: copy to stdlib/graphix-package-gui/tests/review_gui_chart_1.rs, then
//   cargo test -p graphix-package-gui --test review_gui_chart_1 -- --nocapture
// Expected: every case draws (Ok).
// Observed: "a view panned past the end" -> time_range gives (hi - HOUR, hi) and
//   plotters panics at datetime.rs:690 "`DateTime + TimeDelta` overflowed".
//! Review repro gui-chart: a time-series x range that ends at the end of
//! chrono's range, as time_range produces it, panics plotters' key-point
//! loop (`DateTime + TimeDelta` overflowed). Mirrors draw_xy_chart's time
//! path: build_cartesian_2d over ms_datetime(x.0)..ms_datetime(x.1), then
//! the mesh.
use chrono::{DateTime, Utc};
use graphix_package_gui::widgets::chart::{datetime_ms, ms_datetime, time_range};
use plotters::prelude::*;
use plotters_backend::{
    BackendColor, BackendCoord, BackendStyle, BackendTextStyle, DrawingErrorKind,
};
use std::panic::{AssertUnwindSafe, catch_unwind};

struct Null;

impl DrawingBackend for Null {
    type ErrorType = std::convert::Infallible;
    fn get_size(&self) -> (u32, u32) {
        (800, 600)
    }
    fn ensure_prepared(&mut self) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }
    fn present(&mut self) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }
    fn draw_pixel(
        &mut self,
        _: BackendCoord,
        _: BackendColor,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }
    fn draw_line<S: BackendStyle>(
        &mut self,
        _: BackendCoord,
        _: BackendCoord,
        _: &S,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }
    fn draw_text<T: BackendTextStyle>(
        &mut self,
        _: &str,
        _: &T,
        _: BackendCoord,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }
    fn estimate_text_size<T: BackendTextStyle>(
        &self,
        text: &str,
        _: &T,
    ) -> Result<(u32, u32), DrawingErrorKind<Self::ErrorType>> {
        Ok((text.len() as u32 * 8, 14))
    }
}

fn draw(x: (f64, f64)) -> Result<(), String> {
    let x = time_range(x);
    catch_unwind(AssertUnwindSafe(|| {
        let root = Null.into_drawing_area();
        let mut b = ChartBuilder::on(&root);
        b.margin(10).x_label_area_size(30).y_label_area_size(40);
        let mut chart =
            b.build_cartesian_2d(ms_datetime(x.0)..ms_datetime(x.1), 0.0..1.0).unwrap();
        chart.configure_mesh().draw().unwrap();
    }))
    .map_err(|e| {
        e.downcast_ref::<&str>()
            .map(|s| s.to_string())
            .or_else(|| e.downcast_ref::<String>().cloned())
            .unwrap_or_default()
    })
}

#[test]
fn time_ranges_at_the_ends_of_time() {
    let hi = datetime_ms(&DateTime::<Utc>::MAX_UTC);
    let lo = datetime_ms(&DateTime::<Utc>::MIN_UTC);
    let p = |s: &str| datetime_ms(&s.parse::<DateTime<Utc>>().unwrap());
    let pt = p("+262142-12-31T00:00:00Z");
    let cases = [
        ("a day in 2024", (p("2024-01-01T00:00:00Z"), p("2024-01-02T00:00:00Z"))),
        ("one point late in year 262142, padded an hour", (pt - 4e6, pt + 4e6)),
        ("a view panned past the end", (hi + 1e15, hi + 2e15)),
        ("a 100-year view ending past the end", (hi - 3.15e12, hi + 1e12)),
        ("a view panned before the start", (lo - 2e15, lo - 1e15)),
    ];
    let mut failed = 0;
    for (what, x) in cases {
        let r = draw(x);
        println!("{what}: {x:?} -> {:?}: {r:?}", time_range(x));
        failed += r.is_err() as usize;
    }
    assert_eq!(failed, 0);
}
