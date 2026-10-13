// gui-chart-03: a one-category bar chart puts its category tick label at
// plot_width/2 from the canvas edge instead of the plot's centre.
//
// Standalone plotters 0.3.7 probe mirroring draw.rs ChartMode::Bar
// (margin, label areas, `categories.as_slice().into_segmented()`,
// Histogram::vertical, configure_mesh). Build without cargo:
//   D=~/tmp/target/debug/deps; rustc --edition 2021 gui-chart-03.rs -L $D \
//     --extern plotters=$D/libplotters-<hash>.rlib \
//     --extern plotters_backend=$D/libplotters_backend-<hash>.rlib -o probe && ./probe
// Observed:
//   1 categories: plot x range 50..390, bar x 55..384, label "only" at x 169
//   2 categories: bars 55..214 and 224..384, labels at 134 and 304
// Expected: the one label at the plot centre (~219), as the two-category
// labels sit at their bars' centres. The bar itself is placed correctly:
// RangedSlice::map's 0/0 = NaN gives pixel 0, but plotters truncates
// element coordinates to the plotting area (BackendCoordOnly::map), so
// the bar is NOT drawn over the y labels; mesh labels are not truncated.
use plotters::prelude::*;
use plotters::coord::ranged1d::IntoSegmentedCoord;
use plotters_backend::{BackendColor, BackendCoord, BackendStyle, BackendTextStyle, DrawingErrorKind};
use std::cell::RefCell;

struct Rec { rects: RefCell<Vec<(BackendCoord, BackendCoord, (u8,u8,u8))>> }
impl DrawingBackend for &Rec {
    type ErrorType = std::io::Error;
    fn get_size(&self) -> (u32, u32) { (400, 300) }
    fn ensure_prepared(&mut self) -> Result<(), DrawingErrorKind<Self::ErrorType>> { Ok(()) }
    fn present(&mut self) -> Result<(), DrawingErrorKind<Self::ErrorType>> { Ok(()) }
    fn draw_pixel(&mut self, _: BackendCoord, _: BackendColor) -> Result<(), DrawingErrorKind<Self::ErrorType>> { Ok(()) }
    fn draw_rect<S: BackendStyle>(&mut self, a: BackendCoord, b: BackendCoord, s: &S, fill: bool) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if fill { self.rects.borrow_mut().push((a, b, s.color().rgb)); }
        Ok(())
    }
    fn estimate_text_size<TStyle: BackendTextStyle>(&self, t: &str, _: &TStyle) -> Result<(u32, u32), DrawingErrorKind<Self::ErrorType>> { Ok((t.len() as u32 * 7, 12)) }
    fn draw_text<TStyle: BackendTextStyle>(&mut self, t: &str, _: &TStyle, p: BackendCoord) -> Result<(), DrawingErrorKind<Self::ErrorType>> { if !t.contains('.') { println!("  text {t:?} at {p:?}"); } Ok(()) }
}

fn run(cats: &[String]) {
    let rec = Rec { rects: RefCell::new(vec![]) };
    {
        let root = (&rec).into_drawing_area();
        let mut b = ChartBuilder::on(&root);
        b.margin(10).x_label_area_size(20).y_label_area_size(40);
        let chart = b.build_cartesian_2d(cats.into_segmented(), 0.0..6.0).unwrap();
        let (px, _) = chart.plotting_area().get_pixel_range();
        let mut chart = chart; chart.configure_mesh().x_labels(cats.len()).draw().unwrap();
        rec.rects.borrow_mut().clear();
        let mut chart = chart;
        chart.draw_series(Histogram::vertical(&chart).style(RED.filled()).margin(5).baseline(0.0)
            .data(cats.iter().map(|c| (c, 5.0)))).unwrap();
        println!("{} categories: plot x range {:?}", cats.len(), px);
        for (a, b, _) in rec.rects.borrow().iter() { println!("  bar x {}..{}", a.0, b.0); }
    }
}
fn main() {
    run(&["only".to_string()]);
    run(&["a".to_string(), "b".to_string()]);
}
