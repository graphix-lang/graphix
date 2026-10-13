// gui-chart-01: inside a scrolled iced Scrollable, a Canvas program's
// CursorMoved `position` minus its `bounds` (what ChartState::handle_event
// in src/widgets/chart/interact.rs computes) is not the cursor's place in
// the canvas: the Scrollable translates the cursor, never the event.
// Run: copy to stdlib/graphix-package-gui/tests/review_gui_chart_01.rs, then
//   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//     --test review_gui_chart_01 -- --nocapture
// Expected: after a 500px scroll, position - bounds equals the cursor's
// place in the canvas. Observed: position - bounds = (200, 150), cursor
// position_in(bounds) = (200, 650). The test asserts the difference, so it
// passes while the defect is present.
use iced_core::{
    Element, Length, Point, Rectangle, Shell, Size, clipboard,
    event::Event,
    layout::{Layout, Limits},
    mouse,
    widget::Tree,
};
use iced_widget::{Action, Canvas, Scrollable, Theme, canvas};
use std::{sync::Mutex, thread, time::Duration};

#[derive(Default)]
struct Rec(Mutex<Vec<(Point, Rectangle, mouse::Cursor)>>);

impl canvas::Program<(), Theme, ()> for Rec {
    type State = ();
    fn update(
        &self,
        _: &mut (),
        event: &Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<Action<()>> {
        if let Event::Mouse(mouse::Event::CursorMoved { position }) = event {
            self.0.lock().unwrap().push((*position, bounds, cursor));
        }
        None
    }
    fn draw(
        &self,
        _: &(),
        _: &(),
        _: &Theme,
        _: Rectangle,
        _: mouse::Cursor,
    ) -> Vec<canvas::Geometry<()>> {
        vec![]
    }
}

#[test]
fn cursor_moved_position_is_not_translated_by_scrollable() {
    let rec = Rec::default();
    let cv = Canvas::new(&rec).width(Length::Fixed(400.)).height(Length::Fixed(2000.));
    let mut el: Element<(), Theme, ()> =
        Scrollable::new(cv).height(Length::Fixed(300.)).width(Length::Fixed(400.)).into();
    let mut tree = Tree::new(&el);
    let node = el.as_widget_mut().layout(
        &mut tree,
        &(),
        &Limits::new(Size::ZERO, Size::new(400., 300.)),
    );
    let viewport = Rectangle::new(Point::ORIGIN, Size::new(400., 300.));
    let at = Point::new(200., 150.);
    let mut send = |ev: Event| {
        let mut msgs = vec![];
        let mut shell = Shell::new(&mut msgs);
        el.as_widget_mut().update(
            &mut tree,
            &ev,
            Layout::new(&node),
            mouse::Cursor::Available(at),
            &(),
            &mut clipboard::Null,
            &mut shell,
            &viewport,
        );
    };
    send(Event::Mouse(mouse::Event::CursorMoved { position: at }));
    send(Event::Mouse(mouse::Event::WheelScrolled {
        delta: mouse::ScrollDelta::Pixels { x: 0., y: -500. },
    }));
    thread::sleep(Duration::from_millis(150));
    send(Event::Mouse(mouse::Event::CursorMoved { position: at }));
    let seen = rec.0.lock().unwrap();
    for (position, bounds, cursor) in seen.iter() {
        let raw = Point::new(position.x - bounds.x, position.y - bounds.y);
        let local = cursor.position_in(*bounds);
        eprintln!("bounds {bounds:?} position {position:?} cursor {cursor:?}");
        eprintln!("  handle_event's local (position - bounds) = {raw:?}");
        eprintln!("  cursor.position_in(bounds)               = {local:?}");
    }
    let (position, bounds, cursor) = *seen.last().expect("canvas saw the move");
    let raw = Point::new(position.x - bounds.x, position.y - bounds.y);
    let local = cursor.position_in(bounds).expect("cursor over the canvas");
    assert!(
        (raw.y - local.y).abs() > 100.,
        "after scrolling, position - bounds agrees with the cursor: no defect"
    );
}
