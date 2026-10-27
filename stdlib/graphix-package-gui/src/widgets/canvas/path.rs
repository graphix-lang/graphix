use iced_core::{Point, Size};
use iced_widget::canvas::{
    Path,
    path::{
        Builder,
        lyon_path::{Event, geom},
    },
};

pub(super) struct CheckedPath {
    builder: Option<Builder>,
    current: geom::Point<f64>,
    first: Option<geom::Point<f64>>,
}

fn point(p: Point) -> geom::Point<f64> {
    geom::Point::new(p.x as f64, p.y as f64)
}

fn bounded(p: geom::Point<f64>) -> bool {
    p.x.abs() <= super::MAX_EXTENT && p.y.abs() <= super::MAX_EXTENT
}

fn iced(p: geom::Point<f64>) -> Point {
    Point::new(p.x as f32, p.y as f32)
}

impl CheckedPath {
    pub(super) fn build(f: impl FnOnce(&mut Self)) -> Option<Path> {
        let mut b = Self {
            builder: Some(Builder::new()),
            current: geom::Point::zero(),
            first: None,
        };
        f(&mut b);
        let path = b.builder?.build();
        let valid = path.raw().iter().all(|event| {
            let finite = |p: geom::Point<f32>| bounded(p.cast());
            match event {
                Event::Begin { at } => finite(at),
                Event::Line { from, to } => finite(from) && finite(to),
                Event::Quadratic { from, ctrl, to } => {
                    finite(from) && finite(ctrl) && finite(to)
                }
                Event::Cubic { from, ctrl1, ctrl2, to } => {
                    finite(from) && finite(ctrl1) && finite(ctrl2) && finite(to)
                }
                Event::End { first, last, .. } => finite(first) && finite(last),
            }
        });
        valid.then_some(path)
    }

    fn checked(&mut self, points: &[geom::Point<f64>]) -> Option<&mut Builder> {
        if !points.iter().copied().all(bounded) {
            self.builder = None;
        }
        self.builder.as_mut()
    }

    fn move_point(&mut self, p: geom::Point<f64>) {
        if let Some(b) = self.checked(&[p]) {
            b.move_to(iced(p));
        }
        self.current = p;
        self.first = Some(p);
    }

    fn line_point(&mut self, p: geom::Point<f64>) {
        if let Some(b) = self.checked(&[p]) {
            b.line_to(iced(p));
        }
        self.first.get_or_insert(p);
        self.current = p;
    }

    fn quadratic_points(&mut self, ctrl: geom::Point<f64>, to: geom::Point<f64>) {
        if let Some(b) = self.checked(&[ctrl, to]) {
            b.quadratic_curve_to(iced(ctrl), iced(to));
        }
        self.first.get_or_insert(to);
        self.current = to;
    }

    pub(super) fn move_to(&mut self, p: Point) {
        self.move_point(point(p));
    }

    pub(super) fn line_to(&mut self, p: Point) {
        self.line_point(point(p));
    }

    pub(super) fn bezier_curve_to(&mut self, a: Point, b: Point, to: Point) {
        if let Some(builder) = self.checked(&[point(a), point(b), point(to)]) {
            builder.bezier_curve_to(a, b, to);
        }
        self.first.get_or_insert(point(to));
        self.current = point(to);
    }

    pub(super) fn quadratic_curve_to(&mut self, control: Point, to: Point) {
        self.quadratic_points(point(control), point(to));
    }

    pub(super) fn ellipse(&mut self, arc: iced_widget::canvas::path::arc::Elliptical) {
        let arc = geom::Arc {
            center: point(arc.center),
            radii: geom::Vector::new(arc.radii.x as f64, arc.radii.y as f64),
            x_rotation: geom::Angle::radians(arc.rotation.0 as f64),
            start_angle: geom::Angle::radians(arc.start_angle.0 as f64),
            sweep_angle: geom::Angle::radians(
                arc.end_angle.0 as f64 - arc.start_angle.0 as f64,
            ),
        };
        self.move_point(arc.sample(0.0));
        arc.for_each_quadratic_bezier(&mut |curve| {
            self.quadratic_points(curve.ctrl, curve.to)
        });
    }

    pub(super) fn arc(&mut self, arc: iced_widget::canvas::path::Arc) {
        self.ellipse(arc.into());
    }

    pub(super) fn circle(&mut self, center: Point, radius: f32) {
        self.arc(iced_widget::canvas::path::Arc {
            center,
            radius,
            start_angle: iced_core::Radians(0.0),
            end_angle: iced_core::Radians(std::f32::consts::TAU),
        });
        self.close();
    }

    fn rectangle_corners(top_left: Point, size: Size) -> [geom::Point<f64>; 4] {
        let p = point(top_left);
        let x = p.x + size.width as f64;
        let y = p.y + size.height as f64;
        [p, geom::Point::new(x, p.y), geom::Point::new(x, y), geom::Point::new(p.x, y)]
    }

    pub(super) fn rectangle_fits(top_left: Point, size: Size) -> bool {
        Self::rectangle_corners(top_left, size).into_iter().all(bounded)
    }

    pub(super) fn rectangle(&mut self, top_left: Point, size: Size) {
        let [a, b, c, d] = Self::rectangle_corners(top_left, size);
        self.move_point(a);
        for p in [b, c, d] {
            self.line_point(p);
        }
        self.close();
    }

    pub(super) fn rounded_rectangle(&mut self, top_left: Point, size: Size, radius: f32) {
        let p = point(top_left);
        let w = size.width as f64;
        let h = size.height as f64;
        let r = (radius as f64).min(w / 2.0).min(h / 2.0);
        self.move_point(geom::Point::new(p.x + r, p.y));
        for (a, b, c) in [
            ((p.x + w - r, p.y), (p.x + w, p.y), (p.x + w, p.y + r)),
            ((p.x + w, p.y + h - r), (p.x + w, p.y + h), (p.x + w - r, p.y + h)),
            ((p.x + r, p.y + h), (p.x, p.y + h), (p.x, p.y + h - r)),
            ((p.x, p.y + r), (p.x, p.y), (p.x + r, p.y)),
        ] {
            self.line_point(geom::Point::new(a.0, a.1));
            self.arc_points(geom::Point::new(b.0, b.1), geom::Point::new(c.0, c.1), r);
        }
        self.close();
    }

    pub(super) fn arc_to(&mut self, a: Point, b: Point, radius: f32) {
        self.arc_points(point(a), point(b), radius as f64);
    }

    fn arc_points(&mut self, a: geom::Point<f64>, b: geom::Point<f64>, radius: f64) {
        let start = self.current;
        if start == a || a == b || radius == 0.0 {
            self.line_point(a);
            return;
        }
        let to_start = (start - a).normalize();
        let to_end = (b - a).normalize();
        let cos = to_start.dot(to_end).clamp(-1.0, 1.0);
        if !cos.is_finite() || cos.abs() >= 1.0 - f32::EPSILON as f64 {
            self.line_point(a);
            return;
        }
        let distance = radius / (cos.acos() / 2.0).tan();
        let from = a + to_start * distance;
        let to = a + to_end * distance;
        self.line_point(from);
        let arc = geom::SvgArc {
            from,
            to,
            radii: geom::Vector::new(radius.abs(), radius.abs()),
            x_rotation: geom::Angle::radians(0.0),
            flags: geom::ArcFlags {
                large_arc: false,
                sweep: to_start.cross(to_end) < 0.0,
            },
        };
        arc.for_each_quadratic_bezier(&mut |curve| {
            self.quadratic_points(curve.ctrl, curve.to)
        });
    }

    pub(super) fn close(&mut self) {
        if let Some(b) = &mut self.builder {
            b.close();
        }
        if let Some(first) = self.first {
            self.current = first;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn huge_finite_line_is_rejected() {
        assert!(
            CheckedPath::build(|b| {
                b.move_to(Point::new(-1e30, 10.0));
                b.line_to(Point::new(1e30, 20.0));
            })
            .is_none()
        );
    }

    #[test]
    fn turns_curve_and_reversals_fall_back_to_a_line() {
        let turn = CheckedPath::build(|b| {
            b.move_to(Point::ORIGIN);
            b.arc_to(Point::new(100.0, 0.0), Point::new(100.0, 100.0), 10.0);
        })
        .unwrap();
        assert!(turn.raw().iter().any(|e| matches!(e, Event::Quadratic { .. })));
        let reversal = CheckedPath::build(|b| {
            b.move_to(Point::ORIGIN);
            b.arc_to(Point::new(100.0, 0.0), Point::new(0.0, 0.00001), 10.0);
        })
        .unwrap();
        assert!(reversal.raw().iter().any(
            |e| matches!(e, Event::Line { to, .. } if to == geom::Point::new(100.0,0.0))
        ));
        assert!(!reversal.raw().iter().any(|e| matches!(e, Event::Quadratic { .. })));
    }

    #[test]
    fn overflowing_rectangles_and_ellipses_are_rejected() {
        assert!(
            CheckedPath::build(
                |b| b.rectangle(Point::new(3e38, 0.0), Size::new(3e38, 10.0))
            )
            .is_none()
        );
        assert!(
            CheckedPath::build(|b| b.rounded_rectangle(
                Point::new(3e38, 0.0),
                Size::new(3e38, 10.0),
                2.0
            ))
            .is_none()
        );
        assert!(
            CheckedPath::build(|b| b.ellipse(
                iced_widget::canvas::path::arc::Elliptical {
                    center: Point::new(3e38, 0.0),
                    radii: iced_core::Vector::new(3e38, 1.0),
                    rotation: iced_core::Radians(0.0),
                    start_angle: iced_core::Radians(0.0),
                    end_angle: iced_core::Radians(1.0),
                }
            ))
            .is_none()
        );
    }
}
