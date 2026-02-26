//! Plotters DrawingBackend implementation targeting an iced Canvas Frame.
//!
//! Translates plotters drawing operations to iced Frame methods for
//! GPU-accelerated chart rendering.

use super::Renderer;
use iced_core::{Color, Point, Size};
use iced_widget::canvas::{Frame, Path, Stroke};
use plotters_backend::{
    BackendColor, BackendCoord, BackendStyle, BackendTextStyle, DrawingBackend,
    DrawingErrorKind,
};
use std::convert::Infallible;

/// Wraps an iced Canvas Frame as a plotters DrawingBackend.
pub(crate) struct IcedBackend<'a> {
    frame: &'a mut Frame<Renderer>,
    width: u32,
    height: u32,
}

impl<'a> IcedBackend<'a> {
    pub fn new(frame: &'a mut Frame<Renderer>, width: u32, height: u32) -> Self {
        Self { frame, width, height }
    }
}

fn to_color(c: BackendColor) -> Color {
    let (r, g, b) = c.rgb;
    Color::from_rgba8(r, g, b, c.alpha as f32)
}

fn to_point(c: BackendCoord) -> Point {
    Point::new(c.0 as f32, c.1 as f32)
}

impl DrawingBackend for IcedBackend<'_> {
    type ErrorType = Infallible;

    fn get_size(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn ensure_prepared(&mut self) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }

    fn present(&mut self) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        Ok(())
    }

    fn draw_pixel(
        &mut self,
        point: BackendCoord,
        color: BackendColor,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if color.alpha == 0.0 {
            return Ok(());
        }
        self.frame.fill_rectangle(to_point(point), Size::new(1.0, 1.0), to_color(color));
        Ok(())
    }

    fn draw_line<S: BackendStyle>(
        &mut self,
        from: BackendCoord,
        to: BackendCoord,
        style: &S,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if style.color().alpha == 0.0 {
            return Ok(());
        }
        let path = Path::line(to_point(from), to_point(to));
        self.frame.stroke(
            &path,
            Stroke::default()
                .with_color(to_color(style.color()))
                .with_width(style.stroke_width() as f32),
        );
        Ok(())
    }

    fn draw_rect<S: BackendStyle>(
        &mut self,
        upper_left: BackendCoord,
        bottom_right: BackendCoord,
        style: &S,
        fill: bool,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if style.color().alpha == 0.0 {
            return Ok(());
        }
        let tl = to_point(upper_left);
        let size = Size::new(
            (bottom_right.0 - upper_left.0) as f32,
            (bottom_right.1 - upper_left.1) as f32,
        );
        if fill {
            self.frame.fill_rectangle(tl, size, to_color(style.color()));
        } else {
            let path = Path::rectangle(tl, size);
            self.frame.stroke(
                &path,
                Stroke::default()
                    .with_color(to_color(style.color()))
                    .with_width(style.stroke_width() as f32),
            );
        }
        Ok(())
    }

    fn draw_path<S: BackendStyle, I: IntoIterator<Item = BackendCoord>>(
        &mut self,
        path: I,
        style: &S,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if style.color().alpha == 0.0 {
            return Ok(());
        }
        let iced_path = Path::new(|builder| {
            let mut first = true;
            for pt in path {
                let p = to_point(pt);
                if first {
                    builder.move_to(p);
                    first = false;
                } else {
                    builder.line_to(p);
                }
            }
        });
        self.frame.stroke(
            &iced_path,
            Stroke::default()
                .with_color(to_color(style.color()))
                .with_width(style.stroke_width() as f32),
        );
        Ok(())
    }

    fn draw_circle<S: BackendStyle>(
        &mut self,
        center: BackendCoord,
        radius: u32,
        style: &S,
        fill: bool,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if style.color().alpha == 0.0 {
            return Ok(());
        }
        let path = Path::circle(to_point(center), radius as f32);
        if fill {
            self.frame.fill(&path, to_color(style.color()));
        } else {
            self.frame.stroke(
                &path,
                Stroke::default()
                    .with_color(to_color(style.color()))
                    .with_width(style.stroke_width() as f32),
            );
        }
        Ok(())
    }

    fn fill_polygon<S: BackendStyle, I: IntoIterator<Item = BackendCoord>>(
        &mut self,
        vert: I,
        style: &S,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if style.color().alpha == 0.0 {
            return Ok(());
        }
        let path = Path::new(|builder| {
            let mut first = true;
            for pt in vert {
                let p = to_point(pt);
                if first {
                    builder.move_to(p);
                    first = false;
                } else {
                    builder.line_to(p);
                }
            }
            builder.close();
        });
        self.frame.fill(&path, to_color(style.color()));
        Ok(())
    }

    fn draw_text<TStyle: BackendTextStyle>(
        &mut self,
        text: &str,
        style: &TStyle,
        pos: BackendCoord,
    ) -> Result<(), DrawingErrorKind<Self::ErrorType>> {
        if style.color().alpha == 0.0 {
            return Ok(());
        }
        self.frame.fill_text(iced_widget::canvas::Text {
            content: text.to_string(),
            position: to_point(pos),
            color: to_color(style.color()),
            size: (style.size() as f32).into(),
            ..iced_widget::canvas::Text::default()
        });
        Ok(())
    }

    fn estimate_text_size<TStyle: BackendTextStyle>(
        &self,
        text: &str,
        style: &TStyle,
    ) -> Result<(u32, u32), DrawingErrorKind<Self::ErrorType>> {
        let size = style.size();
        let width = (text.len() as f64 * size * 0.6) as u32;
        let height = (size * 1.2) as u32;
        Ok((width, height))
    }
}
