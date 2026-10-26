use super::{GuiW, GuiWidget, Handler, IcedElement, Message, Renderer};
use crate::{theme::GraphixTheme, types::LengthV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle};
use iced_core::{
    Clipboard, Element, Event, Length, Rectangle, Shell, Size, Vector, Widget,
    keyboard::{self, Key, Modifiers, key::Named},
    layout::{self, Layout},
    mouse, overlay, renderer,
    widget::{
        Operation,
        tree::{self, Tree},
    },
};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

/// Helper: map a dimension kind tag to its TRef inner type.
macro_rules! slider_dim_type {
    (length) => { LengthV };
    (scalar) => { Option<f64> };
}

/// Helper: apply a dimension value to the iced slider widget.
macro_rules! slider_dim_set {
    (length, $self:ident, $sl:ident, $dim:ident) => {
        if let Some(v) = $self.p.$dim.t.as_ref() {
            $sl = $sl.$dim(v.0);
        }
    };
    (scalar, $self:ident, $sl:ident, $dim:ident) => {
        if let Some(Some(v)) = $self.p.$dim.t {
            $sl = $sl.$dim(v as f32);
        }
    };
}

/// Generate a horizontal or vertical slider widget. Dims are passed in
/// alphabetical order with a kind tag: `length` (primary axis) or
/// `scalar` (cross axis).
// CR claude for claude: [doc-drift/low] The doc above says the dims are passed in
// alphabetical order. graphix_rt::props! looks each field up by name (compile_field(v,
// stringify!(field))), so order has no effect, and the struct below already lists value
// before height and width. Drop the ordering sentence and keep what the kind tags mean.
// Found by reading; not run. (gui-widgets-b-05)
macro_rules! slider_widget {
    ($name:ident, $props:ident, $label:literal, $Widget:ident,
     $dim1:ident: $kind1:tt, $dim2:ident: $kind2:tt) => {
        graphix_rt::props! {
            struct $props {
                disabled: bool,
                max: f64,
                min: f64,
                step: Option<f64>,
                value: f64,
                $dim1: slider_dim_type!($kind1),
                $dim2: slider_dim_type!($kind2),
            }
        }

        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            p: $props<X>,
            on_change: Handler<X>,
            on_release: Handler<X>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(
                gx: GXHandle<X>,
                source: Value,
            ) -> Result<GuiW<X>> {
                let (p, on_change, on_release) = try_join!(
                    $props::compile(&gx, &source),
                    Handler::field(&gx, &source, "on_change"),
                    Handler::field(&gx, &source, "on_release"),
                )
                .context($label)?;
                Ok(Box::new(Self { gx, p, on_change, on_release }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = self.p.update(id, v).context($label)?;
                self.on_change
                    .update(rt, &self.gx, id, v, &mut changed)
                    .context(concat!($label, " on_change"))?;
                self.on_release
                    .update(rt, &self.gx, id, v, &mut changed)
                    .context(concat!($label, " on_release"))?;
                Ok(changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let val = self.p.value.t.unwrap_or(0.0);
                let min = self.p.min.t.unwrap_or(0.0);
                let max = self.p.max.t.unwrap_or(100.0);
                let bounds = Bounds::new(min, max);
                let disabled = self.p.disabled.t.unwrap_or(false) || bounds.is_none();
                let val = if bounds.is_some() { val } else { 0.0 };
                let bounds = bounds.unwrap_or_default();
                let step = self.p.step.t.flatten().filter(|s| s.is_finite() && *s > 0.0);
                let continuous = step.is_none();
                let scale = Scale::new(bounds, step);
                let (max, val, step) = match scale {
                    Scale::Steps { count, .. } => (count, scale.position(val), 1.0),
                    Scale::Fraction(bounds) => (
                        if bounds.min == bounds.max { 0.0 } else { 1.0 },
                        bounds.fraction(val),
                        step.map_or(1e-6, |s| {
                            (s / 2.0) / (bounds.max / 2.0 - bounds.min / 2.0)
                        })
                        .max(f64::MIN_POSITIVE),
                    ),
                };
                let on_change_id = if disabled { None } else { self.on_change.id() };
                let mut sl = widget::$Widget::new(0.0..=max, val, move |v| {
                    slider_change(on_change_id, scale, v)
                });
                sl = sl.step(step);
                if let Some(id) = self.on_release.id().filter(|_| !disabled) {
                    sl = sl.on_release(Message::Call(
                        id,
                        ValArray::from_iter([Value::Null]),
                    ));
                }
                slider_dim_set!($kind1, self, sl, $dim1);
                slider_dim_set!($kind2, self, sl, $dim2);
                if continuous {
                    Element::new(SliderInput {
                        content: sl,
                        bounds,
                        value: val,
                        on_change: on_change_id,
                    })
                } else {
                    sl.into()
                }
            }
        }
    };
}

slider_widget!(SliderW, SliderProps, "slider", Slider, height: scalar, width: length);
slider_widget!(VerticalSliderW, VerticalSliderProps, "vslider", VerticalSlider, height: length, width: scalar);

#[derive(Clone, Copy)]
struct Bounds {
    min: f64,
    max: f64,
}

impl Default for Bounds {
    fn default() -> Self {
        Self { min: 0.0, max: 1.0 }
    }
}

impl Bounds {
    fn new(min: f64, max: f64) -> Option<Self> {
        (min.is_finite() && max.is_finite() && min <= max).then_some(Self { min, max })
    }

    fn fraction(self, value: f64) -> f64 {
        if self.min == self.max {
            return 0.0;
        }
        let value = value.max(self.min).min(self.max);
        let span = self.max - self.min;
        if span.is_finite() {
            (value - self.min) / span
        } else {
            (value / 2.0 - self.min / 2.0) / (self.max / 2.0 - self.min / 2.0)
        }
    }

    fn value(self, fraction: f64) -> f64 {
        if fraction <= 0.0 {
            self.min
        } else if fraction >= 1.0 {
            self.max
        } else {
            ((1.0 - fraction) * self.min + fraction * self.max).clamp(self.min, self.max)
        }
    }
}

/// What iced's slider holds in place of the program's value: a step
/// count when the steps can be counted, else a fraction of the bounds.
/// Both of iced's input paths, a drag and the arrows or the wheel, then
/// land on the program's lattice, min + k * step.
#[derive(Clone, Copy)]
enum Scale {
    Steps { min: f64, max: f64, step: f64, count: f64 },
    Fraction(Bounds),
}

impl Scale {
    fn new(bounds: Bounds, step: Option<f64>) -> Self {
        match step {
            Some(step) if ((bounds.max - bounds.min) / step).is_finite() => Self::Steps {
                min: bounds.min,
                max: bounds.max,
                step,
                // CR claude for claude: [bug/medium] The step count is floor((max -
                // min) / step), and that quotient often comes out a hair under an
                // integer in floating point: 0.3 / 0.1 is 2.9999999999999996, and 0.6 /
                // 0.1 and 0.7 / 0.1 likewise. slider(#min: &0.0, #max: &0.3, #step:
                // &0.1, ..) therefore has three positions: its right end sends 0.2, 0.3
                // can never be chosen, and a program value of 0.3 draws where 0.2 does.
                // Snap a quotient within a few ulps of an integer to that integer
                // (n.round() when (n - n.round()).abs() <= 4.0 * f64::EPSILON *
                // n.round().max(1.0), else n.floor()); Scale::value's .min(max) then
                // puts the last step exactly on max. Pin it with a 0.1 step over [0,
                // 0.3] in stepped_slider_keys_wheel_and_drags_share_one_lattice, which
                // only uses step 1.0. probe:
                // design/review-2026-10-26/repro/gui-widgets-b-slider-step-floor.py
                // (replays this formula in f64). (gui-widgets-b-01)
                count: ((bounds.max - bounds.min) / step).floor(),
            },
            _ => Self::Fraction(bounds),
        }
    }

    /// Where the program's `value` sits on iced's slider.
    fn position(self, value: f64) -> f64 {
        match self {
            Self::Steps { min, max, step, count } => {
                ((value.clamp(min, max) - min) / step).round().min(count)
            }
            Self::Fraction(bounds) => bounds.fraction(value),
        }
    }

    /// The program's value at iced's `position`.
    fn value(self, position: f64) -> f64 {
        match self {
            Self::Steps { min, max, step, .. } => {
                (min + position.round() * step).min(max)
            }
            Self::Fraction(bounds) => bounds.value(position),
        }
    }
}

fn slider_change(id: Option<CallableId>, scale: Scale, position: f64) -> Message {
    match id {
        Some(id) => {
            Message::Call(id, ValArray::from_iter([Value::F64(scale.value(position))]))
        }
        None => Message::Nop,
    }
}

#[derive(Default)]
struct InputState {
    modifiers: Modifiers,
}

struct SliderInput<W> {
    content: W,
    bounds: Bounds,
    value: f64,
    on_change: Option<CallableId>,
}

impl<W: Widget<Message, GraphixTheme, Renderer>> Widget<Message, GraphixTheme, Renderer>
    for SliderInput<W>
{
    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<InputState>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(InputState::default())
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.content as &dyn Widget<Message, GraphixTheme, Renderer>)]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.children[0]
            .diff(&self.content as &dyn Widget<Message, GraphixTheme, Renderer>);
    }

    fn size(&self) -> Size<Length> {
        self.content.size()
    }

    fn layout(
        &mut self,
        tree: &mut Tree,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        self.content.layout(&mut tree.children[0], renderer, limits)
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation,
    ) {
        self.content.operate(&mut tree.children[0], layout, renderer, operation);
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        let modifiers = &mut tree.state.downcast_mut::<InputState>().modifiers;
        if let Event::Keyboard(keyboard::Event::ModifiersChanged(next)) = event {
            *modifiers = *next;
        }
        let direction = match event {
            Event::Keyboard(keyboard::Event::KeyPressed {
                key: Key::Named(Named::ArrowUp),
                ..
            }) => Some(1.0),
            Event::Keyboard(keyboard::Event::KeyPressed {
                key: Key::Named(Named::ArrowDown),
                ..
            }) => Some(-1.0),
            Event::Mouse(mouse::Event::WheelScrolled { delta })
                if modifiers.control() =>
            {
                let y = match delta {
                    mouse::ScrollDelta::Lines { y, .. }
                    | mouse::ScrollDelta::Pixels { y, .. } => *y,
                };
                Some(if y < 0.0 { -1.0 } else { 1.0 })
            }
            _ => None,
        };
        if let Some(direction) = direction
            && cursor.is_over(layout.bounds())
            && self.on_change.is_some()
        {
            let next = (self.value + direction * 0.01).clamp(0.0, 1.0);
            if self.bounds.value(next) != self.bounds.value(self.value) {
                shell.publish(slider_change(
                    self.on_change,
                    Scale::Fraction(self.bounds),
                    next,
                ));
            }
            shell.capture_event();
            return;
        }
        self.content.update(
            &mut tree.children[0],
            event,
            layout,
            cursor,
            renderer,
            clipboard,
            shell,
            viewport,
        );
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.content.mouse_interaction(
            &tree.children[0],
            layout,
            cursor,
            viewport,
            renderer,
        )
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &GraphixTheme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.content.draw(
            &tree.children[0],
            renderer,
            theme,
            style,
            layout,
            cursor,
            viewport,
        );
    }

    fn overlay<'a>(
        &'a mut self,
        tree: &'a mut Tree,
        layout: Layout<'a>,
        renderer: &Renderer,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'a, Message, GraphixTheme, Renderer>> {
        self.content.overlay(
            &mut tree.children[0],
            layout,
            renderer,
            viewport,
            translation,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slider_bounds_keep_endpoints_and_finite_interiors() {
        for (min, max) in
            [(-f64::MAX, f64::MAX), (1e308, f64::MAX), (-f64::MAX, -1e308), (5.0, 5.0)]
        {
            let bounds = Bounds::new(min, max).unwrap();
            assert_eq!(bounds.value(0.0), min);
            assert_eq!(bounds.value(1.0), max);
            for t in [0.0, 0.25, 0.5, 0.75, 1.0] {
                let value = bounds.value(t);
                assert!(value.is_finite() && (min..=max).contains(&value));
                let fraction = bounds.fraction(value);
                assert!(fraction.is_finite());
                if min != max {
                    assert!((fraction - t).abs() < 1e-14);
                }
            }
        }
        for (min, max) in
            [(f64::NEG_INFINITY, 1.0), (0.0, f64::INFINITY), (f64::NAN, 1.0), (1.0, 0.0)]
        {
            assert!(Bounds::new(min, max).is_none());
        }
    }
}
