use super::{ChildW, TuiW, TuiWidget};
use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use async_trait::async_trait;
use crossterm::event::{
    Event, KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers, MediaKeyCode,
    ModifierKeyCode, MouseButton, MouseEvent, MouseEventKind,
};
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref};
use log::{debug, error};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::IntoValue;
use ratatui::{Frame, layout::Rect};
use smallvec::{SmallVec, smallvec};
use std::collections::VecDeque;

fn media_keycode_to_value(kc: &MediaKeyCode) -> Value {
    use MediaKeyCode::*;
    match kc {
        Play => literal!("Play"),
        Pause => literal!("Pause"),
        PlayPause => literal!("PlayPause"),
        Reverse => literal!("Reverse"),
        Stop => literal!("Stop"),
        FastForward => literal!("FastForward"),
        Rewind => literal!("Rewind"),
        TrackNext => literal!("TrackNext"),
        TrackPrevious => literal!("TrackPrevious"),
        Record => literal!("Record"),
        LowerVolume => literal!("LowerVolume"),
        RaiseVolume => literal!("RaiseVolume"),
        MuteVolume => literal!("MuteVolume"),
    }
    .into()
}

fn modifier_keycode_to_value(mc: &ModifierKeyCode) -> Value {
    use ModifierKeyCode::*;
    match mc {
        LeftShift => literal!("LeftShift"),
        LeftControl => literal!("LeftControl"),
        LeftAlt => literal!("LeftAlt"),
        LeftSuper => literal!("LeftSuper"),
        LeftHyper => literal!("LeftHyper"),
        LeftMeta => literal!("LeftMeta"),
        RightShift => literal!("RightShift"),
        RightControl => literal!("RightControl"),
        RightAlt => literal!("RightAlt"),
        RightSuper => literal!("RightSuper"),
        RightHyper => literal!("RightHyper"),
        RightMeta => literal!("RightMeta"),
        IsoLevel3Shift => literal!("IsoLevel3Shift"),
        IsoLevel5Shift => literal!("IsoLevel5Shift"),
    }
    .into()
}

fn keycode_to_value(kc: &KeyCode) -> Value {
    const ASCII: [ArcStr; 95] = [
        literal!(" "),
        literal!("!"),
        literal!("\""),
        literal!("#"),
        literal!("$"),
        literal!("%"),
        literal!("&"),
        literal!("'"),
        literal!("("),
        literal!(")"),
        literal!("*"),
        literal!("+"),
        literal!(","),
        literal!("-"),
        literal!("."),
        literal!("/"),
        literal!("0"),
        literal!("1"),
        literal!("2"),
        literal!("3"),
        literal!("4"),
        literal!("5"),
        literal!("6"),
        literal!("7"),
        literal!("8"),
        literal!("9"),
        literal!(":"),
        literal!(";"),
        literal!("<"),
        literal!("="),
        literal!(">"),
        literal!("?"),
        literal!("@"),
        literal!("A"),
        literal!("B"),
        literal!("C"),
        literal!("D"),
        literal!("E"),
        literal!("F"),
        literal!("G"),
        literal!("H"),
        literal!("I"),
        literal!("J"),
        literal!("K"),
        literal!("L"),
        literal!("M"),
        literal!("N"),
        literal!("O"),
        literal!("P"),
        literal!("Q"),
        literal!("R"),
        literal!("S"),
        literal!("T"),
        literal!("U"),
        literal!("V"),
        literal!("W"),
        literal!("X"),
        literal!("Y"),
        literal!("Z"),
        literal!("["),
        literal!("\\"),
        literal!("]"),
        literal!("^"),
        literal!("_"),
        literal!("`"),
        literal!("a"),
        literal!("b"),
        literal!("c"),
        literal!("d"),
        literal!("e"),
        literal!("f"),
        literal!("g"),
        literal!("h"),
        literal!("i"),
        literal!("j"),
        literal!("k"),
        literal!("l"),
        literal!("m"),
        literal!("n"),
        literal!("o"),
        literal!("p"),
        literal!("q"),
        literal!("r"),
        literal!("s"),
        literal!("t"),
        literal!("u"),
        literal!("v"),
        literal!("w"),
        literal!("x"),
        literal!("y"),
        literal!("z"),
        literal!("{"),
        literal!("|"),
        literal!("}"),
        literal!("~"),
    ];
    use KeyCode::*;
    match kc {
        Backspace => literal!("Backspace").into(),
        Enter => literal!("Enter").into(),
        Left => literal!("Left").into(),
        Right => literal!("Right").into(),
        Up => literal!("Up").into(),
        Down => literal!("Down").into(),
        Home => literal!("Home").into(),
        End => literal!("End").into(),
        PageUp => literal!("PageUp").into(),
        PageDown => literal!("PageDown").into(),
        Tab => literal!("Tab").into(),
        BackTab => literal!("BackTab").into(),
        Delete => literal!("Delete").into(),
        Insert => literal!("Insert").into(),
        F(n) => ValArray::from_iter_exact(
            [literal!("F").into(), (*n as i64).into()].into_iter(),
        )
        .into(),
        Char(c) => {
            if c.len_utf8() == 1 {
                let mut buf = [0u8];
                c.encode_utf8(&mut buf);
                if buf[0] >= 32 {
                    let i = (buf[0] - 32) as usize;
                    if i < ASCII.len() {
                        return ValArray::from_iter_exact(
                            [literal!("Char").into(), ASCII[i].clone().into()]
                                .into_iter(),
                        )
                        .into();
                    }
                }
            }
            let s = ArcStr::init_with(c.len_utf8(), |buf| {
                c.encode_utf8(buf);
            })
            .unwrap();
            ValArray::from_iter_exact([literal!("Char").into(), s.into()].into_iter())
                .into()
        }
        Null => literal!("Null").into(),
        Esc => literal!("Esc").into(),
        CapsLock => literal!("CapsLock").into(),
        ScrollLock => literal!("ScrollLock").into(),
        NumLock => literal!("NumLock").into(),
        PrintScreen => literal!("PrintScreen").into(),
        Pause => literal!("Pause").into(),
        Menu => literal!("Menu").into(),
        KeypadBegin => literal!("KeypadBegin").into(),
        Media(mc) => ValArray::from_iter_exact(
            [literal!("Media").into(), media_keycode_to_value(mc).into()].into_iter(),
        )
        .into(),
        Modifier(mc) => ValArray::from_iter_exact(
            [literal!("Modifier").into(), modifier_keycode_to_value(mc).into()]
                .into_iter(),
        )
        .into(),
    }
}

fn key_modifiers_to_value(k: &KeyModifiers) -> Value {
    let mut res: SmallVec<[Value; 6]> = smallvec![];
    if k.contains(KeyModifiers::SHIFT) {
        res.push(literal!("Shift").into());
    }
    if k.contains(KeyModifiers::CONTROL) {
        res.push(literal!("Control").into());
    }
    if k.contains(KeyModifiers::ALT) {
        res.push(literal!("Alt").into());
    }
    if k.contains(KeyModifiers::SUPER) {
        res.push(literal!("Super").into());
    }
    if k.contains(KeyModifiers::HYPER) {
        res.push(literal!("Hyper").into());
    }
    if k.contains(KeyModifiers::META) {
        res.push(literal!("Meta").into());
    }
    ValArray::from_iter_exact(res.into_iter()).into()
}

fn key_event_kind_to_value(k: &KeyEventKind) -> Value {
    match k {
        KeyEventKind::Press => literal!("Press").into(),
        KeyEventKind::Release => literal!("Release").into(),
        KeyEventKind::Repeat => literal!("Repeat").into(),
    }
}

fn key_event_states_to_value(s: &KeyEventState) -> Value {
    let mut res: SmallVec<[Value; 3]> = smallvec![];
    if s.contains(KeyEventState::KEYPAD) {
        res.push(literal!("Keypad").into());
    }
    if s.contains(KeyEventState::CAPS_LOCK) {
        res.push(literal!("CapsLock").into());
    }
    if s.contains(KeyEventState::NUM_LOCK) {
        res.push(literal!("NumLock").into());
    }
    ValArray::from_iter_exact(res.into_iter()).into()
}

fn key_event_to_value(e: &KeyEvent) -> Value {
    #[derive(IntoValue)]
    struct Fields {
        code: Value,
        kind: Value,
        modifiers: Value,
        state: Value,
    }
    Fields {
        code: keycode_to_value(&e.code),
        kind: key_event_kind_to_value(&e.kind),
        modifiers: key_modifiers_to_value(&e.modifiers),
        state: key_event_states_to_value(&e.state),
    }
    .into()
}

fn mouse_button_to_value(b: &MouseButton) -> Value {
    match b {
        MouseButton::Left => literal!("Left").into(),
        MouseButton::Right => literal!("Right").into(),
        MouseButton::Middle => literal!("Middle").into(),
    }
}

fn mouse_event_kind_to_value(k: &MouseEventKind) -> Value {
    match k {
        MouseEventKind::Down(b) => ValArray::from_iter_exact(
            [literal!("Down").into(), mouse_button_to_value(b).into()].into_iter(),
        )
        .into(),
        MouseEventKind::Up(b) => ValArray::from_iter_exact(
            [literal!("Up").into(), mouse_button_to_value(b).into()].into_iter(),
        )
        .into(),
        MouseEventKind::Drag(b) => ValArray::from_iter_exact(
            [literal!("Drag").into(), mouse_button_to_value(b).into()].into_iter(),
        )
        .into(),
        MouseEventKind::Moved => literal!("Moved").into(),
        MouseEventKind::ScrollDown => literal!("ScrollDown").into(),
        MouseEventKind::ScrollUp => literal!("ScrollUp").into(),
        MouseEventKind::ScrollLeft => literal!("ScrollLeft").into(),
        MouseEventKind::ScrollRight => literal!("ScrollRight").into(),
    }
}

fn mouse_event_to_value(e: &MouseEvent) -> Value {
    #[derive(IntoValue)]
    struct Fields {
        column: i64,
        kind: Value,
        modifiers: Value,
        row: i64,
    }
    Fields {
        column: e.column as i64,
        kind: mouse_event_kind_to_value(&e.kind),
        modifiers: key_modifiers_to_value(&e.modifiers),
        row: e.row as i64,
    }
    .into()
}

pub(super) fn event_to_value(e: &Event) -> Value {
    match e {
        Event::FocusGained => literal!("FocusGained").into(),
        Event::FocusLost => literal!("FocusLost").into(),
        Event::Key(e) => ValArray::from_iter_exact(
            [literal!("Key").into(), key_event_to_value(e)].into_iter(),
        )
        .into(),
        Event::Mouse(e) => ValArray::from_iter_exact(
            [literal!("Mouse").into(), mouse_event_to_value(e)].into_iter(),
        )
        .into(),
        Event::Paste(s) => ValArray::from_iter_exact(
            [literal!("Paste").into(), ArcStr::from(s).into()].into_iter(),
        )
        .into(),
        Event::Resize(x, y) => ValArray::from_iter_exact(
            [literal!("Resize").into(), (*x as i64).into(), (*y as i64).into()]
                .into_iter(),
        )
        .into(),
    }
}

/// The events a handler may have waiting: past this a new event is
/// dropped, so a handler that falls behind loses input, not memory.
const MAX_QUEUED: usize = 256;

graphix_rt::props! {
    struct Props {
        enabled: Option<bool>,
    }
}

/// Events go to the handler one at a time, each with the answer id its
/// call replies under; the answer decides whether the child sees it.
pub(super) struct InputHandlerW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    handle_ref: Ref<X>,
    handle: Option<Callable<X>>,
    child: ChildW<X>,
    queued: VecDeque<Value>,
    in_flight: Option<(ExprId, Value)>,
}

impl<X: GXExt> InputHandlerW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("input_handler")?;
        let child = ChildW::compile(&gx, &v, "child").await.context("input_handler")?;
        let handle_ref = gx.compile_field(&v, "handle").await?;
        let mut t = Self {
            gx,
            p,
            handle_ref,
            handle: None,
            child,
            queued: VecDeque::new(),
            in_flight: None,
        };
        if let Some(v) = t.handle_ref.last.take() {
            t.set_handle(v).await?
        }
        Ok(Box::new(t))
    }

    async fn send_next(&mut self) -> Result<()> {
        if self.in_flight.is_none()
            && let Some(h) = &self.handle
            && let Some(v) = self.queued.pop_front()
        {
            debug!("sending event: {v}");
            h.call_answered(ValArray::from_iter_exact([v.clone()].into_iter())).await?;
            self.in_flight = Some((h.answer, v));
        }
        Ok(())
    }

    async fn set_handle(&mut self, v: Value) -> Result<()> {
        self.gx.update_callable(&mut self.handle, v).await?;
        self.send_next().await
    }

    /// The handler's answer to the event in flight: the child sees the
    /// event when it continues, and an event the handler did not answer
    /// (its call produced nothing, or raised) goes nowhere.
    async fn answered(&mut self, ev: Value, answer: Value) -> Result<()> {
        match answer {
            Value::String(s) if &*s == "Continue" => {
                self.child.w.handle_event(ev).await?
            }
            Value::String(s) if &*s == "Stop" => (),
            Value::Null => debug!("the handler did not answer {ev}"),
            v => error!("invalid response from input handler {v}"),
        }
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for InputHandlerW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        if self.p.enabled.t.flatten().unwrap_or(true) {
            if self.queued.len() < MAX_QUEUED {
                self.queued.push_back(v);
            } else {
                debug!("the handler is behind; dropped {v}")
            }
            self.send_next().await?
        }
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        if let Some(Some(false)) = self.p.enabled.update(id, &v).context("enabled")? {
            self.queued.clear();
        }
        if let Some((answer, _)) = &self.in_flight
            && *answer == id
        {
            let (_, ev) = self.in_flight.take().expect("an event in flight");
            self.answered(ev, v).await?;
            return self.send_next().await;
        }
        if id == self.handle_ref.id {
            self.set_handle(v.clone()).await?;
        }
        self.child.update(&self.gx, id, v).await
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        self.child.w.draw(frame, rect)
    }
}
