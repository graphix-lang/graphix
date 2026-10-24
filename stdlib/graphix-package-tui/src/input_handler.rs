use super::{ChildW, TuiW, TuiWidget, finish_update};
use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use async_trait::async_trait;
use crossterm::event::{
    Event, KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers, MediaKeyCode,
    ModifierKeyCode, MouseButton, MouseEvent, MouseEventKind,
};
use graphix_compiler::{expr::ExprId, rep};
use graphix_rt::{Callable, GXExt, GXHandle, Ref};
use log::{debug, error};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::IntoValue;
use ratatui::{Frame, layout::Rect};
use smallvec::{SmallVec, smallvec};
use std::collections::VecDeque;

fn media_keycode_to_value(kc: &MediaKeyCode) -> Value {
    use MediaKeyCode::*;
    let tag = match kc {
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
    };
    rep::make_variant(tag, [])
}

fn modifier_keycode_to_value(mc: &ModifierKeyCode) -> Value {
    use ModifierKeyCode::*;
    let tag = match mc {
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
    };
    rep::make_variant(tag, [])
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
    let tag = match kc {
        Backspace => literal!("Backspace"),
        Enter => literal!("Enter"),
        Left => literal!("Left"),
        Right => literal!("Right"),
        Up => literal!("Up"),
        Down => literal!("Down"),
        Home => literal!("Home"),
        End => literal!("End"),
        PageUp => literal!("PageUp"),
        PageDown => literal!("PageDown"),
        Tab => literal!("Tab"),
        BackTab => literal!("BackTab"),
        Delete => literal!("Delete"),
        Insert => literal!("Insert"),
        F(n) => return rep::make_variant(literal!("F"), [(*n as i64).into()]),
        Char(c) => {
            if c.len_utf8() == 1 {
                let mut buf = [0u8];
                c.encode_utf8(&mut buf);
                if buf[0] >= 32 {
                    let i = (buf[0] - 32) as usize;
                    if i < ASCII.len() {
                        return rep::make_variant(
                            literal!("Char"),
                            [ASCII[i].clone().into()],
                        );
                    }
                }
            }
            let s = ArcStr::init_with(c.len_utf8(), |buf| {
                c.encode_utf8(buf);
            })
            .unwrap();
            return rep::make_variant(literal!("Char"), [s.into()]);
        }
        Null => literal!("Null"),
        Esc => literal!("Esc"),
        CapsLock => literal!("CapsLock"),
        ScrollLock => literal!("ScrollLock"),
        NumLock => literal!("NumLock"),
        PrintScreen => literal!("PrintScreen"),
        Pause => literal!("Pause"),
        Menu => literal!("Menu"),
        KeypadBegin => literal!("KeypadBegin"),
        Media(mc) => {
            return rep::make_variant(literal!("Media"), [media_keycode_to_value(mc)]);
        }
        Modifier(mc) => {
            return rep::make_variant(
                literal!("Modifier"),
                [modifier_keycode_to_value(mc)],
            );
        }
    };
    rep::make_variant(tag, [])
}

fn key_modifiers_to_value(k: &KeyModifiers) -> Value {
    let mut res: SmallVec<[Value; 6]> = smallvec![];
    if k.contains(KeyModifiers::SHIFT) {
        res.push(rep::make_variant(literal!("Shift"), []));
    }
    if k.contains(KeyModifiers::CONTROL) {
        res.push(rep::make_variant(literal!("Control"), []));
    }
    if k.contains(KeyModifiers::ALT) {
        res.push(rep::make_variant(literal!("Alt"), []));
    }
    if k.contains(KeyModifiers::SUPER) {
        res.push(rep::make_variant(literal!("Super"), []));
    }
    if k.contains(KeyModifiers::HYPER) {
        res.push(rep::make_variant(literal!("Hyper"), []));
    }
    if k.contains(KeyModifiers::META) {
        res.push(rep::make_variant(literal!("Meta"), []));
    }
    ValArray::from_iter_exact(res.into_iter()).into()
}

fn key_event_kind_to_value(k: &KeyEventKind) -> Value {
    let tag = match k {
        KeyEventKind::Press => literal!("Press"),
        KeyEventKind::Release => literal!("Release"),
        KeyEventKind::Repeat => literal!("Repeat"),
    };
    rep::make_variant(tag, [])
}

fn key_event_states_to_value(s: &KeyEventState) -> Value {
    let mut res: SmallVec<[Value; 3]> = smallvec![];
    if s.contains(KeyEventState::KEYPAD) {
        res.push(rep::make_variant(literal!("Keypad"), []));
    }
    if s.contains(KeyEventState::CAPS_LOCK) {
        res.push(rep::make_variant(literal!("CapsLock"), []));
    }
    if s.contains(KeyEventState::NUM_LOCK) {
        res.push(rep::make_variant(literal!("NumLock"), []));
    }
    ValArray::from_iter_exact(res.into_iter()).into()
}

fn key_event_to_value(e: &KeyEvent) -> Value {
    #[derive(IntoValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
    let tag = match b {
        MouseButton::Left => literal!("Left"),
        MouseButton::Right => literal!("Right"),
        MouseButton::Middle => literal!("Middle"),
    };
    rep::make_variant(tag, [])
}

fn mouse_event_kind_to_value(k: &MouseEventKind) -> Value {
    let (tag, button) = match k {
        MouseEventKind::Down(b) => (literal!("Down"), Some(b)),
        MouseEventKind::Up(b) => (literal!("Up"), Some(b)),
        MouseEventKind::Drag(b) => (literal!("Drag"), Some(b)),
        MouseEventKind::Moved => (literal!("Moved"), None),
        MouseEventKind::ScrollDown => (literal!("ScrollDown"), None),
        MouseEventKind::ScrollUp => (literal!("ScrollUp"), None),
        MouseEventKind::ScrollLeft => (literal!("ScrollLeft"), None),
        MouseEventKind::ScrollRight => (literal!("ScrollRight"), None),
    };
    rep::make_variant(tag, button.map(mouse_button_to_value))
}

fn mouse_event_to_value(e: &MouseEvent) -> Value {
    #[derive(IntoValue)]
    #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        Event::FocusGained => rep::make_variant(literal!("FocusGained"), []),
        Event::FocusLost => rep::make_variant(literal!("FocusLost"), []),
        Event::Key(e) => rep::make_variant(literal!("Key"), [key_event_to_value(e)]),
        Event::Mouse(e) => {
            rep::make_variant(literal!("Mouse"), [mouse_event_to_value(e)])
        }
        Event::Paste(s) => rep::make_variant(literal!("Paste"), [ArcStr::from(s).into()]),
        Event::Resize(x, y) => rep::make_variant(
            literal!("Resize"),
            [(*x as i64).into(), (*y as i64).into()],
        ),
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
        Ok(TuiW::new(t))
    }

    async fn send_next(&mut self) -> Result<()> {
        if self.in_flight.is_none()
            && let Some(h) = &self.handle
            && let Some(v) = self.queued.pop_front()
        {
            debug!("sending event: {v}");
            h.call_answered(ValArray::from_iter_exact([v.clone()].into_iter()))?;
            self.in_flight = Some((h.answer, v));
        }
        Ok(())
    }

    async fn set_handle(&mut self, v: Value) -> Result<()> {
        self.gx.update_callable(&mut self.handle, &v).await?;
        self.send_next().await
    }

    /// The handler's answer to the event in flight: the child sees the
    /// event when it continues, and an event the handler did not answer
    /// (its call produced nothing, or raised) goes nowhere.
    async fn answered(&mut self, ev: Value, answer: Value) -> Result<()> {
        match rep::variant(&answer) {
            Some((s, [])) if s == "Continue" => self.child.w.handle_event(ev).await?,
            Some((s, [])) if s == "Stop" => (),
            _ if answer == Value::Null => debug!("the handler did not answer {ev}"),
            _ => error!("invalid response from input handler {answer}"),
        }
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for InputHandlerW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        // CR claude for claude: [risk/medium] A disabled handler drops every event here:
        // nothing is queued and nothing goes to `self.child`, so the child and every
        // input_handler nested under it are cut off while it is disabled.
        // input_handler.gxi does not document `#enabled`. The book's Conditional
        // Handling section only says a handler can be enabled or disabled, which reads
        // as 'the handler steps aside'. Decide which it is. For bypass, forward to
        // `self.child.w.handle_event(v)` when disabled. Either way, document `#enabled`
        // in the val's doc and add an input_test case that pins it: a disabled outer
        // handler over a counting inner one, plus a flip to false while events are
        // queued, which `handle_update` clears. (tests-tui-03)
        // 2026-10-24 claude: Eric ruled (2026-10-24): steps aside: a disabled handler
        // forwards events to its child.
        // CR claude for claude: [bug/low] When `#enabled` is false this drops the event
        // rather than handing it to `self.child`. A disabled handler therefore cuts off
        // every widget and nested input_handler below it, not just itself. For example,
        // a modal key layer `input_handler(#enabled: &modal_open, ..., &app)` blocks
        // all keys to `app` while the modal is closed. The book's "Conditional
        // Handling" section calls this enabling or disabling "the handler", which reads
        // as transparent. input_handler.gxi does not document `#enabled`, and no test
        // covers it. Decide which rule is intended: transparent (forward to
        // `self.child.w.handle_event` when disabled) or blocks the subtree. Then
        // document it in input_handler.gxi and pin it with a TuiTestHarness test. The
        // book's signature is also stale (`throws 'e`, parameter `a`) and should match
        // the gxi. (tui-core-07)
        // 2026-10-13 claude: input_handler.gxi and the book's Conditional Handling now
        // document the rule as built (a disabled handler drops events for its whole
        // subtree); change both if the ruling is bypass.
        // 2026-10-24 claude: Eric ruled (2026-10-24): steps aside: a disabled handler
        // forwards events to its child.
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

    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        if let Some(Some(false)) = self.p.enabled.update(id, v).context("enabled")? {
            self.queued.clear();
        }
        if let Some((answer, _)) = &self.in_flight
            && *answer == id
        {
            let (_, ev) = self.in_flight.take().expect("an event in flight");
            return Ok(Some(Box::pin(async move {
                self.answered(ev, v.clone()).await?;
                self.send_next().await
            })));
        }
        if id == self.handle_ref.id {
            return Ok(Some(Box::pin(async move {
                self.set_handle(v.clone()).await?;
                finish_update(self.child.update(&self.gx, id, v)?).await
            })));
        }
        self.child.update(&self.gx, id, v)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        self.child.w.draw(frame, rect)
    }
}
