#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use barchart::BarChartW;
use block::BlockW;
use calendar::CalendarW;
use chart::ChartW;
use crossterm::{
    ExecutableCommand,
    event::{
        DisableBracketedPaste, DisableFocusChange, DisableMouseCapture,
        EnableBracketedPaste, EnableFocusChange, EnableMouseCapture, Event, EventStream,
        KeyCode, KeyModifiers,
    },
    terminal::{self, EnterAlternateScreen},
};
use futures::{SinkExt, StreamExt, channel::mpsc, stream::Fuse};
use gauge::GaugeW;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect,
    env::Env,
    errf,
    expr::{ExprId, ModPath},
    image::{self, ImageBuf},
    typ::FnType,
};
use graphix_package::{CustomDisplay, Stop};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, EvalCachedAsync, ImageState, seam_tick,
};
use graphix_rt::{CompExp, GXExt, GXHandle, TRef};
use input_handler::{InputHandlerW, event_to_value};
use layout::LayoutW;
use line_gauge::LineGaugeW;
use list::ListW;
use log::error;
use netidx::publisher::{FromValue, Value};
use netidx_core::pack::PackError;
use netidx_derive::{FromValue, IntoValue};
use paragraph::ParagraphW;
use parking_lot::Mutex;
use ratatui::{
    DefaultTerminal, Frame,
    layout::{Alignment, Direction, Flex, Rect},
    style::{Color, Modifier, Style},
    symbols,
    text::{Line, Span, Text},
    widgets::TitlePosition,
};
use scrollbar::ScrollbarW;
use smallvec::SmallVec;
use sparkline::SparklineW;
use std::{
    borrow::Cow, fmt, future::Future, marker::PhantomData, pin::Pin, time::Duration,
};
use text::TextW;
use tokio::{
    select,
    sync::oneshot,
    task,
    time::{MissedTickBehavior, interval},
};
use triomphe::Arc;
use validate::{Byte, Offset};

mod barchart;
mod block;
mod calendar;
mod canvas;
mod chart;
mod gauge;
mod input_handler;
mod layout;
mod line_gauge;
mod list;
mod overlay;
mod paragraph;
mod scrollbar;
mod sparkline;
mod table;
mod tabs;
mod text;
mod validate;

#[cfg(any(test, feature = "testing"))]
pub mod testing;

#[cfg(test)]
mod test;

#[derive(Clone, Copy)]
struct AlignmentV(Alignment);

impl FromValue for AlignmentV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => match &*s {
                "Left" => Ok(AlignmentV(Alignment::Left)),
                "Right" => Ok(AlignmentV(Alignment::Right)),
                "Center" => Ok(AlignmentV(Alignment::Center)),
                s => bail!("invalid alignment {s}"),
            },
            v => bail!("invalid alignment {v}"),
        }
    }
}

#[derive(Clone, Copy)]
struct ColorV(Color);

impl FromValue for ColorV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => match &*s {
                "Reset" => Ok(Self(Color::Reset)),
                "Black" => Ok(Self(Color::Black)),
                "Red" => Ok(Self(Color::Red)),
                "Green" => Ok(Self(Color::Green)),
                "Yellow" => Ok(Self(Color::Yellow)),
                "Blue" => Ok(Self(Color::Blue)),
                "Magenta" => Ok(Self(Color::Magenta)),
                "Cyan" => Ok(Self(Color::Cyan)),
                "Gray" => Ok(Self(Color::Gray)),
                "DarkGray" => Ok(Self(Color::DarkGray)),
                "LightRed" => Ok(Self(Color::LightRed)),
                "LightGreen" => Ok(Self(Color::LightGreen)),
                "LightYellow" => Ok(Self(Color::LightYellow)),
                "LightBlue" => Ok(Self(Color::LightBlue)),
                "LightMagenta" => Ok(Self(Color::LightMagenta)),
                "LightCyan" => Ok(Self(Color::LightCyan)),
                "White" => Ok(Self(Color::White)),
                s => bail!("invalid color name {s}"),
            },
            v => match v.cast_to::<(ArcStr, Value)>()? {
                (s, v) if &*s == "Rgb" => {
                    #[derive(FromValue)]
                    struct Rgb {
                        b: Byte,
                        g: Byte,
                        r: Byte,
                    }
                    let Rgb { b, g, r } = v.cast_to()?;
                    Ok(Self(Color::Rgb(r.0, g.0, b.0)))
                }
                (s, v) if &*s == "Indexed" => {
                    Ok(Self(Color::Indexed(v.cast_to::<Byte>()?.0)))
                }
                (s, v) => bail!("invalid color ({s} {v})"),
            },
        }
    }
}

#[derive(Clone, Copy)]
struct ModifierV(Modifier);

impl FromValue for ModifierV {
    fn from_value(v: Value) -> Result<Self> {
        let mut m = Modifier::empty();
        if let Some(o) = v.cast_to::<Option<SmallVec<[ArcStr; 2]>>>()? {
            for s in o {
                match &*s {
                    "Bold" => m |= Modifier::BOLD,
                    "Dim" => m |= Modifier::DIM,
                    "Italic" => m |= Modifier::ITALIC,
                    "Underlined" => m |= Modifier::UNDERLINED,
                    "SlowBlink" => m |= Modifier::SLOW_BLINK,
                    "RapidBlink" => m |= Modifier::RAPID_BLINK,
                    "Reversed" => m |= Modifier::REVERSED,
                    "Hidden" => m |= Modifier::HIDDEN,
                    "CrossedOut" => m |= Modifier::CROSSED_OUT,
                    s => bail!("invalid modifier {s}"),
                }
            }
        }
        Ok(Self(m))
    }
}

#[derive(Debug, Clone, Copy)]
struct StyleV(Style);

impl FromValue for StyleV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            add_modifier: ModifierV,
            bg: Option<ColorV>,
            fg: Option<ColorV>,
            sub_modifier: ModifierV,
            underline_color: Option<ColorV>,
        }
        let Fields { add_modifier, bg, fg, sub_modifier, underline_color } =
            v.cast_to()?;
        Ok(Self(Style {
            fg: fg.map(|c| c.0),
            bg: bg.map(|c| c.0),
            underline_color: underline_color.map(|c| c.0),
            add_modifier: add_modifier.0,
            sub_modifier: sub_modifier.0,
        }))
    }
}

struct SpanV(Span<'static>);

impl FromValue for SpanV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            content: String,
            style: StyleV,
        }
        let Fields { content, style } = v.cast_to()?;
        Ok(Self(Span { content: Cow::Owned(content), style: style.0 }))
    }
}

#[derive(Debug, Clone)]
struct LineV(Line<'static>);

impl FromValue for LineV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            alignment: Option<AlignmentV>,
            spans: Value,
            style: StyleV,
        }
        let Fields { alignment, spans, style } = v.cast_to()?;
        let spans = match spans {
            Value::String(s) => vec![Span::raw(String::from(&*s))],
            v => v
                .clone()
                .cast_to::<Vec<SpanV>>()?
                .into_iter()
                .map(|s| s.0)
                .collect::<Vec<_>>(),
        };
        Ok(Self(Line { style: style.0, alignment: alignment.map(|a| a.0), spans }))
    }
}

struct LinesV(Vec<Line<'static>>);

impl FromValue for LinesV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => Ok(Self(Text::raw(String::from(s.as_str())).lines)),
            v => Ok(Self(v.cast_to::<Vec<LineV>>()?.into_iter().map(|l| l.0).collect())),
        }
    }
}

#[derive(Clone, Copy)]
struct FlexV(Flex);

impl FromValue for FlexV {
    fn from_value(v: Value) -> Result<Self> {
        let t = match &*v.cast_to::<ArcStr>()? {
            "Legacy" => Flex::Legacy,
            "Start" => Flex::Start,
            "End" => Flex::End,
            "Center" => Flex::Center,
            "SpaceBetween" => Flex::SpaceBetween,
            "SpaceEvenly" => Flex::SpaceEvenly,
            "SpaceAround" => Flex::SpaceAround,
            s => bail!("invalid flex {s}"),
        };
        Ok(Self(t))
    }
}

/// A scroll in content lines (y) and chars (x).
#[derive(Debug, Clone, Copy, FromValue)]
struct ScrollV {
    x: Offset,
    y: Offset,
}

/// An axis range; a bound that is not a number is read as the empty
/// range, which draws nothing (ratatui paints a NaN at the edge).
#[derive(Clone, Copy)]
struct BoundsV {
    max: f64,
    min: f64,
}

impl FromValue for BoundsV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            max: f64,
            min: f64,
        }
        let Fields { max, min } = v.cast_to()?;
        if max.is_finite() && min.is_finite() {
            Ok(Self { max, min })
        } else {
            log::warn!("bounds [{min}, {max}] are not numbers; drawing nothing");
            Ok(Self { max: 0., min: 0. })
        }
    }
}

#[derive(Clone, Copy)]
struct TitlePositionV(TitlePosition);

impl FromValue for TitlePositionV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Top" => Ok(Self(TitlePosition::Top)),
            "Bottom" => Ok(Self(TitlePosition::Bottom)),
            s => bail!("invalid position {s}"),
        }
    }
}

#[derive(Clone, Copy)]
struct DirectionV(Direction);

impl FromValue for DirectionV {
    fn from_value(v: Value) -> Result<Self> {
        let t = match &*v.cast_to::<ArcStr>()? {
            "Horizontal" => Direction::Horizontal,
            "Vertical" => Direction::Vertical,
            s => bail!("invalid direction tag {s}"),
        };
        Ok(Self(t))
    }
}

#[derive(Clone)]
struct HighlightSpacingV(ratatui::widgets::HighlightSpacing);

impl FromValue for HighlightSpacingV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Always" => Ok(Self(ratatui::widgets::HighlightSpacing::Always)),
            "Never" => Ok(Self(ratatui::widgets::HighlightSpacing::Never)),
            "WhenSelected" => Ok(Self(ratatui::widgets::HighlightSpacing::WhenSelected)),
            s => bail!("invalid highlight spacing {s}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default, IntoValue)]
struct SizeV {
    width: i64,
    height: i64,
}

impl From<Rect> for SizeV {
    fn from(r: Rect) -> Self {
        let s = r.as_size();
        Self::new(s.width, s.height)
    }
}

impl SizeV {
    fn new(width: u16, height: u16) -> Self {
        Self { width: width.into(), height: height.into() }
    }

    fn from_terminal() -> Result<Self> {
        let (width, height) = terminal::size()?;
        Ok(Self::new(width, height))
    }
}

#[derive(Clone, Copy)]
struct MarkerV(symbols::Marker);

impl FromValue for MarkerV {
    fn from_value(v: Value) -> Result<Self> {
        let m = match &*v.cast_to::<ArcStr>()? {
            "Dot" => symbols::Marker::Dot,
            "Block" => symbols::Marker::Block,
            "Bar" => symbols::Marker::Bar,
            "Braille" => symbols::Marker::Braille,
            "HalfBlock" => symbols::Marker::HalfBlock,
            "Quadrant" => symbols::Marker::Quadrant,
            "Sextant" => symbols::Marker::Sextant,
            "Octant" => symbols::Marker::Octant,
            s => bail!("invalid marker {s}"),
        };
        Ok(Self(m))
    }
}

fn into_borrowed_line<'a>(line: &'a Line<'static>) -> Line<'a> {
    let spans = line
        .spans
        .iter()
        .map(|s| {
            let content = match &s.content {
                Cow::Owned(s) => Cow::Borrowed(s.as_str()),
                Cow::Borrowed(s) => Cow::Borrowed(*s),
            };
            Span { content, style: s.style }
        })
        .collect();
    Line { alignment: line.alignment, style: line.style, spans }
}

fn into_borrowed_lines<'a>(lines: &'a [Line<'static>]) -> Vec<Line<'a>> {
    lines.iter().map(|l| into_borrowed_line(l)).collect::<Vec<_>>()
}

#[async_trait]
trait TuiWidget {
    /// A terminal event, as the `tui::event` value; only routing widgets
    /// and input handlers take it.
    async fn handle_event(&mut self, _v: Value) -> Result<()> {
        Ok(())
    }
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()>;
    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()>;
}

type TuiW = Box<dyn TuiWidget + Send + Sync + 'static>;
type CompRes = Pin<Box<dyn Future<Output = Result<TuiW>> + Send + Sync + 'static>>;

fn compile<X: GXExt>(gx: GXHandle<X>, source: Value) -> CompRes {
    Box::pin(async move {
        match source.cast_to::<(ArcStr, Value)>()? {
            (s, v) if &s == "Text" => TextW::compile(gx, v).await,
            (s, v) if &s == "Paragraph" => ParagraphW::compile(gx, v).await,
            (s, v) if &s == "Block" => BlockW::compile(gx, v).await,
            (s, v) if &s == "Scrollbar" => ScrollbarW::compile(gx, v).await,
            (s, v) if &s == "Layout" => LayoutW::compile(gx, v).await,
            (s, v) if &s == "BarChart" => BarChartW::compile(gx, v).await,
            (s, v) if &s == "Chart" => ChartW::compile(gx, v).await,
            (s, v) if &s == "Sparkline" => SparklineW::compile(gx, v).await,
            (s, v) if &s == "LineGauge" => LineGaugeW::compile(gx, v).await,
            (s, v) if &s == "Calendar" => CalendarW::compile(gx, v).await,
            (s, v) if &s == "Table" => table::TableW::compile(gx, v).await,
            (s, v) if &s == "Gauge" => GaugeW::compile(gx, v).await,
            (s, v) if &s == "List" => ListW::compile(gx, v).await,
            (s, v) if &s == "Tabs" => tabs::TabsW::compile(gx, v).await,
            (s, v) if &s == "Canvas" => canvas::CanvasW::compile(gx, v).await,
            (s, v) if &s == "Overlay" => overlay::OverlayW::compile(gx, v).await,
            (s, v) if &s == "InputHandler" => InputHandlerW::compile(gx, v).await,
            (s, v) => bail!("invalid widget type `{s}({v})"),
        }
    })
}

struct EmptyW;

#[async_trait]
impl TuiWidget for EmptyW {
    async fn handle_update(&mut self, _id: ExprId, _v: Value) -> Result<()> {
        Ok(())
    }

    fn draw(&mut self, _frame: &mut Frame, _rect: Rect) -> Result<()> {
        Ok(())
    }
}

/// A widget the struct field `name` refers to, rebuilt when the
/// reference fires.
struct ChildW<X: GXExt> {
    r: graphix_rt::Ref<X>,
    w: TuiW,
}

impl<X: GXExt> ChildW<X> {
    async fn compile(gx: &GXHandle<X>, v: &Value, name: &str) -> Result<Self> {
        let mut r = gx.compile_field(v, name).await?;
        let w = match r.last.take() {
            Some(v) => compile(gx.clone(), v).await.context("child")?,
            None => Box::new(EmptyW),
        };
        Ok(Self { r, w })
    }

    async fn update(&mut self, gx: &GXHandle<X>, id: ExprId, v: Value) -> Result<()> {
        if id == self.r.id {
            self.w = compile(gx.clone(), v).await.context("child")?;
            Ok(())
        } else {
            self.w.handle_update(id, v).await
        }
    }
}

/// `f` over each element of the array `v`, together.
async fn compile_each<T, F: Future<Output = Result<T>>>(
    v: Value,
    f: impl FnMut(Value) -> F,
) -> Result<Vec<T>> {
    futures::future::try_join_all(v.cast_to::<SmallVec<[Value; 8]>>()?.into_iter().map(f))
        .await
}

/// Where a widget tells the program the size its content draws in: the
/// struct field `size`, written when the size changes.
struct SizeReport<X: GXExt> {
    r: graphix_rt::Ref<X>,
    last: SizeV,
}

impl<X: GXExt> SizeReport<X> {
    async fn compile(gx: &GXHandle<X>, v: &Value) -> Result<Self> {
        let r = gx.compile_field(v, "size").await?;
        Ok(Self { r, last: SizeV::default() })
    }

    fn report(&mut self, rect: Rect) -> Result<()> {
        let size = SizeV::from(rect);
        if self.last != size {
            self.last = size;
            self.r.set_deref(size)?
        }
        Ok(())
    }
}

enum ToTui {
    Update(ExprId, Value),
    /// A cycle's updates are all in.
    Draw,
    Stop(oneshot::Sender<()>),
}

/// A request to suspend the display: it leaves the alternate screen and
/// raw mode and releases stdin, then answers with the resume signal the
/// requesting site holds while the terminal is its child's.
struct Suspend {
    ack: oneshot::Sender<Result<oneshot::Sender<()>>>,
}

/// What a program can ask of the running display, shared through
/// libstate: the stop signal Ctrl-C fires (`tui::exit`), and the
/// suspend channel (`tui::suspend`). The receiver is parked here until
/// a display takes it, and handed back when that display ends.
struct TuiControlInner {
    stop: Mutex<Option<Stop>>,
    suspend_tx: mpsc::UnboundedSender<Suspend>,
    suspend_rx: Mutex<Option<mpsc::UnboundedReceiver<Suspend>>>,
    /// No display will ever run (a test harness): a suspend is refused
    /// instead of waiting for one.
    headless: std::sync::atomic::AtomicBool,
}

#[derive(Clone)]
struct TuiControl(Arc<TuiControlInner>);

impl Default for TuiControl {
    fn default() -> Self {
        let (suspend_tx, suspend_rx) = mpsc::unbounded();
        TuiControl(Arc::new(TuiControlInner {
            stop: Mutex::new(None),
            suspend_tx,
            suspend_rx: Mutex::new(Some(suspend_rx)),
            headless: std::sync::atomic::AtomicBool::new(false),
        }))
    }
}

impl fmt::Debug for TuiControl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TuiControl")
    }
}

fn fire<T>(slot: &Mutex<Option<oneshot::Sender<T>>>, v: T) {
    if let Some(tx) = slot.lock().take() {
        let _ = tx.send(v);
    }
}

/// `tui::suspend`: the display is suspended while the input is true.
/// The site holds the resume signal between the rising edge and the
/// falling one — or its own drop, so a program that goes away mid-child
/// still hands the display back.
#[derive(Debug, Default)]
struct SuspendEv {
    control: Option<TuiControl>,
    held: Arc<Mutex<Option<oneshot::Sender<()>>>>,
}

impl Drop for SuspendEv {
    fn drop(&mut self) {
        fire(&self.held, ())
    }
}

impl EvalCachedAsync for SuspendEv {
    type Args = (TuiControl, Arc<Mutex<Option<oneshot::Sender<()>>>>, bool);

    const NAME: &str = "tui_suspend";

    fn attach<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if self.control.is_none() {
            self.control = Some(ctx.libstate.get_or_default::<TuiControl>().clone());
        }
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let suspended = cached.get::<bool>(0)?;
        Some((self.control.clone()?, self.held.clone(), suspended))
    }

    fn eval(
        (control, held, suspended): Self::Args,
    ) -> impl Future<Output = Value> + Send {
        async move {
            if !suspended {
                fire(&held, ());
                return Value::Bool(false);
            }
            if held.lock().is_some() {
                return Value::Bool(true);
            }
            if control.0.headless.load(std::sync::atomic::Ordering::Relaxed) {
                return errf!("TerminalError", "no terminal display is running");
            }
            let (ack, done) = oneshot::channel();
            if control.0.suspend_tx.unbounded_send(Suspend { ack }).is_err() {
                return errf!("TerminalError", "the terminal display has ended");
            }
            match done.await {
                Ok(Ok(resume)) => {
                    *held.lock() = Some(resume);
                    Value::Bool(true)
                }
                Ok(Err(e)) => errf!("TerminalError", "{e:#}"),
                Err(_) => {
                    errf!("TerminalError", "the terminal display ended before suspending")
                }
            }
        }
    }
}

impl ImageState for SuspendEv {
    /// A held resume signal is a suspended display, which only a cycle
    /// can produce.
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.held.lock().is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        Ok(())
    }

    fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        _buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self {
            control: Some(ctx.libstate.get_or_default::<TuiControl>().clone()),
            held: Arc::new(Mutex::new(None)),
        })
    }
}

type SuspendB = CachedArgsAsync<SuspendEv>;

#[derive(Debug)]
struct Exit;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Exit {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "tui_exit";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Self))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Exit {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if let Some(n) = from.get_mut(0)
            && seam_tick(n.update(ctx)).is_some()
            && let Some(stop) = ctx.libstate.get::<TuiControl>()
        {
            fire(&stop.0.stop, Ok(()));
        }
        TagValue::phantom_ref()
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

struct Tui<X: GXExt> {
    to: mpsc::Sender<ToTui>,
    ph: PhantomData<X>,
}

impl<X: GXExt> Tui<X> {
    fn start(gx: &GXHandle<X>, env: Env, root: CompExp<X>, stop: Stop) -> Self {
        let gx = gx.clone();
        let (to_tx, to_rx) = mpsc::channel(3);
        task::spawn(async move {
            let control = gx
                .with_ctx(move |ctx| {
                    let control = ctx.libstate.get_or_default::<TuiControl>().clone();
                    *control.0.stop.lock() = Some(stop);
                    control
                })
                .await;
            let control = match control {
                Ok(control) => control,
                Err(e) => return error!("tui: no runtime to display for {e:?}"),
            };
            let display = task::spawn({
                let control = control.clone();
                async move { run(gx, env, root, to_rx, &control).await }
            });
            let r = match display.await {
                Ok(r) => r,
                Err(e) => Err(anyhow!("the display failed: {e}")),
            };
            if let Err(e) = r {
                fire(&control.0.stop, Err(e))
            }
        });
        Self { to: to_tx, ph: PhantomData }
    }

    async fn clear(&mut self) {
        let (tx, rx) = oneshot::channel();
        let _ = self.to.send(ToTui::Stop(tx)).await;
        let _ = rx.await;
    }

    async fn send(&mut self, m: ToTui) {
        if let Err(_) = self.to.send(m).await {
            error!("could not send to the display because its task died")
        }
    }
}

fn is_ctrl_c(e: &Event) -> bool {
    e.as_key_press_event()
        .map(|e| match e.code {
            KeyCode::Char('c') if e.modifiers == KeyModifiers::CONTROL.into() => true,
            _ => false,
        })
        .unwrap_or(false)
}

fn get_id(env: &Env, name: &ModPath) -> Result<BindId> {
    Ok(env
        .lookup_bind(&ModPath::root(), name)?
        .ok_or_else(|| anyhow!("could not find {name}"))?
        .1
        .id)
}

fn set_mouse(enable: bool) {
    use std::io::stdout;
    let mut stdout = stdout();
    if enable {
        if let Err(e) = stdout.execute(EnableMouseCapture) {
            error!("could not enable mouse capture {e:?}")
        }
        if let Err(e) = stdout.execute(EnableFocusChange) {
            error!("could not enable focus change {e:?}")
        }
    } else {
        if let Err(e) = stdout.execute(DisableMouseCapture) {
            error!("could not disable mouse capture {e:?}")
        }
        if let Err(e) = stdout.execute(DisableFocusChange) {
            error!("could not disable mouse capture {e:?}")
        }
    }
}

/// The widget tree a display shows and the way the program's input
/// reaches it; the display and the test harness drive it alike.
pub(crate) struct Screen<X: GXExt> {
    gx: GXHandle<X>,
    root_id: ExprId,
    root: TuiW,
    size: BindId,
    event: BindId,
}

impl<X: GXExt> Screen<X> {
    pub(crate) fn new(gx: GXHandle<X>, env: &Env, root_id: ExprId) -> Result<Self> {
        Ok(Self {
            gx,
            root_id,
            root: Box::new(EmptyW),
            size: get_id(env, &["tui", "size"].into())?,
            event: get_id(env, &["tui", "event"].into())?,
        })
    }

    /// A value of the program: the root rebuilds the tree, anything else
    /// goes to the widgets.
    pub(crate) async fn update(&mut self, id: ExprId, v: Value) -> Result<()> {
        if id == self.root_id {
            self.root = compile(self.gx.clone(), v)
                .await
                .context("invalid widget specification")?;
            Ok(())
        } else {
            self.root.handle_update(id, v).await
        }
    }

    pub(crate) fn resize(&self, size: SizeV) -> Result<()> {
        self.gx.set(self.size, size)
    }

    /// A terminal event, through `tui::event` and the widgets; false for
    /// Ctrl-C, which stops the display instead.
    pub(crate) async fn event(&mut self, e: &Event) -> Result<bool> {
        if is_ctrl_c(e) {
            return Ok(false);
        }
        if let Event::Resize(width, height) = e {
            self.resize(SizeV::new(*width, *height))?
        }
        let v = event_to_value(e);
        self.gx.set(self.event, v.clone())?;
        self.root.handle_event(v).await?;
        Ok(true)
    }

    pub(crate) fn draw(&mut self, frame: &mut Frame) -> Result<()> {
        self.root.draw(frame, frame.area())
    }
}

/// While the display holds the terminal, stderr goes to a file, written
/// out once the terminal is given back: a diagnostic written into the
/// alternate screen garbles it and is lost with it.
#[cfg(unix)]
struct StderrHeld {
    saved: std::os::fd::OwnedFd,
    file: std::fs::File,
}

#[cfg(unix)]
impl StderrHeld {
    fn hold() -> Result<Self> {
        use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
        let file = tempfile::tempfile().context("a file for stderr")?;
        // SAFETY: dup and dup2 over fds this process owns; `saved` takes the
        // new descriptor dup returns.
        let saved = unsafe { libc::dup(2) };
        if saved < 0 {
            bail!("saving stderr: {}", std::io::Error::last_os_error())
        }
        let saved = unsafe { OwnedFd::from_raw_fd(saved) };
        if unsafe { libc::dup2(file.as_raw_fd(), 2) } < 0 {
            bail!("redirecting stderr: {}", std::io::Error::last_os_error())
        }
        Ok(Self { saved, file })
    }
}

#[cfg(unix)]
impl Drop for StderrHeld {
    fn drop(&mut self) {
        use std::{
            io::{Seek, SeekFrom},
            os::fd::AsRawFd,
        };
        // SAFETY: restores fd 2 from the descriptor `hold` saved.
        unsafe { libc::dup2(self.saved.as_raw_fd(), 2) };
        if self.file.seek(SeekFrom::Start(0)).is_ok() {
            let _ = std::io::copy(&mut self.file, &mut std::io::stderr());
        }
    }
}

/// Give the terminal back: what `Live` took, whatever was enabled.
fn give_back_terminal() {
    let mut out = std::io::stdout();
    let _ = out.execute(DisableBracketedPaste);
    let _ = out.execute(DisableMouseCapture);
    let _ = out.execute(DisableFocusChange);
    let _ = out.execute(crossterm::cursor::Show);
    ratatui::restore();
}

struct GiveBack;

impl Drop for GiveBack {
    fn drop(&mut self) {
        give_back_terminal()
    }
}

/// The terminal while the display holds it: raw mode, the alternate
/// screen, bracketed paste, its events and stderr. Dropping it gives all
/// of that back, the event reader first.
struct Live {
    events: Fuse<EventStream>,
    terminal: DefaultTerminal,
    _give_back: GiveBack,
    #[cfg(unix)]
    _stderr: Option<StderrHeld>,
}

impl Live {
    fn take(mouse: bool) -> Result<Self> {
        static HOOK: std::sync::Once = std::sync::Once::new();
        HOOK.call_once(|| {
            let prev = std::panic::take_hook();
            std::panic::set_hook(Box::new(move |info| {
                give_back_terminal();
                prev(info)
            }))
        });
        terminal::enable_raw_mode().context("raw mode")?;
        let give_back = GiveBack;
        let mut out = std::io::stdout();
        out.execute(EnterAlternateScreen)?.execute(EnableBracketedPaste)?;
        if mouse {
            set_mouse(true)
        }
        let terminal = ratatui::Terminal::new(ratatui::backend::CrosstermBackend::new(
            std::io::stdout(),
        ))?;
        Ok(Self {
            events: EventStream::new().fuse(),
            terminal,
            _give_back: give_back,
            #[cfg(unix)]
            _stderr: StderrHeld::hold()
                .inspect_err(|e| error!("stderr stays on the terminal: {e:?}"))
                .ok(),
        })
    }
}

enum Term {
    Live(Live),
    /// Given to a child until the signal resumes it.
    Suspended(oneshot::Receiver<()>),
}

impl Term {
    /// The next terminal event, or `None` when a suspension ends.
    async fn next(&mut self) -> Option<std::io::Result<Event>> {
        match self {
            Term::Live(live) => Some(live.events.select_next_some().await),
            Term::Suspended(rx) => {
                let _ = rx.await;
                None
            }
        }
    }
}

async fn run<X: GXExt>(
    gx: GXHandle<X>,
    env: Env,
    root_exp: CompExp<X>,
    to_rx: mpsc::Receiver<ToTui>,
    control: &TuiControl,
) -> Result<()> {
    let mut suspend_rx = match control.0.suspend_rx.lock().take() {
        Some(rx) => rx,
        None => mpsc::unbounded().1,
    };
    let notify = display(gx, env, root_exp, to_rx, control, &mut suspend_rx).await;
    *control.0.suspend_rx.lock() = Some(suspend_rx);
    let _ = notify?.send(());
    Ok(())
}

/// Whether the terminal is still there. Crossterm's event reader retries
/// a dead terminal's error forever and reports nothing, so a PTY that
/// went away (ssh, tmux) is seen only by asking. `terminal::size` cannot
/// ask on unix: it falls back to `tput`, which answers without a terminal.
fn terminal_connected() -> Result<()> {
    #[cfg(unix)]
    let r = terminal::window_size().map(|_| ());
    #[cfg(not(unix))]
    let r = terminal::size().map(|_| ());
    r.context("the terminal went away")
}

/// Draw and serve events until told to stop, leaving the terminal
/// restored; the value is who to tell that it is. A frame is drawn once
/// a cycle's updates are all in, never in the middle of one.
async fn display<X: GXExt>(
    gx: GXHandle<X>,
    env: Env,
    root_exp: CompExp<X>,
    mut to_rx: mpsc::Receiver<ToTui>,
    control: &TuiControl,
    suspend_rx: &mut mpsc::UnboundedReceiver<Suspend>,
) -> Result<oneshot::Sender<()>> {
    let mut screen = Screen::new(gx.clone(), &env, root_exp.id)?;
    let mut mouse: TRef<X, bool> =
        TRef::new(gx.compile_ref(get_id(&env, &["tui", "mouse"].into())?).await?)?;
    let mut term = Term::Live(Live::take(mouse.t == Some(true))?);
    screen.resize(SizeV::from_terminal()?)?;
    let mut dirty = true;
    // an update arrived since the last batch ended
    let mut updated = false;
    let mut liveness = interval(Duration::from_secs(1));
    liveness.set_missed_tick_behavior(MissedTickBehavior::Skip);
    loop {
        if let Term::Live(live) = &mut term
            && std::mem::take(&mut dirty)
        {
            live.terminal.draw(|f| {
                if let Err(e) = screen.draw(f) {
                    error!("error drawing {e:?}")
                }
            })?;
        }
        select! {
            _ = liveness.tick() => terminal_connected()?,
            m = to_rx.next() => match m {
                None => break Ok(oneshot::channel().0),
                Some(ToTui::Stop(tx)) => break Ok(tx),
                Some(ToTui::Draw) => dirty |= std::mem::take(&mut updated),
                Some(ToTui::Update(id, v)) => {
                    updated = true;
                    if let Ok(Some(v)) = mouse.update(id, &v)
                        && let Term::Live(_) = term
                    {
                        set_mouse(*v)
                    }
                    if let Err(e) = screen.update(id, v).await {
                        error!("error handling update {e:?}")
                    }
                },
            },
            s = suspend_rx.next() => if let Some(Suspend { ack }) = s {
                if let Term::Suspended(_) = term {
                    let _ = ack.send(Err(anyhow!("the display is already suspended")));
                } else {
                    let (resume_tx, resume_rx) = oneshot::channel();
                    term = Term::Suspended(resume_rx);
                    let _ = ack.send(Ok(resume_tx));
                }
            },
            t = term.next() => match t {
                None => {
                    term = Term::Live(Live::take(mouse.t == Some(true))?);
                    screen.resize(SizeV::from_terminal()?)?;
                    dirty = true;
                },
                Some(Ok(e)) => {
                    dirty |= matches!(e, Event::Resize(..));
                    match screen.event(&e).await {
                        Ok(true) => (),
                        Ok(false) => fire(&control.0.stop, Ok(())),
                        Err(e) => error!("error handling event {e:?}"),
                    }
                },
                Some(Err(e)) => bail!("reading the terminal: {e}"),
            }
        }
    }
}

#[async_trait]
impl<X: GXExt> CustomDisplay<X> for Tui<X> {
    async fn clear(&mut self) {
        self.clear().await;
    }

    async fn process_update(&mut self, _env: &Env, id: ExprId, v: Value) {
        self.send(ToTui::Update(id, v)).await;
    }

    async fn batch_done(&mut self) {
        self.send(ToTui::Draw).await;
    }
}

graphix_derive::defpackage! {
    builtins => [Exit, SuspendB],
    is_custom => |gx, env, e| graphix_package::shows_as(env, e, &["tui", "Tui"], |t| t),
    init_custom => |gx, env, stop, e, _run_on_main| {
        Ok(Box::new(Tui::<X>::start(gx, env.clone(), e, stop)))
    },
}
