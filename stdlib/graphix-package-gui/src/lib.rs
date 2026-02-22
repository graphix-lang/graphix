#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::{
    env::Env,
    expr::{ExprId, ModPath},
    typ::Type,
    BindId,
};
use graphix_package::CustomDisplay;
use graphix_rt::{CompExp, GXExt, GXHandle};
use log::error;
use netidx::publisher::Value;
use std::{future::Future, marker::PhantomData, pin::Pin, sync::LazyLock};
use tokio::sync::oneshot;
use triomphe::Arc;

mod button;
mod column;
mod container;
mod convert;
mod event_loop;
mod render;
mod row;
mod space;
mod text;
mod types;
mod window;

/// Concrete iced renderer type used throughout the GUI package.
/// Must match iced_widget's default Renderer parameter.
pub(crate) type Renderer = iced_renderer::Renderer;

/// Concrete iced Element type with our Message/Theme/Renderer.
pub(crate) type IcedElement<'a> =
    iced_core::Element<'a, Message, iced_core::Theme, Renderer>;

/// Message type for iced widget interactions.
#[derive(Debug, Clone)]
pub(crate) enum Message {
    Set(BindId, Value),
}

/// Trait for GUI widgets. Unlike TUI widgets, GUI widgets are not
/// async — handle_update is synchronous, and the view method builds
/// an iced Element tree.
pub(crate) trait GuiWidget<X: GXExt>: Send + Sync + 'static {
    /// Process a value update from graphix.
    fn handle_update(&mut self, id: ExprId, v: &Value) -> Result<()>;

    /// Returns true if the given ExprId matches a child ref that
    /// needs async recompilation. The event loop handles recompilation
    /// when this returns true.
    fn needs_recompile(&self, _id: ExprId) -> bool {
        false
    }

    /// Build the iced Element tree for rendering.
    fn view(&self) -> IcedElement<'_>;
}

pub(crate) type GuiW<X> = Box<dyn GuiWidget<X>>;

/// Future type for widget compilation (avoids infinite-size async fn).
type CompileFut<X> = Pin<Box<dyn Future<Output = Result<GuiW<X>>> + Send + 'static>>;

/// Empty widget placeholder.
pub(crate) struct EmptyW;

impl<X: GXExt> GuiWidget<X> for EmptyW {
    fn handle_update(&mut self, _id: ExprId, _v: &Value) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> IcedElement<'_> {
        iced_widget::Space::new().into()
    }
}

/// Compile a widget value into a GuiW. Returns a boxed future to
/// avoid infinite-size futures from recursive async calls.
pub(crate) fn compile<X: GXExt>(gx: GXHandle<X>, source: Value) -> CompileFut<X> {
    Box::pin(async move {
        match source.cast_to::<(ArcStr, Value)>()? {
            (s, v) if &s == "Text" => text::TextW::compile(gx, v).await,
            (s, v) if &s == "Column" => column::ColumnW::compile(gx, v).await,
            (s, v) if &s == "Row" => row::RowW::compile(gx, v).await,
            (s, v) if &s == "Container" => container::ContainerW::compile(gx, v).await,
            (s, v) if &s == "Button" => button::ButtonW::compile(gx, v).await,
            (s, v) if &s == "Space" => space::SpaceW::compile(gx, v).await,
            (s, v) => bail!("invalid gui widget type `{s}({v})"),
        }
    })
}

pub(crate) enum ToGui {
    Update(ExprId, Value),
    Stop(oneshot::Sender<()>),
}

struct Gui<X: GXExt> {
    to: std::sync::mpsc::Sender<ToGui>,
    main_thread_rx: Option<graphix_package::MainThreadRx>,
    ph: PhantomData<X>,
}

impl<X: GXExt> Gui<X> {
    fn start(
        gx: &GXHandle<X>,
        env: Env,
        root: CompExp<X>,
        stop: oneshot::Sender<()>,
        main_thread_rx: graphix_package::MainThreadRx,
    ) -> Self {
        let gx = gx.clone();
        let (to_tx, to_rx) = std::sync::mpsc::channel::<ToGui>();
        let rt_handle = tokio::runtime::Handle::current();
        std::thread::Builder::new()
            .name("graphix-gui".into())
            .spawn(move || {
                event_loop::run(gx, env, root, to_rx, stop, rt_handle);
            })
            .expect("failed to spawn GUI thread");
        Self { to: to_tx, main_thread_rx: Some(main_thread_rx), ph: PhantomData }
    }

    fn update(&self, id: ExprId, v: Value) {
        if self.to.send(ToGui::Update(id, v)).is_err() {
            error!("could not send update because gui task died")
        }
    }

    fn clear(&mut self) -> Option<graphix_package::MainThreadRx> {
        let (tx, rx) = oneshot::channel::<()>();
        let _ = self.to.send(ToGui::Stop(tx));
        let _ = rx.blocking_recv();
        self.main_thread_rx.take()
    }
}

static GUITYP: LazyLock<Type> = LazyLock::new(|| Type::Ref {
    scope: ModPath::root(),
    name: ModPath::from(["gui", "Window"]),
    params: Arc::from_iter([]),
});

#[async_trait]
impl<X: GXExt> CustomDisplay<X> for Gui<X> {
    async fn clear(&mut self) -> Option<graphix_package::MainThreadRx> {
        Gui::clear(self)
    }

    async fn process_update(&mut self, _env: &Env, id: ExprId, v: Value) {
        self.update(id, v);
    }
}

graphix_derive::defpackage! {
    builtins => [],
    is_custom => |gx, env, e| {
        if let Some(typ) = e.typ.with_deref(|t| t.cloned())
            && typ != Type::Bottom
            && typ != Type::Any
        {
            GUITYP.contains(env, &typ).unwrap_or(false)
        } else {
            false
        }
    },
    init_custom => |gx, env, stop, e, main_thread_rx| {
        let rx = main_thread_rx.expect("GUI package requires main_thread");
        Ok(Box::new(Gui::<X>::start(gx, env.clone(), e, stop, rx)))
    },
    main_thread => |tx| {
        event_loop::run_main_thread(tx);
    },
}
