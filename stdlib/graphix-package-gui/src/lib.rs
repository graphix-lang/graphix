#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::{
    env::Env,
    expr::ExprId,
    typ::{Mutability, Type},
};
use graphix_package::{CustomDisplay, Stop};
use graphix_rt::{CompExp, GXExt, GXHandle};
use log::error;
use netidx::publisher::Value;
use std::marker::PhantomData;
use tokio::sync::oneshot;
use triomphe::Arc;
use winit::{event_loop::EventLoopProxy, window::WindowId};

mod clipboard;
pub mod convert;
mod event_loop;
pub mod frame;
pub mod render;
pub mod theme;
pub mod types;
pub mod widgets;
pub mod window;

#[cfg(test)]
mod test;

pub(crate) enum ToGui {
    Update(ExprId, Value),
    /// Render tick during a resize drag; the size comes from the
    /// window's `pending_resize` slot. Never touches the size ref.
    ResizeRenderTick(WindowId),
    /// Fires once per resize burst with the final logical size; the
    /// only event that writes the runtime's size ref.
    ResizeSettled(WindowId, crate::types::SizeV),
    /// Wake the event loop to render widget state mutated outside
    /// the iced event cycle.
    Redraw,
    Stop(oneshot::Sender<()>),
}

/// Thread-safe, cheaply cloned handle for widgets that update view
/// state from background tasks; `wake()` posts a `ToGui::Redraw`.
#[derive(Clone)]
pub(crate) struct RedrawWaker {
    proxy: EventLoopProxy<ToGui>,
}

impl RedrawWaker {
    pub(crate) fn new(proxy: EventLoopProxy<ToGui>) -> Self {
        Self { proxy }
    }

    pub(crate) fn wake(&self) {
        let _ = self.proxy.send_event(ToGui::Redraw);
    }
}

/// Set by the event loop once its proxy exists; widgets compiled
/// earlier pick it up from here.
pub(crate) static REDRAW_WAKER: std::sync::OnceLock<RedrawWaker> =
    std::sync::OnceLock::new();

struct Gui<X: GXExt> {
    proxy: EventLoopProxy<ToGui>,
    ph: PhantomData<X>,
}

impl<X: GXExt> Gui<X> {
    async fn start(
        gx: &GXHandle<X>,
        _env: Env,
        root: CompExp<X>,
        stop: Stop,
        run_on_main: graphix_package::MainThreadHandle,
    ) -> Result<Self> {
        let gx = gx.clone();
        let (proxy_tx, proxy_rx) = oneshot::channel();
        let rt_handle = tokio::runtime::Handle::current();
        run_on_main
            .run(Box::new(move || {
                event_loop::run(gx, root, proxy_tx, stop, rt_handle);
            }))
            .context("a gui needs the main thread")?;
        let proxy = proxy_rx.await.context("the event loop did not start")??;
        Ok(Self { proxy, ph: PhantomData })
    }

    fn update(&self, id: ExprId, v: Value) {
        if self.proxy.send_event(ToGui::Update(id, v)).is_err() {
            error!("could not send update because gui event loop closed")
        }
    }
}

#[async_trait]
impl<X: GXExt> CustomDisplay<X> for Gui<X> {
    async fn clear(&mut self) {
        let (tx, rx) = oneshot::channel::<()>();
        // CR claude for claude: [bug/high] This waits for an answer from an event loop
        // that may already have stopped. When the user closes the last window,
        // GuiHandler::finish sends Ok on the Stop channel and exits the loop. The shell
        // then calls clear. If this Stop arrives after the iteration's user-event
        // drain, run_app_on_demand returns with it still queued. event_loop::run then
        // stores the EventLoop, and its receiver with it, back in EVENT_LOOP, so tx is
        // never dropped and rx.await never resolves. The window closes but graphix
        // never exits, and Ctrl-C does not free it: about half of close-button exits
        // hang. The run_app_on_demand error path always hangs, because stop is sent
        // after the loop has returned. Fix: give Gui a way to see that the loop is
        // gone. For example, run() could fire a `done` oneshot (or set a shared flag)
        // once run_app_on_demand returns, and clear would select on rx vs done, or skip
        // the send once done. user_event should also ignore a Stop whose tx is closed.
        // probe: design/review-2026-10-12/repro/gui-core-01.sh (gui-core-01)
        let _ = self.proxy.send_event(ToGui::Stop(tx));
        let _ = rx.await;
    }

    async fn process_update(&mut self, _env: &Env, id: ExprId, v: Value) {
        self.update(id, v);
    }

    async fn batch_done(&mut self) {}
}

graphix_derive::defpackage! {
    builtins => [
        clipboard::ReadText,
        clipboard::WriteText,
        clipboard::ReadImage,
        clipboard::WriteImage,
        clipboard::ReadHtml,
        clipboard::WriteHtml,
        clipboard::ReadFiles,
        clipboard::WriteFiles,
        clipboard::Clear,
    ],
    is_custom => |gx, env, e| {
        graphix_package::shows_as(env, e, &["gui", "Window"], |w| {
            Type::Array(Arc::new(Type::ByRef(Mutability::Shared, Arc::new(w))))
        })
    },
    init_custom => |gx, env, stop, e, run_on_main| {
        Ok(Box::new(Gui::<X>::start(gx, env.clone(), e, stop, run_on_main).await?))
    },
}
