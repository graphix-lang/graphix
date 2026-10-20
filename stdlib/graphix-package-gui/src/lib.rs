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
    ResizeSettled(WindowId, crate::types::WindowSizeV),
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
    done: oneshot::Receiver<()>,
    ph: PhantomData<X>,
}

async fn wait_for_stop(rx: oneshot::Receiver<()>, done: &mut oneshot::Receiver<()>) {
    tokio::select! {
        _ = rx => (),
        _ = done => (),
    }
}

#[cfg(test)]
mod shutdown_tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn a_stopped_event_loop_releases_a_queued_stop() {
        let (stop_tx, stop_rx) = oneshot::channel();
        let (done_tx, mut done_rx) = oneshot::channel();
        done_tx.send(()).unwrap();
        tokio::time::timeout(
            Duration::from_millis(100),
            wait_for_stop(stop_rx, &mut done_rx),
        )
        .await
        .expect("clear waited for a stopped event loop");
        assert!(stop_tx.is_closed());
    }
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
        let (done_tx, done) = oneshot::channel();
        let rt_handle = tokio::runtime::Handle::current();
        run_on_main
            .run(Box::new(move || {
                event_loop::run(gx, root, proxy_tx, stop, rt_handle);
                let _ = done_tx.send(());
            }))
            .context("a gui needs the main thread")?;
        let proxy = proxy_rx.await.context("the event loop did not start")??;
        Ok(Self { proxy, done, ph: PhantomData })
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
        if !matches!(self.done.try_recv(), Err(oneshot::error::TryRecvError::Empty)) {
            return;
        }
        let (tx, rx) = oneshot::channel::<()>();
        let _ = self.proxy.send_event(ToGui::Stop(tx));
        wait_for_stop(rx, &mut self.done).await;
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
    shutdown => || clipboard::release(),
}
