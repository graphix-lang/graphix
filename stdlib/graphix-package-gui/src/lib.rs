#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use async_trait::async_trait;
use graphix_compiler::{
    env::Env,
    expr::{ExprId, ModPath},
    typ::{Type, TypeRef},
};
use graphix_package::CustomDisplay;
use graphix_rt::{CompExp, GXExt, GXHandle};
use log::error;
use netidx::publisher::Value;
use std::{marker::PhantomData, sync::LazyLock};
use tokio::sync::oneshot;
use triomphe::Arc;
use winit::{event_loop::EventLoopProxy, window::WindowId};

mod clipboard;
pub mod convert;
mod event_loop;
pub mod render;
pub mod theme;
pub mod types;
pub mod widgets;
pub mod window;

#[cfg(test)]
mod test;

pub(crate) enum ToGui {
    Update(ExprId, Value),
    /// Fires once per resize burst, 100ms after the first `Resized`
    /// event in the burst. The payload carries only the `WindowId`;
    /// the event loop reads the most recent size from the window's
    /// `pending_resize` slot rather than passing it through here.
    /// Used purely to schedule renders during a drag — never touches
    /// the graphix size ref (see `ResizeEnd`).
    ResizeTimer(WindowId),
    /// Fires once after a resize *burst* ends: the window has not
    /// received a `Resized` event for `RESIZE_END_DEBOUNCE`. Carries
    /// the final logical size the runtime's size ref should be set
    /// to. Separate from `ResizeTimer` so that writing back to the
    /// size ref — which echoes through the runtime and may trigger
    /// a feedback `request_inner_size` — happens exactly once per
    /// drag, not 10x/sec.
    ResizeEnd(WindowId, crate::types::SizeV),
    /// Wake the winit event loop so it runs its `about_to_wait`
    /// render pass, picking up any widget state that was mutated
    /// outside the iced event cycle (e.g. a netidx subscription
    /// update setting `cells.dirty = true` on a data_table).
    Redraw,
    Stop(oneshot::Sender<()>),
}

/// Thread-safe handle the event loop hands out to widgets that
/// update their view state from background tasks (netidx
/// subscriptions in data_table being the motivating case). Calling
/// `.wake()` posts a `ToGui::Redraw` which the event loop processes
/// by flagging every window for redraw. Cheap to clone.
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

/// Set by the event loop after it creates its proxy, so widgets
/// compiled before the proxy exists (compilation happens inside the
/// same call that builds the window) can still pick it up.
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
        stop: oneshot::Sender<()>,
        run_on_main: graphix_package::MainThreadHandle,
    ) -> Self {
        let gx = gx.clone();
        let (proxy_tx, proxy_rx) = oneshot::channel();
        let rt_handle = tokio::runtime::Handle::current();
        run_on_main
            .run(Box::new(move || {
                event_loop::run(gx, root, proxy_tx, stop, rt_handle);
            }))
            .expect("main thread receiver dropped");
        let proxy = proxy_rx.await.expect("event loop failed to send proxy");
        Self { proxy, ph: PhantomData }
    }

    fn update(&self, id: ExprId, v: Value) {
        if self.proxy.send_event(ToGui::Update(id, v)).is_err() {
            error!("could not send update because gui event loop closed")
        }
    }
}

pub static GUITYP: LazyLock<Type> = LazyLock::new(|| {
    Type::Array(Arc::new(Type::ByRef(Arc::new(Type::Ref(TypeRef::synthetic(
        ModPath::root(),
        ModPath::from(["gui", "Window"]),
        Arc::from_iter([]),
    ))))))
});

#[async_trait]
impl<X: GXExt> CustomDisplay<X> for Gui<X> {
    async fn clear(&mut self) {
        let (tx, rx) = oneshot::channel::<()>();
        let _ = self.proxy.send_event(ToGui::Stop(tx));
        let _ = rx.await;
    }

    async fn process_update(&mut self, _env: &Env, id: ExprId, v: Value) {
        self.update(id, v);
    }
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
        if let Some(typ) = e.typ.with_deref(|t| t.cloned())
            && typ != Type::Bottom
            && typ != Type::Any
        {
            GUITYP.contains(env, &typ).unwrap_or(false)
        } else {
            false
        }
    },
    init_custom => |gx, env, stop, e, run_on_main| {
        Ok(Box::new(Gui::<X>::start(gx, env.clone(), e, stop, run_on_main).await))
    },
}
