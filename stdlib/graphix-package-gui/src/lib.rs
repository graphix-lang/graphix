#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use async_trait::async_trait;
use graphix_compiler::{
    env::Env,
    expr::{ExprId, ModPath},
    typ::Type,
};
use graphix_package::CustomDisplay;
use graphix_rt::{CompExp, GXExt, GXHandle};
use log::error;
use netidx::publisher::Value;
use std::{marker::PhantomData, sync::LazyLock};
use tokio::sync::oneshot;
use triomphe::Arc;
use types::SizeV;
use winit::{event_loop::EventLoopProxy, window::WindowId};

mod convert;
mod event_loop;
mod render;
mod types;
pub(crate) mod widgets;
mod window;

#[cfg(test)]
mod test;

pub(crate) enum ToGui {
    Update(ExprId, Value),
    ResizeTimer(WindowId, SizeV),
    Stop(oneshot::Sender<()>),
}

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

static GUITYP: LazyLock<Type> = LazyLock::new(|| {
    Type::Array(Arc::new(Type::ByRef(Arc::new(Type::Ref {
        scope: ModPath::root(),
        name: ModPath::from(["gui", "Window"]),
        params: Arc::from_iter([]),
    }))))
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
    init_custom => |gx, env, stop, e, run_on_main| {
        Ok(Box::new(Gui::<X>::start(gx, env.clone(), e, stop, run_on_main).await))
    },
}
