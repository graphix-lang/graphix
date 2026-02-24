#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::{
    env::Env,
    expr::{ExprId, ModPath},
    typ::Type,
    BindId,
};
use graphix_package::CustomDisplay;
use graphix_rt::{CallableId, CompExp, GXExt, GXHandle, Ref, TRef};
use log::error;
use netidx::publisher::Value;
use smallvec::SmallVec;
use std::{future::Future, marker::PhantomData, pin::Pin, sync::LazyLock};
use tokio::sync::oneshot;
use triomphe::Arc;
use types::{HAlignV, LengthV, PaddingV, SizeV, VAlignV};
use winit::{event_loop::EventLoopProxy, window::WindowId};

mod button;
mod container;
mod convert;
mod event_loop;
mod render;
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
    Call(CallableId),
}

/// Trait for GUI widgets. Unlike TUI widgets, GUI widgets are not
/// async — handle_update is synchronous, and the view method builds
/// an iced Element tree.
pub(crate) trait GuiWidget<X: GXExt>: Send + Sync + 'static {
    /// Process a value update from graphix. Widgets that own child
    /// refs use `rt` to `block_on` recompilation of their subtree.
    /// Returns `true` if the widget changed and the window should redraw.
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool>;

    /// Build the iced Element tree for rendering.
    fn view(&self) -> IcedElement<'_>;
}

pub(crate) type GuiW<X> = Box<dyn GuiWidget<X>>;

/// Future type for widget compilation (avoids infinite-size async fn).
type CompileFut<X> = Pin<Box<dyn Future<Output = Result<GuiW<X>>> + Send + 'static>>;

/// Empty widget placeholder.
pub(crate) struct EmptyW;

impl<X: GXExt> GuiWidget<X> for EmptyW {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        _id: ExprId,
        _v: &Value,
    ) -> Result<bool> {
        Ok(false)
    }

    fn view(&self) -> IcedElement<'_> {
        iced_widget::Space::new().into()
    }
}

/// Generate a flex layout widget (Row or Column). All parameters use
/// call-site tokens to satisfy macro hygiene for local variable names.
macro_rules! flex_widget {
    ($name:ident, $label:literal,
     $spacing:ident, $padding:ident, $width:ident, $height:ident,
     $align_ty:ty, $align:ident, $Widget:ident, $align_set:ident,
     [$($f:ident),+]) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            $spacing: TRef<X, f64>,
            $padding: TRef<X, PaddingV>,
            $width: TRef<X, LengthV>,
            $height: TRef<X, LengthV>,
            $align: TRef<X, $align_ty>,
            children_ref: Ref<X>,
            children: Vec<GuiW<X>>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
                let [(_, children), $((_, $f)),+] =
                    source.cast_to::<[(ArcStr, u64); 6]>()
                        .context(concat!($label, " flds"))?;
                let (children_ref, $($f),+) = tokio::try_join!(
                    gx.compile_ref(children),
                    $(gx.compile_ref($f)),+
                )?;
                let compiled_children = match children_ref.last.as_ref() {
                    None => vec![],
                    Some(v) => compile_children(gx.clone(), v.clone()).await
                        .context(concat!($label, " children"))?,
                };
                Ok(Box::new(Self {
                    gx: gx.clone(),
                    $spacing: TRef::new($spacing)
                        .context(concat!($label, " tref spacing"))?,
                    $padding: TRef::new($padding)
                        .context(concat!($label, " tref padding"))?,
                    $width: TRef::new($width)
                        .context(concat!($label, " tref width"))?,
                    $height: TRef::new($height)
                        .context(concat!($label, " tref height"))?,
                    $align: TRef::new($align)
                        .context(concat!($label, " tref ", stringify!($align)))?,
                    children_ref,
                    children: compiled_children,
                }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = false;
                changed |= self.$spacing.update(id, v)
                    .context(concat!($label, " update spacing"))?.is_some();
                changed |= self.$padding.update(id, v)
                    .context(concat!($label, " update padding"))?.is_some();
                changed |= self.$width.update(id, v)
                    .context(concat!($label, " update width"))?.is_some();
                changed |= self.$height.update(id, v)
                    .context(concat!($label, " update height"))?.is_some();
                changed |= self.$align.update(id, v)
                    .context(concat!($label, " update ", stringify!($align)))?.is_some();
                if id == self.children_ref.id {
                    self.children_ref.last = Some(v.clone());
                    self.children = rt.block_on(
                        compile_children(self.gx.clone(), v.clone())
                    ).context(concat!($label, " children recompile"))?;
                    changed = true;
                }
                for child in &mut self.children {
                    changed |= child.handle_update(rt, id, v)?;
                }
                Ok(changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let mut w = iced_widget::$Widget::new();
                if let Some(sp) = self.$spacing.t {
                    w = w.spacing(sp as f32);
                }
                if let Some(p) = self.$padding.t.as_ref() {
                    w = w.padding(p.0);
                }
                if let Some(wi) = self.$width.t.as_ref() {
                    w = w.width(wi.0);
                }
                if let Some(h) = self.$height.t.as_ref() {
                    w = w.height(h.0);
                }
                if let Some(a) = self.$align.t.as_ref() {
                    w = w.$align_set(a.0);
                }
                for child in &self.children {
                    w = w.push(child.view());
                }
                w.into()
            }
        }
    };
}

flex_widget!(
    RowW,
    "row",
    spacing,
    padding,
    width,
    height,
    VAlignV,
    valign,
    Row,
    align_y,
    [height, padding, spacing, valign, width]
);

flex_widget!(
    ColumnW,
    "column",
    spacing,
    padding,
    width,
    height,
    HAlignV,
    halign,
    Column,
    align_x,
    [halign, height, padding, spacing, width]
);

/// Compile a widget value into a GuiW. Returns a boxed future to
/// avoid infinite-size futures from recursive async calls.
pub(crate) fn compile<X: GXExt>(gx: GXHandle<X>, source: Value) -> CompileFut<X> {
    Box::pin(async move {
        match source.cast_to::<(ArcStr, Value)>()? {
            (s, v) if &s == "Text" => text::TextW::compile(gx, v).await,
            (s, v) if &s == "Column" => ColumnW::compile(gx, v).await,
            (s, v) if &s == "Row" => RowW::compile(gx, v).await,
            (s, v) if &s == "Container" => container::ContainerW::compile(gx, v).await,
            (s, v) if &s == "Button" => button::ButtonW::compile(gx, v).await,
            (s, v) if &s == "Space" => space::SpaceW::compile(gx, v).await,
            (s, v) => bail!("invalid gui widget type `{s}({v})"),
        }
    })
}

/// Compile an array of widget values into a Vec of GuiW.
pub(crate) async fn compile_children<X: GXExt>(
    gx: GXHandle<X>,
    v: Value,
) -> Result<Vec<GuiW<X>>> {
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let futs: Vec<_> = items.into_iter().map(|item| compile(gx.clone(), item)).collect();
    futures::future::try_join_all(futs).await
}

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
