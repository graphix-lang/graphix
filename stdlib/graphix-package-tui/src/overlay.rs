use super::{SizeV, TuiW, TuiWidget, compile, layout::ConstraintV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use futures::future;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use netidx::publisher::Value;
use ratatui::{
    Frame,
    layout::{Constraint, Flex, Layout, Rect},
    widgets::Clear,
};
use smallvec::SmallVec;
use tokio::try_join;

struct LayerW<X: GXExt> {
    child: TuiW,
    width: TRef<X, ConstraintV>,
    height: TRef<X, ConstraintV>,
    size_ref: Ref<X>,
    last_size: SizeV,
}

impl<X: GXExt> LayerW<X> {
    async fn compile(gx: GXHandle<X>, v: Value) -> Result<Self> {
        let ((_, child), (_, height), (_, size), (_, width)) = v
            .cast_to::<((ArcStr, Value), (ArcStr, u64), (ArcStr, u64), (ArcStr, u64))>()
            .context("layer fields")?;
        let child = compile(gx.clone(), child).await.context("compiling layer child")?;
        let (width, height, size_ref) = try_join! {
            gx.compile_ref(width),
            gx.compile_ref(height),
            gx.compile_ref(size)
        }?;
        let width = TRef::<X, ConstraintV>::new(width).context("layer tref width")?;
        let height = TRef::<X, ConstraintV>::new(height).context("layer tref height")?;
        Ok(Self { child, width, height, size_ref, last_size: SizeV::default() })
    }

    /// The layer's rectangle: centered in `rect`, sized by the
    /// constraints (60% until the refs deliver).
    fn rect(&self, rect: Rect) -> Rect {
        let width = self.width.t.map(|c| c.0).unwrap_or(Constraint::Percentage(60));
        let height = self.height.t.map(|c| c.0).unwrap_or(Constraint::Percentage(60));
        let [rect] = Layout::horizontal([width]).flex(Flex::Center).areas(rect);
        let [rect] = Layout::vertical([height]).flex(Flex::Center).areas(rect);
        rect
    }
}

pub(super) struct OverlayW<X: GXExt> {
    gx: GXHandle<X>,
    base: TuiW,
    layers: Vec<LayerW<X>>,
    layers_ref: Ref<X>,
}

impl<X: GXExt> OverlayW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let ((_, base), (_, layers)) =
            v.cast_to::<((ArcStr, Value), (ArcStr, u64))>().context("overlay fields")?;
        let base = compile(gx.clone(), base).await.context("compiling overlay base")?;
        let layers_ref = gx.compile_ref(layers).await.context("compiling layers ref")?;
        let mut t = Self { gx, base, layers: vec![], layers_ref };
        if let Some(v) = t.layers_ref.last.take() {
            t.set_layers(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_layers(&mut self, v: Value) -> Result<()> {
        self.layers =
            future::join_all(v.cast_to::<SmallVec<[Value; 4]>>()?.into_iter().map(|v| {
                let gx = self.gx.clone();
                async move { LayerW::compile(gx, v).await }
            }))
            .await
            .into_iter()
            .collect::<Result<Vec<_>>>()?;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for OverlayW<X> {
    async fn handle_event(&mut self, e: Event, v: Value) -> Result<()> {
        // the modal rule: the topmost layer captures input while any
        // layer is up
        match self.layers.last_mut() {
            Some(l) => l.child.handle_event(e, v).await,
            None => self.base.handle_event(e, v).await,
        }
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        if self.layers_ref.id == id {
            self.set_layers(v.clone()).await?;
        }
        self.base.handle_update(id, v.clone()).await?;
        for l in &mut self.layers {
            l.width.update(id, &v).context("layer width update")?;
            l.height.update(id, &v).context("layer height update")?;
            l.child.handle_update(id, v.clone()).await?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        self.base.draw(frame, rect)?;
        for l in &mut self.layers {
            let lrect = l.rect(rect);
            let size = SizeV::from(lrect);
            if l.last_size != size {
                l.last_size = size;
                l.size_ref.set_deref(size)?;
            }
            frame.render_widget(Clear, lrect);
            l.child.draw(frame, lrect)?;
        }
        Ok(())
    }
}
