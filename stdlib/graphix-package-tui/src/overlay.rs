use super::{SizeReport, TuiW, TuiWidget, compile, compile_each, layout::ConstraintV};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::{Constraint, Flex, Layout, Rect},
    widgets::Clear,
};

graphix_rt::props! {
    struct LayerProps {
        height: ConstraintV,
        width: ConstraintV,
    }
}

struct LayerW<X: GXExt> {
    p: LayerProps<X>,
    child: TuiW,
    size: SizeReport<X>,
}

impl<X: GXExt> LayerW<X> {
    async fn compile(gx: GXHandle<X>, v: Value) -> Result<Self> {
        let p = LayerProps::compile(&gx, &v).await.context("layer")?;
        let size = SizeReport::compile(&gx, &v).await?;
        #[derive(FromValue)]
        struct Fields {
            child: Value,
        }
        let Fields { child } = v.cast_to().context("layer fields")?;
        let child = compile(gx, child).await.context("compiling layer child")?;
        Ok(Self { p, child, size })
    }

    /// The layer's rectangle: centered in `rect`, sized by the
    /// constraints (60% until the refs deliver).
    fn rect(&self, rect: Rect) -> Rect {
        let width = self.p.width.t.map_or(Constraint::Percentage(60), |c| c.0);
        let height = self.p.height.t.map_or(Constraint::Percentage(60), |c| c.0);
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
        #[derive(FromValue)]
        struct Fields {
            base: Value,
            layers: u64,
        }
        let Fields { base, layers } = v.cast_to().context("overlay fields")?;
        let base = compile(gx.clone(), base).await.context("compiling overlay base")?;
        let layers_ref = gx.compile_ref(layers).await.context("compiling layers ref")?;
        let mut t = Self { gx, base, layers: vec![], layers_ref };
        if let Some(v) = t.layers_ref.last.take() {
            t.set_layers(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_layers(&mut self, v: Value) -> Result<()> {
        self.layers = compile_each(v, |v| LayerW::compile(self.gx.clone(), v)).await?;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for OverlayW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        match self.layers.last_mut() {
            Some(l) => l.child.handle_event(v).await,
            None => self.base.handle_event(v).await,
        }
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        if self.layers_ref.id == id {
            self.set_layers(v.clone()).await?;
        }
        self.base.handle_update(id, v.clone()).await?;
        for l in &mut self.layers {
            l.p.update(id, &v).context("layer")?;
            l.child.handle_update(id, v.clone()).await?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        self.base.draw(frame, rect)?;
        for l in &mut self.layers {
            let lrect = l.rect(rect);
            l.size.report(lrect)?;
            frame.render_widget(Clear, lrect);
            l.child.draw(frame, lrect)?;
        }
        Ok(())
    }
}
