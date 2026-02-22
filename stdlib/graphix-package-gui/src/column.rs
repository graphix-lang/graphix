use crate::{
    compile,
    types::{HAlignV, LengthV, PaddingV},
    GuiW, GuiWidget, IcedElement,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use smallvec::SmallVec;
use tokio::try_join;

pub(crate) struct ColumnW<X: GXExt> {
    gx: GXHandle<X>,
    spacing: TRef<X, f64>,
    padding: TRef<X, PaddingV>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    halign: TRef<X, HAlignV>,
    children_ref: Ref<X>,
    children: Vec<GuiW<X>>,
}

impl<X: GXExt> ColumnW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, children), (_, halign), (_, height), (_, padding), (_, spacing), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("column flds")?;
        let (children_ref, halign, height, padding, spacing, width) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(halign),
            gx.compile_ref(height),
            gx.compile_ref(padding),
            gx.compile_ref(spacing),
            gx.compile_ref(width),
        }?;
        let compiled_children = match children_ref.last.as_ref() {
            None => vec![],
            Some(v) => compile_children(gx.clone(), v.clone()).await?,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            spacing: TRef::new(spacing).context("column tref spacing")?,
            padding: TRef::new(padding).context("column tref padding")?,
            width: TRef::new(width).context("column tref width")?,
            height: TRef::new(height).context("column tref height")?,
            halign: TRef::new(halign).context("column tref halign")?,
            children_ref,
            children: compiled_children,
        }))
    }
}

async fn compile_children<X: GXExt>(
    gx: GXHandle<X>,
    v: Value,
) -> Result<Vec<GuiW<X>>> {
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let mut children = Vec::with_capacity(items.len());
    for item in items {
        children.push(compile(gx.clone(), item).await?);
    }
    Ok(children)
}

impl<X: GXExt> GuiWidget<X> for ColumnW<X> {
    fn handle_update(&mut self, id: ExprId, v: &Value) -> Result<()> {
        self.spacing.update(id, v).context("column update spacing")?;
        self.padding.update(id, v).context("column update padding")?;
        self.width.update(id, v).context("column update width")?;
        self.height.update(id, v).context("column update height")?;
        self.halign.update(id, v).context("column update halign")?;
        for child in &mut self.children {
            child.handle_update(id, v)?;
        }
        Ok(())
    }

    fn needs_recompile(&self, id: ExprId) -> bool {
        id == self.children_ref.id
    }

    fn view(&self) -> IcedElement<'_> {
        let mut col = widget::Column::new();
        if let Some(sp) = self.spacing.t {
            col = col.spacing(sp as f32);
        }
        if let Some(p) = self.padding.t.as_ref() {
            col = col.padding(p.0);
        }
        if let Some(w) = self.width.t.as_ref() {
            col = col.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            col = col.height(h.0);
        }
        if let Some(a) = self.halign.t.as_ref() {
            col = col.align_x(a.0);
        }
        for child in &self.children {
            col = col.push(child.view());
        }
        col.into()
    }
}
