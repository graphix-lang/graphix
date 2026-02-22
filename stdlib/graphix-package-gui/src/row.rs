use crate::{
    compile,
    types::{LengthV, PaddingV, VAlignV},
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

pub(crate) struct RowW<X: GXExt> {
    gx: GXHandle<X>,
    spacing: TRef<X, f64>,
    padding: TRef<X, PaddingV>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    valign: TRef<X, VAlignV>,
    children_ref: Ref<X>,
    children: Vec<GuiW<X>>,
}

impl<X: GXExt> RowW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, children), (_, height), (_, padding), (_, spacing), (_, valign), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("row flds")?;
        let (children_ref, height, padding, spacing, valign, width) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(height),
            gx.compile_ref(padding),
            gx.compile_ref(spacing),
            gx.compile_ref(valign),
            gx.compile_ref(width),
        }?;
        let compiled_children = match children_ref.last.as_ref() {
            None => vec![],
            Some(v) => {
                let items = v.clone().cast_to::<SmallVec<[Value; 8]>>()?;
                let mut children = Vec::with_capacity(items.len());
                for item in items {
                    children.push(compile(gx.clone(), item).await?);
                }
                children
            }
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            spacing: TRef::new(spacing).context("row tref spacing")?,
            padding: TRef::new(padding).context("row tref padding")?,
            width: TRef::new(width).context("row tref width")?,
            height: TRef::new(height).context("row tref height")?,
            valign: TRef::new(valign).context("row tref valign")?,
            children_ref,
            children: compiled_children,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for RowW<X> {
    fn handle_update(&mut self, id: ExprId, v: &Value) -> Result<()> {
        self.spacing.update(id, v).context("row update spacing")?;
        self.padding.update(id, v).context("row update padding")?;
        self.width.update(id, v).context("row update width")?;
        self.height.update(id, v).context("row update height")?;
        self.valign.update(id, v).context("row update valign")?;
        for child in &mut self.children {
            child.handle_update(id, v)?;
        }
        Ok(())
    }

    fn needs_recompile(&self, id: ExprId) -> bool {
        id == self.children_ref.id
    }

    fn view(&self) -> IcedElement<'_> {
        let mut row = widget::Row::new();
        if let Some(sp) = self.spacing.t {
            row = row.spacing(sp as f32);
        }
        if let Some(p) = self.padding.t.as_ref() {
            row = row.padding(p.0);
        }
        if let Some(w) = self.width.t.as_ref() {
            row = row.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            row = row.height(h.0);
        }
        if let Some(a) = self.valign.t.as_ref() {
            row = row.align_y(a.0);
        }
        for child in &self.children {
            row = row.push(child.view());
        }
        row.into()
    }
}
