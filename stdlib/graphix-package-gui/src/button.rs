use crate::{
    compile,
    types::{LengthV, PaddingV},
    GuiW, GuiWidget, IcedElement, Message,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct ButtonW<X: GXExt> {
    gx: GXHandle<X>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    padding: TRef<X, PaddingV>,
    on_press: TRef<X, Option<bool>>,
    child_ref: Ref<X>,
    child: GuiW<X>,
}

impl<X: GXExt> ButtonW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, height), (_, on_press), (_, padding), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("button flds")?;
        let (child_ref, height, on_press, padding, width) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(height),
            gx.compile_ref(on_press),
            gx.compile_ref(padding),
            gx.compile_ref(width),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(crate::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("button child")?,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            width: TRef::new(width).context("button tref width")?,
            height: TRef::new(height).context("button tref height")?,
            padding: TRef::new(padding).context("button tref padding")?,
            on_press: TRef::new(on_press).context("button tref on_press")?,
            child_ref,
            child: compiled_child,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ButtonW<X> {
    fn handle_update(&mut self, id: ExprId, v: &Value) -> Result<()> {
        self.width.update(id, v).context("button update width")?;
        self.height.update(id, v).context("button update height")?;
        self.padding.update(id, v).context("button update padding")?;
        self.on_press.update(id, v).context("button update on_press")?;
        self.child.handle_update(id, v)?;
        Ok(())
    }

    fn needs_recompile(&self, id: ExprId) -> bool {
        id == self.child_ref.id
    }

    fn view(&self) -> IcedElement<'_> {
        let mut btn = widget::Button::new(self.child.view());
        // If on_press has a bind target, wire up the press event
        if let Some(bid) = self.on_press.r.target_bid {
            btn = btn.on_press(Message::Set(bid, true.into()));
        }
        if let Some(w) = self.width.t.as_ref() {
            btn = btn.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            btn = btn.height(h.0);
        }
        if let Some(p) = self.padding.t.as_ref() {
            btn = btn.padding(p.0);
        }
        btn.into()
    }
}
