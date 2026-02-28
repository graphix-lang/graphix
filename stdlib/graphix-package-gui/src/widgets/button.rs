use super::{compile, GuiW, GuiWidget, IcedElement, Message};
use crate::types::{LengthV, PaddingV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct ButtonW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    padding: TRef<X, PaddingV>,
    on_press: Ref<X>,
    on_press_callable: Option<Callable<X>>,
    child_ref: Ref<X>,
    child: GuiW<X>,
}

impl<X: GXExt> ButtonW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, disabled), (_, height), (_, on_press), (_, padding), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("button flds")?;
        let (child_ref, disabled, height, on_press, padding, width) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(disabled),
            gx.compile_ref(height),
            gx.compile_ref(on_press),
            gx.compile_ref(padding),
            gx.compile_ref(width),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(super::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("button child")?,
        };
        let callable = match on_press.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("button on_press callable")?,
            ),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("button tref disabled")?,
            width: TRef::new(width).context("button tref width")?,
            height: TRef::new(height).context("button tref height")?,
            padding: TRef::new(padding).context("button tref padding")?,
            on_press,
            on_press_callable: callable,
            child_ref,
            child: compiled_child,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ButtonW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.disabled.update(id, v).context("button update disabled")?.is_some();
        changed |= self.width.update(id, v).context("button update width")?.is_some();
        changed |= self.height.update(id, v).context("button update height")?.is_some();
        changed |= self.padding.update(id, v).context("button update padding")?.is_some();
        if id == self.on_press.id {
            self.on_press.last = Some(v.clone());
            self.on_press_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("button on_press callable recompile")?,
            );
        }
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("button child recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        self.child.editor_action(id, action)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut btn = widget::Button::new(self.child.view());
        if !self.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_press_callable {
                btn = btn.on_press(Message::Call(
                    callable.id(),
                    ValArray::from_iter([Value::Null]),
                ));
            }
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
