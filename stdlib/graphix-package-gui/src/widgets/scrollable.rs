use super::{compile, GuiW, GuiWidget, IcedElement, Message};
use crate::types::{LengthV, ScrollDirectionV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct ScrollableW<X: GXExt> {
    gx: GXHandle<X>,
    child_ref: Ref<X>,
    child: GuiW<X>,
    direction: TRef<X, ScrollDirectionV>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    on_scroll: Ref<X>,
    on_scroll_callable: Option<Callable<X>>,
}

impl<X: GXExt> ScrollableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, direction), (_, height), (_, on_scroll), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("scrollable flds")?;
        let (child_ref, direction, height, on_scroll, width) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(direction),
            gx.compile_ref(height),
            gx.compile_ref(on_scroll),
            gx.compile_ref(width),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(super::EmptyW),
            Some(v) => {
                compile(gx.clone(), v.clone()).await.context("scrollable child")?
            }
        };
        let on_scroll_callable = match on_scroll.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("scrollable on_scroll callable")?,
            ),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            child_ref,
            child: compiled_child,
            direction: TRef::new(direction).context("scrollable tref direction")?,
            width: TRef::new(width).context("scrollable tref width")?,
            height: TRef::new(height).context("scrollable tref height")?,
            on_scroll,
            on_scroll_callable,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ScrollableW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self
            .direction
            .update(id, v)
            .context("scrollable update direction")?
            .is_some();
        changed |= self.width.update(id, v).context("scrollable update width")?.is_some();
        changed |=
            self.height.update(id, v).context("scrollable update height")?.is_some();
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("scrollable child recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        if id == self.on_scroll.id {
            self.on_scroll.last = Some(v.clone());
            self.on_scroll_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("scrollable on_scroll callable recompile")?,
            );
        }
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
        let mut sc = widget::Scrollable::new(self.child.view());
        if let Some(dir) = self.direction.t.as_ref() {
            sc = sc.direction(dir.0);
        }
        if let Some(w) = self.width.t.as_ref() {
            sc = sc.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            sc = sc.height(h.0);
        }
        if let Some(c) = &self.on_scroll_callable {
            let id = c.id();
            sc = sc.on_scroll(move |viewport| {
                let off = viewport.absolute_offset();
                let offset_val: Value = [
                    (arcstr::literal!("x"), Value::F64(off.x as f64)),
                    (arcstr::literal!("y"), Value::F64(off.y as f64)),
                ]
                .into();
                Message::Call(id, ValArray::from_iter([offset_val]))
            });
        }
        sc.into()
    }
}
