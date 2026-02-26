use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::{LengthV, PaddingV, StringVec};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct PickListW<X: GXExt> {
    options: TRef<X, StringVec>,
    selected: TRef<X, Option<String>>,
    target_bid: Option<BindId>,
    placeholder: TRef<X, String>,
    width: TRef<X, LengthV>,
    padding: TRef<X, PaddingV>,
}

impl<X: GXExt> PickListW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, options), (_, padding), (_, placeholder), (_, selected), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("pick_list flds")?;
        let (options, padding, placeholder, selected, width) = try_join! {
            gx.compile_ref(options),
            gx.compile_ref(padding),
            gx.compile_ref(placeholder),
            gx.compile_ref(selected),
            gx.compile_ref(width),
        }?;
        let target_bid = selected.target_bid;
        Ok(Box::new(Self {
            options: TRef::new(options).context("pick_list tref options")?,
            selected: TRef::new(selected).context("pick_list tref selected")?,
            target_bid,
            placeholder: TRef::new(placeholder).context("pick_list tref placeholder")?,
            width: TRef::new(width).context("pick_list tref width")?,
            padding: TRef::new(padding).context("pick_list tref padding")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for PickListW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.options.update(id, v).context("pick_list update options")?.is_some();
        changed |= self.selected.update(id, v).context("pick_list update selected")?.is_some();
        changed |= self.placeholder.update(id, v).context("pick_list update placeholder")?.is_some();
        changed |= self.width.update(id, v).context("pick_list update width")?.is_some();
        changed |= self.padding.update(id, v).context("pick_list update padding")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let options = self.options.t.as_ref().map(|v| v.0.as_slice()).unwrap_or(&[]);
        let selected = self.selected.t.as_ref().and_then(|o| o.clone());
        let target_bid = self.target_bid;
        let mut pl = widget::PickList::new(
            options,
            selected,
            move |s: String| match target_bid {
                Some(bid) => Message::Set(bid, Value::String(s.into())),
                None => Message::Nop,
            },
        );
        let placeholder = self.placeholder.t.as_deref().unwrap_or("");
        if !placeholder.is_empty() {
            pl = pl.placeholder(placeholder);
        }
        if let Some(w) = self.width.t.as_ref() {
            pl = pl.width(w.0);
        }
        if let Some(p) = self.padding.t.as_ref() {
            pl = pl.padding(p.0);
        }
        pl.into()
    }
}
