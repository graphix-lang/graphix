use super::{GuiW, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

// TODO: link hover underlines flicker between adjacent link spans;
// upstream bug in `iced_widget::text::rich::update` (no redraw when
// the hovered link index changes).

pub(crate) struct MarkdownW<X: GXExt> {
    gx: GXHandle<X>,
    content: TRef<X, String>,
    on_link: Ref<X>,
    on_link_callable: Option<Callable<X>>,
    spacing: TRef<X, Option<f64>>,
    text_size: TRef<X, Option<f64>>,
    width: TRef<X, LengthV>,
    items: Vec<widget::markdown::Item>,
}

impl<X: GXExt> MarkdownW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            content: u64,
            on_link: u64,
            spacing: u64,
            text_size: u64,
            width: u64,
        }
        let Fields { content, on_link, spacing, text_size, width } =
            source.cast_to().context("markdown flds")?;
        let (content_ref, on_link, spacing, text_size, width) = try_join! {
            gx.compile_ref(content),
            gx.compile_ref(on_link),
            gx.compile_ref(spacing),
            gx.compile_ref(text_size),
            gx.compile_ref(width),
        }?;
        let callable = compile_callable!(gx, on_link, "markdown on_link");
        let content = TRef::new(content_ref).context("markdown tref content")?;
        let items = match content.t.as_deref() {
            Some(s) => widget::markdown::parse(s).collect(),
            None => vec![],
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            content,
            on_link,
            on_link_callable: callable,
            spacing: TRef::new(spacing).context("markdown tref spacing")?,
            text_size: TRef::new(text_size).context("markdown tref text_size")?,
            width: TRef::new(width).context("markdown tref width")?,
            items,
        }))
    }
}

impl<X: GXExt> super::GuiWidget<X> for MarkdownW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if let Some(_) = self.content.update(id, v).context("markdown update content")? {
            self.items = match self.content.t.as_deref() {
                Some(s) => widget::markdown::parse(s).collect(),
                None => vec![],
            };
            changed = true;
        }
        changed |=
            self.spacing.update(id, v).context("markdown update spacing")?.is_some();
        changed |=
            self.text_size.update(id, v).context("markdown update text_size")?.is_some();
        changed |= self.width.update(id, v).context("markdown update width")?.is_some();
        update_callable!(
            self,
            rt,
            id,
            v,
            on_link,
            on_link_callable,
            "markdown on_link recompile"
        );
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        // CR claude for eric: [bug] Any text size reaches cosmic-text unchecked. A size
        // of 0, or a positive one that becomes 0 as f32 (1e-50), trips its "line height
        // cannot be 0" / "font size cannot be 0" asserts on the main thread, and the
        // program exits; a negative size hangs layout forever in
        // Buffer::shape_until_scroll. A zoom slider whose min is 0 is enough. The same
        // gap exists at text.rs:83, canvas.rs:307 (the panic comes at present),
        // text_input.rs:172 and text_editor.rs:156. Check the size once where it is
        // read (positive and finite after the f32 conversion, for example one text-size
        // type these widgets share) and refuse anything else with a logged error.
        // probe: design/review-2026-10-05/repro/gui-widgets-a-02.rs (headless cargo
        // test; md_drop renders at 16 and then dies when the program writes 0).
        // (gui-widgets-a-02)
        let text_size = self.text_size.t.flatten().unwrap_or(16.0) as f32;
        // CR claude for eric: [bug] view() never reads self.spacing, so
        // Settings::spacing stays at with_text_size's text_size * 0.875 and the
        // documented #spacing (book/src/ui/gui/markdown.md:20) does nothing. The Style
        // comes from iced_core::Theme::Dark, so links are drawn in Dark's primary
        // (#5865F2) under every window theme, while the paragraph text follows the
        // window's theme. view() has no theme to build the Style from. probe:
        // design/review-2026-10-05/repro/gui-widgets-a-07.rs (copy into
        // stdlib/graphix-package-gui/tests/ and run cargo test -p graphix-package-gui
        // --test review_gui_widgets_a_07). The default, #spacing 40 and #spacing 2 all
        // lay out 90.4 px tall. Under CustomPalette(primary red) and Dracula the link
        // has 0 px in the theme's primary and 343/418 px in #5865F2. (gui-widgets-a-07)
        let settings =
            widget::markdown::Settings::with_text_size(text_size, iced_core::Theme::Dark);
        let on_link_id = self.on_link_callable.as_ref().map(|c| c.id());
        let md: iced_core::Element<'_, widget::markdown::Uri, _, _> =
            widget::markdown::view(&self.items, settings);
        let element = md.map(move |uri| match on_link_id {
            Some(id) => {
                Message::Call(id, ValArray::from_iter([Value::String(uri.into())]))
            }
            None => Message::Nop,
        });
        let mut container = iced_widget::Container::new(element);
        if let Some(w) = self.width.t.as_ref() {
            container = container.width(w.0);
        }
        container.into()
    }
}
