use super::{
    AlignmentV, LinesV, ScrollV, StyleV, TRef, TuiW, TuiWidget, into_borrowed_lines,
    validate,
};
use anyhow::{Context, Result};
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Paragraph, Wrap},
};
use tokio::try_join;

pub(super) struct ParagraphW<X: GXExt> {
    alignment: TRef<X, Option<AlignmentV>>,
    lines: TRef<X, LinesV>,
    scroll: TRef<X, ScrollV>,
    style: TRef<X, StyleV>,
    trim: TRef<X, bool>,
    last_warned_scroll_x: Option<i64>,
    last_warned_scroll_y: Option<i64>,
}

impl<X: GXExt> ParagraphW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, source: Value) -> Result<TuiW> {
        #[derive(FromValue)]
        struct Fields {
            alignment: u64,
            lines: u64,
            scroll: u64,
            style: u64,
            trim: u64,
        }
        let Fields { alignment, lines, scroll, style, trim } =
            source.cast_to().context("paragraph flds")?;
        let (alignment, lines, scroll, style, trim) = try_join! {
            gx.compile_ref(alignment),
            gx.compile_ref(lines),
            gx.compile_ref(scroll),
            gx.compile_ref(style),
            gx.compile_ref(trim)
        }?;
        let alignment: TRef<X, Option<AlignmentV>> =
            TRef::new(alignment).context("paragraph tref alignment")?;
        let lines: TRef<X, LinesV> = TRef::new(lines).context("paragraph tref lines")?;
        let scroll: TRef<X, ScrollV> =
            TRef::new(scroll).context("paragraph tref scroll")?;
        let style: TRef<X, StyleV> = TRef::new(style).context("paragraph tref style")?;
        let trim: TRef<X, bool> = TRef::new(trim).context("paragraph tref trim")?;
        Ok(Box::new(Self {
            alignment,
            lines,
            scroll,
            style,
            trim,
            last_warned_scroll_x: None,
            last_warned_scroll_y: None,
        }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ParagraphW<X> {
    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let lines = self.lines.t.as_ref().map(|l| &l.0[..]).unwrap_or(&[]);
        let mut p = Paragraph::new(into_borrowed_lines(lines));
        if let Some(Some(a)) = self.alignment.t {
            p = p.alignment(a.0);
        }
        if let Some(s) = self.style.t {
            p = p.style(s.0);
        }
        // CR claude for claude: [bug] Wrapping is set whenever `trim` has a value, and
        // `#trim: &bool` defaults to `&true`, so every paragraph wraps. ratatui applies
        // `scroll.x` only on its non-wrapping path, so the `scroll.x` that
        // paragraph.gxi documents as "in chars" does nothing with either trim value,
        // and long lines can never be cut off instead of wrapped. Only a trim reference
        // that never has a value (`#trim: &never<bool>()`) reaches the unwrapped path.
        // Wrapping needs its own option (e.g. `trim: &[bool, null]` with null meaning
        // no wrap, or a separate `#wrap`), and the docs should say that x scrolls only
        // unwrapped text. probe: design/review-2026-10-05/repro/tui-widgets-10.gx
        // (tui-widgets-10)
        if let Some(trim) = self.trim.t {
            p = p.wrap(Wrap { trim });
        }
        if let Some(s) = self.scroll.t {
            // CR claude for claude: [bug] scroll.y and scroll.x count content lines and
            // chars, not terminal cells. clamp_u16 caps them at VISUAL_DIMENSION_CAP
            // (1024), so a paragraph longer than about 1024 lines cannot be scrolled
            // past line 1024: `#scroll: &{x: 0, y: 1500}` over 2000 lines shows
            // L1024..L1047 and logs a clamp warning. ratatui only counts with these
            // offsets: render_paragraph loops over or skips scroll.y lines, and
            // LineTruncator widens scroll.x to usize. So every u16 is safe. Clamp them
            // to [0, u16::MAX], and drop "scroll offsets" from the VISUAL_DIMENSION_CAP
            // doc in validate.rs. The scrollbar's position and the list's scroll have
            // no such cap. probe: design/review-2026-10-05/repro/tui-widgets-11.gx (run
            // in a terminal). (tui-widgets-11)
            let y = validate::clamp_u16(
                "paragraph",
                "scroll.y",
                &mut self.last_warned_scroll_y,
                s.y,
            );
            let x = validate::clamp_u16(
                "paragraph",
                "scroll.x",
                &mut self.last_warned_scroll_x,
                s.x,
            );
            p = p.scroll((y, x))
        }
        frame.render_widget(p, rect);
        Ok(())
    }

    async fn handle_event(&mut self, _: Event, _: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            alignment,
            lines,
            scroll,
            style,
            trim,
            last_warned_scroll_x: _,
            last_warned_scroll_y: _,
        } = self;
        alignment.update(id, &v).context("paragraph update alignment")?;
        lines.update(id, &v).context("paragraph update lines")?;
        scroll.update(id, &v).context("paragraph update scroll")?;
        style.update(id, &v).context("paragraph update style")?;
        trim.update(id, &v).context("paragraph update trim")?;
        Ok(())
    }
}
