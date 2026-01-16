use super::{
    compile, into_borrowed_line, AlignmentV, EmptyW, LineV, SizeV, StyleV,
    TitlePositionV, TuiW, TuiWidget,
};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use netidx::publisher::{FromValue, Value};
use ratatui::{
    layout::Rect,
    symbols::merge::MergeStrategy,
    widgets::{Block, BorderType, Borders, Padding},
    Frame,
};
use smallvec::SmallVec;
use tokio::try_join;

#[derive(Clone, Copy)]
struct MergeStrategyV(MergeStrategy);

impl FromValue for MergeStrategyV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => match &*s {
                "Replace" => Ok(Self(MergeStrategy::Replace)),
                "Exact" => Ok(Self(MergeStrategy::Exact)),
                "Fuzzy" => Ok(Self(MergeStrategy::Fuzzy)),
                s => bail!("invalid merge strategy {s}"),
            },
            v => bail!("invalid merge strategy {v}"),
        }
    }
}

#[derive(Clone, Copy)]
struct BordersV(Borders);

impl FromValue for BordersV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => match &*s {
                "All" => Ok(Self(Borders::all())),
                "None" => Ok(Self(Borders::empty())),
                s => bail!("invalid borders {s}"),
            },
            v => {
                let mut res = Borders::empty();
                for b in v.cast_to::<SmallVec<[ArcStr; 4]>>()? {
                    match &*b {
                        "Top" => res.insert(Borders::TOP),
                        "Right" => res.insert(Borders::RIGHT),
                        "Bottom" => res.insert(Borders::BOTTOM),
                        "Left" => res.insert(Borders::LEFT),
                        s => bail!("invalid border {s}"),
                    }
                }
                Ok(Self(res))
            }
        }
    }
}

#[derive(Clone, Copy)]
struct BorderTypeV(BorderType);

impl FromValue for BorderTypeV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Plain" => Ok(Self(BorderType::Plain)),
            "Rounded" => Ok(Self(BorderType::Rounded)),
            "Double" => Ok(Self(BorderType::Double)),
            "Thick" => Ok(Self(BorderType::Thick)),
            "QuadrantInside" => Ok(Self(BorderType::QuadrantInside)),
            "QuadrantOutside" => Ok(Self(BorderType::QuadrantOutside)),
            s => bail!("invalid border type {s}"),
        }
    }
}

#[derive(Clone, Copy)]
struct PaddingV(Padding);

impl FromValue for PaddingV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, bottom), (_, left), (_, right), (_, top)] =
            v.cast_to::<[(ArcStr, u16); 4]>()?;
        Ok(Self(Padding { bottom, left, right, top }))
    }
}

pub(super) struct BlockW<X: GXExt> {
    gx: GXHandle<X>,
    border: TRef<X, Option<BordersV>>,
    border_style: TRef<X, Option<StyleV>>,
    border_type: TRef<X, Option<BorderTypeV>>,
    merge_borders: TRef<X, Option<MergeStrategyV>>,
    child_ref: Ref<X>,
    child: TuiW,
    padding: TRef<X, Option<PaddingV>>,
    style: TRef<X, Option<StyleV>>,
    title: TRef<X, Option<LineV>>,
    title_alignment: TRef<X, Option<AlignmentV>>,
    title_bottom: TRef<X, Option<LineV>>,
    title_position: TRef<X, Option<TitlePositionV>>,
    title_style: TRef<X, Option<StyleV>>,
    title_top: TRef<X, Option<LineV>>,
    size_ref: Ref<X>,
    last_size: SizeV,
}

impl<X: GXExt> BlockW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, border), (_, border_style), (_, border_type), (_, child), (_, merge_borders), (_, padding), (_, size), (_, style), (_, title), (_, title_alignment), (_, title_bottom), (_, title_position), (_, title_style), (_, title_top)] =
            v.cast_to::<[(ArcStr, u64); 14]>().context("block flds")?;
        let (
            border,
            border_style,
            border_type,
            mut child_ref,
            merge_borders,
            padding,
            size_ref,
            style,
            title,
            title_alignment,
            title_bottom,
            title_position,
            title_style,
            title_top,
        ) = try_join! {
            gx.compile_ref(border),
            gx.compile_ref(border_style),
            gx.compile_ref(border_type),
            gx.compile_ref(child),
            gx.compile_ref(merge_borders),
            gx.compile_ref(padding),
            gx.compile_ref(size),
            gx.compile_ref(style),
            gx.compile_ref(title),
            gx.compile_ref(title_alignment),
            gx.compile_ref(title_bottom),
            gx.compile_ref(title_position),
            gx.compile_ref(title_style),
            gx.compile_ref(title_top),
        }?;
        let border =
            TRef::<X, Option<BordersV>>::new(border).context("block tref border")?;
        let border_style = TRef::<X, Option<StyleV>>::new(border_style)
            .context("block tref border_style")?;
        let border_type = TRef::<X, Option<BorderTypeV>>::new(border_type)
            .context("block tref border_type")?;
        let merge_borders = TRef::<X, Option<MergeStrategyV>>::new(merge_borders)
            .context("block tref merge borders")?;
        let padding =
            TRef::<X, Option<PaddingV>>::new(padding).context("block tref padding")?;
        let style = TRef::<X, Option<StyleV>>::new(style).context("block tref style")?;
        let title = TRef::<X, Option<LineV>>::new(title).context("block tref title")?;
        let title_alignment = TRef::<X, Option<AlignmentV>>::new(title_alignment)
            .context("block tref title_alignment")?;
        let title_bottom = TRef::<X, Option<LineV>>::new(title_bottom)
            .context("block tref title_bottom")?;
        let title_position = TRef::<X, Option<TitlePositionV>>::new(title_position)
            .context("block tref title_position")?;
        let title_style = TRef::<X, Option<StyleV>>::new(title_style)
            .context("block tref title_style")?;
        let title_top =
            TRef::<X, Option<LineV>>::new(title_top).context("block tref title_top")?;
        let child = match child_ref.last.take() {
            None => Box::new(EmptyW),
            Some(v) => compile(gx.clone(), v).await.context("block compile child")?,
        };
        let t = Self {
            gx,
            border,
            border_style,
            border_type,
            merge_borders,
            padding,
            size_ref,
            style,
            title,
            title_alignment,
            title_bottom,
            title_position,
            title_style,
            title_top,
            child_ref,
            child,
            last_size: SizeV::default(),
        };
        Ok(Box::new(t))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for BlockW<X> {
    async fn handle_event(&mut self, e: Event, v: Value) -> Result<()> {
        self.child.handle_event(e, v).await
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            gx,
            border,
            border_style,
            border_type,
            merge_borders,
            child_ref,
            child,
            padding,
            size_ref: _,
            last_size: _,
            style,
            title,
            title_alignment,
            title_bottom,
            title_position,
            title_style,
            title_top,
        } = self;
        border.update(id, &v).context("block border update")?;
        border_style.update(id, &v).context("block border_style update")?;
        border_type.update(id, &v).context("block border_type update")?;
        merge_borders.update(id, &v).context("block merge_borders update")?;
        padding.update(id, &v).context("block padding update")?;
        style.update(id, &v).context("block style update")?;
        title.update(id, &v).context("block title update")?;
        title_alignment.update(id, &v).context("block title_alignment update")?;
        title_bottom.update(id, &v).context("block title_bottom update")?;
        title_position.update(id, &v).context("block title_position update")?;
        title_style.update(id, &v).context("block title_style update")?;
        title_top.update(id, &v).context("block title_top update")?;
        if id == child_ref.id {
            *child =
                compile(gx.clone(), v.clone()).await.context("block child compile")?;
        }
        child.handle_update(id, v).await?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let Self {
            gx: _,
            border,
            border_style,
            border_type,
            merge_borders,
            child_ref: _,
            child,
            padding,
            size_ref,
            last_size,
            style,
            title,
            title_alignment,
            title_bottom,
            title_position,
            title_style,
            title_top,
        } = self;
        let mut block = Block::new();
        if let Some(Some(b)) = border.t {
            block = block.borders(b.0);
        }
        if let Some(Some(s)) = border_style.t {
            block = block.border_style(s.0);
        }
        if let Some(Some(t)) = border_type.t {
            block = block.border_type(t.0);
        }
        if let Some(Some(t)) = merge_borders.t {
            block = block.merge_borders(t.0);
        }
        if let Some(Some(p)) = padding.t {
            block = block.padding(p.0);
        }
        if let Some(Some(s)) = style.t {
            block = block.style(s.0);
        }
        if let Some(Some(LineV(l))) = &title.t {
            block = block.title(into_borrowed_line(l));
        }
        if let Some(Some(a)) = title_alignment.t {
            block = block.title_alignment(a.0);
        }
        if let Some(Some(LineV(l))) = &title_bottom.t {
            block = block.title_bottom(into_borrowed_line(l));
        }
        if let Some(Some(p)) = title_position.t {
            block = block.title_position(p.0);
        }
        if let Some(Some(s)) = title_style.t {
            block = block.title_style(s.0);
        }
        if let Some(Some(LineV(l))) = &title_top.t {
            block = block.title_top(into_borrowed_line(l));
        }
        let child_rect = block.inner(rect);
        let size: SizeV = SizeV::from(child_rect);
        if *last_size != size {
            *last_size = size;
            size_ref.set_deref(size)?;
        }
        frame.render_widget(block, rect);
        child.draw(frame, child_rect)?;
        Ok(())
    }
}
