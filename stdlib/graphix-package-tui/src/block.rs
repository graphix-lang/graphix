use super::{
    AlignmentV, ChildW, LineV, SizeReport, StyleV, TitlePositionV, TuiW, TuiWidget,
    into_borrowed_line, validate::Dim,
};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    symbols::merge::MergeStrategy,
    widgets::{Block, BorderType, Borders, Padding},
};
use smallvec::SmallVec;

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
        #[derive(FromValue)]
        struct Fields {
            bottom: Dim,
            left: Dim,
            right: Dim,
            top: Dim,
        }
        let Fields { bottom, left, right, top } = v.cast_to()?;
        Ok(Self(Padding { bottom: bottom.0, left: left.0, right: right.0, top: top.0 }))
    }
}

graphix_rt::props! {
    struct Props {
        border: Option<BordersV>,
        border_style: Option<StyleV>,
        border_type: Option<BorderTypeV>,
        merge_borders: Option<MergeStrategyV>,
        padding: Option<PaddingV>,
        style: Option<StyleV>,
        title: Option<LineV>,
        title_alignment: Option<AlignmentV>,
        title_bottom: Option<LineV>,
        title_position: Option<TitlePositionV>,
        title_style: Option<StyleV>,
        title_top: Option<LineV>,
    }
}

pub(super) struct BlockW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    child: ChildW<X>,
    size: SizeReport<X>,
}

impl<X: GXExt> BlockW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("block")?;
        let child = ChildW::compile(&gx, &v, "child").await.context("block")?;
        let size = SizeReport::compile(&gx, &v).await?;
        Ok(Box::new(Self { gx, p, child, size }))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for BlockW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        self.child.w.handle_event(v).await
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("block")?;
        self.child.update(&self.gx, id, v).await
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let mut block = Block::new();
        if let Some(Some(b)) = p.border.t {
            block = block.borders(b.0);
        }
        if let Some(Some(s)) = p.border_style.t {
            block = block.border_style(s.0);
        }
        if let Some(Some(t)) = p.border_type.t {
            block = block.border_type(t.0);
        }
        if let Some(Some(t)) = p.merge_borders.t {
            block = block.merge_borders(t.0);
        }
        if let Some(Some(pad)) = p.padding.t {
            block = block.padding(pad.0);
        }
        if let Some(Some(s)) = p.style.t {
            block = block.style(s.0);
        }
        if let Some(Some(LineV(l))) = &p.title.t {
            block = block.title(into_borrowed_line(l));
        }
        if let Some(Some(a)) = p.title_alignment.t {
            block = block.title_alignment(a.0);
        }
        if let Some(Some(LineV(l))) = &p.title_bottom.t {
            block = block.title_bottom(into_borrowed_line(l));
        }
        if let Some(Some(pos)) = p.title_position.t {
            block = block.title_position(pos.0);
        }
        if let Some(Some(s)) = p.title_style.t {
            block = block.title_style(s.0);
        }
        if let Some(Some(LineV(l))) = &p.title_top.t {
            block = block.title_top(into_borrowed_line(l));
        }
        let child_rect = block.inner(rect);
        self.size.report(child_rect)?;
        frame.render_widget(block, rect);
        self.child.w.draw(frame, child_rect)
    }
}
