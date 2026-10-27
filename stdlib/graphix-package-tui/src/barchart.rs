use super::{
    DirectionV, LineV, StyleV, TuiW, TuiWidget, into_borrowed_line,
    into_borrowed_line_within, reconcile,
    validate::{Dim, Index},
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::{Direction, Rect},
    widgets::{Bar, BarChart, BarGroup},
};
use smallvec::SmallVec;

graphix_rt::props! {
    struct BarProps {
        label: Option<LineV>,
        style: Option<StyleV>,
        text_value: Option<ArcStr>,
        value: Index,
        value_style: Option<StyleV>,
    }
}

impl<X: GXExt> BarProps<X> {
    /// The bar, its label cut to `label_room` columns and its text value
    /// to `value_room` bytes: ratatui places what does not fit inside the
    /// bar by byte length.
    fn build(&self, label_room: u16, value_room: u16) -> Bar<'_> {
        let mut bar = Bar::default().value(self.value.t.map_or(0, |v| v.0 as u64));
        if let Some(Some(LineV(l))) = &self.label.t {
            bar = bar.label(into_borrowed_line_within(l, label_room));
        }
        if let Some(Some(s)) = &self.style.t {
            bar = bar.style(s.0);
        }
        if let Some(Some(s)) = &self.value_style.t {
            bar = bar.value_style(s.0);
        }
        if let Some(Some(tv)) = &self.text_value.t {
            bar = bar.text_value(&tv[..tv.floor_char_boundary(value_room as usize)]);
        }
        bar
    }
}

struct BarGroupW<X: GXExt> {
    label: Option<LineV>,
    bars: Vec<(Value, BarProps<X>)>,
}

impl<X: GXExt> BarGroupW<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            bars: Value,
            label: Option<LineV>,
        }
        let Fields { bars: bars_v, label } = v.cast_to().context("bargroup fields")?;
        let mut bars = Vec::new();
        reconcile(&mut bars, bars_v.cast_to()?, |b| async move {
            BarProps::compile(gx, &b).await
        })
        .await?;
        Ok(Self { label, bars })
    }
}

graphix_rt::props! {
    struct Props {
        bar_gap: Option<Dim>,
        bar_style: Option<StyleV>,
        bar_width: Option<Dim>,
        direction: Option<DirectionV>,
        group_gap: Option<Dim>,
        label_style: Option<StyleV>,
        max: Option<Index>,
        style: Option<StyleV>,
        value_style: Option<StyleV>,
    }
}

pub(super) struct BarChartW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    data_ref: Ref<X>,
    data: Vec<(Value, BarGroupW<X>)>,
}

impl<X: GXExt> BarChartW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("bar_chart")?;
        let data_ref = gx.compile_field(&v, "data").await?;
        let mut t = Self { gx, p, data_ref, data: vec![] };
        if let Some(v) = t.data_ref.last.take() {
            t.set_data(v).await?;
        }
        Ok(TuiW::new(t))
    }

    async fn set_data(&mut self, v: Value) -> Result<()> {
        reconcile(&mut self.data, v.cast_to()?, |g| BarGroupW::compile(&self.gx, g)).await
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for BarChartW<X> {
    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("bar_chart")?;
        if self.data_ref.id == id {
            return Ok(Some(Box::pin(self.set_data(v.clone()))));
        }
        for (_, g) in self.data.iter_mut() {
            for (_, b) in &mut g.bars {
                b.update(id, v).context("bar")?;
            }
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let dim = |r: &crate::TRef<X, Option<Dim>>, default| {
            r.t.flatten().map_or(default, |d| d.0)
        };
        let (bar_width, bar_gap, group_gap) =
            (dim(&p.bar_width, 1), dim(&p.bar_gap, 1), dim(&p.group_gap, 0));
        let mut chart = BarChart::default()
            .bar_width(bar_width)
            .bar_gap(bar_gap)
            .group_gap(group_gap);
        if let Some(Some(s)) = &p.bar_style.t {
            chart = chart.bar_style(s.0);
        }
        if let Some(Some(s)) = &p.value_style.t {
            chart = chart.value_style(s.0);
        }
        if let Some(Some(s)) = &p.label_style.t {
            chart = chart.label_style(s.0);
        }
        if let Some(Some(s)) = &p.style.t {
            chart = chart.style(s.0);
        }
        if let Some(Some(m)) = p.max.t {
            chart = chart.max(m.0 as u64);
        }
        let direction = p.direction.t.flatten().map_or(Direction::Vertical, |d| d.0);
        chart = chart.direction(direction);
        // only the bars the rect shows: ratatui sums a group's widths in u16
        let room = match direction {
            Direction::Vertical => rect.width,
            Direction::Horizontal => rect.height,
        } as u32;
        // a horizontal chart puts its bars a margin past the widest label
        let label_room = rect.width.saturating_sub(1);
        let value_room = match direction {
            Direction::Vertical => u16::MAX,
            Direction::Horizontal => {
                let label = (self.data.iter().flat_map(|(_, g)| &g.bars))
                    .filter_map(|(_, b)| {
                        b.label.t.as_ref()?.as_ref().map(|l| l.0.width())
                    })
                    .max()
                    .map_or(0, |w| w.min(label_room as usize) as u16);
                rect.width.saturating_sub(label).saturating_sub((label != 0) as u16)
            }
        };
        let mut used = 0u32;
        'groups: for (_, group) in &self.data {
            let mut bars: SmallVec<[Bar; 8]> = SmallVec::new();
            for (_, bar) in &group.bars {
                let gap = if !bars.is_empty() {
                    bar_gap
                } else if used > 0 {
                    group_gap
                } else {
                    0
                };
                let need = gap as u32 + bar_width as u32;
                if used + need > room {
                    if !bars.is_empty() {
                        chart = chart.data(Self::group(group, &bars));
                    }
                    break 'groups;
                }
                used += need;
                bars.push(bar.build(label_room, value_room));
            }
            chart = chart.data(Self::group(group, &bars));
        }
        frame.render_widget(chart, rect);
        Ok(())
    }
}

impl<X: GXExt> BarChartW<X> {
    fn group<'a>(group: &'a BarGroupW<X>, bars: &[Bar<'a>]) -> BarGroup<'a> {
        let g = BarGroup::default().bars(bars);
        match &group.label {
            Some(LineV(l)) => g.label(into_borrowed_line(l)),
            None => g,
        }
    }
}
