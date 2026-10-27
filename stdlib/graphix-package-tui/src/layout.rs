use super::{
    DirectionV, FlexV, SizeReport, TuiW, TuiWidget, compile, reconcile, update_children,
    validate::{Dim, Index, Offset, Percent},
};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::{BindId, expr::ExprId};
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use poolshark::local::LPooled;
use ratatui::{
    Frame,
    layout::{Constraint, Layout, Rect, Spacing},
};

#[derive(Clone, Copy)]
pub(super) struct ConstraintV(pub Constraint);

impl FromValue for ConstraintV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        enum Repr {
            Min(Offset),
            Max(Offset),
            Length(Offset),
            Percentage(Percent),
            Ratio(Index, Index),
            Fill(Offset),
        }
        Ok(Self(match v.cast_to()? {
            Repr::Min(p) => Constraint::Min(p.0),
            Repr::Max(p) => Constraint::Max(p.0),
            Repr::Length(p) => Constraint::Length(p.0),
            Repr::Percentage(p) => Constraint::Percentage(p.0),
            Repr::Ratio(n, d) => {
                let n = n.0.min(u32::MAX as usize) as u32;
                let d = d.0.clamp(1, u32::MAX as usize) as u32;
                Constraint::Ratio(n.min(d), d)
            }
            Repr::Fill(p) => Constraint::Fill(p.0),
        }))
    }
}

#[derive(Clone)]
struct SpacingV(Spacing);

impl FromValue for SpacingV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        enum Repr {
            Space(Dim),
            Overlap(Dim),
        }
        Ok(Self(match v.cast_to()? {
            Repr::Space(p) => Spacing::Space(p.0),
            Repr::Overlap(p) => Spacing::Overlap(p.0),
        }))
    }
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct ChildV {
    child: Value,
    constraint: ConstraintV,
    size: Option<BindId>,
}

/// A child widget, kept while its `Tui` value is: its constraint and
/// size output are set in place.
struct Slot<X: GXExt> {
    size: SizeReport<X>,
    constraint: Constraint,
    child: TuiW,
}

graphix_rt::props! {
    struct Props {
        direction: Option<DirectionV>,
        flex: Option<FlexV>,
        focused: Option<Index>,
        horizontal_margin: Option<Dim>,
        margin: Option<Dim>,
        spacing: Option<SpacingV>,
        vertical_margin: Option<Dim>,
    }
}

pub(super) struct LayoutW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    /// Keyed on each child's `Tui` value.
    children: Vec<(Value, Slot<X>)>,
    children_ref: Ref<X>,
}

impl<X: GXExt> LayoutW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("layout")?;
        let children_ref = gx.compile_field(&v, "children").await?;
        let mut t = Self { gx, p, children: vec![], children_ref };
        if let Some(v) = t.children_ref.last.take() {
            t.set_children(v).await?;
        }
        Ok(TuiW::new(t))
    }

    async fn set_children(&mut self, v: Value) -> Result<()> {
        let children: LPooled<Vec<ChildV>> = v.cast_to()?;
        let gx = &self.gx;
        let tuis = children.iter().map(|c| c.child.clone()).collect();
        reconcile(&mut self.children, tuis, |v| async move {
            let child = compile(gx.clone(), v).await.context("compiling child")?;
            Ok(Slot { size: SizeReport::none(), constraint: Constraint::Min(0), child })
        })
        .await?;
        // every constraint is set before a size output can fail
        for ((_, slot), c) in self.children.iter_mut().zip(children.iter()) {
            slot.constraint = c.constraint.0;
        }
        for ((_, slot), c) in self.children.iter_mut().zip(children.iter()) {
            slot.size.retarget(gx, c.size).await?;
        }
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for LayoutW<X> {
    async fn handle_event(&mut self, v: Value) -> Result<()> {
        let focused = self.p.focused.t.flatten().map_or(0, |i| i.0);
        let idx = focused.min(self.children.len().saturating_sub(1));
        if let Some((_, c)) = self.children.get_mut(idx) {
            c.child.handle_event(v).await?;
        }
        Ok(())
    }

    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("layout")?;
        if self.children_ref.id == id {
            return Ok(Some(Box::pin(self.set_children(v.clone()))));
        }
        update_children(self.children.iter_mut().map(|(_, c)| &mut c.child), id, v)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let mut layout = Layout::default();
        if let Some(Some(d)) = p.direction.t {
            layout = layout.direction(d.0);
        }
        if let Some(Some(f)) = p.flex.t {
            layout = layout.flex(f.0);
        }
        // margin sets both axes, so the per-axis margins go after it
        if let Some(Some(m)) = p.margin.t {
            layout = layout.margin(m.0);
        }
        if let Some(Some(m)) = p.horizontal_margin.t {
            layout = layout.horizontal_margin(m.0);
        }
        if let Some(Some(s)) = &p.spacing.t {
            layout = layout.spacing(s.0.clone());
        }
        if let Some(Some(m)) = p.vertical_margin.t {
            layout = layout.vertical_margin(m.0);
        }
        layout = layout.constraints(self.children.iter().map(|(_, c)| c.constraint));
        let areas = layout.split(rect);
        for (rect, (_, slot)) in areas.iter().zip(self.children.iter_mut()) {
            slot.size.report(*rect)?;
            slot.child.draw(frame, *rect)?
        }
        Ok(())
    }
}
