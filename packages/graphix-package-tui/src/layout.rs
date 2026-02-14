use super::{compile, DirectionV, FlexV, SizeV, TuiW, TuiWidget};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use futures::future;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use netidx::publisher::{FromValue, Value};
use ratatui::{
    layout::{Constraint, Layout, Rect, Spacing},
    Frame,
};
use smallvec::SmallVec;
use tokio::try_join;

#[derive(Clone, Copy)]
pub(super) struct ConstraintV(pub Constraint);

impl FromValue for ConstraintV {
    fn from_value(v: Value) -> Result<Self> {
        let t = match &v.cast_to::<SmallVec<[Value; 3]>>()?[..] {
            [Value::String(s), Value::I64(p)] => match &**s {
                "Min" => Constraint::Min(*p as u16),
                "Max" => Constraint::Max(*p as u16),
                "Percentage" => Constraint::Percentage(*p as u16),
                "Fill" => Constraint::Fill(*p as u16),
                s => bail!("invalid constraint tag {s}"),
            },
            [Value::String(s), Value::I64(n), Value::I64(d)] if &**s == "Ratio" => {
                Constraint::Ratio(*n as u32, *d as u32)
            }
            v => bail!("invalid constraint {v:?}"),
        };
        Ok(Self(t))
    }
}

#[derive(Clone)]
struct SpacingV(Spacing);

impl FromValue for SpacingV {
    fn from_value(v: Value) -> Result<Self> {
        let t = match v.cast_to::<(ArcStr, u16)>()? {
            (s, p) if &*s == "Space" => Spacing::Space(p),
            (s, p) if &*s == "Overlap" => Spacing::Overlap(p),
            (s, _) => bail!("invalid spacing tag {s}"),
        };
        Ok(Self(t))
    }
}

struct ChildW<X: GXExt> {
    size_ref: Ref<X>,
    last_size: SizeV,
    constraint: Constraint,
    child: TuiW,
}

impl<X: GXExt> ChildW<X> {
    async fn compile(gx: GXHandle<X>, v: Value) -> Result<Self> {
        let ((_, child), (_, constraint), (_, size)) =
            v.cast_to::<((ArcStr, Value), (ArcStr, ConstraintV), (ArcStr, u64))>()?;
        let child = compile(gx.clone(), child).await.context("compiling child")?;
        let constraint = constraint.0;
        let size_ref = gx.compile_ref(size).await.context("compiling size ref")?;
        Ok(Self { size_ref, last_size: SizeV::default(), constraint, child })
    }
}

pub(super) struct LayoutW<X: GXExt> {
    gx: GXHandle<X>,
    children: Vec<ChildW<X>>,
    children_ref: Ref<X>,
    direction: TRef<X, Option<DirectionV>>,
    flex: TRef<X, Option<FlexV>>,
    horizontal_margin: TRef<X, Option<u16>>,
    margin: TRef<X, Option<u16>>,
    spacing: TRef<X, Option<SpacingV>>,
    vertical_margin: TRef<X, Option<u16>>,
    focused: TRef<X, Option<u32>>,
}

impl<X: GXExt> LayoutW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, children), (_, direction), (_, flex), (_, focused), (_, horizontal_margin), (_, margin), (_, spacing), (_, vertical_margin)] =
            v.cast_to::<[(ArcStr, u64); 8]>().context("layout fields")?;
        let (
            children_ref,
            direction,
            flex,
            focused,
            horizontal_margin,
            margin,
            spacing,
            vertical_margin,
        ) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(direction),
            gx.compile_ref(flex),
            gx.compile_ref(focused),
            gx.compile_ref(horizontal_margin),
            gx.compile_ref(margin),
            gx.compile_ref(spacing),
            gx.compile_ref(vertical_margin)
        }?;
        let direction = TRef::<X, Option<DirectionV>>::new(direction)
            .context("layout tref direction")?;
        let flex = TRef::<X, Option<FlexV>>::new(flex).context("layout tref flex")?;
        let horizontal_margin = TRef::<X, Option<u16>>::new(horizontal_margin)
            .context("layout tref horizontal_margin")?;
        let margin = TRef::<X, Option<u16>>::new(margin).context("layout tref margin")?;
        let spacing =
            TRef::<X, Option<SpacingV>>::new(spacing).context("layout tref spacing")?;
        let vertical_margin = TRef::<X, Option<u16>>::new(vertical_margin)
            .context("layout tref vertical_margin")?;
        let focused =
            TRef::<X, Option<u32>>::new(focused).context("layout tref focused")?;
        let mut t = Self {
            gx,
            children: vec![],
            children_ref,
            direction,
            flex,
            horizontal_margin,
            margin,
            spacing,
            vertical_margin,
            focused,
        };
        if let Some(v) = t.children_ref.last.take() {
            t.set_children(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_children(&mut self, v: Value) -> Result<()> {
        self.children =
            future::join_all(v.cast_to::<SmallVec<[Value; 8]>>()?.into_iter().map(|v| {
                let gx = self.gx.clone();
                async move {
                    let child = ChildW::compile(gx, v).await?;
                    Ok(child)
                }
            }))
            .await
            .into_iter()
            .collect::<Result<Vec<_>>>()?;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for LayoutW<X> {
    async fn handle_event(&mut self, e: Event, v: Value) -> Result<()> {
        let idx = self.focused.t.and_then(|o| o.map(|i| i as usize)).unwrap_or(0);
        if let Some(c) = self.children.get_mut(idx) {
            c.child.handle_event(e, v).await?;
        }
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            gx: _,
            children: _,
            children_ref,
            direction,
            flex,
            focused,
            horizontal_margin,
            margin,
            spacing,
            vertical_margin,
        } = self;
        direction.update(id, &v).context("layout direction update")?;
        flex.update(id, &v).context("layout flex update")?;
        focused.update(id, &v).context("layout focused update")?;
        horizontal_margin.update(id, &v).context("layout horizontal_margin update")?;
        margin.update(id, &v).context("layout margin update")?;
        spacing.update(id, &v).context("layout spacing update")?;
        vertical_margin.update(id, &v).context("layout vertical_margin update")?;
        if children_ref.id == id {
            self.set_children(v.clone()).await?;
        }
        for c in &mut self.children {
            c.child.handle_update(id, v.clone()).await?
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let Self {
            gx: _,
            children,
            children_ref: _,
            direction,
            flex,
            focused: _,
            horizontal_margin,
            margin,
            spacing,
            vertical_margin,
        } = self;
        let mut layout = Layout::default();
        if let Some(Some(d)) = direction.t {
            layout = layout.direction(d.0);
        }
        if let Some(Some(f)) = flex.t {
            layout = layout.flex(f.0);
        }
        if let Some(Some(m)) = horizontal_margin.t {
            layout = layout.horizontal_margin(m);
        }
        if let Some(Some(m)) = margin.t {
            layout = layout.margin(m);
        }
        if let Some(Some(s)) = &spacing.t {
            layout = layout.spacing(s.0.clone());
        }
        if let Some(Some(m)) = vertical_margin.t {
            layout = layout.vertical_margin(m);
        }
        layout = layout.constraints(children.iter().map(|c| c.constraint));
        let areas = layout.split(rect);
        for (rect, child) in areas.iter().zip(children.iter_mut()) {
            let size = SizeV::from(*rect);
            if child.last_size != size {
                child.last_size = size;
                child.size_ref.set_deref(size)?;
            }
            child.child.draw(frame, *rect)?
        }
        Ok(())
    }
}
