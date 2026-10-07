use super::{StyleV, TuiW, TuiWidget};
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
    style::Style,
    widgets::{RenderDirection, Sparkline, SparklineBar},
};

/// The integer range bars are scaled into: ratatui scales by integer math
/// (`value * height * 8 / max`), which this keeps exact and overflow free.
const SCALE: u64 = 1 << 20;

#[derive(Clone, Copy)]
struct RenderDirectionV(RenderDirection);

impl FromValue for RenderDirectionV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "LeftToRight" => Ok(Self(RenderDirection::LeftToRight)),
            "RightToLeft" => Ok(Self(RenderDirection::RightToLeft)),
            s => bail!("invalid render direction {s}"),
        }
    }
}

/// A bar's value, absent when it is null or not finite.
struct Bar {
    value: Option<f64>,
    style: Option<Style>,
}

impl FromValue for Bar {
    fn from_value(v: Value) -> Result<Self> {
        let (value, style) = match v {
            Value::Array(_) => {
                #[derive(FromValue)]
                struct Fields {
                    style: Option<StyleV>,
                    value: Option<f64>,
                }
                let Fields { style, value } = v.cast_to()?;
                (value, style.map(|s| s.0))
            }
            v => (v.cast_to::<Option<f64>>()?, None),
        };
        Ok(Self { value: value.filter(|v| v.is_finite()), style })
    }
}

/// The scale's top: positive and finite, else the data's maximum.
#[derive(Clone, Copy)]
struct MaxV(Option<f64>);

impl FromValue for MaxV {
    fn from_value(v: Value) -> Result<Self> {
        match v.cast_to::<Option<f64>>()? {
            Some(m) if !(m.is_finite() && m > 0.) => {
                log::warn!("sparkline max {m} is not positive; scaling to the data");
                Ok(Self(None))
            }
            m => Ok(Self(m)),
        }
    }
}

graphix_rt::props! {
    struct Props {
        absent_value_style: Option<StyleV>,
        absent_value_symbol: Option<ArcStr>,
        data: Vec<Bar>,
        direction: Option<RenderDirectionV>,
        max: MaxV,
        style: Option<StyleV>,
    }
}

pub(super) struct SparklineW<X: GXExt>(Props<X>);

impl<X: GXExt> SparklineW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        Ok(Box::new(Self(Props::compile(&gx, &v).await.context("sparkline")?)))
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for SparklineW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.0.update(id, &v).context("sparkline")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.0;
        let data = p.data.t.as_deref().unwrap_or(&[]);
        let max =
            p.max.t.and_then(|m| m.0).unwrap_or_else(|| {
                data.iter().filter_map(|b| b.value).fold(0., f64::max)
            });
        let bars = data.iter().map(|b| {
            let scaled = b.value.map(|v| {
                if max > 0. { ((v / max).clamp(0., 1.) * SCALE as f64) as u64 } else { 0 }
            });
            SparklineBar::from(scaled).style(b.style)
        });
        let mut spark = Sparkline::default().data(bars).max(SCALE);
        if let Some(Some(s)) = &p.absent_value_style.t {
            spark = spark.absent_value_style(s.0);
        }
        if let Some(Some(s)) = &p.absent_value_symbol.t {
            spark = spark.absent_value_symbol(s.to_string());
        }
        if let Some(Some(s)) = &p.style.t {
            spark = spark.style(s.0);
        }
        if let Some(Some(d)) = p.direction.t {
            spark = spark.direction(d.0);
        }
        frame.render_widget(spark, rect);
        Ok(())
    }
}
