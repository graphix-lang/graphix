use super::{SpanV, StyleV, TuiW, TuiWidget};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use netidx::publisher::Value;
use ratatui::{layout::Rect, widgets::Gauge, Frame};
use tokio::try_join;

/// Clamp `raw` into the [0, 1] range ratatui requires (it panics on
/// out-of-range ratios). Logs a warning the first time we see each
/// distinct out-of-range value, so a stuck-bad input warns once
/// instead of on every redraw. `last` carries the previously warned
/// value's bit pattern so the dedup is reliable across NaN and signed
/// zero. NaN clamps to 0.0 since ratatui can't draw it either.
pub(super) fn clamp_ratio(widget: &str, last: &mut Option<u64>, raw: f64) -> f64 {
    if (0.0..=1.0).contains(&raw) {
        *last = None;
        return raw;
    }
    let clamped = if raw.is_nan() { 0.0 } else { raw.clamp(0.0, 1.0) };
    let bits = raw.to_bits();
    if *last != Some(bits) {
        log::warn!("{widget} ratio {raw} outside [0, 1]; clamping to {clamped}");
        *last = Some(bits);
    }
    clamped
}

pub(super) struct GaugeW<X: GXExt> {
    gauge_style: TRef<X, Option<StyleV>>,
    label: TRef<X, Option<SpanV>>,
    ratio: TRef<X, f64>,
    style: TRef<X, Option<StyleV>>,
    use_unicode: TRef<X, Option<bool>>,
    last_clamp_warning: Option<u64>,
}

impl<X: GXExt> GaugeW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, gauge_style), (_, label), (_, ratio), (_, style), (_, use_unicode)] =
            v.cast_to::<[(ArcStr, u64); 5]>()?;
        let (gauge_style, label, ratio, style, use_unicode) = try_join! {
            gx.compile_ref(gauge_style),
            gx.compile_ref(label),
            gx.compile_ref(ratio),
            gx.compile_ref(style),
            gx.compile_ref(use_unicode)
        }?;
        Ok(Box::new(Self {
            gauge_style: TRef::new(gauge_style).context("gage tref gauge_style")?,
            label: TRef::new(label).context("gage tref label")?,
            ratio: TRef::new(ratio).context("gage tref ratio")?,
            style: TRef::new(style).context("gage tref style")?,
            use_unicode: TRef::new(use_unicode).context("gage tref use_unicode")?,
            last_clamp_warning: None,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::clamp_ratio;

    #[test]
    fn in_range_passes_through() {
        let mut last = None;
        for v in [0.0, 0.5, 1.0, 0.999999] {
            assert_eq!(clamp_ratio("g", &mut last, v), v);
        }
        assert_eq!(last, None);
    }

    #[test]
    fn out_of_range_clamps() {
        let mut last = None;
        assert_eq!(clamp_ratio("g", &mut last, 1.5), 1.0);
        assert_eq!(clamp_ratio("g", &mut last, -0.3), 0.0);
        assert_eq!(clamp_ratio("g", &mut last, 100.0), 1.0);
        assert_eq!(clamp_ratio("g", &mut last, f64::INFINITY), 1.0);
        assert_eq!(clamp_ratio("g", &mut last, f64::NEG_INFINITY), 0.0);
    }

    #[test]
    fn nan_clamps_to_zero() {
        let mut last = None;
        assert_eq!(clamp_ratio("g", &mut last, f64::NAN), 0.0);
    }

    #[test]
    fn last_resets_when_back_in_range() {
        let mut last = None;
        clamp_ratio("g", &mut last, 5.0);
        assert!(last.is_some());
        clamp_ratio("g", &mut last, 0.5);
        assert_eq!(last, None);
    }

    #[test]
    fn dedupes_repeated_out_of_range_value() {
        // Same input twice → `last` should still record the bit
        // pattern (so the caller's log!/warn! only fires the first
        // time a given value is seen).
        let mut last = None;
        clamp_ratio("g", &mut last, 1.7);
        let after_first = last;
        clamp_ratio("g", &mut last, 1.7);
        assert_eq!(last, after_first);
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for GaugeW<X> {
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.gauge_style.update(id, &v).context("gage update gauge_style")?;
        self.label.update(id, &v).context("gage update label")?;
        self.ratio.update(id, &v).context("gage update ratio")?;
        self.style.update(id, &v).context("gage update style")?;
        self.use_unicode.update(id, &v).context("gage update use_unicode")?;
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let raw = self.ratio.t.unwrap_or(0.0);
        let ratio = clamp_ratio("gauge", &mut self.last_clamp_warning, raw);
        let mut g = Gauge::default().ratio(ratio);
        if let Some(Some(s)) = &self.label.t {
            g = g.label(s.0.clone());
        }
        if let Some(Some(s)) = &self.style.t {
            g = g.style(s.0);
        }
        if let Some(Some(s)) = &self.gauge_style.t {
            g = g.gauge_style(s.0);
        }
        if let Some(Some(u)) = self.use_unicode.t {
            g = g.use_unicode(u);
        }
        frame.render_widget(g, rect);
        Ok(())
    }
}
