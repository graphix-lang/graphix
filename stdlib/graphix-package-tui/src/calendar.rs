use super::{StyleV, TuiW, TuiWidget};
use anyhow::{Context, Result, bail};
use async_trait::async_trait;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::calendar::{CalendarEventStore, Monthly},
};
use time::{Date, Month};

/// The [`Date`] nearest `(year, month, day)`, and whether it differs.
/// The year stays a step inside `Date`'s range, since a month view
/// steps past the date it shows.
fn coerce_date(year: i64, month: i64, day: i64) -> (Date, bool) {
    let y = year.clamp(Date::MIN.year() as i64 + 1, Date::MAX.year() as i64 - 1);
    let m = month.clamp(1, 12);
    let month_v = Month::try_from(m as u8).expect("a month in 1..=12");
    let d = day.clamp(1, month_v.length(y as i32) as i64);
    let date = Date::from_calendar_date(y as i32, month_v, d as u8)
        .expect("a clamped date is valid");
    (date, (y, m, d) != (year, month, day))
}

#[derive(Clone, Copy)]
struct DateV(Date);

impl FromValue for DateV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            day: i64,
            month: i64,
            year: i64,
        }
        let Fields { day, month, year } = v.cast_to()?;
        let (date, clamped) = coerce_date(year, month, day);
        if clamped {
            log::warn!("calendar date ({year}, {month}, {day}) coerced to {date}");
        }
        Ok(Self(date))
    }
}

#[derive(FromValue)]
struct EventV {
    date: DateV,
    style: StyleV,
}

graphix_rt::props! {
    struct Props {
        default_style: Option<StyleV>,
        display_date: DateV,
        show_month: Option<StyleV>,
        show_surrounding: Option<StyleV>,
        show_weekday: Option<StyleV>,
    }
}

pub(super) struct CalendarW<X: GXExt> {
    p: Props<X>,
    events_ref: Ref<X>,
    events: CalendarEventStore,
}

impl<X: GXExt> CalendarW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        #[derive(FromValue)]
        struct Fields {
            events: u64,
        }
        let p = Props::compile(&gx, &v).await.context("calendar")?;
        let Fields { events } = v.cast_to().context("calendar fields")?;
        let events_ref = gx.compile_ref(events).await?;
        let mut t = Self { p, events_ref, events: CalendarEventStore::default() };
        if let Some(v) = t.events_ref.last.take() {
            t.set_events(&v)?;
        }
        Ok(Box::new(t))
    }

    fn set_events(&mut self, v: &Value) -> Result<()> {
        self.events = CalendarEventStore::default();
        match v {
            Value::Null => return Ok(()),
            Value::Array(a) => {
                for ev in a {
                    let EventV { date, style } = ev.clone().cast_to::<EventV>()?;
                    self.events.add(date.0, style.0);
                }
            }
            v => bail!("invalid calendar events {v}"),
        }
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for CalendarW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("calendar")?;
        if self.events_ref.id == id {
            self.set_events(&v)?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let Some(date) = p.display_date.t else { return Ok(()) };
        let mut cal = Monthly::new(date.0, &self.events);
        if let Some(Some(s)) = &p.show_surrounding.t {
            cal = cal.show_surrounding(s.0);
        }
        if let Some(Some(s)) = &p.show_weekday.t {
            cal = cal.show_weekdays_header(s.0);
        }
        if let Some(Some(s)) = &p.show_month.t {
            cal = cal.show_month_header(s.0);
        }
        if let Some(Some(s)) = &p.default_style.t {
            cal = cal.default_style(s.0);
        }
        frame.render_widget(cal, rect);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::coerce_date;
    use time::{Date, Month};

    fn date(y: i32, m: Month, d: u8) -> Date {
        Date::from_calendar_date(y, m, d).unwrap()
    }

    #[test]
    fn valid_dates_pass_through() {
        assert_eq!(coerce_date(2024, 2, 29), (date(2024, Month::February, 29), false));
    }

    #[test]
    fn days_and_months_clamp() {
        assert_eq!(coerce_date(2023, 2, 30), (date(2023, Month::February, 28), true));
        assert_eq!(coerce_date(2023, 13, 0), (date(2023, Month::December, 1), true));
    }

    #[test]
    fn years_stay_a_step_inside_the_range() {
        assert_eq!(coerce_date(20000, 6, 15), (date(9998, Month::June, 15), true));
        assert_eq!(coerce_date(9999, 12, 31), (date(9998, Month::December, 31), true));
        assert_eq!(coerce_date(-20000, 1, 1), (date(-9998, Month::January, 1), true));
    }
}
