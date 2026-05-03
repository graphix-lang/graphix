use super::{StyleV, TuiW, TuiWidget};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use async_trait::async_trait;
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use netidx::publisher::{FromValue, Value};
use ratatui::{
    layout::Rect,
    widgets::calendar::{CalendarEventStore, Monthly},
    Frame,
};
use time::{Date, Month};
use tokio::try_join;

/// Build a [`Date`] from a possibly-out-of-range (year, month, day)
/// triple. Returns the coerced date and the original `(year, month,
/// day)` if any clamping happened, so the caller can warn once.
///
/// Clamping rules: month → `[1, 12]`, day → `[1, days_in_month]`,
/// year → the i32 range. The result is always a valid `Date` for any
/// finite input, so widgets that depend on `Date` can render without
/// panicking on user input that came from unchecked arithmetic.
fn coerce_date(year: i64, month: i64, day: i64) -> (Date, Option<(i64, i64, i64)>) {
    let mut clamped = false;
    let y = year.clamp(i32::MIN as i64, i32::MAX as i64) as i32;
    if y as i64 != year {
        clamped = true;
    }
    let m_clamped = month.clamp(1, 12);
    if m_clamped != month {
        clamped = true;
    }
    let m = Month::try_from(m_clamped as u8).unwrap_or(Month::January);
    // `time` accepts year-month-aware day ranges, so we have to query
    // the month's length to clamp day correctly.
    let last_day = month_length(y, m) as i64;
    let d_clamped = day.clamp(1, last_day);
    if d_clamped != day {
        clamped = true;
    }
    // After clamping all three components we should always succeed,
    // but if `time` rejects it for another reason fall back to
    // 1970-01-01 — better than panicking.
    let date = Date::from_calendar_date(y, m, d_clamped as u8)
        .unwrap_or_else(|_| Date::from_calendar_date(1970, Month::January, 1).unwrap());
    if clamped {
        (date, Some((year, month, day)))
    } else {
        (date, None)
    }
}

fn month_length(year: i32, month: Month) -> u8 {
    // Walk from the first of the month forward to find the next
    // first; the day before is the last of `month`. Avoids
    // hand-rolling leap-year math.
    let first = Date::from_calendar_date(year, month, 1)
        .unwrap_or_else(|_| Date::from_calendar_date(2000, Month::January, 1).unwrap());
    let next = first.replace_month(month.next()).unwrap_or_else(|_| {
        // December → January next year. If that overflows, fall back
        // to a 31-day default — cosmetic, the widget still renders.
        first
    });
    if month == Month::December {
        31
    } else {
        let days = (next - first).whole_days();
        days.clamp(28, 31) as u8
    }
}

#[derive(Clone, Copy)]
struct DateV(Date);

impl FromValue for DateV {
    fn from_value(v: Value) -> Result<Self> {
        // graphix structs are stored sorted by field name (day, month, year)
        let [(_, day), (_, month), (_, year)] = v.cast_to::<[(ArcStr, i64); 3]>()?;
        let (date, clamped) = coerce_date(year, month, day);
        if let Some((y, m, d)) = clamped {
            log::warn!("calendar date ({y}, {m}, {d}) coerced to {date}");
        }
        Ok(Self(date))
    }
}

struct EventV {
    date: DateV,
    style: StyleV,
}

impl FromValue for EventV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, date), (_, style)] = v.cast_to::<[(ArcStr, Value); 2]>()?;
        Ok(Self { date: date.cast_to::<DateV>()?, style: style.cast_to::<StyleV>()? })
    }
}

pub(super) struct CalendarW<X: GXExt> {
    display_date: TRef<X, DateV>,
    events_ref: Ref<X>,
    events: CalendarEventStore,
    show_month: TRef<X, Option<StyleV>>,
    show_surrounding: TRef<X, Option<StyleV>>,
    show_weekday: TRef<X, Option<StyleV>>,
    default_style: TRef<X, Option<StyleV>>,
}

impl<X: GXExt> CalendarW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let [(_, default_style), (_, display_date), (_, events), (_, show_month), (_, show_surrounding), (_, show_weekday)] =
            v.cast_to::<[(ArcStr, u64); 6]>().context("calendar fields")?;
        let (
            default_style,
            display_date,
            events_ref,
            show_month,
            show_surrounding,
            show_weekday,
        ) = try_join! {
            gx.compile_ref(default_style),
            gx.compile_ref(display_date),
            gx.compile_ref(events),
            gx.compile_ref(show_month),
            gx.compile_ref(show_surrounding),
            gx.compile_ref(show_weekday)
        }?;
        let mut t = Self {
            display_date: TRef::new(display_date)
                .context("calendar tref display_date")?,
            events_ref,
            events: CalendarEventStore::default(),
            show_month: TRef::new(show_month).context("calendar tref show_month")?,
            show_surrounding: TRef::new(show_surrounding)
                .context("calendar tref show_surrounding")?,
            show_weekday: TRef::new(show_weekday)
                .context("calendar tref show_weekday")?,
            default_style: TRef::new(default_style)
                .context("calendar tref default_style")?,
        };
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
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        let Self {
            display_date,
            events_ref,
            events: _,
            show_month,
            show_surrounding,
            show_weekday,
            default_style,
        } = self;
        display_date.update(id, &v).context("calendar update display_date")?;
        show_month.update(id, &v).context("calendar update show_month")?;
        show_surrounding.update(id, &v).context("calendar update show_surrounding")?;
        show_weekday.update(id, &v).context("calendar update show_weekday")?;
        default_style.update(id, &v).context("calendar update default_style")?;
        if events_ref.id == id {
            self.set_events(&v)?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let Self {
            display_date,
            events_ref: _,
            events,
            show_month,
            show_surrounding,
            show_weekday,
            default_style,
        } = self;
        let mut cal = Monthly::new(display_date.t.unwrap().0, &*events);
        if let Some(Some(s)) = &show_surrounding.t {
            cal = cal.show_surrounding(s.0);
        }
        if let Some(Some(s)) = &show_weekday.t {
            cal = cal.show_weekdays_header(s.0);
        }
        if let Some(Some(s)) = &show_month.t {
            cal = cal.show_month_header(s.0);
        }
        if let Some(Some(s)) = &default_style.t {
            cal = cal.default_style(s.0);
        }
        frame.render_widget(cal, rect);
        Ok(())
    }
}
