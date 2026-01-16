# The Calendar Widget

The `calendar` widget displays a monthly calendar view with support for highlighting specific dates and displaying events. It's perfect for date pickers, event schedulers, and time-based visualizations.

## Interface

```graphix
type Date = {
  year: i64,
  month: i64,
  day: i64
};

val date: fn(i64, i64, i64) -> Date;

type CalendarEvent = {
  date: Date,
  style: Style
};

val calendar_event: fn(Style, Date) -> CalendarEvent;

val calendar: fn(
  ?#default_style: &[Style, null],
  ?#show_month: &[Style, null],
  ?#show_surrounding: &[Style, null],
  ?#show_weekday: &[Style, null],
  ?#events: &[Array<CalendarEvent>, null],
  &Date
) -> Tui;
```

## Parameters

### calendar
- **show_month** - Style for the month header
- **show_weekday** - Style for weekday headers (Mon, Tue, etc.)
- **show_surrounding** - Style for dates from surrounding months
- **events** - Array of CalendarEvent objects to highlight dates

### calendar_event
Takes a style and a date to create an event marker.

### date
Creates a date with year, month (1-12), and day (1-31).

## Examples

### Basic Usage

```graphix
{{#include ../../examples/tui/calendar_basic.gx}}
```

![Basic Calendar](media/calendar_basic.png)

### Event Calendar

```graphix
{{#include ../../examples/tui/calendar_events.gx}}
```

![Calendar With Events](media/calendar_events.png)

### Color-coded Events by Type

```graphix
{{#include ../../examples/tui/calendar_typed.gx}}
```

![Calendar Typed](media/calendar_typed.png)


## See Also

- [table](table.md) - For tabular date-based data
- [list](list.md) - For event lists
- [block](block.md) - For containing calendars with borders
