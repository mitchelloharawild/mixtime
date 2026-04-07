# mixtime (development version)

This is the initial CRAN release of the package with provides the core data 
types and temporal manipulation for temporal analysis with mixed granularity
data.

## New features

### Time

* The generic `mixtime()` constructor for creating mixed temporal vectors that 
  combine time points of different granularities (e.g. monthly and quarterly)
  in a single vector via `vecvec`.

### Linear time

* `linear_time()` creates linear time vectors with a user-specified chronon
  (smallest indivisible time unit), supporting both discrete (integer) and
  continuous (fractional) time models.
* Convenience functions for common temporal granularities: `year()`,
  `yearquarter()`, `yearmonth()`, `yearweek()`, `date()`, and `datetime()`.

### Cyclical time

* `cyclical_time()` creates cyclical time vectors representing positions
  within a repeating cycle (e.g. day-of-week, month-of-year).
* Convenience functions for common cyclical temporal granularities:
  `month_of_year()`, `day_of_year()`, `day_of_month()`, `day_of_week()`, 
  `week_of_year()`, and `time_of_day()`.

### Calendar systems

* Three initial calendar systems:
  * `cal_gregorian`: standard Gregorian calendar with year, quarter, month,
    day, hour, minute, second, and millisecond units.
  * `cal_isoweek`: ISO 8601 week-date calendar with ISO year, week, and day
    units. Weeks always start on Monday; the first week of the year is the
    week containing the first Thursday.
  * `cal_sym454`: Symmetry454 perennial solar calendar with a symmetrical
    4–5–4 week pattern per quarter and a leap-week rule.

### Sequences

* `seq()` methods for `mixtime` and `mt_time` objects support integer, string
  (e.g. `"1 month"`), and time unit `by` arguments, as well as `length.out`
  and `along.with`. Overflow behaviour when step units differ from sequence
  units can be controlled with `on_invalid = "nearest"` (default) or
  `"overflow"`.

### Rounding

* `round_time()`, `floor_time()`, and `ceiling_time()` round time objects to a
  specified time unit, preserving the input class and timezone.

### Formatting

* A mixed-calendar general purpose formatting system is provided for `mixtime`
  objects. The format strings use glue-like `{}` substitutions with the helper
  functions `lin(x)` and `cyc(x, y)` to position linear and cyclical time
  components in a string. `x` and `y` are time units from a calendar, which can
  be used to create general purpose mixed-calendar time formats.

### Timezone support

* `tz_name()` extracts the timezone from a time object.
* `tz_offset()` returns the UTC offset for a datetime in its timezone.
* `tz_abbreviation()` returns the timezone abbreviation (e.g. `"EST"`, `"PDT"`).
* `tz_transitions()` returns a data frame of DST and other timezone transitions
  between two time points.

### Accessor functions

* `time_calendar()` extracts the calendar system from a time object.

### tsibble and fable compatibility

* Methods for `tsibble` index validation are provided, enabling `mixtime`
  vectors to be used as time indexes in `tsibble` objects and in forecasting
  with `fable`.

### Extensibility

* `new_calendar()` defines a calendar as a named collection of time unit
  classes, with optional inheritance from another calendar.
* `mt_unit()`, `mt_tz_unit()`, and `mt_loc_unit()` primitives for creating
  new time units.
* `new_linear_time_fn()` and `new_cyclical_time_fn()` are factories for building
  convenient linear and cyclical time functions (e.g. `yearmonth()`).
* Calendar arithmetic with:

  * `chronon_divmod()` performs division-with-remainder arithmetic between time
    units (e.g. days to months), enabling cross-unit calendrical arithmetic.
  * `chronon_cardinality()` returns the number of smaller units contained in a
    larger unit (e.g. days in a month), with context-dependent results for
    variable-length units.
  * `chronon_epoch()` returns the epoch offset for a given time unit.

* Displaying time with:

  * `time_unit_abbr()` and `time_unit_full()` for time unit text
  * `linear_labels()` and `cyclical_labels()` for time labels (e.g. Jan, Feb,
     ... for months of year).
  * `chronon_format_linear()` and `chronon_format_cyclical()` for default
    formatting strings
  * `chronon_format_attr()` for appending attribute information (e.g. timezones)

### Vignettes

* Extending mixtime - a vignette on how to define new calendars and implement
  calendar arithmetic for temporal operations within the calendar's units and
  with other calendars.