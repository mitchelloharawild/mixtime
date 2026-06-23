# Changelog

## mixtime 0.2.0

### New features

- Added support for multiplication and division of time durations.
- Added
  [`round()`](https://rdrr.io/r/base/Round.html)/[`floor()`](https://rdrr.io/r/base/Round.html)/[`ceiling()`](https://rdrr.io/r/base/Round.html)
  methods for rounding time by chronons. (note: use
  [`time_round()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_round.md),
  [`time_floor()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_round.md),
  and
  [`time_ceiling()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_round.md)
  for rounding to a specified time granule).
- Added casting from time durations to character vectors.
- Added explicit conversion to naive time by using `tz = NA`.

### Improvements

- [`time_unit_full()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_unit_labels.md)
  now uses cli-style pluralisation templates (e.g., `"year{?/s}"`,
  `"centur{?y/ies}"`), enabling accurate plural forms beyond a simple
  `s` suffix. A new `time_unit_plural(x, n)` helper resolves the
  template for a given quantity.
- Continuous time model durations now always show at least one decimal
  place.
- Added formatting support for `NaN`, `Inf`, and `-Inf` values in time
  types.
- Added time zone support for arithmetic.
- Time durations now use the same formatting system and format strings
  as linear and cyclical time.
- Improved [`str()`](https://rdrr.io/r/utils/str.html) output to be more
  compact for each time type.

### Bug fixes

- Fixed incorrect usage of
  [`time_chronon()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_chronon.md)
  causing arithmetic to fail.
- Fixed incompatibilities with vctrs sorting, ptype2, and casting
  methods.
- Fixed formatting of cyclical time with mixed-calendar chronon and
  cycle
  ([\#62](https://github.com/mitchelloharawild/mixtime/issues/62)).
- Fixed divmod of Gregorian day -\> month and day -\> year producing
  incorrect divisors near the year boundary for continuous time dates.
- Fixed [`c()`](https://rdrr.io/r/base/c.html) allowing non-time vectors
  in mixtime vectors.

## mixtime 0.1.0

CRAN release: 2026-05-19

This is the initial CRAN release of the package with provides the core
data types and temporal manipulation for temporal analysis with mixed
granularity data.

### New features

#### Time

- The generic
  [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/mixtime.md)
  constructor for creating mixed temporal vectors that combine time
  points of different granularities (e.g. monthly and quarterly) in a
  single vector via `vecvec`.

#### Linear time

- [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
  creates linear time vectors with a user-specified chronon (smallest
  indivisible time granule), supporting both discrete (integer) and
  continuous (fractional) time models.
- Convenience functions for common temporal granularities:
  [`year()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  [`yearquarter()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  [`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  [`date()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  and
  [`datetime()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md).

#### Cyclical time

- [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md)
  creates cyclical time vectors representing positions within a
  repeating cycle (e.g. day-of-week, month-of-year).
- Convenience functions for common cyclical temporal granularities:
  [`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`day_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`week_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  and
  [`time_of_day()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md).

#### Calendar systems

- Three initial calendar systems:
  - `cal_gregorian`: standard Gregorian calendar with year, quarter,
    month, day, hour, minute, second, and millisecond units.
  - `cal_isoweek`: ISO 8601 week-date calendar with ISO year, week, and
    day units. Weeks always start on Monday; the first week of the year
    is the week containing the first Thursday.
  - `cal_sym454`: Symmetry454 perennial solar calendar with a
    symmetrical 4–5–4 week pattern per quarter and a leap-week rule.

#### Sequences

- [`seq()`](https://rdrr.io/r/base/seq.html) methods for `mixtime` and
  `mt_time` objects support integer, string (e.g. `"1 month"`), and time
  unit `by` arguments, as well as `length.out` and `along.with`.
  Overflow behaviour when step granules differ from sequence granules
  can be controlled with `on_invalid = "nearest"` (default) or
  `"overflow"`.

#### Rounding

- `round_time()`, `floor_time()`, and `ceiling_time()` round time
  objects to a specified time granule, preserving the input class and
  timezone.

#### Formatting

- A mixed-calendar general purpose formatting system is provided for
  `mixtime` objects. The format strings use glue-like
  [`{}`](https://rdrr.io/r/base/Paren.html) substitutions with the
  helper functions `lin(x)` and `cyc(x, y)` to position linear and
  cyclical time components in a string. `x` and `y` are time granules
  from a calendar, which can be used to create general purpose
  mixed-calendar time formats.

#### Timezone support

- [`tz_name()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_name.md)
  extracts the timezone from a time object.
- [`tz_offset()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_offset.md)
  returns the UTC offset for a datetime in its timezone.
- [`tz_abbreviation()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_abbreviation.md)
  returns the timezone abbreviation (e.g. `"EST"`, `"PDT"`).
- [`tz_transitions()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_transitions.md)
  returns a data frame of DST and other timezone transitions between two
  time points.

#### Accessor functions

- [`time_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_calendar.md)
  extracts the calendar system from a time object.

#### tsibble and fable compatibility

- Methods for `tsibble` index validation are provided, enabling
  `mixtime` vectors to be used as time indexes in `tsibble` objects and
  in forecasting with `fable`.

#### Extensibility

- [`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md)
  defines a calendar as a named collection of time unit classes, with
  optional inheritance from another calendar.

- [`mt_unit()`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  [`mt_tz_unit()`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  and
  [`mt_loc_unit()`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  primitives for creating new time units.

- [`new_linear_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_linear_time_fn.md)
  and
  [`new_cyclical_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_cyclical_time_fn.md)
  are factories for building convenient linear and cyclical time
  functions
  (e.g. [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)).

- Calendar arithmetic with:

  - [`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_divmod.md)
    performs division-with-remainder arithmetic between time granules
    (e.g. 1 day to 1 month), enabling cross-granule calendrical
    arithmetic.
  - [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md)
    returns the number of smaller granules contained in a coarser
    granule (e.g. days in 1 month), with context-dependent results for
    variable-length granules.
  - [`chronon_epoch()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_epoch.md)
    returns the epoch offset for a given time unit.

- Displaying time with:

  - [`time_unit_abbr()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_unit_labels.md)
    and
    [`time_unit_full()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_unit_labels.md)
    for time unit text.
  - [`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_labels.md)
    and
    [`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_labels.md)
    for time labels (e.g. Jan, Feb, … for months of year).
  - [`chronon_format_linear()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_format.md)
    and
    [`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_format.md)
    for default formatting strings.
  - [`chronon_format_attr()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_format_attr.md)
    for appending attribute information (e.g. timezones)

#### Vignettes

- Extending mixtime - a vignette on how to define new calendars and
  implement calendar arithmetic for temporal operations within the
  calendar’s units and with other calendars.
