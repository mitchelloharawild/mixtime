# Cyclical time points

`cyclical_time()` creates a vector of cyclical time points representing
positions within repeating cycles. This function is useful for creating
custom cyclical time representations that aren't covered by the
convenience functions like
[`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
or
[`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md).

## Usage

``` r
cyclical_time(
  data,
  chronon,
  cycle,
  tz = tz_name(data),
  discrete = TRUE,
  calendar = time_calendar(data)
)
```

## Arguments

- data:

  Input data to convert to cyclical time. Can be:

  - Numeric values (interpreted as chronons, 1-indexed)

  - Character strings (parsed as dates/times)

  - Date or POSIXct objects

  - Other time objects

- chronon:

  A time unit expression representing the chronon (smallest indivisible
  time unit), evaluated in the context of `calendar`. Use unquoted
  expressions like `day(1L)` or `month(1L)`. Chronons from a specific
  calendar can also be used (e.g. `cal_isoweek$day(1L)`).

- cycle:

  A time unit expression representing the cycle (larger time unit that
  defines the period), evaluated in the context of `calendar`. Use
  unquoted expressions like `week(1L)` or `year(1L)`.

- tz:

  Time zone for the time representation. Defaults to the time zone of
  the input `data` (`tz_name(data)`). Time zones need to be valid
  identifiers for the IANA time zone database
  ([`tzdb::tzdb_names()`](https://tzdb.r-lib.org/reference/tzdb_names.html))

- discrete:

  Logical. If `TRUE` (default), returns integer positions within the
  cycle (discrete time model). If `FALSE`, returns fractional positions
  allowing representation of partial time units (continuous time model).

- calendar:

  Calendar system used to evaluate `chronon` and `cycle`. Defaults to
  `time_calendar(data)` for existing time objects. Common options
  include
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md)
  and
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md).

## Value

A `mt_cyclical` time vector, which is a subclass of `mt_time`.

## See also

- [`new_cyclical_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_cyclical_time_fn.md)
  for creating reusable cyclical time functions

- [`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`day_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  for common cyclical representations

- [`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md),
  [`week_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  for other cyclical time helpers

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
# Day of week (1-7, Monday = 1)
cyclical_time(
  Sys.Date(),
  chronon = day(1L),
  cycle = week(1L),
  calendar = cal_isoweek
)
#> <mixtime[1]>
#> [1] Tue

# Month of year (1-12)
cyclical_time(
  Sys.Date(),
  chronon = month(1L),
  cycle = year(1L)
)
#> <mixtime[1]>
#> [1] Feb

# Discrete vs continuous time
# yearweek(x) is linear_time(x, chronon = day(1L), cycle = week(1L), calendar = cal_isoweek)
yearweek(Sys.time(), discrete = TRUE)
#> <mixtime[1]>
#> [1] 2026-W9
yearweek(Sys.time(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-W9-15.6%

# Day of month with Gregorian calendar
cyclical_time(
  Sys.Date(),
  chronon = day(1L),
  cycle = month(1L),
  calendar = cal_gregorian
)
#> <mixtime[1]>
#> [1] 24
```
