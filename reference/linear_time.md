# Linear time points

`linear_time()` creates a vector of linear time points with a specified
chronon (smallest time unit). This function is useful for creating
custom time representations that aren't covered by the convenience
functions like
[`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
or
[`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md).

## Usage

``` r
linear_time(
  data,
  chronon = time_chronon(data),
  discrete = TRUE,
  calendar = time_calendar(data),
  granules = chronon_granules(chronon),
  tz = tz_name(data)
)
```

## Arguments

- data:

  Input data to convert to linear time. Can be:

  - Numeric values (interpreted as chronons since Unix epoch)

  - Character strings (parsed as dates/times)

  - Date or POSIXct objects

  - Other time objects

- chronon:

  A time unit expression representing the chronon (smallest indivisible
  time unit), evaluated in the context of `calendar`. Use unquoted
  expressions like `month(1L)` or `hour(1L)`. Chronons from a specific
  calendar can also be used (e.g. `cal_isoweek$week(1L)`). Defaults to
  the time chronon of the input `data` (`time_chronon(data)`).

- discrete:

  Logical. If `TRUE` (default), returns integer chronons since Unix
  epoch (discrete time model). If `FALSE`, returns fractional chronons
  allowing representation of partial time units (continuous time model).

- calendar:

  Calendar system used to evaluate `chronon` and `granules`. Defaults to
  `time_calendar(data)` for existing time objects. Common options
  include
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md)
  and
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md).

- granules:

  A list of time unit expressions representing structural units larger
  than the chronon (e.g., years, quarters, months). These define how
  time is displayed and grouped. Use unquoted expressions like
  `list(year(1L), quarter(1L))`. Defaults to an empty list.

- tz:

  Time zone for the time representation. Defaults to the time zone of
  the input `data` (`tz_name(data)`). Time zones need to be valid
  identifiers for the IANA time zone database
  ([`tzdb::tzdb_names()`](https://tzdb.r-lib.org/reference/tzdb_names.html))

## Value

A `mt_linear` time vector, which is a subclass of `mt_time`.

## See also

- [`new_linear_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_linear_time_fn.md)
  for creating reusable linear time functions

- [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  [`yearquarter()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md),
  [`year()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  for Gregorian time representations

- [`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  for ISO 8601 week-based time

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
# Hourly time with year-month-day granules
linear_time(
  Sys.time(),
  chronon = hour(1L),
  granules = list(year(1L), month(1L), day(1L))
)
#> <mixtime[1]>
#> [1] 2026-Feb-24-h2

# Monthly chronons with year-quarter granules
linear_time(
  Sys.Date(),
  chronon = month(1L),
  granules = list(year(1L), quarter(1L))
)
#> <mixtime[1]>
#> [1] 2026-Q1-M1

# Discrete vs continuous time
linear_time(Sys.time(), chronon = day(1L), discrete = TRUE)
#> <mixtime[1]>
#> [1] 2026-Feb-24
linear_time(Sys.time(), chronon = day(1L), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-Feb-24-9.3%

# ISO week calendar with week-day structure
linear_time(
  Sys.Date(),
  chronon = day(1L),
  granules = list(year(1L), week(1L)),
  calendar = cal_isoweek
)
#> <mixtime[1]>
#> [1] 2026-W9-Tue
```
