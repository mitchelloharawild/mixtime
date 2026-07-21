# Duration vectors

`duration()` creates a vector of durations with a specified chronon.
Durations represent a fixed span of time measured in a given time
granule (e.g., 3 months, 5 days), without reference to a specific point
in time.

## Usage

``` r
duration(
  data,
  chronon = time_chronon(data),
  discrete = NULL,
  calendar = time_calendar(data)
)
```

## Arguments

- data:

  A time vector of duration magnitudes, or an existing `duration()`
  vector to convert to `chronon` granules.

- chronon:

  A time granule expression representing the chronon, evaluated in the
  context of `calendar`. Use unquoted expressions like `month(1L)` or
  `day(1L)`. Chronons from a specific calendar can also be used (e.g.
  `cal_gregorian$month(1L)`). Defaults to the time chronon of the input
  `data` (`time_chronon(data)`).

- discrete:

  Logical. If `TRUE` (default), returns integer durations always
  rounding down (discrete time model). If `FALSE`, returns fractional
  durations (continuous time model).

- calendar:

  Calendar system used to evaluate `chronon`. Defaults to
  `time_calendar(data)` for existing time objects. Common options
  include
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_gregorian.md)
  and
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_isoweek.md).

## Value

A `mixtime` vector containing an `mt_duration` vector.

## See also

- [`new_duration_fn()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_duration_fn.md)
  for creating reusable duration functions

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
# A duration of 3 months
duration(3L, cal_gregorian$month(1L))
#> <mixtime[1]>
#> [1] 3 months

# A vector of durations in days
duration(1:7, cal_gregorian$day(1L))
#> <mixtime[7]>
#> [1] 1 day  2 days 3 days 4 days 5 days 6 days 7 days

# Convert a duration of 4 days into weeks
duration(days(4), cal_isoweek$week(1L), discrete = FALSE)
#> <mixtime[1]>
#> [1] 0.5714286 weeks
duration(days(4), cal_isoweek$week(1L), discrete = TRUE)
#> <mixtime[1]>
#> [1] 0 weeks
```
