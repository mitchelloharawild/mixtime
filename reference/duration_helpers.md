# Duration helper functions

Convenience functions for creating duration vectors of common time
units. Each function wraps
[`new_duration_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_duration_fn.md)
for its respective chronon.

## Usage

``` r
years(data, calendar = time_calendar(data), ...)

quarters(data, calendar = time_calendar(data), ...)

months(data, calendar = time_calendar(data), ...)

weeks(data, calendar = time_calendar(data), ...)

days(data, calendar = time_calendar(data), ...)

hours(data, calendar = time_calendar(data), ...)

minutes(data, calendar = time_calendar(data), ...)

seconds(data, calendar = time_calendar(data), ...)

milliseconds(data, calendar = time_calendar(data), ...)
```

## Arguments

- data:

  A time vector of duration magnitudes.

- calendar:

  Calendar system used to evaluate `chronon`. Defaults to
  `time_calendar(data)` for existing time objects. Common options
  include
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md)
  and
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md).

- ...:

  Additional arguments passed to the chronon (e.g. `tz` for timezones).

## Value

A `mixtime` vector containing an `mt_duration` vector.

## See also

- [`new_duration_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_duration_fn.md)
  for creating custom duration functions

- [`duration()`](https://pkg.mitchelloharawild.com/mixtime/reference/duration.md)
  for creating duration vectors directly

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
years(3L)
#> <mixtime[1]>
#> [1] 3 years
quarters(2L)
#> <mixtime[1]>
#> [1] 2 quarters
months(6L)
#> <mixtime[1]>
#> [1] 6 months
weeks(4L)
#> <mixtime[1]>
#> [1] 4 weeks
days(7L)
#> <mixtime[1]>
#> [1] 7 days
hours(12L)
#> <mixtime[1]>
#> [1] 12 hours
minutes(30L)
#> <mixtime[1]>
#> [1] 30 minutes
seconds(45L)
#> <mixtime[1]>
#> [1] 45 seconds
milliseconds(500L)
#> <mixtime[1]>
#> [1] 500 milliseconds
```
