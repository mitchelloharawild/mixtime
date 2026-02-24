# Cyclical time helpers

Helper functions for creating cyclical time representations. These
functions create time objects that repeat within a larger time cycle,
useful for identifying seasonal patterns or positions within a calendar
period.

## Usage

``` r
month_of_year(data, discrete = TRUE, calendar = time_calendar(data), ...)

day_of_year(data, discrete = TRUE, calendar = time_calendar(data), ...)

day_of_month(data, discrete = TRUE, calendar = time_calendar(data), ...)

day_of_week(data, discrete = TRUE, calendar = time_calendar(data), ...)

week_of_year(data, discrete = TRUE, calendar = time_calendar(data), ...)
```

## Arguments

- data:

  Another object to be coerced into the specified cyclical time.

- discrete:

  If `TRUE`, the position within the cycle that `data` falls into is
  returned as an integer. If `FALSE`, a fractional position is returned
  (analagous to time using a continuous time model).

- calendar:

  A calendar object specifying the calendar system to use.

- ...:

  Additional arguments for
  [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md),
  such as `tz` for timezones.

## Gregorian cyclical time representations

- `month_of_year()`: Represents the month position within a year (1-12).
  The chronon is one month, cycling within a year.

- `day_of_year()`: Represents the day position within a year (1-365 or
  1-366 for leap years). The chronon is one day, cycling within a year.

- `day_of_month()`: Represents the day position within a month (1-28,
  1-29, 1-30, or 1-31 depending on the month). The chronon is one day,
  cycling within a month.

## ISO 8601 cyclical time representations

- `day_of_week()`: Represents the day position within a week (1-7) using
  the ISO 8601 standard where weeks start on Monday.

- `week_of_year()`: Represents the week position within a year (1-52 or
  1-53) using the ISO 8601 week numbering system.

## Custom cyclical time representations

You can create custom cyclical time representations using
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md)
with any of the supported time units (see
[calendar_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md)
and
[calendar_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)).

For example, to create a representation for day of the month:

    day_of_month <- new_cyclical_time_fn(
      chronon = day(1L), cycle = month(1L),
      fallback_calendar = cal_gregorian
    )

## See also

[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md)
for creating custom cyclical time vectors,
[linear_time_helpers](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
for linear time helper functions,
[`new_cyclical_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_cyclical_time_fn.md)
for creating cyclical time helper functions

## Examples

``` r
month_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] Feb
day_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] 55
day_of_week(Sys.Date())
#> <mixtime[1]>
#> [1] Tue
day_of_week(as.Date("2025-12-15") + 0:6)
#> <mixtime[7]>
#> [1] Mon Tue Wed Thu Fri Sat Sun
```
