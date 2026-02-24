# Linear time helper functions

Convenience functions for creating common linear time representations.
These functions work with different calendar systems and adapt based on
the input data's calendar.

## Usage

``` r
year(data, discrete = TRUE, calendar = time_calendar(data), ...)

yearquarter(data, discrete = TRUE, calendar = time_calendar(data), ...)

yearmonth(data, discrete = TRUE, calendar = time_calendar(data), ...)

yearmonthday(data, discrete = TRUE, calendar = time_calendar(data), ...)

yearweek(data, discrete = TRUE, calendar = time_calendar(data), ...)
```

## Arguments

- data:

  A vector of time points (e.g.
  [base::Date](https://rdrr.io/r/base/Dates.html),
  [base::POSIXt](https://rdrr.io/r/base/DateTimeClasses.html))

- discrete:

  If `TRUE`, the number of chronons since Unix epoch that `.data` falls
  into is returned as an integer. If `FALSE`, a fractional number of
  chronons is returned (analagous to time using a continuous time
  model).

- calendar:

  A calendar used to evaluate the time units. Defaults to the calendar
  of the input data. Common options include
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md)
  and
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md).

- ...:

  Additional arguments for
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md),
  such as `tz` for timezones.

## Value

A `mt_linear` time vector.

## Details

These functions create linear time representations with different
chronons and granules:

- `year()`: Represents time in whole years. The chronon is one year.

- `yearquarter()`: Represents time in quarters, grouped by year. The
  chronon is one quarter, with years as the granule.

- `yearmonth()`: Represents time in months, grouped by year. The chronon
  is one month, with years as the granule.

- `yearweek()`: Represents time in weeks, grouped by year. The chronon
  is one week, with years as the granule. Defaults to ISO 8601 week
  calendar.

## Calendar flexibility

These functions adapt to the calendar system of the input data. For
example:

- `year("2025-12-29")` returns a Gregorian year

- `year(yearweek("2025-12-29"))` returns an ISO week-based year

You can also explicitly specify a calendar using the `calendar`
argument:

    year(yearweek("2025-12-29"), calendar = cal_isoweek)

## Custom time representations

For more complex time structures, use
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
or
[`new_linear_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_linear_time_fn.md)
to create custom representations with any combination of chronons and
granules.

## See also

- [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
  for creating custom linear time representations

- [`new_linear_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_linear_time_fn.md)
  for creating reusable linear time functions

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
# Gregorian year
year(Sys.Date())
#> <mixtime[1]>
#> [1] 2026
year(Sys.Date(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-14.8%

# ISO week-based year
year(yearweek(Sys.Date()))
#> <mixtime[1]>
#> [1] 2034

# Year-quarter
yearquarter(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-Q1
yearquarter(Sys.Date(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-Q1-59.2%

# Year-month
yearmonth(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-Feb
yearmonth(Sys.Date(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-Feb-82.1%

# Year-week (ISO 8601)
yearweek(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-W9
yearweek(0:52)
#> <mixtime[53]>
#>  [1] 1970-W1  1970-W2  1970-W3  1970-W4  1970-W5  1970-W6  1970-W7  1970-W8 
#>  [9] 1970-W9  1970-W10 1970-W11 1970-W12 1970-W13 1970-W14 1970-W15 1970-W16
#> [17] 1970-W17 1970-W18 1970-W19 1970-W20 1970-W21 1970-W22 1970-W23 1970-W24
#> [25] 1970-W25 1970-W26 1970-W27 1970-W28 1970-W29 1970-W30 1970-W31 1970-W32
#> [33] 1970-W33 1970-W34 1970-W35 1970-W36 1970-W37 1970-W38 1970-W39 1970-W40
#> [41] 1970-W41 1970-W42 1970-W43 1970-W44 1970-W45 1970-W46 1970-W47 1970-W48
#> [49] 1970-W49 1970-W50 1970-W51 1970-W52 1970-W53
```
