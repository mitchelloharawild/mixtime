# Linear time function factory

`new_linear_time_fn()` creates a linear time function for a specified
chronon and granules. Granules are larger time units that define the
structure of time (e.g., years, months), while the chronon is the
smallest indivisible time unit (e.g., days, hours).

## Usage

``` r
new_linear_time_fn(
  chronon,
  granules = list(),
  fallback_calendar = cal_gregorian
)
```

## Arguments

- chronon:

  A time unit object representing the chronon (e.g., `day(1)`)

- granules:

  A list of time unit objects representing the granules (e.g.,
  `list(year(1), month(1))`)

- fallback_calendar:

  A fallback calendar used to find the time units for conversion if they
  don't exist in the calendar of the input data (e.g., `cal_isoweek`)

## Value

A function used to create linear time points.

## Examples

``` r
# A year-month time representation with months as the chronon
ym <- new_linear_time_fn(month(1L), list(year(1L)))
ym(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-Feb

# A year-quarter-month time representation with months as the chronon
yqm <- new_linear_time_fn(month(1L), list(year(1L), quarter(1L)))
yqm(1:100)
#> <mixtime[100]>
#>   [1] 1970-Q1-M1 1970-Q1-M2 1970-Q2-M0 1970-Q2-M1 1970-Q2-M2 1970-Q3-M0
#>   [7] 1970-Q3-M1 1970-Q3-M2 1970-Q4-M0 1970-Q4-M1 1970-Q4-M2 1971-Q1-M0
#>  [13] 1971-Q1-M1 1971-Q1-M2 1971-Q2-M0 1971-Q2-M1 1971-Q2-M2 1971-Q3-M0
#>  [19] 1971-Q3-M1 1971-Q3-M2 1971-Q4-M0 1971-Q4-M1 1971-Q4-M2 1972-Q1-M0
#>  [25] 1972-Q1-M1 1972-Q1-M2 1972-Q2-M0 1972-Q2-M1 1972-Q2-M2 1972-Q3-M0
#>  [31] 1972-Q3-M1 1972-Q3-M2 1972-Q4-M0 1972-Q4-M1 1972-Q4-M2 1973-Q1-M0
#>  [37] 1973-Q1-M1 1973-Q1-M2 1973-Q2-M0 1973-Q2-M1 1973-Q2-M2 1973-Q3-M0
#>  [43] 1973-Q3-M1 1973-Q3-M2 1973-Q4-M0 1973-Q4-M1 1973-Q4-M2 1974-Q1-M0
#>  [49] 1974-Q1-M1 1974-Q1-M2 1974-Q2-M0 1974-Q2-M1 1974-Q2-M2 1974-Q3-M0
#>  [55] 1974-Q3-M1 1974-Q3-M2 1974-Q4-M0 1974-Q4-M1 1974-Q4-M2 1975-Q1-M0
#>  [61] 1975-Q1-M1 1975-Q1-M2 1975-Q2-M0 1975-Q2-M1 1975-Q2-M2 1975-Q3-M0
#>  [67] 1975-Q3-M1 1975-Q3-M2 1975-Q4-M0 1975-Q4-M1 1975-Q4-M2 1976-Q1-M0
#>  [73] 1976-Q1-M1 1976-Q1-M2 1976-Q2-M0 1976-Q2-M1 1976-Q2-M2 1976-Q3-M0
#>  [79] 1976-Q3-M1 1976-Q3-M2 1976-Q4-M0 1976-Q4-M1 1976-Q4-M2 1977-Q1-M0
#>  [85] 1977-Q1-M1 1977-Q1-M2 1977-Q2-M0 1977-Q2-M1 1977-Q2-M2 1977-Q3-M0
#>  [91] 1977-Q3-M1 1977-Q3-M2 1977-Q4-M0 1977-Q4-M1 1977-Q4-M2 1978-Q1-M0
#>  [97] 1978-Q1-M1 1978-Q1-M2 1978-Q2-M0 1978-Q2-M1
yqm(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-Q1-M1

# A year-day time representation with days as the chronon
yd <- new_linear_time_fn(day(1L), list(year(1L)))
yd(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-55

# Gregorian date time with hourly precision
ymd_h <- new_linear_time_fn(hour(1L), list(year(1L), month(1L), day(1L)))
ymd_h(Sys.time())
#> <mixtime[1]>
#> [1] 2026-Feb-24-h2

# ISO-week-date calendar
ywd <- new_linear_time_fn(day(1L), list(year(1L), week(1L)), fallback_calendar = cal_isoweek)
ywd(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-W9-Tue
```
