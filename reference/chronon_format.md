# Default formatting strings for chronons

Provides default linear time formatting strings for a given chronon
(time unit). The format strings use placeholders like `{year}`,
`{month}`, `{day}`, etc., that can be interpolated with actual values.

## Usage

``` r
chronon_format_linear(x, cal = time_calendar(x), ...)

chronon_format_cyclical(x, y, ...)
```

## Arguments

- x:

  A chronon (time unit) object.

- cal:

  The calendar of the chronon, used to disambiguate format strings for
  time units that are shared across calendars (e.g. `cal_gregorian$day`
  and `cal_isoweek$day`).

- ...:

  Additional arguments for methods.

- y:

  A chronon (time unit) for the cycle size.

## Value

A character string containing the default format template for the
chronon.

## Examples

``` r
chronon_format_linear(cal_gregorian$year(1L))
#> [1] "{lin(year)}"
chronon_format_linear(cal_gregorian$month(1L))
#> [1] "{lin(year)} {cyc(month,year,label=TRUE,abbreviate=TRUE)}"
chronon_format_linear(cal_gregorian$day(1L))
#> [1] "{lin(year)}-{cyc(month,year)}-{cyc(day,month)}"
chronon_format_linear(cal_isoweek$day(1L))
#> [1] "{lin(year)}-W{cyc(week,year)}-{cyc(day,week,label=TRUE)}"

chronon_format_cyclical(cal_gregorian$month(1L), cal_gregorian$year(1L))
#> [1] "{cyc(month,year,label=TRUE,abbreviate=TRUE)}"
chronon_format_cyclical(cal_gregorian$day(1L), cal_gregorian$month(1L))
#> [1] "D{cyc(day,month)}"
chronon_format_cyclical(cal_isoweek$day(1L), cal_isoweek$week(1L))
#> [1] "{cyc(day,week,label=TRUE)}"
chronon_format_cyclical(cal_isoweek$week(1L), cal_isoweek$year(1L))
#> [1] "W{cyc(week,year)}"
```
