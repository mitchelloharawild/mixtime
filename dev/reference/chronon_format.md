# Default formatting strings for chronons

Provides default linear time formatting strings for a given chronon
(finest time granule). The format strings use placeholders like
`{lin(year(1L))}`, `{cyc(month(1L), year(1L)}` and
`{cyc(day(1L), month(1L)}`, which are evaluated in the context of the
data's
[`time_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_calendar.md).

## Usage

``` r
chronon_format_linear(x, cal = time_calendar(x), ...)

chronon_format_cyclical(x, y, ...)
```

## Arguments

- x:

  A time granule for the chronon.

- cal:

  The calendar of the chronon, used to disambiguate suitable format
  strings for time units that are shared across calendars (e.g.
  `cal_gregorian$day` and `cal_isoweek$day`).

- ...:

  Additional arguments for methods.

- y:

  A time granule for the cycle

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
#> [1] "D{cyc(chronon_common(time_chronon(.time)), chronon_common(time_cycle(.time)))}"
chronon_format_cyclical(cal_isoweek$day(1L), cal_isoweek$week(1L))
#> [1] "{cyc(day,week,label=TRUE)}"
chronon_format_cyclical(cal_isoweek$week(1L), cal_isoweek$year(1L))
#> [1] "W{cyc(week,year)}"
```
