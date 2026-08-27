# Default parsing format strings for chronons

Provides candidate format strings for parsing text into a given chronon
(finest time granule), for use as the `format` argument of
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md).
Dispatches the same way as
[`chronon_format_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)/[`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md),
but returns every common format instead of a single default, so
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)
can try each in turn and keep whichever parses the most values. Methods
should build their return value with
[`parse_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/parse_format.md).

## Usage

``` r
chronon_parse_linear(x, cal = time_calendar(x), ...)

chronon_parse_cyclical(x, y, ...)
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

A character vector of format templates, ordered from most to least
common, built with
[`parse_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/parse_format.md).
The first element typically matches
[`chronon_format_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
(for `chronon_parse_linear()`) or
[`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
(for `chronon_parse_cyclical()`) for the same chronon.

## See also

[`parse_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/parse_format.md)
for building candidate format strings (what these methods are built
from),
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)
for using the result as candidate formats,
[`chronon_format_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)/[`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
for the single default format these are based on.

## Examples

``` r
chronon_parse_linear(cal_gregorian$year(1L))
#> [1] "{lin(year)}"
chronon_parse_linear(cal_gregorian$month(1L))
#> [1] "[[:space:]]*{lin(year)}(?:[-/,._[:space:]]*(?i:months?|mth|m)[-/,._[:space:]]*|[-/,._[:space:]]+|(?=[[:alpha:]])){cyc(month,year)}[[:space:]]*"
#> [2] "[[:space:]]*(?:(?i:months?|mth|m)[-/,._[:space:]]*)?{cyc(month,year)}[-/,._[:space:]]+{lin(year)}[[:space:]]*"                                 
#> attr(,"regex")
#> [1] TRUE
chronon_parse_linear(cal_gregorian$day(1L))
#> [1] "{lin(year)}[-/,\\s]+{cyc(month,year)}[-/,\\s]+{cyc(day,month)}(?:st|nd|rd|th)?"
#> [2] "{cyc(day,month)}(?:st|nd|rd|th)?[-/,\\s]+{cyc(month,year)}[-/,\\s]+{lin(year)}"
#> [3] "{cyc(month,year)}[-/,\\s]+{cyc(day,month)}(?:st|nd|rd|th)?[-/,\\s]+{lin(year)}"
#> attr(,"regex")
#> [1] TRUE
chronon_parse_linear(cal_isoweek$day(1L))
#> [1] "[[:space:]]*{lin(year)}[-/,._[:space:]]*(?i:weeks?|wk|w)[-/,._[:space:]]*{cyc(week,year)}[-/,._[:space:]]+{cyc(day,week)}[[:space:]]*"
#> [2] "[[:space:]]*{cyc(day,week)}[-/,._[:space:]]+(?i:weeks?|wk|w)[-/,._[:space:]]*{cyc(week,year)}[-/,._[:space:]]+{lin(year)}[[:space:]]*"
#> attr(,"regex")
#> [1] TRUE

chronon_parse_cyclical(cal_gregorian$month(1L), cal_gregorian$year(1L))
#> [1] "{cyc(month,year)}"
#> attr(,"regex")
#> [1] FALSE
chronon_parse_cyclical(cal_isoweek$day(1L), cal_isoweek$week(1L))
#> [1] "{cyc(day,week)}"
#> attr(,"regex")
#> [1] FALSE
```
