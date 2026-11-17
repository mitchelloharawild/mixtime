# Format mixtime vectors

Formats a mixtime vector as a character vector, using a glue-style
format string of `{lin(...)}`/`{cyc(...)}` (or, for durations,
`{dur(...)}`) tokens tailored to the vector's chronon and cycle. If
`format` is omitted, a sensible default is derived automatically. See
[`vignette("time-format-strings")`](https://pkg.mitchelloharawild.com/mixtime/dev/articles/time-format-strings.md)
for the format string syntax.

## Usage

``` r
# S4 method for class 'mt_time'
format(x, ..., attr = TRUE)
# S4 method for class 'mt_cyclical_free'
format(x, format = NULL, ..., attr = TRUE)

## S7 method for class <mixtime::mt_cyclical_free>
format(x, format = NULL, ..., attr = TRUE)
```

## Arguments

- x:

  A mixtime vector.

- ...:

  Additional arguments for methods, including `format` for time points.

- attr:

  If `TRUE` (default), append attribute information (e.g. timezone) to
  the default format.

- format:

  A glue-style format string, defaulting to one derived automatically
  for `x`. For time points it is passed via `...`.

## Value

A character vector the same length as `x`.

## Underspecified cyclical time

Underspecified cyclical time (e.g. `month_of_year(3)`) has no position
on the time line, so only `{cyc(...)}` tokens can be used. Attribute
tokens such as `{tz(.time)}` render as empty text.

## Examples

``` r
format(yearmonth("2024-03"))
#> [1] "2024 Mar"
format(date("2024-03-15"), format = "{cyc(day(1L), month(1L))} {lin(year(1L))}")
#> [1] "15 2024"
format(month_of_year(3))
#> [1] "Mar"
```
