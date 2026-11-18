# Check the time type of values

Test whether elements of a `mixtime` vector are linear, cyclical,
underspecified cyclical, or durations.

## Usage

``` r
time_is_linear(x, ...)

time_is_cyclical(x, ...)

time_is_cyclical_free(x, ...)

time_is_duration(x, ...)
```

## Arguments

- x:

  A time object (typically a `mixtime` vector).

- ...:

  Additional arguments for methods.

## Value

A logical vector the same length as `x`.

## Details

These helpers return a logical vector the same length as `x` identifying
the type of time represented by each element. `time_is_cyclical()` and
`time_is_cyclical_free()` are distinct: located cyclical time (e.g.
[`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
of a real date) knows its enclosing cycle (which year), while
underspecified cyclical time (e.g. bare `month_of_year(3)`) doesn't. See
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md)
for the differences.

## Examples

``` r
t <- c(yearmonth(0), month_of_year(as.Date("2020-01-01")), months(0L))
time_is_linear(t)
#> [1]  TRUE FALSE FALSE
time_is_cyclical(t)
#> [1] FALSE  TRUE FALSE
time_is_duration(t)
#> [1] FALSE FALSE  TRUE

time_is_cyclical_free(month_of_year(3))
#> [1] TRUE
```
