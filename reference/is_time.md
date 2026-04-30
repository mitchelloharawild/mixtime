# Check the time type of values

Test whether elements of a `mixtime` vector are linear, cyclical, or
durations.

## Usage

``` r
is_time_linear(x, ...)

is_time_cyclical(x, ...)

is_time_duration(x, ...)
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
the type of time represented by each element.

## Examples

``` r
t <- c(yearmonth(0), month_of_year(0), months(0L))
is_time_linear(t)
#> [1]  TRUE FALSE FALSE
is_time_cyclical(t)
#> [1] FALSE  TRUE FALSE
is_time_duration(t)
#> [1] FALSE FALSE  TRUE
```
