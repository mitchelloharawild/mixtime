# Obtain the cycle of a time object

This S7 generic function extracts the cycle (the cyclical time granule)
from a time object, such as cyclical time representations.

## Usage

``` r
time_cycle(x, ...)
```

## Arguments

- x:

  A time object (e.g., [base::Date](https://rdrr.io/r/base/Dates.html),
  [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html),
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md),
  etc.)

- ...:

  Additional arguments for methods.

## Value

A time
[`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md)
object representing the cycle of each value (e.g. `weeks(1L)`), or `NA`
if the object has no cyclical component.

## Examples

``` r

# Non-cyclical objects return NA
time_cycle(Sys.Date())
#> <mixtime[1]>
#> [1] NA

# The cycle of a cyclical time object
time_cycle(month_of_year(Sys.Date()))
#> <mixtime[1]>
#> [1] 1 year
```
