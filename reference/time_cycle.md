# Obtain the cycle of a time object

This S7 generic function extracts the cycle (the cyclical time unit)
from a time object, such as cyclical time representations.

## Usage

``` r
time_cycle(x, ...)
```

## Arguments

- x:

  A time object (e.g., [base::Date](https://rdrr.io/r/base/Dates.html),
  [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html),
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md),
  etc.)

- ...:

  Additional arguments for methods.

## Value

A time unit object representing the cycle, or `NULL` if the object has
no cyclical component.

## Examples

``` r
# Non-cyclical objects return NULL
time_cycle(Sys.Date())
#> NULL

# The cycle of a cyclical time object
time_cycle(month_of_year(Sys.Date()))
#> <mixtime::tu_year> int 1
#>  @ tz: chr "UTC"
```
