# Obtain the chronon of a time object

This S7 generic function extracts the chronon (the smallest time unit)
from a time object, such as continuous time or cyclical time
representations.

## Usage

``` r
time_chronon(x, ...)
```

## Arguments

- x:

  A time object (e.g., [base::Date](https://rdrr.io/r/base/Dates.html),
  [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html),
  [`linear_time()`](linear_time.md), etc.)

- ...:

  Additional arguments for methods.

## Value

A time unit object representing the chronon (e.g., `tu_day(1L)`)

## Examples

``` r
# The chronon of a Date object is 1 day
time_chronon(Sys.Date())
#> <mixtime::tu_day> int 1

# The chronon of a POSIXct object is 1 second
time_chronon(Sys.time())
#> <mixtime::tu_second> int 1

# The chronon of a continuous time year and month is 1 month
time_chronon(yearmonth(Sys.Date()))
#> <mixtime::tu_month> int 1

# The common chronon of a mixed time object is the finest chronon
time_chronon(c(yearmonth(Sys.Date()), Sys.Date()))
#> <mixtime::tu_day> int 1
```
