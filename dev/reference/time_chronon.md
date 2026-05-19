# Obtain the chronon of a time object

This S7 generic function extracts the chronon (the smallest time
granule) from a time object, such as continuous time or cyclical time
representations.

## Usage

``` r
time_chronon(x, ...)
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
vector representing the chronon of each value (e.g., `days(1L)`).

## Examples

``` r

# The chronon of a Date object is 1 day
time_chronon(Sys.Date())
#> <mixtime[1]>
#> [1] 1 day

# The chronon of a POSIXct object is 1 second
time_chronon(Sys.time())
#> <mixtime[1]>
#> [1] 1 second

# The chronon of a continuous time year and month is 1 month
time_chronon(yearmonth(Sys.Date()))
#> <mixtime[1]>
#> [1] 1 month

# The common chronon of a mixed time object is the finest chronon
time_chronon(c(yearmonth(Sys.Date()), Sys.Date()))
#> <mixtime[2]>
#> [1] 1 month 1 day  
```
