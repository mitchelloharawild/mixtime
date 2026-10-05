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

## Details

For [base::Date](https://rdrr.io/r/base/Dates.html) and
[base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) objects the
chronon is identified from the greatest common divisor of the intervals
between observations, in the same way as a tsibble's interval. For
example, hourly datetimes have a 1 hour chronon rather than 1 second. A
multiple of a unit (e.g. 3 hours) is only used if every observation
falls on a boundary of that chronon; otherwise a single unit is used so
that no information is lost when converting.

## Examples

``` r

# The chronon of a Date or POSIXct object is identified from the interval
# between observations (as with tsibble's interval)
time_chronon(Sys.Date() + 0:2)
#> <mixtime[3]>
#> [1] 1 day 1 day 1 day
time_chronon(as.POSIXct("2024-01-01", tz = "UTC") + 3600 * 0:2)
#> <mixtime[3]>
#> [1] 1 hour 1 hour 1 hour

# Without an interval, the chronon of a Date is 1 day and a POSIXct is 1 second
time_chronon(Sys.Date())
#> <mixtime[1]>
#> [1] 1 day
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
