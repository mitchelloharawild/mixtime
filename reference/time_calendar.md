# Obtain the calendar of a time object

This S7 generic function extracts the calendar system from a time
object. The calendar defines the collection of time units (years,
months, days, etc.) used to interpret the time representation.

## Usage

``` r
time_calendar(x, ...)
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

A calendar object (e.g., `cal_gregorian`, `cal_isoweek`)

## Examples

``` r
# The calendar of a Date object is the Gregorian calendar
time_calendar(Sys.Date())
#> <cal_gregorian>
#> Time units:
#>   - year
#>   - quarter
#>   - month
#>   - day
#>   - hour
#>   - minute
#>   - second
#>   - millisecond

# The calendar of a POSIXct object is also Gregorian
time_calendar(Sys.time())
#> <cal_gregorian>
#> Time units:
#>   - year
#>   - quarter
#>   - month
#>   - day
#>   - hour
#>   - minute
#>   - second
#>   - millisecond

# The calendar of a yearweek object is the ISO week calendar
time_calendar(yearweek(Sys.Date()))
#> <cal_isoweek>
#> Time units:
#>   - year
#>   - week
#>   - day
#>   - hour
#>   - minute
#>   - second
#>   - millisecond

# A mixed time object returns a list of calendars
time_calendar(c(yearmonth(Sys.Date()), Sys.Date()))
#> <cal_gregorian>
#> Time units:
#>   - year
#>   - quarter
#>   - month
#>   - day
#>   - hour
#>   - minute
#>   - second
#>   - millisecond
```
