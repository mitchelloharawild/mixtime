# Cyclical time function factory

`new_cyclical_time_fn()` creates a cyclical time function for a
specified chronon and cycle. The cycle is the larger time granule that
defines the time period over which the chronon loops (e.g., a week). The
chronon is the smaller time granule that iterates within each cycle
(e.g., a day). Combined, these two granules form a cyclical time
relationship (e.g., day of the week).

## Usage

``` r
new_cyclical_time_fn(chronon, cycle, default_calendar = cal_gregorian)
```

## Arguments

- chronon:

  A time granule object representing the chronon (e.g., `day(1L)`)

- cycle:

  A time granule object representing the cycle (e.g., `week(1L)`)

- default_calendar:

  A default calendar used to find the time units for conversion if they
  don't exist in the calendar of the input data (e.g., `cal_isoweek`)

## Value

A function used to create cyclical time points with a specific chronon
and cycle.

## Examples

``` r

day_of_week <- new_cyclical_time_fn(day(1L), week(1L), default_calendar = cal_isoweek)
day_of_week(Sys.Date())
#> <mixtime[1]>
#> [1] Thu

month_of_year <- new_cyclical_time_fn(month(1L), year(1L))
month_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] Apr
```
