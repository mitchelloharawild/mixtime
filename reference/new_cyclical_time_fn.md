# Cyclical time function factory

`new_cyclical_time_fn()` creates a cyclical time function for a
specified chronon and cycle. The cycle is the larger time unit that
defines the time period over which the chronon loops (e.g., a week). The
chronon is the smaller time unit that iterates within each cycle (e.g.,
a day). Combined, these two units form a cyclical time point (e.g., day
of the week).

## Usage

``` r
new_cyclical_time_fn(chronon, cycle, fallback_calendar = cal_gregorian)
```

## Arguments

- chronon:

  A time unit object representing the chronon (e.g., `day(1L)`)

- cycle:

  A time unit object representing the cycle (e.g., `week(1L)`)

- fallback_calendar:

  A fallback calendar used to find the time units for conversion if they
  don't exist in the calendar of the input data (e.g., `cal_isoweek`)

## Value

A function used to create cyclical time points.

## Examples

``` r
day_of_week <- new_cyclical_time_fn(day(1L), week(1L), fallback_calendar = cal_isoweek)
day_of_week(Sys.Date())
#> <mixtime[1]>
#> [1] Tue

month_of_year <- new_cyclical_time_fn(month(1L), year(1L))
month_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] Feb
```
