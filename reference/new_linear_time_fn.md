# Linear time function factory

`new_linear_time_fn()` creates a linear time function for a specified
chronon. A chronon is the smallest indivisible time granule (e.g., days,
hours).

## Usage

``` r
new_linear_time_fn(chronon, default_calendar = cal_gregorian)
```

## Arguments

- chronon:

  A bare call for a time granule object representing the chronon (e.g.,
  `day(1)`)

- default_calendar:

  A default calendar used to find the time units for conversion if they
  don't exist in the calendar of the input data (e.g., `cal_isoweek` for
  week chronons to work with gregorian calendar inputs).

## Value

A function used to create linear time points with a specific chronon.

## Examples

``` r

# Linear time with 1 month granules as the chronon
ym <- new_linear_time_fn(month(1L))
ym(Sys.Date())
#> <mixtime[1]>
#> [1] 2026 May

# Linear time with 1 day granules as the chronon
yd <- new_linear_time_fn(day(1L))
yd(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-05-18

# Linear time with 1 week granules as the chronon, using the ISO week calendar
yw <- new_linear_time_fn(week(1L), default_calendar = cal_isoweek)
yw(Sys.Date())
#> <mixtime[1]>
#> [1] 2026 W21

# Linear time with 1 hour granules as the chronon
ymd_h <- new_linear_time_fn(hour(1L))
ymd_h(Sys.time())
#> <mixtime[1]>
#> [1] 2026-05-18 08h
```
