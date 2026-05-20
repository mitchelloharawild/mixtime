# Constructor for mixtime time vectors

Creates a `mixtime` time vector at a specific time point, with a
specified chronon and optional cycle. The `chronon` defines the smallest
indivisible time granule for the time vector, while the `cycle` allows
for cyclical time representations (e.g. day-of-week, month-of-year).

## Usage

``` r
new_time(x = integer(), chronon = mt_unit(1L), cycle = NULL, class = NULL)
```

## Arguments

- x:

  A numeric vector of time points, integers for discrete time or doubles
  for continuous time.

- chronon:

  A time granule object representing the smallest indivisible time
  granule (chronon) for the time vector (e.g. `cal_gregorian$day(1L)`).

- cycle:

  An optional time granule object representing the cycle for cyclical
  time (e.g. `cal_gregorian$week(1L)` for day-of-week). If not provided,
  the time vector will be treated as linear time.

- class:

  An optional character vector of additional S3 classes to assign to the
  resulting time vector. This allows for further subclassing of
  `mt_time` for specific time types (e.g. linear, cyclical, durations,
  etc.).

## Value

A `mt_time` vector representing the time points in `x` according to the
specified `chronon` and `cycle`.

## Examples

``` r
# Create a continuous mixtime time vector for today
new_time(
  as.double(Sys.Date()),
  chronon = cal_gregorian$day(1L, tz = Sys.timezone()),
  class = "mt_linear"
)
#> <mt_linear[1]>
#> [1] 2026-05-20 0.0%

# Create a discrete mixtime time vector for the current date and time
new_time(
  as.integer(Sys.time()),
  chronon = cal_gregorian$second(1L, tz = Sys.timezone()),
  class = "mt_linear"
)
#> <mt_linear[1]>
#> [1] 2026-05-20 14:32:46

# Create a discrete mixtime time vector for the time of day (cyclical time)
new_time(
  as.integer(Sys.time()), 
  chronon = cal_gregorian$second(1L, tz = Sys.timezone()), 
  cycle = cal_gregorian$day(1L, tz = Sys.timezone()),
  class = "mt_cyclical"
)
#> <mt_cyclical[1]>
#> [1] 14:32:46
```
