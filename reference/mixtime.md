# Create a mixtime vector

A mixtime is a vector which describes a point in time. It uses a
calendar definition to translate a vector of numbers into a point in
time.

## Usage

``` r
mixtime(
  data,
  chronon = time_chronon(data),
  cycle = time_cycle(data),
  discrete = TRUE
)
```

## Arguments

- data:

  A vector of time values. This can be a character vector (e.g.
  "2024-01-01"), a numeric vector (e.g. seconds since epoch), or a time
  class (e.g. Date, POSIXct, yearmonth, etc.).

- chronon:

  A time unit object representing the smallest indivisible time unit
  (chronon) for the mixtime. This is used to interpret the numeric
  values in `data` and to define the time resolution of the mixtime. If
  not provided, it will be inferred from `data`.

- cycle:

  An optional time unit object representing the cycle for cyclical time.
  This is used to define the repeating cycle for cyclical time
  representations (e.g. day-of-week, month-of-year). If not provided,
  the mixtime will be treated as linear time.

- discrete:

  A logical indicating whether the time values should be treated as
  discrete (integer) or continuous (fractional). This affects how
  numeric values are interpreted and how time arithmetic is performed.
  The default is `TRUE` (discrete).

## Value

A `mixtime` object representing the time values in `data` according to
the specified `chronon` and `cycle`.

## Examples

``` r
# Create a mixtime for today
mixtime(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-04-08

# Create a mixtime for the current date and time
mixtime(Sys.time())
#> <mixtime[1]>
#> [1] 2026-04-08 00:48:49

# Convert time from tsibble units to mixtime
mixtime(tsibble::yearmonth("2024 Jan"))
#> <mixtime[1]>
#> [1] 2024 Jan

# Create a mixtime for the time of day (cyclical time)
mixtime(Sys.time(), cycle = cal_gregorian$day(1L))
#> <mixtime[1]>
#> [1] 00:48:50

# Specify a timezone for the chronon
mixtime(Sys.time(), chronon = cal_gregorian$second(1L, tz = Sys.timezone()))
#> <mixtime[1]>
#> [1] 2026-04-08 00:48:50
mixtime(Sys.time(), chronon = cal_gregorian$second(1L, tz = "Pacific/Honolulu"))
#> <mixtime[1]>
#> [1] 2026-04-07 14:48:50 HST
mixtime(Sys.time(), chronon = cal_gregorian$second(1L, tz = "Australia/Melbourne"))
#> <mixtime[1]>
#> [1] 2026-04-08 10:48:50 AEST

# Dates (and all granularities) can have timezones
mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = Sys.timezone()))
#> <mixtime[1]>
#> [1] 2026-04-08
mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = "Pacific/Honolulu"))
#> <mixtime[1]>
#> [1] 2026-04-07 HST
mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = "Australia/Melbourne"))
#> <mixtime[1]>
#> [1] 2026-04-08 AEST

# Continuous time tracks progress within the chronon
mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = Sys.timezone()), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-04-08 3.4%

# Mixtime can combine different granularities and timezones in a vector
now <- Sys.time()
c(
  # Datetime (second chronon) in UTC
  mixtime(now),
  # Date (minute chronon) in local timezone
  mixtime(now, chronon = cal_gregorian$minute(1L, tz = Sys.timezone())),
  # Month (month chronon) in UTC
  mixtime(now, chronon = cal_gregorian$month(1L))
)
#> <mixtime[3]>
#> [1] 2026-04-08 00:48:50 2026-04-08 00:48    2026 Apr           
```
