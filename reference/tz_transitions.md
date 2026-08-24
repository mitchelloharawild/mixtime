# Get timezone transitions

Returns all timezone transitions (e.g., daylight saving time changes)
that occur between two datetimes. The timezone is taken from the start
datetime.

## Usage

``` r
tz_transitions(start, end)
```

## Arguments

- start:

  A POSIXct datetime object or something coercible to POSIXct,
  representing the start of the time range. The timezone is extracted
  from this object.

- end:

  A POSIXct datetime object or something coercible to POSIXct,
  representing the end of the time range.

## Value

A data frame with columns:

- `time`: A `mixtime` linear time point (continuous, UTC seconds) giving
  the instant of the transition.

- `offset_before`, `offset_after`: `mixtime` durations (UTC seconds)
  giving the UTC offset immediately before and after the transition.

## Examples

``` r
# Get all DST transitions in 2024 for New York
tz_transitions(
  as.POSIXct("2024-01-01", tz = "America/New_York"),
  as.POSIXct("2024-12-31", tz = "America/New_York")
)
#>                       time    offset_before     offset_after
#> 1 2024-03-10 07:00:00 0.0% -18000.0 seconds -14400.0 seconds
#> 2 2024-11-03 06:00:00 0.0% -14400.0 seconds -18000.0 seconds
```
