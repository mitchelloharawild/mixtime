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

A data frame containing information about timezone transitions in the
specified range.

## Examples

``` r
# Get all DST transitions in 2024 for New York
tz_transitions(
  as.POSIXct("2024-01-01", tz = "America/New_York"),
  as.POSIXct("2024-12-31", tz = "America/New_York")
)
#> [1] time          offset_before offset_after 
#> <0 rows> (or 0-length row.names)
```
