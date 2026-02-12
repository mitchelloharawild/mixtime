# Get timezone offset

Returns the UTC offset for a given datetime in its specified timezone.

## Usage

``` r
tz_offset(x, ...)
```

## Arguments

- x:

  A time class coercible to POSIXt with an associated time zone.

- ...:

  Additional arguments passed to methods.

## Value

A numeric vector of offsets from UTC in the same chronon (e.g. seconds
for POSIXt, days for dates, etc.)

## Examples

``` r
tz_offset(Sys.time())
#> [1] 0
tz_offset(as.POSIXct("2024-06-15 12:00:00", tz = "America/New_York"))
#> [1] -14400
```
