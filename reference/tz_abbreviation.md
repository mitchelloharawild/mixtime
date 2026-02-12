# Get timezone abbreviation

Returns the timezone abbreviation (e.g., "EST", "PDT") for a given
datetime in its specified timezone.

## Usage

``` r
tz_abbreviation(x)
```

## Arguments

- x:

  A POSIXct datetime object or something coercible to POSIXct. The
  timezone is extracted from this object.

## Value

A character vector of timezone abbreviations.

## Examples

``` r
tz_abbreviation(Sys.time())
#> [1] "UTC"
tz_abbreviation(as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York"))
#> [1] "EST"
```
