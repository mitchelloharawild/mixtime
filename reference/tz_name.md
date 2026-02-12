# Extract timezone from an object

Generic function to extract the timezone from objects that have timezone
information.

## Usage

``` r
tz_name(x, ...)
```

## Arguments

- x:

  An object with timezone information.

- ...:

  Additional arguments passed to methods.

## Value

A character string representing the timezone (e.g., "America/New_York",
"UTC").

## Examples

``` r
tz_name(Sys.time())
#> [1] "UTC"
tz_name(as.POSIXct("2024-06-15 12:00:00", tz = "America/New_York"))
#> [1] "America/New_York"
```
