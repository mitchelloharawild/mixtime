# Round, floor and ceiling transformations for time objects

A family of helpers to round date/time objects to a specified time
granule such as second, minute, hour, or day. These functions preserve
the input time class, as rounded by the attributes of the `unit`.

## Usage

``` r
round_time(x, unit, ...)

ceiling_time(x, unit, ...)

floor_time(x, unit, ...)
```

## Arguments

- x:

  A date/time object to be rounded. Accepted types include Date,
  POSIXct, POSIXlt and other objects that inherit from POSIXt. The
  returned object will be of the same class as the input.

- unit:

  A time unit (or object coercible to a time unit, e.g. "day").

- ...:

  Additional arguments passed to specific implementations.

## Value

An object of the same class as x with its time components adjusted to
the requested unit.

## See also

base::round,
[lubridate::round_date](https://lubridate.tidyverse.org/reference/round_date.html)

## Examples

``` r
# Round POSIXct to the nearest minute (preserving tz)
t <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
round_time(t, unit = cal_gregorian$minute(1L))
#> [1] "2020-01-01 12:35:00 UTC"

# Floor to the nearest hour
floor_time(t, unit = cal_gregorian$hour(1L))
#> [1] "2020-01-01 12:00:00 UTC"

# Ceiling a Date (treated as midnight-of-day rounding)
d <- as.Date("2020-01-01")
ceiling_time(d, unit = cal_gregorian$month(1L))
#> [1] "2020-02-01"
```
