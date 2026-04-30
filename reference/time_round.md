# Round, floor and ceiling transformations for time objects

A family of helpers to round date/time objects to a specified time
granule such as second, minute, hour, or day. These functions preserve
the input time class, as rounded by the attributes of the `granule`.

## Usage

``` r
time_round(x, granule, ...)

time_ceiling(x, granule, ...)

time_floor(x, granule, ...)
```

## Arguments

- x:

  A date/time object to be rounded. Accepted types include Date,
  POSIXct, POSIXlt and other objects that inherit from POSIXt. The
  returned object will be of the same class as the input.

- granule:

  A time granule (or object coercible to a time granule, e.g. "day").

- ...:

  Additional arguments passed to specific implementations.

## Value

An object of the same class as x with its time components adjusted to
the requested granule.

## See also

[base::round](https://rdrr.io/r/base/Round.html),
[lubridate::round_date](https://lubridate.tidyverse.org/reference/round_date.html)

## Examples

``` r
# Round POSIXct to the nearest minute (preserving tz)
t <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
time_round(t, granule = cal_gregorian$minute(1L))
#> [1] "2020-01-01 12:35:00 UTC"

# Floor to the nearest hour
time_floor(t, granule = cal_gregorian$hour(1L))
#> [1] "2020-01-01 12:00:00 UTC"

# Ceiling a Date (treated as midnight-of-day rounding)
d <- as.Date("2020-01-01")
time_ceiling(d, granule = cal_gregorian$month(1L))
#> [1] "2020-02-01"
```
