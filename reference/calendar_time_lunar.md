# Lunar time unit classes

Time unit constructors for the lunar time system where the boundary of
each day is at sunrise, sunset, or noon. This calendar is intended to be
built on by other calendars to add common time components.

## Usage

``` r
cal_time_lunar
```

## Format

A location-based calendar containing lunar time units.

## Details

The following time units are available in the lunar calendar systems.

- `month()`: Synodic month unit

- `phase()`: Synodic phase unit

## See also

[`cal_time_civil_midnight`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_civil.md)

## Examples

``` r
# Find the time of a new moon in the Gregorian calendar
t <- linear_time(Sys.Date(), cal_time_lunar$month(1L, lat = -37.8136, lon = 144.9631))
datetime(t, tz = "Australia/Melbourne")
#> <mixtime[1]>
#> [1] 2026-03-19 12:25:48 AEDT

```
