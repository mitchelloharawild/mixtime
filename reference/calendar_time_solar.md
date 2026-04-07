# Solar time unit classes

Time unit constructors for the solar time system where the boundary of
each day is at sunrise, sunset, or noon. This calendar is intended to be
built on by other calendars to add common time components.

## Usage

``` r
cal_time_solar_sunset

cal_time_solar_sunrise

cal_time_solar_noon

cal_time_solar_midnight

cal_time_solar_dawn

cal_time_solar_dusk
```

## Format

An object of class `mt_calendar` of length 1.

An object of class `mt_calendar` of length 1.

An object of class `mt_calendar` of length 1.

An object of class `mt_calendar` of length 1.

An object of class `mt_calendar` of length 1.

An object of class `mt_calendar` of length 1.

## Value

A time unit object for the Gregorian calendar system.

## Details

The following time units are available in the solar calendar systems.

- `day()`: Day unit

## See also

[`cal_time_civil_midnight`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_civil.md)

## Examples

``` r
# Find the time of sunset in the Gregorian calendar
t <- linear_time(Sys.Date(), cal_time_solar_sunset$day(1L, lat = -37.8136, lon = 144.9631))
datetime(t, tz = "Australia/Melbourne")
#> <mixtime[1]>
#> [1] 2026-04-06 18:06:17 AEST
```
