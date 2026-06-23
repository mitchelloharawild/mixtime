# Lunar time unit classes

Time unit constructors for the synodic lunar time system, where the
boundary of each month is at the new moon (lunar conjunction) and the
boundary of each phase is at a lunar octant (each eighth of the synodic
cycle). Both boundaries are location-independent, as the new moon is a
geocentric astronomical event. `cal_time_lunar` is an alias for
`cal_time_lunar_synodic`.

## Usage

``` r
cal_time_lunar_synodic

cal_time_lunar
```

## Format

A calendar containing synodic lunar time units.

## Value

An S3 list of class `c("cal_time_lunar", "mt_calendar")` containing the
named time unit classes of the synodic lunar calendar. Each unit is
accessible via `$` notation and calling it with a step size produces a
time granule (e.g., 1 synodic month granule as
`cal_time_lunar$month(1L)`). Lunar month and phase boundaries are
location-independent.

## Details

The following time units are available in the lunar calendar systems.

- `month()`: Synodic month unit

- `phase()`: Synodic phase unit

## See also

[`cal_time_civil`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_civil.md),
[`cal_time_solar`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_solar.md)

## Examples

``` r
# Find the time of a new moon in the Gregorian calendar
t <- linear_time(Sys.Date(), cal_time_lunar$month(1L))
datetime(t, tz = "Australia/Melbourne")
#> <mixtime[1]>
#> [1] 2026-06-15 12:56:14 AEST

```
