# Solar time unit classes

Time unit constructors for the transit-based solar time system, where
the boundary of each day is at apparent solar midnight. Solar events
define the `ampm` (midnight and noon) and `illumination` (dawn, sunrise,
sunset, dusk) units. `cal_time_solar` is an alias for
`cal_time_solar_transit`.

## Usage

``` r
cal_time_solar_transit

cal_time_solar
```

## Format

A location-based calendar containing transit-based solar time units.

## Value

An S3 list of class `c("cal_time_solar", "mt_calendar")` containing the
named time unit classes of the solar transit calendar. Each unit is
accessible via `$` notation and calling it with a step size and location
produces a time granule (e.g., 1 solar day granule as
`cal_time_solar$day(1L, lat = 0, lon = 0)`). Because solar day
boundaries depend on the observer's position, each unit constructor
requires `lat` and `lon` arguments.

## Details

The following time units are available in the solar calendar systems.

- `day()`: Day unit

- `ampm()`: Half-day units (AM = before solar noon, PM = after solar
  noon)

- `hour()`: Hour units within the solar day

- `minute()`: Minute units within the solar hour

- `second()`: Second units within the solar minute

- `degree()`: Solar angle units within the day

- `arcminute()`: Arcminute units within the solar degree

- `arcsecond()`: Arcsecond units within the solar arcminute

- `illumination()`: Illumination phases (night,
  astronomical/nautical/civil dawn, day, civil/nautical/astronomical
  dusk)

### AM/PM half-days

The `ampm` unit divides each solar day into two halves between solar
noon and solar midnight:

|      |                        |                                     |
|------|------------------------|-------------------------------------|
| Half | Period                 | Description                         |
| AM   | Solar midnight to noon | Morning half; before solar transit  |
| PM   | Solar noon to midnight | Afternoon half; after solar transit |

### Solar illumination phases

Phases describe the illumination state of the sky and correspond to
standard twilight definitions used in astronomy and navigation. Each
phase is bounded by a pair of solar altitude thresholds:

|  |  |  |
|----|----|----|
| Phase | Solar altitude range | Description |
| Night | \< -18° | Sky fully dark; from last dusk to first dawn (spans noon) |
| Astronomical dawn | -18° to -12° | Astronomical twilight before sunrise; faint objects obscured |
| Nautical dawn | -12° to -6° | Nautical twilight before sunrise; horizon visible at sea |
| Civil dawn | -6° to -0.833° | Civil twilight before sunrise; sky brightening in the east |
| Day | \> -0.833° | Sun above the horizon; spans solar noon |
| Civil dusk | -0.833° to -6° | Civil twilight after sunset; sky fading in the west |
| Nautical dusk | -6° to -12° | Nautical twilight after sunset; horizon visible at sea |
| Astronomical dusk | -12° to -18° | Astronomical twilight after sunset; faint objects obscured |

The -0.833° threshold for sunrise and sunset accounts for the mean
angular radius of the solar disc (0.267°) plus the standard atmospheric
refraction at the horizon (0.566°). Noon and midnight are derived from
the equation of time rather than a fixed altitude. Locations that
experience polar day or polar night (civil days where sunrise does not
occur) are not currently supported, it is recommended to use an
alternative reference location.

## See also

[`cal_time_civil`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_time_civil.md),
[`cal_time_lunar`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_time_lunar.md)

## Examples

``` r
# Find the current solar time in Melbourne
datetime(Sys.time(), calendar = cal_time_solar, lat = -37.8136, lon = 144.9631)
#> <mixtime[1]>
#> [1] 2026-06-28 22:19:37 [37.81S 144.96E]
```
