# Solar time

This time calendar contains solar time units, where the boundary of each
day is at apparent solar midnight. Solar events define the `ampm`
(midnight and noon) and `illumination` (dawn, sunrise, sunset, dusk)
units.

## Usage

``` r
cal_time_solar
```

## Format

A location-based calendar containing solar time units.

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

## See also

[`cal_time_civil`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_civil.md)

## Examples

``` r
# Find the current solar time in Melbourne
datetime(Sys.time(), calendar = cal_time_solar, lat = -37.8136, lon = 144.9631)
#> <mixtime[1]>
#> [1] 2026-05-07 04:23:23 [37.81S 144.96E]
```
