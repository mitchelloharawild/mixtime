# Extract locations from an object

Generic function to extract the location from objects that have location
information.

## Usage

``` r
loc_latitude(x, ...)

loc_longitude(x, ...)

loc_altitude(x, ...)
```

## Arguments

- x:

  An object with location information.

- ...:

  Additional arguments passed to methods.

## Value

A numeric value representing the location (e.g., longitude, latitude,
etc.).

## Examples

``` r
t <- linear_time(
  1:3, 
  cal_time_solar_sunset$day(1L, lat = -37.8136, lon = 144.9631)
)

loc_longitude(t)
#> [1] 144.9631 144.9631 144.9631
loc_latitude(t)
#> [1] -37.8136 -37.8136 -37.8136
loc_altitude(t)
#> [1] 0 0 0
```
