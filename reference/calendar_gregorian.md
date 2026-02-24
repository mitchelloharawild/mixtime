# Gregorian time unit classes

Time unit constructors for the Gregorian calendar system. These units
can be used with
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
to create custom time representations.

## Usage

``` r
cal_gregorian
```

## Format

An object of class `cal_gregorian` (inherits from `mt_calendar`) of
length 8.

## Value

A time unit object for the Gregorian calendar system.

## Details

The following time units are available in the Gregorian calendar
(`cal_gregorian$`).

- [`year()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md):
  Year unit

- `quarter()`: Quarter (3-month period) unit

- `month()`: Month unit

- `day()`: Day unit

- `hour()`: Hour unit

- `minute()`: Minute unit

- `second()`: Second unit

- `millisecond()`: Millisecond unit

These units form a hierarchy where conversions between adjacent units
follow the Gregorian calendar rules. For units that don't have a fixed
relationship (e.g., months to days), the conversion requires a time
context.

## See also

[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
for creating linear time points.

## Examples

``` r
# Create a custom time representation using Gregorian units
linear_time(
  Sys.time(),
  chronon = hour(1L)
)
#> <mixtime[1]>
#> [1] 2026-Feb-24-h2
```
