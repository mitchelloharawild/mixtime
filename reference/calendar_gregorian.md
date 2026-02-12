# Gregorian time unit classes

Time unit constructors for the Gregorian calendar system. These units
can be used with [`linear_time()`](linear_time.md) to create custom time
representations.

## Usage

``` r
tu_year(.data = 1L, tz = "UTC")

tu_quarter(.data = 1L, tz = "UTC")

tu_month(.data = 1L, tz = "UTC")

tu_day(.data = 1L, tz = "UTC")

tu_hour(.data = 1L, tz = "UTC")

tu_minute(.data = 1L, tz = "UTC")

tu_second(.data = 1L, tz = "UTC")

tu_millisecond(.data = 1L, tz = "UTC")
```

## Arguments

- .data:

  The number of time units

- tz:

  The timezone name for the unit (valid units can be found with
  `[tzdb::tzdb_names()]`)

## Value

A time unit object for the Gregorian calendar system.

## Details

The following Gregorian time units are available:

- `tu_year()`: Year unit

- `tu_quarter()`: Quarter (3-month period) unit

- `tu_month()`: Month unit

- `tu_day()`: Day unit

- `tu_hour()`: Hour unit

- `tu_minute()`: Minute unit

- `tu_second()`: Second unit

- `tu_millisecond()`: Millisecond unit

These units form a hierarchy where conversions between adjacent units
follow the Gregorian calendar rules. For units that don't have a fixed
relationship (e.g., months to days), the conversion requires a time
context.

## See also

[`linear_time()`](linear_time.md) for creating custom time
representations, [linear_gregorian](linear_gregorian.md) for pre-defined
Gregorian time representations

## Examples

``` r
# Create a custom time representation using Gregorian units
dayhour <- linear_time(
  granules = list(tu_day(1L)),
  chronon = tu_hour(1L)
)
```
