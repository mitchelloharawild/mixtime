# Base S7 class for creating new time units

This class is the primative class for time units, and should be extended
from when creating new time units. A new class is typically created with
S7 using: `S7::new_class("tu_***", parent = mt_tz_unit)`

## Usage

``` r
mt_unit(.data = 1L)

mt_tz_unit(.data = 1L, tz = "UTC")

mt_loc_unit(.data = 1L, lat = 0, lon = 0, alt = 0)
```

## Arguments

- .data:

  The number of time units

- tz:

  The timezone name for the unit (valid units can be found with
  `[tzdb::tzdb_names()]`)

- lat:

  Numeric. Latitude in decimal degrees. Range: -90 to 90. Default: 0
  (equator).

- lon:

  Numeric. Longitude in decimal degrees. Range: -180 to 180. Default: 0
  (Prime Meridian).

- alt:

  Numeric. Altitude in meters above sea level. Default: 0 (sea level).

## Value

A time unit object of class `mt_unit`

## Details

Time units are the building blocks of calendars in mixtime. Each unit
represents a specific temporal component (e.g., day, month, year) and
can be combined using
[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md)
to create a calendar system.

When creating custom calendars, define time unit classes that inherit
from either `mt_unit` (for standard units) or `mt_tz_unit` (for
timezone-aware units), then pass them as named arguments to
[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md).
The calendar will use these names to create constructor functions
accessible via `$` notation (e.g., `calendar$day(1L)`).

## Calendar Algebra Methods

Time units enable calendar arithmetic through two key generic methods
that should be implemented for custom time units:

- `chronon_cardinality(from, to, at)` - Returns the number of `to` units
  that fit within one `from` unit. This can be a fixed value (e.g., 7
  days per week) or variable based on `at` (e.g., 28-31 days per month).

- `chronon_divmod(x, from, to)` - Converts time unit `x` from units of
  `from` to units of `to`, returning a list with `chronon` (the
  quotient) and `remainder`. This enables conversions between units that
  have variable cardinality (e.g., the date 2020-03-23 to the month
  2020-03). All conversions should be based on chronons since epoch
  (1970-01-01), in the UTC time zone.

These methods work together to enable mixtime to perform calendar-aware
arithmetic, understanding that months have variable lengths and handling
timezone-aware conversions.

## See also

[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md)
for creating calendars from time units

## Examples

``` r
# Create a timezone-aware unit class

# Use these units to create a calendar
my_calendar <- new_calendar(
  day = S7::new_class("tu_my_day", parent = mt_unit),
  month = S7::new_class("tu_my_month", parent = mt_tz_unit),
  class = "my_calendar"
)

# Access unit constructors from the calendar
my_calendar$day(1L)
#> <tu_my_day> int 1
my_calendar$month(3L, tz = "America/New_York")
#> <tu_my_month> int 3
#>  @ tz: chr "America/New_York"
```
