# Base S7 class for creating new time units

This class is the primative class for time units, and should be extended
from when creating new time units. A new class is typically created with
S7 using: `tu_day <- S7::new_class("tu_day", parent = mt_unit)`

## Usage

``` r
mt_unit(.data = 1L)

mt_tz_unit(.data = 1L, tz = "UTC")
```

## Arguments

- .data:

  The number of time units

- tz:

  The timezone name for the unit (valid units can be found with
  `[tzdb::tzdb_names()]`)

## Value

A time unit object of class `mt_unit`
