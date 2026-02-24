# Package index

## Linear Time Representations

Functions to create and work with linear time representations

### Linear Time Vectors

The general function for creating linear time representations

- [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
  : Linear time points

### Linear Time Helpers

Convenience functions for common linear time representations

- [`year()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  [`yearquarter()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  [`yearmonthday()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  [`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
  : Linear time helper functions

## Cyclical Time Representations

Functions to create and work with cyclical time representations

### Cyclical Time Vectors

The general function for creating cyclical time representations

- [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md)
  : Cyclical time points

### Cyclical Time Helpers

Convenience functions for common cyclical time representations

- [`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  [`day_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  [`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  [`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  [`week_of_year()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time_helpers.md)
  : Cyclical time helpers

## Time Manipulation

Functions for manipulating and transforming time objects.

- [`seq(`*`<mixtime>`*`)`](https://pkg.mitchelloharawild.com/mixtime/reference/seq.mixtime.md)
  [`seq(`*`<mt_linear>`*`)`](https://pkg.mitchelloharawild.com/mixtime/reference/seq.mixtime.md)
  [`seq(`*`<mt_cyclical>`*`)`](https://pkg.mitchelloharawild.com/mixtime/reference/seq.mixtime.md)
  : Generate sequences of mixtime values
- [`round_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/round_time.md)
  [`ceiling_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/round_time.md)
  [`floor_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/round_time.md)
  : Round, floor and ceiling transformations for time objects

## Calendar Systems

Calendar systems are composed of their associated time units. These
define how time is divided and measured in different calendar systems.

### Gregorian Calendar System

The standard civil calendar system with years, quarters, months, days,
and sub-day units.

- [`cal_gregorian`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md)
  : Gregorian time unit classes

### ISO 8601 Calendar System

The ISO 8601 week date calendar system with ISO years and weeks.

- [`cal_isoweek`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  : ISO 8601 time unit classes

### Custom Calendars

Functions for creating custom calendar systems

- [`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md)
  : Create a new calendar

## Time Systems

Time systems define when the boundary of a day occurs and how a day is
divided into time units. These can be combined with calendar systems to
create custom time representations.

### Civil Time System

The civil time system where each day begins at midnight on the 24-hour
clock in a specific time zone.

- [`cal_time_civil_midnight`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_civil.md)
  : Civil time unit classes

### Solar Time Systems

Solar time systems where day boundaries are defined by solar events
(sunrise, noon, or sunset) at specific geographic locations.

- [`cal_time_solar_sunset`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_solar.md)
  [`cal_time_solar_sunrise`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_solar.md)
  [`cal_time_solar_noon`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_time_solar.md)
  : Solar time unit classes

## Timezones

Functions for working with timezones and timezone information

- [`tz_name()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_name.md)
  : Extract timezone from an object
- [`tz_offset()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_offset.md)
  : Get timezone offset
- [`tz_abbreviation()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_abbreviation.md)
  : Get timezone abbreviation
- [`tz_transitions()`](https://pkg.mitchelloharawild.com/mixtime/reference/tz_transitions.md)
  : Get timezone transitions

## Mixtime Objects

Functions for creating and working with mixtime objects that can contain
multiple time granularities.

- [`new_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_mixtime.md)
  : Create a new mixtime
- [`as_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/as_mixtime.md)
  : Convert time class into a mixtime
- [`is_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/is_mixtime.md)
  : Check if the object is a mixtime

## Extensibility methods

These low-level functions define the relationships and labels between
time units. Adding S7 methods for them allow the creation of custom time
units and calendars.

- [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md)
  [`chronon_cardinality.S7_methods()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md)
  : Cardinality between time units
- [`chronon_convert()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_convert.md)
  [`chronon_convert.S7_methods()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_convert.md)
  : Convert between chronons
- [`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_divmod.md)
  [`chronon_divmod.S7_methods()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_divmod.md)
  : Convert between chronons of different time units
- [`chronon_common()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_common.md)
  : Find a common chronon from a set of chronons
- [`chronon_granules()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_granules.md)
  : Default linear time granules for chronons
- [`mt_unit()`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  [`mt_tz_unit()`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  [`mt_loc_unit()`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  : Base S7 class for creating new time units
- [`time_chronon()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_chronon.md)
  : Obtain the chronon of a time object
- [`time_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_calendar.md)
  : Obtain the calendar of a time object
- [`time_unit_full()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_unit_labels.md)
  [`time_unit_abbr()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_unit_labels.md)
  : Time units as a string
- [`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_labels.md)
  [`cyclical_labels.S7_methods()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_labels.md)
  : Friendly labels for cyclical relationships
- [`mixtime_valid()`](https://pkg.mitchelloharawild.com/mixtime/reference/mixtime_valid.md)
  : Check if times can be used within mixtime
- [`new_cyclical_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_cyclical_time_fn.md)
  : Cyclical time function factory
- [`new_linear_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_linear_time_fn.md)
  : Linear time function factory
- [`circsum()`](https://pkg.mitchelloharawild.com/mixtime/reference/circsum.md)
  : Compute circular rolling sums
