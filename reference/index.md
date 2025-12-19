# Package index

## Linear Time Representations

Functions to create and work with linear time representations

- [`linear_time()`](linear_time.md) : Linear time representation

### Gregorian Calendar

Common linear time representations in the Gregorian calendar system.

- [`year()`](linear_gregorian.md) [`yearquarter()`](linear_gregorian.md)
  [`yearmonth()`](linear_gregorian.md) : Gregorian continuous time
  representations

### ISO 8601 Calendar

Common linear time representations in the ISO 8601 week date system.

- [`yearweek()`](linear_iso8601.md) : ISO 8601 year-week time
  representation

## Cyclical Time Representations

Functions to create and work with cyclical time representations

- [`cyclical_time()`](cyclical_time.md) : Cyclical time representation

### Cyclical Gregorian Time

Cyclical time representations in the Gregorian calendar system.

- [`month_of_year()`](cyclical_gregorian.md)
  [`day_of_year()`](cyclical_gregorian.md)
  [`day_of_month()`](cyclical_gregorian.md) : Gregorian cyclical time
  representations

### Cyclical ISO 8601 Time

Cyclical time representations in the ISO 8601 week date system.

- [`day_of_week()`](cyclical_iso8601.md)
  [`week_of_year()`](cyclical_iso8601.md) : ISO 8601 day of week

## Calendar Systems

Calendar systems are composed of their associated time units. These
define how time is divided and measured in different calendar systems.

### Gregorian Calendar System

The standard civil calendar system with years, quarters, months, days,
and sub-day units.

- [`tu_year()`](calendar_gregorian.md)
  [`tu_quarter()`](calendar_gregorian.md)
  [`tu_month()`](calendar_gregorian.md)
  [`tu_day()`](calendar_gregorian.md)
  [`tu_hour()`](calendar_gregorian.md)
  [`tu_minute()`](calendar_gregorian.md)
  [`tu_second()`](calendar_gregorian.md)
  [`tu_millisecond()`](calendar_gregorian.md) : Gregorian time unit
  classes

### ISO 8601 Calendar System

The ISO 8601 week date calendar system with ISO years and weeks.

- [`tu_isoyear()`](calendar_iso8601.md)
  [`tu_week()`](calendar_iso8601.md) : ISO 8601 time unit classes

## Extensibility methods

These low-level functions define the relationships and labels between
time units. Adding S7 methods for them allow the creation of custom time
units and calendars.

- [`chronon_cardinality()`](chronon_cardinality.md)
  [`chronon_cardinality.S7_methods()`](chronon_cardinality.md) :
  Cardinality between time units
- [`chronon_convert()`](chronon_convert.md)
  [`chronon_convert.S7_methods()`](chronon_convert.md) : Convert between
  chronons
- [`chronon_divmod()`](chronon_divmod.md)
  [`chronon_divmod.S7_methods()`](chronon_divmod.md) : Convert between
  chronons of different time units
- [`chronon_common()`](chronon_common.md) : Find a common chronon from a
  set of chronons
- [`mt_unit()`](mt_unit.md) : Base S7 class for creating new time units
- [`time_chronon()`](time_chronon.md) : Obtain the chronon of a time
  object
- [`time_unit_full()`](time_unit_labels.md)
  [`time_unit_abbr()`](time_unit_labels.md) : Time units as a string
- [`cyclical_labels()`](cyclical_labels.md)
  [`cyclical_labels.S7_methods()`](cyclical_labels.md) : Friendly labels
  for cyclical relationships
- [`mixtime_valid()`](mixtime_valid.md) : Check if times can be used
  within mixtime

## Mixtime Objects

Functions for creating and working with mixtime objects that can contain
multiple time granularities.

- [`new_mixtime()`](new_mixtime.md) : Create a new mixtime
- [`as_mixtime()`](as_mixtime.md) : Convert time class into a mixtime
- [`is_mixtime()`](is_mixtime.md) : Check if the object is a mixtime

## Time Manipulation

Functions for manipulating and transforming time objects.

- [`round_time()`](round_time.md) [`ceiling_time()`](round_time.md)
  [`floor_time()`](round_time.md) : Round, floor and ceiling
  transformations for time objects
