# Create a new calendar

Define a new calendar as a collection of time units. Calendars are the
foundation for representing dates and times in terms of human-readable
components like years, months, days, hours, minutes, and seconds. Each
calendar is defined by specifying the time units it contains, which
determine how time values can be interpreted and manipulated.

## Usage

``` r
new_calendar(..., name, inherit = NULL, class = lifecycle::deprecated())
```

## Arguments

- ...:

  Named time unit class definitions. Each argument should be a time unit
  class (typically created with
  [`new_time_unit()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_time_unit.md))
  that inherits from `mt_unit` or `mt_tz_unit`. The names define the
  calendar's fields and are used to access unit constructors (e.g.,
  `calendar$year()`).

- name:

  A string naming the calendar (e.g. `"cal_gregorian"`). Together with
  the package that defines it, this identifies the calendar of time
  units. Calendars defined in a package should be bound in that
  package's namespace under this name, so that they can be found when
  time objects are read back in from a file.

- inherit:

  Optional calendar to inherit time units from. Units defined in `...`
  will override inherited units with the same name.

- class:

  **\[deprecated\]** Use `name` instead.

## Value

A calendar object, the single instance of a new S7 class `name` which
inherits from
[`mt_calendar`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_calendar.md).
A calendar is a named list containing the specified time unit classes.

## Details

Time units are typically S7 class definitions that inherit from
[`mt_unit`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
for standard units,
[`mt_tz_unit`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
for timezone-aware units (civil time), or
[`mt_loc_unit`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
for location-aware units (astronomical time). The calendar object
provides a namespace for accessing these unit constructors and defines
the relationships between them for calendar arithmetic.

## See also

[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md),
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md)

## Examples

``` r
# Create a simple calendar with year and month units
# (inheriting from civil time units for day, hour, minute, second, ...)
cal_simple <- new_calendar(
  year = new_time_unit("tu_year", parent = mt_tz_unit),
  month = new_time_unit("tu_month", parent = mt_tz_unit),
  inherit = cal_time_civil,
  name = "cal_simple"
)

# Create time granules from the calendar
cal_simple$year(1L)
#> <tu_year>
#>  @ n       : int 1
#>  @ calendar: <cal_simple> List of 10
#>  .. $ year       : <tu_year/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ month      : <tu_month/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ day        : <mixtime::tu_day/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ ampm       : <mixtime::tu_ampm/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ hour       : <mixtime::tu_hour/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ minute     : <mixtime::tu_minute/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ second     : <mixtime::tu_second/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ millisecond: <mixtime::tu_millisecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ microsecond: <mixtime::tu_microsecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ nanosecond : <mixtime::tu_nanosecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  @ tz      : 'mt_naive' chr NA
cal_simple$month(1L)
#> <tu_month>
#>  @ n       : int 1
#>  @ calendar: <cal_simple> List of 10
#>  .. $ year       : <tu_year/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ month      : <tu_month/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ day        : <mixtime::tu_day/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ ampm       : <mixtime::tu_ampm/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ hour       : <mixtime::tu_hour/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ minute     : <mixtime::tu_minute/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ second     : <mixtime::tu_second/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ millisecond: <mixtime::tu_millisecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ microsecond: <mixtime::tu_microsecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ nanosecond : <mixtime::tu_nanosecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  @ tz      : 'mt_naive' chr NA
```
