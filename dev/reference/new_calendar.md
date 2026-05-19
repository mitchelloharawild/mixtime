# Create a new calendar

Define a new calendar as a collection of time units. Calendars are the
foundation for representing dates and times in terms of human-readable
components like years, months, days, hours, minutes, and seconds. Each
calendar is defined by specifying the time units it contains, which
determine how time values can be interpreted and manipulated.

## Usage

``` r
new_calendar(..., inherit = NULL, class = character())
```

## Arguments

- ...:

  Named time unit class definitions. Each argument should be a time unit
  class (typically created with
  [`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html))
  that inherits from `mt_unit` or `mt_tz_unit`. The names define the
  calendar's fields and are used to access unit constructors (e.g.,
  `calendar$year()`).

- inherit:

  Optional calendar to inherit time units from. Units defined in `...`
  will override inherited units with the same name.

- class:

  Character vector of additional classes for the calendar object.

## Value

A calendar object of class `c(class, "mt_calendar")`, consisting of a
named list containing the specified time unit classes.

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
  class = "cal_simple"
)

# Create time granules from the calendar
cal_simple$year(1L)
#> <tu_year>
#>  @ n : int 1
#>  @ tz: 'mt_naive' chr NA
cal_simple$month(1L)
#> <tu_month>
#>  @ n : int 1
#>  @ tz: 'mt_naive' chr NA
```
