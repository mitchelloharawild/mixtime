# Base S7 class for calendars

`mt_calendar` is the parent class of every calendar. A calendar is a
named list of time unit classes, and each calendar created with
[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_calendar.md)
is the single instance of its own subclass of `mt_calendar` (e.g.
`cal_isoweek` is an instance of class `cal_isoweek`). This allows
methods to dispatch on the calendar, such as formatting a shared unit
differently in each calendar that contains it.

## Usage

``` r
mt_calendar(.data = list())
```

## Arguments

- .data:

  A named list of time unit classes.

## Value

An S7 class object, or a calendar when called as a constructor. End
users should use
[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_calendar.md)
to create calendars.

## Details

Time units record their calendar by its identifier
(`"<package>::<name>"`), which is resolved back to the calendar object
when the unit's `@calendar` is accessed. This keeps time objects small
when serialised, and their calendar is restored from the current
definition when read back in.

## See also

[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_calendar.md)
for creating calendars,
[`time_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_calendar.md)
for obtaining the calendar of a time object.
