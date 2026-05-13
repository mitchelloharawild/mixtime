# ISO 8601 time unit classes

Time unit constructors for the ISO 8601 calendar system. These units can
be used with
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
to create custom time representations.

## Usage

``` r
cal_isoweek
```

## Format

A civil-based calendar containing ISO 8601 time units.

## Value

An S3 list of class `c("cal_isoweek", "mt_calendar")` containing the
named time unit classes of the ISO 8601 week calendar. Each unit is
accessible via `$` notation and calling it with a step size produces a
time granule (e.g., 1 week granule as `cal_isoweek$week(1L)`).

## Details

The following time units are available in the ISO week date calendar:

- [`year()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md):
  ISO year unit (years start on the week containing the first Thursday)

- `week()`: Week unit (7-day periods)

- `day()`: Day unit

- `hour()`: Hour unit

- `minute()`: Minute unit

- `second()`: Second unit

- `millisecond()`: Millisecond unit

ISO 8601 weeks always start on Monday and the first week of a year is
the week containing the first Thursday of that year. This means that
some days in early January may belong to the last week of the previous
ISO year, and some days in late December may belong to the first week of
the next ISO year.

## See also

[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
for creating custom time representations,
[`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md)
for a pre-defined ISO 8601 year-week representation
