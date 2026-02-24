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

An object of class `cal_isoweek` (inherits from `mt_calendar`) of length
7.

## Value

A time unit object for the ISO 8601 calendar system.

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
