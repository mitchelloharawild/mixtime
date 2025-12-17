# ISO 8601 time unit classes

Time unit constructors for the ISO 8601 calendar system. These units can
be used with [`linear_time()`](linear_time.md) to create custom time
representations.

## Usage

``` r
tu_isoyear(.data = 1L)

tu_week(.data = 1L)
```

## Arguments

- .data:

  The number of time units

## Value

A time unit object for the ISO 8601 calendar system.

## Details

The following ISO 8601 time units are available:

- `tu_isoyear()`: ISO year unit (years start on the week containing the
  first Thursday)

- `tu_week()`: Week unit (7-day periods)

ISO 8601 weeks always start on Monday and the first week of a year is
the week containing the first Thursday of that year. This means that
some days in early January may belong to the last week of the previous
ISO year, and some days in late December may belong to the first week of
the next ISO year.

## See also

[`linear_time()`](linear_time.md) for creating custom time
representations, [`yearweek()`](linear_iso8601.md) for a pre-defined ISO
8601 year-week representation
