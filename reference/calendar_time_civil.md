# Civil time unit classes

Time unit constructors for the civil time system where the boundary of
each day is at midnight on the 24 hour clock. This calendar is intended
to be built on by other calendars (e.g. `[cal_time_civil_midnight]` and
`[cal_isoweek]`) to add common time components. These units can be used
with
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
to create custom time representations.

## Usage

``` r
cal_time_civil_midnight
```

## Format

An object of class `cal_time_civil_midnight` (inherits from
`mt_calendar`) of length 5.

## Value

A time unit object for the civil time system.

## Details

The following time units are available (`cal_time_civil_midnight$`).

- `day()`: Day unit

- `hour()`: Hour unit

- `minute()`: Minute unit

- `second()`: Second unit

- `millisecond()`: Millisecond unit

## See also

`cal_time_civil_midnight`,
[`cal_isoweek`](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)

## Examples

``` r
# Create a custom time representation using Gregorian units
hms <- new_cyclical_time_fn(
  chronon = second(1L),
  cycle = hour(1L)
)
```
