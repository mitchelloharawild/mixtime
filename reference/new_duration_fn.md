# Duration function factory

`new_duration_fn()` creates a duration function for a specified chronon.
A chronon is the smallest indivisible time unit (e.g., days, months)
that defines what the numeric magnitudes in the resulting duration
vector represent.

## Usage

``` r
new_duration_fn(chronon, default_calendar = cal_gregorian)
```

## Arguments

- chronon:

  A bare call for a time unit object representing the chronon (e.g.,
  `month(1L)`, `day(1L)`).

- default_calendar:

  A default calendar used to resolve the time units if they don't exist
  in the calendar of the input data (e.g., `cal_gregorian`).

## Value

A function used to create duration vectors with a specific chronon. The
returned function accepts:

- `data`:

  A numeric vector of duration magnitudes.

- `calendar`:

  A calendar system used to evaluate `chronon`. Defaults to
  `time_calendar(data)`.

- `...`:

  Additional arguments passed to the chronon (e.g., `tz` for timezones).

## See also

- [`duration()`](https://pkg.mitchelloharawild.com/mixtime/reference/duration.md)
  for creating duration vectors directly

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
# Create a months duration function
months <- new_duration_fn(month(1L), default_calendar = cal_gregorian)
months(1:6)
#> <mixtime[6]>
#> [1] 1 month  2 months 3 months 4 months 5 months 6 months

# Create a days duration function
days <- new_duration_fn(day(1L), default_calendar = cal_gregorian)
days(1:7)
#> <mixtime[7]>
#> [1] 1 day  2 days 3 days 4 days 5 days 6 days 7 days
```
