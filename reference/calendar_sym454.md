# Symmetry454 time unit classes

Time unit constructors for the Symmetry454 calendar system. These units
can be used with
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
to create custom time representations.

## Usage

``` r
cal_sym454
```

## Format

A civil-based calendar containing Symmetry454 time units.

## Details

The Symmetry454 calendar (Sym454) is a perennial solar calendar proposed
by Dr. Irv Bromberg. It preserves the traditional 12-month structure and
7-day week, with months arranged in a symmetrical 4-5-4 week pattern per
quarter. Every month starts on Monday and has a whole number of weeks,
meaning no month ever contains a partial week.

The following time units are available in the Symmetry454 calendar
(`cal_sym454$`).

- [`year()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md):
  Year unit

- `month()`: Month unit

- `week()`: Week unit

- `day()`: Day unit

- `hour()`: Hour unit

- `minute()`: Minute unit

- `second()`: Second unit

- `millisecond()`: Millisecond unit

### Leap years

Rather than intercalary days, Symmetry454 uses a **leap week** appended
to December once every 5 or 6 years, making December a 5-week month in
leap years. A year is a leap year if `(52 * year + 146) %% 293 < 52`.

This yields a mean year of 365 + 71/293 days (≈ 365 days, 5 hours, 48
minutes, 56.5 seconds), intentionally slightly shorter than the mean
northward equinoctial year.

## See also

[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
for creating linear time points, and
<https://en.wikipedia.org/wiki/Symmetry454> for more calendar details.

## Examples

``` r
# Create a custom time representation using Symmetry454 units
linear_time(
  Sys.time(),
  chronon = cal_sym454$week(1L)
)
#> <mixtime[1]>
#> [1] 2026-Apr-W2
```
