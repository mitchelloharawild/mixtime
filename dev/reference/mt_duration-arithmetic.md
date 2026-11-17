# Arithmetic for durations (`mt_duration`)

Durations (e.g. `days(3)`) support:

- **Combining**: `a + b` and `a - b` give a duration in their finest
  common chronon. The chronons must have a fixed ratio, so
  `months(1) + days(1)` is an error.

- **Ratios**: `a / b` is a number (e.g. `weeks(1) / days(1)` is 7).

- **Scaling**: `d * n`, `n * d` and `d / n`.

- **Adding numbers**: `d + n` and `d - n` count `n` in `d`'s chronon.

Adding a duration to a time point shifts it, see
[mt_time-arithmetic](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-arithmetic.md).

## Usage

``` r
# S4 method for class 'mt_duration'
e1 + e2
# S4 method for class 'mt_duration'
e1 - e2
# S4 method for class 'mt_duration'
e1 * e2
# S4 method for class 'mt_duration'
e1/e2
```

## Arguments

- e1, e2:

  Operands, at least one a duration (`mt_duration`) vector.

## Value

A duration, or a number for the ratio of two durations.

## Examples

``` r
weeks(1) + days(2)
#> <mixtime[1]>
#> [1] 9.0 days
weeks(1) / days(1)
#> [1] 7
days(3) * 2
#> <mixtime[1]>
#> [1] 6.0 days
try(months(1) + days(1))
#> Error : The number of days in a month requires the time context `at`.
```
