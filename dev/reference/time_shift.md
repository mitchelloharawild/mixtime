# Shift time points by a duration

Shifts time points by a duration, like `x + by`, with control over how
time points that don't exist in the target granule are resolved.
Shifting by a duration with an irregular relationship to the time
points' chronon (e.g. days within months) can produce time points that
don't exist, such as shifting 31 January by one month. The `+` and `-`
operators always use `on_invalid = "clamp"`.

## Usage

``` r
time_shift(x, by, on_invalid = c("clamp", "nearest", "overflow", "reject"))
```

## Arguments

- x:

  A time point vector.

- by:

  A time
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md)
  (e.g., `months(1L)`), or a character string specifying one (e.g.,
  `"1 month"`).

- on_invalid:

  How to resolve time points that don't exist in the target granule:

  - `"clamp"` (default): Reduce the invalid component to its last valid
    value, preserving finer components (e.g., the time of day).

  - `"nearest"`: Use the end of the target granule, so finer components
    are not preserved. For discrete time this is the last chronon within
    the granule (e.g., 23:59:59 on the last day); for continuous time it
    is the end of that chronon (e.g., 00:00:00 on the following day).

  - `"overflow"`: Allow the invalid component to overflow into the next
    granule.

  - `"reject"`: Return `NA` with a warning.

## Value

A time point vector of the same type as `x`.

## Examples

``` r
x <- datetime("2021-01-30 09:30:00")
time_shift(x, months(1L))
#> <mixtime[1]>
#> [1] 2021-02-28 09:30:00
time_shift(x, months(1L), on_invalid = "nearest")
#> <mixtime[1]>
#> [1] 2021-02-28 23:59:59
time_shift(x, months(1L), on_invalid = "overflow")
#> <mixtime[1]>
#> [1] 2021-03-02 09:30:00
time_shift(x, months(1L), on_invalid = "reject")
#> Warning: 1 time point did not exist in the target granule and was replaced with "NA".
#> ℹ Use `on_invalid = "clamp"` or `"nearest"` to resolve them to valid time
#>   points instead.
#> <mixtime[1]>
#> [1] NA

# Shifting backwards
time_shift(date("2021-03-31"), months(-1L), on_invalid = "overflow")
#> <mixtime[1]>
#> [1] 2021-03-03
```
