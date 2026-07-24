# Linear and cyclical component helpers

`lin()` and `cyc()` name the time components addressed by mixtime's
*component-aware contexts* — a single vocabulary shared across
[`format()`](https://rdrr.io/r/base/format.html) (and parsing) format
strings and
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)
expressions. They are only meaningful inside one of these contexts;
calling them directly is an error.

## Usage

``` r
lin(granule)

cyc(granule, cycle)
```

## Arguments

- granule:

  The time granule to address, given as a granule generator (e.g.
  `year`) or a sized time unit (e.g. `year(1L)`). Resolved in the
  calendar of the time vector being formatted or decomposed.

- cycle:

  The coarser granule defining the cycle a `cyc()` component repeats
  within (e.g. `year` in `cyc(month, year)`).

## Value

A component specification, consumed internally by the component-aware
context (e.g. [`format()`](https://rdrr.io/r/base/format.html) or
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)).

## Details

A linear and a cyclical component of the same granule store the *same*
value: the count of that chronon since the Unix epoch. The reduction to
a within-cycle position happens only when a cyclical vector is
formatted. `cyc()` therefore behaves like `lin()` on its finest granule
but additionally records the cycle.

## See also

[`format()`](https://rdrr.io/r/base/format.html) and
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)
