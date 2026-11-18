# Linear, cyclical and duration component helpers

`lin()`, `cyc()` and `dur()` name the time components addressed by
mixtime's *component-aware contexts* — a single vocabulary shared across
[`format()`](https://rdrr.io/r/base/format.html) (and parsing) format
strings and
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_components.md)
expressions. They are only meaningful inside one of these contexts;
calling them directly is an error.

## Usage

``` r
lin(granule)

cyc(granule, cycle)

dur(granule)
```

## Arguments

- granule:

  The time granule to address, given as a granule generator (e.g.
  `year`) or a sized time unit (e.g. `year(1L)`). Resolved in the
  calendar of the time vector being formatted, parsed or decomposed.

- cycle:

  The coarser granule defining the cycle a `cyc()` component repeats
  within (e.g. `year` in `cyc(month, year)`).

## Value

A component specification, consumed internally by the component-aware
context (e.g. [`format()`](https://rdrr.io/r/base/format.html),
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_parse.md)
or
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_components.md)).

## Details

A linear and a cyclical component of the same granule store the *same*
value: the count of that chronon since the Unix epoch. The reduction to
a within-cycle position happens only when a cyclical vector is
formatted. `cyc()` therefore behaves like `lin()` on its finest granule
but additionally records the cycle.

`dur()` addresses a *duration* (an unanchored span of time, see
[`duration()`](https://pkg.mitchelloharawild.com/mixtime/reference/duration.md))
counted in units of a granule, e.g. `"{dur(day)}"` formats and parses
text like `"3 days"`. A duration has no position in the calendar, so
`dur()` must be the only token in its format string and isn't available
in
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_components.md).
Its granule is always a 1-unit granule, so `dur(hour(3L))` behaves like
`dur(hour)`.

## See also

[`format()`](https://rdrr.io/r/base/format.html),
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_parse.md),
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_components.md)
and
[`duration()`](https://pkg.mitchelloharawild.com/mixtime/reference/duration.md)
