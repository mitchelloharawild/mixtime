# Compose a linear time vector from linear and cyclical components

`time_compose()` is the inverse of
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md):
given a set of
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
components it reconstructs the corresponding time points. Each component
is either a two-sided formula pairing a spec with its value, or an
already-tagged linear/cyclical time vector (e.g. produced by
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md),
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md),
or a
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)
column).

## Usage

``` r
time_compose(..., discrete = TRUE, calendar = cal_gregorian)
```

## Arguments

- ...:

  Components used to build the time point. Each element is either:

  - a two-sided formula, `lin(<granule>) ~ <value>` or
    `cyc(<granule>, <cycle>) ~ <value>` (see
    [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)),
    or

  - an existing linear or cyclical `mixtime` vector.

- discrete:

  Logical. If `TRUE` (default), returns integer chronons since Unix
  epoch (discrete time model). If `FALSE`, returns fractional chronons
  allowing representation of fractional time granules (continuous time
  model).

- calendar:

  Calendar used to resolve bare granule names in
  [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/
  [`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
  formulas. Defaults to
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_gregorian.md).

## Value

A `mixtime` linear time vector, at the finest chronon reached by the
supplied chain (or the anchor's own chronon, if no
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
components were supplied).

## Details

Exactly one
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component (the **anchor**) must be supplied. It fixes the absolute
(non-repeating) position at some granule (e.g. the year). Every other
component must be a
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component that connects, without gaps or branches, from the anchor down
to the target chronon. Each cycle of
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
must equal the chronon of another supplied component exactly, forming a
single chain.

Values of linear and cyclical components are specified on the
right-hand-side of the formula. A
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
value is the real-world count (e.g. the literal year 1980); a
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
value is the 1-indexed position within the cycle (e.g.
`cyc(month, year) ~ 3` is the 3rd month, March), matching everyday
counting rather than the raw 0-indexed position
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)
uses internally.

## See also

[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)
for the inverse operation,
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
for the component vocabulary shared with
[`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)
and [`format()`](https://rdrr.io/r/base/format.html).

## Examples

``` r
# cyc() values are 1-indexed positions: month 3 is March, day 15 is the 15th
time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(day, month) ~ 15)
#> <mixtime[1]>
#> [1] 1980-03-15

# A lin() anchor alone is a valid (coarser) time point
time_compose(lin(year) ~ 1980)
#> <mixtime[1]>
#> [1] 1980

# Round-tripping through time_components()
parts <- time_components(as.Date("2024-03-15"), yr = lin(year), mth = cyc(month, year))
with(parts, time_compose(yr, mth))
#> <mixtime[1]>
#> [1] 2024 Mar

# Multi-unit (self-referencing) cycles: the 3rd month (1-indexed) of the
# 4th 3-month block since epoch (block 3 = months 9-11 -> December 1970)
time_compose(lin(month(3L)) ~ 3, cyc(month(1L), month(3L)) ~ 3)
#> <mixtime[1]>
#> [1] 1970 Dec
```
