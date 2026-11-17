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
time_compose(
  ...,
  discrete = TRUE,
  calendar = cal_gregorian,
  on_ambiguous = getOption("mixtime.on_ambiguous", "earliest"),
  offset = NULL
)
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

- on_ambiguous:

  How to resolve a time zone aware time point whose local time occurs
  twice (when daylight saving ends) and that `offset` doesn't resolve.
  One of `"earliest"` (the default), `"latest"`, `"reject"` (`NA` with a
  warning) or `"error"`. The default is set by the
  `mixtime.on_ambiguous` option. Without a strategy chosen by either, an
  ambiguous time point warns and resolves to its earliest occurrence.

- offset:

  The UTC offset of each time point, which tells apart the two
  occurrences of a repeated local time. Either a duration of fixed
  length (e.g. `hours(11L)`, or
  [`tz_offset()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_offset.md)
  of the times the components came from), or time zone abbreviations of
  the components' time zone (e.g. `"AEDT"`). `NA` where not known. Each
  offset must be one in use at that local time. For a local time skipped
  when daylight saving starts, the offset gives the instant it describes
  (e.g. 02:30 AEST is 03:30 AEDT).

## Value

A `mixtime` time vector, at the finest chronon reached by the chain (or
the root's own chronon, if only one component is given). Linear with a
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component; underspecified cyclical (`mt_cyclical_free`) otherwise, since
there is then no point in time for the result to be a position within.

## Details

A
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component, when supplied, locates the time point at some granule (e.g.
the year). Every other component must be
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md),
chaining without gaps or branches from the
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
down to the target chronon: each cycle must equal another component's
chronon exactly.

With no
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component, every component must be
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md),
chained the same way but rooted at whichever component's cycle isn't
itself another component's chronon. The result is underspecified
cyclical time (see
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md))
tagged with that root's cycle: `cyc(month, year) ~ 3` alone matches
[`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md)
for any March. Chained components collapse to one (chronon, cycle)
offset, e.g. `cyc(minute, hour) ~ 30` and `cyc(hour, day) ~ 3` give
minute-of-day 149. This is only possible when every link has a fixed
length: the number of days in a month varies, so day 29 of month 3 isn't
a fixed day of the year and is an error without a
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component. Supply a
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
component (or an existing linear vector) to locate it, e.g.
`time_compose(lin(year) ~ 2024, cyc(month, year) ~ 3, cyc(day, month) ~ 29)`.
Underspecified components (e.g.
[`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md))
are then validated against the located time and may be given in any
order.

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

## Time zones

Time zone aware components are composed in local (wall clock) time, and
the result is then resolved to the instant it describes. When daylight
saving ends, part of the day repeats, and a local time in it is
ambiguous. It is resolved, in order, by:

1.  a time zone aware
    [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
    component (e.g. `lin(hour)` from
    [`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md)),
    which already identifies one of the two occurrences;

2.  its `offset`;

3.  the `on_ambiguous` strategy, which warns if it wasn't chosen.

When daylight saving starts, part of the day is skipped. A local time in
it is resolved by its `offset` if given, and otherwise by the
`mixtime.on_invalid` option, as in
[`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_shift.md)
(see
[`vignette("invalid-time-points")`](https://pkg.mitchelloharawild.com/mixtime/dev/articles/invalid-time-points.md)).

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

# A lin() component alone is a valid (coarser) time point
time_compose(lin(year) ~ 1980)
#> <mixtime[1]>
#> [1] 1980

# No lin() component: composes underspecified cyclical time
time_compose(cyc(month, year) ~ 3)
#> <mixtime[1]>
#> [1] Mar

# Fixed-length chains collapse to one (chronon, cycle) pair: minute 30 of
# hour 3 becomes minute-of-day
time_compose(cyc(minute, hour) ~ 30, cyc(hour, day) ~ 3)
#> <mixtime[1]>
#> [1] 02:29

# Variable-length links (days in a month) need a lin() component
time_compose(lin(year) ~ 2024, day_of_month(29), month_of_year(2))
#> <mixtime[1]>
#> [1] 2024-02-29

# Round-tripping through time_components()
parts <- time_components(as.Date("2024-03-15"), yr = lin(year), mth = cyc(month, year))
with(parts, time_compose(yr, mth))
#> <mixtime[1]>
#> [1] 2024 Mar

# Repeated local times when daylight saving ends: 02:30 occurs twice in
# Melbourne on 5 April 2026, first in AEDT (+11) then in AEST (+10)
x <- datetime(c("2026-04-05 01:30:00", "2026-04-05 03:30:00"), tz = "Australia/Melbourne")
x <- c(x[1] + hours(1L), x[2] - hours(1L))
parts <- time_components(x, d = lin(day), h = cyc(hour, day), m = cyc(minute, hour))
with(parts, time_compose(d, h, m, offset = tz_offset(x)))
#> <mixtime[2]>
#> [1] 2026-04-05 02:30 AEDT 2026-04-05 02:30 AEST
with(parts, time_compose(d, h, m, offset = c("AEDT", "AEST")))
#> <mixtime[2]>
#> [1] 2026-04-05 02:30 AEDT 2026-04-05 02:30 AEST
with(parts, time_compose(d, h, m, on_ambiguous = "latest"))
#> <mixtime[2]>
#> [1] 2026-04-05 02:30 AEST 2026-04-05 02:30 AEST

# Multi-unit (self-referencing) cycles: the 3rd month (1-indexed) of the
# 4th 3-month block since epoch (block 3 = months 9-11 -> December 1970)
time_compose(lin(month(3L)) ~ 3, cyc(month(1L), month(3L)) ~ 3)
#> <mixtime[1]>
#> [1] 1970 Dec
```
