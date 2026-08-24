# Extract linear and cyclical time components

`time_components()` decomposes a time vector into its constituent parts
using
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)-like
semantics. Each named expression is built from the
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
and
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
helpers (the same vocabulary used in
[`format()`](https://rdrr.io/r/base/format.html) strings) and produces a
component time vector:

## Usage

``` r
time_components(x, ..., calendar = time_calendar(x))
```

## Arguments

- x:

  A `mixtime` (or an object coercible to one via
  [`as_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/as_mixtime.md),
  such as a `Date` or `POSIXct`).

- ...:

  Named expressions using
  [`lin()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
  and
  [`cyc()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
  describing the components to extract. The granule names (e.g. `year`,
  `month`, `day`) are resolved in the calendar of `x`.

- calendar:

  Calendar system used to resolve granule names, overlaid on the
  calendar of `x`. Defaults to `time_calendar(x)`. Supply e.g.
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/reference/calendar_isoweek.md)
  to make ISO `week`-based components available.

## Value

A data frame with one column per requested component.
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
columns are linear (`mt_linear`) time vectors and
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
columns are cyclical (`mt_cyclical`) time vectors.

## Details

- `lin(<granule>)` extracts a **linear** component (a non-repeating
  count, e.g. the year), returning a linear time vector.

- `cyc(<granule>, <cycle>)` extracts a **cyclical** component (a
  repeating position within a larger cycle, e.g. the month within the
  year), returning a cyclical time vector.

All requested components are computed together in a single decomposition
of the underlying time vector (via `chronon_parts()`), reusing the
shared recursive
[`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_divmod.md)
results rather than converting each component independently.

## See also

[`lin()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
and
[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
for the component helpers,
[`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md)
and
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md)
for constructing individual component vectors, and
[`format()`](https://rdrr.io/r/base/format.html) for the string
counterpart of this interface.

## Examples

``` r
t <- yearmonth(as.Date("2026-02-14") + c(0, 40, 400))

# Extract the year (linear) and month-of-year (cyclical)
time_components(t, yr = lin(year), mth = cyc(month, year))
#> # A tibble: 3 × 2
#>   yr        mth      
#>   <mixtime> <mixtime>
#> 1 2026      Feb      
#> 2 2026      Mar      
#> 3 2027      Mar      

# Components can be named automatically from the expression
time_components(as.Date("2025-12-15") + 0:3, cyc(day, cal_isoweek$week))
#> # A tibble: 4 × 1
#>   `cyc(day, cal_isoweek$week)`
#>   <mixtime>                   
#> 1 Mon                         
#> 2 Tue                         
#> 3 Wed                         
#> 4 Thu                         
```
