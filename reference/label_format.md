# Render and parse a granule's labels

The generics turning a granule's internal position into display text
(`linear_labels_format()` for a non-repeating position, e.g. the year;
`cyclical_labels_format()` for a position within a larger cycle, e.g.
the month within the year) and back (`linear_labels_parse()`/
`cyclical_labels_parse()`).

## Usage

``` r
linear_labels_format(granule, ...)

cyclical_labels_format(granule, cycle, ...)

linear_labels_parse(granule, ...)

cyclical_labels_parse(granule, cycle, ...)
```

## Arguments

- granule:

  A time granule object representing the granule (e.g. `month(1L)`).

- ...:

  Passed on to the method. The default method (see below) takes:

  - `i`: Integer vector: the position along the linear axis, or within
    the cycle for `cyclical_labels_format()`.

  - `at`: The linear position of the cycle granule, letting a method
    produce labels specific to that cycle instance (mainly for irregular
    cycles, e.g. a leap month). Same convention as `at` in
    [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md).

  - `label`: If `TRUE`, return named labels (e.g. "February"). If
    `FALSE`, return the numeric position as character.

  - `abbreviate`: If `TRUE`, return abbreviated labels (e.g. "Feb"). If
    `FALSE`, return full labels (e.g. "February").

  - `type`: Overrides `abbreviate`'s abbreviated/wide choice of
    [`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
    type outright, e.g. `type = "emoji"`. `NULL` (default) defers to
    `abbreviate`.

  - `width`,`locale`: Call-time overrides of the scheme's
    `width`/`locale` defaults (see
    [`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md));
    `NULL` defers to the scheme.

- cycle:

  A time granule object representing the cycle (e.g. `year(1L)`).

## Value

Character vector of labels for the time point.

## Details

Each has one default method (on the base `mt_unit` class) that looks up
the granule's scheme via
[`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)/[`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)
and applies it (see
[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)
for the fields). Calendar authors usually declare a scheme instead of
writing a method here directly. Writing one directly overrides the
generics for labels that aren't index-shaped at all, see
[`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)'s
"Overriding the generics directly" section.

## Epoch shift

`linear_labels_format()`'s `i` arrives already epoch-shifted for display
(`chronon_parts()` adds
[`chronon_epoch()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_epoch.md)
before calling it, e.g. a Gregorian year's `i` is `2024`, not `54`). The
default `linear_labels_parse()` method shifts `decode()`'s output back,
by subtracting
[`chronon_epoch()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_epoch.md)
after the scheme's own decode runs. A hand-written override method skips
this and must do the same subtraction itself if the granule has a
non-zero epoch; see the example in
[`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md).

## See also

[`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)/[`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)
for the authoring interface these generics' default methods interpret.

## Examples

``` r
# Labels for years on a linear axis
with(cal_gregorian, linear_labels_format(year(1L), 2020:2025))
#> [1] 2020 2021 2022 2023 2024 2025

# Labels for months in a year
with(cal_gregorian, cyclical_labels_format(month(1L), year(1L), 0:11))
#>  [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"
```
