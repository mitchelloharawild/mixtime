# Time units as a string

**\[deprecated\]**

## Usage

``` r
time_unit_full(x)

time_unit_plural(x, n = 2L)

time_unit_abbr(x)
```

## Arguments

- x:

  A time granule object (e.g., `cal_gregorian$month(1L)`)

- n:

  Numeric quantity used to select singular or plural form.

## Value

A string representing the time unit

## Details

These functions are superseded by
[`unit_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/unit_scheme.md)
and
[`unit_scheme()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/unit_scheme.md),
and are only kept as thin wrappers for backwards compatibility.
`time_unit_full()` returns the full unit name template (e.g.
`"year{?/s}"`), `time_unit_plural()` pluralises it for a quantity, and
`time_unit_abbr()` returns the unit symbol (e.g. `"Y"`).
