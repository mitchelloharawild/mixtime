# Time vector classes

The `mt_time` family are the S7 vector classes that store time points as
a numeric count of chronons. `mt_time` is the (internal) base class
carrying the `chronon` property; the common modes of time are:

- `mt_linear` - linear time points, typically produced with
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md).

- `mt_cyclical` - located cyclical time: positions within a `cycle`
  granule that know their enclosing cycle, typically produced with
  [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md)
  from time points.

- `mt_duration` - time durations, typically produced with
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md).

- `mt_cyclical_free` - underspecified cyclical time: positions within a
  cycle whose enclosing cycle is unknown (e.g. "day 60 of the year" with
  no year known), produced by
  [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md)
  with bare numeric input, or by
  [`time_compose()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_compose.md)/[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)
  with no
  [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
  component.

The underlying data can be either **integer** (discrete time) or
**double** (continuous time). The `chronon` (and, for `mt_cyclical`, the
`cycle`) are time granules, the result from a
[mt_unit](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
object.

## Usage

``` r
mt_cyclical_free(..., chronon = mt_unit(1L), cycle = mt_unit(1L))
```

## Arguments

- ...:

  Passed to the parent constructor, typically `.data`: a numeric vector
  of chronon counts (integer or double).

- chronon, cycle:

  Time granules
  ([mt_unit](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
  objects) giving the unit of the chronon counts and, for `mt_cyclical`,
  the length of the cycle.

## Value

An S7 class object (used for method dispatch), or a time vector when
called as a constructor.

## See also

`mt_linear()`, `mt_duration()`, and `mt_cyclical()` to construct time
vectors, and
[mt_unit](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
for the granule type stored in `chronon`/`cycle`.
