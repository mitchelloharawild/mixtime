# Time vector classes

The `mt_time` family are the S7 vector classes that store time points as
a numeric count of chronons. `mt_time` is the (internal) base class
carrying the `chronon` property; the common modes of time are:

- `mt_linear` - linear time points, typically produced with
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time.md).

- `mt_cyclical` - cyclical time points with additional `cycle` granule,
  typically produced with
  [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md).

- `mt_duration` - time durations, typically produced with
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/reference/duration.md).

The underlying data can be either **integer** (discrete time) or
**double** (continuous time). The `chronon` (and, for `mt_cyclical`, the
`cycle`) are time granules, the result from a
[mt_unit](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
object.

## Usage

``` r
mt_linear(.data = integer(), chronon = mt_unit(1L))

mt_duration(.data = integer(), chronon = mt_unit(1L))

mt_cyclical(.data = integer(), chronon = mt_unit(1L), cycle = mt_unit(1L))
```

## Arguments

- .data:

  A numeric vector of chronon counts (integer or double).

- chronon, cycle:

  Time granules
  ([mt_unit](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  objects) giving the unit of the chronon counts and, for `mt_cyclical`,
  the length of the cycle.

## Value

An S7 class object (used for method dispatch), or a time vector when
called as a constructor.

## See also

`mt_linear()`, `mt_duration()`, and `mt_cyclical()` to construct time
vectors, and
[mt_unit](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
for the granule type stored in `chronon`/`cycle`.
