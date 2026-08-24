# Comparison operators for durations (`mt_duration`)

A duration is a scalar magnitude of time measured in a given chronon
(e.g. `days(3)`), with no reference to a point in time. Comparing two
durations is therefore a plain magnitude comparison, once both operands
have been expressed in a common chronon:

- `a == b` iff the two magnitudes are equal in their common chronon

- `a < b`, `a <= b`, `a > b`, `a >= b` compare the magnitudes directly

Unlike `mt_linear` comparison there is no interval/span to consider, so
this *is* a total order. When both operands already share a chronon the
magnitudes are compared as-is; otherwise both are scaled to their finest
common chronon (the same scaling used when combining durations
arithmetically, see `duration_combine()`).

## Usage

``` r
# S4 method for class 'mt_duration'
e1 == e2
# S4 method for class 'mt_duration'
e1 != e2
# S4 method for class 'mt_duration'
e1 < e2
# S4 method for class 'mt_duration'
e1 <= e2
# S4 method for class 'mt_duration'
e1 > e2
# S4 method for class 'mt_duration'
e1 >= e2
```

## Arguments

- e1, e2:

  `mt_duration` vectors (or values castable to one, such as plain
  numeric vectors interpreted in the other operand's chronon).

## Value

A logical vector.
