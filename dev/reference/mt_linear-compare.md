# Comparison operators for linear time (`mt_linear`)

Discrete linear time values represent a *closed interval* spanning their
chronon (e.g. `year(2020)` spans every instant from the start of 2020 to
the end of 2020), while continuous linear time values represent a single
instant. Comparing two `mt_linear` vectors therefore compares the
start/end instants of the (possibly zero-width) interval each value
represents:

- `a == b` iff `start(a) == start(b)` and `end(a) == end(b)`

- `a < b` iff `end(a) <= start(b)` (i.e. before with or without a gap)

- `a > b` iff `start(a) >= end(b)` (i.e. after with or without a gap)

- `a <= b` iff `end(a) <= end(b)` (right-bound comparison)

- `a >= b` iff `start(a) >= start(b)` (left-bound comparison)

This is not a total order for comparisons between time points at
different granularities: `<=`/`>=` are **not** shorthand for
`(< or ==)`/`(> or ==)`. For example,
`yearquarter("2020 Q3") <= year("2020")` is TRUE despite
`yearquarter("2020 Q3") < year("2020")` and
`yearquarter("2020 Q3") == year("2020")` being FALSE.

The inequality operators `<`/`>` and `<=`/`>=` are useful conjugations
of Allen's interval algebra for common data manipulation needs. The
complete set of Allen's 13 base relations are documented in
[allen-interval-algebra](https://pkg.mitchelloharawild.com/mixtime/dev/reference/allen-interval-algebra.md).

## Usage

``` r
# S4 method for class 'mt_linear'
e1 == e2
# S4 method for class 'mt_linear'
e1 != e2
# S4 method for class 'mt_linear'
e1 < e2
# S4 method for class 'mt_linear'
e1 <= e2
# S4 method for class 'mt_linear'
e1 > e2
# S4 method for class 'mt_linear'
e1 >= e2
```

## Arguments

- e1, e2:

  `mt_linear` vectors (or values castable to one, such as plain numeric
  vectors sharing the other operand's chronon).

## Value

A logical vector.
