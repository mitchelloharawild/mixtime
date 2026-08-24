# Comparison operators for cyclical time (`mt_cyclical`)

A cyclical time value stores an absolute chronon count but *means* a
position within its cycle (e.g.
[`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md)
means a weekday, not a particular Wednesday). Comparison therefore
reduces both operands to their position within the cycle - the same
reduction [`format()`](https://rdrr.io/r/base/format.html) displays -
and compares those:

- `day_of_week(date("2020-01-15")) == day_of_week(date("2020-01-22"))`
  is `TRUE`, since both are a Wednesday.

- `day_of_year(date("2020-01-15")) == day_of_year(date("2021-01-15"))`
  is `TRUE`, since both are the 15th day of their year.

Both operands must share a `cycle`: a cycle is a modulus rather than a
unit, so there is no meaningful common cycle between (say) a weekday and
a day-of-year, and comparing them is an error. Differing *chronons*
within a shared cycle are reconciled as they are for
[mt_linear](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_linear-compare.md):
both positions are expressed in the finest common chronon, and a
discrete value spans the closed interval of its chronon, so

- `a == b` iff `start(a) == start(b)` and `end(a) == end(b)`

- `a < b` iff `end(a) < start(b)`, `a > b` iff `start(a) > end(b)`

- `a <= b` iff `end(a) <= end(b)`, `a >= b` iff `start(a) >= start(b)`

As for `mt_linear`, this is not a total order when chronons differ.
Ordering follows the position within the cycle (so `Mon < Wed`); the
cycle's wrap-around is not treated as circular.

## Usage

``` r
# S4 method for class 'mt_cyclical'
e1 == e2
# S4 method for class 'mt_cyclical'
e1 != e2
# S4 method for class 'mt_cyclical'
e1 < e2
# S4 method for class 'mt_cyclical'
e1 <= e2
# S4 method for class 'mt_cyclical'
e1 > e2
# S4 method for class 'mt_cyclical'
e1 >= e2
```

## Arguments

- e1, e2:

  `mt_cyclical` vectors sharing a cycle (or values castable to one, such
  as plain numeric vectors sharing the other operand's chronon).

## Value

A logical vector.
