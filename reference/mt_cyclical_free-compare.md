# Comparison operators for underspecified cyclical time (`mt_cyclical_free`)

Underspecified cyclical time (see
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/cyclical_time.md))
is a magnitude within a cycle whose enclosing cycle is unknown (e.g.
`day_of_year(60)`, "day 60 of the year", with no year known) - modelled
on
[mt_duration](https://pkg.mitchelloharawild.com/mixtime/reference/mt_time-class.md),
not
[mt_cyclical](https://pkg.mitchelloharawild.com/mixtime/reference/mt_time-class.md).
Comparing two free values is therefore
[mt_duration-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_duration-compare.md)'s
plain magnitude comparison, not
[mt_cyclical-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_cyclical-compare.md)'s
interval/position reduction:

- `a == b` iff the two magnitudes are equal in their common chronon

- `a < b`, `a <= b`, `a > b`, `a >= b` compare the magnitudes directly

Both operands must share a `cycle` (a type-compatibility tag, not a
unit - see
[mt_cyclical-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_cyclical-compare.md)):
comparing (say) a day-of-week-free against a day-of-year-free is an
error. Differing chronons within a shared cycle are reconciled by
scaling both magnitudes to their finest common chronon via
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md) -
never
[`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_divmod.md),
which would assert that the stored number's position is measured
relative to some specific enclosing cycle, a claim that is false by
construction for data defined to carry no knowledge of which cycle it
falls in.

Underspecified cyclical time can also be compared against located
[mt_cyclical](https://pkg.mitchelloharawild.com/mixtime/reference/mt_time-class.md):
the located side reduces to its position within the cycle via its own
existing, unchanged machinery (the same reduction
[`format()`](https://rdrr.io/r/base/format.html) and
[mt_cyclical-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_cyclical-compare.md)
use), and both sides are then compared as magnitudes.

Positions are ordered linearly within one cycle (January \< ... \<
December, with no wrapping), which
[`sort()`](https://rdrr.io/r/base/sort.html),
[`order()`](https://rdrr.io/r/base/order.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html) and
[`range()`](https://rdrr.io/r/base/range.html) follow. Subtraction
follows the same order: `a - b` between free values of the same cycle is
the duration between their positions, so
`month_of_year(3) - month_of_year(1)` is 2 months and
`month_of_year(1) - month_of_year(3)` is -2 months (see
[mt_cyclical_free-arithmetic](https://pkg.mitchelloharawild.com/mixtime/reference/mt_cyclical_free-arithmetic.md)
for the arithmetic rules).

Unlike `mt_cyclical`, this is a total order (there is no interval to
compare - see
[mt_duration-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_duration-compare.md)),
so Allen's interval algebra (`%p%`, `%m%`, ...) is not supported,
matching `mt_duration`'s own documented exclusion.

## Usage

``` r
# S4 method for class 'mt_cyclical_free'
e1 == e2
# S4 method for class 'mt_cyclical_free'
e1 != e2
# S4 method for class 'mt_cyclical_free'
e1 < e2
# S4 method for class 'mt_cyclical_free'
e1 <= e2
# S4 method for class 'mt_cyclical_free'
e1 > e2
# S4 method for class 'mt_cyclical_free'
e1 >= e2
```

## Arguments

- e1, e2:

  `mt_cyclical_free` vectors sharing a cycle (or values castable to one,
  such as plain numeric vectors sharing the other operand's chronon), or
  one `mt_cyclical_free` and one located `mt_cyclical` sharing a cycle.

## Value

A logical vector.
