# Comparison operators for linear time (`mt_linear`)

Discrete linear time values represent a *closed interval* spanning their
chronon (e.g. `year(2020)` spans every instant from the start of 2020 to
the end of 2020), while continuous linear time values represent a single
instant. Comparing two `mt_linear` vectors therefore compares the
start/end instants of the (possibly zero-width) interval each value
represents:

- `a == b` iff `start(a) == start(b)` and `end(a) == end(b)`

- `a < b` iff `end(a) < start(b)`

- `a > b` iff `start(a) > end(b)`

- `a <= b` iff `end(a) <= end(b)` (right-bound comparison)

- `a >= b` iff `start(a) >= start(b)` (left-bound comparison)

This is **not** a total order: `<=`/`>=` are endpoint comparisons, not
shorthand for `(< or ==)`/`(> or ==)`, so it is possible for none of
`==`, `<`, `>` to hold between two values.

If both operands are continuous (fractional chronons), or share an
identical chronon, the comparison simplifies to a direct numeric
comparison, since there is no interval to consider.

## Arguments

- e1, e2:

  `mt_linear` vectors (or values castable to one, such as plain numeric
  vectors sharing the other operand's chronon).

## Value

A logical vector.
