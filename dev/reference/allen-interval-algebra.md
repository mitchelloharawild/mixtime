# Allen's interval algebra for time vectors

Allen's interval algebra has thirteen base relations for time intervals.
`==` is defined in
[mt_linear-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_linear-compare.md)/[mt_cyclical-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical-compare.md);
the twelve operators below define the rest, computed from the same
interval endpoints (`start`/`end`) that comparison derives. Together,
they cover all thirteen relations:

|  |  |  |
|----|----|----|
| Operator | Relation | `e1 %op% e2` holds iff |
| `%p%` | precedes | `e1` ends before `e2` begins (a gap) |
| `%m%` | meets | `e1` ends exactly where `e2` begins (no gap, no overlap) |
| `%o%` | overlaps | `e1` begins before `e2`, `e1` ends between the start and end of `e2` |
| `%s%` | starts | `e1` and `e2` begin together, and `e1` ends first |
| `%d%` | during | `e1` is strictly within `e2`'s span |
| `%f%` | finishes | `e1` and `e2` end together, and `e1` begins later |
| `==` | equals | `e1` and `e2` share both endpoints |
| `%pi%` | preceded by | `e2 %p% e1` |
| `%mi%` | met by | `e2 %m% e1` |
| `%oi%` | overlapped by | `e2 %o% e1` |
| `%si%` | started by | `e2 %s% e1` |
| `%di%` | contains | `e2 %d% e1` |
| `%fi%` | finished by | `e2 %f% e1` |

[mt_linear-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_linear-compare.md)/[mt_cyclical-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical-compare.md)'s
`<`/`>` are a deliberate deviations from their use as 'precedes' in
Allen's interval algebra (`%p%`/`%pi%` operators). The important
difference is that `<`/`>` include 'meets' relations, while `%p%`/`%pi%`
requires gaps, so in the examples below `jan < feb` is TRUE in mixtime
but the equivalent symbol in Allen's interval algebra (`jan %p% feb`) is
FALSE because there is no gap.

The implementation here also extends to continuous time instants, which
are treated as degenerate zero-width intervals. This allows testing how
a specific time instant relates to time spans, for example does January
2020 contain 30% through the day 2020-01-24 is
`yearmonth("2020-01") %di% date(datetime("2020-01-24 07:12:00"), discrete = FALSE)`.
This is a technical deviation from Allen's interval algebra, which
assumes that intervals are non-degenerate (i.e. end \> start, not end
\>= start).

## Usage

``` r
e1 %p% e2

e1 %pi% e2

e1 %m% e2

e1 %mi% e2

e1 %o% e2

e1 %oi% e2

e1 %s% e2

e1 %si% e2

e1 %d% e2

e1 %di% e2

e1 %f% e2

e1 %fi% e2
```

## Arguments

- e1, e2:

  `mt_linear` or `mt_cyclical` vectors sharing a mode of time (and, for
  `mt_cyclical`, a cycle) - or values castable to one, such as plain
  numeric vectors sharing the other operand's chronon. `mt_duration` has
  no interval to compare and so is not supported.

## Value

A logical vector.

## References

Allen, J. F. (1983). Maintaining knowledge about temporal intervals.
*Communications of the ACM*, 26(11), 832-843.

## See also

[mt_linear-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_linear-compare.md)
and
[mt_cyclical-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical-compare.md)
for `==`, `<`, `>`, `<=`, `>=` - the three of Allen's relations that
also form the package's ordering (`<`/`>` in a looser,
adjacency-inclusive form than `%p%`/`%pi%`).

## Examples

``` r
jan <- yearmonth("2020 Jan")
feb <- yearmonth("2020 Feb")
mar <- yearmonth("2020 Mar")
q1 <- yearquarter("2020 Q1")

jan == jan    # January equals itself
#> [1] TRUE

jan %p% mar   # January precedes March: February leaves a genuine gap
#> [1] TRUE
mar %pi% jan  # ... equivalently, March is preceded by January
#> [1] TRUE

jan %m% feb   # January meets February: adjacent, no gap
#> [1] TRUE
feb %mi% jan  # ... equivalently, February is met by January
#> [1] TRUE
jan < feb     # `<` also holds for adjacent pairs, unlike `%p%`
#> [1] TRUE
jan %p% feb   # FALSE: no gap between them, so they don't (strictly) precede
#> [1] FALSE

jan %s% q1    # January and Q1 start together, but January finishes first
#> [1] TRUE
q1 %si% jan   # ... equivalently, Q1 is started by January
#> [1] TRUE

feb %d% q1    # February is strictly within Q1
#> [1] TRUE
q1 %di% feb   # ... equivalently, Q1 contains February
#> [1] TRUE

mar %f% q1    # March and Q1 finish together, but March starts later
#> [1] TRUE
q1 %fi% mar   # ... equivalently, Q1 is finished by March
#> [1] TRUE
```
