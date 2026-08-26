#' Comparison operators for linear time (`mt_linear`)
#'
#' @description
#' Discrete linear time values represent a *closed interval* spanning their
#' chronon (e.g. `year(2020)` spans every instant from the start of 2020 to
#' the end of 2020), while continuous linear time values represent a single
#' instant. Comparing two `mt_linear` vectors therefore compares the
#' start/end instants of the (possibly zero-width) interval each value
#' represents:
#'
#' - `a == b` iff `start(a) == start(b)` and `end(a) == end(b)`
#' - `a < b` iff `end(a) <= start(b)` (i.e. before with or without a gap)
#' - `a > b` iff `start(a) >= end(b)` (i.e. after with or without a gap)
#' - `a <= b` iff `end(a) <= end(b)` (right-bound comparison)
#' - `a >= b` iff `start(a) >= start(b)` (left-bound comparison)
#'
#' This is not a total order for comparisons between time points at different
#' granularities: `<=`/`>=` are **not** shorthand for `(< or ==)`/`(> or ==)`.
#' For example, `yearquarter("2020 Q3") <= year("2020")` is TRUE despite 
#' `yearquarter("2020 Q3") < year("2020")` and 
#' `yearquarter("2020 Q3") == year("2020")` being FALSE.
#' 
#' The inequality operators `<`/`>` and `<=`/`>=` are useful conjugations of
#' Allen's interval algebra for common data manipulation needs. The complete set
#' of Allen's 13 base relations are documented in [allen-interval-algebra].
#' 
#' @param e1,e2 `mt_linear` vectors (or values castable to one, such as
#'   plain numeric vectors sharing the other operand's chronon).
#'
#' @return A logical vector.
#'
#' @usage
#' \S4method{==}{mt_linear}(e1, e2)
#' \S4method{!=}{mt_linear}(e1, e2)
#' \S4method{<}{mt_linear}(e1, e2)
#' \S4method{<=}{mt_linear}(e1, e2)
#' \S4method{>}{mt_linear}(e1, e2)
#' \S4method{>=}{mt_linear}(e1, e2)
#'
#' @aliases ==,mt_linear-method !=,mt_linear-method <,mt_linear-method <=,mt_linear-method >,mt_linear-method >=,mt_linear-method
#' @name mt_linear-compare
NULL

# Express both operands as half-open intervals ([start, end)) of real chronon
# instants in their common (finest shared) chronon, then defer to
# `interval_relation()`. A discrete value spans a whole common-chronon unit
# ([v, v+1)); a continuous value is a zero-width instant ([v, v)).
linear_compare <- function(op, e1, e2) {
  if (!S7_inherits(e1, mt_linear)) e1 <- vec_cast(e1, e2)
  if (!S7_inherits(e2, mt_linear)) e2 <- vec_cast(e2, e1)

  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  xv <- S7_data(e1)
  yv <- S7_data(e2)
  x_discrete <- is.integer(xv)
  y_discrete <- is.integer(yv)

  if (identical(x_chronon, y_chronon)) {
    # No conversion needed: a shared chronon unit is always exactly 1 wide
    x_start <- as.double(xv)
    y_start <- as.double(yv)
    x_end <- if (x_discrete) x_start + 1 else x_start
    y_end <- if (y_discrete) y_start + 1 else y_start
  } else {
    # Convert to the common chronon before measuring each operand's width in it
    common <- chronon_common_impl(list(x_chronon, y_chronon))
    x_start <- as.double(chronon_convert_impl(xv, x_chronon, common, discrete = x_discrete))
    y_start <- as.double(chronon_convert_impl(yv, y_chronon, common, discrete = y_discrete))

    x_end <- x_start
    if (x_discrete) x_end <- x_start + chronon_cardinality(common, x_chronon, at = xv)
    y_end <- y_start
    if (y_discrete) y_end <- y_start + chronon_cardinality(common, y_chronon, at = yv)
  }

  interval_relation(op, list(x_start, x_end), list(y_start, y_end))
}

method(`==`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("==", e1, e2)
method(`==`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("==", e1, e2)

method(`!=`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("!=", e1, e2)
method(`!=`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("!=", e1, e2)

method(`<`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("<", e1, e2)
method(`<`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("<", e1, e2)

method(`<=`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("<=", e1, e2)
method(`<=`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("<=", e1, e2)

method(`>`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare(">", e1, e2)
method(`>`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare(">", e1, e2)

method(`>=`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare(">=", e1, e2)
method(`>=`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare(">=", e1, e2)

#' Comparison operators for durations (`mt_duration`)
#'
#' @description
#' A duration is a scalar magnitude of time measured in a given chronon (e.g.
#' `days(3)`), with no reference to a point in time. Comparing two durations is
#' therefore a plain magnitude comparison, once both operands have been
#' expressed in a common chronon:
#'
#' - `a == b` iff the two magnitudes are equal in their common chronon
#' - `a < b`, `a <= b`, `a > b`, `a >= b` compare the magnitudes directly
#'
#' Unlike `mt_linear` comparison there is no interval/span to consider, so this
#' *is* a total order. When both operands already share a chronon the magnitudes
#' are compared as-is; otherwise both are scaled to their finest common chronon
#' (the same scaling used when combining durations arithmetically, see
#' `duration_combine()`).
#'
#' @param e1,e2 `mt_duration` vectors (or values castable to one, such as plain
#'   numeric vectors interpreted in the other operand's chronon).
#'
#' @return A logical vector.
#'
#' @usage
#' \S4method{==}{mt_duration}(e1, e2)
#' \S4method{!=}{mt_duration}(e1, e2)
#' \S4method{<}{mt_duration}(e1, e2)
#' \S4method{<=}{mt_duration}(e1, e2)
#' \S4method{>}{mt_duration}(e1, e2)
#' \S4method{>=}{mt_duration}(e1, e2)
#'
#' @aliases ==,mt_duration-method !=,mt_duration-method <,mt_duration-method <=,mt_duration-method >,mt_duration-method >=,mt_duration-method
#' @name mt_duration-compare
NULL

duration_compare <- function(op, e1, e2) {
  if (!S7_inherits(e1, mt_duration)) e1 <- vec_cast(e1, e2)
  if (!S7_inherits(e2, mt_duration)) e2 <- vec_cast(e2, e1)

  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  xd <- S7_data(e1)
  yd <- S7_data(e2)

  if (!identical(x_chronon, y_chronon)) {
    # Scale both magnitudes to their finest common chronon (mirrors `duration_combine()`)
    tu <- chronon_common_impl(list(x_chronon, y_chronon))
    xd <- xd * chronon_cardinality(tu, x_chronon)
    yd <- yd * chronon_cardinality(tu, y_chronon)
  }

  switch(
    op,
    "==" = xd == yd,
    "!=" = xd != yd,
    "<"  = xd < yd,
    ">"  = xd > yd,
    "<=" = xd <= yd,
    ">=" = xd >= yd
  )
}

method(`==`, list(mt_duration, class_any)) <- function(e1, e2) duration_compare("==", e1, e2)
method(`==`, list(class_any, mt_duration)) <- function(e1, e2) duration_compare("==", e1, e2)

method(`!=`, list(mt_duration, class_any)) <- function(e1, e2) duration_compare("!=", e1, e2)
method(`!=`, list(class_any, mt_duration)) <- function(e1, e2) duration_compare("!=", e1, e2)

method(`<`, list(mt_duration, class_any)) <- function(e1, e2) duration_compare("<", e1, e2)
method(`<`, list(class_any, mt_duration)) <- function(e1, e2) duration_compare("<", e1, e2)

method(`<=`, list(mt_duration, class_any)) <- function(e1, e2) duration_compare("<=", e1, e2)
method(`<=`, list(class_any, mt_duration)) <- function(e1, e2) duration_compare("<=", e1, e2)

method(`>`, list(mt_duration, class_any)) <- function(e1, e2) duration_compare(">", e1, e2)
method(`>`, list(class_any, mt_duration)) <- function(e1, e2) duration_compare(">", e1, e2)

method(`>=`, list(mt_duration, class_any)) <- function(e1, e2) duration_compare(">=", e1, e2)
method(`>=`, list(class_any, mt_duration)) <- function(e1, e2) duration_compare(">=", e1, e2)

#' Comparison operators for cyclical time (`mt_cyclical`)
#'
#' @description
#' A cyclical time value stores an absolute chronon count but *means* a position
#' within its cycle (e.g. `day_of_week()` means a weekday, not a particular
#' Wednesday). Comparison therefore reduces both operands to their position
#' within the cycle - the same reduction [format()] displays - and compares
#' those:
#'
#' - `day_of_week(date("2020-01-15")) == day_of_week(date("2020-01-22"))` is
#'   `TRUE`, since both are a Wednesday.
#' - `day_of_year(date("2020-01-15")) == day_of_year(date("2021-01-15"))` is
#'   `TRUE`, since both are the 15th day of their year.
#'
#' Both operands must share a `cycle`: a cycle is a modulus rather than a unit,
#' so there is no meaningful common cycle between (say) a weekday and a
#' day-of-year, and comparing them is an error. Differing *chronons* within a
#' shared cycle are reconciled as they are for [mt_linear][mt_linear-compare]:
#' both positions are expressed in the finest common chronon, and a discrete
#' value spans the closed interval of its chronon, so
#'
#' - `a == b` iff `start(a) == start(b)` and `end(a) == end(b)`
#' - `a < b`/`a > b` iff `a`/`b` ends at or before the other begins (a gap, or
#'   just adjacency - see [mt_linear-compare] for why this includes adjacency)
#' - `a <= b` iff `end(a) <= end(b)`, `a >= b` iff `start(a) >= start(b)`
#'
#' As for `mt_linear`, this is not a total order when chronons differ. Ordering
#' follows the position within the cycle (so `Mon < Wed`); the cycle's wrap-around
#' is not treated as circular.
#'
#' @param e1,e2 `mt_cyclical` vectors sharing a cycle (or values castable to one,
#'   such as plain numeric vectors sharing the other operand's chronon).
#'
#' @return A logical vector.
#'
#' @usage
#' \S4method{==}{mt_cyclical}(e1, e2)
#' \S4method{!=}{mt_cyclical}(e1, e2)
#' \S4method{<}{mt_cyclical}(e1, e2)
#' \S4method{<=}{mt_cyclical}(e1, e2)
#' \S4method{>}{mt_cyclical}(e1, e2)
#' \S4method{>=}{mt_cyclical}(e1, e2)
#'
#' @aliases ==,mt_cyclical-method !=,mt_cyclical-method <,mt_cyclical-method <=,mt_cyclical-method >,mt_cyclical-method >=,mt_cyclical-method
#' @name mt_cyclical-compare
NULL

# Reduce an absolute chronon count to its position within `cycle`, as format()
# displays: the integer position from chronon_parts(), plus the fraction a
# continuous (double) time model carries but chronon_parts() floors away.
cyclical_position <- function(x, chronon, cycle) {
  x <- mt_cyclical(x, chronon = chronon, cycle = cycle)
  pos <- chronon_parts(x, cyclical = list(list(chronon, cycle)))$cyclical[[1L]]
  xd <- S7_data(x)
  if (is.double(xd)) {
    xd <- xd + tz_offset_impl(xd, attr(x, "chronon"))
    pos <- pos + (xd - floor(xd))
  }
  pos
}

# As `linear_compare()`, but positions are first reduced to their place
# within the shared cycle before forming half-open interval endpoints.
cyclical_compare <- function(op, e1, e2) {
  if (!S7_inherits(e1, mt_cyclical)) e1 <- vec_cast(e1, e2)
  if (!S7_inherits(e2, mt_cyclical)) e2 <- vec_cast(e2, e1)

  # A cycle is a modulus, not a unit, so differing cycles have no common cycle
  cycle <- cycle_common(e1@cycle, e2@cycle)
  if (is.null(cycle)) {
    vctrs::stop_incompatible_op(op, e1, e2, details = cycle_incompatible_details)
  }

  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  xv <- S7_data(e1)
  yv <- S7_data(e2)
  x_discrete <- is.integer(xv)
  y_discrete <- is.integer(yv)

  if (identical(x_chronon, y_chronon)) {
    # No conversion needed: a shared chronon unit is always exactly 1 wide
    x_start <- cyclical_position(xv, x_chronon, cycle)
    y_start <- cyclical_position(yv, y_chronon, cycle)
    x_end <- if (x_discrete) x_start + 1 else x_start
    y_end <- if (y_discrete) y_start + 1 else y_start
  } else {
    # Convert to the common chronon before reducing to a cycle position and
    # measuring each operand's width in it, mirroring `linear_compare()`
    common <- chronon_common_impl(list(x_chronon, y_chronon))
    x_start <- cyclical_position(
      chronon_convert_impl(xv, x_chronon, common, discrete = x_discrete),
      common, cycle
    )
    y_start <- cyclical_position(
      chronon_convert_impl(yv, y_chronon, common, discrete = y_discrete),
      common, cycle
    )

    x_end <- x_start
    if (x_discrete) x_end <- x_start + chronon_cardinality(common, x_chronon, at = xv)
    y_end <- y_start
    if (y_discrete) y_end <- y_start + chronon_cardinality(common, y_chronon, at = yv)
  }

  interval_relation(op, list(x_start, x_end), list(y_start, y_end))
}

method(`==`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("==", e1, e2)
method(`==`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("==", e1, e2)

method(`!=`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("!=", e1, e2)
method(`!=`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("!=", e1, e2)

method(`<`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("<", e1, e2)
method(`<`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("<", e1, e2)

method(`<=`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("<=", e1, e2)
method(`<=`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("<=", e1, e2)

method(`>`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare(">", e1, e2)
method(`>`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare(">", e1, e2)

method(`>=`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare(">=", e1, e2)
method(`>=`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare(">=", e1, e2)

interval_relation <- function(op, e1, e2) {
  switch(
    op,
    "==" = e1[[1L]] == e2[[1L]] & e1[[2L]] == e2[[2L]],
    "!=" = !(e1[[1L]] == e2[[1L]] & e1[[2L]] == e2[[2L]]),
    "<"  = e1[[2L]] <= e2[[1L]],
    ">"  = e1[[1L]] >= e2[[2L]],
    "<=" = e1[[2L]] <= e2[[2L]],
    ">=" = e1[[1L]] >= e2[[1L]],
    "%p%"  = e1[[2L]] <  e2[[1L]],
    "%pi%" = e2[[2L]] <  e1[[1L]],
    "%m%"  = e1[[2L]] == e2[[1L]],
    "%mi%" = e2[[2L]] == e1[[1L]],
    "%o%"  = e1[[1L]] <  e2[[1L]] & e2[[1L]] <  e1[[2L]] & e1[[2L]] <  e2[[2L]],
    "%oi%" = e2[[1L]] <  e1[[1L]] & e1[[1L]] <  e2[[2L]] & e2[[2L]] <  e1[[2L]],
    "%s%"  = e1[[1L]] == e2[[1L]] & e1[[2L]] <  e2[[2L]],
    "%si%" = e1[[1L]] == e2[[1L]] & e1[[2L]] >  e2[[2L]],
    "%d%"  = e1[[1L]] >  e2[[1L]] & e1[[2L]] <  e2[[2L]],
    "%di%" = e1[[1L]] <  e2[[1L]] & e1[[2L]] >  e2[[2L]],
    "%f%"  = e1[[2L]] == e2[[2L]] & e1[[1L]] >  e2[[1L]],
    "%fi%" = e1[[2L]] == e2[[2L]] & e1[[1L]] <  e2[[1L]]
  )
}

#' Allen's interval algebra for time vectors
#'
#' @description
#' Allen's interval algebra has thirteen base relations for time intervals.
#' `==` is defined in [mt_linear-compare]/[mt_cyclical-compare]; the twelve
#' operators below define the rest, computed from the same interval endpoints
#' (`start`/`end`) that comparison derives. Together, they cover all thirteen
#' relations:
#'
#' | Operator | Relation      | `e1 %op% e2` holds iff                                    |
#' | -------- | ------------- | ---------------------------------------------------------- |
#' | `%p%`    | precedes      | `e1` ends before `e2` begins (a gap)                        |
#' | `%m%`    | meets         | `e1` ends exactly where `e2` begins (no gap, no overlap)    |
#' | `%o%`    | overlaps      | `e1` begins before `e2`, `e1` ends between the start and end of `e2` |
#' | `%s%`    | starts        | `e1` and `e2` begin together, and `e1` ends first           |
#' | `%d%`    | during        | `e1` is strictly within `e2`'s span                         |
#' | `%f%`    | finishes      | `e1` and `e2` end together, and `e1` begins later           |
#' | `==`     | equals        | `e1` and `e2` share both endpoints                          |
#' | `%pi%`   | preceded by   | `e2 %p% e1`                                                 |
#' | `%mi%`   | met by        | `e2 %m% e1`                                                 |
#' | `%oi%`   | overlapped by | `e2 %o% e1`                                                 |
#' | `%si%`   | started by    | `e2 %s% e1`                                                 |
#' | `%di%`   | contains      | `e2 %d% e1`                                                 |
#' | `%fi%`   | finished by   | `e2 %f% e1`                                                 |
#'
#' [mt_linear-compare]/[mt_cyclical-compare]'s `<`/`>` are a deliberate
#' deviations from their use as 'precedes' in Allen's interval algebra
#' (`%p%`/`%pi%` operators). The important difference is that `<`/`>` include
#' 'meets' relations, while `%p%`/`%pi%` requires gaps, so in the examples
#' below `jan < feb` is TRUE in mixtime but the equivalent symbol in Allen's
#' interval algebra (`jan %p% feb`) is FALSE because there is no gap.
#'
#' The implementation here also extends to continuous time instants, which are
#' treated as degenerate zero-width intervals. This allows testing how a 
#' specific time instant relates to time spans, for example does January 2020
#' contain 30% through the day 2020-01-24 is 
#' `yearmonth("2020-01") %di% date(datetime("2020-01-24 07:12:00"), discrete = FALSE)`.
#' This is a technical deviation from Allen's interval algebra, which assumes
#' that intervals are non-degenerate (i.e. end > start, not end >= start).
#' 
#' @param e1,e2 `mt_linear` or `mt_cyclical` vectors sharing a mode of time
#'   (and, for `mt_cyclical`, a cycle) - or values castable to one, such as
#'   plain numeric vectors sharing the other operand's chronon. `mt_duration`
#'   has no interval to compare and so is not supported.
#'
#' @return A logical vector.
#'
#' @seealso [mt_linear-compare] and [mt_cyclical-compare] for `==`, `<`, `>`,
#'   `<=`, `>=` - the three of Allen's relations that also form the package's
#'   ordering (`<`/`>` in a looser, adjacency-inclusive form than `%p%`/`%pi%`).
#'
#' @references
#' Allen, J. F. (1983). Maintaining knowledge about temporal intervals.
#' *Communications of the ACM*, 26(11), 832-843.
#'
#' @examples
#' jan <- yearmonth("2020 Jan")
#' feb <- yearmonth("2020 Feb")
#' mar <- yearmonth("2020 Mar")
#' q1 <- yearquarter("2020 Q1")
#'
#' jan == jan    # January equals itself
#'
#' jan %p% mar   # January precedes March: February leaves a genuine gap
#' mar %pi% jan  # ... equivalently, March is preceded by January
#'
#' jan %m% feb   # January meets February: adjacent, no gap
#' feb %mi% jan  # ... equivalently, February is met by January
#' jan < feb     # `<` also holds for adjacent pairs, unlike `%p%`
#' jan %p% feb   # FALSE: no gap between them, so they don't (strictly) precede
#'
#' jan %s% q1    # January and Q1 start together, but January finishes first
#' q1 %si% jan   # ... equivalently, Q1 is started by January
#'
#' feb %d% q1    # February is strictly within Q1
#' q1 %di% feb   # ... equivalently, Q1 contains February
#'
#' mar %f% q1    # March and Q1 finish together, but March starts later
#' q1 %fi% mar   # ... equivalently, Q1 is finished by March
#'
#' @name allen-interval-algebra
NULL

#' @rdname allen-interval-algebra
#' @export
`%p%` <- S7::new_generic("%p%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%pi%` <- S7::new_generic("%pi%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%m%` <- S7::new_generic("%m%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%mi%` <- S7::new_generic("%mi%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%o%` <- S7::new_generic("%o%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%oi%` <- S7::new_generic("%oi%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%s%` <- S7::new_generic("%s%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%si%` <- S7::new_generic("%si%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%d%` <- S7::new_generic("%d%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%di%` <- S7::new_generic("%di%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%f%` <- S7::new_generic("%f%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

#' @rdname allen-interval-algebra
#' @export
`%fi%` <- S7::new_generic("%fi%", c("e1", "e2"), fun = function(e1, e2) S7::S7_dispatch())

# Custom infix operators aren't aware of vecvec's slot dispatch
# This function factory defines the mixtime dispatch for these new operators
mixtime_compare <- function(.generic) {
  function(e1, e2) vecvec::vecvec_mapply(list(e1, e2), .generic, ptype = logical())
}

method(`%p%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%p%", e1, e2)
method(`%p%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%p%", e1, e2)
method(`%p%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%p%", e1, e2)
method(`%p%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%p%", e1, e2)
method(`%p%`, list(class_mixtime, class_any)) <- mixtime_compare(`%p%`)
method(`%p%`, list(class_any, class_mixtime)) <- mixtime_compare(`%p%`)

method(`%pi%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%pi%", e1, e2)
method(`%pi%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%pi%", e1, e2)
method(`%pi%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%pi%", e1, e2)
method(`%pi%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%pi%", e1, e2)
method(`%pi%`, list(class_mixtime, class_any)) <- mixtime_compare(`%pi%`)
method(`%pi%`, list(class_any, class_mixtime)) <- mixtime_compare(`%pi%`)

method(`%m%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%m%", e1, e2)
method(`%m%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%m%", e1, e2)
method(`%m%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%m%", e1, e2)
method(`%m%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%m%", e1, e2)
method(`%m%`, list(class_mixtime, class_any)) <- mixtime_compare(`%m%`)
method(`%m%`, list(class_any, class_mixtime)) <- mixtime_compare(`%m%`)

method(`%mi%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%mi%", e1, e2)
method(`%mi%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%mi%", e1, e2)
method(`%mi%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%mi%", e1, e2)
method(`%mi%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%mi%", e1, e2)
method(`%mi%`, list(class_mixtime, class_any)) <- mixtime_compare(`%mi%`)
method(`%mi%`, list(class_any, class_mixtime)) <- mixtime_compare(`%mi%`)

method(`%o%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%o%", e1, e2)
method(`%o%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%o%", e1, e2)
method(`%o%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%o%", e1, e2)
method(`%o%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%o%", e1, e2)
method(`%o%`, list(class_mixtime, class_any)) <- mixtime_compare(`%o%`)
method(`%o%`, list(class_any, class_mixtime)) <- mixtime_compare(`%o%`)

# `%o%` overrides base's outer-product operator (both are named `%o%`); when neither
# operand is a mixtime type, fall back to that base behaviour instead of erroring, so
# attaching mixtime doesn't break existing `%o%` usage.
method(`%o%`, list(class_any, class_any)) <- function(e1, e2) outer(e1, e2)

method(`%oi%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%oi%", e1, e2)
method(`%oi%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%oi%", e1, e2)
method(`%oi%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%oi%", e1, e2)
method(`%oi%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%oi%", e1, e2)
method(`%oi%`, list(class_mixtime, class_any)) <- mixtime_compare(`%oi%`)
method(`%oi%`, list(class_any, class_mixtime)) <- mixtime_compare(`%oi%`)

method(`%s%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%s%", e1, e2)
method(`%s%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%s%", e1, e2)
method(`%s%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%s%", e1, e2)
method(`%s%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%s%", e1, e2)
method(`%s%`, list(class_mixtime, class_any)) <- mixtime_compare(`%s%`)
method(`%s%`, list(class_any, class_mixtime)) <- mixtime_compare(`%s%`)

method(`%si%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%si%", e1, e2)
method(`%si%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%si%", e1, e2)
method(`%si%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%si%", e1, e2)
method(`%si%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%si%", e1, e2)
method(`%si%`, list(class_mixtime, class_any)) <- mixtime_compare(`%si%`)
method(`%si%`, list(class_any, class_mixtime)) <- mixtime_compare(`%si%`)

method(`%d%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%d%", e1, e2)
method(`%d%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%d%", e1, e2)
method(`%d%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%d%", e1, e2)
method(`%d%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%d%", e1, e2)
method(`%d%`, list(class_mixtime, class_any)) <- mixtime_compare(`%d%`)
method(`%d%`, list(class_any, class_mixtime)) <- mixtime_compare(`%d%`)

method(`%di%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%di%", e1, e2)
method(`%di%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%di%", e1, e2)
method(`%di%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%di%", e1, e2)
method(`%di%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%di%", e1, e2)
method(`%di%`, list(class_mixtime, class_any)) <- mixtime_compare(`%di%`)
method(`%di%`, list(class_any, class_mixtime)) <- mixtime_compare(`%di%`)

method(`%f%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%f%", e1, e2)
method(`%f%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%f%", e1, e2)
method(`%f%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%f%", e1, e2)
method(`%f%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%f%", e1, e2)
method(`%f%`, list(class_mixtime, class_any)) <- mixtime_compare(`%f%`)
method(`%f%`, list(class_any, class_mixtime)) <- mixtime_compare(`%f%`)

method(`%fi%`, list(mt_linear, class_any)) <- function(e1, e2) linear_compare("%fi%", e1, e2)
method(`%fi%`, list(class_any, mt_linear)) <- function(e1, e2) linear_compare("%fi%", e1, e2)
method(`%fi%`, list(mt_cyclical, class_any)) <- function(e1, e2) cyclical_compare("%fi%", e1, e2)
method(`%fi%`, list(class_any, mt_cyclical)) <- function(e1, e2) cyclical_compare("%fi%", e1, e2)
method(`%fi%`, list(class_mixtime, class_any)) <- mixtime_compare(`%fi%`)
method(`%fi%`, list(class_any, class_mixtime)) <- mixtime_compare(`%fi%`)

#' Ordering of mixtime vectors
#'
#' @description
#' `sort()` and `xtfrm()` (which in turn powers `order()`, `rank()`, and
#' comparisons like `min()`/`max()`) order a mixtime vector chronologically.
#' Continuous time points (e.g. `POSIXct`-backed values) order by their exact
#' instant, but a *discrete* time value (e.g. `yearmonth(2020, 1)`) represents
#' an entire span of time rather than a single instant, so ordering it
#' against a value of another granularity (or against a continuous value)
#' first requires picking the instant *within* that span to compare from.
#'
#' `align_discrete` controls that choice: `0` anchors discrete values to the
#' start of their span, `1` to the end, and the default `0.5` to the
#' midpoint - so, for example, `yearmonth(2020, 1)` sorts before, at the same
#' position as, or after `date("2020-01-15")` according to whether
#' `align_discrete` is closer to `0`, exactly `0.5`, or closer to `1`. This
#' only affects relative order between differing chronons/granularities;
#' values that already share a chronon keep their exact relative order for
#' any `align_discrete`, since shifting every value by the same amount can't
#' change their order.
#'
#' Because base R's `xtfrm()` generic takes no arguments beyond `x`,
#' `align_discrete` can only be customised through `sort()`; `xtfrm()` (and
#' anything built on it, such as `order()` or `rank()`) always aligns to the
#' midpoint.
#'
#' @param x,decreasing,na.last,... See [sort()].
#' @param align_discrete The fractional position (`0` = start, `1` = end)
#'   within a discrete time span's chronon to use as its ordering instant
#'   when reconciling it against a value of another chronon. Default `0.5`
#'   (the midpoint).
#'
#' @return
#' `sort()` returns a mixtime vector; `xtfrm()` returns a numeric vector
#' suitable for ranking.
#'
#' @examples
#' x <- c(yearmonth(2020, 1), date("2020-01-15"), yearmonth(2020, 2))
#' sort(x)
#' sort(x, align_discrete = 0)   # yearmonth(2020, 1) now sorts after the 15th
#' sort(x, decreasing = TRUE)
#'
#' @name mixtime-order
#' @aliases xtfrm.mixtime sort.mixtime
NULL

# Ordering and de-duplication of mixtime vectors

#' Ordering proxy for mixtime vectors
#'
#' Builds the ordering key underlying [xtfrm()] and [sort()] on mixtime
#' vectors (see `mixtime-order`).
#'
#' @param x A mixtime vector.
#' @param align_discrete The fractional position (0 = start, 1 = end) within
#'   a discrete time span's chronon to use as its ordering instant.
#'
#' @noRd
#' @importFrom vctrs vec_proxy_order
mixtime_order_proxy <- function(x, align_discrete = 0.5) {
  mode <- check_common_time_mode(x)
  if (!length(x@x)) return(vec_proxy_order(vecvec::unvecvec(x)))

  # The granules each part is reduced to, read before converting the parts below
  # replaces them with bare chronon counts carrying neither granule.
  chronon <- chronon_common_impl(lapply(x@x, function(v) attr(v, "chronon")))
  cycle <- if (identical(mode, "cyclical")) check_common_cycle(x)

  # Convert all time values to a common chronon, which a single part already is
  if (length(x@x) > 1L) {
    x@x <- lapply(x@x, function(v) {
      if (is.integer(v)) v <- v + align_discrete
      chronon_convert(v, chronon)
    })
  }

  if (!is.null(cycle)) {
    # Cyclical values order by their position within the cycle, matching `==`
    # and `<` on `mt_cyclical` (see `mt_cyclical-compare`) rather than the
    # absolute chronon count stored underneath.
    x@x <- lapply(x@x, function(v) cyclical_position(vec_data(v), chronon, cycle))
  }

  vec_proxy_order(vecvec::unvecvec(x))
}


#' @rdname mixtime-order
#' @export
method(xtfrm, class_mixtime) <- function(x) {
  xtfrm(mixtime_order_proxy(x, align_discrete = 0.5))
}

#' @rdname mixtime-order
#' @export
method(sort, class_mixtime) <- function(x, decreasing = FALSE, na.last = NA, ..., align_discrete = 0.5) {
  key <- xtfrm(mixtime_order_proxy(x, align_discrete = align_discrete))
  x[order(key, na.last = na.last, decreasing = decreasing)]
}

method(unique, class_mixtime) <- function(x, incomparables = FALSE, ...) {
  vctrs::vec_unique(x)
}

method(duplicated, class_mixtime) <- function(x, incomparables = FALSE, ...) {
  vctrs::vec_duplicate_id(x) != seq_along(x)
}

method(anyDuplicated, class_mixtime) <- function(x, incomparables = FALSE, ...) {
  match(TRUE, duplicated(x), nomatch = 0L)
}

#' Tolerant comparison of mixtime time values
#'
#' @description
#' [vecvec::class_vecvec]'s `all.equal()` method (which `mixtime` inherits,
#' since a mixtime vector is a `vecvec`) handles length/`NA` mismatches and
#' groups elements by their underlying storage slot, then compares each
#' group's *raw* values with `all.equal()`. That raw comparison has no notion
#' of chronon: two elements that are `==` (e.g. `days(1)` and `hours(24)`, or
#' a `yearmonth` and an equivalent-instant `yearweek`) can end up stored with
#' different chronons or magnitudes, and so are wrongly reported as unequal.
#'
#' This method fixes that at the per-slot level: elements already `==` (which
#' is chronon-aware, see [mt_linear-compare]/[mt_duration-compare]) count as
#' equal outright; for the rest, the discrepancy is measured as the duration
#' between them in their common chronon. A time point has no "typical
#' magnitude" to scale a *relative* tolerance against the way a plain number
#' does, so - unlike [base::all.equal.numeric()] - `tolerance` is always
#' absolute, in (possibly fractional) chronon units.
#'
#' @param target,current `mt_time` vectors of the same length to compare (as
#'   passed down by [vecvec::class_vecvec]'s `all.equal()`).
#' @param tolerance Numeric tolerance, as an absolute number of `target`'s
#'   chronon units. Defaults to `sqrt(.Machine$double.eps)`, as for
#'   [base::all.equal()].
#' @param ... Ignored.
#'
#' @return `TRUE` if `target` and `current` are equal within `tolerance`,
#'   otherwise a string describing the discrepancy.
#'
#' @examples
#' all.equal(yearmonth(0), yearmonth(0))
#' all.equal(yearmonth(0), yearmonth(1))
#' all.equal(days(1), hours(24))
#'
#' @keywords internal
#' @method all.equal mixtime::mt_time
#' @export
`all.equal.mixtime::mt_time` <- function(
  target, current, tolerance = sqrt(.Machine$double.eps), ...
) {
  # `class_vecvec`'s all.equal() only filters *sparse* missingness (an NA
  # vecvec index); an NA stored inline within a slot's own data (as here)
  # still reaches us and needs handling directly - mirroring the NA check in
  # `all.equal.numeric()`.
  na_t <- is.na(target)
  na_c <- tryCatch(is.na(current), error = function(e) NULL)
  if (is.null(na_c)) {
    return(sprintf(
      "target is %s, current is %s",
      class(target)[[1L]], class(current)[[1L]]
    ))
  }
  if (!identical(na_t, na_c)) {
    return(sprintf(
      "'is.NA' value mismatch: %d in current, %d in target",
      sum(na_c), sum(na_t)
    ))
  }

  keep <- !na_t
  eq <- tryCatch(target[keep] == current[keep], error = function(e) NULL)
  if (is.null(eq)) {
    return(sprintf(
      "target is %s, current is %s",
      class(target)[[1L]], class(current)[[1L]]
    ))
  }
  if (all(eq)) return(TRUE)

  d <- target[keep][!eq] - current[keep][!eq]
  xy <- mean(abs(as.double(d)))
  if (xy > tolerance) {
    sprintf("Mean absolute difference: %s", format(xy))
  } else {
    TRUE
  }
}
