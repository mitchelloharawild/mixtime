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
#' - `a < b` iff `end(a) < start(b)`
#' - `a > b` iff `start(a) > end(b)`
#' - `a <= b` iff `end(a) <= end(b)` (right-bound comparison)
#' - `a >= b` iff `start(a) >= start(b)` (left-bound comparison)
#'
#' This is **not** a total order: `<=`/`>=` are endpoint comparisons, not
#' shorthand for `(< or ==)`/`(> or ==)`, so it is possible for none of
#' `==`, `<`, `>` to hold between two values.
#'
#' If both operands are continuous (fractional chronons), or share an
#' identical chronon, the comparison simplifies to a direct numeric
#' comparison, since there is no interval to consider.
#'
#' @param e1,e2 `mt_linear` vectors (or values castable to one, such as
#'   plain numeric vectors sharing the other operand's chronon).
#'
#' @return A logical vector.
#'
#' @name mt_linear-compare
NULL

linear_compare <- function(op, e1, e2) {
  if (!S7_inherits(e1, mt_linear)) e1 <- vec_cast(e1, e2)
  if (!S7_inherits(e2, mt_linear)) e2 <- vec_cast(e2, e1)

  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  xv <- S7_data(e1)
  yv <- S7_data(e2)

  if (identical(x_chronon, y_chronon)) {
    # Identical granules: no span to consider, compare the raw data directly
    x_start <- x_end <- xv
    y_start <- y_end <- yv
  } else {
    # Different granules: express both values as closed intervals
    # ([start, end]) in their common (finest shared) chronon, then compare
    # endpoints. Continuous values are zero-width intervals (start == end).
    common <- chronon_common_impl(list(x_chronon, y_chronon))
    x_start <- chronon_convert_impl(xv, x_chronon, common, discrete = is.integer(xv))
    y_start <- chronon_convert_impl(yv, y_chronon, common, discrete = is.integer(yv))

    x_end <- x_start
    if (is.integer(xv)) {
      x_end <- x_start + chronon_cardinality(common, x_chronon, at = xv) - 1
    }
    y_end <- y_start
    if (is.integer(yv)) {
      y_end <- y_start + chronon_cardinality(common, y_chronon, at = yv) - 1
    }
  }

  switch(
    op,
    "==" = x_start == y_start & x_end == y_end,
    "!=" = !(x_start == y_start & x_end == y_end),
    "<"  = x_end < y_start,
    ">"  = x_start > y_end,
    "<=" = x_end <= y_end,
    ">=" = x_start >= y_start
  )
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
    # Different chronons: scale both magnitudes to their finest common chronon
    # before comparing (mirrors `duration_combine()` in arithmetic.R).
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
#' - `day_of_week("2020-01-15") == day_of_week("2020-01-22")` is `TRUE`, since
#'   both are a Wednesday.
#' - `day_of_year("2020-01-15") == day_of_year("2021-01-15")` is `TRUE`, since
#'   both are the 15th day of their year.
#'
#' Both operands must share a `cycle`: a cycle is a modulus rather than a unit,
#' so there is no meaningful common cycle between (say) a weekday and a
#' day-of-year, and comparing them is an error. Differing *chronons* within a
#' shared cycle are reconciled as they are for [mt_linear][mt_linear-compare]:
#' both positions are expressed in the finest common chronon, and a discrete
#' value spans the closed interval of its chronon, so
#'
#' - `a == b` iff `start(a) == start(b)` and `end(a) == end(b)`
#' - `a < b` iff `end(a) < start(b)`, `a > b` iff `start(a) > end(b)`
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
#' @name mt_cyclical-compare
NULL

# Reduce absolute chronon counts to their position within `cycle`, matching the
# reduction format() displays: the integer position from chronon_parts(), plus
# the fraction carried by a continuous (double) time model, which chronon_parts()
# floors away for display but which distinguishes values here.
cyclical_position <- function(x, chronon, cycle) {
  x <- mt_cyclical(x, chronon = chronon, cycle = cycle)
  pos <- chronon_parts(x, cyclical = list(list(chronon, cycle)))$cyclical[[1L]]
  xd <- S7_data(x)
  if (is.double(xd)) {
    xd <- xd + tz_offset(x)
    pos <- pos + (xd - floor(xd))
  }
  pos
}

cyclical_compare <- function(op, e1, e2) {
  if (!S7_inherits(e1, mt_cyclical)) e1 <- vec_cast(e1, e2)
  if (!S7_inherits(e2, mt_cyclical)) e2 <- vec_cast(e2, e1)

  # A cycle is a modulus, not a unit: there is no common cycle to reconcile to
  # (see `cycle_common()`), so differing cycles cannot be compared.
  cycle <- cycle_common(e1@cycle, e2@cycle)
  if (is.null(cycle)) {
    vctrs::stop_incompatible_op(op, e1, e2, details = cycle_incompatible_details)
  }

  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  xv <- S7_data(e1)
  yv <- S7_data(e2)

  if (identical(x_chronon, y_chronon)) {
    # Identical granules: no span to consider, compare the positions directly
    x_start <- x_end <- cyclical_position(xv, x_chronon, cycle)
    y_start <- y_end <- cyclical_position(yv, y_chronon, cycle)
  } else {
    # Different granules: express both positions as closed intervals ([start,
    # end]) in their common (finest shared) chronon, then compare endpoints,
    # mirroring `linear_compare()`.
    common <- chronon_common_impl(list(x_chronon, y_chronon))
    x_start <- cyclical_position(
      chronon_convert_impl(xv, x_chronon, common, discrete = is.integer(xv)),
      common, cycle
    )
    y_start <- cyclical_position(
      chronon_convert_impl(yv, y_chronon, common, discrete = is.integer(yv)),
      common, cycle
    )

    x_end <- x_start
    if (is.integer(xv)) {
      x_end <- x_start + chronon_cardinality(common, x_chronon, at = xv) - 1
    }
    y_end <- y_start
    if (is.integer(yv)) {
      y_end <- y_start + chronon_cardinality(common, y_chronon, at = yv) - 1
    }
  }

  switch(
    op,
    "==" = x_start == y_start & x_end == y_end,
    "!=" = !(x_start == y_start & x_end == y_end),
    "<"  = x_end < y_start,
    ">"  = x_start > y_end,
    "<=" = x_end <= y_end,
    ">=" = x_start >= y_start
  )
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

# Ordering and de-duplication of mixtime vectors
method(xtfrm, class_mixtime) <- function(x) {
  xtfrm(vec_proxy_order(x))
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
