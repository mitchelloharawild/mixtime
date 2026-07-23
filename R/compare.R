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
