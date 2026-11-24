# Coercion with `as.*()` family.
#
# These are plain S3 methods rather than `S7::method(as.double, ...) <-`, which
# would bind a copy of `as.double` in the mixtime namespace. R then treats it as
# a generic local to mixtime and registers the `S3method(as.double, ...)`
# entries in mixtime's own S3 table, out of reach of `as.double()` calls from
# other packages (S7 0.2.2 also registers `method(as.double, cls)` under the
# name `as.numeric.<cls>`, which `as.double()` never dispatches to).
#
# vecvec's ALTREP base reads stored times with `as.double()`, so base R and C
# code (`lm()`, `prcomp()`) sees cyclical time as its position in the cycle, as
# `as.numeric()` does.
#' @method as.character mixtime::mt_time
#' @export
`as.character.mixtime::mt_time` <- function(x, ...) vctrs::vec_cast(x, character())
#' @method as.integer mixtime::mt_time
#' @export
`as.integer.mixtime::mt_time` <- function(x, ...) {
  x <- time_as_number(x)
  if (is.double(x)) vctrs::vec_cast(x, integer()) else x
}
#' @method as.integer mixtime::mixtime
#' @export
`as.integer.mixtime::mixtime` <- function(x, ...) {
  as.integer(vecvec::unvecvec(vecvec::vecvec_apply(x, time_as_number), ptype = double()))
}
#' @method as.double mixtime::mt_time
#' @export
`as.double.mixtime::mt_time` <- function(x, ...) as.double(time_as_number(x))
#' @method as.double mixtime::mixtime
#' @export
`as.double.mixtime::mixtime` <- function(x, ...) {
  vecvec::unvecvec(vecvec::vecvec_apply(x, time_as_number), ptype = double())
}

# A time as a plain number. Cyclical time is its position within the cycle
# counted from the first label's `start` (so `month_of_year(8)` is 8 and
# `time_of_day(0)` is 0), reading back as `cyclical_time()` reads numbers.
# Linear time and durations are their chronon counts.
time_as_number <- function(x) {
  if (S7_inherits(x, mt_cyclical_free)) {
    chronon <- attr(x, "chronon")
    cycle <- attr(x, "cycle")
    vec_data(x) + cyclical_labels(chronon, cycle)$start
  } else if (S7_inherits(x, mt_cyclical)) {
    chronon <- attr(x, "chronon")
    cycle <- attr(x, "cycle")
    cyclical_position(vec_data(x), chronon, cycle) + cyclical_labels(chronon, cycle)$start
  } else {
    vctrs::vec_data(x)
  }
}
#' @export
S7::method(as.Date, mt_linear) <- function(x, ...) vctrs::vec_cast(x, vctrs::new_date())
#' @export
S7::method(as.POSIXct, mt_linear) <- function(x, tz = posixct_tz(x), ...) {
  vctrs::vec_cast(x, vctrs::new_datetime(tzone = tz))
}
#' @export
S7::method(as.POSIXct, class_mixtime) <- function(x, tz = posixct_tz(x), ...) {
  vecvec::unvecvec(x, ptype = vctrs::new_datetime(tzone = tz))
}

# The time zone of a POSIXct converted from `x` defaults to the chronon's own
# zone, or UTC if `x` has several (or naive) time zones.
posixct_tz <- function(x) {
  tz <- unique(tz_name(x))
  if (length(tz) == 1L && !is.na(tz)) tz else "UTC"
}

#' Convert a time class into a mixtime
#'
#' Coerces a time object (e.g. `Date`, `POSIXct`, `yearmonth`) to a `mixtime`
#' vector using [vctrs::vec_cast()]. The chronon and cycle are inferred from
#' `x` via [time_chronon()] and [time_cycle()].
#'
#' @param x A time value to convert to a `mixtime`. Any time class with a defined `time_chronon()` method can be converted (e.g. `Date`, `POSIXct`, `yearmonth`, etc.).
#' @param ... Additional arguments passed to the underlying [vec_cast()] method.
#'
#' @return A `mixtime` object corresponding to `x`.
#'
#' @seealso [mixtime()] for constructing a `mixtime` directly from data,
#'   [is_mixtime()] for testing if an object is a `mixtime`.
#'
#' @examples
#' as_mixtime(Sys.Date())
#' as_mixtime(Sys.time())
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, mixtime_any())
}
