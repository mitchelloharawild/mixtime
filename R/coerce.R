# Coercion with `as.*()` family.
#' @export
S7::method(as.character, mt_time) <- function(x, ...) vctrs::vec_cast(x, character())
#' @export
S7::method(as.integer, mt_time) <- function(x, ...) vctrs::vec_cast(x, integer())
#' @export
S7::method(as.double, mt_time) <- function(x, ...) vctrs::vec_cast(x, double())
#' @export
S7::method(as.Date, mt_linear) <- function(x, ...) vctrs::vec_cast(x, vctrs::new_date())
#' @export
S7::method(as.POSIXct, mt_linear) <- function(x, tz = "", ...) {
  vctrs::vec_cast(x, vctrs::new_datetime(tzone = tz))
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
  vec_cast(x, new_mixtime())
}
