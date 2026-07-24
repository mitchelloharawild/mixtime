#' Check the time type of values
#'
#' Test whether elements of a `mixtime` vector are linear, cyclical, or durations.
#'
#' These helpers return a logical vector the same length as `x` identifying the
#' type of time represented by each element.
#'
#' @param x A time object (typically a `mixtime` vector).
#' @param ... Additional arguments for methods.
#'
#' @return A logical vector the same length as `x`.
#'
#' @examples
#' t <- c(yearmonth(0), month_of_year(0), months(0L))
#' time_is_linear(t)
#' time_is_cyclical(t)
#' time_is_duration(t)
#'
#' @name is_time
#' @export
time_is_linear <- S7::new_generic("time_is_linear", "x")
method(time_is_linear, class_mixtime) <- vecvec::vecvec_apply_fn(time_is_linear, logical())
method(time_is_linear, class_any) <- function(x) rep.int(FALSE, length(x))
method(time_is_linear, mt_linear) <- function(x) rep.int(TRUE, length(x))

#' @rdname is_time
#' @export
time_is_cyclical <- S7::new_generic("time_is_cyclical", "x")
method(time_is_cyclical, class_mixtime) <- vecvec::vecvec_apply_fn(time_is_cyclical, logical())
method(time_is_cyclical, class_any) <- function(x) rep.int(FALSE, length(x))
method(time_is_cyclical, mt_cyclical) <- function(x) rep.int(TRUE, length(x))

#' @rdname is_time
#' @export
time_is_duration <- S7::new_generic("time_is_duration", "x")
method(time_is_duration, class_mixtime) <- vecvec::vecvec_apply_fn(time_is_duration, logical())
method(time_is_duration, class_any) <- function(x) rep.int(FALSE, length(x))
method(time_is_duration, new_S4_class("Period", package = "lubridate")) <- function(x) rep.int(TRUE, length(x))

#' @rdname is_time
#' @export
is_time_linear <- function(x, ...) {
  lifecycle::deprecate_stop("0.3.0", "is_time_linear()", "time_is_linear()")
}

#' @rdname is_time
#' @export
is_time_cyclical <- function(x, ...) {
  lifecycle::deprecate_stop("0.3.0", "is_time_cyclical()", "time_is_cyclical()")
}

#' @rdname is_time
#' @export
is_time_duration <- function(x, ...) {
  lifecycle::deprecate_stop("0.3.0", "is_time_duration()", "time_is_duration()")
}

#' Test whether time is determinate at a granule's precision
#'
#' `time_is_determinate_at()` tests, for each element of a `mixtime` vector, whether
#' the time point is well-defined at the precision of `granule`.
#'
#' Discrete (integer) time cannot resolve a granule finer than its own chronon (a
#' `year()` has no determinate month), so those elements are `FALSE`. Continuous
#' (fractional) time tracks progress within its chronon and so resolves finer
#' granules exactly (0% through 2020 is 0% through January), giving `TRUE`.
#' Coarser-or-equal granules are always determinate. Missing (`NA`) and infinite
#' times give `NA`.
#'
#' @param x A time object (typically a `mixtime` vector).
#' @param granule The time granule whose precision to test, given as a granule
#'   generator (e.g. `cal_gregorian$month`) or a sized time unit (e.g.
#'   `cal_gregorian$month(1L)`).
#' @param ... Additional arguments for methods.
#'
#' @return A logical vector the same length as `x`.
#'
#' @examples
#' # Discrete: a year has no determinate month
#' time_is_determinate_at(year(2020L), cal_gregorian$month(1L))
#'
#' # Continuous: 0% through 2020 is 0% through January
#' time_is_determinate_at(year(2020), cal_gregorian$month(1L))
#'
#' # A coarser granule is always determinate
#' time_is_determinate_at(yearmonth(as.Date("2020-02-01")), cal_gregorian$year(1L))
#'
#' @export
time_is_determinate_at <- S7::new_generic(
  "time_is_determinate_at", "x",
  function(x, granule, ...) S7::S7_dispatch()
)
method(time_is_determinate_at, class_mixtime) <-
  vecvec::vecvec_apply_fn(time_is_determinate_at, logical())
method(time_is_determinate_at, class_any) <- function(x, granule, ...) {
  time_is_determinate_at(as_mixtime(x), granule, ...)
}
method(time_is_determinate_at, mt_time) <- function(x, granule, ...) {
  # Coerce a bare granule generator (e.g. cal_gregorian$month) into a unit.
  if (!S7_inherits(granule, mt_unit)) granule <- granule(1L)

  # Continuous (fractional) time tracks progress within its chronon and so
  # resolves any finer granule exactly; discrete (integer) time cannot subdivide
  # below its chronon, so a finer granule is temporally indeterminate.
  ok <- !is.integer(vec_data(x)) || chronon_nests_in(attr(x, "chronon"), granule)
  out <- rep(ok, length(x))
  # A missing or infinite time is not a determinate instant of any granule.
  out[is.na(x) | is.infinite(vec_data(x))] <- NA
  out
}
method(time_is_duration, mt_duration) <- function(x) rep.int(TRUE, length(x))