
#' Default formatting strings for chronons
#'
#' Provides default linear time formatting strings for a given chronon (time unit).
#' The format strings use placeholders like `{year}`, `{month}`, `{day}`, etc.,
#' that can be interpolated with actual values.
#'
#' @param x A chronon (time unit) object.
#' @param y A chronon (time unit) for the cycle size.
#' @param cal The calendar of the chronon, used to disambiguate format strings for 
#'   time units that are shared across calendars (e.g. `cal_gregorian$day` and `cal_isoweek$day`).
#' @param ... Additional arguments for methods.
#'
#' @return A character string containing the default format template for the chronon.
#'
#' @export
#' @examples
#' chronon_format_linear(cal_gregorian$year(1L))
#' chronon_format_linear(cal_gregorian$month(1L))
#' chronon_format_linear(cal_gregorian$day(1L))
#' chronon_format_linear(cal_isoweek$day(1L))
#' 
#' @rdname chronon_format
chronon_format_linear <- new_generic("chronon_format_linear", c("x", "cal"), function(x, cal = time_calendar(x), ...) {
  S7::S7_dispatch()
})
method(chronon_format_linear, list(mt_unit, class_any)) <- function(x, cal) {
  tu_i <- match(S7_class_id(x), vapply(cal, S7_class_id, character(1L)))
  paste0("{lin(", names(cal)[tu_i], ")}")
}
#' @rdname chronon_format
#' @export
chronon_format_cyclical <- new_generic("chronon_format_cyclical", c("x", "y"))
method(chronon_format_cyclical, list(mt_unit, mt_unit)) <- function(x, y) {
  cal <- time_calendar(x)
  tu_i <- match(
    c(S7_class_id(x), S7_class_id(y)),
    vapply(cal, S7_class_id, character(1L))
  )
  paste0("{cyc(", paste0(names(cal)[tu_i], collapse = ","), ")}")
}

#' Default formatting strings for chronon attributes
#' 
#' Provides suffixes for default formatting strings for a given chronon (time unit).
#' This provides useful information such as timezones or locations in the string.
#' 
#' @param x A chronon (time unit) object.
#' @param ... Additional arguments for methods.
#'
#' @return A character string containing the default format suffix for the chronon.
chronon_format_attr <- new_generic("chronon_format_attr", "x")
method(chronon_format_attr, mt_unit) <- function(x) {
  ""
}
