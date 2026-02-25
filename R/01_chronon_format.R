
#' Default formatting strings for chronons
#'
#' Provides default linear time formatting strings for a given chronon (time unit).
#' The format strings use placeholders like `{year}`, `{month}`, `{day}`, etc.,
#' that can be interpolated with actual values.
#'
#' @param x A chronon (time unit) object.
#'
#' @return A character string containing the default format template for the chronon.
#'
#' @export
#' @examples
#' chronon_format(cal_gregorian$year)
#' chronon_format(cal_gregorian$month)
#' chronon_format(cal_gregorian$day)
chronon_format <- new_generic("chronon_format", "x")
method(chronon_format, mt_unit) <- function(x) {
  cal <- time_calendar(x)
  tu_i <- match(S7_class_id(x), vapply(cal, S7_class_id, character(1L)))
  paste0("{", names(cal)[tu_i], "}{frac}")
}
