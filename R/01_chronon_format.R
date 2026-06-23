
#' Default formatting strings for chronons
#'
#' Provides default linear time formatting strings for a given chronon (finest 
#' time granule). The format strings use placeholders like `{lin(year(1L))}`,
#' `{cyc(month(1L), year(1L)}` and `{cyc(day(1L), month(1L)}`, which are
#' evaluated in the context of the data's [`time_calendar()`].
#' 
#' @param x A time granule for the chronon.
#' @param y A time granule for the cycle
#' @param cal The calendar of the chronon, used to disambiguate suitable format 
#'   strings for time units that are shared across calendars (e.g. 
#'   `cal_gregorian$day` and `cal_isoweek$day`).
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
  paste0(time_unit_abbr(x), "{lin(chronon_common(.time))}")
}

#' @examples
#' chronon_format_cyclical(cal_gregorian$month(1L), cal_gregorian$year(1L))
#' chronon_format_cyclical(cal_gregorian$day(1L), cal_gregorian$month(1L))
#' chronon_format_cyclical(cal_isoweek$day(1L), cal_isoweek$week(1L))
#' chronon_format_cyclical(cal_isoweek$week(1L), cal_isoweek$year(1L))
#' 
#' @rdname chronon_format
#' @export
chronon_format_cyclical <- new_generic("chronon_format_cyclical", c("x", "y"))
method(chronon_format_cyclical, list(mt_unit, mt_unit)) <- function(x, y) {
  paste0(time_unit_abbr(x), "{cyc(chronon_common(.time), chronon_common(time_cycle(.time)))}")
}

#' @examples
#' chronon_format_duration(cal_gregorian$year(1L))
#' chronon_format_duration(cal_gregorian$month(1L))
#' chronon_format_duration(cal_gregorian$day(1L))
#' 
#' @rdname chronon_format
#' @export
chronon_format_duration <- new_generic("chronon_format_duration", "x")
method(chronon_format_duration, mt_unit) <- function(x) {
  "{vec_data(.time)} {time_unit_plural(chronon_common(.time), vec_data(.time))}"
}

#' Default formatting strings for chronon attributes
#' 
#' Provides suffixes for default formatting strings for a given chronon (time granule).
#' This provides useful information such as timezones or locations in the string.
#' 
#' @param x A chronon (time granule) object.
#' @param ... Additional arguments for methods.
#'
#' @return A character string containing the default format suffix for the chronon.
#' 
#' @export
chronon_format_attr <- new_generic("chronon_format_attr", "x")
method(chronon_format_attr, mt_unit) <- function(x) {
  ""
}
