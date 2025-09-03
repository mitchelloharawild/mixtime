# TODO: the parent class should ideally be double.
# This requires a rework of the methods to not calculate the value as a ratio of non-1L durations...
# But this could cause problems with identifying the appropriate range of time specified.
# QUESTION: Should these methods EVER return a non-integer value?

#' Gregorian time unit classes
#'
#' @rdname gregorian_time_units
#' @export
tu_year <- S7::new_class("tu_year", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_quarter <- S7::new_class("tu_quarter", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_month <- S7::new_class("tu_month", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_week <- S7::new_class("tu_week", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_day <- S7::new_class("tu_day", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_hour <- S7::new_class("tu_hour", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_minute <- S7::new_class("tu_minute", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_second <- S7::new_class("tu_second", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_millisecond <- S7::new_class("tu_millisecond", parent = mt_unit)

### Calendar algebra methods for Gregorian time units

#' @export
method(calendar_algebra, list(tu_year, tu_quarter)) <- function(x, y, at = NULL) {
  as.integer(x)*4*as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_year, tu_month)) <- function(x, y, at = NULL) {
  as.integer(x)*12/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_year, tu_day)) <- function(x, y, at = NULL) {
  # TODO: Handle leap years if `at` is provided
  as.integer(x)*365/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_quarter, tu_month)) <- function(x, y, at = NULL) {
  as.integer(x)*3/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_week, tu_day)) <- function(x, y, at = NULL) {
  as.integer(x)*7/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_day, tu_hour)) <- function(x, y, at = NULL) {
  as.integer(x)*24/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_hour, tu_minute)) <- function(x, y, at = NULL) {
  as.integer(x)*60/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_minute, tu_second)) <- function(x, y, at = NULL) {
  # if(at %in% .leap.seconds) 61 else 60

  as.integer(x)*60/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_second, tu_millisecond)) <- function(x, y, at = NULL) {
  as.integer(x)*1000/as.integer(y)
}

#' @export
method(calendar_algebra, list(tu_day, tu_month)) <- function(x, y, at = NULL) {
  # lubridate::days_in_month(at)
  stop("Not yet supported: Durations between days and months require a specific date context to calculate ratio")
}

