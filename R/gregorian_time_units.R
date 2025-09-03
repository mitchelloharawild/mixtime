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

### Time unit labels for Gregorian time units

#' @export
method(time_unit_full, tu_year) <- function(x) "year"
#' @export
method(time_unit_abbr, tu_year) <- function(x) "Y"

#' @export
method(time_unit_full, tu_quarter) <- function(x) "quarter"
#' @export
method(time_unit_abbr, tu_quarter) <- function(x) "Q"

#' @export
method(time_unit_full, tu_month) <- function(x) "month"
#' @export
method(time_unit_abbr, tu_month) <- function(x) "M"

#' @export
method(time_unit_full, tu_week) <- function(x) "week"
#' @export
method(time_unit_abbr, tu_week) <- function(x) "W"

#' @export
method(time_unit_full, tu_day) <- function(x) "day"
#' @export
method(time_unit_abbr, tu_day) <- function(x) "D"

#' @export
method(time_unit_full, tu_hour) <- function(x) "hour"
#' @export
method(time_unit_abbr, tu_hour) <- function(x) "h"

#' @export
method(time_unit_full, tu_minute) <- function(x) "minute"
#' @export
method(time_unit_abbr, tu_minute) <- function(x) "min"

#' @export
method(time_unit_full, tu_second) <- function(x) "second"
#' @export
method(time_unit_abbr, tu_second) <- function(x) "s"

#' @export
method(time_unit_full, tu_millisecond) <- function(x) "millisecond"
#' @export
method(time_unit_abbr, tu_millisecond) <- function(x) "ms"


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

### Cyclical labels for Gregorian time units
#' @export
method(cyclical_labels, list(tu_month, tu_year)) <- function(granule, cycle, i) {
  month.abb[i]
}

#' @export
method(cyclical_labels, list(tu_day, tu_week)) <- function(granule, cycle, i) {
  # TODO: Add offset for different week starting days
  format(as.Date(i - 4L), "%a")
}