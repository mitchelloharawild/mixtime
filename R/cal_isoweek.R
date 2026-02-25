#' ISO 8601 time unit classes
#'
#' Time unit constructors for the ISO 8601 calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @return A time unit object for the ISO 8601 calendar system.
#' 
#' @details
#' The following time units are available in the ISO week date calendar:
#' 
#' - `year()`: ISO year unit (years start on the week containing the first Thursday)
#' - `week()`: Week unit (7-day periods)
#' - `day()`: Day unit
#' - `hour()`: Hour unit
#' - `minute()`: Minute unit
#' - `second()`: Second unit
#' - `millisecond()`: Millisecond unit
#' 
#' ISO 8601 weeks always start on Monday and the first week of a year is the week
#' containing the first Thursday of that year. This means that some days in early
#' January may belong to the last week of the previous ISO year, and some days in
#' late December may belong to the first week of the next ISO year.
#' 
#' @seealso [linear_time()] for creating custom time representations,
#'   [yearweek()] for a pre-defined ISO 8601 year-week representation
#' 
#' @name calendar_isoweek
#' @export
cal_isoweek <- new_calendar(
  year = S7::new_class("tu_isoyear", parent = mt_tz_unit),
  week = S7::new_class("tu_week", parent = mt_tz_unit),
  day = cal_time_civil_midnight$day,
  hour = cal_time_civil_midnight$hour,
  minute = cal_time_civil_midnight$minute,
  second = cal_time_civil_midnight$second,
  millisecond = cal_time_civil_midnight$millisecond,
  class = "cal_isoweek"
)

# Time unit labels
S7::method(time_unit_full, cal_isoweek$year) <- function(x) "isoyear"
S7::method(time_unit_abbr, cal_isoweek$year) <- function(x) "IY"
S7::method(time_unit_full, cal_isoweek$week) <- function(x) "week"
S7::method(time_unit_abbr, cal_isoweek$week) <- function(x) "W"

# Default granules
method(chronon_granules, cal_isoweek$week) <- function(x) list(cal_isoweek$year(1L))

# Default formats
method(chronon_format, cal_isoweek$year) <- function(x) "{year}"
method(chronon_format, cal_isoweek$week) <- function(x) "{year} W{week}"

# 1:1 mapping for isoyears to years 
# TODO - this is not entirely accurate, but is currently necessary
# for converting the 1970 epoch in the print method
S7::method(chronon_cardinality, list(cal_isoweek$year, cal_isoweek$year)) <- function(x, y, at = NULL) {
  vec_data(x)*1L/vec_data(y)
}

S7::method(chronon_cardinality, list(cal_isoweek$week, cal_isoweek$day)) <- function(x, y, at = NULL) {
  vec_data(x)*7L/vec_data(y)
}

S7::method(chronon_divmod, list(cal_isoweek$day, cal_isoweek$week)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  divisor <- chronon_cardinality(to, from)
  list(
    chronon = (x + 3L) %/% divisor,
    remainder = (x + 3L) %% divisor
  )
}

S7::method(chronon_divmod, list(cal_isoweek$week, cal_isoweek$day)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  mult <- chronon_cardinality(from, to)

  list(
    chronon = x*mult - 3L,
    remainder = 0L
  )
}

S7::method(chronon_divmod, list(cal_isoweek$week, cal_isoweek$year)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to months
  if (chronon_cardinality(to, cal_isoweek$year(1L)) != 1L) {
    stop("Converting to multi-year chronons from weeks is not yet supported", call. = FALSE)
  }
  # TODO: should be swapped out to arithmetic on integer days since epoch

  # ISO 8601 years for weekly chronons cycle with the first week with a Thursday
  # Unix epoch 1970-01-01 is a Thursday, a convenient reference point
  
  x <- as.POSIXlt(as.Date(x*7))
  list(
    chronon = x$year-70L,
    remainder = x$yday %/% 7L
  )
}

week.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
week.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

S7::method(cyclical_labels, list(cal_isoweek$day, cal_isoweek$week)) <- function(granule, cycle, i, label = TRUE, abbr = TRUE) {
  # TODO: Add offset for different week starting days
  if (!label) {
    i + 1L
  } else if (abbr) {
    week.abb[i + 1L]
  } else {
    week.name[i + 1L]
  }
}
S7::method(cyclical_labels, list(cal_isoweek$week, S7::class_any)) <- function(granule, cycle, i) {
  # Weeks count with 1-indexing
  sprintf("%02d", i + 1L)
}
