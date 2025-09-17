# TODO: the parent class should ideally be double.
# This requires a rework of the S7::methods to not calculate the value as a ratio of non-1L durations...
# But this could cause problems with identifying the appropriate range of time specified.
# QUESTION: Should these S7::methods EVER return a non-integer value?

#' Gregorian time unit classes
#'
#' @rdname calendar_gregorian
#' @export
tu_year <- S7::new_class("tu_year", parent = mt_unit)
S7::method(time_unit_full, tu_year) <- function(x) "year"
S7::method(time_unit_abbr, tu_year) <- function(x) "Y"

#' @rdname calendar_gregorian
#' @export
tu_quarter <- S7::new_class("tu_quarter", parent = mt_unit)
S7::method(time_unit_full, tu_quarter) <- function(x) "quarter"
S7::method(time_unit_abbr, tu_quarter) <- function(x) "Q"

#' @rdname calendar_gregorian
#' @export
tu_month <- S7::new_class("tu_month", parent = mt_unit)
S7::method(time_unit_full, tu_month) <- function(x) "month"
S7::method(time_unit_abbr, tu_month) <- function(x) "M"

#' @rdname calendar_gregorian
#' @export
tu_day <- S7::new_class("tu_day", parent = mt_unit)
S7::method(time_unit_full, tu_day) <- function(x) "day"
S7::method(time_unit_abbr, tu_day) <- function(x) "D"

#' @rdname calendar_gregorian
#' @export
tu_hour <- S7::new_class("tu_hour", parent = mt_unit)
S7::method(time_unit_full, tu_hour) <- function(x) "hour"
S7::method(time_unit_abbr, tu_hour) <- function(x) "h"

#' @rdname calendar_gregorian
#' @export
tu_minute <- S7::new_class("tu_minute", parent = mt_unit)
S7::method(time_unit_full, tu_minute) <- function(x) "minute"
S7::method(time_unit_abbr, tu_minute) <- function(x) "min"

#' @rdname calendar_gregorian
#' @export
tu_second <- S7::new_class("tu_second", parent = mt_unit)
S7::method(time_unit_full, tu_second) <- function(x) "second"
S7::method(time_unit_abbr, tu_second) <- function(x) "s"


#' @rdname calendar_gregorian
#' @export
tu_millisecond <- S7::new_class("tu_millisecond", parent = mt_unit)
S7::method(time_unit_full, tu_millisecond) <- function(x) "millisecond"
S7::method(time_unit_abbr, tu_millisecond) <- function(x) "ms"


### Calendar algebra S7::methods for Gregorian time units
S7::method(chronon_cardinality, list(tu_year, tu_quarter)) <- function(x, y, at = NULL) {
  x@x*4/y@x
}
S7::method(chronon_cardinality, list(tu_year, tu_month)) <- function(x, y, at = NULL) {
  x@x*12/y@x
}
S7::method(chronon_cardinality, list(tu_year, tu_day)) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop("The number of days in a year requires the time context `at`.", call. = FALSE)
  }
  is_leap_year(1970L + as.integer(at)) + 365L
}
S7::method(chronon_cardinality, list(tu_quarter, tu_month)) <- function(x, y, at = NULL) {
  x@x*3/y@x
}
S7::method(chronon_cardinality, list(tu_day, tu_hour)) <- function(x, y, at = NULL) {
  # TODO: Handle timezones if `at` is provided
  x@x*24/y@x
}
S7::method(chronon_cardinality, list(tu_hour, tu_minute)) <- function(x, y, at = NULL) {
  # TODO: Handle timezones if `at` is provided
  x@x*60/y@x
}
S7::method(chronon_cardinality, list(tu_minute, tu_second)) <- function(x, y, at = NULL) {
  # if(at %in% .leap.seconds) 61 else 60

  x@x*60/y@x
}
S7::method(chronon_cardinality, list(tu_second, tu_millisecond)) <- function(x, y, at = NULL) {
  x@x*1000/y@x
}

monthdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

is_leap_year <- function(year) {
  (year %% 4L == 0L & year %% 100L != 0L) | (year %% 400L == 0L)
}

S7::method(chronon_cardinality, list(tu_month, tu_day)) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop("The number of days in a month requires the time context `at`.", call. = FALSE)
  }
  at <- as.integer(at)
  year <- 1970L + at %/% 12
  month <- ((at + 1L) %% 12) + 1L
  monthdays[month] + (month == 2L & is_leap_year(year))
}

### Chronon casting between Gregorian time units
S7::method(chronon_divmod, list(tu_day, tu_month)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to months
  if (chronon_cardinality(to, tu_month(1L)) != 1L) {
    stop("Converting to non-month chronons from days not yet supported", call. = FALSE)
  }

  # TODO: should be swapped out to arithmetic on integer days since epoch
  x <- as.POSIXlt(as.Date(x))
  list(
    chronon = (x$year-70L)*12L + x$mon,
    remainder = x$mday - 1L
  )
}
S7::method(chronon_divmod, list(tu_month, tu_day)) <- function(from, to, x) {
  # Convert to months since epoch
  x <- chronon_cardinality(from, tu_month(1L))*x
  

  year <- x%/%12L + 1970L
  month <- (x%%12L) + 1L

  # Start of the month in days since epoch
  result <- 
    # Years since 1970
    365 * (year - 1970) +
    # Leap days since 1970
    (year - 1968)%/%4 - (year - 1900)%/%100 + (year - 1600)%/%400 + 
    # Days this year before this month
    (367 * month - 362)%/%12 +
    (month > 2) * (-2 + is_leap_year(year))


  list(
    chronon = result,
    remainder = 0L
  )
}


S7::method(chronon_divmod, list(tu_day, tu_year)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to years
  if (chronon_cardinality(to, tu_year(1L)) != 1L) {
    stop("Converting to non-year chronons from days not yet supported", call. = FALSE)
  }

  x <- as.POSIXlt(as.Date(x))
  list(
    chronon = x$year-70L,
    remainder = x$yday + 1L
  )
}
S7::method(chronon_divmod, list(tu_year, tu_day)) <- function(from, to, x) {
  # Convert to years since epoch
  x <- chronon_cardinality(from, tu_year(1L))*x
  
  # TODO: should be swapped out to arithmetic on integer years since epoch
  list(
    chronon = unclass(ISOdate(1970L + x, 1L, 1L, 0L, 0L, 0L))/86400L,
    remainder = 0L
  )
}


### Cyclical labels for Gregorian time units
S7::method(cyclical_labels, list(tu_month, tu_year)) <- function(granule, cycle, i) {
  month.abb[i]
}


#' Gregorian continuous time representations
#' 
#' \lifecycle{experimental}
#' 
#' @param .data Another object to be coerced into the specified time.
#' @param tz Timezone, defaults to "UTC".
#' @param discrete If `TRUE`, the number of chronons since Unix epoch that
#' `.data` falls into is returned as an integer. If `FALSE`, a fractional number
#'  of chronons is returned (analagous to time using a continuous time model).
#' @param ... Arguments for methods.
#' 
#' @examples
#' 
#' year(Sys.Date())
#' 
#' @rdname gregorian-continuous
#' @export
year <- linear_time(
  chronon = tu_year(1L)
)

#' @examples
#' 
#' yearquarter(0:7)
#' 
#' @rdname gregorian-continuous
#' @export
yearquarter <- linear_time(
  granules = list(tu_year(1L)),
  chronon = tu_quarter(1L)
)

#' @examples
#' 
#' yearmonth(Sys.Date())
#' 
#' @rdname gregorian-continuous
#' @export
yearmonth <- linear_time(
  granules = list(tu_year(1L)),
  chronon = tu_month(1L)
)
