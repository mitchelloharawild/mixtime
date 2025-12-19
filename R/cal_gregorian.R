#' Gregorian time unit classes
#'
#' Time unit constructors for the Gregorian calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @inheritParams mt_unit
#' 
#' @return A time unit object for the Gregorian calendar system.
#' 
#' @details
#' The following Gregorian time units are available:
#' 
#' - `tu_year()`: Year unit
#' - `tu_quarter()`: Quarter (3-month period) unit
#' - `tu_month()`: Month unit
#' - `tu_day()`: Day unit
#' - `tu_hour()`: Hour unit
#' - `tu_minute()`: Minute unit
#' - `tu_second()`: Second unit
#' - `tu_millisecond()`: Millisecond unit
#' 
#' These units form a hierarchy where conversions between adjacent units follow
#' the Gregorian calendar rules. For units that don't have a fixed relationship
#' (e.g., months to days), the conversion requires a time context.
#' 
#' @seealso [linear_time()] for creating custom time representations,
#'   [linear_gregorian] for pre-defined Gregorian time representations
#' 
#' @examples
#' # Create a custom time representation using Gregorian units
#' dayhour <- linear_time(
#'   granules = list(tu_day(1L)),
#'   chronon = tu_hour(1L)
#' )
#' 
#' @name calendar_gregorian
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
  vec_data(x)*4/vec_data(y)
}
S7::method(chronon_cardinality, list(tu_year, tu_month)) <- function(x, y, at = NULL) {
  vec_data(x)*12/vec_data(y)
}
S7::method(chronon_cardinality, list(tu_year, tu_day)) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop("The number of days in a year requires the time context `at`.", call. = FALSE)
  }
  is_leap_year(1970L + as.integer(at)) + 365L
}
S7::method(chronon_cardinality, list(tu_quarter, tu_month)) <- function(x, y, at = NULL) {
  vec_data(x)*3/vec_data(y)
}
S7::method(chronon_cardinality, list(tu_day, tu_hour)) <- function(x, y, at = NULL) {
  # TODO: Handle timezones if `at` is provided
  vec_data(x)*24/vec_data(y)
}
S7::method(chronon_cardinality, list(tu_hour, tu_minute)) <- function(x, y, at = NULL) {
  # TODO: Handle timezones if `at` is provided
  vec_data(x)*60/vec_data(y)
}
S7::method(chronon_cardinality, list(tu_minute, tu_second)) <- function(x, y, at = NULL) {
  # if(at %in% .leap.seconds) 61 else 60

  vec_data(x)*60/vec_data(y)
}
S7::method(chronon_cardinality, list(tu_second, tu_millisecond)) <- function(x, y, at = NULL) {
  vec_data(x)*1000/vec_data(y)
}

monthdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

is_leap_year <- function(year) {
  year <- floor(year)
  (year %% 4L == 0L & year %% 100L != 0L) | (year %% 400L == 0L)
}

S7::method(chronon_cardinality, list(tu_month, tu_day)) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop("The number of days in a month requires the time context `at`.", call. = FALSE)
  }
  at <- as.integer(at)
  year <- 1970L + at %/% 12
  month <- (at %% 12) + 1L
  monthdays[month] + (month == 2L & is_leap_year(year))
}

### Chronon casting between Gregorian time units
S7::method(chronon_divmod, list(tu_day, tu_month)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to months
  if (chronon_cardinality(to, tu_month(1L)) != 1L) {
    stop("Converting to non-month chronons from days not yet supported", call. = FALSE)
  }

  # Shift to days since 0000-03-01 (algorithm anchor)
  z <- x + 719468L
  
  era   <- (z >= 0L) * (z %/% 146097L) + (z < 0L) * ((z - 146096L) %/% 146097L)
  doe   <- z - era * 146097L                      # day-of-era [0, 146096]
  yoe   <- (doe - doe %/% 1460L + doe %/% 36524L - doe %/% 146096L) %/% 365L
  doy   <- doe - (365L * yoe + yoe %/% 4L - yoe %/% 100L + yoe %/% 400L)
  mp    <- (5L * doy + 2L) %/% 153L               # month prime [0, 11]
  day   <- doy - (153L * mp + 2L) %/% 5L          # day [0, 30]
  month <- mp + 3L - 12L * (mp >= 10L)            # month [1, 12]
  year  <- yoe + era * 400L + (month <= 2L)       # year (proleptic Gregorian)

  list(
    chronon = (year-1970L)*12L + month - 1L,
    remainder = day
  )
}
S7::method(chronon_divmod, list(tu_month, tu_day)) <- function(from, to, x) {
  # Convert to months since epoch
  x <- chronon_cardinality(from, tu_month(1L))*x
  
  year <- x%/%12L + 1970L
  ly <- as.integer(is_leap_year(year))
  month <- (x%%12L) + 1L

  # Start of the month in days since epoch
  result <- 
    # Years since 1970
    365 * (year - 1970) +
    # Leap days since 1970
    (year - 1968)%/%4 - (year - 1900)%/%100 + (year - 1600)%/%400 + 
    # Days this year before this month
    (367 * month - 362)%/%12 +
    (month > 2) * (-2 + ly) - ly

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

  # Shift to days since 0000-03-01 (algorithm anchor)
  z     <- x + 719468L

  era   <- (z >= 0L) * (z %/% 146097L) + (z < 0L) * ((z - 146096L) %/% 146097L)
  doe   <- z - era * 146097L                      # day-of-era [0, 146096]
  yoe   <- (doe - doe %/% 1460L + doe %/% 36524L - doe %/% 146096L) %/% 365L
  doy   <- doe - (365L * yoe + yoe %/% 4L - yoe %/% 100L + yoe %/% 400L)
  mp    <- (5L * doy + 2L) %/% 153L               # month prime [0, 11]
  month <- mp + 3L - 12L * (mp >= 10L)            # month [1, 12]
  year  <- yoe + era * 400L + (month <= 2L)       # year (proleptic Gregorian)

  ly <- is_leap_year(year)
  yday <- (doy + 59 + ly) %% (365L + ly)
  
  list(
    chronon = year-1970L,
    remainder = yday
  )
}
S7::method(chronon_divmod, list(tu_year, tu_day)) <- function(from, to, x) {
  # Convert to years since epoch
  x <- chronon_cardinality(from, tu_year(1L))*x
  
  floor_int <- function(x) as.integer(floor(x))

  # Days since epoch
  d <- 365L * x + 
    # Leap days since epoch
    floor_int((x+1L) / 4) - 
    # Subtract century years (not leap years)
    floor_int((x+69L) / 100) + 
    # Add back quad-century years (leap years)
    floor_int((x+369L) / 400L) + 
    # Add fractional part of leap year
    is_leap_year(x+1970L) * (x - floor_int(x))

  list(
    chronon = d,
    remainder = 0L
  )
}


### Cyclical labels for Gregorian time units
S7::method(cyclical_labels, list(tu_month, tu_year)) <- function(granule, cycle, i) {
  month.abb[i+1L]
}



#' Gregorian continuous time representations
#' 
#' Linear time representations for the Gregorian calendar system. These functions
#' create time objects measured in years, year-quarters, or year-months since the
#' Unix epoch (1970-01-01).
#' 
#' @param .data Another object to be coerced into the specified time.
#' @param tz Timezone, defaults to "UTC".
#' @param discrete If `TRUE`, the number of chronons since Unix epoch that
#' `.data` falls into is returned as an integer. If `FALSE`, a fractional number
#'  of chronons is returned (analagous to time using a continuous time model).
#' 
#' @details
#' - `year()`: Represents time in whole years since 1970. The chronon is one year.
#' - `yearquarter()`: Represents time in quarters, grouped by year. The chronon
#'   is one quarter, with years as the granule for display and grouping.
#' - `yearmonth()`: Represents time in months, grouped by year. The chronon is
#'   one month, with years as the granule for display and grouping.
#' 
#' @section Custom Gregorian time representations:
#' You can create custom time representations using [linear_time()] with any of
#' the supported Gregorian time units (see [calendar_gregorian]).
#' 
#' For example, to create a time representation in hours since epoch with day granules:
#' ```r
#' dayhour <- linear_time(
#'   granules = list(tu_day(1L)),
#'   chronon = tu_hour(1L)
#' )
#' ```
#' 
#' @examples
#' 
#' year(Sys.Date())
#' year(Sys.Date(), discrete = FALSE)
#' 
#' @name linear_gregorian
#' @export
year <- linear_time(
  chronon = tu_year(1L)
)

#' @examples
#' 
#' yearquarter(Sys.Date())
#' yearquarter(Sys.Date(), discrete = FALSE)
#' 
#' @rdname linear_gregorian
#' @export
yearquarter <- linear_time(
  chronon = tu_quarter(1L),
  granules = list(tu_year(1L))
)

#' @examples
#' 
#' yearmonth(Sys.Date())
#' yearmonth(Sys.Date(), discrete = FALSE)
#' 
#' @rdname linear_gregorian
#' @export
yearmonth <- linear_time(
  chronon = tu_month(1L),
  granules = list(tu_year(1L))
)


#' Gregorian cyclical time representations
#' 
#' Cyclical time representations for the Gregorian calendar system. These functions
#' create time objects that repeat within a larger time cycle, useful for identifying
#' seasonal patterns or positions within a calendar period.
#' 
#' @param .data Another object to be coerced into the specified cyclical time.
#' @param discrete If `TRUE`, the position within the cycle that `.data` 
#' falls into is returned as an integer. If `FALSE`, a fractional 
#' position is returned (analagous to time using a continuous time model).
#' @inheritParams linear_gregorian
#' 
#' @details
#' - `month_of_year()`: Represents the month position within a year (1-12).
#'   The chronon is one month, cycling within a year.
#' - `day_of_year()`: Represents the day position within a year (1-365 or 1-366
#'   for leap years). The chronon is one day, cycling within a year.
#' - `day_of_month()`: Represents the day position within a month (1-28, 1-29,
#'   1-30, or 1-31 depending on the month). The chronon is one day, cycling
#'   within a month.
#' 
#' These cyclical representations are useful for analyzing seasonal patterns or
#' comparing time points at similar positions across different years.
#' 
#' @section Custom Gregorian cyclical time representations:
#' You can create custom cyclical time representations using [cyclical_time()]
#' with any of the supported Gregorian time units (see [calendar_gregorian]).
#' 
#' For example, to create a representation for day of the month:
#' ```r
#' day_of_month <- cyclical_time(
#'   chronon = tu_day(1L),
#'   cycle = tu_month(1L)
#' )
#' ```
#' 
#' @seealso [linear_gregorian] for linear Gregorian time representations,
#'   [cyclical_time()] for creating custom cyclical time representations
#' 
#' @examples
#' 
#' month_of_year(Sys.Date())
#' 
#' @name cyclical_gregorian
#' @export
month_of_year <- cyclical_time(
  chronon = tu_month(1L),
  cycle = tu_year(1L)
)

#' @examples
#' 
#' day_of_year(Sys.Date())
#' 
#' @rdname cyclical_gregorian
#' @export
day_of_year <- cyclical_time(
  chronon = tu_day(1L),
  cycle = tu_year(1L)
)

#' @rdname cyclical_gregorian
#' @export
day_of_month <- cyclical_time(
  chronon = tu_day(1L),
  cycle = tu_month(1L)
)