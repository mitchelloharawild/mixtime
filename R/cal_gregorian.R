#' Gregorian time unit classes
#'
#' Time unit constructors for the Gregorian calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @return A time unit object for the Gregorian calendar system.
#' 
#' @details
#' The following time units are available in the Gregorian calendar (`cal_gregorian$`).
#' 
#' - `year()`: Year unit
#' - `quarter()`: Quarter (3-month period) unit
#' - `month()`: Month unit
#' - `day()`: Day unit
#' - `hour()`: Hour unit
#' - `minute()`: Minute unit
#' - `second()`: Second unit
#' - `millisecond()`: Millisecond unit
#' 
#' These units form a hierarchy where conversions between adjacent units follow
#' the Gregorian calendar rules. For units that don't have a fixed relationship
#' (e.g., months to days), the conversion requires a time context.
#' 
#' @seealso [linear_time()] for creating linear time points.
#' 
#' @examples
#' # Create a custom time representation using Gregorian units
#' linear_time(
#'   Sys.time(),
#'   chronon = hour(1L)
#' )
#' 
#' @name calendar_gregorian
#' @export
cal_gregorian <- new_calendar(
  year = new_class("tu_year", parent = mt_tz_unit),
  quarter = new_class("tu_quarter", parent = mt_tz_unit),
  month = new_class("tu_month", parent = mt_tz_unit),
  day = cal_time_civil_midnight$day,
  hour = cal_time_civil_midnight$hour,
  minute = cal_time_civil_midnight$minute,
  second = cal_time_civil_midnight$second,
  millisecond = cal_time_civil_midnight$millisecond,
  class = "cal_gregorian"
)

# Time unit labels
method(time_unit_full, cal_gregorian$year) <- function(x) "year"
method(time_unit_abbr, cal_gregorian$year) <- function(x) "Y"
method(time_unit_full, cal_gregorian$quarter) <- function(x) "quarter"
method(time_unit_abbr, cal_gregorian$quarter) <- function(x) "Q"
method(time_unit_full, cal_gregorian$month) <- function(x) "month"
method(time_unit_abbr, cal_gregorian$month) <- function(x) "M"

# Default granules
method(chronon_granules, cal_gregorian$quarter) <- function(x) list(cal_gregorian$year(1L))
method(chronon_granules, cal_gregorian$month) <- function(x) list(cal_gregorian$year(1L))

# Default formats
method(chronon_format, cal_gregorian$year) <- function(x) "{year}"
method(chronon_format, cal_gregorian$quarter) <- function(x) "{year} Q{quarter}"
method(chronon_format, cal_gregorian$month) <- function(x) "{year} {month}"

### Calendar algebra methods for Gregorian time units
method(chronon_cardinality, list(cal_gregorian$year, cal_gregorian$quarter)) <- function(x, y, at = NULL) {
  vec_data(x)*4/vec_data(y)
}
method(chronon_cardinality, list(cal_gregorian$year, cal_gregorian$month)) <- function(x, y, at = NULL) {
  vec_data(x)*12/vec_data(y)
}
method(chronon_cardinality, list(cal_gregorian$year, cal_gregorian$day)) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop("The number of days in a year requires the time context `at`.", call. = FALSE)
  }
  if(vec_data(x) != 1L) {
    cli::cli_abort("Multi-year chronons are not yet supported for conversion to days.")
  }
  (is_leap_year(1970L + as.integer(at)) + 365L)/vec_data(y)
}
method(chronon_cardinality, list(cal_gregorian$quarter, cal_gregorian$month)) <- function(x, y, at = NULL) {
  vec_data(x)*3/vec_data(y)
}

monthdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

is_leap_year <- function(year) {
  year <- floor(year)
  (year %% 4L == 0L & year %% 100L != 0L) | (year %% 400L == 0L)
}

method(chronon_cardinality, list(cal_gregorian$month, cal_gregorian$day)) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop("The number of days in a month requires the time context `at`.", call. = FALSE)
  }

  n_months <- vec_data(x)

  if(n_months >= 12) {
    cli::cli_abort("Month chronons >= 12 are not yet supported for conversion to days.")
  }

  period_len <- circsum(monthdays, n_months)

  at <- as.integer(at)
  period_idx <- (at %% length(period_len)) + 1L
  start_month <- ((at * n_months) %% 12L) + 1L
  year <- 1970L + (at * n_months) %/% 12L

  feb_offset <- (2L - start_month + 12L) %% 12L
  contains_feb <- feb_offset < n_months
  feb_year <- year + (start_month - 1L + feb_offset) %/% 12L
  md <- period_len[period_idx] + (contains_feb & is_leap_year(feb_year))

  md/vec_data(y)
}

### Chronon casting between Gregorian time units
method(chronon_divmod, list(cal_gregorian$day, cal_gregorian$month)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to months
  # if (chronon_cardinality(to, cal_gregorian$month(1L)) != 1L) {
  #   stop("Converting to non-month chronons from days not yet supported", call. = FALSE)
  # }

  # Scale `x` to be 1 day increments
  x_scale <- chronon_cardinality(from, cal_gregorian$day(1L))
  x <- x_scale * x

  # Shift to days since 0000-03-01 (algorithm anchor)
  z <- x + 719468L
  
  # (day) -> (year, month, day) arithmetic
  era   <- (z >= 0L) * (z %/% 146097L) + (z < 0L) * ((z - 146096L) %/% 146097L)
  doe   <- z - era * 146097L                      # day-of-era [0, 146096]
  yoe   <- (doe - doe %/% 1460L + doe %/% 36524L - doe %/% 146096L) %/% 365L
  doy   <- doe - (365L * yoe + yoe %/% 4L - yoe %/% 100L + yoe %/% 400L)
  mp    <- (5L * doy + 2L) %/% 153L               # month prime [0, 11]
  day   <- doy - (153L * mp + 2L) %/% 5L          # day [0, 30]
  month <- mp + 3L - 12L * (mp >= 10L)            # month [1, 12]
  year  <- yoe + era * 400L + (month <= 2L)       # year (proleptic Gregorian)

  # Scale 1 month result (res) to `to` months increments
  res <- (year-1970L)*12L + month - 1L
  res_scale <- chronon_cardinality(to, cal_gregorian$month(1L))
  # Importantly, updating the remainder days that now span multiple months
  if (res_scale != 1L) {
    chronon_cardinality(to, from, res %/% res_scale)
  }

  list(
    div = res %/% res_scale,
    mod = day / x_scale # This is in cal_gregorian$day(1L), should be cal_gregorian$day(***?)
  )
}
method(chronon_divmod, list(cal_gregorian$month, cal_gregorian$day)) <- function(from, to, x) {
  # Convert to months since epoch
  x <- chronon_cardinality(from, cal_gregorian$month(1L))*x
  
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

  # Scale by `to` day chronons
  result <- result / chronon_cardinality(to, cal_gregorian$day(1L))

  list(
    div = result,
    mod = 0L
  )
}


method(chronon_divmod, list(cal_gregorian$day, cal_gregorian$year)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to years
  if (chronon_cardinality(to, cal_gregorian$year(1L)) != 1L) {
    stop("Converting to non-year chronons from days not yet supported", call. = FALSE)
  }

  # Scale `x` to be 1 day increments
  x_scale <- chronon_cardinality(from, cal_gregorian$day(1L))
  x <- x_scale * x

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
    div = year-1970L,
    mod = yday/x_scale
  )
}
method(chronon_divmod, list(cal_gregorian$year, cal_gregorian$day)) <- function(from, to, x) {
  # Convert to years since epoch
  x <- chronon_cardinality(from, cal_gregorian$year(1L))*x
  
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
  
  
  # Scale by `to` day chronons
  d <- d / chronon_cardinality(to, cal_gregorian$day(1L))

  list(
    div = d,
    mod = 0L
  )
}

### Cyclical labels for Gregorian time units
method(cyclical_labels, list(cal_gregorian$quarter, S7::class_any)) <- function(granule, cycle, i) {
  # Quarters count with 1-indexing
  as.character(i + 1L)
}
method(cyclical_labels, list(cal_gregorian$month, cal_gregorian$year)) <- function(granule, cycle, i, label = TRUE, abbr = TRUE) {
  if (label) {
    (if (abbr) month.abb else month.name)[i+1L] 
  } else {
    sprintf("%02d", i + 1L)
  }
}