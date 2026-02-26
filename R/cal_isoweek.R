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
    div = (x + 3L) %/% divisor,
    mod = (x + 3L) %% divisor
  )
}

S7::method(chronon_divmod, list(cal_isoweek$week, cal_isoweek$day)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  mult <- chronon_cardinality(from, to)

  list(
    div = x*mult - 3L,
    mod = 0L
  )
}

S7::method(chronon_divmod, list(cal_isoweek$week, cal_isoweek$year)) <- function(from, to, x) {
  # Modulo arithmetic to convert from weeks to ISO years
  if (chronon_cardinality(to, cal_isoweek$year(1L)) != 1L) {
    stop("Converting to multi-year chronons from weeks is not yet supported", call. = FALSE)
  }

  td   <- x * 7L
  y    <- as.integer(td %/% 365.2425 + 1970L)
  j    <- 365L * (y - 1970L) + (y - 1969L) %/% 4L - (y - 1901L) %/% 100L + (y - 1601L) %/% 400L
  leap <- ((y %% 4L == 0L) & (y %% 100L != 0L)) | (y %% 400L == 0L)
  jn   <- j + 365L + leap

  up   <- jn <= td
  dn   <- j > td
  y    <- y + up - dn
  j[up]  <- jn[up]
  j[dn]  <- 365L * (y[dn] - 1970L) + (y[dn] - 1969L) %/% 4L - (y[dn] - 1901L) %/% 100L + (y[dn] - 1601L) %/% 400L

  dow  <- (j + 3L) %% 7L + 1L
  w1   <- j + (4L - dow) %% 7L - 3L

  list(
    # year offset from epoch
    div = y - 1970L,  
    # 0-indexed week within ISO year [0, 51] or [0, 52]
    mod = (td - 3L - w1) %/% 7L
  )
}

S7::method(chronon_divmod, list(cal_isoweek$year, cal_isoweek$week)) <- function(from, to, x) {
  if (chronon_cardinality(from, cal_isoweek$year(1L)) != 1L) {
    stop("Converting from multi-year chronons to weeks is not yet supported", call. = FALSE)
  }

  # Split integer year offset and fractional (0-indexed week within year)
  y_off <- floor(x)
  frac  <- x - y_off
  y     <- as.integer(y_off) + 1970L

  j    <- 365L * (y - 1970L) + (y - 1969L) %/% 4L - (y - 1901L) %/% 100L + (y - 1601L) %/% 400L
  dow  <- (j + 3L) %% 7L + 1L
  w1   <- j + (4L - dow) %% 7L - 3L  # days since epoch of Monday of ISO week 1

  # weeks since epoch of week 1, plus fractional weeks within the year
  w1_week <- (w1 + 3L) %/% 7L        # since w1 = w*7 - 3, so w = (w1 + 3) / 7

  # frac is 0-indexed weeks within year (the mod from the forward direction)
  # total weeks count within year = 52 or 53
  leap  <- ((y %% 4L == 0L) & (y %% 100L != 0L)) | (y %% 400L == 0L)
  jn    <- j + 365L + leap
  dowN  <- (jn + 3L) %% 7L + 1L
  w1n   <- jn + (4L - dowN) %% 7L - 3L
  n_weeks <- (w1n - w1) %/% 7L       # 52 or 53

  list(
    div = w1_week + as.integer(floor(frac * n_weeks)),
    mod = 0L
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
