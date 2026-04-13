#' ISO 8601 time unit classes
#'
#' Time unit constructors for the ISO 8601 calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @format A civil-based calendar containing ISO 8601 time units.
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
  inherit = cal_time_civil,
  class = "cal_isoweek"
)

# Time unit labels
method(time_unit_full, cal_isoweek$year) <- function(x) "isoyear"
method(time_unit_abbr, cal_isoweek$year) <- function(x) "IY"
method(time_unit_full, cal_isoweek$week) <- function(x) "week"
method(time_unit_abbr, cal_isoweek$week) <- function(x) "W"

# Epoch for years
method(chronon_epoch, cal_isoweek$year) <- function(x) 1970L

# Default formats
method(chronon_format_linear, list(cal_isoweek$year, class_any)) <- function(x, cal) "{lin(year)}"
method(chronon_format_linear, list(cal_isoweek$week, class_any)) <- function(x, cal) "{lin(year)} W{cyc(week,year)}"
method(chronon_format_linear, list(cal_isoweek$day, S7::new_S3_class("cal_isoweek"))) <- function(x, cal) "{lin(year)}-W{cyc(week,year)}-{cyc(day,week,label=TRUE)}"
method(chronon_format_cyclical, list(cal_isoweek$day, cal_isoweek$week)) <- function(x, y) "{cyc(day,week,label=TRUE)}"
method(chronon_format_cyclical, list(cal_isoweek$week, cal_isoweek$year)) <- function(x, y) "W{cyc(week,year)}"

method(chronon_cardinality, list(cal_isoweek$week, cal_isoweek$year)) <- function(x, y, at = NULL) {
  if(y@n != 1L) {
    cli::cli_abort("The number of weeks in multi-year chronons is not yet supported.")
  }
  if(x@n != 1L) {
    cli::cli_abort("The number of multi-weeks in year chronons is not yet supported.")
  }
  year <- at + 1970L
  p <- (year + year %/% 4 - year %/% 100 + year %/% 400) %% 7
  ifelse(p == 4 | (p == 3 & is_leap_year(year)), 53L, 52L)  
}

method(chronon_cardinality, list(cal_isoweek$day, cal_isoweek$week)) <- function(x, y, at = NULL) {
  y@n*7L/x@n
}

method(chronon_divmod, list(cal_isoweek$day, cal_isoweek$week)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  divisor <- chronon_cardinality(from, to)
  list(
    div = (x + 3L) %/% divisor,
    mod = (x + 3L) %% divisor
  )
}

method(chronon_divmod, list(cal_isoweek$week, cal_isoweek$day)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  mult <- chronon_cardinality(to, from)

  list(
    div = x*mult - 3L,
    mod = 0L
  )
}

method(chronon_divmod, list(cal_isoweek$week, cal_isoweek$year)) <- function(from, to, x) {
  # Modulo arithmetic to convert from weeks to ISO years
  if (to@n != 1L) {
    stop("Converting to multi-year chronons from weeks is not yet supported", call. = FALSE)
  }
  if (from@n != 1L) {
    stop("Converting from multi-week chronons to years is not yet supported", call. = FALSE)
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

method(chronon_divmod, list(cal_isoweek$year, cal_isoweek$week)) <- function(from, to, x) {
  if (to@n != 1L) {
    stop("Converting to multi-year chronons from weeks is not yet supported", call. = FALSE)
  }
  if (from@n != 1L) {
    stop("Converting from multi-week chronons to years is not yet supported", call. = FALSE)
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

method(cyclical_labels, list(cal_isoweek$day, cal_isoweek$week)) <- function(granule, cycle, i, label = FALSE, abbreviate = TRUE) {
  # TODO: Add offset for different week starting days
  if (!label) {
    as.character(i + 1L)
  } else if (abbreviate) {
    week.abb[i + 1L]
  } else {
    week.name[i + 1L]
  }
}
method(cyclical_labels, list(cal_isoweek$week, S7::class_any)) <- function(granule, cycle, i) {
  # Weeks count with 1-indexing
  sprintf("%02d", i + 1L)
}
