#' Gregorian time unit classes
#'
#' Time unit constructors for the Gregorian calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @format A civil-based calendar containing Gregorian time units.
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
#' @return An S3 list of class `c("cal_gregorian", "mt_calendar")` containing
#'   the named time unit classes of the Gregorian calendar. Each unit is
#'   accessible via `$` notation and calling it with a step size produces a
#'   time granule (e.g., 1 month granule as `cal_gregorian$month(1L)`).
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
  inherit = cal_time_civil,
  class = "cal_gregorian"
)

# Time unit labels
method(time_unit_full, cal_gregorian$year) <- function(x) "year{?/s}"
method(time_unit_abbr, cal_gregorian$year) <- function(x) "Y"
method(time_unit_full, cal_gregorian$quarter) <- function(x) "quarter{?/s}"
method(time_unit_abbr, cal_gregorian$quarter) <- function(x) "Q"
method(time_unit_full, cal_gregorian$month) <- function(x) "month{?/s}"
method(time_unit_abbr, cal_gregorian$month) <- function(x) "M"

# Epoch for years
method(chronon_epoch, cal_gregorian$year) <- function(x) 1970L

# Default formats
method(chronon_format_linear, list(cal_gregorian$year, class_any)) <- function(
  x,
  cal
) {
  "{lin(year)}"
}
method(
  chronon_format_linear,
  list(cal_gregorian$quarter, class_any)
) <- function(x, cal) "{lin(year)} Q{cyc(quarter,year)}"
method(chronon_format_linear, list(cal_gregorian$month, class_any)) <- function(
  x,
  cal
) {
  "{lin(year)} {cyc(month,year,label=TRUE,abbreviate=TRUE)}"
}
method(
  chronon_format_linear,
  list(cal_gregorian$day, S7::new_S3_class("cal_gregorian"))
) <- function(x, cal) "{lin(year)}-{cyc(month,year)}-{cyc(day,month)}"

method(
  chronon_format_cyclical,
  list(cal_gregorian$month, cal_gregorian$year)
) <- function(x, y) "{cyc(month,year,label=TRUE,abbreviate=TRUE)}"

### Calendar algebra methods for Gregorian time units
method(
  chronon_cardinality_fixed,
  list(cal_gregorian$quarter, cal_gregorian$year)
) <- function(x, y) {
  4L
}
method(
  chronon_cardinality_fixed,
  list(cal_gregorian$month, cal_gregorian$year)
) <- function(x, y) {
  12L
}
method(
  chronon_cardinality,
  list(cal_gregorian$day, cal_gregorian$year)
) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop(
      "The number of days in a year requires the time context `at`.",
      call. = FALSE
    )
  }
  if (y@n != 1L) {
    cli::cli_abort(
      "The cardinality of days in multi-year chronons is not yet supported."
    )
  }
  (is_leap_year(1970L + as.integer(at)) + 365L) / x@n
}
method(
  chronon_cardinality_fixed,
  list(cal_gregorian$month, cal_gregorian$quarter)
) <- function(x, y) {
  3L
}

monthdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

is_leap_year <- function(year) {
  year <- floor(year)
  (year %% 4L == 0L & year %% 100L != 0L) | (year %% 400L == 0L)
}

# Days since epoch (1970-01-01) to the 1st of the Gregorian month
# `month_index` months after epoch (0-indexed: 0 is 1970-01, -1 is 1969-12,
# ...). Shared by `chronon_cardinality()` and `chronon_divmod()` below so
# that both agree on where a multi-month window starts and ends.
month_start_days <- function(month_index) {
  year <- fdiv(month_index, 12L) + 1970L
  month <- month_index - 12L * (year - 1970L) + 1L
  ly <- as.integer(is_leap_year(year))

  # Years since 1970
  365L *
    (year - 1970L) +
    # Leap days since 1970
    fdiv(year - 1968L, 4L) -
    fdiv(year - 1900L, 100L) +
    fdiv(year - 1600L, 400L) +
    # Days this year before this month
    fdiv(367L * month - 362L, 12L) +
    (month > 2L) * (-2L + ly) -
    ly
}

method(
  chronon_cardinality,
  list(cal_gregorian$day, cal_gregorian$month)
) <- function(x, y, at = NULL) {
  if (is.null(at)) {
    stop(
      "The number of days in a month requires the time context `at`.",
      call. = FALSE
    )
  }

  n_months <- y@n
  at <- as.integer(at)

  # `at` is a block index in `y`-month units; its window covers the
  # `abs(n_months)` calendar months starting at absolute month `at *
  # n_months`, regardless of `n_months`'s sign.
  start_idx <- at * n_months
  md <- month_start_days(start_idx + abs(n_months)) - month_start_days(start_idx)

  md / x@n
}

### Chronon casting between Gregorian time granules
method(
  chronon_divmod,
  list(cal_gregorian$day, cal_gregorian$month)
) <- function(from, to, x) {
  # Scale `x` to be 1 day increments
  x_scale <- from@n
  x <- x_scale * x
  x_int <- floor(x)
  x_frac <- x - x_int

  # Shift to days since 0000-03-01 (algorithm anchor)
  z <- x_int + 719468L

  # (day) -> (year, month, day) arithmetic
  era <- (z >= 0L) * fdiv(z, 146097L) + (z < 0L) * fdiv(z - 146096L, 146097L)
  doe <- z - era * 146097L # day-of-era [0, 146096]
  yoe <- fdiv(
    doe - fdiv(doe, 1460L) + fdiv(doe, 36524L) - fdiv(doe, 146096L),
    365L
  )
  doy <- doe - (365L * yoe + fdiv(yoe, 4L) - fdiv(yoe, 100L) + fdiv(yoe, 400L))
  mp <- fdiv(5L * doy + 2L, 153L) # month prime [0, 11]
  day <- doy - fdiv(153L * mp + 2L, 5L) # day [0, 30]
  month <- mp + 3L - 12L * (mp >= 10L) # month [1, 12]
  year <- yoe + era * 400L + (month <= 2L) # year (proleptic Gregorian)

  # Absolute month index of `x` (0-indexed months since epoch)
  res <- (year - 1970L) * 12L + month - 1L

  # Block index: the integer `div` whose window [div*n, div*n + |n|)
  # contains `res`. The window always runs forward from `div * n`, so for
  # n > 0 that's floor(res / n); for n < 0 it's -floor(res / -n).
  n <- to@n
  div <- if (n > 0L) fdiv(res, n) else -fdiv(res, -n)

  # Day offset from the window's start to `x`.
  list(
    div = div,
    mod = (x_int - month_start_days(div * n) + x_frac) / x_scale
  )
}
method(
  chronon_divmod,
  list(cal_gregorian$month, cal_gregorian$day)
) <- function(from, to, x) {
  # Convert to months since epoch
  x <- from@n * x

  # Separate integer and fractional parts to correctly handle fractional months
  x_int <- floor(x)
  x_frac <- x - x_int

  year <- fdiv(x_int, 12L) + 1970L
  ly <- as.integer(is_leap_year(year))
  month <- x_int - 12L * (year - 1970L) + 1L

  # Start of the month in days since epoch
  result <-
    # Years since 1970
    365 *
    (year - 1970) +
    # Leap days since 1970
    fdiv(year - 1968, 4) -
    fdiv(year - 1900, 100) +
    fdiv(year - 1600, 400) +
    # Days this year before this month
    fdiv(367 * month - 362, 12) +
    (month > 2) * (-2 + ly) -
    ly

  # Convert fractional months to fractional days using days in current month
  days_in_month <- monthdays[month] + (month == 2L) * ly
  result <- result + x_frac * days_in_month

  # Scale by `to` day chronons
  result <- result / to@n

  list(
    div = result,
    mod = 0L
  )
}


method(chronon_divmod, list(cal_gregorian$day, cal_gregorian$year)) <- function(
  from,
  to,
  x
) {
  # Modulo arithmetic to convert from days to years
  if (to@n != 1L) {
    stop(
      "Converting to non-year chronons from days not yet supported",
      call. = FALSE
    )
  }

  # Scale `x` to be 1 day increments
  x_scale <- from@n
  x <- x_scale * x
  x_int <- floor(x)
  x_frac <- x - x_int

  # Shift to days since 0000-03-01 (algorithm anchor)
  z <- x_int + 719468L

  era <- (z >= 0L) * fdiv(z, 146097L) + (z < 0L) * fdiv(z - 146096L, 146097L)
  doe <- z - era * 146097L # day-of-era [0, 146096]
  yoe <- fdiv(
    doe - fdiv(doe, 1460L) + fdiv(doe, 36524L) - fdiv(doe, 146096L),
    365L
  )
  doy <- doe - (365L * yoe + fdiv(yoe, 4L) - fdiv(yoe, 100L) + fdiv(yoe, 400L))
  mp <- fdiv(5L * doy + 2L, 153L) # month prime [0, 11]
  month <- mp + 3L - 12L * (mp >= 10L) # month [1, 12]
  year <- yoe + era * 400L + (month <= 2L) # year (proleptic Gregorian)

  ly <- is_leap_year(year)
  yday <- (doy + 59 + ly) %% (365L + ly)

  list(
    div = year - 1970L,
    mod = (yday + x_frac) / x_scale
  )
}
method(chronon_divmod, list(cal_gregorian$year, cal_gregorian$day)) <- function(
  from,
  to,
  x
) {
  # Convert to years since epoch
  x <- from@n * x

  floor_int <- function(x) as.integer(floor(x))

  # Days since epoch
  d <- 365L *
    x +
    # Leap days since epoch
    floor_int((x + 1L) / 4) -
    # Subtract century years (not leap years)
    floor_int((x + 69L) / 100) +
    # Add back quad-century years (leap years)
    floor_int((x + 369L) / 400L) +
    # Add fractional part of leap year
    is_leap_year(x + 1970L) * (x - floor_int(x))

  # Scale by `to` day chronons
  d <- d / to@n

  list(
    div = d,
    mod = 0L
  )
}

## Time labels
bc_ad_format <- function(granule, i, ...) {
  ifelse(i <= 0L, paste0(-i + 1L, "BC"), i)
}
bc_ad_parse <- function(granule, ...) {
  list(
    pattern = "\\d+(?:BC)?",
    decode = function(text, at = NULL) {
      bc <- grepl("BC$", text)
      n <- as.integer(sub("BC$", "", text))
      # This override bypasses linear_labels_parse()'s default method, so
      # (unlike a linear_labels() scheme) it must undo the epoch shift
      # chronon_parts() applies for display itself -- see
      # linear_labels_format()'s "Epoch shift" section.
      ifelse(bc, 1L - n, n) - chronon_epoch(granule)
    }
  )
}
method(linear_labels_format, cal_gregorian$year) <- bc_ad_format
method(linear_labels_parse, cal_gregorian$year) <- bc_ad_parse

### Cyclical labels for Gregorian time granules
method(cyclical_labels, list(cal_gregorian$quarter, S7::class_any)) <- label_scheme(start = 1L)
method(cyclical_labels, list(cal_gregorian$month, cal_gregorian$year)) <- label_scheme(
  start = 1L,
  width = 2L,
  vocab = vocab_table(`en-GB` = list(wide = month.name, abbreviated = month.abb))
)
