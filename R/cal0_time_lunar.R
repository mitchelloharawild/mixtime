#' Lunar time unit classes
#'
#' Time unit constructors for the lunar time system where the boundary of each
#' day is at sunrise, sunset, or noon. This calendar is intended to be
#' built on by other calendars to add common time components.
#'
#' @return A time unit object for the Gregorian calendar system.
#' 
#' @details
#' The following time units are available in the lunar calendar systems.
#' 
#' - `month()`: Synodic month unit
#' - `phase()`: Synodic phase unit
#' 
#' @seealso [`cal_time_civil_midnight`]
#' 
#' @examples
#' # Find the time of a new moon in the Gregorian calendar
#' t <- linear_time(Sys.Date(), cal_time_lunar$month(1L, lat = -37.8136, lon = 144.9631))
#' datetime(t, tz = "Australia/Melbourne")
#' 
#' 
#' @name calendar_time_solar
#' @export
cal_time_lunar <- new_calendar(
  month = S7::new_class("tu_month_synodic", parent = mt_loc_unit),
  phase = S7::new_class("tu_phase_synodic", parent = mt_loc_unit)
)

# Time unit labels
method(time_unit_full, cal_time_lunar$month) <- function(x) "synodic month"
method(time_unit_abbr, cal_time_lunar$month) <- function(x) "LM"
method(time_unit_full, cal_time_lunar$phase) <- function(x) "synodic phase"
method(time_unit_abbr, cal_time_lunar$phase) <- function(x) "LP"

# Default formats
method(chronon_format_linear, list(cal_time_lunar$month, class_any)) <- function(x, cal) "L{lin(month)}"
method(chronon_format_linear, list(cal_time_lunar$phase, class_any)) <- function(x, cal) "L{lin(month)} {cyc(phase,month,label=TRUE)}"
method(chronon_format_cyclical, list(cal_time_lunar$phase, cal_time_lunar$month)) <- function(x, y) "{cyc(phase,month,label=TRUE)}"

# Epoch for lunations
method(chronon_epoch, cal_time_lunar$month) <- function(x) 953L

# The number of UTC seconds in a lunar (synodic) phase
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_lunar$phase)) <- function(x, y, at = NULL) {
  at <- approx_lunations_from_utc(at / 8)
  (approx_utc_from_lunations(at + vec_data(y)) - approx_utc_from_lunations(at)) / vec_data(x)
}

# The number of lunar phases in a lunar (synodic) month
method(chronon_cardinality, list(cal_time_lunar$phase, cal_time_lunar$month)) <- function(x, y, at = NULL) {
  8L
}

# The number of UTC seconds in a lunar (synodic) phase
method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_lunar$phase)) <- function(from, to, x) {
  list(
    div = approx_lunations_from_utc(as.double(x)) * 8,
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_lunar$phase, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_lunations(as.double(x) / 8),
    mod = 0
  )
}

lunar_phase_abbr <- c("NM", "WxC", "FQ", "WxG", "FM", "WnG", "LQ", "WnC")
lunar_phase_full <- c("new moon", "waxing crescent", "first quarter", "waxing gibbous", "full moon", "waning gibbous", "last quarter", "waning crescent")
lunar_phase_emoji <- c("🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘")
method(cyclical_labels, list(cal_time_lunar$phase, cal_time_lunar$month)) <- function(granule, cycle, i, label = TRUE, abbreviate = TRUE, emoji = TRUE, intermediate = TRUE) {
  i <- i + 1L
  if (label) {
    if (emoji) {
      lunar_phase_emoji[i]
    } else if (abbreviate) {
      lunar_phase_abbr[i]
    } else {
      lunar_phase_full[i]
    }
  } else {
    as.character(i)
  }
}