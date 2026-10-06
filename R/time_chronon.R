#' Obtain the chronon of a time object
#' 
#' This S7 generic function extracts the chronon (the smallest time granule) from a
#' time object, such as continuous time or cyclical time representations.
#'
#' For [base::Date] and [base::POSIXct] objects the chronon is identified from
#' the greatest common divisor of the intervals between observations, in the
#' same way as a tsibble's interval. For example, hourly datetimes have a 1 hour
#' chronon rather than 1 second. A multiple of a unit (e.g. 3 hours) is only
#' used if every observation falls on a boundary of that chronon; otherwise a
#' single unit is used so that no information is lost when converting.
#'
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A time [duration()] vector representing the chronon of each
#'   value (e.g., `days(1L)`).
#' 
#' @examples
#' 
#' # The chronon of a Date or POSIXct object is identified from the interval
#' # between observations (as with tsibble's interval)
#' time_chronon(Sys.Date() + 0:2)
#' time_chronon(as.POSIXct("2024-01-01", tz = "UTC") + 3600 * 0:2)
#'
#' # Without an interval, the chronon of a Date is 1 day and a POSIXct is 1 second
#' time_chronon(Sys.Date())
#' time_chronon(Sys.time())
#' 
#' # The chronon of a continuous time year and month is 1 month
#' time_chronon(yearmonth(Sys.Date()))
#' 
#' # The common chronon of a mixed time object is the finest chronon
#' time_chronon(c(yearmonth(Sys.Date()), Sys.Date()))
#' 
#' @export
time_chronon <- S7::new_generic("time_chronon", "x")

S7::method(time_chronon, class_mixtime) <- function(x) {
  x@x <- lapply(x@x, function(x) time_chronon(x)@x[[1L]])
  x
}

S7::method(time_chronon, mt_time) <- function(x) {
  duration(rep(1L, length(x)), attr(x, "chronon"))
}

S7::method(time_chronon, class_any) <- function(x) {
  duration(rep(1L, length(x)), mt_unit(1L))
}

S7::method(time_chronon, S7::new_S3_class("Date")) <- function(x) {
  # Date is a naive chronon of days, spanning the observed interval
  duration(
    rep(1L, length(x)),
    chronon = chronon_from_interval(
      as.numeric(x),
      units = list(cal_gregorian$day),
      sizes = 1
    )
  )
}

S7::method(time_chronon, S7::new_S3_class("POSIXt")) <- function(x) {
  # POSIXct is a chronon of the observed interval with time zone information
  x <- as.POSIXct(x)
  tz <- attr(x, "tzone") %||% "UTC"
  # An empty time zone is the session's local time zone (as in base R): the TZ
  # environment variable, or else the system time zone.
  if (identical(tz, "")) {
    tz <- Sys.getenv("TZ")
    if (!nzchar(tz)) tz <- Sys.timezone()
  }
  # Measure the interval in local (wall clock) seconds, so that e.g. daily
  # observations at local midnight are whole days across DST transitions.
  x <- as.numeric(x)
  local <- x + get_tz_offset(x, tz)
  duration(
    rep(1L, length(x)),
    chronon = chronon_from_interval(
      local,
      units = with(cal_gregorian, list(day, hour, minute, second, millisecond)),
      sizes = c(86400, 3600, 60, 1, 1e-3),
      fallback = cal_gregorian$second,
      tz = tz
    )
  )
}

# Identify the chronon of a time vector from its observed interval, in the same
# way as {tsibble}'s interval: the greatest common divisor of the differences
# between observations.
#
# `x` is numeric time since epoch (in local time), and `units` are candidate
# unit constructors ordered from coarsest to finest, with fixed `sizes` in the
# units of `x`. The chronon's unit is the coarsest in which every observation is
# a whole number, and its size `n` is the gcd of the differences between
# observations measured in that unit.
#
# A chronon of `n > 1` units floors values into epoch-aligned blocks, so `n` is
# only used when every observation falls on a block boundary. Otherwise the
# chronon is a single unit, so that the conversion never loses information.
#
# With fewer than two distinct observations there is no interval, so the
# `fallback` unit is used (defaulting to the finest candidate unit).
chronon_from_interval <- function(x, units, sizes, ..., fallback = units[[length(units)]]) {
  x <- unique(x[is.finite(x)])
  if (length(x) < 2L) return(fallback(1L, ...))

  for (i in seq_along(units)) {
    v <- x / sizes[[i]]
    if (any(abs(v - round(v)) > 1e-6)) next

    v <- round(v)
    chronon <- units[[i]](1L, ...)
    n <- Reduce(gcd, abs(diff(sort(v))))
    if (n > 1L && all(v %% n == 0)) {
      chronon@n <- chronon@n * n
    }
    return(chronon)
  }
  fallback(1L, ...)
}

# {tsibble} time classes
S7::method(time_chronon, S7::new_S3_class("yearquarter")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_gregorian$quarter(1L))
}
S7::method(time_chronon, S7::new_S3_class("yearmonth")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_gregorian$month(1L))
}
S7::method(time_chronon, S7::new_S3_class("yearweek")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_isoweek$week(1L))
}

# {zoo} time classes
S7::method(time_chronon, S7::new_S3_class("yearqtr")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_gregorian$quarter(1L))
}
S7::method(time_chronon, S7::new_S3_class("yearmon")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_gregorian$month(1L))
}

# {hms} time class
S7::method(time_chronon, S7::new_S3_class("hms")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_gregorian$second(1L))
}

# {lubridate::Period} time class
S7::method(time_chronon, new_S4_class("Period", package = "lubridate")) <- function(x) {
  components <- c(second = list(`attributes<-`(x, NULL)), attributes(unclass(x)))
  has_component <- vapply(components, function(x) sum(x) != 0L, logical(1L))
  if (sum(has_component) != 1L) {
    cli::cli_abort("Support for {.fn lubridate::period} data in mixtime is limited to single unit periods.")
  }
  chronon <- switch(
    names(components)[has_component],
    year = cal_gregorian$year(1L),
    month = cal_gregorian$month(1L),
    day = cal_gregorian$day(1L),
    hour = cal_gregorian$hour(1L),
    minute = cal_gregorian$minute(1L),
    second = cal_gregorian$second(1L)
  )
  duration(rep(1L, length(x)), chronon = chronon)
}