#' Obtain the chronon of a time object
#' 
#' This S7 generic function extracts the chronon (the smallest time granule) from a
#' time object, such as continuous time or cyclical time representations.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A time [duration()] vector representing the chronon of each
#'   value (e.g., `days(1L)`).
#' 
#' @examples
#' 
#' # The chronon of a Date object is 1 day
#' time_chronon(Sys.Date())
#' 
#' # The chronon of a POSIXct object is 1 second
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

S7::method(time_chronon, S7::new_S3_class("mt_time")) <- function(x) {
  duration(rep(1L, length(x)), attr(x, "chronon"))
}

S7::method(time_chronon, class_any) <- function(x) {
  duration(rep(1L, length(x)), mt_unit(1L))
}

S7::method(time_chronon, S7::new_S3_class("Date")) <- function(x) {
  # Date is a 1-day naive chronon
  duration(
    rep(1L, length(x)), 
    chronon = cal_gregorian$day(1L)
  )
}

S7::method(time_chronon, S7::new_S3_class("POSIXt")) <- function(x) {
  # POSIXct is a 1-second chronon with time zone information
  duration(
    rep(1L, length(x)),
    chronon = cal_gregorian$second(1L, tz = attr(x, "tzone") %||% naive_tz)
  )
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