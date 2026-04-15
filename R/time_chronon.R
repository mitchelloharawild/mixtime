#' Obtain the chronon of a time object
#' 
#' This S7 generic function extracts the chronon (the smallest time unit) from a
#' time object, such as continuous time or cyclical time representations.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A time unit object representing the chronon (e.g., `cal_gregorian$day(1L)`)
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
  chronon_common(!!!lapply(x@x, time_chronon))
}

S7::method(time_chronon, S7::new_S3_class("mt_time")) <- function(x) {
  attr(x, "chronon")
}

S7::method(time_chronon, S7::new_S3_class("Date")) <- function(x) {
  cal_gregorian$day(1L, tz = "")
}

S7::method(time_chronon, S7::new_S3_class("POSIXt")) <- function(x) {
  cal_gregorian$second(1L, tz = attr(x, "tzone") %||% "")
}

# {tsibble} time classes
S7::method(time_chronon, S7::new_S3_class("yearquarter")) <- function(x) {
  cal_gregorian$quarter(1L, tz = "")
}
S7::method(time_chronon, S7::new_S3_class("yearmonth")) <- function(x) {
  cal_gregorian$month(1L, tz = "")
}
S7::method(time_chronon, S7::new_S3_class("yearweek")) <- function(x) {
  cal_isoweek$week(1L, tz = "")
}

# {zoo} time classes
S7::method(time_chronon, S7::new_S3_class("yearqtr")) <- function(x) {
  cal_isoweek$week(1L, tz = "")
}
S7::method(time_chronon, S7::new_S3_class("yearmon")) <- function(x) {
  cal_isoweek$week(1L, tz = "")
}

# {hms} time class
S7::method(time_chronon, S7::new_S3_class("hms")) <- function(x) {
  cal_gregorian$second(1L, tz = "")
}