#' Obtain the chronon of a time object
#' 
#' This S7 generic function extracts the chronon (the smallest time unit) from a
#' time object, such as continuous time or cyclical time representations.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' 
#' @return A time unit object representing the chronon (e.g., `tu_day(1L)`)
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
#' time_chronon(linear_time(tu_month(1L), list(tu_year(1L)))(Sys.Date()))
time_chronon <- S7::new_generic("time_unit", "x")

S7::method(time_chronon, S7::new_S3_class("mt_linear")) <- function(x) {
  attr(x, "chronon")
}

S7::method(time_chronon, S7::new_S3_class("mt_linear")) <- function(x) {
  attr(x, "chronon")
}

S7::method(time_chronon, S7::new_S3_class("Date")) <- function(x) {
  tu_day(1L)
}

S7::method(time_chronon, S7::new_S3_class("POSIXt")) <- function(x) {
  tu_second(1L)
}