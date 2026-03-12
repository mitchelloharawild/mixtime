#' Obtain the cycle of a time object
#' 
#' This S7 generic function extracts the cycle (the cyclical time unit) from a
#' time object, such as cyclical time representations.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A time unit object representing the cycle, or `NULL` if the object
#'   has no cyclical component.
#' 
#' @examples
#' 
#' # Non-cyclical objects return NULL
#' time_cycle(Sys.Date())
#' 
#' # The cycle of a cyclical time object
#' time_cycle(month_of_year(Sys.Date()))
#' 
#' @export
time_cycle <- S7::new_generic("time_cycle", "x")

S7::method(time_cycle, S7::class_any) <- function(x) {
  NULL
}

S7::method(time_cycle, S7::new_S3_class("mixtime")) <- function(x) {
  v <- attr(x, "v")
  if (length(v) > 1L) {
    cli::cli_abort("time_cycle() only supports single-typed mixtime vectors, not multi-typed.")
  }
  time_cycle(v[[1L]])
}

S7::method(time_cycle, S7::new_S3_class("mt_time")) <- function(x) {
  attr(x, "cycle")
}

S7::method(time_cycle, S7::new_S3_class("hms")) <- function(x) {
  cal_gregorian$day(1L, tz = "UTC")
}