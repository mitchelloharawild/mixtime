#' Obtain the cycle of a time object
#' 
#' This S7 generic function extracts the cycle (the cyclical time granule) from
#' a time object, such as cyclical time representations.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A time [duration()] object representing the cycle of each value
#'   (e.g. `weeks(1L)`), or `NA` if the object has no cyclical component.
#' 
#' @examples
#' 
#' # Non-cyclical objects return NA
#' time_cycle(Sys.Date())
#' 
#' # The cycle of a cyclical time object
#' time_cycle(month_of_year(Sys.Date()))
#' 
#' @export
time_cycle <- S7::new_generic("time_cycle", "x")

S7::method(time_cycle, S7::class_any) <- function(x) {
  duration(rep(NA_integer_, length(x)), mt_unit(1L))
}

S7::method(time_cycle, class_mixtime) <- function(x) {
  x@x <- lapply(x@x, function(x) time_cycle(x)@x[[1L]])
  x
}

S7::method(time_cycle, S7::new_S3_class("mt_time")) <- function(x) {
  if (is.null(attr(x, "cycle"))) {
    return(duration(NA_real_, mt_unit(1L)))
  }
  
  duration(rep(1L, length(x)), attr(x, "cycle"))
}

# {hms} time class
S7::method(time_cycle, S7::new_S3_class("hms")) <- function(x) {
  duration(rep(1L, length(x)), chronon = cal_gregorian$day(1L))
}
