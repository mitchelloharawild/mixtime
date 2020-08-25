#' Yearmonth
#'
#' \lifecycle{experimental}
#'
#' @param x value
#'
#' @export
yearmonth <- function(x){
  UseMethod("yearmonth")
}

#' @export
yearmonth.numeric <- function(x){
  tu <- new_time_unit(1, "tu_month")
  cal <- new_calendar(tu, origin = TRUE)
  new_moment(x, cal)
}

#' @export
vec_ptype_full.tu_month <- function(x, ...) {
  "month"
}

format_time.tu_month <- function(tu, x, ...){
  x <- vec_data(tu)*x
  paste(1970 + x%/%12, month.abb[x%%12 + 1])
}
