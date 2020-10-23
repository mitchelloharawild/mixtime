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
  tu <- new_time_unit(1L, class = "tu_month")
  cal <- new_calendar(tu, origin = TRUE)
  new_moment(x, cal)
}

#' @export
vec_ptype_full.tu_month <- function(x, ...) {
  "month"
}

#' @export
vec_ptype_abbr.tu_month <- function(x, ...) {
  "M"
}

#' @export
format_time.tu_month <- function(tu, x, origin = TRUE, ...){
  x <- vec_data(tu)*x
  if(origin) {
    paste(1970 + x%/%12, month.abb[x%%12 + 1])
  } else {
    sprintf("%i %s%s", x, "month", ifelse(x!=1, "s", ""))
  }
}

#' @export
vec_cast.tu_quarter.tu_month <- function(x, to, ...){
  new_time_unit(vec_cast(vec_data(x)/3L, integer()), class = "tu_quarter")
}
