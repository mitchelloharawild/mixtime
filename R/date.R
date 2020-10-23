#' Moment date
#'
#' \lifecycle{experimental}
#'
#' @param x value
#'
#' @export
yearmonthday <- function(x){
  UseMethod("yearmonthday")
}

#' @export
yearmonthday.numeric <- function(x){
  tu <- new_time_unit(1L, class = "tu_day")
  cal <- new_calendar(tu, origin = TRUE)
  new_moment(x, cal)
}

#' @export
vec_ptype2.moment.Date <- function(x, y, ...){
  vec_ptype2(x, yearmonthday(double()))
}
#' @export
vec_ptype2.Date.moment <- function(x, y, ...){
  vec_ptype2(yearmonthday(double()), y)
}

#' @export
vec_cast.moment.Date <- function(x, to, ...) {
  vec_cast(yearmonthday(as.double(x)), to)
}

#' @export
vec_ptype_full.tu_day <- function(x, ...) {
  "day"
}

#' @export
vec_ptype_abbr.tu_day <- function(x, ...) {
  "D"
}

#' @export
format_time.tu_day <- function(tu, x, origin = TRUE, ...){
  x <- vec_data(tu)*x
  if(origin) {
    format(.Date(x))
  } else {
    sprintf("%i %s%s", x, "day", ifelse(x!=1, "s", ""))
  }
}
