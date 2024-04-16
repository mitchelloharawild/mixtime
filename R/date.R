#' Date
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
  new_mixtime(x, new_calendar(tu_day(1L), origin = TRUE))
}

#' @rdname yearmonthday
#' @export
tu_day <- function(x){
  x <- vec_cast(x, integer())
  list_of_time_units(list(new_time_unit(x, class = "tu_day")))
}

#' @export
vec_ptype2.mixtime.Date <- function(x, y, ...){
  vec_ptype2(x, yearmonthday(double()))
}
#' @export
vec_ptype2.Date.mixtime <- function(x, y, ...){
  vec_ptype2(yearmonthday(double()), y)
}

#' @export
vec_cast.mixtime.Date <- function(x, to, ...) {
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

interval_pull.tu_day <- function(x) {
  tsibble::new_interval(day = vec_data(x))
}
