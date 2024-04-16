#' Yearquarter
#'
#' \lifecycle{experimental}
#'
#' @param x value
#'
#' @export
yearquarter <- function(x){
  UseMethod("yearquarter")
}

#' @export
yearquarter.numeric <- function(x){
  new_mixtime(x, new_calendar(tu_quarter(1L), origin = TRUE))
}

#' @rdname yearquarter
#' @export
tu_quarter <- function(x){
  x <- vec_cast(x, integer())
  list_of_time_units(list(new_time_unit(x, class = "tu_quarter")))
}

#' @export
vec_ptype_full.tu_quarter <- function(x, ...) {
  "quarter"
}

#' @export
vec_ptype_abbr.tu_quarter <- function(x, ...) {
  "Q"
}

#' @export
format_time.tu_quarter <- function(tu, x, origin = TRUE, ...){
  mult <- vec_data(tu)
  x <- mult*x
  if(origin) {
    if(mult > 1) {
      sprintf("%i Q%i-Q%i", 1970 + x%/%4, x%%4 + 1, (x+mult-1)%%4 + 1)
    } else {
      paste0(1970 + x%/%4, " Q", x%%4 + 1)
    }
  } else {
    sprintf("%i %s%s", x, "quarter", ifelse(x!=1, "s", ""))
  }
}

interval_pull.tu_quarter <- function(x) {
  tsibble::new_interval(quarter = vec_data(x))
}

#' @export
vec_cast.tu_month.tu_quarter <- function(x, to, ...){
  new_time_unit(vec_data(x)*3L, class = "tu_month")
}

#' @export
vec_ptype2.mixtime.yearquarter <- function(x, y, ...){
  vec_ptype2(x, yearquarter(double()))
}
#' @export
vec_ptype2.yearquarter.mixtime <- function(x, y, ...){
  vec_ptype2(yearquarter(double()), y)
}
#' @export
vec_cast.mixtime.yearquarter <- function(x, to, ...){
  vec_cast(yearquarter(as.double(x)), to)
}
