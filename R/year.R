#' Year
#'
#' \lifecycle{experimental}
#'
#' @param x value
#'
#' @export
year <- function(x){
  UseMethod("year")
}

#' @export
year.numeric <- function(x){
  new_mixtime(x-1970, new_calendar(tu_year(1L), origin = TRUE))
}

#' @rdname yearmonthday
#' @export
tu_year <- function(x){
  x <- vec_cast(x, integer())
  list_of_time_units(list(new_time_unit(x, class = "tu_year")))
}

#' @export
vec_ptype2.mixtime.numeric <- function(x, y, ...){
  vec_ptype2(x, year(double()))
}
#' @export
vec_ptype2.double.mixtime <- function(x, y, ...){
  vec_ptype2(year(double()), y)
}

#' @export
vec_cast.mixtime.double <- function(x, to, ...) {
  vec_cast(year(x), to)
}

#' @export
vec_ptype_full.tu_year <- function(x, ...) {
  "year"
}

#' @export
vec_ptype_abbr.tu_year <- function(x, ...) {
  "Y"
}

#' @export
format_time.tu_year <- function(tu, x, origin = TRUE, ...){
  x <- vec_data(tu)*x
  if(origin) {
    format(1970 + x)
  } else {
    sprintf("%i %s%s", x, "year", ifelse(x!=1, "s", ""))
  }
}

interval_pull.tu_year <- function(x) {
  tsibble::new_interval(year = vec_data(x))
}

#' @export
vec_cast.tu_month.tu_year <- function(x, to, ...){
  new_time_unit(vec_data(x)*12L, class = "tu_month")
}
#' @export
vec_cast.tu_quarter.tu_year <- function(x, to, ...){
  new_time_unit(vec_data(x)*4L, class = "tu_quarter")
}
