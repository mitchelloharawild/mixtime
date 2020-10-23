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
  tu <- new_time_unit(1L, class = "tu_year")
  cal <- new_calendar(tu, origin = TRUE)
  new_moment(x, cal)
}

#' @export
vec_ptype2.moment.numeric <- function(x, y, ...){
  vec_ptype2(x, year(double()))
}
#' @export
vec_ptype2.numeric.moment <- function(x, y, ...){
  vec_ptype2(year(double()), y)
}

#' @export
vec_cast.moment.numeric <- function(x, to, ...) {
  vec_cast(year(x-1970), to)
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

#' @export
vec_cast.tu_month.tu_year <- function(x, to, ...){
  new_time_unit(vec_data(x)*12L, class = "tu_month")
}
#' @export
vec_cast.tu_quarter.tu_year <- function(x, to, ...){
  new_time_unit(vec_data(x)*4L, class = "tu_quarter")
}
