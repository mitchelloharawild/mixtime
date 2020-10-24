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
  new_moment(x, new_calendar(tu_month(1L), origin = TRUE))
}

#' @rdname yearmonth
#' @export
tu_month <- function(x){
  x <- vec_cast(x, integer())
  list_of_time_units(list(new_time_unit(x, class = "tu_month")))
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
  mult <- vec_data(tu)
  x <- mult*x
  if(origin) {
    if(mult > 1) {
      sprintf("%i %s-%s", 1970 + x%/%12, month.abb[x%%12 + 1], month.abb[(x+mult-1)%%12 + 1])
    } else {
      paste(1970 + x%/%12, month.abb[x%%12 + 1])
    }
  } else {
    sprintf("%i %s%s", x, "month", ifelse(x!=1, "s", ""))
  }
}

#' @export
vec_cast.tu_quarter.tu_month <- function(x, to, ...){
  new_time_unit(vec_cast(vec_data(x)/3L, integer()), class = "tu_quarter")
}

#' @export
vec_cast.moment.yearmonth <- function(x, to, ...){
  vec_cast(yearmonth(as.double(x)), to)
}
