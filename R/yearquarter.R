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
  tu <- new_time_unit(1L, class = "tu_quarter")
  cal <- new_calendar(tu, origin = TRUE)
  new_moment(x, cal)
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

#' @export
vec_cast.tu_month.tu_quarter <- function(x, to, ...){
  new_time_unit(vec_data(x)*3L, class = "tu_month")
}
