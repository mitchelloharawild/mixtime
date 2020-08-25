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

format_time.tu_quarter <- function(tu, x, ...){
  x <- vec_data(tu)*x
  paste0(1970 + x%/%4, " Q", x%%4 + 1)
}
