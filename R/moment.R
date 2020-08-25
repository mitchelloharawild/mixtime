#' Create a new moment
#'
#' A moment is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The iterable value of the moment (for example, a value of 1 may indicate the first day).
#' @param calendar The calendar structure over which the moment iterates. This can be created using `new_calendar()`.
#'
#' @export
new_moment <- function(x, calendar) {
  vctrs::new_vctr(x, cal = calendar, class = "moment")
}

#' @export
format.moment <- function(x, ...) {
  cal <- calendar_data(x)
  if(cal$origin) {
    format_time(cal$granularity, vec_data(x))
  } else {
    sprintf("%i %s%s", x, vec_ptype_full(cal$granularity), ifelse(vec_data(x)!=1, "s", ""))
  }
}

#' @export
calendar_data.moment <- function(x) {
  attr(x, "cal")
}

#' @export
vec_arith.moment <- function(op, x, y, ...){
  x_cal <- calendar_data(x)
  y_cal <- calendar_data(y)
  if(x_cal$origin && y_cal$origin) {
    if(op != "-") abort(str_glue("Cannot use operation {op} with moments that both have origins."))
    x_cal$origin <- FALSE
    new_moment(do.call(op, list(vec_data(x), vec_data(y))), x_cal)
  } else if(x_cal$origin || y_cal$origin) {
    if(!vec_in(op, c("-", "+"))) abort(str_glue("Cannot use operation {op} with moments that have origins."))
    origin_cal <- if(x_cal$origin) x_cal else y_cal
    new_moment(do.call(op, list(vec_data(x), vec_data(y))), origin_cal)
  } else {
    new_moment(do.call(op, list(vec_data(x), vec_data(y))), x_cal)
  }
}

#' @export
vec_cast.Date.moment <- function(x, to, ...) {
  cal <- calendar_data(x)
  if(any(!cal$origin)) abort("Only moments with origins can be converted to dates.")
  abort("Moments cannot yet be converted to dates (awaiting daily moment support).")
  # 1. Convert to date moment
  # 2. Convert date moment to Date
}
