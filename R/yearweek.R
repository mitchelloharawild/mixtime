#' yearweek
#'
#' \lifecycle{experimental}
#'
#' @param x value
#'
#' @export
yearweek <- function(x){
  UseMethod("yearweek")
}

#' @export
yearweek.numeric <- function(x){
  tu <- new_time_unit(1L, class = "tu_week")
  cal <- new_calendar(tu, origin = TRUE)
  new_moment(x, cal)
}

#' @export
vec_ptype_full.tu_week <- function(x, ...) {
  "week"
}

#' @export
vec_ptype_abbr.tu_week <- function(x, ...) {
  "W"
}

#' @export
format_time.tu_week <- function(tu, x, origin = TRUE, ...){
  x <- vec_data(tu)*x
  if(origin) {
    x <- as.POSIXlt(new_date(-3 + x*7))
    wk <- format(x, "W%V")
    bump_year_up <- x$mon == 11 & wk == "W01"
    bump_year_down <- x$mon == 0 & wk == "W53"
    x$year <- x$year + bump_year_up - bump_year_down
    format(x, paste("%Y", wk))
  } else {
    sprintf("%i %s%s", x, "week", ifelse(x!=1, "s", ""))
  }
}

#' @export
vec_cast.tu_day.tu_week <- function(x, to, ...){
  new_time_unit(vec_data(x)*7L, class = "tu_day")
}
