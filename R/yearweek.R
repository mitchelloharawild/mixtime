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
  new_mixtime(x, new_calendar(tu_week(1L), origin = TRUE))
}

#' @rdname yearweek
#' @export
tu_week <- function(x){
  x <- vec_cast(x, integer())
  list_of_time_units(list(new_time_unit(x, class = "tu_week")))
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
  mult <- vec_data(tu)
  x <- mult*x
  if(origin) {
    x_from <- as.POSIXlt(new_date(-3 + x*7))
    wk_from <- format(x_from, "W%V")
    bump_year_up <- x_from$mon == 11 & wk_from == "W01"
    bump_year_down <- x_from$mon == 0 & wk_from == "W53"
    x_from$year <- x_from$year + bump_year_up - bump_year_down
    if(mult > 1) {
      x_to <- as.POSIXlt(new_date(-3 + (x+mult-1)*7))
      wk_to <- format(x_to, "W%V")
      bump_year_up <- x_to$mon == 11 & wk_to == "W01"
      bump_year_down <- x_to$mon == 0 & wk_to == "W53"
      x_to$year <- x_to$year + bump_year_up - bump_year_down
      out <- vec_init_along(character(), x)
      same_year <- x_from$year == x_to$year
      out[same_year] <- format(x_from[same_year], paste0("%Y ", wk_from[same_year], "-", wk_to[same_year]))
      out[!same_year] <- paste(
        format(x_from[!same_year], paste("%Y", wk_from[!same_year])),
        format(x_to[!same_year], paste("%Y", wk_to[!same_year])),
        sep="-")
      out
    } else {
      format(x_from, paste("%Y", wk_from))
    }
  } else {
    sprintf("%i %s%s", x, "week", ifelse(x!=1, "s", ""))
  }
}

interval_pull.tu_week <- function(x) {
  tsibble::new_interval(week = vec_data(x))
}

#' @export
vec_cast.tu_day.tu_week <- function(x, to, ...){
  new_time_unit(vec_data(x)*7L, class = "tu_day")
}

#' @export
vec_ptype2.mixtime.yearweek <- function(x, y, ...){
  vec_ptype2(x, yearweek(double()))
}
#' @export
vec_ptype2.yearweek.mixtime <- function(x, y, ...){
  vec_ptype2(yearweek(double()), y)
}
#' @export
vec_cast.mixtime.yearweek <- function(x, to, ...){
  vec_cast(yearweek(as.double(x)), to)
}
