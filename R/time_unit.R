#' Create a new time unit
#'
#' \lifecycle{experimental}
#'
#' A time unit describes the granularity of a calendar. The time unit class is
#' used to identify how different time units nest another.
#'
#' @param x The number of units (must be an integer of length 1).
#' @param ... Additional attributes of the time unit (for example, timezones).
#' @param class The class of the unit. Recommended class format is "tu_*", for
#'   example, "tu_month".
#'
#' @return A time unit.
#'
#' @export
new_time_unit <- function(x, ..., class){
  vec_assert(x, ptype = integer(), size = 1L)
  list_of_time_units(
    list(
      new_vctr(x, ..., class = c(class, "time_unit"))
    )
  )
}

list_of_time_units <- function(x = list()) {
  new_list_of(
    x,
    ptype = new_vctr(integer(), class = "time_unit"),
    class = "moment_time_units"
  )
}

#' @export
format.time_unit <- function(x, ..., abbr = FALSE){
  if(abbr) {
    paste0(vec_data(x), vec_ptype_abbr(x))
  } else {
    paste(vec_data(x), vec_ptype_full(x))
  }
}

#' @export
format.moment_time_units <- function(x, ...){
  vapply(x, format, character(1L), ...)
}
