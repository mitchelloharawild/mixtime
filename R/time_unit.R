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
new_time_unit <- function(x = 1L, ..., class = NULL){
  vec_assert(x, ptype = integer(), size = 1L)
  new_vctr(x, ..., class = c(class, "time_unit"))
}

}

list_of_time_units <- function(x = list()) {
  new_list_of(
    x,
    ptype = new_vctr(integer(), class = "time_unit"),
    class = "moment_time_units"
  )
}

interval_pull.moment_time_units <- function(x) {
  # if(vec_size(x) == 1) return(interval_pull(x[[1]]))
  intvl <- paste(format(x, abbr = TRUE), collapse = ", ")
  val <- vctrs::new_vctr(1, class = "interval_hide_number")
  tsibble::new_interval(.others = stats::setNames(list(val), intvl))
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
