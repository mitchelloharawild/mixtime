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

time_unit <- function(x, ...){
  UseMethod("time_unit")
}

#' @export
time_unit.mixtime <- function(x, ...){
  calendar_data(x)$granularity
}

#' @rdname set_time_units
#' @export
`time_unit<-` <- function(x, value) {
  UseMethod("time_unit<-")
}

#' Set the time units of a mixtime
#'
#' \lifecycle{experimental}
#'
#' Modify a mixtime's granularity to use a different granularity. This is HIGHLY
#' experimental, and more of a proof of concept for how periods of time can be
#' combined by changing the granularity.
#'
#' @param x A mixtime.
#' @param value The new time unit
#'
#' @export
set_time_units <- `time_unit<-`

#' @export
`time_unit<-.mixtime` <- function(x, value){
  vec_assert(value, list_of_time_units(), size = 1L)
  tu <- time_unit(x)
  if(vec_size(tu) > 1) {
    abort("Changing time units is not yet supported for mixed granularity mixtimes.")
  }
  common_unit <- vec_data(vec_cast(value[[1]], tu[[1]]))
  base_unit <- vec_data(tu[[1]])

  field(x, "x") <- field(x, "x")%/%(common_unit/base_unit)

  attr(x, "cal")$granularity <- value
  x
}


list_of_time_units <- function(x = list()) {
  new_list_of(
    x,
    ptype = new_vctr(integer(), class = "time_unit"),
    class = "mixtime_time_units"
  )
}

interval_pull.mixtime_time_units <- function(x) {
  require_package("tsibble")
  if(vec_size(x) == 1) return(tsibble::interval_pull(x[[1]]))
  intvl <- paste(format(x, abbr = TRUE), collapse = ", ")
  val <- vctrs::new_vctr(1, class = c("interval_hide_number", "numeric"), inherit_base_type = TRUE)
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
format.mixtime_time_units <- function(x, ...){
  vapply(x, format, character(1L), ...)
}

pillar_shaft.mixtime_time_units <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format(x), align = "left")
}
