#' Create a new time unit
#'
#' \lifecycle{experimental}
#'
#' A time unit describes the granularity of a calendar. The time unit class is
#' used to identify how different time units nest another.
#'
#' @param x The number of units (must be an integer of length 1).
#' @param class The class of the unit. Recommended class format is "tu_*", for
#'   example, "tu_month".
#'
#' @return A time unit.
#'
#' @export
new_time_unit <- function(x, class){
  vec_assert(x, ptype = integer(), size = 1L)
  vctrs::new_vctr(
    x,
    class = c(class, "time_unit")
  )
}
