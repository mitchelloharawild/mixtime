#' Time units as a string
#' 
#' These S7 generic functions provide the full and abbreviated names for time
#' units.
#' 
#' @param x A time unit object (e.g., `tu_month(1L)`)
#' 
#' @return A string representing the time unit
#' 
#' @examples
#' time_unit_full(tu_year(1L))
#' 
#' @rdname time_unit_labels
#' @export
time_unit_full <- S7::new_generic("time_unit_full", "x")

#' @rdname time_unit_labels
#' @export
time_unit_abbr <- S7::new_generic("time_unit_abbr", "x")
