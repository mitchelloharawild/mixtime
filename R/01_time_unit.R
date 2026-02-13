#' Time units as a string
#' 
#' These S7 generic functions provide the full and abbreviated names for time
#' units. `time_unit_full()` is used in messages and durations (e.g., "2 months").
#' `time_unit_abbr()` is used for tsibble index interval displays (e.g., "1M").
#' 
#' @param x A time unit object (e.g., `tu_month(1L)`)
#' @param ... Additional arguments for methods.
#' 
#' @return A string representing the time unit
#' 
#' @examples
#' time_unit_full(tu_year(1L))
#' time_unit_abbr(tu_year(1L))
#' 
#' @rdname time_unit_labels
#' @export
time_unit_full <- S7::new_generic("time_unit_full", "x")

#' @rdname time_unit_labels
#' @export
time_unit_abbr <- S7::new_generic("time_unit_abbr", "x")
