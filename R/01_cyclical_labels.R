#' Friendly labels for cyclical relationships
#'
#' This S7 generic function provides the labels for cyclical relationships 
#' between time units. These functions should return locale specific labels.
#'
#' @param granule A time unit object representing the granule (e.g., `tu_month(1L)`)
#' @param cycle A time unit object representing the cycle (e.g., `tu_year(1L)`)
#' @param i Integer vector representing the position within the cycle.
#' @param ... Additional arguments for methods.
#'
#' @return Character vector of labels for the time point within the cycle.
#' 
#' @examples
#' # Labels for months in a year
#' cyclical_labels(tu_month(1L), tu_year(1L), 1:12)
#' 
#' # Labels for days in a week
#' cyclical_labels(tu_day(1L), tu_week(1L), 1:7)
#' 
#' # Labels for weeks in a year, defaulted from time_unit_abbr()
#' cyclical_labels(tu_week(1L), tu_year(1L), 1:52)
#' 
#' @export
cyclical_labels <- S7::new_generic("cyclical_labels", c("granule", "cycle"))

#' @rdname cyclical_labels
cyclical_labels.S7_methods <- function(granule, cycle, i) S7_method_docs()


method(cyclical_labels, list(mt_unit, mt_unit)) <- function(granule, cycle, i) {
  paste0(time_unit_abbr(granule), i + 1L)
}