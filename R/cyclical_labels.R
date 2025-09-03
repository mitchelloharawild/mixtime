#' Friendly labels for cyclical relationships
#'
#' This S7 generic function provides the labels for cyclical relationships 
#' between time units. These functions should return locale specific labels.
#'
#' @param granule A time unit object representing the granule (e.g., `tu_month(1L)`)
#' @param cycle A time unit object representing the cycle (e.g., `tu_year(1L)`)
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
#' @export
cyclical_labels <- S7::new_generic("cyclical_labels", c("granule", "cycle"))
