#' Create a temporal durations from time units
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' Constructs a new duration object (`mt_duration`) from a set of mixtime time 
#' units. This functionality is in active development and is not ready for use.
#'
#' @param ... A set of time unit objects (e.g., `tu_year(1)`, `tu_month(2)`, etc.)
#'
#' @return An object of class `mt_duration` representing the specified duration.
#'
#' @details
#' This is a low-level constructor function that creates duration objects.
#'
#' @name duration
new_duration <- function(...) {
  vctrs::new_vctr(rlang::list2(...), class = "mt_duration")
}