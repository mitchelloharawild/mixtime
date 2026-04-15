#' Friendly labels for linear relationships
#'
#' This S7 generic function provides the labels for linear (non-repeating)
#' positions of a time unit. These functions should return locale specific
#' labels.
#'
#' @param granule A time unit object representing the granule (e.g., `year(1L)`)
# #' @param i Integer vector representing the position along the linear axis.
# #' @param label If `TRUE`, return labels for the positions along the linear axis. If `FALSE`, return the integer positions as character.
# #' @param abbreviate If `TRUE`, return abbreviated labels (e.g. "year" as "Y"). If `FALSE`, return full labels.
#' @param ... Additional arguments for methods.
#'
#' @return Character vector of labels for the time point.
#'
#' @examples
#' # Labels for years on a linear axis
#' with(cal_gregorian, linear_labels(year(1L), 2020:2025))
#'
#' @export
linear_labels <- S7::new_generic("linear_labels", "granule")

# #' @rdname linear_labels
method(linear_labels, mt_unit) <- function(granule, i, label = FALSE, abbreviate = TRUE, ...) {
  if (!label) return(as.character(i))
  # TODO - pluralise the full time unit
  paste0(i, if (abbreviate) time_unit_abbr(granule) else time_unit_full(granule))
}
