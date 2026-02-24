#' Default linear time granules for chronons
#'
#' Provides a default set of linear time granules for a given chronon (time unit).
#' Granules represent the subdivisions or breakpoints used when representing
#' time values at that chronon level.
#'
#' @param x A chronon (time unit) object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of granule values for the chronon. Returns an empty list
#'   for the base `mt_unit` class.
#'
#' @export
#' @examples
#' chronon_granules(cal_gregorian$year(1L))
#' chronon_granules(cal_gregorian$month(1L))
chronon_granules <- S7::new_generic("chronon_granules", "x")
S7::method(chronon_granules, mt_unit) <- function(x) list()