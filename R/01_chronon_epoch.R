
#' Epoch offset for chronons
#'
#' Returns the epoch offset for a given chronon (time unit). The epoch defines
#' the starting point of the chronon's linear numbering, used when converting
#' from internal representations to common displays (e.g. applying an epoch of
#' 1970-01-01).
#'
#' @param x A chronon (time unit) object.
#' @param ... Additional arguments for methods.
#'
#' @return A numeric value representing the epoch for the chronon.
#'
#' @export
#' @examples
#' # The epoch for year linear time displays is 1970
#' chronon_epoch(cal_gregorian$year(1L))
chronon_epoch <- S7::new_generic("chronon_epoch", c("x"))

# No epoch specified, default 0
method(chronon_epoch, mt_unit) <- function(x) 0L
