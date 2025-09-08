# TODO: Conversions between chronons from one time unit to another.
# This is especially efficient for converting irregular time units.
# Defaults to chronon_cardinality() which is efficient for regular time conversions
# (e.g. days in a week).


#' Convert between chronons of different time units
#' 
#' This function converts between chronons measured in different time units. It
#' is used internally for converting between different continuous time types,
#' and is particularly useful for efficiently converting between irregular time
#' units. The default method uses `chronon_cardinality()` to cast between time
#' units, which is efficient for regular time units.
#' 
#' @param from The time unit that `x` is measured in (e.g., `tu_day(1L)`).
#' @param to The time unit to convert `x` into (e.g., `tu_week(1L)`).
#' @param x An integer vector of chronons measured in the `from` time unit.
#' 
#' @return An list of two elements:
#' - `chronon`: integer vector of chronons measured in the `to` time unit.
#' - `remainder`: integer vector of the remainder (in `from` time unit) after
#'  converting to the `to` time unit.
#' 
#' @examples
#' # Convert day 16 since epoch into weeks since epoch (and remainder days)
#' chronon_divmod(tu_day(1L), tu_week(1L), 16L)
#' 
#' # Convert week 4 since epoch into days since epoch
#' chronon_divmod(tu_week(1L), tu_day(1L), 4L)
chronon_divmod <- S7::new_generic("chronon_divmod", c("from", "to"))

#' @export
S7::method(chronon_divmod, list(mt_unit, mt_unit)) <- function(from, to, x) {
  # No casting needed for identical time units
  if (identical(from, to)) {
    return(
      list(
        chronon = vec_data(x),
        remainder = rep(1L, length(x))
      )
    )
  }

  # TODO: Apply graph dispatch to find shortest path between from and to using
  # known conversions between time units (e.g. tu_day -> tu_month)

  ## Fallback to chronon_cardinality for regular time units
  x <- as.integer(x)
  divisor <- chronon_cardinality(to, from)
  list(
    chronon = x %/% divisor,
    remainder = x %% divisor + 1L
  )
}