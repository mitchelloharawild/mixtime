# TODO: Conversions between chronons from one time unit to another.
# This is especially efficient for converting irregular time units.
# Defaults to calendar_algebra() which is efficient for regular time conversions
# (e.g. days in a week).


#' Convert between chronons of different time units
#' 
#' This function converts between chronons measured in different time units. It
#' is used internally for converting between different continuous time types,
#' and is particularly useful for efficiently converting between irregular time
#' units. The default method uses `calendar_algebra()` to cast between time
#' units, which is efficient for regular time units.
#' 
#' @param from The time unit that `x` is measured in (e.g., `tu_day(1L)`).
#' @param to The time unit to convert `x` into (e.g., `tu_week(1L)`).
#' @param x An integer vector of chronons measured in the `from` time unit.
#' 
#' @return An integer vector of chronons measured in the `to` time unit.
#' 
#' @examples
#' # Convert day 14 since epoch into weeks since epoch
#' chronon_cast(tu_day(1L), tu_week(1L), 14L)
chronon_cast <- S7::new_generic("chronon_cast", c("from", "to"))

#' @export
S7::method(chronon_cast, list(mt_unit, mt_unit)) <- function(from, to, x) {
  # TODO: Apply graph dispatch to find shortest path between from and to
  # (with fallback to calendar_algebra conversions)
  as.integer(x) %/% calendar_algebra(to, from)
}
