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
#' @param ... Additional arguments for methods.
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
#'
#' @export
chronon_divmod <- S7::new_generic("chronon_divmod", c("from", "to"))

#' @rdname chronon_divmod
chronon_divmod.S7_methods <- function(from, to, x) S7_method_docs()

#' @export
S7::method(chronon_divmod, list(mt_unit, mt_unit)) <- function(from, to, x) {
  # No casting needed for identical time units
  if (identical(S7::S7_class(from), S7::S7_class(to))) {
    divisor <- vec_data(to) / vec_data(from)
    return(
      list(
        chronon = vec_data(x) %/% divisor,
        remainder = vec_data(x) %% divisor
      )
    )
  }

  # TODO: Apply graph dispatch to find shortest path between from and to using
  # divmod conversions between time units (e.g. tu_day -> tu_month)
  path <- S7_graph_dispatch(
    unique(c(
      # Chronon divmod should be directional
      method_signatures(chronon_divmod),
      # Chronon cardinality is a undirected fallback
      method_signatures(chronon_cardinality)
    )),
    from,
    to
  )

  path[[1]] <- from
  path[[length(path)]] <- to
  # Initialise intermediate classes with 1L
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(x) x(1L))

  chronon <- vector("list", length(path))
  chronon[[1]] <- vec_data(x)
  remainder <- vector("list", length(path)-1L)
  # Forward convert chronons
  for (i in seq(2, length.out = length(path)-1)) {
    result <- chronon_divmod_dispatch(path[[i-1L]], path[[i]], chronon[[i-1L]])
    chronon[[i]] <- result$chronon
    remainder[[i-1L]] <- result$remainder
  }

  # Backward convert remainder
  for (i in seq(length(remainder), by = -1L, length.out = length(remainder) - 1L)) {
    remainder[[i-1L]] <- remainder[[i-1L]] + chronon_cardinality(path[[i]], path[[i-1L]], chronon[[i]])*remainder[[i]]
  }

  list(
    chronon = chronon[[length(chronon)]],
    remainder = remainder[[1L]]
  )

}

chronon_divmod_dispatch <- function(from, to, x) {
  if (is.null(chronon_divmod@methods[[S7_class_id(from)]][[S7_class_id(to)]])) {
    return(chronon_divmod_regular(from, to, x))
  } else {
    return(chronon_divmod(from, to, x))
  }
}

## Fallback to chronon_cardinality for regular time units
chronon_divmod_regular <- function(from, to, x) {
  divisor <- chronon_cardinality(to, from)
  list(
    chronon = x %/% divisor,
    remainder = x %% divisor
  )
}