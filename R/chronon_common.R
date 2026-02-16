#' Find a common chronon from a set of chronons
#' 
#' This utility function takes a set of chronons and identifies a common chronon
#' of the finest granularity that can represent all input chronons without loss 
#' of information. This is useful for operations that require a shared time 
#' unit, such as combining or comparing different time measured at different
#' precisions.
#' 
#' @param ... A set of chronons to find a common chronon for.
#' @param .ptype If NULL, the default, the output returns the common chronon 
#' across all elements of `...`. Alternatively, a prototype chronon can be
#' supplied to `.ptype` to demand a specific chronon is used. If the supplied
#' `.ptype` cannot represent all input chronons without loss of information,
#' an error is raised.
#' 
#' @return A time unit object representing the common chronon.
#' 
#' @examples
#' 
#' # The common chronon between days and weeks is a day
#' with(cal_isoweek, chronon_common(day(1L), week(1L)))
#' 
#' # The common chronon between days and months is a day
#' chronon_common(cal_isoweek$week(1L), cal_gregorian$month(1L))
#' 
#' # The common chronon between hours, months, and years is an hour
#' with(cal_gregorian, chronon_common(hour(1L), month(1L), year(1L)))
#' 
#' # The common chronon between months, quarters, and years is a month
#' with(cal_gregorian, chronon_common(month(1L), quarter(1L), year(1L)))
#' 
#' @export
chronon_common <- function(..., .ptype = NULL) {
  # TODO: Validate that the supplied .ptype can represent all input chronons
  if (!is.null(.ptype)) return(.ptype)
  
  # Identify the finest granularity chronon that can represent all chronons
  chronons <- rlang::list2(...)

  if (vec_size(chronons) == 1L) return(chronons[[1L]])

  # Search strategy:
  # The cardinality methods are directional
  # (such that `x` is a finer chronon than `y`)
  # Construct a graph of the methods to find the common root chronon.
  S7_graph_glb(method_signatures(chronon_cardinality), chronons)(1L)
}