#' Find the common chronon of a time object
#'
#' @description
#' 
#' This utility function takes a set of chronons and identifies a common chronon
#' of the finest granularity that can represent all input chronons without loss 
#' of information. This is useful for operations that require a shared time 
#' granule, such as combining or comparing different time measured at different
#' precisions.
#' 
#' The result is obtained by finding the greatest lower bound (GLB) of the input
#' chronons using the ordered relationships defined by `chronon_cardinality()`
#' methods. The GLB represents the finest chronon that can represent all input
#' chronons without loss of information.
#' 
#' @param x A time object (typically a [`mixtime`]).
#' @param .ptype If NULL, the default, the output returns the common chronon 
#' across all chronons of `x`. Alternatively, a prototype chronon can be
#' supplied to `.ptype` to demand a specific chronon is used. If the supplied
#' `.ptype` cannot represent all input chronons without loss of information,
#' an error is raised.
#' 
#' @param ... Additional arguments for methods.
#'
#' @return A time granule object representing the common chronon.
#'
#' @examples
#' # The common chronon between a year-month and a day is a day
#' chronon_common(c(yearmonth(Sys.Date()), date(Sys.Date())))
#' 
#' # The common chronon between a Gregorian month and an ISO week is a day
#' chronon_common(c(yearmonth(Sys.Date()), yearweek(Sys.Date())))
#' 
#' # The common chronon between a ISO week and an hour is an hour
#' chronon_common(c(yearweek(Sys.Date()), linear_time(Sys.time(), hour(1L))))
#' 
#' @name chronon_common
#' @export
chronon_common <- S7::new_generic("chronon_common", "x")


#' @rdname chronon_common
chronon_common.mixtime <- function(x, .ptype = NULL, ...) {
  cli::cli_abort(
    c(
      "This method is for documentation purposes only. The actual method is S7::method(chronon_common, class_mixtime).",
      i = "This documentation workaround is pending https://github.com/r-lib/roxygen2/issues/1872"
    ),
    call = NULL
  )
}

# TODO - mixtime can't yet assume all values are mt_time
# https://github.com/mitchelloharawild/mixtime/issues/82
# S7::method(chronon_common, class_mixtime) <- function(x, .ptype = NULL, ...) {
#   chronon_common_impl(
#     lapply(x@x, function(x) attr(x, "chronon")),
#     .ptype = .ptype
#   )
# }

S7::method(chronon_common, class_any) <- function(x, .ptype = NULL, ...) {
  chronon_common_impl(
    lapply(time_chronon(x)@x, attr, "chronon"),
    .ptype = .ptype
  )
}

chronon_common_impl <- function(chronons, .ptype = NULL) {
  # TODO: Validate that the supplied .ptype can represent all input chronons
  if (!is.null(.ptype)) return(.ptype)
  
  if (vec_size(chronons) == 1L) return(chronons[[1L]])

  # Search strategy:
  # The cardinality methods are directional
  # (such that `x` is a finer chronon than `y`)
  # Construct a graph of the methods to find the common root chronon.
  S7_graph_glb(method_signatures(chronon_cardinality), chronons)(1L)
}
