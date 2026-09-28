#' Find the finest or coarsest common chronon of a time object
#'
#' @description
#'
#' These utility functions take a set of chronons and identify a common
#' chronon that can represent all input chronons without loss of
#' information, using the ordered relationships defined by
#' `chronon_cardinality()` methods. This is useful for operations that
#' require a shared time granule, such as combining or comparing different
#' time measured at different precisions.
#'
#' `chronon_glb()` finds the greatest lower bound (GLB): the common chronon
#' of the *finest* granularity that can represent all input chronons without
#' loss of information.
#'
#' `chronon_lub()` finds the least upper bound (LUB): the common chronon of
#' the *coarsest* granularity that every input chronon evenly aggregates
#' into - the dual of `chronon_glb()`.
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
#' # The finest common chronon between a year-month and a day is a day
#' chronon_glb(c(yearmonth(Sys.Date()), date(Sys.Date())))
#'
#' # The finest common chronon between a Gregorian month and an ISO week is a day
#' chronon_glb(c(yearweek(Sys.Date()), yearmonth(Sys.Date())))
#'
#' # The finest common chronon between a ISO week and an hour is an hour
#' chronon_glb(c(yearweek(Sys.Date()), linear_time(Sys.time(), hour(1L))))
#'
#' # The coarsest common chronon between a second and a minute is a minute
#' chronon_lub(c(linear_time(Sys.time(), second(1L)), linear_time(Sys.time(), minute(1L))))
#'
#' @name chronon_glb
#' @export
chronon_glb <- S7::new_generic("chronon_glb", "x")


#' @rdname chronon_glb
chronon_glb.mixtime <- function(x, .ptype = NULL, ...) {
  cli::cli_abort(
    c(
      "This method is for documentation purposes only. The actual method is S7::method(chronon_glb, class_mixtime).",
      i = "This documentation workaround is pending https://github.com/r-lib/roxygen2/issues/1872"
    ),
    call = NULL
  )
}

S7::method(chronon_glb, class_mixtime) <- function(x, .ptype = NULL, ...) {
  chronon_glb_impl(
    lapply(x@x, attr, "chronon"),
    .ptype = .ptype
  )
}

S7::method(chronon_glb, class_any) <- function(x, .ptype = NULL, ...) {
  chronon_glb_impl(
    list(attr(time_chronon(x[1L])@x[[1L]], "chronon")),
    .ptype = .ptype
  )
}

#' @rdname chronon_glb
#' @export
chronon_lub <- S7::new_generic("chronon_lub", "x")

#' @rdname chronon_glb
chronon_lub.mixtime <- function(x, .ptype = NULL, ...) {
  cli::cli_abort(
    c(
      "This method is for documentation purposes only. The actual method is S7::method(chronon_lub, class_mixtime).",
      i = "This documentation workaround is pending https://github.com/r-lib/roxygen2/issues/1872"
    ),
    call = NULL
  )
}

S7::method(chronon_lub, class_mixtime) <- function(x, .ptype = NULL, ...) {
  chronon_lub_impl(
    lapply(x@x, attr, "chronon"),
    .ptype = .ptype
  )
}

S7::method(chronon_lub, class_any) <- function(x, .ptype = NULL, ...) {
  chronon_lub_impl(
    list(attr(time_chronon(x[1L])@x[[1L]], "chronon")),
    .ptype = .ptype
  )
}

# Shared implementation for `chronon_glb()`/`chronon_lub()`: both reduce a
# list of chronons to a single bound via a graph search over
# `chronon_cardinality()` methods, differing only in which graph function
# (`S7_graph_glb()`/`S7_graph_lub()`) is used to find that bound.
chronon_bound_impl <- function(chronons, .ptype = NULL, graph_fn) {
  # TODO: Validate that the supplied .ptype can represent all input chronons
  if (!is.null(.ptype)) {
    return(.ptype)
  }

  chronons <- unique(chronons)
  if (vec_size(chronons) == 1L) {
    return(chronons[[1L]])
  }

  # Search strategy:
  # The cardinality methods are directional
  # (such that `x` is a finer chronon than `y`)
  # Construct a graph of the methods to find the common bound chronon.
  bound <- graph_fn(chronon_cardinality_graph(), chronons)(1L)

  # Restore properties of the common chronon from the input chronons.
  # If the properties (e.g. timezone) vary across the input chronons, they are
  # dropped from the common chronon (resulting in a timezone-naive chronon).
  common <- granule_inherit_shared_props(bound, chronons)

  # If the common chronon has a naive time zone, then prevent it from inheriting
  # timezone attributes in downstream operations.
  granule_harden_naive(common)
}

chronon_glb_impl <- function(chronons, .ptype = NULL) {
  chronon_bound_impl(chronons, .ptype = .ptype, graph_fn = S7_graph_glb)
}

chronon_lub_impl <- function(chronons, .ptype = NULL) {
  chronon_bound_impl(chronons, .ptype = .ptype, graph_fn = S7_graph_lub)
}

#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `chronon_common()` was renamed to [chronon_glb()] to distinguish it from
#' its dual, [chronon_lub()].
#'
#' @rdname chronon_glb
#' @export
chronon_common <- function(x, .ptype = NULL, ...) {
  lifecycle::deprecate_soft("0.3.0.9000", "chronon_common()", "chronon_glb()")
  chronon_glb(x, .ptype = .ptype, ...)
}
