# TODO: rename to granules() or time_granules()
# The user-facing variant should accept a list/expression of lin() and cyc() similar to the format strings.

# Compute numeric time parts from a time object
#
# `chronon_parts()` decomposes a time object into its numeric linear and cyclical
# components by repeatedly applying `chronon_divmod()` along the call path of a
# minimal Steiner tree. The linear parts are the `$div` result, which is re-used
# along the divmod path where `$mod` results form the required cyclical parts.
#
# @param x A `mt_time` object (an uni-typed mixtime vector)
# @param linear A list of time units representing linear (non-repeating)
#   components, e.g. `list(cal_gregorian$year(1L))`.
# @param cyclical A list of cyclical component specifications, where each
#   element is a list of two time units `list(from, to)` (chronon and cycle),
#   e.g. `list(list(cal_gregorian$month(1L), cal_gregorian$year(1L)))` for
#   month-of-year.
#
# @return A list with two elements mirroring the inputs:
#   - `$linear`: a list of integer vectors, one per element of `linear`.
#   - `$cyclical`: a list of integer vectors, one per element of `cyclical`.
chronon_parts <- function(x, linear = list(), cyclical = list()) {
  start_tu <- time_chronon(x)

  # Apply time zone offset to x, with truncation for discrete time models.
  x_tz <- tz_offset(x)
  x <- vec_data(x)
  if(is.integer(x)) x_tz <- trunc(x_tz)
  x <- floor(x + x_tz)

  # Find suitable graph path for repeated chronon_divmod() calls
  # that computes all cyclical and linear parts.
  path <- S7_graph_dispatch_multi(
    signatures = method_signatures(chronon_cardinality),
    start      = start_tu,
    terminals  = linear,
    groups     = cyclical
  )

  # Identify linear/cyclical targets
  # Linear: match on child node class id, keyed by position
  linear_ids <- vapply(linear, S7_class_id, character(1L))

  # Cyclical: match on (parent class id, child class id) pairs, keyed by position
  cyclical_ids <- data.frame(
    from = vapply(cyclical, function(x) S7_class_id(x[[1L]]), character(1L)),
    to = vapply(cyclical, function(x) S7_class_id(x[[2L]]), character(1L))
  )

  # Prepare results to be filled via recursive divmod execution
  linear_results   <- vector("list", length(linear))
  cyclical_results <- vector("list", length(cyclical))

  # Handle initial case where the start_tu is a target linear part
  start_id <- S7_class_id(start_tu)
  if (!is.na(i <- vec_match(start_id, linear_ids))) {
    linear_results[[i]] <- x + chronon_epoch(start_tu)
  }

  # Traverse the divmod path to compute parts
  traverse <- function(node, parent_tu, x) {
    for (child in node$children) {
      child_tu <- child$node

      dm <- chronon_divmod(
        from = parent_tu,
        to   = child_tu,
        x    = x
      )

      child_id  <- S7_class_id(child_tu)
      parent_id <- S7_class_id(parent_tu)

      # Collect linear result: div when child matches a linear target
      if (!is.na(i <- vec_match(child_id, linear_ids))) {
        linear_results[[i]] <<- dm$div + chronon_epoch(child_tu)
      }

      # Collect cyclical result: mod when (parent, child) matches a cyclical spec
      if (!is.na(i <- vec_match(data.frame(from = parent_id, to = child_id), cyclical_ids))) {
        cyclical_results[[i]] <<- dm$mod
      }

      # Recurse with div as the new values
      traverse(child, child_tu, dm$div)
    }
  }
  traverse(path, start_tu, x)

  # Check if all targets were found
  if (any(lin_missed <- vapply(linear_results, is.null, logical(1L)))) {
    cli::cli_abort(
      c(
        "The following linear time parts could not be computed from the input time object:",
        i = "{time_unit_full(linear[lin_missed])} ({linear[lin_missed]})",
        i = "All requested linear parts need to be included as granules for {.code linear_time()}"
      ),
      call = NULL
    )
  }
  if (any(cyc_missed <- vapply(cyclical_results, is.null, logical(1L)))) {
    cli::cli_abort(
      c(
        "The following cyclical time parts could not be computed from the input time object:",
        i = "{time_unit_full(cyclical[cyc_missed])} ({cyclical[cyc_missed][[1L]]$from} -> {cyclical[cyc_missed][[1L]]$to})",
        i = "All requested cyclical parts need to be included as granules for {.code linear_time()}"
      ),
      call = NULL
    )
  }

  # Return list of the same order as input
  list(linear = linear_results, cyclical = cyclical_results)
}