# Compute numeric time parts from a time object
#
# `chronon_parts()` decomposes a time object into its numeric linear and cyclical
# components by repeatedly applying `chronon_divmod()` along the call path of a
# minimal Steiner tree. The linear parts are the `$div` result, which is re-used
# along the divmod path where `$mod` results form the required cyclical parts.
#
# @param x A `mt_time` object (an uni-typed mixtime vector)
# @param linear A list of time granules representing linear (non-repeating)
#   components, e.g. `list(cal_gregorian$year(1L))`.
# @param cyclical A list of cyclical component specifications, where each
#   element is a list of two time granules `list(from, to)` (chronon and cycle),
#   e.g. `list(list(cal_gregorian$month(1L), cal_gregorian$year(1L)))` for
#   month-of-year.
#
# @return A list with three elements:
#   - `$linear`: a list of integer vectors, one per element of `linear`.
#   - `$cyclical`: a list of integer vectors, one per element of `cyclical`.
#   - `$cyclical_at`: a list of integer vectors, one per element of `cyclical`,
#     giving the linear position of the cycle granule.
chronon_parts <- function(x, linear = list(), cyclical = list()) {
  start_tu <- attr(x, "chronon")

  # Shift the time points into local time, truncating the offset for discrete
  # time models so they remain whole chronons. A continuous time model keeps
  # its fractional position instead of flooring it away here.
  x <- vec_data(x)
  discrete <- is.integer(x)
  offset <- tz_offset_impl(x, start_tu)
  x <- x + if (discrete) trunc(offset) else offset
  if (discrete) x <- floor(x)

  # The two granules of a cyclical part: the chronon is the finer unit (the
  # desired output unit) and the cycle the coarser unit it repeats within.
  cyclical_chronon <- lapply(cyclical, `[[`, 1L)
  cyclical_cycle   <- lapply(cyclical, `[[`, 2L)

  # Parts are recognised by matching class ids against the node reached at each
  # step of the traversal, so each set of targets is keyed by position in a
  # vector of the class id it is matched on (NA where the match cannot apply).
  linear_ids  <- vapply(linear, S7_class_id, character(1L))
  chronon_ids <- vapply(cyclical_chronon, S7_class_id, character(1L))
  cycle_ids   <- vapply(cyclical_cycle, S7_class_id, character(1L))

  # Multi-unit cycles (e.g. a month within 15 months) repeat within a coarser
  # granule of the *same* time unit, so their chronon and cycle resolve to one
  # node: a self-loop in the class-keyed cardinality graph rather than a path
  # through it. They ask the graph only to reach that node (a terminal, with no
  # co-occurrence to enforce) and are matched on their chronon, since matching
  # them on their cycle would pair them with the degenerate divmod of a granule
  # with itself.
  self_cycle <- chronon_ids == cycle_ids
  self_cycle_ids <- chronon_ids
  self_cycle_ids[!self_cycle] <- NA_character_
  cycle_ids[self_cycle] <- NA_character_

  # Prepare results to be filled via recursive divmod execution
  linear_results   <- vector("list", length(linear))
  cyclical_results <- vector("list", length(cyclical))
  cyclical_at      <- vector("list", length(cyclical))

  # Resolve parts with finer granules than the chronon for continuous time data
  cycle_is_root <- !self_cycle &
    cycle_ids == S7_class_id(start_tu) &
    vapply(cyclical_cycle, function(cy) cy@n == start_tu@n, logical(1L))
  if (any(cycle_is_root)) {
    whole <- if (discrete) x else floor(x)
    for (i in which(cycle_is_root)) {
      chronon_i <- cyclical_chronon[[i]]
      cyclical_results[[i]] <- if (discrete) {
        rep(NA_integer_, length(x))
      } else {
        chronon_divmod(start_tu, chronon_i, x)$div -
          chronon_divmod(start_tu, chronon_i, whole)$div
      }
      cyclical_at[[i]] <- whole
    }
    cycle_ids[cycle_is_root] <- NA_character_
  }

  # Find a suitable tree of chronon_divmod() steps that computes all cyclical
  # and linear parts. Cross-unit cycles must reach both of their granules on one
  # root-to-leaf path, while the other targets need only be reached.
  tree <- S7_graph_dispatch_multi(
    graph      = chronon_cardinality_graph(),
    start      = start_tu,
    terminals  = c(linear, cyclical_chronon[self_cycle]),
    groups     = cyclical[!self_cycle & !cycle_is_root]
  )

  # Traverse the divmod path to compute parts. `parent_id` is the caller's
  # `child_id`, so it is handed down rather than looked up again.
  traverse <- function(node, parent_tu, parent_id, x) {
    child_tu <- node$node
    child_id <- S7_class_id(child_tu)

    dm <- chronon_divmod(
      from = parent_tu,
      to   = child_tu,
      x    = x
    )

    # Collect linear result: div when child matches a linear target
    for (i in which(child_id == linear_ids)) {
      linear_results[[i]] <<- count_in_granule(dm$div, child_tu, linear[[i]]) +
        chronon_epoch(child_tu)
    }

    # Complete multi-unit cycles rooted at this granule. `dm$div` is the time
    # point in `child_tu` units (at the root, an identity divmod of `x`), so the
    # position within the cycle is a single divmod away.
    for (i in which(child_id == self_cycle_ids)) {
      dm_self <- chronon_divmod(
        from = cyclical_chronon[[i]],
        to   = cyclical_cycle[[i]],
        x    = count_in_granule(dm$div, child_tu, cyclical_chronon[[i]])
      )
      cyclical_results[[i]] <<- dm_self$mod
      cyclical_at[[i]]      <<- dm_self$div
    }

    # Recurse each child with $div as the new time point (now in child_tu units)
    incomplete <- unlist(lapply(node$children, traverse, child_tu, child_id, dm$div))

    # Unwind recursion with backward conversion for cyclical parts: an
    # incomplete result counts `child_tu` units so far, so re-expressing it in
    # the finer `parent_tu` multiplies by the cardinality at the current `dm$div`
    # position and adds this step's remainder. The cardinality is a property of
    # this step alone, so it is found once for all of them.
    if (length(incomplete) > 0L) {
      cardinality <- chronon_cardinality(parent_tu, child_tu, dm$div)
      for (i in incomplete) {
        cyclical_results[[i]] <<- cyclical_results[[i]] * cardinality + dm$mod
      }
    }

    # Cycles whose coarser granule is this node start counting here
    starting <- which(child_id == cycle_ids)
    if (length(starting) > 0L) {
      cyclical_results[starting] <<- list(dm$mod)
      cyclical_at[starting]      <<- list(dm$div)
      incomplete <- c(incomplete, starting)
    }

    # Cyclical parts accumulate in `parent_tu`, the finest granule of their
    # chronon's unit, so those completing here are rescaled to the size asked for.
    complete <- chronon_ids[incomplete] == parent_id
    for (i in incomplete[complete]) {
      cyclical_results[[i]] <<- count_in_granule(
        cyclical_results[[i]], parent_tu, cyclical_chronon[[i]]
      )
    }

    # Return vector of which cyclical targets are still incomplete after this step
    incomplete[!complete]
  }
  traverse(tree, start_tu, S7_class_id(start_tu), x)

  # Check if all targets were found
  abort_missing_parts(linear_results, linear, time_granule_label, "linear")
  abort_missing_parts(cyclical_results, cyclical, cycle_label, "cyclical")

  # Return list of the same order as input
  list(linear = linear_results, cyclical = cyclical_results, cyclical_at = cyclical_at)
}

# Re-express `value`, a count of `from` granules, as a count of `to` granules of
# the same time unit (e.g. months at the month node into quarters for
# `month(3L)`). Nodes hold the finest granule of their time unit, so `to` is
# never finer than `from` and the conversion is a division away.
count_in_granule <- function(value, from, to) {
  if (from@n == to@n) value else chronon_divmod(from, to, value)$div
}

# Abort when the traversal did not reach every requested part, describing those
# it missed with `label()` applied to the corresponding element of `targets`.
abort_missing_parts <- function(results, targets, label, type) {
  found <- !vapply(results, is.null, logical(1L))
  if (all(found)) return(invisible(NULL))

  missed <- vapply(targets[!found], label, character(1L))
  cli::cli_abort(
    c(
      "The following {type} time parts could not be computed from the input time object:",
      i = "{missed}",
      i = "All requested {type} parts need to be included as granules for {.code linear_time()}"
    ),
    call = NULL
  )
}

# Describe a cyclical part for error messages, e.g. "1 month -> 15 months".
cycle_label <- function(x) {
  paste(time_granule_label(x[[1L]]), "->", time_granule_label(x[[2L]]))
}
