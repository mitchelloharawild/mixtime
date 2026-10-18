# Internal functions that would likely be better reimplemented on top of
# chronon_traverse() (not yet done; most work on integer node ids for speed,
# so a rewrite should check it doesn't slow down hot paths like
# chronon_convert()):
#
# * directed_reachable(), chronon_graph_reachable(), chronon_fixed_reachable():
#   reachability is a traversal with `.until` true at the target, then checking
#   whether the target's id is among the names of the result.
# * The graph-walk fallback in method(chronon_cardinality_fixed,
#   list(mt_unit, mt_unit)): a product fold of chronon_cardinality_fixed() until
#   the target is reached, taking the target's value. It walks the fixed graph
#   undirected today, so it would need a "coarser" then a "finer" (inverted)
#   traversal; fixed edges are all monotone, so nothing is lost.
# * chronon_nests_in_fixed(), chronon_nests_in_fixed_ratio() and
#   chronon_fixed_multiple(): the same product fold yields reachability and the
#   ratio in one walk, instead of a reachability walk followed by a dispatch
#   walk inside chronon_cardinality_fixed().
# * chronon_nests_in(): its irregular-path branch looks for an intermediate
#   granule that `chronon` nests in fixedly and that nests in `granule`; the
#   candidates are a "coarser" traversal of the cardinality graph from `chronon`
#   (`.until` at `granule`), rather than the interior of chronon_longest_path().
# * S7_graph_lub() / S7_graph_glb() via least_upper_bound() /
#   greatest_lower_bound(): fold the depth (`acc + 1`) "coarser" (lub) or
#   "finer" (glb) from each chronon, intersect the reached ids, and take the one
#   with the smallest maximum depth. That replaces get_ancestors() plus one
#   node_depth() BFS per candidate and target.
# * The commented-out S7_order_granules(): granule `a` is finer than `b` when
#   `b`'s id is among the names of a "coarser" traversal from `a`, giving the
#   pairwise order to sort by (a topological sort is only needed for speed).
#
# Not a fit: dag_longest_path() / chronon_longest_path() want the longest path,
# which a first-visit-wins BFS can't give; bfs_shortest_path() works on the
# undirected divmod graph with `exclude`, and is better kept as an integer
# engine (chronon_traverse() could share its compressed adjacency instead).

#' Traverse the graph of time unit relationships
#'
#' Walks out from a chronon through the time units it is related to by
#' [chronon_cardinality()] methods, either to coarser or finer time units,
#' accumulating a value along the way. This is a low-level building block for
#' questions about how time units relate, such as "which coarser time units
#' does a month nest within a fixed number of times?" or "which finer time
#' units divide a day between 3 and 15 times?".
#'
#' @param x A time granule to start from (e.g. `cal_gregorian$month(1L)`).
#' @param .f A function `function(acc, from, to)` returning the accumulated
#'   value at time granule `to`, given the accumulated value `acc` at the
#'   adjacent time granule `from` it was reached from. `from` is `x` for the
#'   time units adjacent to `x`.
#' @param .init The accumulated value at `x`.
#' @param .until A function `function(acc, granule)` that returns `TRUE` to
#'   stop the traversal beyond `granule`. The granule is still included in the
#'   result, but time units only reachable through it are not visited.
#' @param direction Walk to `"coarser"` time units (e.g. from months to years)
#'   or `"finer"` time units (e.g. from months to days).
#' @param graph Which relationships to walk along: `"fixed"` uses only the
#'   constant relationships of [chronon_cardinality_fixed()] methods (e.g. 12
#'   months in a year), while `"cardinality"` also includes context-dependent
#'   ones from [chronon_cardinality()] methods (e.g. days in a month).
#'
#' @return A list with one element for each time unit reached (excluding `x`'s
#'   own time unit), named by its class and in breadth-first order. Each element
#'   is a list of `granule`, a single unit (`n = 1L`) granule of that time unit
#'   which inherits the calendar and properties (such as time zone) of `x`, and
#'   `value`, the accumulated value. A time unit with no registered
#'   relationships gives an empty list.
#'
#' @details
#'
#' The traversal is breadth-first, and when a time unit can be reached along
#' several paths only the first (shortest) is followed. The accumulated value
#' is a fold along that path, which suits values that compose along a path,
#' such as fixed cardinalities, the path itself, or its length.
#'
#' Context-dependent cardinalities don't compose along a path: the 29 days of
#' February 2024 times the 3 months of a quarter is not the 91 days of the
#' first quarter of 2024. Compute these from `x` itself, for example with
#' `function(acc, from, to) chronon_cardinality(x, to, at = at)`.
#'
#' @examples
#' # Cumulative fixed cardinality of each coarser time unit
#' card_up <- function(acc, from, to) acc * chronon_cardinality_fixed(from, to)
#'
#' # There are 3 months in a quarter, and 12 in a year
#' traversal <- chronon_traverse(cal_gregorian$month(1L), card_up, 1)
#' vapply(traversal, function(e) e$value, numeric(1L))
#'
#' # Coarser time units containing at most 15 hours
#' traversal <- chronon_traverse(
#'   cal_gregorian$hour(1L), card_up, 1,
#'   .until = function(acc, granule) acc >= 15
#' )
#' vapply(traversal, function(e) e$value, numeric(1L))
#'
#' # Finer time units that divide a day up to 15 times
#' card_down <- function(acc, from, to) acc * chronon_cardinality_fixed(to, from)
#' traversal <- chronon_traverse(
#'   cal_gregorian$day(1L), card_down, 1,
#'   .until = function(acc, granule) acc > 15,
#'   direction = "finer"
#' )
#' vapply(traversal, function(e) e$value, numeric(1L))
#'
#' # The path from minutes to each coarser time unit
#' traversal <- chronon_traverse(
#'   cal_gregorian$minute(1L),
#'   function(acc, from, to) c(acc, class(to)[[1L]]),
#'   .init = "mixtime::tu_minute"
#' )
#' vapply(traversal, function(e) paste(e$value, collapse = " > "), character(1L))
#'
#' @export
chronon_traverse <- function(
  x,
  .f,
  .init,
  .until = function(acc, granule) FALSE,
  direction = c("coarser", "finer"),
  graph = c("fixed", "cardinality")
) {
  if (!S7_inherits(x, mt_unit)) {
    cli::cli_abort(
      "{.arg x} must be a time granule (e.g. {.code cal_gregorian$month(1L)}), not {.obj_type_friendly {x}}."
    )
  }
  if (!is.function(.f)) {
    cli::cli_abort("{.arg .f} must be a function, not {.obj_type_friendly {(.f)}}.")
  }
  if (!is.function(.until)) {
    cli::cli_abort("{.arg .until} must be a function, not {.obj_type_friendly {(.until)}}.")
  }
  direction <- rlang::arg_match(direction)
  graph <- rlang::arg_match(graph)
  # The divmod graph isn't offered: its edges are registered both ways, so
  # they have no coarser/finer direction to walk along.
  graph <- switch(
    graph,
    fixed = chronon_cardinality_fixed_graph(),
    cardinality = chronon_cardinality_graph()
  )

  # Edges run fine -> coarse; walking finer reverses them. Self-loops (the
  # mt_unit -> mt_unit fallback methods) lead nowhere new.
  from <- graph$edge_from
  to <- graph$edge_to
  if (direction == "finer") {
    tmp <- from
    from <- to
    to <- tmp
  }
  loop <- from == to
  adjacent <- split(to[!loop], factor(from[!loop], seq_along(graph$chr_classes)))

  start <- match(S7_class_id(x), graph$chr_classes)
  out <- list()
  if (is.na(start)) {
    return(rlang::set_names(out))
  }

  # Graph nodes are reached as unit granules carrying `x`'s calendar and
  # properties (e.g. time zone), so `.f` sees the context `x` was given in.
  node_granule <- function(i) {
    granule <- graph$classes[[i]](1L)
    granule@calendar <- NULL
    granule_inherit_props(granule, x)
  }

  # `.f` gets only the adjacent pair, not `x`: the caller already has `x` (and
  # any `at`) to close over when a value must be measured from the origin,
  # which keeps `.f` a plain fold over edges.
  visited <- start
  queue <- list(list(node = start, granule = x, acc = .init))
  while (length(queue) > 0L) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    for (i in setdiff(adjacent[[current$node]], visited)) {
      visited <- c(visited, i)
      granule <- node_granule(i)
      acc <- .f(current$acc, current$granule, granule)
      out[[graph$chr_classes[[i]]]] <- list(granule = granule, value = acc)
      if (!isTRUE(.until(acc, granule))) {
        queue[[length(queue) + 1L]] <- list(node = i, granule = granule, acc = acc)
      }
    }
  }
  out
}
