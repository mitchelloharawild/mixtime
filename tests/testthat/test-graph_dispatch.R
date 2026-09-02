# bfs_shortest_path(): ordinary behaviour, unaffected by adding `exclude`.

test_that("bfs_shortest_path() finds a simple two-node path", {
  # 1 -- 2
  path <- bfs_shortest_path(from = 1L, to = 2L, start = 1L, end = 2L)
  expect_equal(path, c(1L, 2L))
})

test_that("bfs_shortest_path() behaves exactly as before with no `exclude` argument", {
  # 1 -- 2 -- 3 -- 4, plus a longer detour 1 -- 5 -- 6 -- 4 that must not be
  # taken since the direct route is shorter.
  from <- c(1L, 2L, 3L, 1L, 5L, 6L)
  to <- c(2L, 3L, 4L, 5L, 6L, 4L)

  expect_equal(
    bfs_shortest_path(from = from, to = to, start = 1L, end = 4L),
    c(1L, 2L, 3L, 4L)
  )
  # No path between disconnected components.
  expect_equal(
    bfs_shortest_path(from = 1L, to = 2L, start = 1L, end = 3L),
    integer(0)
  )
  # start == end short-circuits to just the start vertex.
  expect_equal(
    bfs_shortest_path(from = 1L, to = 2L, start = 1L, end = 1L),
    1L
  )
})

# bfs_shortest_path(): new `exclude` argument.

test_that("bfs_shortest_path() routes around excluded nodes", {
  # 1 -- 2 -- 3 (direct, through 2), and 1 -- 4 -- 3 (detour, through 4).
  from <- c(1L, 2L, 1L, 4L)
  to <- c(2L, 3L, 4L, 3L)

  # Without exclusion, the shorter/first-found path via 2 is taken.
  expect_equal(
    bfs_shortest_path(from = from, to = to, start = 1L, end = 3L),
    c(1L, 2L, 3L)
  )
  # Excluding 2 forces the detour through 4.
  expect_equal(
    bfs_shortest_path(from = from, to = to, start = 1L, end = 3L, exclude = 2L),
    c(1L, 4L, 3L)
  )
  # Excluding every alternative node leaves no path.
  expect_equal(
    bfs_shortest_path(from = from, to = to, start = 1L, end = 3L, exclude = c(2L, 4L)),
    integer(0)
  )
})

test_that("bfs_shortest_path() lets `start` remain usable even if excluded", {
  from <- c(1L, 2L)
  to <- c(2L, 3L)
  # Excluding the start node itself must not block the path leaving it.
  expect_equal(
    bfs_shortest_path(from = from, to = to, start = 1L, end = 3L, exclude = 1L),
    c(1L, 2L, 3L)
  )
})

# steiner_tree_paths(): regression tests for the hub-node duplication bug.
#
# `group_path()` orders a requested group of "waypoint" nodes shallowest-first
# from `start`, then chains independent BFS segments start -> g1 -> g2 -> ...
# and concatenates them. If segment 1 (start -> g1) and segment 2 (g1 -> g2)
# both happen to route through the same intermediate hub node H (distinct
# from g1 itself), the naive concatenation contained H twice, and
# `insert_path()` then nested a second, disconnected copy of H into the tree
# instead of recognising it as already present.
#
# Below, H is node 2, reachable only from start (node 1), with g1 (node 3)
# and g2 (node 4) both hanging off it.

test_that("steiner_tree_paths() reroutes around an already-claimed hub instead of duplicating it", {
  #      1 (start)
  #      |
  #      2 (H, hub)
  #     / \
  #    3   4   <-- direct (shorter) route to g2, but 2 is already claimed
  #    |
  #    6
  #    |
  #    7
  #    |
  #    4 (also reachable the long way, avoiding the hub)
  from <- c(1L, 2L, 2L, 3L, 6L, 7L)
  to <- c(2L, 3L, 4L, 6L, 7L, 4L)

  result <- steiner_tree_paths(
    from = from,
    to = to,
    start = 1L,
    terminals = integer(0),
    groups = list(c(3L, 4L))
  )
  tree <- result$tree

  tree_nodes <- function(t) c(t$node, unlist(lapply(t$children, tree_nodes)))
  nodes <- tree_nodes(tree)

  # 1. Every real node -- crucially the hub, 2 -- appears exactly once.
  expect_equal(anyDuplicated(nodes), 0L)
  expect_setequal(nodes, c(1L, 2L, 3L, 4L, 6L, 7L))
  # The group co-located successfully, so it's not reported as failed.
  expect_equal(result$failed_groups, integer(0))

  # 2. The tree is genuinely connected/correct: walking parent -> child edges
  # reproduces valid adjacency from the original graph.
  collect_edges <- function(t) {
    do.call(rbind, c(
      lapply(t$children, function(child) data.frame(parent = t$node, child = child$node)),
      lapply(t$children, collect_edges)
    ))
  }
  edges <- collect_edges(tree)
  for (i in seq_len(nrow(edges))) {
    valid <- any(from == edges$parent[[i]] & to == edges$child[[i]]) ||
      any(from == edges$child[[i]] & to == edges$parent[[i]])
    expect_true(valid, info = paste("edge", edges$parent[[i]], "->", edges$child[[i]]))
  }

  # The hub (2) has a single child (3, the shallower group member); the
  # shorter direct hub -> 4 edge is *not* taken since 2 is already claimed by
  # segment 1, forcing segment 2 through the longer detour 3 -> 6 -> 7 -> 4.
  hub <- tree$children[[1L]]
  expect_equal(hub$node, 2L)
  expect_length(hub$children, 1L)
  expect_equal(hub$children[[1L]]$node, 3L)
})

test_that("steiner_tree_paths() drops (not corrupts) a group with no alternative to a claimed hub", {
  # Same shape as the real censored-calendar failure: g1 and g2 are *only*
  # reachable from start via the shared hub, with no alternative route for
  # segment 2 to fall back on.
  #      1 (start)
  #      |
  #      2 (H, hub)
  #     / \
  #    3   4
  from <- c(1L, 2L, 2L)
  to <- c(2L, 3L, 4L)

  result <- steiner_tree_paths(
    from = from,
    to = to,
    start = 1L,
    terminals = integer(0),
    groups = list(c(3L, 4L))
  )
  tree <- result$tree

  tree_nodes <- function(t) c(t$node, unlist(lapply(t$children, tree_nodes)))
  nodes <- tree_nodes(tree)

  # No path avoiding the claimed hub exists for segment 2, so `group_path()`
  # signals "no path found" (integer(0)) the same way it already does for a
  # genuinely disconnected group, and the group is cleanly skipped -- no
  # duplicate/disconnected copy of the hub is ever inserted.
  expect_equal(anyDuplicated(nodes), 0L)
  expect_equal(nodes, 1L)
  # The failed group is reported back to the caller by index, so it can be
  # treated as genuinely missing rather than silently matched elsewhere.
  expect_equal(result$failed_groups, 1L)
})

# S7_graph_dispatch_multi()/chronon_parts(): regression tests for the
# "stray match" bug that `failed_groups` exists to prevent.
#
# `steiner_tree_paths()` correctly declines to insert a group that cannot be
# co-located, but a member class of that failed group can still end up
# elsewhere in the tree anyway -- reached independently by a different group
# or a plain terminal. Before `failed_groups` was propagated out of
# `S7_graph_dispatch_multi()`, `chronon_parts()`'s `traverse()` had no way to
# tell that occurrence apart from a real co-located match: it matches purely
# by class id at whatever node the DFS happens to visit, so it silently
# treated the failed group as satisfied and computed a wrong, non-NULL
# number from that unrelated node instead of leaving the part missing.
#
# Graph:
#      start
#      /   \
#    hub1   hub2
#     |     /  \
#     x1   y1   y2
#     |
#     x2
#
# Group 1 = (x1, x2): a genuine chain, co-locates fine (start -> hub1 -> x1 -> x2).
# Group 2 = (y1, y2): siblings under hub2 with no edge between them, so no
# simple path visits both -- this group must fail. `y2` is *also* requested
# as a lone terminal, which the terminal-insertion pass (unaware the group
# failed) happily reaches via the very same start -> hub2 -> y2 route.
test_that("S7_graph_dispatch_multi() reports a failed group even when one of its classes is separately reachable", {
  tu_bug_start <- S7::new_class("tu_bug_start", parent = mt_unit)
  tu_bug_hub1  <- S7::new_class("tu_bug_hub1", parent = mt_unit)
  tu_bug_hub2  <- S7::new_class("tu_bug_hub2", parent = mt_unit)
  tu_bug_x1    <- S7::new_class("tu_bug_x1", parent = mt_unit)
  tu_bug_x2    <- S7::new_class("tu_bug_x2", parent = mt_unit)
  tu_bug_y1    <- S7::new_class("tu_bug_y1", parent = mt_unit)
  tu_bug_y2    <- S7::new_class("tu_bug_y2", parent = mt_unit)

  method(chronon_cardinality_fixed, list(tu_bug_start, tu_bug_hub1)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug_start, tu_bug_hub2)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug_hub1, tu_bug_x1)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug_x1, tu_bug_x2)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug_hub2, tu_bug_y1)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug_hub2, tu_bug_y2)) <- function(x, y) 2L

  dispatch <- S7_graph_dispatch_multi(
    graph = chronon_cardinality_graph(),
    start = tu_bug_start(1L),
    terminals = list(tu_bug_y2(1L)),
    groups = list(
      list(tu_bug_x1(1L), tu_bug_x2(1L)),
      list(tu_bug_y1(1L), tu_bug_y2(1L))
    )
  )

  tree_classes <- function(t) c(S7_class_id(t$node), unlist(lapply(t$children, tree_classes)))
  classes <- tree_classes(dispatch$tree)

  # Group 1 succeeded (not reported failed) and group 2 -- at index 2, not 1 --
  # is the one that failed; getting this index wrong (e.g. off-by-one) would
  # silently point the caller's null-out at the wrong group.
  expect_equal(dispatch$failed_groups, 2L)

  # And yet `y2` (a member of the *failed* group) is genuinely present in the
  # resolved tree -- reached only via the unrelated terminal request, not
  # via any co-located path with `y1`. A caller matching by class id alone
  # (as chronon_parts()'s traverse() used to) would be fooled by this into
  # treating the failed group as satisfied.
  expect_true("tu_bug_y2" %in% classes)
  # `y1` was never reached at all: nothing besides the failed group asked for it.
  expect_false("tu_bug_y1" %in% classes)
})

test_that("chronon_parts() reports a group as missing, not a wrong value, when it fails to co-locate despite one member being reachable elsewhere", {
  tu_bug2_start <- S7::new_class("tu_bug2_start", parent = mt_unit)
  tu_bug2_hub1  <- S7::new_class("tu_bug2_hub1", parent = mt_unit)
  tu_bug2_hub2  <- S7::new_class("tu_bug2_hub2", parent = mt_unit)
  tu_bug2_x1    <- S7::new_class("tu_bug2_x1", parent = mt_unit)
  tu_bug2_x2    <- S7::new_class("tu_bug2_x2", parent = mt_unit)
  tu_bug2_y1    <- S7::new_class("tu_bug2_y1", parent = mt_unit)
  tu_bug2_y2    <- S7::new_class("tu_bug2_y2", parent = mt_unit)

  method(chronon_cardinality_fixed, list(tu_bug2_start, tu_bug2_hub1)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug2_start, tu_bug2_hub2)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug2_hub1, tu_bug2_x1)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug2_x1, tu_bug2_x2)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug2_hub2, tu_bug2_y1)) <- function(x, y) 2L
  method(chronon_cardinality_fixed, list(tu_bug2_hub2, tu_bug2_y2)) <- function(x, y) 2L

  x <- structure(0L, chronon = tu_bug2_start(1L))

  # `y2` is requested both as the (failing) group's cycle *and* independently
  # as a linear target -- exactly the shape that tripped up the real
  # censored-calendar bug (a format string's `{lin(year)}` and
  # `{cyc(month,year)}` both touching `year`). Group (y1, y2) cannot
  # co-locate (siblings under hub2, no edge between them), so `y2`'s
  # cyclical part must be reported missing, not silently computed from the
  # unrelated linear traversal that legitimately reaches `y2` on its own.
  expect_error(
    chronon_parts(
      x,
      linear = list(tu_bug2_y2(1L)),
      cyclical = list(list(tu_bug2_y1(1L), tu_bug2_y2(1L)))
    ),
    "could not be computed"
  )
})
