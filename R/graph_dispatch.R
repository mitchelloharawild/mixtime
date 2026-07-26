## TODO: These functions are crude internal implementations of graph algorithms
## that should ideally be refactored to share common code, or provided by a
## package implementing graph algorithms.

# Order a list of S7 classes (or mt_unit objects) from finest to coarsest using
# directed reachability in the chronon_cardinality method graph, where edges
# point from fine -> coarse. Returns an integer vector of indices.
# S7_order_granules <- function(classes) {
#   class_ids <- vapply(classes, S7_class_id, character(1L))

#   sigs        <- method_signatures(chronon_cardinality)
#   all_sig_cls <- vec_unique(list_unchop(sigs))
#   chr_all     <- vapply(all_sig_cls, S7_class_id, character(1L))

#   int_edges     <- vec_match(
#     unlist(lapply(sigs, function(s) vapply(s, S7_class_id, character(1L)))),
#     chr_all
#   )
#   int_edge_from <- int_edges[seq(1L, length(int_edges), by = 2L)]
#   int_edge_to   <- int_edges[seq(2L, length(int_edges), by = 2L)]

#   # Kahn's topological sort: fine nodes (in-degree 0) come first
#   n         <- length(chr_all)
#   in_degree <- tabulate(int_edge_to, nbins = n)
#   queue     <- which(in_degree == 0L)
#   topo      <- integer(0L)

#   while (length(queue) > 0L) {
#     v         <- queue[[1L]]; queue <- queue[-1L]
#     topo      <- c(topo, v)
#     nbrs      <- int_edge_to[int_edge_from == v]
#     in_degree[nbrs] <- in_degree[nbrs] - 1L
#     queue     <- c(queue, nbrs[in_degree[nbrs] == 0L])
#   }

#   int_targets <- vec_match(class_ids, chr_all)
#   order(match(int_targets, topo))
# }

method_signatures <- function(generic) {
  traverse_methods(generic@methods)
}

S7_signature_id <- function(sig) {
  # If the argument is an S7 class, return the class identifiers (package and name)
  if (!is.list(sig)) return(S7_class_id(sig))

  # Iterate within the signature to access S7 classes
  lapply(sig, S7_signature_id)
}

# Compile a signature list into the flattened graph structures shared by
# S7_graph_dispatch(), S7_graph_dispatch_multi() and S7_graph_glb(): the
# de-duplicated node classes, their character ids, and integer edge endpoints.
compile_signature_graph <- function(signatures) {
  classes <- vec_unique(list_unchop(signatures))
  chr_classes <- vapply(classes, S7_class_id, character(1L))

  int_edges <- vec_match(unlist(S7_signature_id(signatures)), chr_classes)

  list(
    classes = classes,
    chr_classes = chr_classes,
    edge_from = int_edges[seq(1, length(int_edges), by = 2)],
    edge_to = int_edges[seq(2, length(int_edges), by = 2)]
  )
}

# Cache of compiled signature graphs used for chronon graph dispatch.
#
# The registered S7 methods for chronon_divmod()/chronon_cardinality() are
# fixed after package load for the built-in calendars, and extensions only
# ever add methods (S7 provides no way to remove one), so the graphs
# compiled from them are cached and only rebuilt when a raw (pre-dedup)
# signature count changes. This avoids re-deriving the dispatch graph
# (unique()-ing ~35 signature objects) on every chronon_convert() call.
.chronon_signature_cache <- new.env(parent = emptyenv())

# The divmod graph only uses edges from direct chronon_divmod() methods and
# fixed (context-independent) chronon_cardinality_fixed() methods. 
chronon_divmod_graph <- function() {
  sig_divmod <- method_signatures(chronon_divmod)
  sig_fixed <- method_signatures(chronon_cardinality_fixed)
  n <- length(sig_divmod) + length(sig_fixed)

  cache <- .chronon_signature_cache
  if (!identical(cache$divmod_n, n)) {
    cache$divmod_n <- n
    cache$divmod_graph <- compile_signature_graph(unique(c(sig_divmod, sig_fixed)))
  }
  cache$divmod_graph
}

chronon_cardinality_graph <- function() {
  sig_card <- method_signatures(chronon_cardinality)
  sig_fixed <- method_signatures(chronon_cardinality_fixed)
  n <- length(sig_card) + length(sig_fixed)

  cache <- .chronon_signature_cache
  if (!identical(cache$cardinality_n, n)) {
    cache$cardinality_n <- n
    cache$cardinality_graph <- compile_signature_graph(unique(c(sig_card, sig_fixed)))
  }
  cache$cardinality_graph
}

# The fixed-cardinality graph only uses edges from chronon_cardinality_fixed()
# methods, since these are the only relationships guaranteed to be constant
# (context-independent) and therefore safely multiplied together along a path.
chronon_cardinality_fixed_graph <- function() {
  sig_fixed <- method_signatures(chronon_cardinality_fixed)
  n <- length(sig_fixed)

  cache <- .chronon_signature_cache
  if (!identical(cache$cardinality_fixed_n, n)) {
    cache$cardinality_fixed_n <- n
    cache$cardinality_fixed_graph <- compile_signature_graph(unique(sig_fixed))
  }
  cache$cardinality_fixed_graph
}

S7_graph_dispatch <- function(graph, start, end) {
  int_node_start <- vec_match(S7_class_id(start), graph$chr_classes)
  int_node_end <- vec_match(S7_class_id(end), graph$chr_classes)
  if (is.na(int_node_start) || is.na(int_node_end)) {
    missing_units <- c(
      if (is.na(int_node_start)) S7_class_id(start),
      if (is.na(int_node_end)) S7_class_id(end)
    )
    cli::cli_abort(
      c(
        "There were no registered calendar arithmetic methods for the following classes: {paste(missing_units, collapse = \", \")}.",
        "i" = "Have you registered calendar arithmetic S7 methods for these classes?",
        ">" = "See the {.vignette mixtime::extending-mixtime} vignette for more details."
      ),
      call. = FALSE
    )
  }

  int_path <- bfs_shortest_path(
    from = graph$edge_from,
    to = graph$edge_to,
    start = int_node_start,
    end = int_node_end
  )

  if (length(int_path) == 0L) {
    cli::cli_abort(
      c(
        "There is no path of registered calendar arithmetic methods between the classes {S7_class_id(start)} and {S7_class_id(end)}.",
        "i" = "Have you registered calendar arithmetic S7 methods connecting these classes?",
        ">" = "See the {.vignette mixtime::extending-mixtime} vignette for more details."
      ),
      call. = FALSE
    )
  }

  # Instantiate path of classed S7 objects for dispatch
  graph$classes[int_path]
}

# Directed reachability: is `end` reachable from `start` by following edges in
# the from -> to direction only? Used to test the fine -> coarse ordering of the
# chronon_cardinality graph (undirected BFS above would ignore direction).
directed_reachable <- function(from, to, start, end) {
  frontier <- start
  visited <- start
  while (length(frontier) > 0L) {
    reached <- to[from %in% frontier]
    if (end %in% reached) return(TRUE)
    frontier <- setdiff(reached, visited)
    visited <- c(visited, frontier)
  }
  FALSE
}

# TRUE if `chronon` nests within `granule`, i.e. `granule` is equal to or a
# coarser container of `chronon`. Edges in the chronon_cardinality graph point
# fine -> coarse, so this holds exactly when `granule` is directed-reachable from
# `chronon`. FALSE when the two granules have no registered relationship.
chronon_nests_in <- function(chronon, granule) {
  from_id <- S7_class_id(chronon)
  to_id <- S7_class_id(granule)
  if (from_id == to_id) return(granule@n %% chronon@n == 0L)

  graph <- chronon_cardinality_graph()
  start <- match(from_id, graph$chr_classes)
  end <- match(to_id, graph$chr_classes)
  if (is.na(start) || is.na(end)) return(FALSE)

  directed_reachable(graph$edge_from, graph$edge_to, start, end)
}

S7_class_id <- function(x) {
  if (inherits(x, "S7_object") && !inherits(x, "S7_class")) {
    # An S7 object's class attribute already begins with its class id, so an
    # instance needs neither a class lookup nor any property access.
    return(class(x)[[1L]])
  }
  # A class not attached to a package is identified by its name alone.
  package <- x@package
  if (is.null(package)) x@name else paste0(package, "::", x@name)
}

traverse_methods <- function(x) {
  if (!is.environment(x)) {
    return(list(x@signature))
  }

  methods <- lapply(names(x), function(class) traverse_methods(x[[class]]))
  unlist(methods, recursive = FALSE)
}


bfs_shortest_path <- function(from = integer(), to = integer(), start = integer(), end = integer()) {
  # Input validation
  if (length(from) != length(to)) {
    return(integer(0))
  }

  if (length(start) != 1 || length(end) != 1) {
    return(integer(0))
  }

  if (length(from) == 0) {
    return(integer(0))
  }

  # Convert to integers
  from <- as.integer(from)
  to <- as.integer(to)
  start <- as.integer(start)
  end <- as.integer(end)

  # If start equals end, return just the start vertex
  if (start == end) {
    return(start)
  }

  # Edges are undirected, so each is walked in both directions. Interleaving the
  # two directions of an edge (rather than listing every forward direction and
  # then every reverse one) leaves each vertex's neighbours in edge order.
  n <- max(from, to, start, end)
  tail <- c(rbind(from, to))
  head <- c(rbind(to, from))

  # Compressed adjacency, built with whole-vector operations rather than an
  # edge-by-edge loop: `nbr` holds every vertex's neighbours end to end, `deg`
  # how many each has, and `off` where each vertex's run starts. The explicit
  # second sort key keeps neighbours in edge order without relying on the sort
  # being stable.
  ord <- order(tail, seq_along(tail))
  nbr <- head[ord]
  deg <- tabulate(tail, nbins = n)
  off <- c(0L, cumsum(deg))

  visited <- logical(n)
  parent <- integer(n)
  visited[[start]] <- TRUE
  frontier <- start

  # Expand a whole BFS level per iteration: gathering the frontier's neighbour
  # runs in one indexing operation makes the number of interpreted iterations
  # proportional to the length of the path rather than to the size of the graph.
  while (length(frontier) > 0L && !visited[[end]]) {
    d <- deg[frontier]
    adjacent <- nbr[rep.int(off[frontier], d) + sequence(d)]
    origin <- rep.int(frontier, d)

    # Keep each vertex's first unvisited occurrence, which is the one a queue
    # popping the frontier in order would have reached first.
    new <- !visited[adjacent] & !duplicated(adjacent)
    frontier <- adjacent[new]
    parent[frontier] <- origin[new]
    visited[frontier] <- TRUE
  }

  # No path found
  if (!visited[[end]]) {
    return(integer(0))
  }

  # Reconstruct the path by walking parents back from `end`, filling a buffer
  # from the right so it needs no growing.
  path <- integer(n)
  i <- n
  node <- end
  while (node != start) {
    path[[i]] <- node
    node <- parent[[node]]
    i <- i - 1L
  }
  path[[i]] <- start
  path[i:n]
}

S7_graph_dispatch_multi <- function(graph, start, terminals = list(), groups = list()) {
  chr_classes <- graph$chr_classes

  int_start <- vec_match(S7_class_id(start), chr_classes)

  # terminals: individual nodes that just need to appear somewhere in the tree
  int_terminals <- vec_match(
    vapply(terminals, S7_class_id, character(1L)),
    chr_classes
  )

  # groups: lists of nodes that must all co-occur on the same root-to-leaf path
  int_groups <- lapply(groups, function(group) {
    if (!is.list(group)) group <- list(group)
    vec_match(vapply(group, S7_class_id, character(1L)), chr_classes)
  })

  int_tree <- steiner_tree_paths(
    from      = graph$edge_from,
    to        = graph$edge_to,
    start     = int_start,
    terminals = int_terminals,
    groups    = int_groups
  )

  # Nodes are conversion waypoints rather than the granules asked for, so each
  # resolves to the finest granule of its time unit: `start` for the unit the
  # time points are measured in (which cannot be subdivided further), and a
  # single unit elsewhere. Resolving to a requested granule instead would make a
  # node as coarse as the coarsest granule requested of it, discarding the
  # remainder that finer granules of the same unit need. Callers rescale each
  # requested granule from its node, which relies on no node being coarser than
  # a granule requested of it.
  start_id <- S7_class_id(start)

  # A granule the caller already supplied is reused when it is itself no coarser
  # than a single unit: it needs no rebuilding and carries their tz / location.
  supplied    <- c(terminals, unlist(groups, recursive = FALSE))
  supplied_id <- vapply(supplied, S7_class_id, character(1L))
  supplied_n  <- vapply(supplied, function(granule) granule@n, numeric(1L))

  # Properties only need copying onto a constructed node when `start` holds one
  # that is not naive (e.g. a time zone) for it to inherit.
  start_informs <- any(!is.naive(props(start)[setdiff(names(props(start)), "n")]))

  resolve_tree <- function(node) {
    node_id <- chr_classes[[node$node]]
    finest <- which(supplied_id == node_id)
    if (length(finest)) finest <- finest[[which.min(supplied_n[finest])]]

    resolved <- if (identical(node_id, start_id)) {
      start
    } else if (length(finest) && supplied_n[[finest]] <= 1) {
      supplied[[finest]]
    } else {
      granule <- graph$classes[[node$node]](1L)
      if (start_informs) granule_inherit_props(granule, start) else granule
    }

    list(
      node     = resolved,
      children = lapply(node$children, resolve_tree)
    )
  }
  resolve_tree(int_tree)
}

# Returns a nested list: list(node = int, children = list(...))
# Groups are inserted first (chained BFS ensuring co-occurrence on one path).
# Terminals are only inserted if not already present in the tree.
steiner_tree_paths <- function(
    from      = integer(),
    to        = integer(),
    start     = integer(),
    terminals = integer(),
    groups    = list()) {
  if (length(from) != length(to)) return(list())
  if (length(start) != 1L)        return(list())
  if (length(from) == 0L)         return(list())

  # Collect all nodes currently present in the tree
  tree_nodes <- function(tree) {
    c(tree$node, unlist(lapply(tree$children, tree_nodes)))
  }

  # For a group of nodes, order by BFS depth from start (shallowest first),
  # then chain BFS segments: start -> g1 -> g2 -> ...
  group_path <- function(group) {
    if (length(group) == 1L) {
      return(bfs_shortest_path(from = from, to = to, start = start, end = group))
    }
    depths <- vapply(group, function(node) {
      p <- bfs_shortest_path(from = from, to = to, start = start, end = node)
      if (length(p) == 0L) Inf else length(p) - 1L
    }, numeric(1L))
    ordered_group <- group[order(depths)]

    waypoints <- c(start, ordered_group)
    path <- integer(0)
    for (i in seq_len(length(waypoints) - 1L)) {
      segment <- bfs_shortest_path(
        from  = from, to = to,
        start = waypoints[[i]], end = waypoints[[i + 1L]]
      )
      if (length(segment) == 0L) return(integer(0))
      path <- c(path, if (i == 1L) segment else segment[-1L])
    }
    path
  }

  insert_path <- function(tree, path) {
    if (length(path) == 0L) return(tree)

    node <- path[[1L]]
    rest <- path[-1L]

    child_idx <- which(vapply(tree$children, function(c) c$node == node, logical(1L)))

    if (length(child_idx) == 0L) {
      new_child <- insert_path(list(node = node, children = list()), rest)
      tree$children <- c(tree$children, list(new_child))
    } else {
      tree$children[[child_idx]] <- insert_path(tree$children[[child_idx]], rest)
    }

    tree
  }

  tree <- list(node = start, children = list())

  # 1. Insert group paths first (enforce co-occurrence)
  for (group in groups) {
    path <- group_path(group)
    if (length(path) > 1L) {
      tree <- insert_path(tree, path[-1L])
    }
  }

  # 2. Insert terminals only if not already in the tree
  for (terminal in terminals) {
    if (!terminal %in% tree_nodes(tree)) {
      path <- bfs_shortest_path(from = from, to = to, start = start, end = terminal)
      if (length(path) > 1L) {
        tree <- insert_path(tree, path[-1L])
      }
    }
  }

  tree
}

S7_graph_glb <- function(graph, chronons) {
  int_chronons <- vec_match(vapply(chronons, S7_class_id, character(1L)), graph$chr_classes)

  int_glb <- greatest_lower_bound(
    from = graph$edge_from,
    to = graph$edge_to,
    nodes = int_chronons
  )

  if(rlang::is_empty(int_glb)) {
    stop(
      "One or more of the provided chronons do not share a common chronon.",
      call. = FALSE
    )
  }

  # Return glb
  graph$classes[[int_glb]]
}

# Finds the greatest lower bound that contains all `nodes` in a graph defined by
# directed edges from `from` to `to`.
greatest_lower_bound <- function(from = integer(), to = integer(), nodes = integer()) {
  if (length(nodes) == 1) {
    return(nodes)
  }
  all_nodes <- unique(c(from, to))
  
  # Build parent map: each node -> set of its parents
  parent_map <- split(from, to)
  
  # Helper: Function to get all ancestors (including self) for a node
  get_ancestors <- function(node) {
    stack <- node
    visited <- logical()
    ancestors <- node
    while (length(stack) > 0) {
      current <- stack[[1]]
      stack <- stack[-1]
      parents <- parent_map[[as.character(current)]]
      # visit parents not yet visited
      if (!is.null(parents)) {
        new_parents <- setdiff(parents, ancestors)
        if (length(new_parents) > 0) {
          ancestors <- c(ancestors, new_parents)
          stack <- c(stack, new_parents)
        }
      }
    }
    ancestors
  }
  
  # Find all ancestors for each node in 'nodes'
  ancestors_list <- lapply(nodes, get_ancestors)
  
  # The GLB are the nodes that are common to all ancestor sets
  common_ancestors <- Reduce(intersect, ancestors_list)
  
  if (length(common_ancestors) == 0)
    return(integer(0)) # No common ancestor
  
  # Among common ancestors, pick the one that is farthest from root (i.e., closest to nodes)
  # For this, compute for each such node the minimal distance to any of the target nodes
  
  node_depth <- function(target, candidate) {
    # Walk down from candidate to target
    # We'll perform BFS from candidate to target
    queue <- list(candidate)
    depth_map <- `names<-`(0, candidate)
    visited <- candidate
    while (length(queue) > 0) {
      current <- queue[[1]]
      queue <- queue[-1]
      if (current == target) {
        return(depth_map[[as.character(current)]])
      }
      # Find "children" (those where current is parent)
      children <- to[which(from == current)]
      new_children <- setdiff(children, visited)
      if (length(new_children) > 0) {
        queue <- c(queue, new_children)
        new_depths <- depth_map[[as.character(current)]] + 1
        depth_map <- c(depth_map, `names<-`(rep(new_depths, length(new_children)), new_children))
        visited <- c(visited, new_children)
      }
    }
    Inf # No path found
  }

  distance_to_nodes <- function(candidate) {
    # For each node in the set, find the length of the path from candidate to node
    min(vapply(nodes, node_depth, numeric(1L), candidate = candidate), na.rm = TRUE)
  }
  
  # For each common ancestor, compute max depth to any node in 'nodes'.
  # Take the one with *minimal* (i.e., greatest lower bound, or furthest from root)
  depths <- vapply(common_ancestors, distance_to_nodes, numeric(1L))
  # The greatest lower bound is the one with minimal distance to its furthest descendant in 'nodes'
  glb <- common_ancestors[which.min(depths)]
  
  as.integer(glb)
}