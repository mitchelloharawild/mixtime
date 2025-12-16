## TODO: These functions are crude internal implementations of graph algorithms
## that should ideally be refactored to share common code, or provided by a
## package implementing graph algorithms.

method_signatures <- function(generic) {
  traverse_methods(generic@methods)
}

S7_graph_dispatch <- function(signatures, start, end) {
  # Find all unique classes (the graph dispatch nodes)
  classes <- vec_unique(list_unchop(signatures))

  S7_signature_id <- function(sig) {
    # If the argument is an S7 class, return the class identifiers (package and name)
    if (!is.list(sig)) return(S7_class_id(sig))
    
    # Iterate within the signature to access S7 classes
    lapply(sig, S7_signature_id)
  }
  
  chr_signatures <- S7_signature_id(signatures)
  chr_classes <- vapply(classes, S7_class_id, character(1L))

  int_nodes <- seq_along(classes)
  int_edges <- vec_match(unlist(chr_signatures), chr_classes)
  int_edge_from <- int_edges[seq(1, length(int_edges), by = 2)]
  int_edge_to <- int_edges[seq(2, length(int_edges), by = 2)]
  
  int_path <- bfs_shortest_path(
    from = int_edge_from,
    to = int_edge_to,
    start = vec_match(S7_class_id(start), chr_classes),
    end = vec_match(S7_class_id(end), chr_classes)
  )

  # Instantiate path of classed S7 objects for dispatch
  classes[int_path]
}

S7_class_id <- function(x) {
  if(inherits(x, "S7_object") && !inherits(x, "S7_class")) {
    x <- attr(x, "S7_class")
  }
  paste(x@package, x@name, sep = "::")
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
  
  # Find all unique vertices and create mapping
  all_vertices <- unique(c(from, to, start, end))
  num_vertices <- length(all_vertices)
  
  # Create mapping from vertex value to index (1-based)
  vertex_to_index <- integer(max(all_vertices))
  for (i in seq_along(all_vertices)) {
    vertex_to_index[all_vertices[i]] <- i
  }
  
  # Check if start and end vertices exist in the graph
  if (start > length(vertex_to_index) || end > length(vertex_to_index) ||
      vertex_to_index[start] == 0 || vertex_to_index[end] == 0) {
    return(integer(0))
  }
  
  # Convert start and end to indices
  start_idx <- vertex_to_index[start]
  end_idx <- vertex_to_index[end]
  
  # Build adjacency list using indices
  adj_list <- vector("list", num_vertices)
  vertex_counts <- integer(num_vertices)
  
  # Count neighbors for pre-allocation
  for (i in seq_along(from)) {
    from_idx <- vertex_to_index[from[i]]
    to_idx <- vertex_to_index[to[i]]
    
    vertex_counts[from_idx] <- vertex_counts[from_idx] + 1L
    vertex_counts[to_idx] <- vertex_counts[to_idx] + 1L
  }
  
  # Pre-allocate adjacency vectors
  for (i in 1:num_vertices) {
    adj_list[[i]] <- integer(vertex_counts[i])
  }
  
  # Fill adjacency list
  current_indices <- integer(num_vertices)
  
  for (i in seq_along(from)) {
    from_idx <- vertex_to_index[from[i]]
    to_idx <- vertex_to_index[to[i]]
    
    # Add edge from_idx -> to_idx
    current_indices[from_idx] <- current_indices[from_idx] + 1L
    adj_list[[from_idx]][current_indices[from_idx]] <- to_idx
    
    # Add edge to_idx -> from_idx (undirected)
    current_indices[to_idx] <- current_indices[to_idx] + 1L
    adj_list[[to_idx]][current_indices[to_idx]] <- from_idx
  }
  
  # BFS implementation
  queue <- integer(num_vertices)
  queue_start <- 1L
  queue_end <- 1L
  
  visited <- logical(num_vertices)
  parent <- integer(num_vertices)
  
  # Initialize BFS
  queue[queue_end] <- start_idx
  queue_end <- queue_end + 1L
  visited[start_idx] <- TRUE
  parent[start_idx] <- 0L  # No parent for start
  
  # BFS main loop
  while (queue_start < queue_end) {
    current <- queue[queue_start]
    queue_start <- queue_start + 1L
    
    # Check if we reached the destination
    if (current == end_idx) {
      # Reconstruct path
      path_indices <- integer(0)
      node <- end_idx
      
      # Build path backwards
      while (node != 0L) {
        path_indices <- c(node, path_indices)
        node <- parent[node]
      }
      
      # Convert indices back to original vertex values
      path <- integer(length(path_indices))
      for (i in seq_along(path_indices)) {
        path[i] <- all_vertices[path_indices[i]]
      }
      
      return(path)
    }
    
    # Explore neighbors
    neighbors <- adj_list[[current]]
    
    for (j in seq_along(neighbors)) {
      neighbor <- neighbors[j]
      
      if (!visited[neighbor]) {
        visited[neighbor] <- TRUE
        parent[neighbor] <- current
        queue[queue_end] <- neighbor
        queue_end <- queue_end + 1L
      }
    }
  }
  
  # No path found
  return(integer(0))
}

S7_graph_glb <- function(signatures, chronons) {
  # Find all unique classes (the graph dispatch nodes)
  classes <- vec_unique(list_unchop(signatures))

  S7_signature_id <- function(sig) {
    # If the argument is an S7 class, return the class identifiers (package and name)
    if (!is.list(sig)) return(S7_class_id(sig))
    
    # Iterate within the signature to access S7 classes
    lapply(sig, S7_signature_id)
  }
  
  chr_signatures <- S7_signature_id(signatures)
  chr_classes <- vapply(classes, S7_class_id, character(1L))

  int_nodes <- seq_along(classes)
  int_edges <- vec_match(unlist(chr_signatures), chr_classes)
  int_edge_to <- int_edges[seq(1, length(int_edges), by = 2)]
  int_edge_from <- int_edges[seq(2, length(int_edges), by = 2)]
  
  int_chronons <- vec_match(vapply(chronons, S7_class_id, character(1L)), chr_classes)

  int_glb <- greatest_lower_bound(
    from = int_edge_from,
    to = int_edge_to,
    nodes = int_chronons
  )

  if(rlang::is_empty(int_glb)) {
    stop(
      "One or more of the provided chronons do not share a common chronon.",
      call. = FALSE
    )
  }

  # Return glb
  classes[[int_glb]]
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