S7_graph_dispatch <- function(generic, start, end) {
  # Get the methods for the generic function (the graph dispatch edges)
  method_signatures <- traverse_methods(generic@methods)
  
  # Find all unique classes (the graph dispatch nodes)
  classes <- vec_unique(vec_unchop(method_signatures))

  S7_signature_id <- function(sig) {
    # If the argument is an S7 class, return the class identifiers (package and name)
    if (!is.list(sig)) return(S7_class_id(sig))
    
    # Iterate within the signature to access S7 classes
    lapply(sig, S7_signature_id)
  }
  
  chr_signatures <- S7_signature_id(method_signatures)
  chr_classes <- lapply(classes, S7_class_id)

  int_nodes <- seq_along(classes)
  int_edges <- vec_match(vec_unchop(chr_signatures), chr_classes)
  int_edge_from <- int_edges[seq(1, length(int_edges), by = 2)]
  int_edge_to <- int_edges[seq(2, length(int_edges), by = 2)]
  
  int_path <- bfs_shortest_path(
    from = int_edge_from,
    to = int_edge_to,
    start = vec_match(list(attr(start, "S7_class")), classes),
    end = vec_match(list(attr(end, "S7_class")), classes)
  )

  # Instantiate path of classed S7 objects for dispatch
  classes[int_path]
}

S7_class_id <- function(x) paste(x@package, x@name, sep = "::")

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
