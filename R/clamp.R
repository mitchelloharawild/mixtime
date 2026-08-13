# Calendar-field clamping
#
# Shared machinery for resolving invalid time points produced by temporal
# operations between granularities with irregular cardinality (e.g. Jan 31 + 1
# month). Only the whole-unit part of the offending remainder is clamped;
# fractional (e.g. time-of-day) and regular-cardinality remainders are left
# alone.

# --- longest path -----------------------------------------------------------

# Longest fine -> coarse chain of registered edges from `from` to `to`, so
# clamping visits every intermediate boundary (day -> month -> year) instead of
# skipping via a shorter direct edge (day -> year) with no "month" to clamp
# against. Falls back to S7_graph_dispatch()'s shortest path when no such DAG
# exists to walk.
chronon_longest_path <- function(from, to) {
  # Same time unit is always one hop - avoids collapsing to a node that can't
  # hold both `n`s.
  if (identical(S7_class_id(from), S7_class_id(to))) {
    return(list(from, to))
  }

  graph <- chronon_divmod_graph()
  int_from <- vec_match(S7_class_id(from), graph$chr_classes)
  int_to <- vec_match(S7_class_id(to), graph$chr_classes)

  int_path <- integer()
  if (!is.null(graph$edge_fine) && !is.na(int_from) && !is.na(int_to)) {
    int_path <- dag_longest_path(
      graph$edge_fine,
      graph$edge_coarse,
      int_from,
      int_to
    )
  }

  path <- if (length(int_path) == 0L) {
    # No monotone route - fall back to shortest path.
    S7_graph_dispatch(chronon_divmod_graph(), from, to)
  } else {
    graph$classes[int_path]
  }
  path[[1L]] <- from
  path[[length(path)]] <- to
  path[c(-1L, -length(path))] <- lapply(
    path[c(-1L, -length(path))],
    function(tu) {
      granule_inherit_props(granule_inherit_props(tu(1L), to), from)
    }
  )
  path
}

# --- component composition ----------------------------------------------------

# TRUE if from<->to has a fixed (context-independent) cardinality ratio - never
# needs clamping.
chronon_hop_is_regular <- function(from, to) {
  from_id <- S7_class_id(from)
  to_id <- S7_class_id(to)
  fixed_methods <- chronon_cardinality_fixed@methods

  identical(from_id, to_id) ||
    !is.null(fixed_methods[[from_id]][[to_id]]) ||
    !is.null(fixed_methods[[to_id]][[from_id]])
}

# TRUE when the component machinery below is worth using: `to` must be a coarser
# field containing `from` (chronon_nests_in()), and the conversion must not
# already be exact via chronon_cardinality_fixed() alone
# (!chronon_nests_in_fixed()). Otherwise callers should fall back to a plain
# chronon_convert_impl()/chronon_divmod() round trip.
chronon_needs_clamping <- function(from, to) {
  chronon_nests_in(from, to) && !chronon_nests_in_fixed(from, to)
}

# Forward walk: decompose `x` (path[[1]] units) into the (div, mod) pair at
# every hop of `path`. `div[[i]]` is `x` in path[[i]] units; `mod[[i]]` is the
# remainder from converting path[[i+1]] -> path[[i]].
chronon_decompose <- function(x, path) {
  n <- length(path)
  div <- vector("list", n)
  mod <- vector("list", n - 1L)
  div[[1L]] <- x
  for (i in seq(2L, length.out = n - 1L)) {
    result <- chronon_divmod_dispatch(path[[i - 1L]], path[[i]], div[[i - 1L]])
    div[[i]] <- result$div
    mod[[i - 1L]] <- result$mod
  }
  list(div = div, mod = mod)
}

# Backward walk: reconstruct a path[[1]]-unit value from `div`/`mod`
# (chronon_decompose()'s output, `div` possibly shifted). Clamps each irregular
# hop's whole-unit remainder to its paired coarser count's valid range;
# `reverse` clamps to the lower bound for descending shifts. `clamped` flags
# where clamping changed a value (for warning on overflow).
chronon_recompose <- function(div, mod, path, clamp = TRUE, reverse = FALSE) {
  n <- length(path)
  value <- div
  clamped <- rep(FALSE, length(value))
  for (i in seq(n, by = -1L, length.out = n - 1L)) {
    m <- mod[[i - 1L]]
    if (clamp && !chronon_hop_is_regular(path[[i - 1L]], path[[i]])) {
      card <- chronon_cardinality(path[[i - 1L]], path[[i]], at = value)
      m_int <- floor(m)
      m_clamped <- (if (reverse) pmax else pmin)(card - 1, m_int)
      clamped <- clamped | (m_clamped != m_int)
      m <- m_clamped + (m - m_int)
    }
    value <- chronon_divmod_dispatch(path[[i]], path[[i - 1L]], value)$div + m
  }
  list(value = value, clamped = clamped)
}
