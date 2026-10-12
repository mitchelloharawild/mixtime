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
# (chronon_decompose()'s output, `div` possibly shifted). An irregular hop's
# whole-unit remainder that exceeds its paired coarser count's valid range is
# resolved according to `on_invalid`:
#
# * "clamp": reduce the offending remainder to the last valid unit, keeping
#   finer remainders (e.g. 2026-01-31 14:30 + 1 month -> 2026-02-28 14:30).
# * "previous": the end of the target granule, discarding finer remainders.
#   For `discrete` time this is its last path[[1]] chronon (e.g. 2026-01-31
#   14:30 + 1 month -> 2026-02-28 23:59:59); for continuous time it is the end
#   of that chronon, the same instant as "next" (-> 2026-03-01 00:00:00).
# * "next": the start of the following granule, discarding finer remainders
#   (e.g. 2026-01-31 14:30 + 1 month -> 2026-03-01 00:00:00).
# * "overflow": leave the remainder to carry into the next granule.
# * "reject", "error": NA (callers signal the warning or error).
#
# `invalid` flags which elements needed resolving (for warnings).
chronon_recompose <- function(
  div,
  mod,
  path,
  on_invalid = "clamp",
  discrete = TRUE
) {
  n <- length(path)
  to_boundary <- on_invalid %in% c("previous", "next")
  value <- div
  invalid <- rep(FALSE, length(value))
  for (i in seq(n, by = -1L, length.out = n - 1L)) {
    m <- rep_len(mod[[i - 1L]], length(value))
    # A coarser hop already resolved to the start of the next granule: finer
    # components are discarded (for "previous" on discrete time, 1 chronon is
    # subtracted below).
    if (to_boundary) m[invalid] <- 0
    if (
      on_invalid != "overflow" &&
        !chronon_hop_is_regular(path[[i - 1L]], path[[i]])
    ) {
      card <- rep_len(
        chronon_cardinality(path[[i - 1L]], path[[i]], at = value),
        length(value)
      )
      m_int <- floor(m)
      bad <- (m_int > card - 1) %in% TRUE
      invalid <- invalid | bad
      m[bad] <- if (on_invalid == "clamp") {
        card[bad] - 1 + (m[bad] - m_int[bad])
      } else if (to_boundary) {
        card[bad]
      } else {
        NA
      }
    }
    value <- chronon_divmod_dispatch(path[[i]], path[[i - 1L]], value)$div + m
  }
  if (on_invalid == "previous" && discrete) {
    value[invalid] <- value[invalid] - 1
  }
  list(value = value, invalid = invalid)
}

on_invalid_strategies <- c("clamp", "previous", "next", "overflow", "reject", "error")

# Validate an `on_invalid` strategy, which defaults to the `mixtime.on_invalid`
# option.
check_on_invalid <- function(on_invalid, call = rlang::caller_env()) {
  if (!(rlang::is_string(on_invalid) && on_invalid %in% on_invalid_strategies)) {
    cli::cli_abort(
      c(
        "{.arg on_invalid} must be one of {.or {.val {on_invalid_strategies}}}.",
        "i" = "The default is set by the {.code mixtime.on_invalid} option."
      ),
      call = call
    )
  }
  on_invalid
}

# Signal invalid time points for the "reject" (warning) and "error" strategies.
signal_invalid <- function(invalid, on_invalid) {
  if (!any(invalid)) {
    return(invisible())
  }
  hint <- c(
    "i" = "Set {.arg on_invalid} to {.val clamp}, {.val previous}, {.val next}, or {.val overflow} to resolve them to valid time points instead."
  )
  if (on_invalid == "error") {
    cli::cli_abort(
      c(
        "{sum(invalid)} time point{?s} did not exist in the target granule, at {cli::qty(sum(invalid))}position{?s} {which(invalid)}.",
        hint
      ),
      call = NULL
    )
  }
  if (on_invalid == "reject") {
    cli::cli_warn(
      c(
        "{sum(invalid)} time point{?s} did not exist in the target granule and {cli::qty(sum(invalid))}{?was/were} replaced with {.val NA}.",
        hint
      ),
      call = NULL
    )
  }
}
