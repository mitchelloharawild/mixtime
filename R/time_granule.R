#' @importFrom S7 props props<-
granule_inherit_props <- function(x, parent) {
  naive_props <- is.naive(props <- props(x))
  parent_props <- props(parent)
  inherit_props <- intersect(names(props)[naive_props], names(parent_props))
  props[inherit_props] <- parent_props[inherit_props]
  props(x) <- props
  x
}

# Inherit naive properties on which every parent shares the same attribute.
granule_inherit_shared_props <- function(x, parents) {
  # Build a stand-in parent by filling x's own naive properties with the
  # value every parent agrees on, then let granule_inherit_props() perform
  # the actual copy (a no-op for properties left untouched below).
  shared <- x
  shared_props <- props(shared)

  for (prop in names(shared_props)[is.naive(shared_props)]) {
    values <- lapply(parents, function(parent) props(parent)[[prop]])

    is_unset <- vapply(values, function(value) {
      # `tz` (and other naive-fillable properties) can carry an explicit NA
      # rather than the classed `mt_naive` sentinel (e.g. `datetime(x, tz =
      # NA)`) -- both mean "unknown", so both must block reconciliation.
      is.null(value) || inherits(value, "mt_naive") || (length(value) == 1L && is.na(value))
    }, logical(1L))

    if (any(is_unset)) {
      next
    }

    if (prop == "tz") {
      # Differing known zones can always be reconciled at a common zone (UTC,
      # since every named zone converts to it without loss); only a genuinely
      # unknown zone (handled above) leaves the result naive.
      shared_props[[prop]] <- if (length(unique(values)) == 1L) values[[1L]] else "UTC"
      next
    }

    if (!all(vapply(values, identical, logical(1L), values[[1L]]))) {
      next
    }

    shared_props[[prop]] <- values[[1L]]
  }

  props(shared) <- shared_props
  granule_inherit_props(x, shared)
}