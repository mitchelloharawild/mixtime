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

# Are `x` and `y` the exact same chronon - same class and same property
# values? Used to detect a pure identity conversion in
# chronon_convert_impl(). `identical()` alone is too strict for this: S7's
# `class_numeric` doesn't normalise storage mode, so a numeric property (e.g.
# `n`) can be an integer on one side and a double on the other even when the
# chronons are otherwise the same - `identical()` would then treat two
# genuinely identical chronons as different.
granule_identical <- function(x, y) {
  if (!identical(S7_class_id(x), S7_class_id(y))) {
    return(FALSE)
  }
  props_equal(props(x), props(y))
}

# Are the named property lists `x` and `y` the same, treating integer and
# double values as equal? Classes are kept, so a naive property (mt_naive)
# differs from an explicit one with the same value unless both are numeric.
props_equal <- function(x, y) {
  if (!identical(names(x), names(y))) {
    return(FALSE)
  }
  for (nm in names(x)) {
    a <- x[[nm]]
    b <- y[[nm]]
    same <- if (is.numeric(a) && is.numeric(b)) {
      identical(as.double(a), as.double(b))
    } else {
      identical(a, b)
    }
    if (!isTRUE(same)) {
      return(FALSE)
    }
  }
  TRUE
}

# Convert a chronon's *default*-naive properties (the classed `mt_naive`
# sentinel, e.g. an unset `tz`) into an explicit NA of the same underlying
# type (e.g. `datetime(x, tz = NA)`'s naive result)
granule_harden_naive <- function(x) {
  props <- props(x)
  naive_props <- is.naive(props)
  props[naive_props] <- lapply(props[naive_props], unclass)
  props(x) <- props
  x
}
