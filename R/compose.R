#' Compose a linear time vector from linear and cyclical components
#'
#' `time_compose()` is the inverse of [time_components()]: given a set of
#' [lin()]/[cyc()] components it reconstructs the corresponding time points.
#' Each component is either a two-sided formula pairing a spec with its value,
#' or an already-tagged linear/cyclical time vector (e.g. produced by
#' [linear_time()], [cyclical_time()], or a [time_components()] column).
#'
#' A `lin()` component (the **anchor**), when supplied, fixes the absolute
#' position at some granule (e.g. the year). Every other component must be
#' `cyc()`, chaining without gaps or branches from the anchor down to the
#' target chronon: each cycle must equal another component's chronon exactly.
#'
#' With no `lin()` anchor, every component must be `cyc()`, chained the same
#' way but rooted at whichever component's cycle isn't itself another
#' component's chronon. The result is cyclical time tagged with that root's
#' cycle: `cyc(month, year) ~ 3` alone matches `month_of_year()` for any
#' March; chaining `cyc(day, month) ~ 15` onto it collapses to one (day,
#' year) pair, day-of-year 74, matching `day_of_year()` for 15 March.
#'
#' Values of linear and cyclical components are specified on the right-hand-side
#' of the formula. A `lin()` value is the real-world count (e.g. the literal
#' year 1980); a `cyc()` value is the 1-indexed position within the cycle
#' (e.g. `cyc(month, year) ~ 3` is the 3rd month, March), matching everyday
#' counting rather than the raw 0-indexed position [time_components()] uses
#' internally.
#'
#' @param ... Components used to build the time point. Each element is either:
#'   * a two-sided formula, `lin(<granule>) ~ <value>` or
#'   `cyc(<granule>, <cycle>) ~ <value>` (see [lin()]/[cyc()]), or
#'   * an existing linear or cyclical `mixtime` vector.
#' @param calendar Calendar used to resolve bare granule names in `lin()`/
#'   `cyc()` formulas. Defaults to [cal_gregorian].
#' @inheritParams linear_time
#'
#' @return A `mixtime` time vector, at the finest chronon reached by the
#'   chain (or the root's own chronon, if only one component is given).
#'   Linear with a `lin()` anchor, cyclical otherwise.
#'
#' @seealso [time_components()] for the inverse operation, [lin()]/[cyc()] for
#'   the component vocabulary shared with [time_components()] and [format()].
#'
#' @examples
#' # cyc() values are 1-indexed positions: month 3 is March, day 15 is the 15th
#' time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(day, month) ~ 15)
#'
#' # A lin() anchor alone is a valid (coarser) time point
#' time_compose(lin(year) ~ 1980)
#'
#' # No lin() anchor: composes cyclical time
#' time_compose(cyc(month, year) ~ 3)
#'
#' # Chaining collapses to one (chronon, cycle) pair: day 15 of month 3
#' # becomes day-of-year 74
#' time_compose(cyc(day, month) ~ 15, cyc(month, year) ~ 3)
#'
#' # Round-tripping through time_components()
#' parts <- time_components(as.Date("2024-03-15"), yr = lin(year), mth = cyc(month, year))
#' with(parts, time_compose(yr, mth))
#'
#' # Multi-unit (self-referencing) cycles: the 3rd month (1-indexed) of the
#' # 4th 3-month block since epoch (block 3 = months 9-11 -> December 1970)
#' time_compose(lin(month(3L)) ~ 3, cyc(month(1L), month(3L)) ~ 3)
#'
#' @export
time_compose <- function(..., discrete = TRUE, calendar = cal_gregorian) {
  dots <- list(...)
  if (length(dots) == 0L) {
    cli::cli_abort(
      c(
        "Must supply at least one component to compose a time point.",
        i = "Use {.fn lin} for the anchor, e.g. {.code lin(year) ~ 1980}.",
        i = "Use {.fn cyc} for the rest, e.g. {.code cyc(month, year) ~ 3}."
      ),
      call = NULL
    )
  }

  comps <- lapply(dots, compose_tag, calendar = calendar)

  sizes <- vctrs::vec_size_common(!!!lapply(comps, `[[`, "value"))
  comps <- lapply(comps, function(x) {
    x$value <- vctrs::vec_recycle(x$value, sizes)
    x
  })

  chain <- compose_chain(comps)
  compose_recompose(chain$chain, discrete = discrete, cycle = chain$cycle)
}

# Turn one `...` element into a normalized component: a plain
# list(chronon, cycle, value), where `cycle` is NULL for a linear (anchor)
# component, and `value` is always the *raw, 0-indexed within-cycle offset*
# for a cyclical component (never the absolute epoch-relative count
# mt_cyclical itself stores, and never the formula's 1-indexed input) -- this
# is the one convention chain-walking and the already-tagged and
# formula-derived inputs all agree on.
#
# Already-tagged mixtime input passes through (reduced to this convention);
# a `spec ~ value` formula is evaluated and tagged.
compose_tag <- function(x, calendar) {
  if (S7_inherits(x, class_mixtime)) {
    if (length(x@x) != 1L) {
      cli::cli_abort(
        c(
          "Each {.fn time_compose} component must have a single, consistent chronon.",
          i = "This one mixes {length(x@x)} different chronons; split it first."
        ),
        call = NULL
      )
    }
    x <- x@x[[1L]]
  }
  if (S7_inherits(x, mt_linear)) {
    return(list(
      chronon = attr(x, "chronon"),
      cycle = NULL,
      value = vec_data(x)
    ))
  }
  if (S7_inherits(x, mt_cyclical)) {
    chronon <- attr(x, "chronon")
    cycle <- attr(x, "cycle")
    # A real mt_cyclical stores the same absolute, epoch-relative count as the
    # equivalent mt_linear (see time_components()); reduce it to the raw
    # within-cycle offset the same way chronon_parts() does, so it matches a
    # formula-derived component's convention.
    offset <- chronon_divmod(from = chronon, to = cycle, x = vec_data(x))$mod
    return(list(chronon = chronon, cycle = cycle, value = offset))
  }

  if (!inherits(x, "formula") || length(x) != 3L) {
    cli::cli_abort(
      c(
        "Each {.fn time_compose} component must be a {.fn lin}/{.fn cyc} formula or an existing linear/cyclical time vector.",
        i = "e.g. {.code lin(year) ~ 1980} or {.code cyc(month, year) ~ 3}."
      ),
      call = NULL
    )
  }

  spec <- eval_tidy(
    rlang::f_lhs(x),
    data = component_mask(calendar),
    env = rlang::f_env(x)
  )
  value <- eval(rlang::f_rhs(x), rlang::f_env(x))

  if (length(spec) == 1L) {
    # The value is the real-world count (e.g. the literal year 1980);
    # chronon_epoch() gives the same origin offset mixtime() itself applies
    # to reach the raw, epoch-relative count.
    list(
      chronon = spec[[1L]],
      cycle = NULL,
      value = value - chronon_epoch(spec[[1L]])
    )
  } else {
    # The value is the 1-indexed position within the cycle (e.g. 3 is the
    # 3rd month, matching everyday counting); shift to the raw 0-indexed
    # offset chronon_parts()/time_components() use internally.
    list(chronon = spec[[1L]], cycle = spec[[2L]], value = value - 1L)
  }
}

# Returns list(chain, cycle): chain is the ordered root-to-finest component
# list; cycle is NULL for a lin() anchor, or the root's cycle for cyclical
# time (no lin() given).
#
# A lin() anchor, if supplied, must be the only one; every cyc() then chains
# from it via exact chronon equality, without gaps or branches. With no
# lin(), every component must be cyc(), rooted at whichever one's cycle
# isn't supplied as another component's chronon; its own within-cycle
# offset seeds the chain as an absolute count (mt_cyclical's convention,
# see compose_tag()).
compose_chain <- function(comps) {
  is_lin <- vapply(comps, function(x) is.null(x$cycle), logical(1L))
  n_lin <- sum(is_lin)

  if (n_lin > 1L) {
    labels <- vapply(
      comps[is_lin],
      function(x) time_granule_label(x$chronon, 1),
      character(1L)
    )
    cli::cli_abort(
      c(
        "{.fn time_compose} needs exactly one {.fn lin} anchor, but {n_lin} were supplied ({paste(labels, collapse = ', ')}).",
        i = "Keep one as the anchor and express the rest as {.fn cyc} components relative to it."
      ),
      call = NULL
    )
  }

  if (n_lin == 1L) {
    is_root <- is_lin
    root_cycle <- NULL
  } else {
    supplied_chronons <- lapply(comps, `[[`, "chronon")
    is_root <- vapply(
      comps,
      function(x) !any(vapply(supplied_chronons, identical, logical(1L), y = x$cycle)),
      logical(1L)
    )
    n_root <- sum(is_root)

    if (n_root == 0L) {
      cli::cli_abort(
        c(
          "{.fn time_compose} needs a {.fn lin} anchor, or a {.fn cyc} chain with a coarsest link, but every {.fn cyc} component's cycle is itself another's chronon.",
          i = "Add a {.fn lin} anchor, or drop one of the components."
        ),
        call = NULL
      )
    }
    if (n_root > 1L) {
      labels <- vapply(
        comps[is_root],
        function(x) paste0(time_granule_label(x$chronon, 1), "/", time_granule_label(x$cycle, 1)),
        character(1L)
      )
      cli::cli_abort(
        c(
          "Without a {.fn lin} anchor, {.fn time_compose} needs a single connected {.fn cyc} chain, but {n_root} disconnected chain{?s} were supplied ({paste(labels, collapse = ', ')}).",
          i = "Add a {.fn lin} anchor to combine them, or keep only one chain."
        ),
        call = NULL
      )
    }

    root_cycle <- comps[[which(is_root)]]$cycle
  }

  anchor <- comps[[which(is_root)]]
  links <- comps[!is_root]
  used <- logical(length(links))

  chain <- list(anchor)
  frontier <- anchor$chronon

  repeat {
    matches <- which(
      !used &
        vapply(links, function(l) identical(l$cycle, frontier), logical(1L))
    )
    if (length(matches) == 0L) {
      break
    }
    if (length(matches) > 1L) {
      labels <- vapply(
        links[matches],
        function(x) time_granule_label(x$chronon, 1),
        character(1L)
      )
      cli::cli_abort(
        c(
          "Multiple {.fn cyc} components build from the same {time_granule_label(frontier, 1)}: {paste(labels, collapse = ', ')}.",
          i = "Supply only one path from the anchor to the target granularity."
        ),
        call = NULL
      )
    }
    used[matches] <- TRUE
    chain[[length(chain) + 1L]] <- links[[matches]]
    frontier <- links[[matches]]$chronon
  }

  if (!all(used)) {
    labels <- vapply(
      links[!used],
      function(x) {
        paste0(
          time_granule_label(x$chronon, 1),
          "/",
          time_granule_label(x$cycle, 1)
        )
      },
      character(1L)
    )
    cli::cli_abort(
      c(
        "The following {.fn cyc} component{?s} do{?es/} not connect to the other supplied components: {paste(labels, collapse = ', ')}.",
        i = "Each {.fn cyc}'s cycle must exactly match another component's chronon (same unit and size)."
      ),
      call = NULL
    )
  }

  list(chain = chain, cycle = root_cycle)
}

# Walk the validated chain coarse -> fine, reconstructing the absolute count
# at each link from the base of its coarser period (chronon_divmod()'s
# coarse -> fine `div`, which is always an exact base with no remainder) plus
# the link's own within-cycle offset.
#
# `chain[[1L]]` seeds `running_count`/`running_chronon` directly, whether it
# is a lin() anchor (`cycle` NULL) or, for cyclical time, the chain's root
# cyc() component (see compose_chain()). `cycle` then tags the result
# mt_cyclical instead of mt_linear via mixtime().
#
# `strict = FALSE` produces NA for invalid rows rather than aborting
compose_recompose <- function(chain, discrete, cycle = NULL, strict = TRUE) {
  anchor <- chain[[1L]]
  running_chronon <- anchor$chronon
  running_count <- anchor$value
  keep <- !(is.na(running_count) | is.infinite(running_count))

  for (link in chain[-1L]) {
    chronon_i <- link$chronon

    base_i <- chronon_divmod(
      from = running_chronon,
      to = chronon_i,
      x = running_count[keep]
    )$div

    # `link$value` is usually a plain offset vector (from a `spec ~ value`
    # formula, or an already-tagged mixtime component). time_parse() instead
    # supplies a closure `function(at) -> offset` for a decoded cyc() token,
    # since decoding it may depend on `at` -- the coarser instance's resolved
    # position -- which has only just become available above. `at` is built
    # full-length (NA outside `keep`) so the closure sees the same shape a
    # plain offset vector would, and everything below stays unchanged.
    offset_i <- if (is.function(link$value)) {
      at <- rep(NA_real_, length(running_count))
      at[keep] <- base_i
      link$value(at)
    } else {
      link$value
    }

    # `at` is the raw, epoch-relative count in terms of the coarser
    # (running) chronon -- the same convention chronon_cardinality() methods
    # use everywhere else (e.g. compare.R, predicates.R), so no mixtime()
    # round-trip is needed here.
    cardinality <- chronon_cardinality(
      chronon_i,
      running_chronon,
      at = running_count[keep]
    )
    bad <- offset_i[keep] < 0 | offset_i[keep] >= cardinality
    if (any(bad, na.rm = TRUE)) {
      if (!strict) {
        # Drop the invalid rows out of the chain quietly as NA
        bad[is.na(bad)] <- TRUE
        idx <- which(keep)

        new_count <- running_count
        new_count[idx[!bad]] <- base_i[!bad] + offset_i[keep][!bad]
        new_count[idx[bad]] <- NA_real_
        running_count <- new_count
        keep[idx[bad]] <- FALSE
        running_chronon <- chronon_i
        next
      }
      bad_i <- which(bad)[1L]
      cli::cli_abort(
        c(
          "{.val {offset_i[keep][bad_i] + 1L}} is not a valid {time_granule_label(chronon_i, 1)} within that {time_granule_label(running_chronon, 1)}.",
          i = "Valid values are {.val {1}} to {.val {cardinality[bad_i]}}."
        ),
        call = NULL
      )
    }

    new_count <- running_count
    new_count[keep] <- base_i + offset_i[keep]
    running_count <- new_count
    running_chronon <- chronon_i
  }

  if (discrete) {
    running_count <- round(running_count)
    # Integer storage can't hold +/-Inf (unlike double), so only narrow when
    # nothing special remains -- matches how e.g. year(c(2020, Inf)) itself
    # keeps a special-containing discrete vector as double.
    if (all(is.na(running_count) | is.finite(running_count))) {
      running_count <- as.integer(running_count)
    }
  }
  mixtime(
    running_count + chronon_epoch(running_chronon),
    chronon = running_chronon,
    cycle = cycle,
    discrete = discrete
  )
}
