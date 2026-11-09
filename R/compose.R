#' Compose a linear time vector from linear and cyclical components
#'
#' `time_compose()` is the inverse of [time_components()]: given a set of
#' [lin()]/[cyc()] components it reconstructs the corresponding time points.
#' Each component is either a two-sided formula pairing a spec with its value,
#' or an already-tagged linear/cyclical time vector (e.g. produced by
#' [linear_time()], [cyclical_time()], or a [time_components()] column).
#'
#' A `lin()` component, when supplied, locates the time point at some granule
#' (e.g. the year). Every other component must be
#' `cyc()`, chaining without gaps or branches from the `lin()` down to the
#' target chronon: each cycle must equal another component's chronon exactly.
#'
#' With no `lin()` component, every component must be `cyc()`, chained the
#' same way but rooted at whichever component's cycle isn't itself another
#' component's chronon. The result is underspecified cyclical time (see
#' [cyclical_time()]) tagged with that root's cycle: `cyc(month, year) ~ 3` alone matches `month_of_year()` for any
#' March. Chained components collapse to one (chronon, cycle) offset, e.g.
#' `cyc(minute, hour) ~ 30` and `cyc(hour, day) ~ 3` give minute-of-day 149.
#' This is only possible when every link has a fixed length: the number of
#' days in a month varies, so day 29 of month 3 isn't a fixed day of the year
#' and is an error without a `lin()` component. Supply a `lin()` component (or
#' an existing linear vector) to locate it, e.g. `time_compose(lin(year) ~
#' 2024, cyc(month, year) ~ 3, cyc(day, month) ~ 29)`. Underspecified
#' components (e.g. [day_of_month()]) are then validated against the located
#' time and may be given in any order.
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
#' @param on_ambiguous How to resolve a time zone aware time point whose local
#'   time occurs twice (when daylight saving ends) and that `offset` doesn't
#'   resolve. One of `"earliest"` (the default), `"latest"`, `"reject"`
#'   (`NA` with a warning) or `"error"`. The default is set by the
#'   `mixtime.on_ambiguous` option. Without a strategy chosen by either, an
#'   ambiguous time point warns and resolves to its earliest occurrence.
#' @param offset The UTC offset of each time point, which tells apart the two
#'   occurrences of a repeated local time. Either a duration of fixed length
#'   (e.g. `hours(11L)`, or [tz_offset()] of the times the components came
#'   from), or time zone abbreviations of the components' time zone (e.g.
#'   `"AEDT"`). `NA` where not known. Each offset must be one in use at that
#'   local time. For a local time skipped when daylight saving starts, the
#'   offset gives the instant it describes (e.g. 02:30 AEST is 03:30 AEDT).
#' @inheritParams linear_time
#'
#' @section Time zones:
#' Time zone aware components are composed in local (wall clock) time, and the
#' result is then resolved to the instant it describes. When daylight saving
#' ends, part of the day repeats, and a local time in it is ambiguous. It is
#' resolved, in order, by:
#'
#' 1. a time zone aware `lin()` component (e.g. `lin(hour)` from
#'    [time_components()]), which already identifies one of the two
#'    occurrences;
#' 2. its `offset`;
#' 3. the `on_ambiguous` strategy, which warns if it wasn't chosen.
#'
#' When daylight saving starts, part of the day is skipped. A local time in it
#' is resolved by its `offset` if given, and otherwise by the
#' `mixtime.on_invalid` option, as in [time_shift()] (see
#' `vignette("invalid-time-points")`).
#'
#' @return A `mixtime` time vector, at the finest chronon reached by the
#'   chain (or the root's own chronon, if only one component is given).
#'   Linear with a `lin()` component; underspecified cyclical (`mt_cyclical_free`)
#'   otherwise, since there is then no point in time for the result to be a
#'   position within.
#'
#' @seealso [time_components()] for the inverse operation, [lin()]/[cyc()] for
#'   the component vocabulary shared with [time_components()] and [format()].
#'
#' @examples
#' # cyc() values are 1-indexed positions: month 3 is March, day 15 is the 15th
#' time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(day, month) ~ 15)
#'
#' # A lin() component alone is a valid (coarser) time point
#' time_compose(lin(year) ~ 1980)
#'
#' # No lin() component: composes underspecified cyclical time
#' time_compose(cyc(month, year) ~ 3)
#'
#' # Fixed-length chains collapse to one (chronon, cycle) pair: minute 30 of
#' # hour 3 becomes minute-of-day
#' time_compose(cyc(minute, hour) ~ 30, cyc(hour, day) ~ 3)
#'
#' # Variable-length links (days in a month) need a lin() component
#' time_compose(lin(year) ~ 2024, day_of_month(29), month_of_year(2))
#'
#' # Round-tripping through time_components()
#' parts <- time_components(as.Date("2024-03-15"), yr = lin(year), mth = cyc(month, year))
#' with(parts, time_compose(yr, mth))
#'
#' # Repeated local times when daylight saving ends: 02:30 occurs twice in
#' # Melbourne on 5 April 2026, first in AEDT (+11) then in AEST (+10)
#' x <- datetime(c("2026-04-05 01:30:00", "2026-04-05 03:30:00"), tz = "Australia/Melbourne")
#' x <- c(x[1] + hours(1L), x[2] - hours(1L))
#' parts <- time_components(x, d = lin(day), h = cyc(hour, day), m = cyc(minute, hour))
#' with(parts, time_compose(d, h, m, offset = tz_offset(x)))
#' with(parts, time_compose(d, h, m, offset = c("AEDT", "AEST")))
#' with(parts, time_compose(d, h, m, on_ambiguous = "latest"))
#'
#' # Multi-unit (self-referencing) cycles: the 3rd month (1-indexed) of the
#' # 4th 3-month block since epoch (block 3 = months 9-11 -> December 1970)
#' time_compose(lin(month(3L)) ~ 3, cyc(month(1L), month(3L)) ~ 3)
#'
#' @export
time_compose <- function(
  ...,
  discrete = TRUE,
  calendar = cal_gregorian,
  on_ambiguous = getOption("mixtime.on_ambiguous", "earliest"),
  offset = NULL
) {
  ambiguous_chosen <- !missing(on_ambiguous) ||
    !is.null(getOption("mixtime.on_ambiguous"))
  check_on_ambiguous(on_ambiguous)
  dots <- list(...)
  if (length(dots) == 0L) {
    cli::cli_abort(
      c(
        "Must supply at least one component to compose a time point.",
        i = "Use {.fn lin} to locate the time point, e.g. {.code lin(year) ~ 1980}.",
        i = "Use {.fn cyc} for the rest, e.g. {.code cyc(month, year) ~ 3}."
      ),
      call = NULL
    )
  }

  comps <- lapply(dots, compose_tag, calendar = calendar)

  sizes <- vctrs::vec_size_common(!!!lapply(comps, `[[`, "value"))
  comps <- lapply(comps, function(x) {
    x$value <- vctrs::vec_recycle(x$value, sizes)
    if (!is.null(x$absolute)) x$absolute <- vctrs::vec_recycle(x$absolute, sizes)
    x
  })

  chain <- compose_chain(comps)
  out <- compose_recompose(chain$chain, discrete = discrete, cycle = chain$cycle)
  out <- compose_from_local(
    out,
    root = chain$chain[[1L]],
    discrete = discrete,
    on_ambiguous = on_ambiguous,
    ambiguous_chosen = ambiguous_chosen,
    offset = offset
  )
  new_mixtime(out)
}

# Turn one `...` element into a normalized component: a plain
# list(chronon, cycle, value), where `cycle` is NULL for a lin()
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
    # In element order (the slot itself may be stored in another order)
    x <- vecvec::unvecvec(x)
  }
  # Components are composed on the local (wall clock) time line, which is what
  # the context-blind chronon_divmod() graph works on. Time zone aware times
  # store absolute (UTC based) counts, so they're read as local labels here and
  # resolved back to absolute time once composed (see compose_from_local()).
  if (S7_inherits(x, mt_linear)) {
    chronon <- attr(x, "chronon")
    stored <- vec_data(x)
    return(list(
      chronon = chronon,
      cycle = NULL,
      value = chronon_to_local(chronon, stored),
      # The absolute count, which tells apart repeated local times
      absolute = if (!chronon_is_local(chronon)) stored
    ))
  }
  if (S7_inherits(x, mt_cyclical)) {
    chronon <- attr(x, "chronon")
    cycle <- attr(x, "cycle")
    # A real mt_cyclical stores the same absolute, epoch-relative count as the
    # equivalent mt_linear (see time_components()); reduce it to the raw
    # within-cycle offset the same way chronon_parts() does, so it matches a
    # formula-derived component's convention.
    offset <- chronon_divmod(
      from = chronon,
      to = cycle,
      x = chronon_to_local(chronon, vec_data(x))
    )$mod
    return(list(chronon = chronon, cycle = cycle, value = offset))
  }
  if (S7_inherits(x, mt_cyclical_free)) {
    # Already a bare within-cycle offset - pass through, no chronon_divmod().
    return(list(chronon = attr(x, "chronon"), cycle = attr(x, "cycle"), value = vec_data(x)))
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
# list; cycle is NULL for a lin() root, or the root's cycle for cyclical
# time (no lin() given).
#
# A lin() component, if supplied, must be the only one; every cyc() then chains
# from it via exact chronon equality, without gaps or branches. With no
# lin(), every component must be cyc(), rooted at whichever one's cycle
# isn't supplied as another component's chronon; its own within-cycle
# offset seeds the chain as an absolute count (mt_cyclical's convention,
# see compose_tag()).
# `fixed_hint` replaces the fix suggested when a chain without lin() has a
# variable-length link (see compose_check_fixed()).
compose_chain <- function(comps, fixed_hint = NULL) {
  is_lin <- vapply(comps, function(x) is.null(x$cycle), logical(1L))
  n_lin <- sum(is_lin)

  if (n_lin > 1L) {
    labels <- vapply(
      comps[is_lin],
      function(x) time_granule_label(x$chronon),
      character(1L)
    )
    cli::cli_abort(
      c(
        "{.fn time_compose} needs at most one {.fn lin} component, but {n_lin} were supplied ({paste(labels, collapse = ', ')}).",
        i = "Keep one {.fn lin} and express the rest as {.fn cyc} components relative to it."
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
          "{.fn time_compose} needs a {.fn lin} component, or a {.fn cyc} chain with a coarsest link, but every {.fn cyc} component's cycle is itself another's chronon.",
          i = "Add a {.fn lin} component, or drop one of the components."
        ),
        call = NULL
      )
    }
    if (n_root > 1L) {
      labels <- vapply(
        comps[is_root],
        function(x) paste0(time_granule_label(x$chronon), "/", time_granule_label(x$cycle)),
        character(1L)
      )
      cli::cli_abort(
        c(
          "Without a {.fn lin} component, {.fn time_compose} needs a single connected {.fn cyc} chain, but {n_root} disconnected chain{?s} were supplied ({paste(labels, collapse = ', ')}).",
          i = "Add a {.fn lin} component to combine them, or keep only one chain."
        ),
        call = NULL
      )
    }

    root_cycle <- comps[[which(is_root)]]$cycle
  }

  root <- comps[[which(is_root)]]
  links <- comps[!is_root]
  used <- logical(length(links))

  chain <- list(root)
  frontier <- root$chronon

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
        function(x) time_granule_label(x$chronon),
        character(1L)
      )
      cli::cli_abort(
        c(
          "Multiple {.fn cyc} components build from the same {time_granule_label(frontier)} chronon: {paste(labels, collapse = ', ')}.",
          i = "Supply only one path from the coarsest component to the target granularity."
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
          time_granule_label(x$chronon),
          "/",
          time_granule_label(x$cycle)
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

  # Without a lin() there is no enclosing cycle to resolve a variable-length link
  # against, so collapsing the chain into one offset is only valid when every
  # link below the root has a fixed length.
  if (!is.null(root_cycle)) {
    compose_check_fixed(chain, fixed_hint)
  }

  list(chain = chain, cycle = root_cycle)
}

# Abort if any link (below the root) of a chain without lin() has a
# context-dependent length, e.g. days in a month.
compose_check_fixed <- function(chain, hint = NULL) {
  hint <- hint %||% c(
    i = "Locate it with a {.fn lin} component, e.g. {.code time_compose(lin(year) ~ 2024, cyc(month, year) ~ 3, cyc(day, month) ~ 29)}.",
    i = "Or use an existing linear vector, e.g. {.code time_compose(yearmonth(\"2024-03\"), day_of_month(29))}."
  )
  prev <- chain[[1L]]$chronon
  for (link in chain[-1L]) {
    if (!chronon_cardinality_is_fixed(link$chronon, prev)) {
      cur <- sub("^1 ", "", time_granule_label(link$chronon))
      par <- sub("^1 ", "", time_granule_label(prev))
      cli::cli_abort(
        c(
          "Can't collapse {cur}-of-{par} without a {.fn lin} component: the number of {cur} granules in a {par} varies, so the position isn't a fixed offset in the larger cycle.",
          hint
        ),
        call = NULL
      )
    }
    prev <- link$chronon
  }
  invisible(chain)
}

# Walk the validated chain coarse -> fine, reconstructing the absolute count
# at each link from the base of its coarser period (chronon_divmod()'s
# coarse -> fine `div`, which is always an exact base with no remainder) plus
# the link's own within-cycle offset.
#
# `chain[[1L]]` seeds `running_count`/`running_chronon` directly, whether it
# is a lin() component (`cycle` NULL) or, for cyclical time, the chain's root
# cyc() component (see compose_chain()). `cycle` then tags the result
# mt_cyclical instead of mt_linear.
#
# `strict = FALSE` produces NA for invalid rows rather than aborting
compose_recompose <- function(chain, discrete, cycle = NULL, strict = TRUE) {
  root <- chain[[1L]]
  running_chronon <- root$chronon
  running_count <- root$value
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
          "{.val {offset_i[keep][bad_i] + 1L}} is not a valid position for {time_granule_label(chronon_i)} granules within {time_granule_label(running_chronon)}.",
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
  if (is.null(cycle)) {
    mt_linear(running_count, chronon = running_chronon)
  } else {
    # No lin() - an underspecified position (mt_cyclical_free), not
    # mt_cyclical. Its constructor drops any time zone.
    mt_cyclical_free(running_count, chronon = running_chronon, cycle = cycle)
  }
}

on_ambiguous_strategies <- c("earliest", "latest", "reject", "error")

# Validate an `on_ambiguous` strategy, which defaults to the
# `mixtime.on_ambiguous` option.
check_on_ambiguous <- function(on_ambiguous, call = rlang::caller_env()) {
  if (!(rlang::is_string(on_ambiguous) && on_ambiguous %in% on_ambiguous_strategies)) {
    cli::cli_abort(
      c(
        "{.arg on_ambiguous} must be one of {.or {.val {on_ambiguous_strategies}}}.",
        "i" = "The default is set by the {.code mixtime.on_ambiguous} option."
      ),
      call = call
    )
  }
  on_ambiguous
}

# Resolve a composed time `x`, counted on the local (wall clock) time line, to
# the absolute counts its time zone aware chronon stores. A local time that
# occurs twice (when daylight saving ends) is resolved, in order, by:
#
# 1. the absolute count of a time zone aware lin() `root` (e.g. a
#    time_components() column), when it falls in only one of the two;
# 2. the UTC `offset` (or time zone abbreviation) given for it;
# 3. the `on_ambiguous` strategy, warning when no strategy was chosen.
#
# A local time that doesn't occur (when daylight saving starts) is the instant
# its `offset` gives, if one is given, and is otherwise resolved like other
# invalid time points by the `mixtime.on_invalid` option.
compose_from_local <- function(
  x,
  root,
  discrete,
  on_ambiguous,
  ambiguous_chosen,
  offset
) {
  chronon <- attr(x, "chronon")
  if (!S7_inherits(chronon, mt_tz_unit) || chronon_is_local(chronon)) {
    if (!is.null(offset)) {
      cli::cli_abort(
        c(
          "{.arg offset} can only be used when composing time zone aware time.",
          "i" = "The composed time has no UTC offsets, so it has no repeated or skipped local times."
        ),
        call = NULL
      )
    }
    return(x)
  }

  local <- vec_data(x)
  n <- length(local)
  ch <- tz_local_candidates(chronon, local)
  i <- pmin(ch$first, ch$second, na.rm = TRUE)
  unresolved <- ch$ambiguous

  # 1. The occurrence that the lin() root's absolute count falls in
  if (!is.null(root$absolute) && any(unresolved)) {
    k <- which(unresolved)
    in_root <- function(b) {
      chronon_convert_impl(b, chronon, root$chronon, discrete = TRUE)
    }
    a <- floor_ulp(root$absolute[k])
    first_ok <- (in_root(ch$first[k]) == a) %in% TRUE
    second_ok <- (in_root(ch$second[k]) == a) %in% TRUE
    pick <- xor(first_ok, second_ok)
    i[k[pick & second_ok]] <- ch$second[k[pick & second_ok]]
    unresolved[k[pick]] <- FALSE
  }

  # 2. The occurrence with the given UTC offset
  literal <- rep_len(FALSE, n)
  literal_value <- NULL
  if (!is.null(offset)) {
    off <- compose_offset(offset, n, chronon, ch)
    given <- !is.na(off)
    is_first <- given & (off == ch$before) %in% TRUE & !is.na(ch$first)
    # (an offset matching neither is a mismatch, reported below)
    is_second <- given & !is_first & (off == ch$after) %in% TRUE & !is.na(ch$second)
    # A skipped local time with an offset in use either side of the gap is the
    # instant that offset gives (e.g. 02:30 AEDT is 03:30 AEST).
    literal <- given & ch$gap & (off == ch$before | off == ch$after) %in% TRUE
    mismatch <- given & !(is_first | is_second | literal)
    if (any(mismatch)) {
      compose_abort_offset(offset, mismatch, chronon)
    }
    i[is_second] <- ch$second[is_second]
    unresolved[given] <- FALSE
    if (any(literal)) {
      s <- chronon_convert_graph(local[literal], chronon, tu_utc_second()) - off[literal]
      literal_value <- chronon_from_utc(chronon, s, discrete)
    }
  }

  # 3. The `on_ambiguous` strategy
  if (any(unresolved)) {
    if (on_ambiguous == "latest") {
      i[unresolved] <- pmax(ch$first, ch$second)[unresolved]
    } else if (on_ambiguous %in% c("reject", "error")) {
      i[unresolved] <- NA
    }
    compose_signal_ambiguous(unresolved, on_ambiguous, ambiguous_chosen, chronon@tz)
  }

  on_invalid <- getOption("mixtime.on_invalid", "clamp")
  ch$gap <- ch$gap & !literal
  resolved <- tz_local_value(chronon, local, i, ch, discrete, on_invalid)
  value <- resolved$value
  value[literal] <- literal_value
  signal_invalid(resolved$invalid, on_invalid)

  if (discrete && all(is.na(value) | is.finite(value))) {
    value <- as.integer(value)
  }
  if (S7_inherits(x, mt_cyclical)) {
    mt_cyclical(value, chronon = chronon, cycle = attr(x, "cycle"))
  } else {
    mt_linear(value, chronon = chronon)
  }
}

# The UTC offset (seconds) given by `offset` for each of `n` composed times:
# durations (e.g. `hours(11L)` or `tz_offset(x)`), or time zone abbreviations
# (e.g. "AEDT") of `chronon`'s time zone at those times. NA where not given.
compose_offset <- function(offset, n, chronon, ch) {
  if (is.character(offset)) {
    abbr <- vctrs::vec_recycle(offset, n, x_arg = "offset")
    off <- rep(NA_real_, n)
    k <- which(!is.na(abbr) & !is.na(ch$local))
    if (length(k)) {
      tz <- rep(chronon@tz, length(k))
      # Abbreviations either side of any nearby change, read where
      # tz_offsets_around() reads the offsets `before` and `after` it.
      abbr_before <- get_tz_abbreviation(ch$local[k] - 172800, tz)
      abbr_after <- get_tz_abbreviation(ch$local[k] + 172800, tz)
      # An abbreviation used on both sides of a change doesn't say which side
      same <- abbr_before == abbr_after & ch$before[k] != ch$after[k] &
        abbr[k] == abbr_before & (ch$ambiguous[k] | ch$gap[k])
      if (any(same)) {
        cli::cli_abort(
          c(
            "Time zone abbreviation {.val {unique(abbr[k][same])}} is used on both sides of a UTC offset change in {.val {chronon@tz}}, so it can't tell repeated local times apart.",
            "i" = "At {cli::qty(sum(same))}position{?s} {k[same]}.",
            ">" = "Supply {.arg offset} as a duration instead, e.g. {.code offset = tz_offset(x)} or {.code offset = hours(10L)}."
          ),
          call = NULL
        )
      }
      off[k] <- ifelse(
        abbr[k] == abbr_before,
        ch$before[k],
        # An abbreviation not in use there matches no offset (see mismatch)
        ifelse(abbr[k] == abbr_after, ch$after[k], Inf)
      )
    }
    return(off)
  }

  if (is_mixtime(offset) && all(vapply(offset@x, S7_inherits, logical(1L), mt_duration))) {
    offset <- vctrs::vec_recycle(offset, n, x_arg = "offset")
    # An offset is a length of time, whatever time zone its unit has (e.g. the
    # seconds of tz_offset())
    unit <- chronon_glb(offset)
    if (S7_inherits(unit, mt_tz_unit)) unit@tz <- NA_character_
    second <- cal_gregorian$second(1L)
    seconds <- if (chronon_cardinality_is_fixed(second, unit)) {
      as.numeric(chronon_cardinality(second, unit))
    } else {
      NA_real_
    }
    if (is.na(seconds)) {
      cli::cli_abort(
        "{.arg offset} must be a duration of fixed length, such as hours or minutes.",
        call = NULL
      )
    }
    return(as.numeric(offset) * seconds)
  }

  cli::cli_abort(
    c(
      "{.arg offset} must be a duration or time zone abbreviations.",
      "i" = "For example {.code offset = tz_offset(x)}, {.code offset = hours(11L)} or {.code offset = \"AEDT\"}."
    ),
    call = NULL
  )
}

compose_abort_offset <- function(offset, mismatch, chronon) {
  k <- which(mismatch)
  given <- if (is.character(offset)) {
    vctrs::vec_recycle(offset, length(mismatch))[k]
  } else {
    format(vctrs::vec_recycle(offset, length(mismatch))[k])
  }
  cli::cli_abort(
    c(
      "{.arg offset} doesn't match the UTC offset of {length(k)} composed time{?s} in time zone {.val {chronon@tz}}.",
      "x" = "At {cli::qty(length(k))}position{?s} {paste0(utils::head(k, 5L), ' (', utils::head(given, 5L), ')')}{if (length(k) > 5L) ', ...' else ''}.",
      "i" = "Each offset must be one in use in that time zone at that local time."
    ),
    call = NULL
  )
}

compose_signal_ambiguous <- function(ambiguous, on_ambiguous, chosen, tz) {
  n <- sum(ambiguous)
  pos <- which(ambiguous)
  where <- "{cli::qty(n)}{?It occurs/They occur} twice, in local time repeated by a time zone change (e.g. when daylight saving ends), at {cli::qty(n)}position{?s} {utils::head(pos, 5L)}{if (n > 5L) ', ...' else ''}."
  avoid <- c(
    "i" = "To avoid the ambiguity:",
    "*" = "Supply the UTC offset or time zone abbreviation of each time with {.arg offset}, e.g. {.code offset = tz_offset(x)} or {.code offset = c(\"AEDT\", \"AEST\")}.",
    "*" = "Include a time zone aware {.fn lin} component finer than the repeated time, such as {.code lin(hour)} from {.fn time_components}.",
    "*" = "Compose in UTC, or a time zone without daylight saving."
  )
  if (on_ambiguous == "error") {
    cli::cli_abort(
      c("{n} composed time{?s} {?is/are} ambiguous in time zone {.val {tz}}.", "i" = where, avoid),
      class = "mixtime_ambiguous_time",
      call = NULL
    )
  }
  if (on_ambiguous == "reject") {
    cli::cli_warn(
      c("{n} composed time{?s} {?is/are} ambiguous in time zone {.val {tz}} and {cli::qty(n)}{?was/were} replaced with {.val NA}.", "i" = where, avoid),
      class = "mixtime_ambiguous_time",
      call = NULL
    )
  }
  if (!chosen) {
    cli::cli_warn(
      c(
        "{n} composed time{?s} {?is/are} ambiguous in time zone {.val {tz}} and {cli::qty(n)}{?was/were} resolved to {cli::qty(n)}{?its/their} earliest occurrence.",
        "i" = where,
        avoid,
        "*" = "Choose a strategy with {.arg on_ambiguous} (or the {.code mixtime.on_ambiguous} option) to silence this warning."
      ),
      class = "mixtime_ambiguous_time",
      call = NULL
    )
  }
}
