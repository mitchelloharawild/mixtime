#' Extract linear and cyclical time components
#'
#' `time_components()` decomposes a time vector into its constituent parts using
#' `dplyr::mutate()`-like semantics. Each named expression is built from the
#' [lin()] and [cyc()] helpers (the same vocabulary used in [format()] strings)
#' and produces a component time vector:
#'
#' - `lin(<granule>)` extracts a **linear** component (a non-repeating count,
#'   e.g. the year), returning a linear time vector.
#' - `cyc(<granule>, <cycle>)` extracts a **cyclical** component (a repeating
#'   position within a larger cycle, e.g. the month within the year), returning a
#'   cyclical time vector.
#'
#' All requested components are computed together in a single decomposition of
#' the underlying time vector (via `chronon_parts()`), reusing the shared
#' recursive `chronon_divmod()` results rather than converting each component
#' independently.
#'
#' @param x A `mixtime` (or an object coercible to one via [as_mixtime()], such
#'   as a `Date` or `POSIXct`).
#' @param ... Named expressions using [lin()] and [cyc()] describing the
#'   components to extract. The granule names (e.g. `year`, `month`, `day`) are
#'   resolved in the calendar of `x`.
#' @param calendar Calendar system used to resolve granule names, overlaid on the
#'   calendar of `x`. Defaults to `time_calendar(x)`. Supply e.g. [cal_isoweek]
#'   to make ISO `week`-based components available.
#'
#' @return A data frame with one column per requested component. `lin()` columns
#'   are linear (`mt_linear`) time vectors and `cyc()` columns are cyclical
#'   (`mt_cyclical`) time vectors.
#'
#' @seealso [lin()] and [cyc()] for the component helpers, [linear_time()] and
#'   [cyclical_time()] for constructing individual component vectors, and
#'   [format()] for the string counterpart of this interface.
#'
#' @examples
#' t <- yearmonth(as.Date("2026-02-14") + c(0, 40, 400))
#'
#' # Extract the year (linear) and month-of-year (cyclical)
#' time_components(t, yr = lin(year), mth = cyc(month, year))
#'
#' # Components can be named automatically from the expression
#' time_components(as.Date("2025-12-15") + 0:3, cyc(day, cal_isoweek$week))
#'
#' @export
time_components <- function(x, ..., calendar = time_calendar(x)) {
  specs <- enquos(..., .named = TRUE)
  if (length(specs) == 0L) {
    cli::cli_abort(
      c(
        "Must supply at least one component to extract.",
        i = "Use {.fn lin} for linear components, e.g. {.code lin(year)}.",
        i = "Use {.fn cyc} for cyclical components, e.g. {.code cyc(month, year)}."
      ),
      call = NULL
    )
  }

  x <- as_mixtime(x)

  # Decompose each underlying (uni-typed) mt_time part once, computing every
  # requested component together via a single chronon_parts() traversal.
  decomp <- lapply(x@x, component_parts, specs = specs, calendar = calendar)

  # Rebuild each component as a mixtime column, reusing x's ordering index by
  # swapping the vecvec parts (each decomposed part matches the original length).
  cols <- lapply(seq_along(specs), function(j) {
    col <- x
    col@x <- lapply(decomp, `[[`, j)
    col
  })

  vctrs::new_data_frame(
    rlang::set_names(cols, names(specs)),
    class = c("tbl_df", "tbl")
  )
}

#' Linear, cyclical and duration component helpers
#'
#' `lin()`, `cyc()` and `dur()` name the time components addressed by mixtime's
#' *component-aware contexts* — a single vocabulary shared across [format()] (and
#' parsing) format strings and [time_components()] expressions. They are only
#' meaningful inside one of these contexts; calling them directly is an error.
#'
#' A linear and a cyclical component of the same granule store the *same* value:
#' the count of that chronon since the Unix epoch. The reduction to a within-cycle
#' position happens only when a cyclical vector is formatted. `cyc()` therefore
#' behaves like `lin()` on its finest granule but additionally records the cycle.
#'
#' `dur()` addresses a *duration* (an unanchored span of time, see [duration()])
#' counted in units of a granule, e.g. `"{dur(day)}"` formats and parses text
#' like `"3 days"`. A duration has no position in the calendar, so `dur()` must
#' be the only token in its format string and isn't available in
#' [time_components()]. Its granule is always a 1-unit granule, so
#' `dur(hour(3L))` behaves like `dur(hour)`.
#'
#' @param granule The time granule to address, given as a granule generator (e.g.
#'   `year`) or a sized time unit (e.g. `year(1L)`). Resolved in the calendar of
#'   the time vector being formatted, parsed or decomposed.
#' @param cycle The coarser granule defining the cycle a `cyc()` component repeats
#'   within (e.g. `year` in `cyc(month, year)`).
#'
#' @return A component specification, consumed internally by the component-aware
#'   context (e.g. [format()], [time_parse()] or [time_components()]).
#'
#' @seealso [format()], [time_parse()], [time_components()] and [duration()]
#'
#' @name component_helpers
#' @export
lin <- function(granule) {
  cli::cli_abort(
    c(
      "{.fn lin} must be used inside a component-aware context.",
      i = "Use it in a {.fn format} string, when parsing, or in a {.fn time_components} expression, e.g. {.code lin(year)}."
    ),
    call = NULL
  )
}

#' @rdname component_helpers
#' @export
cyc <- function(granule, cycle) {
  cli::cli_abort(
    c(
      "{.fn cyc} must be used inside a component-aware context.",
      i = "Use it in a {.fn format} string, when parsing, or in a {.fn time_components} expression, e.g. {.code cyc(month, year)}."
    ),
    call = NULL
  )
}

#' @rdname component_helpers
#' @export
dur <- function(granule) {
  cli::cli_abort(
    c(
      "{.fn dur} must be used inside a component-aware context.",
      i = "Use it in a {.fn format} string or when parsing a duration, e.g. {.code dur(day)}."
    ),
    call = NULL
  )
}

# Decompose a single mt_time into the requested lin()/cyc() component vectors.
# Returns a list of mt_linear / mt_cyclical vectors, one per element of `specs`,
# in the same order.
#
# Every component is obtained from the `$linear` decomposition of its finest
# granule (see the note on [lin()]/[cyc()]); `cyc()` merely also records the
# cycle. All granules are extracted together in a single `chronon_parts()` call.
component_parts <- function(x_time, specs, calendar) {
  chronon <- attr(x_time, "chronon")
  mask <- component_mask(calendar, chronon)

  evaled <- lapply(specs, eval_component, mask = mask)
  if (any(vapply(evaled, inherits, logical(1L), "mt_dur_spec"))) {
    cli::cli_abort(
      c(
        "{.fn dur} can't be used in {.fn time_components}.",
        i = "Use {.fn lin} or {.fn cyc} to extract components of a time point."
      ),
      call = NULL
    )
  }

  # Drop special values (NA / Inf) before decomposition, as format() does.
  xd <- vec_data(x_time)
  keep <- !(is.na(x_time) | is.infinite(xd))
  n <- length(x_time)

  # Match the discreteness of the input: discrete (integer) time yields integer
  # components, continuous (double) time keeps fractional positions.
  discrete <- is.integer(xd)
  coerce <- if (discrete) as.integer else identity

  # A component finer than the input chronon is temporally indeterminate in
  # discrete time (the month of a year() is undefined), so its value is NA
  # everywhere and it takes no part in the decomposition; continuous time
  # resolves finer granules exactly (specials are handled separately below via
  # `keep`).
  determinable <- vapply(
    evaled,
    function(spec) chronon_is_determinate_at(chronon, spec[[1L]], discrete),
    logical(1L)
  )

  # Extract the finest granule (chronon) of every determinable component as a
  # linear part in a single shared traversal, then re-align to all specs (the
  # indeterminate ones stay NULL and become NA below).
  linear_vals <- vector("list", length(evaled))
  linear_vals[determinable] <- chronon_parts(
    x_time[keep],
    linear = lapply(evaled[determinable], `[[`, 1L)
  )$linear

  .mapply(
    function(spec, val, ok) {
      if (ok) {
        if (chronon_is_local(spec[[1L]])) {
          # chronon_parts() adds chronon_epoch() to linear parts for display;
          # undo it so the stored value is a count of chronons since the Unix
          # epoch.
          val <- coerce(val - chronon_epoch(spec[[1L]]))
        } else {
          # chronon_parts() gives local wall-clock labels, but a chronon with a
          # UTC offset (e.g. hour(tz = "Australia/Melbourne")) stores absolute
          # counts, so convert those directly. Reading the labels back would
          # apply the offset twice, and they're ambiguous when clocks go back.
          val <- coerce(chronon_convert_impl(
            xd[keep], chronon, spec[[1L]], discrete = TRUE
          ))
        }
      }
      if (length(spec) == 2L) {
        # Cyclical: NA for an indeterminate component, and for a special
        # (NA / Inf) input which has no cyclical position.
        vals <- if (ok) {
          component_scatter(val, n, keep)
        } else {
          xd[rep(NA_integer_, n)]
        }
        mt_cyclical(vals, chronon = spec[[1L]], cycle = spec[[2L]])
      } else {
        # Linear: NA for an indeterminate component; otherwise an infinite time
        # has an infinite count, so carry the input's +/-Inf (and NA) through.
        vals <- if (ok) {
          component_scatter(val, n, keep, special = xd)
        } else {
          xd[rep(NA_integer_, n)]
        }
        mt_linear(vals, chronon = spec[[1L]])
      }
    },
    dots = list(evaled, linear_vals, determinable),
    MoreArgs = NULL
  )
}

# Build the lin()/cyc() data-mask shared by component-aware contexts.
#
# `chronon` (if specified) provides the primary source of time units that form
# granules for the components, and `calendar` adds additional units not present
# in the calendar of `chronon` (useful for cyclical granularities between
# calendars, e.g. day-of-week).
component_mask <- function(calendar, chronon = NULL) {
  cal <- calendar_units(calendar)
  if (!is.null(chronon)) {
    cal <- calendar_units(time_calendar(chronon))
    extra <- setdiff(names(calendar), names(cal))
    cal[extra] <- calendar[extra]
  }

  as_tu <- function(g) {
    if (!S7_inherits(g, mt_unit)) {
      g <- g(1L)
    }
    if (!is.null(chronon)) {
      g <- granule_inherit_props(g, chronon)
    }
    g
  }

  # `...` carries options (e.g. label arguments) attached as attribute
  c(
    cal,
    list(
      lin = function(granule, ...) structure(list(as_tu(granule)), ...),
      cyc = function(granule, cycle, ...) {
        structure(list(as_tu(granule), as_tu(cycle)), ...)
      },
      # Classed to distinguish it from lin(), with its granule fixed to 1 unit.
      dur = function(granule, ...) {
        g <- as_tu(granule)
        if (!identical(g@n, 1L)) S7::prop(g, "n") <- 1L
        structure(list(g), ..., class = "mt_dur_spec")
      }
    )
  )
}

# Evaluate a single component expression in the lin()/cyc() mask, adding the
# familiar hint when a unit (commonly `week`) is missing from the calendar.
eval_component <- function(quo, mask) {
  tryCatch(
    eval_tidy(quo, data = mask),
    error = function(e) {
      msg <- conditionMessage(e)
      if (
        grepl("week", msg, fixed = TRUE) &&
          grepl("not found", msg, fixed = TRUE)
      ) {
        cli::cli_abort(
          c(
            msg,
            i = "The {.code week} unit is not in this calendar.",
            ">" = "Supply the ISO week calendar, e.g. {.code calendar = cal_isoweek}."
          ),
          call = NULL
        )
      }
      cli::cli_abort(msg, call = NULL)
    }
  )
}

# Scatter computed component values back into their original positions. Dropped
# (special) positions are filled from `special`, a length-n template that
# defaults to NA of `val`'s type; linear components pass the input data so that
# +/-Inf carries through unchanged (and NA stays NA).
component_scatter <- function(
  val,
  n,
  keep,
  special = val[rep(NA_integer_, n)]
) {
  if (all(keep)) {
    return(val)
  }
  out <- special
  out[keep] <- val
  out
}
