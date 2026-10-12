# Chronon context
#
# A chronon's *context* is the set of granule properties that tie its labels to
# absolute time: a time zone (mt_tz_unit) or a location (mt_loc_unit). The
# chronon_divmod graph is keyed by class alone and is blind to context, so every
# context-aware operation is phrased in terms of two frames:
#
# * absolute time: UTC seconds, shared by every context; and
# * local time: the context's own wall-clock reading, expressed in the
#   chronon's units, which is what the context-blind graph operates on.
#
# A context type implements the four generics below to move a chronon's stored
# values in and out of those frames. Conversions between chronons
# (chronon_convert_impl()) and calendar arithmetic (duration_shift(), seq(),
# time_round(), chronon_parts(), ...) are written against these generics only.
#
# The defaults suit any context whose stored values are already local and whose
# graph edges to civil seconds already account for the context (e.g. solar
# time, whose civil <-> solar edges apply the location). Time zones store
# absolute bucket positions instead; see R/timezone.R.

# The context properties of a chronon, as a named list.
chronon_context <- S7::new_generic("chronon_context", "x")
method(chronon_context, mt_unit) <- function(x) list()
method(chronon_context, mt_tz_unit) <- function(x) list(tz = x@tz)
method(chronon_context, mt_loc_unit) <- function(x) {
  list(lat = x@lat, lon = x@lon, alt = x@alt)
}

# Whether `x`'s stored values are its local wall-clock values, so the
# context-blind graph can convert them directly to another chronon in the same
# context.
chronon_is_local <- S7::new_generic("chronon_is_local", "chronon")
method(chronon_is_local, mt_unit) <- function(chronon) TRUE

# Stored values -> absolute time (continuous UTC seconds).
chronon_to_utc <- S7::new_generic(
  "chronon_to_utc",
  "chronon",
  function(chronon, x) S7::S7_dispatch()
)
method(chronon_to_utc, mt_unit) <- function(chronon, x) {
  chronon_convert_graph(x, chronon, tu_utc_second())
}

# Absolute time (UTC seconds) -> stored values.
chronon_from_utc <- S7::new_generic(
  "chronon_from_utc",
  "chronon",
  function(chronon, x, discrete = FALSE) S7::S7_dispatch()
)
method(chronon_from_utc, mt_unit) <- function(chronon, x, discrete = FALSE) {
  chronon_convert_graph(x, tu_utc_second(), chronon, discrete)
}

# Stored values -> local wall-clock values in the chronon's units: the local
# label of each chronon plus the fraction through it.
chronon_to_local <- S7::new_generic(
  "chronon_to_local",
  "chronon",
  function(chronon, x) S7::S7_dispatch()
)
method(chronon_to_local, mt_unit) <- function(chronon, x) as.double(x)

# Local wall-clock values -> stored values, returned as `$value` along with
# which local readings were `$invalid`. A local reading that occurs more than
# once (e.g. when daylight saving ends) resolves to its earliest occurrence.
# One that doesn't occur (e.g. when daylight saving starts) is invalid, and is
# resolved like other invalid time points (see chronon_recompose()):
#
# * "clamp": back by the size of the gap, keeping finer fields (02:30 in a
#   skipped 02:00-03:00 hour -> 01:30, before the gap).
# * "previous": the end of the valid time before the gap. For discrete time
#   this is its last chronon (01:59:59); for continuous time it is the
#   transition itself, the same instant as "next".
# * "next": the start of the valid time after the gap, the transition itself
#   (03:00).
# * "overflow": forward by the size of the gap (02:30 -> 03:30, after it).
# * "reject", "error": NA (callers signal the warning or error).
chronon_from_local <- S7::new_generic(
  "chronon_from_local",
  "chronon",
  function(chronon, x, discrete = FALSE, on_invalid = "clamp") S7::S7_dispatch()
)
method(chronon_from_local, mt_unit) <- function(
  chronon,
  x,
  discrete = FALSE,
  on_invalid = "clamp"
) {
  list(
    value = if (discrete) floor_ulp(x) else as.double(x),
    invalid = rep_len(FALSE, length(x))
  )
}

# Whether shifting `chronon`'s values by `unit` (see chronon_shift()) is exact
# arithmetic on absolute time, rather than calendar arithmetic that keeps the
# local wall-clock reading. Defaults to calendar arithmetic, which is the same
# thing for contexts whose stored values are already local.
chronon_shift_is_exact <- S7::new_generic(
  "chronon_shift_is_exact",
  "chronon",
  function(chronon, unit) S7::S7_dispatch()
)
method(chronon_shift_is_exact, mt_unit) <- function(chronon, unit) FALSE

# The chronon used as the absolute time frame, built once since constructing
# it validates the time zone name.
tu_utc_second <- local({
  chronon <- NULL
  function() chronon %||% (chronon <<- cal_time_civil$second(1L, tz = "UTC"))
})

# Length of `unit` in seconds, or NA when it isn't fixed (e.g. a month).
chronon_fixed_seconds <- function(unit) {
  s <- tu_utc_second()
  if (!chronon_fixed_reachable(s, unit) && !chronon_fixed_reachable(unit, s)) {
    return(NA_real_)
  }
  chronon_cardinality(s, unit)
}

# `chronon` with its context replaced by `ctx`'s (where they share one).
chronon_in_context <- function(chronon, ctx) {
  shared <- intersect(
    names(chronon_context(chronon)),
    names(chronon_context(ctx))
  )
  if (length(shared)) {
    props(chronon)[shared] <- props(ctx)[shared]
  }
  chronon
}

# --- context comparisons -------------------------------------------------------

context_is_naive <- function(ctx) {
  length(ctx) > 0L && any(vapply(ctx, function(v) anyNA(v), logical(1L)))
}

# Context values are compared by their underlying value, ignoring mt_naive.
context_equal <- function(x, y) {
  props_equal(lapply(x, unclass), lapply(y, unclass))
}

# Both chronons store local values in the same context, so the context-blind
# graph converts between them directly.
chronon_shares_frame <- function(from, to) {
  context_equal(chronon_context(from), chronon_context(to)) &&
    chronon_is_local(from) &&
    chronon_is_local(to)
}

# Both chronons have the same kind of context, which `to` leaves unspecified
# (e.g. a zoned time converted to a naive one).
chronon_drops_context <- function(from, to) {
  ctx_from <- chronon_context(from)
  ctx_to <- chronon_context(to)
  identical(names(ctx_from), names(ctx_to)) &&
    !context_is_naive(ctx_from) &&
    context_is_naive(ctx_to)
}

# Both chronons have the same kind of context, which `from` leaves unspecified
# (e.g. a solar time read without a location). The local reading is placed in
# `to`'s context.
chronon_gains_context <- function(from, to) {
  chronon_drops_context(to, from)
}

# A naive time can't be placed in a time zone: its local reading may occur
# twice, or not at all, around a daylight saving transition.
chronon_check_context <- function(from, to) {
  if (S7_inherits(to, mt_tz_unit) && chronon_gains_context(from, to)) {
    cli::cli_abort(
      "Cannot convert from timezone-naive chronon to timezone-aware chronon.",
      call = NULL
    )
  }
  invisible()
}
