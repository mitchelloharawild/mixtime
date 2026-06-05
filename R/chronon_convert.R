#' Convert between chronons
#'
#' @param x A linear time object (of class `mt_linear`)
#' @param to The target chronon to convert to (a time granule object)
#' @param discrete If `TRUE`, the number of target chronons since Unix epoch 
#' @param ... Additional arguments for methods.
#' that `x` falls into is returned as an integer. If `FALSE`, a fractional 
#' number of target chronons is returned (analagous to time using a continuous
#' time model).
#' 
#' @return A numeric or integer representing the time since Unix epoch in terms of the target
#' chronon's precision.
#' 
#' @examples
#' 
#' # Convert from months since epoch to years since epoch
#' chronon_convert(yearmonth(Sys.Date()), cal_gregorian$year(1L))
#' 
#' # Convert from days since epoch to months since epoch
#' chronon_convert(Sys.Date(), cal_gregorian$month(1L))
#' chronon_convert(Sys.Date(), cal_gregorian$month(1L), discrete = TRUE)
#' 
#' # Convert from seconds since epoch to hours since epoch
#' chronon_convert(Sys.time(), cal_gregorian$hour(1L))
#' 
#' @noRd
#' @keywords internal
chronon_convert <- S7::new_generic("chronon_convert", "x")

chronon_convert_impl <- function(x, from, to, discrete, tz = NULL) {
  # Inherit granule properties from `from` if not given in `to` granule
  to <- granule_inherit_props(to, from)

  # Handle timezones
  # TODO - Generalise for converting to any different granule attribute
  # (e.g. locA -> locB)

  is_tz_conv <- S7::S7_inherits(from, mt_tz_unit) && S7::S7_inherits(to, mt_tz_unit)
  if (is_tz_conv) {
    if (is.na(tz_name(from)) && !is.na(tz_name(to))) {
      cli::cli_abort(
        "Cannot convert from timezone-naive chronon to timezone-aware chronon.",
        call = NULL
      )
    }
  }
  
  # Find path along convertable time units
  path <- S7_graph_dispatch(
    unique(c(
      # Chronon divmod should be directional
      method_signatures(chronon_divmod),
      # Chronon cardinality is a undirected fallback
      method_signatures(chronon_cardinality)
    )),
    from,
    to
  )

  # Convert to target tz naive time for standard chronon boundaries
  # Skipped if:  
  # * target tz is UTC (since that matches the internal representation)
  is_tz_utc <- identical(tz_name(to), "UTC") || identical(tz, "UTC")
  # * target is not a timezone-aware mt_tz_unit
  is_tz_aware <- is_tz_conv && !is.na(tz_name(to))

  if (!is_tz_utc && is_tz_aware) {
    tzo <- tz_offset_impl(x, chronon = from, tz = tz_name(to))
    x <- x + tzo
  } else if (is_tz_conv && is.na(tz_name(to)) && !is.na(tz_name(from))) {
    # Converting from tz-aware to tz-naive - remove offset
    tzo <- tz_offset_impl(x, chronon = from, tz = tz_name(from))
    x <- x + tzo
  }

  # Convert between time points
  if (length(path) == 1L) {
    # Simple direct conversion by cardinality
    x <- vec_data(x) / chronon_cardinality(from, to, at = x)
  } else {
    # Convert along the path of time granules
    path[[1]] <- from
    prop_from <- S7::prop_names(from)
    path[[length(path)]] <- to
    prop_to <- S7::prop_names(to)
    # Initialise intermediate classes with 1L and adjacent properties
    path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(tu){
      # Ideally inherit from `to`, but if incomplete inherit from `from`
      granule_inherit_props(granule_inherit_props(tu(1L), to), from)
    })
  
    # Convert chronons along the path
    not_na <- !is.na(x)
    for (i in seq(2, length.out = length(path)-1)) {
      res <- chronon_divmod_dispatch(path[[i-1L]], path[[i]], x[not_na])
      x[not_na] <- res$div
      nz_mod <- res$mod != 0
      part <- chronon_cardinality(path[[i-1L]], path[[i]], floor(res$div[nz_mod]))
      x[not_na][nz_mod] <- x[not_na][nz_mod] + res$mod[nz_mod]/part
    }
  }

  # Convert back to absolute time for time zoned data
  # Skipped if target tz is:
  # * NA (since that indicates a timezone-naive chronon)
  # * UTC (since that matches the internal representation)
  if (!is_tz_utc && is_tz_aware) {
    f <- if (discrete) as.integer else identity
    x <- x - f(chronon_convert_impl(tzo, from, to, discrete = FALSE, tz = "UTC"))
  }

  if (discrete) x <- as.integer(floor(x))
  
  x
}

method(chronon_convert, S7::new_S3_class("mt_linear")) <- function(x, to, discrete = FALSE, ...) {
  chronon_convert_impl(vec_data(x), attr(x, "chronon"), to, discrete)
}

method(chronon_convert, class_mixtime) <- vecvec::vecvec_apply_fn(chronon_convert, SIMPLIFY = TRUE)

method(chronon_convert, S7::class_any) <- function(x, to, discrete = FALSE, ...) {
  chronon_convert_impl(as.numeric(x), chronon_common(x), to, discrete)
}
