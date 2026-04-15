#' Convert between chronons
#'
#' @param x A linear time object (of class `mt_linear`)
#' @param to The target chronon to convert to (a time unit object)
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
chronon_convert <- S7::new_generic("chronon_cardinality", "x")

chronon_convert_impl <- function(x, from, to, discrete, tz = NULL) {
  # Convert between same time unit types
  if (identical(S7::S7_class(from), S7::S7_class(to))) {
    x <- vec_data(x) * from@n / to@n
    if (discrete) x <- as.integer(floor(x))
    return(x)
  }

  # Add default tz if not given in `to` chronon
  if (S7::prop_exists(to, "tz") && !nzchar(to@tz)){
    to@tz <- if (S7::prop_exists(from, "tz")) from@tz else ""
  }
  if (is.null(tz)) {
    tz <- tz_name(to)
  }
  if (identical(tz, "")) {
    tz <- "UTC"
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

  # Find timezone offsets for chronon boundaries
  if (tz != "UTC") {
    `1s` <- cal_time_civil$second(1L)
    xs <- chronon_convert_impl(x, from, `1s`, discrete = FALSE, tz = "UTC")
    xso <- get_tz_offset(as.double(xs), tz)
    x <- chronon_convert_impl(xs + xso, `1s`, from, discrete = FALSE, tz = "UTC")
  }

  path[[1]] <- from
  path[[length(path)]] <- to
  # Initialise intermediate classes with 1L
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(x) x(1L))

  # Convert chronons along the path
  not_na <- !is.na(x)
  for (i in seq(2, length.out = length(path)-1)) {
    res <- chronon_divmod_dispatch(path[[i-1L]], path[[i]], x[not_na])
    x[not_na] <- res$div
    nz_mod <- res$mod != 0
    part <- chronon_cardinality(path[[i-1L]], path[[i]], floor(res$div[nz_mod]))
    x[not_na][nz_mod] <- x[not_na][nz_mod] + res$mod[nz_mod]/part
  }

  # Convert back to UTC time internally
  if (tz != "UTC") {
    f <- if (discrete) as.integer else identity
    x <- x - f(chronon_convert_impl(xso, `1s`, to, discrete = FALSE, tz = "UTC"))
  }

  if (discrete) x <- as.integer(floor(x))
  
  x
}

method(chronon_convert, S7::new_S3_class("mt_linear")) <- function(x, to, discrete = FALSE, ...) {
  chronon_convert_impl(vec_data(x), time_chronon(x), to, discrete)
}

method(chronon_convert, class_mixtime) <- vecvec::vecvec_apply_fn(chronon_convert, SIMPLIFY = TRUE)

method(chronon_convert, S7::class_any) <- function(x, to, discrete = FALSE, ...) {
  chronon_convert_impl(as.numeric(x), time_chronon(x), to, discrete)
}
