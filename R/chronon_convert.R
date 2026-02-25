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
#' @export
chronon_convert <- S7::new_generic("chronon_cardinality", "x")

#' @rdname chronon_convert
chronon_convert.S7_methods <- function(x, to, discrete = FALSE) S7_method_docs()

chronon_convert_impl <- function(x, from, to, discrete, tz = tz_name(to)) {
  # Convert between same time unit types
  if (identical(S7::S7_class(from), S7::S7_class(to))) {
    x <- vec_data(x) * vec_data(from) / vec_data(to)
    if (discrete) x <- as.integer(floor(x))
    return(x)
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
    `1s` <- cal_time_civil_midnight$second(1L)
    xs <- chronon_convert_impl(x, from, `1s`, discrete = FALSE, tz = "UTC")
    xso <- get_tz_offset(as.double(xs), tz)
    x <- chronon_convert_impl(xs + xso, `1s`, from, discrete = FALSE, tz = "UTC")
  }

  path[[1]] <- from
  path[[length(path)]] <- to
  # Initialise intermediate classes with 1L
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(x) x(1L))

  # Convert chronons along the path
  for (i in seq(2, length.out = length(path)-1)) {
    res <- chronon_divmod_dispatch(path[[i-1L]], path[[i]], x)
    x <- res$chronon
    nz_mod <- res$remainder != 0
    part <- chronon_cardinality(path[[i]], path[[i-1L]], floor(res$chronon[nz_mod]))
    x[nz_mod] <- x[nz_mod] + res$remainder[nz_mod]/part
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

method(chronon_convert, S7::new_S3_class("mixtime")) <- function(x, to, discrete = FALSE, ...) {
  res <- vecvec::vecvec_apply(x, chronon_convert, to = to, discrete = discrete)
  vecvec::unvecvec(res)
}

method(chronon_convert, S7::class_any) <- function(x, to, discrete = FALSE, ...) {
  chronon_convert_impl(as.numeric(x), time_chronon(x), to, discrete)
}
