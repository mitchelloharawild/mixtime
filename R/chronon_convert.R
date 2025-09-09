#' Convert between chronons
#' 
#' @param x A linear time object (of class `mt_linear`)
#' @param to The target chronon to convert to (a time unit object)
#' @param discrete If `TRUE`, the number of target chronons since Unix epoch 
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
#' chronon_convert(yearmonth(Sys.Date()), tu_year(1L))
#' 
#' # Convert from days since epoch to months since epoch
#' chronon_convert(Sys.Date(), tu_month(1L))
#' chronon_convert(Sys.Date(), tu_month(1L), discrete = TRUE)
#' 
#' # Convert from seconds since epoch to hours since epoch
#' chronon_convert(Sys.time(), tu_hour(1L))
#' 
#' @export
chronon_convert <- S7::new_generic("chronon_cardinality", "x")

chronon_convert_impl <- function(x, from, to, discrete) {
  divmod <- chronon_divmod(from, to, x)
  if (discrete) {
    return(as.integer(divmod$chronon))
  } else {
    return(divmod$chronon + divmod$remainder/chronon_cardinality(to, from, divmod$chronon))
  }
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
