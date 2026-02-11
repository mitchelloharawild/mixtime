#' Create a new mixtime
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The list of time values to become a mixtime.
#'
#' @importFrom rlang is_empty
#' @export
new_mixtime <- function(x = list()) {
  validate_x <- vapply(x, mixtime_valid, logical(1L))

  if (!all(validate_x)) {
    stop("A mixtime must contain only valid time points")
  }
  vecvec::new_vecvec(x, class = "mixtime")
}

#' @export
mixtime <- function(...) {
  new_mixtime(rlang::list2(...))
}

#' Convert time class into a mixtime
#'
#' @param x A time value to convert to a mixtime
#' @param ... Additional arguments for methods
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, new_mixtime())
}

#' Check if the object is a mixtime
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `mixtime` class.
#'
#' @examples
#' is_mixtime(Sys.Date())
#' is_mixtime(yearmonth(1))
#'
#' @export
is_mixtime <- function(x) {
  inherits(x, "mixtime")
}

#' Check if times can be used within mixtime
#' 
#' @param x A vector of times.
#' 
#' @return A logical vector.
#' 
#' @export
mixtime_valid <- function(x) {
  UseMethod("mixtime_valid")
}

#' @export
mixtime_valid.default <- function(x) {
  isTRUE(tsibble::index_valid(x))
}

#' @export
mixtime_valid.mixtime <- function(x) TRUE

#' @export
vec_ptype_full.mixtime <- function(x, ...) "mixtime"

#' @export
vec_ptype_abbr.mixtime <- function(x, ...) "mixtime"

vec_cast_to_mixtime <- function(x, to, ...) mixtime(x)

vec_cast_from_mixtime <- function(x, to, ...) {
  class(x) <- setdiff(class(x), "mixtime")
  vec_cast(x, to, ...)
}

## Custom vec cast methods since some time classes don't have cast methods

#' @export
#' @method vec_cast.character mixtime
vec_cast.character.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.character, ...)
  vecvec::unvecvec(x)
}

#' @export
#' @method vec_cast.double mixtime
vec_cast.double.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.double, ...)
  vecvec::unvecvec(x)
}

#' @export
vec_ptype2.mixtime <- function(x, y, ...) {
  x_is_time <- isTRUE(index_valid(x))
  y_is_time <- isTRUE(index_valid(y))

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

#' @export
#' @importFrom vctrs vec_proxy_order
vec_proxy_order.mixtime <- function(x, ...) {
  if (length(attr(x, "v")) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(attr(x, "v"), time_chronon)
    chronon_type <- chronon_common(!!!chronons)

    attr(x, "v") <- lapply(attr(x, "v"), function(v) {
      if (is.integer(v)) v <- v + 0.5
      mixtime::chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vctrs::vec_data(vecvec::unvecvec(x)))
}

#' @export
seq.mixtime <- function(from, to, by = 1L, length.out = NULL, along.with = NULL, ...) {
  if (!missing(along.with)) {
    length.out <- length(along.with)
  } 
  else if (!is.null(length.out)) {
    if (length(length.out) != 1L) 
      stop(sprintf("'%s' must be of length 1", length.out))
    length.out <- ceiling(length.out)
  }

  # Strip mixtime vecvec wrapper
  arg <- list()
  if (has_from <- !missing(from)) arg$from <- vecvec::unvecvec(from)
  if (has_to <- !missing(to)) arg$to <- vecvec::unvecvec(to)
  arg <- c(arg, rlang::list2(by = by, length.out = length.out, ...))

  # Call seq method with bare vectors
  mixtime(rlang::exec(seq, !!!arg))
}