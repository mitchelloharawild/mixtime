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
  validate_x <- vapply(x, function(x) isTRUE(tsibble::index_valid(x)), logical(1L))

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
  if (!isTRUE(index_valid(x)) || !isTRUE(index_valid(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

#' @export
#' @importFrom vctrs vec_proxy_order
vec_proxy_order.mixtime <- function(x, ...) {
  if (length(attr(x, "v")) > 1L) {
    cli::cli_abort("Ordering mixtime objects with multiple granularities is not yet supported.")
  }
  vec_proxy_order(vctrs::vec_data(vecvec::unvecvec(x)))
}