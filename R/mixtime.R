#' Create a new mixtime
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The iterable value of the mixtime (for example, a value of 1 may indicate the first day).
#' @param calendar The calendar structure over which the mixtime iterates. This can be created using `new_calendar()`.
#'
#' @importFrom rlang is_empty
#' @export
new_mixtime <- function(x = NULL) {
  if (!is.null(x) && !isTRUE(tsibble::index_valid(x))) {
    stop("A mixtime must contain only valid time points")
  }
  vecvec::new_vecvec(list(x), class = "mixtime")
}

#' @export
mixtime <- function(x, ...) {
  UseMethod("mixtime")
}

#' @export
mixtime.default <- function(x, ...){
  as_mixtime(x, ...)
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

#' @export
seq.mixtime <- function(from, to, by, length.out, along.with, ...){
  vec_assert(from, size = 1)
  if(!missing(along.with)) {
    length.out <- vec_size(along.with)
  }
  res <- if(!missing(by)) {
    by <- vec_cast(by, numeric())
    if(missing(to)) {
      seq.int(field(from, "x"), by = by, length.out = length.out)
    } else {
      seq.int(field(from, "x"), field(to, "x"), by = by)
    }
  } else {
    seq.int(field(from, "x"), field(to, "x"), length.out = length.out)
  }
  res <- list(x = res, c = vec_rep(1L, vec_size(res)))
  vec_restore(res, from)
}

vec_cast_to_mixtime <- function(x, to, ...) new_mixtime(x)

#' @export
vec_ptype2.mixtime <- function(x, y, ...) {
  if (!isTRUE(index_valid(x)) || !isTRUE(index_valid(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}


