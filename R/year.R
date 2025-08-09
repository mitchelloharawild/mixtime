#' Represent years
#'
#' Create or coerce using `year()`
#'
#' \lifecycle{experimental}
#'
#' @param x Another object to be coerced into years
#' @param ... Arguments for methods.
#'
#' @export
year <- function(x, ...){
  UseMethod("year")
}

#' @export
new_year <- function(x) {
  x <- vec_cast(x, integer())
  vctrs::new_vctr(x, class = "mixtime_year")
}

#' @export
year.numeric <- function(x, ...) {
  new_mixtime(new_year(x - 1970L))
}

#' @export
year.default <- function(x, ...) {
  new_mixtime(new_year(as.integer(strftime(x, "%Y")) - 1970L))
}

#' @export
format.mixtime_year <- function(x, ...) {
  format(vec_data(x) + 1970L, ...)
}

#' @export
index_valid.mixtime_year <- function(x) TRUE

#' @export
interval_pull.mixtime_year <- function(x) {
  tsibble::new_interval(
    year = tsibble::gcd_interval(x)
  )
}

#' @importFrom vctrs vec_arith
#' @method vec_arith mixtime_year
#' @export
vec_arith.mixtime_year <- function(op, x, y, ...) {
  UseMethod("vec_arith.mixtime_year", y)
}
#' @method vec_arith.mixtime_year mixtime_year
#' @export
vec_arith.mixtime_year.mixtime_year <- function(op, x, y, ...) {
  if(!op %in% c("+", "-")) stop_incompatible_op(op, x, y)
  vctrs::vec_arith_base(op, x, y)
}
