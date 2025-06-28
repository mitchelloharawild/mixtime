#' @inherit tsibble::yearquarter
#' @export
yearquarter <- function(x = NULL, ...) {
  new_mixtime(
    tsibble::yearquarter(x, ...)
  )
}

#' @inherit tsibble::yearmonth
#' @export
yearmonth <- function(x = NULL, ...) {
  new_mixtime(
    tsibble::yearmonth(x, ...)
  )
}

#' @inherit tsibble::yearweek
#' @export
yearweek <- function(x = NULL, ...) {
  new_mixtime(
    tsibble::yearweek(x, ...)
  )
}

