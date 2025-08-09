#' @inherit tsibble::yearquarter
#' @export
yearquarter <- function(x = NULL, ...) {
  mixtime(
    tsibble::yearquarter(x, ...)
  )
}

#' @inherit tsibble::yearmonth
#' @export
yearmonth <- function(x = NULL, ...) {
  mixtime(
    tsibble::yearmonth(x, ...)
  )
}

#' @inherit tsibble::yearweek
#' @export
yearweek <- function(x = NULL, ...) {
  mixtime(
    tsibble::yearweek(x, ...)
  )
}

