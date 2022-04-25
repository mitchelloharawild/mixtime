#' Is a moment nested within another moment?
#'
#' \lifecycle{experimental}
#'
#' This function helps identify if one temporal value is nested within the
#' granularity of another temporal value. For example, is 2020 Jan in 2020 Q1?
#'
#' @param x A vector of moments
#' @param by A vector of moments for which `x` might be nested by
#'
#' @examples
#' moment_nested(yearmonth(0:5), by = yearquarter(0))
#' moment_nested(yearmonth(0:5), by = yearquarter(1))
#'
#' @return A logical vector of same length as `x`.
#'
#' @export
moment_nested <- function(x, by) {
  by <- vec_recycle(by, vec_size(x))
  x_cal <- calendar_data(x)
  by_cal <- calendar_data(by)
  common_granularity <- vec_c(!!!mapply(vec_cast, by_cal$granularity, x_cal$granularity, SIMPLIFY = FALSE))
}
