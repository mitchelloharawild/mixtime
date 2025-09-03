time_cast <- function(x, to, ...) {
  UseMethod("time_cast")
}

#' @export
time_cast.Date <- function(x, to, ...) {
  as.integer(x)%/%calendar_algebra(to, tu_day(1L))
}

#' @export
time_cast.POSIXt <- function(x, to, ...) {
  as.integer(x)%/%calendar_algebra(to, tu_second(1L))
}

#' @export
time_cast.numeric <- function(x, to, ...) {
  x
}

#' @export
time_cast.default <- function(x, to, ...) {
  stop("Casting from ", class(x)[1L], " to ", class(to)[1L], " not implemented", call. = FALSE)
}