# TODO: Consider reworking into a user-facing function that inherits granules 
# from `x` rebased onto chronon `to`.

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

#' @method vec_cast.Date mt_linear
#' @export
vec_cast.Date.mt_linear <- function(x, ...) {
  chronon <- time_chronon(x)
  as.Date(chronon_cast(chronon, tu_day(1L), vec_data(x))$chronon)
}

#' @method vec_cast.POSIXct mt_linear
#' @export
vec_cast.POSIXct.mt_linear <- function(x, ...) {
  chronon <- time_chronon(x)
  as.POSIXct(
    chronon_cast(chronon, tu_second(1L), vec_data(x))$chronon,
    origin = "1970-01-01", tz = attr(x, "tz")
  )
}