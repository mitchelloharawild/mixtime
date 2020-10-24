#' Create a new moment
#'
#' A moment is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The iterable value of the moment (for example, a value of 1 may indicate the first day).
#' @param calendar The calendar structure over which the moment iterates. This can be created using `new_calendar()`.
#'
#' @importFrom rlang is_empty
#' @export
new_moment <- function(x = numeric(), calendar = new_calendar()) {
  x <- vec_cast(x, double())
  # calendar[[".rows"]] <- new_list_of(if(is_empty(x)) list() else list(seq_along(x)), ptype = integer())
  vctrs::new_rcrd(list(x = x, c = vec_rep(1L, length(x))), cal = calendar, class = "moment")
}

#' @export
moment <- function(x, ...) {
  UseMethod("moment")
}

#' Convert time class into a moment
#'
#' @param x A time value to convert to a moment
#'
#' @export
as_moment <- function(x, ...) {
  vec_cast(x, new_moment())
}

#' @export
moment.default <- function(x, ...){
  vec_cast(x, new_moment())
}

#' @export
format.moment <- function(x, ...) {
  cal <- calendar_data(x)
  val <- field(x, "x")
  cal_id <- field(x, "c")
  fmt <- vec_group_loc(cal_id)
  fmt$tu <- cal$granularity[fmt$key]
  fmt$origin <- cal$origin[fmt$key]
  fmt$val <- lapply(fmt$loc, vec_slice, x = val)
  fmt <- mapply(format_time, tu = fmt$tu, fmt$val, origin = fmt$origin, SIMPLIFY = FALSE)
  vec_c(!!!fmt)
}

#' @export
calendar_data.moment <- function(x) {
  attr(x, "cal")
}

#' @export
vec_arith.moment <- function(op, x, y, ...){
  x_cal <- calendar_data(x)
  y_cal <- calendar_data(y)
  if(x_cal$origin && y_cal$origin) {
    if(op != "-") abort(str_glue("Cannot use operation {op} with moments that both have origins."))
    x_cal$origin <- FALSE
    new_moment(do.call(op, list(field(x, "x"), field(y, "x"))), x_cal)
  } else if(x_cal$origin || y_cal$origin) {
    if(!vec_in(op, c("-", "+"))) abort(str_glue("Cannot use operation {op} with moments that have origins."))
    origin_cal <- if(x_cal$origin) x_cal else y_cal
    duration_cal <- if(x_cal$origin) y_cal else x_cal
    duration_cal$granularity <- vec_cast(duration_cal$granularity, origin_cal$granularity)
    new_moment(do.call(op, list(field(x, "x"), field(y, "x"))), origin_cal)
  } else {
    new_moment(do.call(op, list(field(x, "x"), field(y, "x"))), x_cal)
  }
}

#' @export
vec_ptype2.moment.moment <- function(x, y, ...) {
  x_cal <- calendar_data(x)
  y_cal <- calendar_data(y)
  field(y, "c") <- field(y, "c") + vec_size(x_cal)
  attr(x, "cal") <- vec_unique(vec_rbind(x_cal, y_cal))
  x
}

#' @export
vec_cast.moment.moment <- function(x, to, ...) {
  pos <- vec_match(calendar_data(x), calendar_data(to))
  field(x, "c") <- pos[field(x, "c")]
  attr(x, "cal") <- calendar_data(to)
  x
}

#' @export
vec_restore.moment <- function(x, to, ..., n = NULL) {
  # attr(to, "cal") <- vec_slice(calendar_data(to), unique(x$c))
  NextMethod()
}

#' @export
vec_cast.Date.moment <- function(x, to, ...) {
  cal <- calendar_data(x)
  if(any(!cal$origin)) abort("Only moments with origins can be converted to dates.")
  abort("Moments cannot yet be converted to dates (awaiting daily moment support).")
  # 1. Convert to date moment
  # 2. Convert date moment to Date
}

#' @export
vec_cast.character.moment <- function(x, to, ...) {
  format(x)
}

#' @export
seq.moment <- function(from, to, by, length.out, along.with, ...){
  vec_assert(by, numeric(), 1L)
  vec_assert(from, size = 1)
  vec_cast(to, from)
  res <- seq.int(
    field(from, "x"), field(to, "x"), by = by
    # length.out = length.out, along.with = along.with, ...
  )
  res <- list(x = res, c = vec_rep(1L, vec_size(res)))
  vec_restore(res, from)
}
