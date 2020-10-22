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
  calendar[[".rows"]] <- new_list_of(if(is_empty(x)) list() else list(seq_along(x)), ptype = integer())
  vctrs::new_vctr(x, cal = calendar, class = "moment")
}

#' @export
format.moment <- function(x, ...) {
  cd <- calendar_data(x)
  x <- vec_proxy(x)$x
  n_cal <- vec_size(cd)
  out <- vector("list", n_cal)
  for(i in seq_len(n_cal)) {
    cal <- vec_slice(cd, i)
    val <- x[cal[[".rows"]][[1]]]
    out[[i]] <- if(cal$origin) {
      format_time(cal$granularity[[1]], val)
    } else {
      sprintf("%i %s%s", val, vec_ptype_full(cal$granularity[[1]]), ifelse(val!=1, "s", ""))
    }
  }
  vec_c(!!!out)
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
    new_moment(do.call(op, list(vec_proxy(x)$x, vec_proxy(y)$x)), x_cal)
  } else if(x_cal$origin || y_cal$origin) {
    if(!vec_in(op, c("-", "+"))) abort(str_glue("Cannot use operation {op} with moments that have origins."))
    origin_cal <- if(x_cal$origin) x_cal else y_cal
    duration_cal <- if(x_cal$origin) y_cal else x_cal
    duration_cal$granularity <- vec_cast(duration_cal$granularity, origin_cal$granularity)
    new_moment(do.call(op, list(vec_proxy(x)$x, vec_proxy(y)$x)), origin_cal)
  } else {
    new_moment(do.call(op, list(vec_proxy(x)$x, vec_proxy(y)$x)), x_cal)
  }
}

#' @export
vec_ptype2.moment.moment <- function(x, y, ...) {
  x_cal <- calendar_data(x)
  y_cal <- calendar_data(y)
  y_cal[[".rows"]] <- as_list_of(lapply(y_cal[[".rows"]], `+`, sum(lengths(x_cal[[".rows"]]))))
  attr(x, "cal") <- vec_rbind(x_cal, y_cal)
  x
}

#' @export
vec_cast.moment.moment <- function(x, to, ...) {
  attr(x, "cal") <- calendar_data(to)
  x
}

#' @export
vec_proxy.moment <- function(x, ...) {
  r <- calendar_data(x)[[".rows"]]
  p <- vec_init(integer(), length(x))
  for(i in seq_along(r)) p[r[[i]]] <- i
  attributes(x) <- NULL
  new_data_frame(list(x = x, p = p))
}

#' @export
vec_restore.moment <- function(x, to, ..., n = NULL) {
  keep <- vec_group_loc(x$p)
  cal <- calendar_data(to)[keep$key,]
  cal$.rows <- keep$loc
  new_moment(x$x, cal)
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
    vec_data(from), vec_data(to), by = vec_data(by)
    # length.out = length.out, along.with = along.with, ...
  )
  attr(from, "cal")$.rows <- list(seq_along(res))
  vec_restore(res, from)
}
