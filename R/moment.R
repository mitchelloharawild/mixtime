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
  x <- vec_data(x)
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
    new_moment(do.call(op, list(vec_data(x), vec_data(y))), x_cal)
  } else if(x_cal$origin || y_cal$origin) {
    if(!vec_in(op, c("-", "+"))) abort(str_glue("Cannot use operation {op} with moments that have origins."))
    origin_cal <- if(x_cal$origin) x_cal else y_cal
    new_moment(do.call(op, list(vec_data(x), vec_data(y))), origin_cal)
  } else {
    new_moment(do.call(op, list(vec_data(x), vec_data(y))), x_cal)
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

# #' @export
# vec_proxy.moment <- function(x, ...) {
#   r <- calendar_data(x)[[".rows"]]
#   x <- vec_data(x)
#   p <- vec_init(integer(), vec_size(x))
#   for(i in seq_along(r)) p[r[[i]]] <- i
#
#   new_data_frame(list(x = x, p = p))
# }

#' @export
`[.moment` <- function(x, i, ...){
  cal <- calendar_data(x)
  r <- cal[[".rows"]]
  p <- vec_init(integer(), vec_size(x))
  x <- NextMethod()
  for(j in seq_along(r)) p[r[[j]]] <- j
  p <- vec_group_loc(p[i])
  cal <- cal[p$key,]
  cal[[".rows"]] <- new_list_of(p$loc, ptype = integer())
  attr(x, "cal") <- cal
  x
}

#' @export
vec_cast.Date.moment <- function(x, to, ...) {
  cal <- calendar_data(x)
  if(any(!cal$origin)) abort("Only moments with origins can be converted to dates.")
  abort("Moments cannot yet be converted to dates (awaiting daily moment support).")
  # 1. Convert to date moment
  # 2. Convert date moment to Date
}
