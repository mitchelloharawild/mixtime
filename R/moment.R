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

#' @export
moment.default <- function(x, ...){
  vec_cast(x, new_moment())
}

#' Convert time class into a moment
#'
#' @param x A time value to convert to a moment
#'
#' @export
as_moment <- function(x, ...) {
  vec_cast(x, new_moment())
}

#' Check if the object is a moment
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `moment` class.
#'
#' @examples
#' is_moment(Sys.Date())
#' is_moment(yearmonth(1))
#'
#' @export
is_moment <- function(x) {
  inherits(x, "moment")
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
`names<-.moment` <- function(x, value) {
  x
}

#' @export
calendar_data.moment <- function(x) {
  attr(x, "cal")
}

#' @export
#' @method vec_arith moment
vec_arith.moment <- function(op, x, y, ...){
  UseMethod("vec_arith.moment", y)
}

#' @export
#' @method vec_arith.moment moment
vec_arith.moment.moment <- function(op, x, y, ...) {
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
#' @method vec_arith.moment numeric
vec_arith.moment.numeric <- function(op, x, y, ...) {
  x_cal <- calendar_data(x)
  if(!vec_in(op, c("+", "-"))) {
    abort(str_glue("Cannot use operation {op} for a <moment> and <double>."))
  }
  x <- vec_recycle_common(x, y)
  field(x[[1]], "x") <- vec_arith(op, field(x[[1]], "x"), x[[2]], ...)
  x[[1]]
}

#' @export
#' @method vec_arith.numeric moment
vec_arith.numeric.moment <- function(op, x, y, ...) {
  vec_arith.moment.numeric(op, y, x, ...)
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
  cal <- calendar_data(to)
  cal_x <- calendar_data(x)
  pos <- vec_match(cal_x, cal)
  missing_cal <- is.na(pos)
  if(any(missing_cal)) {
    pos[missing_cal] <- nrow(cal) + seq_len(sum(missing_cal))
    cal <- vec_rbind(cal, cal_x[missing_cal,])
  }
  field(x, "c") <- pos[field(x, "c")]
  attr(x, "cal") <- cal
  x
}

#' @export
vec_restore.moment <- function(x, to, ..., n = NULL) {
  if(!is_empty(x$c) & !any(is.na(x$c))) {
    attr(to, "cal") <- vec_slice(calendar_data(to), unique(x$c))
    x$c <- `attributes<-`(vctrs::vec_group_id(x$c), NULL)
  }
  NextMethod()
}

#' @export
vec_cast.Date.moment <- function(x, to, ...) {
  cal <- calendar_data(x)
  if(any(!cal$origin)) abort("Only moments with origins can be converted to dates.")
  unit <- vapply(cal$granularity, vec_ptype_full, character(1L))
  scale <- vapply(cal$granularity, vec_data, numeric(1L))
  cal_id <- field(x, "c")
  x <- field(x, "x")
  out <- vec_split(x, cal_id)
  origin <- .Date(0)
  out$val <- lapply(vec_seq_along(out), function(i) {
    u <- unit[out$key[[i]]]
    if(u == "week") origin <- origin - 3
    by <- paste(out$val[[i]] * scale[out$key[[i]]], u)
    vec_c(!!!lapply(by, function(x) seq(origin, by = by, length.out = 2)[2]))
  })
  vec_c(!!!out$val)
}

#' @export
vec_cast.double.moment <- function(x, to, ...) {
  field(x, "x")
}

#' @export
vec_cast.character.moment <- function(x, to, ...) {
  format(x)
}

#' @export
seq.moment <- function(from, to, by, length.out, along.with, ...){
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
