#' Create a new mixtime
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The iterable value of the mixtime (for example, a value of 1 may indicate the first day).
#' @param calendar The calendar structure over which the mixtime iterates. This can be created using `new_calendar()`.
#'
#' @importFrom rlang is_empty
#' @export
new_mixtime <- function(x = numeric(), calendar = new_calendar()) {
  x <- vec_cast(x, double())
  # calendar[[".rows"]] <- new_list_of(if(is_empty(x)) list() else list(seq_along(x)), ptype = integer())
  vctrs::new_rcrd(list(x = x, c = vec_rep(1L, length(x))), cal = calendar, class = "mixtime")
}

#' @export
mixtime <- function(x, ...) {
  UseMethod("mixtime")
}

#' @export
mixtime.default <- function(x, ...){
  vec_cast(x, new_mixtime())
}

#' Convert time class into a mixtime
#'
#' @param x A time value to convert to a mixtime
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, new_mixtime())
}

#' Check if the object is a mixtime
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `mixtime` class.
#'
#' @examples
#' is_mixtime(Sys.Date())
#' is_mixtime(yearmonth(1))
#'
#' @export
is_mixtime <- function(x) {
  inherits(x, "mixtime")
}

#' @export
format.mixtime <- function(x, ...) {
  cal <- calendar_data(x)
  val <- field(x, "x")
  cal_id <- field(x, "c")
  fmt <- vec_group_loc(cal_id)
  fmt$tu <- cal$granularity[fmt$key]
  fmt$origin <- cal$origin[fmt$key]
  fmt$val <- lapply(fmt$loc, vec_slice, x = val)
  out <- mapply(format_time, tu = fmt$tu, fmt$val, origin = fmt$origin, SIMPLIFY = FALSE)
  vec_c(!!!out)[order(vec_c(!!!fmt$loc))]
}

#' @export
`names<-.mixtime` <- function(x, value) {
  x
}

#' @export
calendar_data.mixtime <- function(x) {
  attr(x, "cal")
}

#' @export
#' @method vec_arith mixtime
vec_arith.mixtime <- function(op, x, y, ...){
  UseMethod("vec_arith.mixtime", y)
}

#' @export
#' @method vec_math mixtime
vec_math.mixtime <- function(op, x, ...){
  field(x, "x") <- vec_math(op, field(x, "x"))
  if(is.logical(field(x, "x"))) return(field(x, "x"))
  x
}

#' @export
#' @method vec_arith.mixtime mixtime
vec_arith.mixtime.mixtime <- function(op, x, y, ...) {
  x_cal <- calendar_data(x)
  y_cal <- calendar_data(y)
  if(x_cal$origin && y_cal$origin) {
    if(op != "-") abort(str_glue("Cannot use operation {op} with mixtimes that both have origins."))
    x_cal$origin <- FALSE
    new_mixtime(do.call(op, list(field(x, "x"), field(y, "x"))), x_cal)
  } else if(x_cal$origin || y_cal$origin) {
    if(!vec_in(op, c("-", "+"))) abort(str_glue("Cannot use operation {op} with mixtimes that have origins."))
    origin_cal <- if(x_cal$origin) x_cal else y_cal
    duration_cal <- if(x_cal$origin) y_cal else x_cal
    duration_cal$granularity <- vec_cast(duration_cal$granularity, origin_cal$granularity)
    new_mixtime(do.call(op, list(field(x, "x"), field(y, "x"))), origin_cal)
  } else {
    new_mixtime(do.call(op, list(field(x, "x"), field(y, "x"))), x_cal)
  }
}

#' @export
#' @method vec_arith.mixtime numeric
vec_arith.mixtime.numeric <- function(op, x, y, ...) {
  x_cal <- calendar_data(x)
  if(!vec_in(op, c("+", "-"))) {
    abort(str_glue("Cannot use operation {op} for a <mixtime> and <double>."))
  }
  x <- vec_recycle_common(x, y)
  field(x[[1]], "x") <- vec_arith(op, field(x[[1]], "x"), x[[2]], ...)
  x[[1]]
}

#' @export
#' @method vec_arith.numeric mixtime
vec_arith.numeric.mixtime <- function(op, x, y, ...) {
  vec_arith.mixtime.numeric(op, y, x, ...)
}

#' @export
vec_ptype2.mixtime.mixtime <- function(x, y, ...) {
  x_cal <- calendar_data(x)
  y_cal <- calendar_data(y)
  field(y, "c") <- field(y, "c") + vec_size(x_cal)
  attr(x, "cal") <- vec_unique(vec_rbind(x_cal, y_cal))
  x
}

#' @export
vec_cast.mixtime.mixtime <- function(x, to, ...) {
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
vec_restore.mixtime <- function(x, to, ..., n = NULL) {
  if(!is_empty(x$c) & !any(is.na(x$c))) {
    attr(to, "cal") <- vec_slice(calendar_data(to), unique(x$c))
    x$c <- `attributes<-`(vctrs::vec_group_id(x$c), NULL)
  }
  NextMethod()
}

#' @export
vec_cast.Date.mixtime <- function(x, to, ...) {
  cal <- calendar_data(x)
  if(any(!cal$origin)) abort("Only mixtimes with origins can be converted to dates.")
  unit <- vapply(cal$granularity, vec_ptype_full, character(1L))
  scale <- vapply(cal$granularity, vec_data, numeric(1L))
  cal_id <- field(x, "c")
  x <- field(x, "x")
  out <- vec_group_loc(cal_id)
  origin <- .Date(0)
  out$val <- lapply(vec_seq_along(out), function(i) {
    u <- unit[out$key[[i]]]
    if(u == "week") origin <- origin - 3
    by <- paste(x[out$loc[[i]]] * scale[out$key[[i]]], u)
    vec_c(!!!lapply(by, function(x) seq(origin, by = x, length.out = 2)[2]))
  })
  vec_c(!!!out$val)[order(vec_c(!!!out$loc))]
}

#' @export
vec_cast.double.mixtime <- function(x, to, ...) {
  field(x, "x")
}

#' @export
vec_cast.character.mixtime <- function(x, to, ...) {
  format(x)
}

#' @export
seq.mixtime <- function(from, to, by, length.out, along.with, ...){
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
