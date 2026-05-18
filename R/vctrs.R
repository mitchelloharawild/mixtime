# ----------------------------------------------------------------
# class_mixtime methods
# ----------------------------------------------------------------

#' @export
method(vec_ptype_full, class_mixtime) <- function(x, ...) "mixtime"

#' @export
method(vec_ptype_abbr, class_mixtime) <- function(x, ...) "mixtime"


time_valid <- function(x) {
  if (is_mixtime(x)) return(TRUE)
  !inherits(try(time_chronon(x), silent = TRUE), "try-error")
}

#' @method vec_ptype2 mixtime::mixtime
#' @export
`vec_ptype2.mixtime::mixtime` <- function(x, y, ...) {
  x_is_time <- time_valid(x)
  y_is_time <- time_valid(y)

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

#' @importFrom vctrs vec_proxy_order
method(vec_proxy_order, class_mixtime) <- function(x, ...) {
  if (length(x@x) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(x@x, function(v) attr(v, "chronon"))
    chronon_type <- chronon_common_impl(chronons)

    x@x <- lapply(x@x, function(v) {
      if (is.integer(v)) v <- v + 0.5
      chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vecvec::unvecvec(x))
}

#' @importFrom vctrs vec_proxy_equal
method(vec_proxy_equal, class_mixtime) <- function(x, ...) {
  data_frame(
    x = as.integer(x),
    g = unvecvec(vecvec_apply(x, function(x) rep(rlang::hash(attr(x, "chronon")), length(x))))
  )
}

# ----------------------------------------------------------------
# mt_linear methods
# ----------------------------------------------------------------

#' @method vec_cast.character mt_linear
#' @export
vec_cast.character.mt_linear <- function(x, to, ...) {
  time_format_impl(x)
}

#' @method vec_cast.integer mt_linear
#' @export
vec_cast.integer.mt_linear <- function(x, to, ...) {
  vec_cast(vec_data(x), integer())
}

#' @method vec_cast.double mt_linear
#' @export
vec_cast.double.mt_linear <- function(x, to, ...) {
  vec_cast(vec_data(x), double())
}

#' @export
vec_cast.mt_linear.integer <- function(x, to, ...) {
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_cast.mt_linear.double <- function(x, to, ...) {
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_ptype2.mt_linear.mt_linear <- function(x, y, ..., x_arg, y_arg) {
  new_time(
    vec_ptype2(vec_data(x), vec_data(y)),
    chronon = chronon_common_impl(
      list(attr(x, "chronon"), attr(y, "chronon"))
    ),
    class = "mt_linear"
  )
}

#' @export
vec_cast.mt_linear.mt_linear <- function(x, to, ..., x_arg, to_arg) {
  x <- chronon_convert(x, attr(to, "chronon"), discrete = is.integer(to))
  attributes(x) <- attributes(to)
  x
}

#' @method vec_restore mt_linear
#' @export
vec_restore.mt_linear <- function(x, to, ..., x_arg, to_arg) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var x} must be a numeric vector.", call. = FALSE)
  }
  attributes(x) <- attributes(to)
  x
}

# ----------------------------------------------------------------
# mt_duration methods
# ----------------------------------------------------------------

#' @method vec_cast.character mt_duration
#' @export
vec_cast.character.mt_duration <- function(x, to, ...) {
  format(x)
}

#' @export
vec_cast.mt_duration.integer <- vec_cast.mt_linear.integer

#' @export
vec_cast.mt_duration.mt_duration <- function(x, to, ..., x_arg, to_arg) {
  discrete <- is.integer(vec_data(to))
  x <- as.numeric(x) * chronon_cardinality(attr(to, "chronon"), attr(x, "chronon"))
  if (discrete) x <- as.integer(x)
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_ptype2.mt_duration.mt_duration <- function(x, y, ..., x_arg, y_arg) {
  new_time(
    vec_ptype2(vec_data(x), vec_data(y)),
    chronon = chronon_common_impl(list(attr(x, "chronon"), attr(y, "chronon"))),
    class = "mt_duration"
  )
}

#' @export
vec_ptype2.mt_duration.double <- function(x, y, ..., x_arg, y_arg) x

#' @export
vec_ptype2.double.mt_duration <- function(x, y, ..., x_arg, y_arg) y

#' @export
vec_cast.mt_duration.double <- function(x, to, ..., x_arg, to_arg) {
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_ptype2.mt_duration.integer <- function(x, y, ..., x_arg, y_arg) x

#' @export
vec_ptype2.integer.mt_duration <- function(x, y, ..., x_arg, y_arg) y

#' @method vec_restore mt_duration
#' @export
vec_restore.mt_duration <- vec_restore.mt_linear

# ----------------------------------------------------------------
# mt_cyclical methods
# ----------------------------------------------------------------

#' @method vec_cast.character mt_cyclical
#' @export
vec_cast.character.mt_cyclical <- function(x, to, ...) {
  time_format_impl(x)
}

#' @method vec_cast.integer mt_cyclical
#' @export
vec_cast.integer.mt_cyclical <- function(x, to, ...) {
  vec_cast(vec_data(x), integer())
}

#' @method vec_cast.double mt_cyclical
#' @export
vec_cast.double.mt_cyclical <- function(x, to, ...) {
  vec_cast(vec_data(x), double())
}

#' @export
vec_cast.mt_cyclical.integer <- function(x, to, ...) {
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_cast.mt_cyclical.double <- function(x, to, ...) {
  attributes(x) <- attributes(to)
  x
}

#' @export
vec_ptype2.mt_cyclical.mt_cyclical <- function(x, y, ..., x_arg, y_arg) {
  new_time(
    chronon = chronon_common_impl(list(attr(x, "chronon"), attr(y, "chronon"))),
    cycle = chronon_common_impl(list(attr(x, "cycle"), attr(y, "cycle"))),
    class = "mt_cyclical"
  )
}

#' @export
vec_cast.mt_cyclical.mt_cyclical <- function(x, to, ..., x_arg, to_arg) {
  x <- chronon_convert(x, attr(to, "chronon"), discrete = is.integer(to))
  attributes(x) <- attributes(to)
  x
}

#' @method vec_restore mt_cyclical
#' @export
vec_restore.mt_cyclical <- function(x, to, ..., x_arg, to_arg) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var x} must be a numeric vector.", call. = FALSE)
  }
  attributes(x) <- attributes(to)
  x
}
