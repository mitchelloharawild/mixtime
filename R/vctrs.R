method(vec_proxy_equal, class_mixtime) <- function(x, ...) {
  data_frame(
    x = as.integer(x),
    g = unvecvec(vecvec_apply(x, function(x) rlang::hash(time_chronon(x))))
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
    chronon = chronon_common(time_chronon(x), time_chronon(y)),
    class = "mt_linear"
  )
}

#' @export
vec_cast.mt_linear.mt_linear <- function(x, to, ..., x_arg, to_arg) {
  new_time(
    chronon_convert(x, time_chronon(to), discrete = is.integer(to)),
    chronon = chronon_common(time_chronon(x), time_chronon(to)),
    class = "mt_linear"
  )
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