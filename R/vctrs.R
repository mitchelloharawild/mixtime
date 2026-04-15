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