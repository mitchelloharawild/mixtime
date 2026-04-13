method(vec_proxy_equal, class_mixtime) <- function(x, ...) {
  data_frame(
    x = as.integer(x),
    g = unvecvec(vecvec_apply(x, function(x) rlang::hash(time_chronon(x))))
  )
}