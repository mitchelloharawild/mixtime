index_valid.mixtime <- function(x) {
  TRUE
}

interval_pull.mixtime <- function(x) {
  require_package("tsibble")
  tsibble::interval_pull(calendar_data(x)$granularity)
}

# Sorry Earo for this terrible hack for formatting the interval.
# I hope that a tsibble can display and represent multiple intervals some day.
#' @export
format.interval_hide_number <- function(x, ...) {
  ""
}
#' @export
vec_ptype2.interval_hide_number.double <- function(x, y, ...) {
  vctrs::new_vctr(1, class = c("interval_hide_number", "numeric"), inherit_base_type = TRUE)
}
#' @export
vec_ptype2.double.interval_hide_number <- vec_ptype2.interval_hide_number.double
#' @export
vec_ptype2.interval_hide_number.character <- function(x, y, ...) {
  character()
}
#' @export
vec_cast.character.interval_hide_number <- function(x, to, ...) {
  format(x)
}
#' @export
vec_cast.double.interval_hide_number <- function(x, to, ...) {
  vec_proxy(x)
}
