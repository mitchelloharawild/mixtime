index_valid.moment <- function(x) {
  TRUE
}

interval_pull.moment <- function(x) {
  require_package("tsibble")
  intvl <- paste(format(calendar_data(x)$granularity, abbr = TRUE), collapse = ", ")
  val <- vctrs::new_vctr(1, class = "interval_hide_number")
  tsibble::new_interval(.others = setNames(list(val), intvl))
}

# Sorry Earo for this terrible hack for formatting the interval.
# I hope that a tsibble can display and represent multiple intervals some day.
#' @export
format.interval_hide_number <- function(x, ...) {
  ""
}
#' @export
vec_ptype2.interval_hide_number.double <- function(x, y, ...) {
  vctrs::new_vctr(1, class = "interval_hide_number")
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
