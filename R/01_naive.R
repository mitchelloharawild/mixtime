# A naive time granule property inherits from specified properties
naive <- function(x = NA) structure(x, class = "mt_naive")

#' @export
format.mt_naive <- function(x, ...) {
  paste0("naive [", NextMethod(), "]")
}

is.naive <- function(x) {
  vapply(x, inherits, logical(1L), "mt_naive")
}