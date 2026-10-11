#' Constructor for mixtime time vectors
#'
#' @description
#'
#' `r lifecycle::badge("defunct")`
#'
#' `new_time()` was the low-level constructor for `mt_time` vectors. It is now
#' defunct; call the concrete time class constructors directly instead:
#' [mt_linear()] for linear time, [mt_duration()] for durations, and
#' [mt_cyclical()] for cyclical time.
#'
#' @param x,chronon,cycle,class Ignored.
#'
#' @export
new_time <- function(x = integer(), chronon = mt_unit(1L), cycle = NULL, class = NULL) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "new_time()",
    details = "Use `mt_linear()`, `mt_duration()`, or `mt_cyclical()` instead."
  )
}

# mt_time vector compatibility methods -----------------------------------------

#' @importFrom vctrs vec_restore
#' @export
S7::method(vec_restore, mt_time) <- function(x, to, ...) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var x} must be a numeric vector.", call. = FALSE)
  }
  attributes(x) <- attributes(to)
  x
}

#' @export
S7::method(`[`, mt_time) <- function(x, i, ...) {
  S7::S7_data(x) <- S7::S7_data(x)[i]
  x
}

#' @export
S7::method(`[[`, mt_time) <- function(x, i, ...) {
  S7::S7_data(x) <- S7::S7_data(x)[[i]]
  x
}

#' @method [<- mixtime::mt_time
#' @export
`[<-.mixtime::mt_time` <- function(x, i, value) {
  data <- S7::S7_data(x)
  data[i] <- S7::S7_data(value)
  S7::S7_data(x) <- data
  x
}

#' @method [[<- mixtime::mt_time
#' @export
`[[<-.mixtime::mt_time` <- function(x, i, value) {
  data <- S7::S7_data(x)
  data[[i]] <- S7::S7_data(value)
  S7::S7_data(x) <- data
  x
}

#' @export
S7::method(rep, mt_time) <- function(x, ...) {
  S7::S7_data(x) <- rep(S7::S7_data(x), ...)
  x
}

#' @method Summary mixtime::mt_time
#' @export
`Summary.mixtime::mt_time` <- function(..., na.rm = FALSE) {
  dots <- list(...)
  res <- do.call(
    .Generic,
    c(lapply(dots, function(x) unclass(S7::S7_data(x))), list(na.rm = na.rm))
  )
  # min/max/range return time points; any/all/sum/prod do not
  if (.Generic %in% c("min", "max", "range")) {
    x <- dots[[1L]]
    S7::S7_data(x) <- structure(res, class = "mt_time_data")
    x
  } else {
    res
  }
}

#' @method diff mixtime::mt_time
#' @export
`diff.mixtime::mt_time` <- function(x, lag = 1L, differences = 1L, ...) {
  if (length(lag) != 1L || length(differences) != 1L ||
      lag < 1L || differences < 1L) {
    cli::cli_abort(
      "{.arg lag} and {.arg differences} must be integers >= 1.",
      call. = FALSE
    )
  }
  # Use the class's own `-` method so the result keeps its time class and
  # chronon (e.g. differences of linear times are durations).
  for (i in seq_len(differences)) {
    idx <- seq_len(max(length(x) - lag, 0L))
    x <- x[idx + lag] - x[idx]
  }
  x
}

#' @export
S7::method(c, mt_time) <- function(...) {
  dots <- list(...)
  x <- dots[[1L]]
  S7::S7_data(x) <- do.call(base::c, lapply(dots, S7::S7_data))
  x
}
