#' Constructor for mixtime time vectors
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `new_time()` was the low-level constructor for `mt_time` vectors. It has been
#' deprecated in favour of calling the concrete time class constructors directly:
#' [mt_linear()] for linear time, [mt_duration()] for durations, and
#' [mt_cyclical()] for cyclical time.
#'
#' Creates a `mixtime` time vector at a specific time point, with a specified
#' chronon and optional cycle. The `chronon` defines the smallest indivisible
#' time granule for the time vector, while the `cycle` allows for cyclical time
#' representations (e.g. day-of-week, month-of-year).
#' 
#' @param x A numeric vector of time points, integers for discrete time or 
#'   doubles for continuous time. 
#' @param chronon A time granule object representing the smallest indivisible time
#'   granule (chronon) for the time vector (e.g. `cal_gregorian$day(1L)`). 
#' @param cycle An optional time granule object representing the cycle for cyclical
#'   time (e.g. `cal_gregorian$week(1L)` for day-of-week). If not provided, the 
#'   time vector will be treated as linear time.
#' @param class An optional character vector of additional S3 classes to assign 
#'   to the resulting time vector. This allows for further subclassing of 
#'   `mt_time` for specific time types (e.g. linear, cyclical, durations, etc.).
#' 
#' @return A `mt_time` vector representing the time points in `x` according to 
#'   the specified `chronon` and `cycle`.
#' 
#' @examples
#' # Create a continuous mixtime time vector for today
#' new_time(
#'   as.double(Sys.Date()),
#'   chronon = cal_gregorian$day(1L, tz = Sys.timezone()),
#'   class = "mt_linear"
#' )
#'
#' # Create a discrete mixtime time vector for the current date and time
#' new_time(
#'   as.integer(Sys.time()),
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone()),
#'   class = "mt_linear"
#' )
#' 
#' # Create a discrete mixtime time vector for the time of day (cyclical time)
#' new_time(
#'   as.integer(Sys.time()), 
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone()), 
#'   cycle = cal_gregorian$day(1L, tz = Sys.timezone()),
#'   class = "mt_cyclical"
#' )
#' 
#' @export
new_time <- function(x = integer(), chronon = mt_unit(1L), cycle = NULL, class = NULL) {
  # Route to the concrete S7 time class. The `chronon`/`cycle` length is validated
  # by the typed properties of these classes (see `mt_time-class`).
  class <- class %||% if (is.null(cycle)) "mt_linear" else "mt_cyclical"
  # Validate `class` before warning so the deprecation message can point at a
  # valid replacement constructor.
  if (!class %in% c("mt_linear", "mt_duration", "mt_cyclical")) {
    cli::cli_abort("Unknown time class {.val {class}}.", call. = FALSE)
  }
  lifecycle::deprecate_warn(
    "0.2.0",
    "new_time()",
    with = paste0(class, "()")
  )
  switch(
    class,
    mt_linear = mt_linear(x, chronon = chronon),
    mt_duration = mt_duration(x, chronon = chronon),
    mt_cyclical = {
      if (is.null(cycle)) {
        cli::cli_abort("{.var cycle} is required for cyclical time.", call. = FALSE)
      }
      mt_cyclical(x, chronon = chronon, cycle = cycle)
    }
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

#' @export
S7::method(c, mt_time) <- function(...) {
  dots <- list(...)
  x <- dots[[1L]]
  S7::S7_data(x) <- do.call(base::c, lapply(dots, S7::S7_data))
  x
}
