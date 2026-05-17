#' @export
format.mt_duration <- function(x, ...) {
  # TODO - better pluralisation
  unit <- time_unit_full(attr(x, "chronon"))
  x <- vec_data(x)
  x_na <- is.na(x)
  out <- rep("NA", length(x))
  out[!x_na] <- paste0(x[!x_na], " ", unit, ifelse(x[!x_na] == 1, "", "s"))
  out
}

#' Duration vectors
#' 
#' `duration()` creates a vector of durations with a specified chronon.
#' Durations represent a fixed span of time measured in a given time granule
#' (e.g., 3 months, 5 days), without reference to a specific point in time.
#' 
#' @param data A time vector of duration magnitudes.
#' @param chronon A time granule expression representing the chronon, evaluated
#'  in the context of `calendar`. Use unquoted expressions like `month(1L)` or
#'  `day(1L)`. Chronons from a specific calendar can also be used (e.g. 
#'  `cal_gregorian$month(1L)`). Defaults to the time chronon of the input
#'  `data` (`time_chronon(data)`).
#' @param calendar Calendar system used to evaluate `chronon`. Defaults to
#'   `time_calendar(data)` for existing time objects. Common options include
#'   [cal_gregorian] and [cal_isoweek].
#' 
#' @return A `mixtime` vector containing an `mt_duration` vector.
#' 
#' @seealso 
#' - [new_duration_fn()] for creating reusable duration functions
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#' 
#' @examples
#' # A duration of 3 months
#' duration(3L, cal_gregorian$month(1L))
#' 
#' # A vector of durations in days
#' duration(1:7, cal_gregorian$day(1L))
#' 
#' @export
duration <- function(
  data, chronon = time_chronon(data), calendar = time_calendar(data)
) {
  if (!is.numeric(data)) {
    cli::cli_abort("{.var data} must be a numeric vector.", call. = FALSE)
  }

  # Evaluate chronon and cycle with a calendar mask
  quo_chronon <- enquo(chronon)
  chronon <- eval_tidy(quo_chronon, data = calendar, env = emptyenv())

  if (!inherits(chronon, "mixtime::mt_unit")) {
    cli::cli_abort("{.var chronon} must be a time granule object.", call. = FALSE)
  }
  new_mixtime(new_time(data, chronon = chronon, class = "mt_duration"))
}

#' @method vec_cast.integer mt_duration
#' @export
vec_cast.integer.mt_duration <- function(x, to, ...) {
  vec_cast(vec_data(x), integer())
}

#' @method vec_cast.double mt_duration
#' @export
vec_cast.double.mt_duration <- function(x, to, ...) {
  vec_cast(vec_data(x), double())
}

#' @importFrom vctrs vec_arith
#' @method vec_arith mt_duration
#' @export
vec_arith.mt_duration <- function(op, x, y, ...) {
  UseMethod("vec_arith.mt_duration", y)
}

#' @method vec_arith.mt_duration mt_duration
#' @export
vec_arith.mt_duration.mt_duration <- function(op, x, y, ...) {
  if (!op %in% c("-", "+", "/")) {
    cli::cli_abort("Only addition, subtraction, and division are supported between two durations.", call. = FALSE)
  }

  # Find common chronon for x and y as the basis for arithmetic operations.
  x_chronon <- attr(x, "chronon")
  y_chronon <- attr(y, "chronon")
  tu <- chronon_common_impl(list(x_chronon, y_chronon))

  # Scale magnitudes to common chronon units before performing arithmetic
  x <- vec_data(x) * chronon_cardinality(tu, x_chronon)
  y <- vec_data(y) * chronon_cardinality(tu, y_chronon)

  res <- vec_arith_base(op, x, y, ...)
  if (op == "/") {
    # duration / duration = numeric ratio
    return(res)
  }

  # Return a duration with the common chronon
  new_time(res, chronon = tu, class = "mt_duration")
}

#' @method vec_arith.mt_duration numeric
#' @export
vec_arith.mt_duration.numeric <- function(op, x, y, ...) {
  if (!op %in% c("+", "-", "*", "/")) {
    stop("Only addition, subtraction, multiplication, and division supported for time durations", call. = FALSE)
  }
  res <- vec_arith_base(op, x, y, ...)
  vec_restore(res, x)
}

#' @method vec_arith.numeric mt_duration
#' @export
vec_arith.numeric.mt_duration <- function(op, x, y, ...) {
  if (!op %in% c("+", "-", "*")) {
    stop("Only addition, subtraction, and multiplication supported for time durations", call. = FALSE)
  }
  res <- vec_arith_base(op, x, y, ...)
  vec_restore(res, y)
}

#' Duration function factory
#' 
#' `new_duration_fn()` creates a duration function for a specified chronon. A
#' chronon is the smallest indivisible time unit (e.g., days, months) that
#' defines what the numeric magnitudes in the resulting duration vector
#' represent.
#' 
#' @param chronon A bare call for a time unit object representing the chronon
#'   (e.g., `month(1L)`, `day(1L)`).
#' @param default_calendar A default calendar used to resolve the time units
#'   if they don't exist in the calendar of the input data (e.g.,
#'   `cal_gregorian`).
#' 
#' @return A function used to create duration vectors with a specific chronon.
#'   The returned function accepts:
#'   \describe{
#'     \item{`data`}{A numeric vector of duration magnitudes.}
#'     \item{`calendar`}{A calendar system used to evaluate `chronon`. Defaults
#'       to `time_calendar(data)`.}
#'     \item{`...`}{Additional arguments passed to the chronon (e.g., `tz` for
#'       timezones).}
#'   }
#' 
#' @seealso 
#' - [duration()] for creating duration vectors directly
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#' 
#' @examples
#' # Create a months duration function
#' months <- new_duration_fn(month(1L), default_calendar = cal_gregorian)
#' months(1:6)
#' 
#' # Create a days duration function
#' days <- new_duration_fn(day(1L), default_calendar = cal_gregorian)
#' days(1:7)
#' 
#' @export
new_duration_fn <- function(chronon, default_calendar = cal_gregorian) {
  chronon <- rlang::new_quosure(
    enexpr(chronon), 
    env = rlang::as_data_mask(default_calendar)
  )
  function(
    data, calendar = time_calendar(data), ...
  ) {
    # Add tz / loc to chronon
    chronon <- quo_add_dots(chronon, ...)

    duration(data, chronon = !!chronon, calendar = calendar)
  }
}

#' Duration helper functions
#'
#' Convenience functions for creating duration vectors of common time units.
#' Each function wraps [new_duration_fn()] for its respective chronon.
#'
#' @inheritParams duration
#' @param ... Additional arguments passed to the chronon (e.g. `tz` for
#'   timezones).
#'
#' @return A `mixtime` vector containing an `mt_duration` vector.
#'
#' @seealso
#' - [new_duration_fn()] for creating custom duration functions
#' - [duration()] for creating duration vectors directly
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#'
#' @examples
#' years(3L)
#' quarters(2L)
#' months(6L)
#' weeks(4L)
#' days(7L)
#' hours(12L)
#' minutes(30L)
#' seconds(45L)
#' milliseconds(500L)
#'
#' @name duration_helpers
NULL

#' @rdname duration_helpers
#' @export
years <- new_duration_fn(year(1L))

#' @rdname duration_helpers
#' @export
quarters <- new_duration_fn(quarter(1L))

#' @rdname duration_helpers
#' @export
months <- new_duration_fn(month(1L))

#' @rdname duration_helpers
#' @export
weeks <- new_duration_fn(week(1L), default_calendar = cal_isoweek)

#' @rdname duration_helpers
#' @export
days <- new_duration_fn(day(1L))

#' @rdname duration_helpers
#' @export
hours <- new_duration_fn(hour(1L))

#' @rdname duration_helpers
#' @export
minutes <- new_duration_fn(minute(1L))

#' @rdname duration_helpers
#' @export
seconds <- new_duration_fn(second(1L))

#' @rdname duration_helpers
#' @export
milliseconds <- new_duration_fn(millisecond(1L))
