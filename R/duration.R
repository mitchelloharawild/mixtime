#' Constructor for mixtime duration vectors
#' 
#' @description
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Constructs a new duration object (`mt_duration`) from a set of mixtime time 
#' units. This functionality is in active development and is not ready for use.
#' 
#' @inheritParams new_time
#'
#' @return An object of class `mt_duration` representing the specified duration.
#'
#' @details
#' 
#' This is a low-level constructor function that creates duration objects.
new_duration <- function(x = integer(), chronon = NULL) {
  new_time(x, chronon = chronon, class = "mt_duration")
}

#' @export
format.mt_duration <- function(x, ...) {
  paste(vec_data(x), time_unit_full(attr(x, "chronon")))
}

#' Duration vectors
#' 
#' `duration()` creates a vector of durations with a specified chronon (time
#' unit). Durations represent a fixed span of time measured in a given unit
#' (e.g., 3 months, 5 days), without reference to a specific point in time.
#' 
#' @param x A numeric vector of duration magnitudes.
#' @param chronon A time unit expression representing the chronon (unit of the
#'   duration), evaluated in the context of `calendar`. Use unquoted expressions
#'   like `month(1L)` or `day(1L)`. Chronons from a specific calendar can also
#'   be used (e.g. `cal_gregorian$month(1L)`). Defaults to the time chronon of
#'   the input `data` (`time_chronon(data)`).
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
  x, chronon = time_chronon(data), calendar = time_calendar(data)
) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var x} must be a numeric vector.", call. = FALSE)
  }

  # Evaluate chronon and cycle with a calendar mask
  quo_chronon <- enquo(chronon)
  chronon <- eval_tidy(quo_chronon, data = calendar, env = emptyenv())

  if (!inherits(chronon, "mixtime::mt_unit")) {
    cli::cli_abort("{.var chronon} must be a time unit object.", call. = FALSE)
  }
  new_mixtime(new_duration(x, chronon = chronon))
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
  function(n) {
    new_duration(n, chronon = chronon)
  }
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
#' @param n A numeric vector of duration magnitudes.
#' @param calendar A calendar system used to evaluate the chronon. Defaults to
#'   the calendar associated with the input data `n` (via 
#'   `time_calendar(data)`). If `n` is numeric, the default calendar is
#'   `cal_gregorian`.
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
