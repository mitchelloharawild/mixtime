#' Duration vectors
#' 
#' `duration()` creates a vector of durations with a specified chronon.
#' Durations represent a fixed span of time measured in a given time granule
#' (e.g., 3 months, 5 days), without reference to a specific point in time.
#' 
#' @param data A time vector of duration magnitudes, or an existing
#'  [duration()] vector to convert to `chronon` granules.
#' @param chronon A time granule expression representing the chronon, evaluated
#'  in the context of `calendar`. Use unquoted expressions like `month(1L)` or
#'  `day(1L)`. Chronons from a specific calendar can also be used (e.g.
#'  `cal_gregorian$month(1L)`). Defaults to the time chronon of the input
#'  `data` (`time_chronon(data)`).
#' @param discrete Logical. If `TRUE` (default), returns integer durations 
#'   always rounding down (discrete time model). If `FALSE`, returns fractional 
#'   durations (continuous time model).
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
#' # Convert a duration of 4 days into weeks
#' duration(days(4), cal_isoweek$week(1L), discrete = FALSE)
#' duration(days(4), cal_isoweek$week(1L), discrete = TRUE)
#'
#' @export
duration <- function(
  data, chronon = time_chronon(data), discrete = NULL,
  calendar = time_calendar(data)
) {
  # Evaluate chronon and cycle with a calendar mask
  quo_chronon <- enquo(chronon)
  chronon <- eval_tidy(quo_chronon, data = calendar, env = emptyenv())

  if (!inherits(chronon, "mixtime::mt_unit")) {
    cli::cli_abort("{.var chronon} must be a time granule object.", call. = FALSE)
  }

  # Convert an existing duration into the requested chronon
  if (is_mixtime(data) && time_is_duration(data)[1L]) {
    if (length(data@x) > 1L) {
      cli::cli_abort(
        c(
          "{.fn duration} currently only supports converting durations with a single chronon.",
          i = "To combine several chronons, combine several duration vectors with {.fun c}."
        ),
        call = NULL
      )
    }
    raw <- data@x[[1L]]
    from_chronon <- attr(raw, "chronon")
    if (is.null(discrete)) discrete <- is.integer(vec_data(raw))
    x <- as.numeric(data) * chronon_cardinality(chronon, from_chronon)
    if (discrete) x <- as.integer(floor(x))
    return(new_mixtime(mt_duration(x, chronon = chronon)))
  }

  if (!is.numeric(data)) {
    cli::cli_abort("{.var data} must be a numeric vector.", call. = FALSE)
  }
  new_mixtime(mt_duration(data, chronon = chronon))
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
