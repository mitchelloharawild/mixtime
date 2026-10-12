#' Constructor for mixtime vectors
#' 
#' Creates a `mixtime` vector, which can contain time points of different 
#' granularities (e.g. monthly and quarterly) in a single vector via `vecvec`.
#' 
#' @param x A mixtime time vector (e.g. created with [mt_linear()]) to wrap in a mixtime class.
#' 
#' @return A `mixtime` object, which allows mixed-type time vectors to coexist in a single vector.
#' 
#' @export
new_mixtime <- function(x = mt_linear()) {
  stopifnot(S7_inherits(x, mt_time))
  class_mixtime(list(x))
}

#' Create a mixtime vector
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param data A vector of time values. This can be a character vector (e.g. "2024-01-01"),
#'  a numeric vector (e.g. seconds since epoch), or a time class (e.g. Date, POSIXct, yearmonth, etc.).
#' @param chronon A time granule object representing the smallest indivisible time granule (chronon) for the mixtime. This is used to interpret the numeric values in `data` and to define the time resolution of the mixtime. If not provided, it will be inferred from `data`.
#' @param cycle An optional time granule object representing the cycle for cyclical time. This is used to define the repeating cycle for cyclical time representations (e.g. day-of-week, month-of-year). If not provided, the mixtime will be treated as linear time.
#' @param discrete A logical indicating whether the time values should be treated as discrete (integer) or continuous (fractional). This affects how numeric values are interpreted and how time arithmetic is performed. The default is `TRUE` (discrete).
#' 
#' @return A `mixtime` object representing the time values in `data` according to the specified `chronon` and `cycle`.
#' 
#' @examples
#' # Create a mixtime for today
#' mixtime(Sys.Date())
#' 
#' # Create a mixtime for the current date and time
#' mixtime(Sys.time())
#' 
#' # Convert time from tsibble classes to mixtime
#' mixtime(tsibble::yearmonth("2024 Jan"))
#' 
#' # Create a mixtime for the time of day (cyclical time)
#' mixtime(Sys.time(), cycle = cal_gregorian$day(1L))
#' 
#' # Specify a timezone for the chronon
#' mixtime(Sys.time(), chronon = cal_gregorian$second(1L, tz = Sys.timezone()))
#' mixtime(Sys.time(), chronon = cal_gregorian$second(1L, tz = "Pacific/Honolulu"))
#' mixtime(Sys.time(), chronon = cal_gregorian$second(1L, tz = "Australia/Melbourne"))
#' 
#' # Dates (and all granularities) can have timezones
#' mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = Sys.timezone()))
#' mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = "Pacific/Honolulu"))
#' mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = "Australia/Melbourne"))
#' 
#' # Continuous time tracks progress within the chronon
#' mixtime(Sys.time(), chronon = cal_gregorian$day(1L, tz = Sys.timezone()), discrete = FALSE)
#' 
#' # Mixtime can combine different granularities and timezones in a vector
#' now <- Sys.time()
#' c(
#'   # Datetime (second chronon) in UTC
#'   mixtime(now),
#'   # Date (minute chronon) in local timezone
#'   mixtime(now, chronon = cal_gregorian$minute(1L, tz = Sys.timezone())),
#'   # Month (month chronon) in UTC
#'   mixtime(now, chronon = cal_gregorian$month(1L))
#' )
#' @export
mixtime <- function(data, chronon = time_chronon(data), cycle = time_cycle(data), discrete = TRUE) {
  # Handle duration-based chronon and cycle inputs
  if (is_mixtime(chronon)) {
    if (length(chronon@x) > 1L) {
      cli::cli_abort(
        c(
          "mixtime() currently only supports singular granule chronons",
          i = "To mix chronons, combine several mixtime vectors with {.fun c}."
        ),
        call = NULL
      )
    }
    chronon_duration <- chronon@x[[1L]]
    chronon <- attr(chronon_duration, "chronon")
    if (length(chronon_duration) == 1L) {
      # Scale the granule by the chronon duration (e.g. 3 months -> 3 * month(1L))
      chronon@n <- chronon@n * as.numeric(chronon_duration)
    }
  }
  if (is_mixtime(cycle)) {
    if (length(cycle@x) > 1L) {
      cli::cli_abort(
        c(
          "mixtime() currently only supports singular granule cycles",
          i = "To mix cycles, combine several mixtime vectors with {.fun c}."
        ),
        call = NULL
      )
    }
    cycle_duration <- cycle@x[[1L]]
    if (anyNA(cycle_duration)) {
      cycle <- NULL
    } else {
      cycle <- attr(cycle_duration, "chronon")
      if (length(cycle_duration) == 1L) {
        # Scale the granule by the cycle duration (e.g. 3 months -> 3 * month(1L))
        cycle@n <- cycle@n * as.numeric(cycle_duration)
      }
    }
  }
  
  # Add default granule properties if not given in chronon or cycle
  chronon <- granule_inherit_props(chronon, chronon_glb(data))
  if (!is.null(cycle)) {
    cycle <- granule_inherit_props(cycle, chronon)
  }

  # Validate time granules
  if (!inherits(chronon, "mixtime::mt_unit")) {
    cli::cli_abort("{.var chronon} must be a time granule object.", call. = FALSE)
  }
  if (!is.null(cycle) && !inherits(cycle, "mixtime::mt_unit")) {
    cli::cli_abort("{.var cycle} must be a time granule object.", call. = FALSE)
  }

  # Parse text data
  if (is.character(data)) {
    return(time_parse(data, chronon = chronon, cycle = cycle, discrete = discrete))
  } else {
    # Apply origin offset for numeric data
    if (!is_mixtime(data) && is.numeric(data) && (epoch <- chronon_epoch(chronon)) != 0L) {
      data <- data - epoch
    }

    # Cast from Date, POSIXct, etc.
    if (!is.numeric(data) || !is.null(attributes(data))) {
      # TODO: Escalate this deprecation warning to an error in the next release,
      # by removing the handler (and `mixtime_user_env()`) and leaving
      # `allow_indeterminate = FALSE`.
      frame <- sys.nframe()
      data <- tryCatch(
        chronon_convert(
          data,
          chronon,
          discrete = discrete,
          allow_indeterminate = FALSE
        ),
        mixtime_error_indeterminate = function(cnd) {
          lifecycle::deprecate_warn(
            "0.4.0",
            I("Converting a discrete time to a chronon it doesn't nest within"),
            details = c(
              i = "The result is temporally indeterminate, so each time point is converted from its start.",
              i = "This will become an error in the next release. Convert from a time point that is precise enough, or use continuous time."
            ),
            user_env = mixtime_user_env(frame)
          )
          chronon_convert(data, chronon, discrete = discrete)
        }
      )
    }
  }

  new_mixtime(
    if (is.null(cycle)) {
      mt_linear(data, chronon = chronon)
    } else {
      mt_cyclical(data, chronon = chronon, cycle = cycle)
    }
  )
}

# The environment that called into mixtime, past mixtime's own wrappers (e.g.
# `yearmonth()` -> `linear_time()` -> `mixtime()`), so that deprecation warnings
# name the package responsible, and are recognised as direct calls by lifecycle
# (which `lifecycle::expect_deprecated()` needs to warn on every call).
# `frame` is the number of the frame to start from, i.e. `mixtime()`'s own.
mixtime_user_env <- function(frame) {
  ns <- topenv(environment(mixtime_user_env))
  parents <- sys.parents()
  i <- frame
  while (i > 0L && identical(topenv(environment(sys.function(i))), ns)) {
    # Frames can be their own parent (e.g. under dplyr's data masking), so stop
    # rather than loop if the walk no longer moves outwards.
    if (parents[i] >= i) break
    i <- parents[i]
  }
  if (i == 0L) globalenv() else sys.frame(i)
}

#' Check if an object is a mixtime
#'
#' Tests whether `x` inherits from the `mixtime` class.
#'
#' @param x An object to test.
#'
#' @return A scalar logical: `TRUE` if `x` is a `mixtime` vector, `FALSE`
#'   otherwise.
#'
#' @seealso [as_mixtime()] to coerce objects to `mixtime`,
#'   [mixtime()] to construct a `mixtime`.
#'
#' @examples
#' is_mixtime(Sys.Date())
#' is_mixtime(mixtime(Sys.Date()))
#'
#' @export
is_mixtime <- function(x) {
  S7_inherits(x, class_mixtime)
}

#' @method Summary mixtime::mixtime
#' @export
`Summary.mixtime::mixtime` <- function(..., na.rm = FALSE) {
  res <- NextMethod()
  if (S7_inherits(res, mt_time)) new_mixtime(res) else res
}

method(c, class_mixtime) <- function(...) {
  dots <- unname(rlang::list2(...))
  is_mt <- vapply(dots, is_mixtime, logical(1L))
  dots[!is_mt] <- lapply(dots[!is_mt], vec_cast_to_mixtime, to = new_mixtime())
  do.call(utils::getS3method("c", "vecvec::vecvec"), dots)
}
