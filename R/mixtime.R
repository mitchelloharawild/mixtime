#' Constructor for mixtime vectors
#' 
#' Creates a `mixtime` vector, which can contain time points of different 
#' granularities (e.g. monthly and quarterly) in a single vector via `vecvec`.
#' 
#' @param x A mixtime time vector (created with [new_time()]) to wrap in a mixtime class.
#' 
#' @return A `mixtime` object, which allows mixed-type time vectors to coexist in a single vector.
#' 
#' @export
new_mixtime <- function(x = new_time()) {
  stopifnot(inherits(x, "mt_time"))
  class_mixtime(list(x))
}

#' Create a mixtime vector
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param data A vector of time values. This can be a character vector (e.g. "2024-01-01"),
#'  a numeric vector (e.g. seconds since epoch), or a time class (e.g. Date, POSIXct, yearmonth, etc.).
#' @param chronon A time unit object representing the smallest indivisible time unit (chronon) for the mixtime. This is used to interpret the numeric values in `data` and to define the time resolution of the mixtime. If not provided, it will be inferred from `data`.
#' @param cycle An optional time unit object representing the cycle for cyclical time. This is used to define the repeating cycle for cyclical time representations (e.g. day-of-week, month-of-year). If not provided, the mixtime will be treated as linear time.
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
#' # Convert time from tsibble units to mixtime
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
  # Add default tz if not given in chronon or cycle
  if (S7::prop_exists(chronon, "tz") && !nzchar(chronon@tz)){
    # TODO - handle case where data has multiple timezones
    # Requires splitting up data into groups with the same timezone
    chronon@tz <- tz_name(data[1L])
  }
  if (!is.null(cycle) && S7::prop_exists(cycle, "tz") && !nzchar(cycle@tz)){
    cycle@tz <- chronon@tz
  }

  # Parse text data
  if (is.character(data)) {
    data <- as.POSIXct(data, tz = tz_name(chronon))
  }
  
  # Apply origin offset for numeric data
  if (is.numeric(data) && (epoch <- chronon_epoch(chronon)) != 0L) {
    data <- data - epoch
  }

  # Validate time units
  if (!inherits(chronon, "mixtime::mt_unit")) {
    cli::cli_abort("{.var chronon} must be a time unit object.", call. = FALSE)
  }
  if (!is.null(cycle) && !inherits(cycle, "mixtime::mt_unit")) {
    cli::cli_abort("{.var cycle} must be a time unit object.", call. = FALSE)
  }

  # Cast from Date, POSIXct, etc.
  if (!is.numeric(data) || !is.null(attributes(data))) {
    data <- chronon_convert(
      data,
      chronon,
      discrete = discrete
    )
  }

  new_mixtime(
    new_time(
      data,
      chronon,
      cycle,
      if (is.null(cycle)) "mt_linear" else "mt_cyclical"
    )
  )
}

#' Convert a time class into a mixtime
#'
#' Coerces a time object (e.g. `Date`, `POSIXct`, `yearmonth`) to a `mixtime`
#' vector using [vctrs::vec_cast()]. The chronon and cycle are inferred from
#' `x` via [time_chronon()] and [time_cycle()].
#'
#' @param x A time value to convert to a `mixtime`. Any time class with a defined `time_chronon()` method can be converted (e.g. `Date`, `POSIXct`, `yearmonth`, etc.).
#' @param ... Additional arguments passed to the underlying [vec_cast()] method.
#'
#' @return A `mixtime` object corresponding to `x`.
#'
#' @seealso [mixtime()] for constructing a `mixtime` directly from data,
#'   [is_mixtime()] for testing if an object is a `mixtime`.
#'
#' @examples
#' as_mixtime(Sys.Date())
#' as_mixtime(Sys.time())
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, new_mixtime())
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
  S7::S7_inherits(x, class_mixtime)
}

#' @export
method(vec_ptype_full, class_mixtime) <- function(x, ...) "mixtime"

#' @export
method(vec_ptype_abbr, class_mixtime) <- function(x, ...) "mixtime"


time_valid <- function(x) {
  if (is_mixtime(x)) return(TRUE)
  !inherits(try(time_chronon(x), silent = TRUE), "try-error")
}

#' @method vec_ptype2 mixtime::mixtime
#' @export
`vec_ptype2.mixtime::mixtime` <- function(x, y, ...) {
  x_is_time <- time_valid(x)
  y_is_time <- time_valid(y)

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

#' @importFrom vctrs vec_proxy_order
method(vec_proxy_order, class_mixtime) <- function(x, ...) {
  if (length(x@x) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(x@x, time_chronon)
    chronon_type <- chronon_common(!!!chronons)

    x@x <- lapply(x@x, function(v) {
      if (is.integer(v)) v <- v + 0.5
      chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vecvec::unvecvec(x))
}
