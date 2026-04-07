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
  vecvec::new_vecvec(list(x), class = "mixtime")
}

#' Constructor for mixtime time vectors
#' 
#' Creates a `mixtime` time vector at a specific time point, with a specified 
#' chronon and optional cycle. The `chronon` defines the smallest indivisible
#' time unit for the time vector, while the `cycle` allows for cyclical time 
#' representations (e.g. day-of-week, month-of-year). 
#' 
#' @param x A numeric vector of time points, integers for discrete time or 
#'   doubles for continuous time. 
#' @param chronon A time unit object representing the smallest indivisible time
#'   unit (chronon) for the time vector (e.g. `cal_gregorian$day(1L)`). 
#' @param cycle An optional time unit object representing the cycle for cyclical
#'   time (e.g. `cal_gregorian$week(1L)` for day-of-week). If not provided, the 
#'   time vector will be treated as linear time.
#' 
#' @return A `mt_time` vector representing the time points in `x` according to 
#'   the specified `chronon` and `cycle`.
#' 
#' @examples
#' # Create a continuous mixtime time vector for today
#' new_time(
#'   as.double(Sys.Date()),
#'   chronon = cal_gregorian$day(1L, tz = Sys.timezone())
#' )
#'
#' # Create a discrete mixtime time vector for the current date and time
#' new_time(
#'   as.integer(Sys.time()),
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone())
#' )
#' 
#' # Create a discrete mixtime time vector for the time of day (cyclical time)
#' new_time(
#'   as.integer(Sys.time()), 
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone()), 
#'   cycle = cal_gregorian$day(1L, tz = Sys.timezone())
#' )
#' 
#' @export
new_time <- function(x, chronon = NULL, cycle = NULL) {
   vctrs::new_vctr(
    x,
    class = c(
      if (!is.null(cycle)) "mt_cyclical" else "mt_linear",
      "mt_time"
    ),
    chronon = chronon,
    cycle = cycle
  )
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
  if (is.numeric(data)) {
    data <- data - chronon_epoch(chronon)
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

  new_mixtime(new_time(data, chronon, cycle))
}

#' Convert time class into a mixtime
#'
#' @param x A time value to convert to a mixtime
#' @param ... Additional arguments for methods
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, new_mixtime())
}

#' Check if the object is a mixtime
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `mixtime` class.
#'
#' @examples
#' is_mixtime(Sys.Date())
#' is_mixtime(yearmonth(1))
#'
#' @export
is_mixtime <- function(x) {
  inherits(x, "mixtime")
}

#' @export
vec_ptype_full.mixtime <- function(x, ...) "mixtime"

#' @export
vec_ptype_abbr.mixtime <- function(x, ...) "mixtime"

vec_cast_to_mixtime <- function(x, to, ...) mixtime(x)

vec_cast_from_mixtime <- function(x, to, ...) {
  class(x) <- setdiff(class(x), "mixtime")
  vec_cast(x, to, ...)
}

## Custom vec cast methods since some time classes don't have cast methods

#' @export
#' @method vec_cast.character mixtime
vec_cast.character.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.character, ...)
  vecvec::unvecvec(x)
}

#' @export
#' @method vec_cast.double mixtime
vec_cast.double.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.double, ...)
  vecvec::unvecvec(x)
}

#' @export
vec_ptype2.mixtime <- function(x, y, ...) {
  x_is_time <- isTRUE(index_valid(x))
  y_is_time <- isTRUE(index_valid(y))

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  # new_mixtime()
  vecvec::new_vecvec(class = "mixtime")
}

#' @export
#' @importFrom vctrs vec_proxy_order
vec_proxy_order.mixtime <- function(x, ...) {
  if (length(attr(x, "v")) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(attr(x, "v"), time_chronon)
    chronon_type <- chronon_common(!!!chronons)

    attr(x, "v") <- lapply(attr(x, "v"), function(v) {
      if (is.integer(v)) v <- v + 0.5
      mixtime::chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vctrs::vec_data(vecvec::unvecvec(x)))
}
