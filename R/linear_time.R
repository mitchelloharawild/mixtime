#' Linear time function factory
#' 
#' `new_linear_time_fn()` creates a linear time function for a specified
#' chronon. A chronon is the smallest indivisible time unit (e.g., days, hours).
#' 
#' @param chronon A bare call for a time unit object representing the chronon (e.g., `day(1)`)
#' @param default_calendar A default calendar used to find the time units for 
#'   conversion if they don't exist in the calendar of the input data (e.g., `cal_isoweek`)
#' 
#' @return A function used to create linear time points with a specific chronon.
#' 
#' @examples
#' 
#' # NOTE: These examples need updating to define default granules/format strings.
#' 
#' # A year-month time representation with months as the chronon
#' ym <- new_linear_time_fn(month(1L))
#' ym(Sys.Date())
#' 
#' # A year-quarter-month time representation with months as the chronon
#' yqm <- new_linear_time_fn(month(1L))
#' yqm(1:100)
#' yqm(Sys.Date())
#' 
#' # A year-day time representation with days as the chronon
#' yd <- new_linear_time_fn(day(1L))
#' yd(Sys.Date())
#' 
#' # Gregorian date time with hourly precision
#' ymd_h <- new_linear_time_fn(hour(1L))
#' ymd_h(Sys.time())
#' 
#' @export
new_linear_time_fn <- function(chronon, default_calendar = cal_gregorian) {
  chronon <- rlang::new_quosure(
    enexpr(chronon), 
    env = rlang::as_data_mask(default_calendar)
  )

  function(
    data, discrete = TRUE, calendar = time_calendar(data), ...
  ) {
    # Add tz / loc to chronon
    chronon <- quo_add_dots(chronon, ...)

    linear_time(
      data, chronon = !!chronon, discrete = discrete, calendar = calendar
    )
  }
}

#' Linear time points
#' 
#' `linear_time()` creates a vector of linear time points with a specified
#' chronon (smallest time unit). This function is useful for creating custom
#' time representations that aren't covered by the convenience functions like
#' [yearmonth()] or [yearweek()].
#' 
#' @param data Input data to convert to linear time. Can be:
#'   - Numeric values (interpreted as chronons since Unix epoch)
#'   - Character strings (parsed as dates/times)
#'   - Date or POSIXct objects
#'   - Other time objects
#' @param chronon A time unit expression representing the chronon (smallest 
#'   indivisible time unit), evaluated in the context of `calendar`. Use 
#'   unquoted expressions like `month(1L)` or `hour(1L)`. Chronons from a
#'   specific calendar can also be used (e.g. `cal_isoweek$week(1L)`).
#'   Defaults to the time chronon of the input `data` (`time_chronon(data)`).
#' @param discrete Logical. If `TRUE` (default), returns integer chronons since
#'   Unix epoch (discrete time model). If `FALSE`, returns fractional chronons 
#'   allowing representation of partial time units (continuous time model).
#' @param calendar Calendar system used to evaluate `chronon` and `granules`.
#'   Defaults to `time_calendar(data)` for existing time objects. Common options
#'   include [cal_gregorian] and [cal_isoweek].
#' 
#' @return A `mixtime` time vector containing an `mt_linear` vector.
#' 
#' @seealso 
#' - [new_linear_time_fn()] for creating reusable linear time functions
#' - [yearmonth()], [yearquarter()], [year()] for Gregorian time representations
#' - [yearweek()] for ISO 8601 week-based time
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#' 
#' @examples
#' # Hourly time
#' linear_time(
#'   Sys.time(),
#'   chronon = hour(1L)
#' )
#' 
#' # Monthly time
#' linear_time(
#'   Sys.Date(),
#'   chronon = month(1L)
#' )
#' 
#' # Discrete vs continuous time
#' linear_time(Sys.time(), chronon = day(1L), discrete = TRUE)
#' linear_time(Sys.time(), chronon = day(1L), discrete = FALSE)
#' 
#' # ISO week calendar with week-day structure
#' linear_time(
#'   Sys.Date(),
#'   chronon = day(1L),
#'   calendar = cal_isoweek
#' )
#' 
#' @export
linear_time <- function(
  data, chronon = time_chronon(data), discrete = TRUE, 
  calendar = time_calendar(data)
) {
  # Evaluate chronon and granules with a calendar mask
  quo_chronon <- enquo(chronon)
  tryCatch({
    chronon <- eval_tidy(quo_chronon, data = calendar, env = emptyenv())
  }, error = function(e) {
    # Special hint for common error of 'week' unit not found in Gregorian calendar
    if (e$message == "could not find function \"week\"") {
      e$message <- c(
        e$message,
        "i" = "This error often occurs when trying to use a 'week' chronon without the ISO week calendar.\n",
        ">" = "Try specifying the calendar explicitly, e.g. `calendar = cal_isoweek`."
      )
    }
    cli::cli_abort(e$message, call = NULL)
  })

  mixtime(data, chronon = chronon, discrete = discrete)
}

#' Linear time helper functions
#' 
#' Convenience functions for creating common linear time representations. These
#' functions work with different calendar systems and adapt based on the input
#' data's calendar.
#' 
#' @param data A vector of time points (e.g. [base::Date], [base::POSIXt])
#' @param discrete If `TRUE`, the number of chronons since Unix epoch that
#' `data` falls into is returned as an integer. If `FALSE`, a fractional number
#'  of chronons is returned (analagous to time using a continuous time model).
#' @param calendar A calendar used to evaluate the time units. Defaults to the
#'   calendar of the input data. Common options include [cal_gregorian] and
#'   [cal_isoweek].
#' @param ... Additional arguments for [linear_time()], such as `tz` for timezones.
#' 
#' @details
#' These functions create linear time representations with different chronons
#' and granules:
#' 
#' - `year()`: Represents time in whole years. The chronon is one year.
#' - `yearquarter()`: Represents time in quarters, grouped by year. The chronon
#'   is one quarter, with years as the granule.
#' - `yearmonth()`: Represents time in months, grouped by year. The chronon is
#'   one month, with years as the granule.
#' - `yearweek()`: Represents time in weeks, grouped by year. The chronon is
#'   one week, with years as the granule. Defaults to ISO 8601 week calendar.
#' 
#' @section Calendar flexibility:
#' These functions adapt to the calendar system of the input data. For example:
#' 
#' - `year("2025-12-29")` returns a Gregorian year
#' - `year(yearweek("2025-12-29"))` returns an ISO week-based year
#' 
#' You can also explicitly specify a calendar using the `calendar` argument:
#' 
#' ```r
#' year(yearweek("2025-12-29"), calendar = cal_isoweek)
#' ```
#' 
#' @section Custom linear time representations:
#' For more complex time structures, use [linear_time()] or [new_linear_time_fn()]
#' to create custom representations with any combination of chronons and granules.
#' 
#' @return A `mixtime` time vector containing an `mt_linear` vector with chronons matching the function used.
#' 
#' @seealso 
#' - [linear_time()] for creating custom linear time representations
#' - [new_linear_time_fn()] for creating reusable linear time functions
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#' 
#' @examples
#' 
#' # Gregorian year
#' year(Sys.Date())
#' year(Sys.Date(), discrete = FALSE)
#' 
#' # ISO week-based year
#' year(yearweek(Sys.Date()))
#' 
#' # Year-quarter
#' yearquarter(Sys.Date())
#' yearquarter(Sys.Date(), discrete = FALSE)
#' 
#' # Year-month
#' yearmonth(Sys.Date())
#' yearmonth(Sys.Date(), discrete = FALSE)
#' 
#' # Year-week (ISO 8601)
#' yearweek(Sys.Date())
#' yearweek(0:52)
#' 
#' @name linear_time_helpers
#' @export
year <- new_linear_time_fn(
  chronon = year(1L)
)

#' @rdname linear_time_helpers
#' @export
yearquarter <- new_linear_time_fn(
  chronon = quarter(1L)
)

#' @rdname linear_time_helpers
#' @export
yearmonth <- new_linear_time_fn(
  chronon = month(1L)
)

#' @rdname linear_time_helpers
#' @export
yearweek <- new_linear_time_fn(
  chronon = week(1L),
  default_calendar = cal_isoweek
)

#' @rdname linear_time_helpers
#' @export
date <- new_linear_time_fn(
  chronon = day(1L)
)

#' @rdname linear_time_helpers
#' @export
datetime <- new_linear_time_fn(
  chronon = second(1L)
)

#' @importFrom vctrs vec_math
#' @method vec_math mt_time
#' @export
vec_math.mt_time <- function(.fn, .x, ...) {
  if (.fn == "mean") {
    res <- vctrs::vec_math_base(.fn, .x, ...)
    if (is.integer(.x)) {
      res <- as.integer(res)
    }
    return(vec_restore(res, .x))
  }
  if (.fn %in% c("is.nan", "is.finite", "is.infinite")) {
    return(vctrs::vec_math_base(.fn, .x, ...))
  }
  stop(sprintf("Math function '%s' not supported for continuous time", .fn), call. = FALSE)
}

#' @importFrom vctrs vec_arith
#' @method vec_arith mt_time
#' @export
vec_arith.mt_time <- function(op, x, y, ...) {
  UseMethod("vec_arith.mt_time", y)
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_time integer
#' @export
vec_arith.mt_time.integer <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only numeric addition and subtraction supported for continuous time", call. = FALSE)
  }
  res <- vec_arith_base(op, x, y, ...)
  # TODO: This should be vec_restore(), but it needs integer->double support
  attributes(res) <- attributes(x)
  res
}
#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_time double
#' @export
vec_arith.mt_time.double <- vec_arith.mt_time.integer

#' @method vec_arith.mt_time mt_duration
#' @export
vec_arith.mt_time.mt_duration <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only additing and subtracting durations are supported for continuous time", call. = FALSE)
  }
  cardinality <- chronon_cardinality(
    time_chronon(x), time_chronon(y),
    at = vec_data(x)
  )

  res <- vec_arith_base(op, vec_data(x), vec_data(y) * cardinality, ...)
  attributes(res) <- attributes(x)
  res
}
#' @method vec_arith.mt_duration mt_time
#' @export
vec_arith.mt_duration.mt_time <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only additing and subtracting durations are supported for continuous time", call. = FALSE)
  }
  cardinality <- chronon_cardinality(
    time_chronon(y), time_chronon(x),
    at = vec_data(y)
  )

  res <- vec_arith_base(op, vec_data(x) * cardinality, vec_data(y), ...)
  attributes(res) <- attributes(y)
  res
}

#' @method vec_arith.mt_time mt_time
#' @export
vec_arith.mt_time.mt_time <- function(op, x, y, ...) {
  if (op != "-") {
     stop("Only subtracting two time points is supported for continuous time", call. = FALSE)
  }
  tu <- chronon_common(time_chronon(x), time_chronon(y))
  res <- vec_arith_base(
    op,
    chronon_convert(x, tu, discrete = FALSE),
    chronon_convert(y, tu, discrete = FALSE),
    ...
  )
  new_duration(res, chronon = tu)
}

#' @method vec_cast.Date mt_linear
#' @export
vec_cast.Date.mt_linear <- function(x, to, ...) {
  vec_restore(
    chronon_convert(x, cal_gregorian$day(1L, tz = tz_name(time_chronon(x)))),
    to
  )
}

#' @method vec_cast.POSIXct mt_linear
#' @export
vec_cast.POSIXct.mt_linear <- function(x, to, ...) {
  vec_restore(
    chronon_convert(x, cal_gregorian$second(1L, tz = "UTC"), discrete = FALSE),
    to
  )
}
