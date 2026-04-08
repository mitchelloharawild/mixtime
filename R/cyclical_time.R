#' Cyclical time function factory
#' 
#' `new_cyclical_time_fn()` creates a cyclical time function for a specified
#' chronon and cycle. The cycle is the larger time unit that defines the time 
#' period over which the chronon loops (e.g., a week). The chronon is the smaller 
#' time unit that iterates within each cycle (e.g., a day). Combined, these two
#' units form a cyclical time point (e.g., day of the week).
#' 
#' @param chronon A time unit object representing the chronon (e.g., `day(1L)`)
#' @param cycle A time unit object representing the cycle (e.g., `week(1L)`)
#' @param fallback_calendar A fallback calendar used to find the time units for 
#'   conversion if they don't exist in the calendar of the input data (e.g., `cal_isoweek`)
#' 
#' @return A function used to create cyclical time points with a specific chronon and cycle.
#' 
#' @examples
#' 
#' day_of_week <- new_cyclical_time_fn(day(1L), week(1L), fallback_calendar = cal_isoweek)
#' day_of_week(Sys.Date())
#' 
#' month_of_year <- new_cyclical_time_fn(month(1L), year(1L))
#' month_of_year(Sys.Date())
#' 
#' @export
new_cyclical_time_fn <- function(chronon, cycle, fallback_calendar = cal_gregorian) {
  # Capture chronon and cycle for later evaluation within
  # the user-specified calendar
  chronon <- enquo(chronon)
  cycle <- enquo(cycle)
  force(fallback_calendar)

  function(
    data, discrete = TRUE, calendar = time_calendar(data), ...
  ) {
    # Add tz / loc to chronon
    chronon <- quo_add_dots(chronon, ...)

    cyclical_time(
      data, chronon = !!chronon, cycle = !!cycle, discrete = discrete, 
      calendar = cal_fallback(calendar, fallback_calendar)
    )
  }
}

#' Cyclical time points
#' 
#' `cyclical_time()` creates a vector of cyclical time points representing
#' positions within repeating cycles. This function is useful for creating custom
#' cyclical time representations that aren't covered by the convenience functions
#' like [day_of_week()] or [month_of_year()].
#' 
#' @param data Input data to convert to cyclical time. Can be:
#'   - Numeric values (interpreted as chronons, 1-indexed)
#'   - Character strings (parsed as dates/times)
#'   - Date or POSIXct objects
#'   - Other time objects
#' @param chronon A time unit representing the chronon (smallest 
#'   indivisible time unit), evaluated in the context of `calendar`. Use 
#'   unquoted expressions like `day(1L)` or `month(1L)`. Chronons from a
#'   specific calendar can also be used (e.g. `cal_isoweek$day(1L)`).
#' @param cycle A time unit representing the cycle (larger time unit
#'   that defines the period), evaluated in the context of `calendar`. Use
#'   unquoted expressions like `week(1L)` or `year(1L)`. The
#'   time units should be ordered from coarsest (e.g. year) to finest (e.g second).
#' @param discrete Logical. If `TRUE` (default), returns integer positions within
#'   the cycle (discrete time model). If `FALSE`, returns fractional positions 
#'   allowing representation of partial time units (continuous time model).
#' @param calendar Calendar system used to evaluate `chronon` and `cycle`.
#'   Defaults to `time_calendar(data)` for existing time objects. Common options
#'   include [cal_gregorian] and [cal_isoweek].
#' 
#' @return A `mixtime` time vector containing an `mt_cyclical` vector.
#' 
#' @seealso 
#' - [new_cyclical_time_fn()] for creating reusable cyclical time functions
#' - [day_of_week()], [day_of_month()], [day_of_year()] for common cyclical representations
#' - [month_of_year()], [week_of_year()] for other cyclical time helpers
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#' 
#' @examples
#' # Day of week (1-7, Monday = 1)
#' cyclical_time(
#'   Sys.Date(),
#'   chronon = day(1L),
#'   cycle = week(1L),
#'   calendar = cal_isoweek
#' )
#' 
#' # Month of year (1-12)
#' cyclical_time(
#'   Sys.Date(),
#'   chronon = month(1L),
#'   cycle = year(1L)
#' )
#' 
#' # Discrete vs continuous time
#' # yearweek(x) is linear_time(x, chronon = day(1L), cycle = week(1L), calendar = cal_isoweek)
#' yearweek(Sys.time(), discrete = TRUE)
#' yearweek(Sys.time(), discrete = FALSE)
#' 
#' # Day of month with Gregorian calendar
#' cyclical_time(
#'   Sys.Date(),
#'   chronon = day(1L),
#'   cycle = month(1L),
#'   calendar = cal_gregorian
#' )
#' 
#' # Hours, minutes, and seconds
#' cyclical_time(
#'   Sys.time(),
#'   chronon = second(1L),
#'   cycle = day(1L)
#' )
#' 
#' @export
cyclical_time <- function(
  data, chronon = time_chronon(data), cycle,
  discrete = TRUE, calendar = time_calendar(data)
) {
  # Evaluate chronon and cycle with a calendar mask
  quo_chronon <- enquo(chronon)
  quo_cycle <- enquo(cycle)
  tryCatch({
    chronon <- eval_tidy(quo_chronon, data = calendar)
    stopifnot(S7::S7_inherits(chronon, mt_unit))
    cycle <- eval_tidy(quo_cycle, data = calendar)
    stopifnot(S7::S7_inherits(cycle, mt_unit))
  }, error = function(e) {
    if (inherits(calendar, "mt_calendar_fb")) {
      chronon <<- eval_tidy(quo_chronon, data = attr(calendar, "fallback"))
      cycle <<- eval_tidy(quo_cycle, data = attr(calendar, "fallback"))
    } else {
      cli::cli_abort(e$message, call = NULL)
    }
  })

  # Make numeric data input 1-indexed
  if (is.numeric(data)) data <- data - 1L
  # TODO - numeric data should default to origin-less cyclical time


  mixtime(data, chronon = chronon, cycle = cycle, discrete = discrete)

  # TODO - For origin-less cyclical time, reduce to cyclical time with divmod methods
  # data <- chronon_divmod(from = chronon, to = cycle, x = data)$mod
}

#' @export
format.mt_cyclical <- function(x, format = time_format_default(x), ...) {
  time_format_impl(x, format = format, ...)
}

#' @method vec_cast.character mt_cyclical
#' @export
vec_cast.character.mt_cyclical <- function(x, to, ...) {
  chronon <- time_chronon(x)
  cycle <- time_cycle(x)

  xf <- floor(x <- vec_data(x))
  out <- cyclical_labels(chronon, cycle[[1L]], xf)

  is_discrete <- is.integer(x)
  if(!is_discrete) {
    out <- paste0(out, sprintf("-%.1f%%", (x-xf)*100))
  }
  out
}

#' @method vec_cast.integer mt_cyclical
#' @export
vec_cast.integer.mt_cyclical <- function(x, to, ...) {
  vec_cast(vec_data(x), integer())
}

#' @method vec_cast.double mt_cyclical
#' @export
vec_cast.double.mt_cyclical <- function(x, to, ...) {
  vec_cast(vec_data(x), double())
}

# #' @importFrom vctrs vec_arith
# #' @method vec_arith mt_cyclical
# #' @export
# vec_arith.mt_cyclical <- function(op, x, y, ...) {
#   UseMethod("vec_arith.mt_cyclical", y)
# }

# #' @importFrom vctrs vec_arith_base
# #' @method vec_arith.mt_cyclical integer
# #' @export
# vec_arith.mt_cyclical.integer <- function(op, x, y, ...) {
#   if (!op %in% c("+", "-")) {
#     stop("Only integer addition and subtraction supported for cyclical time", call. = FALSE)
#   }
#   period <- chronon_cardinality(attr(x, "chronon"), attr(x, "cycle"))
#   vec_restore((vec_arith_base(op, vec_data(x), y, ...) - 1L) %% period + 1L, x)
# }
# #' @importFrom vctrs vec_arith_base
# #' @method vec_arith.mt_cyclical double
# #' @export
# vec_arith.mt_cyclical.double <- vec_arith.mt_cyclical.integer

#' Cyclical time helpers
#' 
#' Helper functions for creating cyclical time representations. These functions
#' create time objects that repeat within a larger time cycle, useful for identifying
#' seasonal patterns or positions within a calendar period.
#' 
#' @param data Another object to be coerced into the specified cyclical time.
#' @param discrete If `TRUE`, the position within the cycle that `data` 
#' falls into is returned as an integer. If `FALSE`, a fractional 
#' position is returned (analagous to time using a continuous time model).
#' @param calendar A calendar object specifying the calendar system to use.
#' @param ... Additional arguments for [cyclical_time()], such as `tz` for timezones.
#' 
#' @section Cyclical time representations:
#' - `day_of_week()`: Represents the day position within a week (1-7) using
#'   the ISO 8601 standard where weeks start on Monday.
#' - `day_of_month()`: Represents the day position within a month (1-28, 1-29,
#'   1-30, or 1-31 depending on the month). The chronon is one day, cycling
#'   within a month.
#' - `day_of_year()`: Represents the day position within a year (1-365 or 1-366
#'   for leap years). The chronon is one day, cycling within a year.
#' - `week_of_year()`: Represents the week position within a year (1-52 or 1-53)
#'   using the ISO 8601 week numbering system.
#' - `month_of_year()`: Represents the month position within a year (1-12).
#'   The chronon is one month, cycling within a year.
#' 
#' @section Custom cyclical time representations:
#' You can create custom cyclical time representations using [cyclical_time()]
#' with any of the supported time units (see [calendar_gregorian] and [calendar_isoweek]).
#' 
#' For example, to create a representation for day of the month:
#' ```r
#' day_of_month <- new_cyclical_time_fn(
#'   chronon = day(1L), cycle = month(1L),
#'   fallback_calendar = cal_gregorian
#' )
#' ```
#'  
#' @return A `mixtime` time vector containing an `mt_cyclical` vector with chronon and cycle matching the function used.
#' 
#' @seealso [cyclical_time()] for creating cyclical time vectors,
#'   [new_cyclical_time_fn()] for creating cyclical time helper functions
#' 
#' @examples
#' 
#' month_of_year(Sys.Date())
#' day_of_year(Sys.Date())
#' day_of_week(Sys.Date())
#' day_of_week(as.Date("2025-12-15") + 0:6)
#' 
#' @name cyclical_time_helpers
#' @export
month_of_year <- new_cyclical_time_fn(
  chronon = month(1L),
  cycle = year(1L)
)

#' @rdname cyclical_time_helpers
#' @export
day_of_year <- new_cyclical_time_fn(
  chronon = day(1L),
  cycle = year(1L)
)

#' @rdname cyclical_time_helpers
#' @export
day_of_month <- new_cyclical_time_fn(
  chronon = day(1L),
  cycle = month(1L)
)

#' @rdname cyclical_time_helpers
#' @export
time_of_day <- new_cyclical_time_fn(
  chronon = second(1L),
  cycle = day(1L),
  fallback_calendar = cal_time_civil_midnight
)

#' @rdname cyclical_time_helpers
#' @export
day_of_week <- new_cyclical_time_fn(
  chronon = day(1L),
  cycle = week(1L),
  fallback_calendar = cal_isoweek
)

#' @rdname cyclical_time_helpers
#' @export
week_of_year <- new_cyclical_time_fn(
  chronon = week(1L),
  cycle = year(1L),
  fallback_calendar = cal_isoweek
)