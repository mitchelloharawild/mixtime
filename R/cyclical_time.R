#' Cyclical time function factory
#' 
#' `new_cyclical_time_fn()` creates a cyclical time function for a specified
#' chronon and cycle. The cycle is the larger time granule that defines the time 
#' period over which the chronon loops (e.g., a week). The chronon is the smaller 
#' time granule that iterates within each cycle (e.g., a day). Combined, these two
#' granules form a cyclical time relationship (e.g., day of the week).
#' 
#' @param chronon A time granule object representing the chronon (e.g., `day(1L)`)
#' @param cycle A time granule object representing the cycle (e.g., `week(1L)`)
#' @param default_calendar A default calendar used to find the time units for 
#'   conversion if they don't exist in the calendar of the input data (e.g., `cal_isoweek`)
#' 
#' @return A function used to create cyclical time points with a specific chronon and cycle.
#' 
#' @examples
#' 
#' day_of_week <- new_cyclical_time_fn(day(1L), week(1L), default_calendar = cal_isoweek)
#' day_of_week(Sys.Date())
#' 
#' month_of_year <- new_cyclical_time_fn(month(1L), year(1L))
#' month_of_year(Sys.Date())
#' 
#' @export
new_cyclical_time_fn <- function(chronon, cycle, default_calendar = cal_gregorian) {
  chronon <- rlang::new_quosure(
    enexpr(chronon), 
    env = rlang::as_data_mask(default_calendar)
  )
  cycle <- rlang::new_quosure(
    enexpr(cycle), 
    env = rlang::as_data_mask(default_calendar)
  )

  function(
    data, discrete = TRUE, calendar = time_calendar(data), ...
  ) {
    # Add tz / loc to chronon
    chronon <- quo_add_dots(chronon, ...)

    cyclical_time(
      data, chronon = !!chronon, cycle = !!cycle, 
      discrete = discrete, calendar = calendar
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
#'   - Numeric values: positions within the cycle with no known cycle
#'     instance (an `mt_cyclical_free` vector), counted the way the chronon is
#'     displayed (e.g. `3` is March for months of the year, `0` is midnight
#'     for seconds of the day). Positions beyond the longest possible cycle
#'     (e.g. day 32 of a month) are `NA`.
#'   - Character strings (parsed as dates/times)
#'   - Date or POSIXct objects
#'   - Other time objects
#' @param chronon A time granule representing the chronon (finest 
#'   indivisible time granule), evaluated in the context of `calendar`. Use 
#'   unquoted expressions like `day(1L)` or `month(1L)`. Chronons from a
#'   specific calendar can also be used (e.g. `cal_isoweek$day(1L)`).
#' @param cycle A time granule representing the cycle (coarser time granule
#'   that defines the period), evaluated in the context of `calendar`. Use
#'   unquoted expressions like `week(1L)` or `year(1L)`.
#' @param discrete Logical. If `TRUE` (default), returns integer positions within
#'   the cycle (discrete time model). If `FALSE`, returns fractional positions 
#'   allowing representation of fractional time chronons (continuous time model).
#' @param calendar Calendar system used to evaluate `chronon` and `cycle`.
#'   Defaults to `time_calendar(data)` for existing time objects. Common options
#'   include [cal_gregorian] and [cal_isoweek].
#' 
#' @return A `mixtime` time vector containing an `mt_cyclical` vector, or an
#'   `mt_cyclical_free` vector for numeric `data`.
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
#' # Continuous time (discrete = FALSE) for fractional month of year
#' cyclical_time(
#'   Sys.Date(),
#'   chronon = month(1L),
#'   cycle = year(1L),
#'   discrete = FALSE
#' )
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
  data, chronon = time_chronon(data), cycle = time_cycle(data),
  discrete = TRUE, calendar = time_calendar(data)
) {
  # Evaluate chronon and cycle with a calendar mask
  quo_chronon <- enquo(chronon)
  quo_cycle <- enquo(cycle)
  tryCatch({
    chronon <- eval_tidy(quo_chronon, data = calendar, env = emptyenv())
    cycle <- eval_tidy(quo_cycle, data = time_calendar(chronon), env = emptyenv())
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

  # A bare number has no anchor, so it builds mt_cyclical_free, not mt_cyclical.
  # An all-NA logical (e.g. `month_of_year(NA)`) is a missing bare number.
  if (is.logical(data) && all(is.na(data)) && is.null(attributes(data))) {
    data <- rep(NA_integer_, length(data))
  }
  if (!is_mixtime(data) && !S7_inherits(data, mt_time) && is.numeric(data)) {
    if (discrete) data <- as.integer(round(data)) else data <- as.double(data)
    ptype <- mt_cyclical_free(data[0L], chronon = chronon, cycle = cycle)
    return(new_mixtime(free_from_position(data, ptype)))
  }

  mixtime(data, chronon = chronon, cycle = cycle, discrete = discrete)
}

# Read 1-indexed bare positions (e.g. 3 is March) into a free value like
# `ptype`, as `cyclical_time()` does. Positions follow the chronon's own label
# scheme (e.g. day_of_year(60) is the 60th day, time_of_day(0) is midnight),
# and those outside the cycle become NA.
free_from_position <- function(x, ptype) {
  chronon <- attr(ptype, "chronon")
  cycle <- attr(ptype, "cycle")
  x <- x - cyclical_labels(chronon, cycle)$start
  card <- cyclical_cardinality_max(chronon, cycle)
  if (!is.null(card)) {
    x[!is.na(x) & (x < 0 | x >= card)] <- NA
  }
  attributes(x) <- attributes(ptype)
  x
}

# The most `chronon`s a `cycle` can hold: its fixed cardinality, or for a
# variable-length cycle the largest over many cycle instances (e.g. 31 days in
# a month, 366 days in a year, 53 ISO weeks in a year). NULL if unknown.
cyclical_cardinality_max <- function(chronon, cycle) {
  if (chronon_cardinality_is_fixed(chronon, cycle)) {
    return(tryCatch(chronon_cardinality(chronon, cycle), error = function(e) NULL))
  }
  # 4800 consecutive cycles span every Gregorian leap pattern (400 years) even
  # for monthly cycles.
  tryCatch(
    max(chronon_cardinality(chronon, cycle, at = seq.int(-2400L, 2399L))),
    error = function(e) NULL
  )
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
#' 
#' \preformatted{
#' day_of_month <- new_cyclical_time_fn(
#'   chronon = day(1L), cycle = month(1L),
#'   default_calendar = cal_gregorian
#' )
#' }
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
  default_calendar = cal_time_civil
)

#' @rdname cyclical_time_helpers
#' @export
day_of_week <- new_cyclical_time_fn(
  chronon = day(1L),
  cycle = week(1L),
  default_calendar = cal_isoweek
)

#' @rdname cyclical_time_helpers
#' @export
week_of_year <- new_cyclical_time_fn(
  chronon = week(1L),
  cycle = year(1L),
  default_calendar = cal_isoweek
)