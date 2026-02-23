#' Linear time function factory
#' 
#' `new_linear_time_fn()` creates a linear time function for a specified
#' chronon and granules. Granules are larger time units that define the structure
#' of time (e.g., years, months), while the chronon is the smallest indivisible
#' time unit (e.g., days, hours).
#' 
#' @param chronon A time unit object representing the chronon (e.g., `day(1)`)
#' @param granules A list of time unit objects representing the granules 
#'   (e.g., `list(year(1), month(1))`)
#' @param fallback_calendar A fallback calendar used to find the time units for 
#'   conversion if they don't exist in the calendar of the input data (e.g., `cal_isoweek`)
#' 
#' @return A function used to create linear time points.
#' 
#' @examples
#' 
#' # A year-month time representation with months as the chronon
#' ym <- new_linear_time_fn(month(1L), list(year(1L)))
#' ym(Sys.Date())
#' 
#' # A year-quarter-month time representation with months as the chronon
#' yqm <- new_linear_time_fn(month(1L), list(year(1L), quarter(1L)))
#' yqm(1:100)
#' yqm(Sys.Date())
#' 
#' # A year-day time representation with days as the chronon
#' yd <- new_linear_time_fn(day(1L), list(year(1L)))
#' yd(Sys.Date())
#' 
#' # Gregorian date time with hourly precision
#' ymd_h <- new_linear_time_fn(hour(1L), list(year(1L), month(1L), day(1L)))
#' ymd_h(Sys.time())
#' 
#' # ISO-week-date calendar
#' ywd <- new_linear_time_fn(day(1L), list(year(1L), week(1L)), fallback_calendar = cal_isoweek)
#' ywd(Sys.Date())
#' 
#' @export
new_linear_time_fn <- function(chronon, granules = list(), fallback_calendar = cal_gregorian) {
  # Capture chronon and granularity for later evaluation within
  # the user-specified calendar
  chronon <- enquo(chronon)
  granules <- enquo(granules)
  force(fallback_calendar)

  function(
    data, tz = tz_name(data), discrete = TRUE, 
    calendar = time_calendar(data)
  ) {
    linear_time(
      data, chronon = !!chronon, granules = !!granules, 
      tz = tz, discrete = discrete, 
      calendar = cal_fallback(calendar, fallback_calendar)
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
#' @param tz Time zone for the time representation. Defaults to the time zone
#'   of the input `data` (`tz_name(data)`). Time zones need to be valid 
#'   identifiers for the IANA time zone database ([`tzdb::tzdb_names()`])
#' @param discrete Logical. If `TRUE` (default), returns integer chronons since
#'   Unix epoch (discrete time model). If `FALSE`, returns fractional chronons 
#'   allowing representation of partial time units (continuous time model).
#' @param calendar Calendar system used to evaluate `chronon` and `granules`.
#'   Defaults to `time_calendar(data)` for existing time objects. Common options
#'   include [cal_gregorian] and [cal_isoweek].
#' @param granules A list of time unit expressions representing structural units
#'   larger than the chronon (e.g., years, quarters, months). These define how
#'   time is displayed and grouped. Use unquoted expressions like 
#'   `list(year(1L), quarter(1L))`. Defaults to an empty list.
#' 
#' @return A `mt_linear` time vector, which is a subclass of `mt_time`.
#' 
#' @seealso 
#' - [new_linear_time_fn()] for creating reusable linear time functions
#' - [yearmonth()], [yearquarter()], [year()] for Gregorian time representations
#' - [yearweek()] for ISO 8601 week-based time
#' - [cal_gregorian], [cal_isoweek] for calendar systems
#' 
#' @examples
#' # Hourly time with year-month-day granules
#' linear_time(
#'   Sys.time(),
#'   chronon = hour(1L),
#'   granules = list(year(1L), month(1L), day(1L))
#' )
#' 
#' # Monthly chronons with year-quarter granules
#' linear_time(
#'   Sys.Date(),
#'   chronon = month(1L),
#'   granules = list(year(1L), quarter(1L))
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
#'   granules = list(year(1L), week(1L)),
#'   calendar = cal_isoweek
#' )
#' 
#' @export
linear_time <- function(
  data, chronon = time_chronon(data), tz = tz_name(data),
  discrete = TRUE, calendar = time_calendar(data), granules = list()) {
  # Parse text data
  if (is.character(data)) {
    data <- as.POSIXct(data, tz = tz)
  }

  # Evaluate chronon and granules with a calendar mask
  quo_chronon <- enquo(chronon)
  quo_granules <- enquo(granules)
  tryCatch({
    chronon <- eval_tidy(quo_chronon, data = calendar)
    granules <- eval_tidy(quo_granules, data = calendar)
  }, error = function(e) {
    if (inherits(calendar, "mt_calendar_fb")) {
      chronon <<- eval_tidy(quo_chronon, data = attr(calendar, "fallback"))
      granules <<- eval_tidy(quo_granules, data = attr(calendar, "fallback"))
    } else {
      cli::cli_abort(e$message, call = NULL)
    }
  })
  
  if (!all(vapply(granules, function(g) inherits(g, "mixtime::mt_unit"), logical(1L)))) {
    stop("All elements in granules must be time unit objects", call. = FALSE)
  }
  
  if (!inherits(chronon, "mixtime::mt_unit")) {
    stop("chronon must be a time unit object", call. = FALSE)
  }
  
  # Attach timezone to chronon and granules
  if (!is.null(tz)) {
    chronon@tz <- tz
    granules <- lapply(granules, function(g) {g@tz <- tz; g})
  }

  # Cast from Date, POSIXct, etc.
  if (!is.numeric(data) || !is.null(attributes(data))) {
    data <- chronon_convert(
      data + tz_offset(data, tz_name(data)), 
      chronon,
      discrete = discrete
    )
  }

  mixtime(
    vctrs::new_vctr(
      data, 
      class = c("mt_linear", "mt_time"),
      tz = tz, granules = granules, chronon = chronon
    )
  )
}

#' @export
mixtime_valid.mt_linear <- function(x) TRUE

#' @importFrom rlang inject
#' @export
format.mt_linear <- function(x, ...) {
  as.character(x)
}

#' @method vec_cast.character mt_linear
#' @export
vec_cast.character.mt_linear <- function(x, to, ...) {
  # Cascading formatting based on granules and chronon
  units <- c(
    attr(x, "granules"),
    list(attr(x, "chronon"))
  )
  
  is_discrete <- is.integer(x)
  if (is_zoned <- tz_name(time_chronon(x)) != "UTC") {
    # Apply timezone offset to produce local time
    tz_ext <- tz_abbreviation(x)
    x <- vec_data(x) + floor(tz_offset(x))
  } else {
    x <- vec_data(x)
  }
  
  parts <- rep(list(numeric(length(x))), n_units <- length(units))
  parts[[n_units]] <- floor(x)
  
  # Compute fractional component of chronon
  if(!is_discrete) {
    frac <- x - parts[[n_units]]
  }

  for (i in seq(n_units, by = -1L, length.out = n_units - 1L)) {
    mod <- chronon_divmod(units[[i]], units[[i-1L]], parts[[i]])
    parts[[i - 1L]] <- mod$chronon
    parts[[i]] <- mod$remainder
  }

  # Add epoch offset to the largest granule
  # TODO: Use calendar specific epochs
  parts[[1L]] <- parts[[1L]] - chronon_convert(year(-1970L), units[[1L]])

  # Use cyclical labels for all but the largest granule
  for (i in seq(length(units), by = -1L, length.out = n_units - 1L)) {
    parts[[i]] <- cyclical_labels(units[[i]], units[[i-1L]], parts[[i]])
  }

  if(!is_discrete) {
    parts[[n_units + 1L]] <- sprintf("%.1f%%", frac*100)
  }

  if (is_zoned) {
    parts[[length(parts) + 1L]] <- tz_ext
  }

  # The largest granule is displayed continuously, smaller units are displayed cyclically
  # For example, year-week-day would show 2023-W15-Wed for the 3rd day of the 15th week of 2023.
  inject(paste(!!!parts, sep = "-"))
}

#' @method vec_cast.integer mt_linear
#' @export
vec_cast.integer.mt_linear <- function(x, to, ...) {
  vec_cast(vec_data(x), integer())
}

#' @method vec_cast.double mt_linear
#' @export
vec_cast.double.mt_linear <- function(x, to, ...) {
  vec_cast(vec_data(x), double())
}


#' Linear time helper functions
#' 
#' Convenience functions for creating common linear time representations. These
#' functions work with different calendar systems and adapt based on the input
#' data's calendar.
#' 
#' @param data A vector of time points (e.g. [base::Date], [base::POSIXt])
#' @param tz Timezone, defaults to "UTC".
#' @param discrete If `TRUE`, the number of chronons since Unix epoch that
#' `.data` falls into is returned as an integer. If `FALSE`, a fractional number
#'  of chronons is returned (analagous to time using a continuous time model).
#' @param calendar A calendar used to evaluate the time units. Defaults to the
#'   calendar of the input data. Common options include [cal_gregorian] and
#'   [cal_isoweek].
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
#' @section Custom time representations:
#' For more complex time structures, use [linear_time()] or [new_linear_time_fn()]
#' to create custom representations with any combination of chronons and granules.
#' 
#' @return A `mt_linear` time vector.
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
  chronon = quarter(1L),
  granules = list(year(1L))
)

#' @rdname linear_time_helpers
#' @export
yearmonth <- new_linear_time_fn(
  chronon = month(1L),
  granules = list(year(1L))
)

#' @rdname linear_time_helpers
#' @export
yearmonthday <- new_linear_time_fn(
  chronon = day(1L),
  granules = list(year(1L), month(1L))
)

#' @rdname linear_time_helpers
#' @export
yearweek <- new_linear_time_fn(
  granules = list(year(1L)), chronon = week(1L),
  fallback_calendar = cal_isoweek
)

#' @importFrom vctrs vec_math
#' @method vec_math mt_linear
#' @export
vec_math.mt_linear <- function(.fn, .x, ...) {
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
#' @method vec_arith mt_linear
#' @export
vec_arith.mt_linear <- function(op, x, y, ...) {
  UseMethod("vec_arith.mt_linear", y)
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_linear integer
#' @export
vec_arith.mt_linear.integer <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only numeric addition and subtraction supported for continuous time", call. = FALSE)
  }
  res <- vec_arith_base(op, x, y, ...)
  # TODO: This should be vec_restore(), but it needs integer->double support
  attributes(res) <- attributes(x)
  res
}
#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_linear double
#' @export
vec_arith.mt_linear.double <- vec_arith.mt_linear.integer


#' @method vec_arith.mt_linear mt_linear
#' @export
vec_arith.mt_linear.mt_linear <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only numeric addition and subtraction supported for continuous time", call. = FALSE)
  }
  vec_arith_base(op, x, y, ...)
}

#' @method vec_cast.Date mt_linear
#' @export
vec_cast.Date.mt_linear <- function(x, ...) {
  chronon <- time_chronon(x)
  as.Date(chronon_divmod(chronon, cal_gregorian$day(1L), vec_data(x))$chronon)
}

#' @method vec_cast.POSIXct mt_linear
#' @export
vec_cast.POSIXct.mt_linear <- function(x, ...) {
  chronon <- time_chronon(x)
  .POSIXct(
    chronon_convert(x, cal_gregorian$second(1L), discrete = FALSE)
  )
  # as.POSIXct(
  #   chronon_divmod(chronon, second(1L), vec_data(x))$chronon,
  #   origin = "1970-01-01", tz = attr(x, "tz")
  # )
}
