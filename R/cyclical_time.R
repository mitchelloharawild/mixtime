#' Cyclical time representation
#' 
#' `cyclical_time()` creates a cyclical time representation using specified
#' cycle and chronon (and optionally display granules). The cycles is the 
#' larger time unit that defines the time period over which the chronon loops
#' over (e.g. a week, `week(1L)`). The chronon is the smaller time unit that
#' iterates within each cycle (e.g. a day, `day(1L)`). Combined, these two
#' granules form a cyclical time point (e.g. day of the week).
#' 
#' @param chronon A time unit object representing the chronon (e.g., `month(1L)`)
#' @param cycle A time unit object representing the cycle (e.g., `year(1L)`)
#' @param calendar A calendar used to find the time units (e.g., `cal_isoweek`)
#' 
#' @return An function used to create cyclical time points.
#' 
#' @examples
#' 
#' day_of_week <- cyclical_time(day(1L), week(1L), calendar = cal_isoweek)
#' day_of_week(Sys.Date())
#' 
#' month_of_year <- cyclical_time(month(1L), year(1L))
#' month_of_year(Sys.Date())
#' 
#' @export
#' @importFrom rlang enquo eval_tidy
cyclical_time <- function(chronon, cycle, calendar = cal_gregorian) {
  # Add calendar data mask for evaluating chronon and cycle
  chronon <- eval_tidy(enquo(chronon), data = calendar)
  cycle <- eval_tidy(enquo(cycle), data = calendar)

  if (!inherits(chronon, "mixtime::mt_unit")) {
    stop("`chronon` must be a time unit object", call. = FALSE)
  }
  if (!inherits(cycle, "mixtime::mt_unit")) {
    stop("`cycle` must be a time unit object", call. = FALSE)
  }
  
  # TODO: Ensure c(granules, chronon) are in decreasing order of size
  
  function(.data, tz = NULL, discrete = TRUE) {
    # Attach timezone to chronon and granules
    if (!is.null(tz)) {
      chronon@tz <- tz
      granules <- lapply(granules, function(g) {g@tz <- tz; g})
    }

    # Make numeric .data input 1-indexed
    if (is.numeric(.data)) .data <- .data - 1L
    
    # Cast to continuous time from Date, POSIXct, etc.
    if (!is.numeric(.data) || !is.null(attributes(.data))) {
      .data <- chronon_convert(.data, chronon, discrete = discrete)
    }

    # Reduce to cyclical time with divmod methods
    .data <- chronon_divmod(from = chronon, to = cycle, x = .data)$remainder
    
    # Ensure .data is integer for discrete data
    # (since chronon_divmod()$remainder may not respect discrete)
    if (discrete) {
      .data <- as.integer(.data)
    }

    # if (!is.character(tz) || length(tz) != 1L) {
    #   cli::cli_abort("{tz} must be a length 1 string describing the timezone. Mixed timezones currently need to be combined separately.")
    # }

    mixtime(
      vctrs::new_vctr(
        .data, 
        class = c("mt_cyclical", "mt_time"),
        tz = tz, chronon = chronon, cycle = cycle
      )
    )
  }
}

#' @export
mixtime_valid.mt_cyclical <- function(x) TRUE

#' @export
format.mt_cyclical <- function(x, ...) {
    as.character(x)
}

#' @method vec_cast.character mt_cyclical
#' @export
vec_cast.character.mt_cyclical <- function(x, to, ...) {
  chronon <- attr(x, "chronon")
  cycle <- attr(x, "cycle")
  tz <- attr(x, "tz")

  xf <- floor(x <- vec_data(x))
  out <- cyclical_labels(chronon, cycle, xf)

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

#' @importFrom vctrs vec_arith
#' @method vec_arith mt_cyclical
#' @export
vec_arith.mt_cyclical <- function(op, x, y, ...) {
  UseMethod("vec_arith.mt_cyclical", y)
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_cyclical integer
#' @export
vec_arith.mt_cyclical.integer <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only integer addition and subtraction supported for cyclical time", call. = FALSE)
  }
  period <- chronon_cardinality(attr(x, "cycle"), attr(x, "chronon"))
  vec_restore((vec_arith_base(op, vec_data(x), y, ...) - 1L) %% period + 1L, x)
}
#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_cyclical double
#' @export
vec_arith.mt_cyclical.double <- vec_arith.mt_cyclical.integer

#' Gregorian cyclical time representations
#' 
#' Cyclical time representations for the Gregorian calendar system. These functions
#' create time objects that repeat within a larger time cycle, useful for identifying
#' seasonal patterns or positions within a calendar period.
#' 
#' @param .data Another object to be coerced into the specified cyclical time.
#' @param discrete If `TRUE`, the position within the cycle that `.data` 
#' falls into is returned as an integer. If `FALSE`, a fractional 
#' position is returned (analagous to time using a continuous time model).
#' @inheritParams linear_gregorian
#' 
#' @details
#' - `month_of_year()`: Represents the month position within a year (1-12).
#'   The chronon is one month, cycling within a year.
#' - `day_of_year()`: Represents the day position within a year (1-365 or 1-366
#'   for leap years). The chronon is one day, cycling within a year.
#' - `day_of_month()`: Represents the day position within a month (1-28, 1-29,
#'   1-30, or 1-31 depending on the month). The chronon is one day, cycling
#'   within a month.
#' 
#' These cyclical representations are useful for analyzing seasonal patterns or
#' comparing time points at similar positions across different years.
#' 
#' @section Custom Gregorian cyclical time representations:
#' You can create custom cyclical time representations using [cyclical_time()]
#' with any of the supported Gregorian time units (see [calendar_gregorian]).
#' 
#' For example, to create a representation for day of the month:
#' ```r
#' day_of_month <- cyclical_time(
#'   chronon = cal_gregorian$day(1L),
#'   cycle = cal_gregorian$month(1L)
#' )
#' ```
#' 
#' @seealso [linear_gregorian] for linear Gregorian time representations,
#'   [cyclical_time()] for creating custom cyclical time representations
#' 
#' @examples
#' 
#' month_of_year(Sys.Date())
#' 
#' @name cyclical_gregorian
#' @export
month_of_year <- cyclical_time(
  chronon = cal_gregorian$month(1L),
  cycle = cal_gregorian$year(1L)
)

#' @examples
#' 
#' day_of_year(Sys.Date())
#' 
#' @rdname cyclical_gregorian
#' @export
day_of_year <- cyclical_time(
  chronon = cal_gregorian$day(1L),
  cycle = cal_gregorian$year(1L)
)

#' @rdname cyclical_gregorian
#' @export
day_of_month <- cyclical_time(
  chronon = day(1L),
  cycle = month(1L)
)

#' ISO 8601 day of week
#'
#' A cyclical time representation for days within a week using the ISO 8601 
#' standard where weeks start on Monday.
#'
#' @param .data A vector to be coerced into day of week. This can be a date, 
#'   date-time, or numeric vector.
#' @inheritParams linear_iso8601
#' @inheritParams cyclical_gregorian
#'
#' @return A cyclical time object representing the day of the week.
#'
#' @seealso [yearweek()] for ISO 8601 year-week representation,
#'   [cyclical_time()] for creating custom cyclical time representations
#'
#' @examples
#' day_of_week(Sys.Date())
#' day_of_week(as.Date("2025-12-15") + 0:6)
#'
#' @name cyclical_iso8601
#' @export
day_of_week <- cyclical_time(
  chronon = cal_isoweek$day(1L),
  cycle = cal_isoweek$week(1L)
)

#' @export
#' @rdname cyclical_iso8601
week_of_year <- cyclical_time(
  chronon = cal_isoweek$week(1L),
  cycle = cal_isoweek$year(1L)
)

# TODO, attach calendar argument (which is a list of time units) to user-facing linear_time() and cyclical_time() non-factory variants.
# Factory outputs should now also have a calendar argument (this is likely harder)