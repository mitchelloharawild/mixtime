# #' Base S7 class for time
# #'
# #' @export
# mt_time <- S7::new_class("mt_time", parent = S7::class_integer)

#' Continuous time representation
#' 
#' `continuous_time()` creates a continuous time representation using specified
#' granules and a chronon. Granules are larger time units that define the structure
#' of time (e.g., years, months), while the chronon is the smallest indivisible
#' time unit (e.g., days, hours).
#' 
#' @param granules A list of time unit objects representing the granules (e.g., `list(tu_year(1), tu_month(1))`)
#' @param chronon A time unit object representing the chronon (e.g., `tu_day(1)`)
#' 
#' @return An function used to create continuous time points.
#' 
#' @examples
#' 
#' # A year-month time representation with months as the chronon
#' ym <- continuous_time(tu_month(1L), list(tu_year(1L)))
#' ym(Sys.Date())
#' 
#' # A year-quarter-month time representation with months as the chronon
#' yqm <- continuous_time(tu_month(1L), list(tu_year(1L), tu_quarter(1L)))
#' yqm(1:100)
#' yqm(Sys.Date())
#' 
#' # A year-day time representation with days as the chronon
#' yd <- continuous_time(tu_day(1L), list(tu_year(1L)))
#' yd(Sys.Date())
#' 
#' ymd_h <- continuous_time(tu_hour(1L), list(tu_year(1L), tu_month(1L), tu_day(1L)))
#' ymd_h(Sys.time())
#' 
continuous_time <- function(chronon, granules = list()) {
  if (!all(vapply(granules, function(g) inherits(g, "mixtime::mt_unit"), logical(1L)))) {
    stop("All elements in granules must be time unit objects", call. = FALSE)
  }
  
  if (!inherits(chronon, "mixtime::mt_unit")) {
    stop("chronon must be a time unit object", call. = FALSE)
  }
  
  # TODO: Ensure c(granules, chronon) are in decreasing order of size

  # It is unclear to me how S7 can store granules and chronon in attributes
  # S7::new_class(
  #   "mt_continuous", parent = mt_time,
  #   properties = list(
  #     .data = S7::class_integer,
  #     tz = S7::class_character
  #   )
  # )

  function(.data, tz = "UTC") {
    # Cast from Date, POSIXct, etc.
    if (!is.numeric(.data) || !is.null(attributes(.data))) {
      # Drop the remainder, we only want the chronon here
      # TODO: Optionally preserve the remainder as fractional chronons
      .data <- chronon_cast(time_chronon(.data), chronon, vec_data(.data))$chronon
    }

    if (!is.character(tz) || length(tz) != 1L) {
      cli::cli_abort("{tz} must be a length 1 string describing the timezone. Mixed timezones currently need to be combined separately.")
    }

    mixtime(
      vctrs::new_vctr(
        .data, 
        class = c("mt_continuous", "mt_time"),
        tz = tz, granules = granules, chronon = chronon
      )
    )
  }
}

#' @importFrom rlang inject
#' @export
format.mt_continuous <- function(x, ...) {
  # Cascading formatting based on granules and chronon
  units <- c(
    attr(x, "granules"),
    list(attr(x, "chronon"))
  )
  parts <- rep(list(numeric(length(x))), n_units <- length(units))
  parts[[n_units]] <- vec_data(x)
  for (i in seq(n_units, by = -1L, length.out = n_units - 1L)) {
    mod <- chronon_cast(units[[i]], units[[i-1L]], parts[[i]])
    parts[[i - 1L]] <- mod$chronon
    parts[[i]] <- mod$remainder
  }
  # Add epoch offset to the largest granule
  # TODO: Replace calendar_algebra for suppor of irregular time units
  parts[[1L]] <- parts[[1L]] + calendar_algebra(tu_year(1970L), units[[1L]])

  # Use cyclical labels for all but the largest granule
  for (i in seq(2L, by = -1L, length.out = n_units - 1L)) {
    parts[[i]] <- cyclical_labels(units[[i]], units[[i-1L]], parts[[i]])
  }

  # The largest granule is displayed continuously, smaller units are displayed cyclically
  # For example, year-week-day would show 2023-W15-Wed for the 3rd day of the 15th week of 2023.
  inject(paste(!!!parts, sep = "-"))
}


#' @importFrom tsibble index_valid
#' @export
index_valid.mt_continuous <- function(x) TRUE

#' @importFrom tsibble interval_pull
#' @export
interval_pull.mt_continuous <- function(x) {
  chronon <- time_chronon(x)
  tsbl_unit <- vec_match(S7_class_id(chronon), tsbl_interval_units)

  interval <- list(vec_data(chronon))
  names(interval) <- names(tsbl_interval_units)[tsbl_unit]

  inject(tsibble::new_interval(!!!interval))
}

tsbl_interval_units <- c(
  "year" = "mixtime::tu_year",
  "quarter" = "mixtime::tu_quarter",
  "month" = "mixtime::tu_month",
  "week" = "mixtime::tu_week",
  "day" = "mixtime::tu_day",
  "hour" = "mixtime::tu_hour",
  "minute" = "mixtime::tu_minute",
  "second" = "mixtime::tu_second",
  "millisecond" = "mixtime::tu_millisecond"
)

#' @importFrom vctrs vec_arith
#' @method vec_arith mt_continuous
#' @export
vec_arith.mt_continuous <- function(op, x, y, ...) {
  UseMethod("vec_arith.mt_continuous", y)
}

#' @importFrom vctrs vec_arith_base
#' @method vec_arith.mt_continuous integer
#' @export
vec_arith.mt_continuous.integer <- function(op, x, y, ...) {
  if (!op %in% c("+", "-")) {
    stop("Only integer addition and subtraction supported for continuous time", call. = FALSE)
  }
  vec_restore(vec_arith_base(op, x, y, ...), x)
}