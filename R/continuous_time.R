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
#' # A yearmonth time representation with months as the chronon
#' yw <- continuous_time(tu_week(1L), list(tu_year(1L)))
#' yw(Sys.Date())
#' 
#' yqm <- continuous_time(tu_month(1L), list(tu_year(1L), tu_quarter(1L)))
#' yqm(1:100)
#' 
#' yd <- continuous_time(tu_day(1L), list(tu_year(1L)))
#' yd(Sys.Date())
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
    .data <- time_cast(.data, chronon)

    if (!is.character(tz) || length(tz) != 1L) {
      cli::cli_abort("{.tz} must be a length 1 string describing the timezone. Mixed timezones currently need to be combined separately.")
    }

    vctrs::new_vctr(
      .data, 
      class = c("mt_continuous", "mt_time"),
      tz = tz, granules = granules, chronon = chronon
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
    ratio <- calendar_algebra(units[[i-1L]], units[[i]])
    parts[[i - 1L]] <- parts[[i]] %/% ratio
    parts[[i]] <- parts[[i]] %% ratio + 1L # (+1L to make 1-indexed for display)
  }
  # Add epoch offset to the largest granule
  parts[[1L]] <- parts[[1L]] + calendar_algebra(tu_year(1970L), units[[1L]])

  # Use cyclical labels for all but the largest granule
  for (i in seq(2L, n_units)) {
    parts[[i]] <- cyclical_labels(units[[i]], units[[i-1L]], parts[[i]])
  }

  inject(paste(!!!parts, sep = "-"))
  # The largest granule is displayed continuously, smaller units are displayed cyclically
  # For example, year-week-day would show 2023-W15-Wed for the 3rd day of the 15th week of 2023.
}