# #' Base S7 class for time
# #'
# #' @export
# mt_time <- S7::new_class("mt_time", parent = S7::class_integer)

#' Linear time representation
#' 
#' `linear_time()` creates a linear time representation using specified
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
#' ym <- linear_time(tu_month(1L), list(tu_year(1L)))
#' ym(Sys.Date())
#' 
#' # A year-quarter-month time representation with months as the chronon
#' yqm <- linear_time(tu_month(1L), list(tu_year(1L), tu_quarter(1L)))
#' yqm(1:100)
#' yqm(Sys.Date())
#' 
#' # A year-day time representation with days as the chronon
#' yd <- linear_time(tu_day(1L), list(tu_year(1L)))
#' yd(Sys.Date())
#' 
#' ymd_h <- linear_time(tu_hour(1L), list(tu_year(1L), tu_month(1L), tu_day(1L)))
#' ymd_h(Sys.time())
#' 
#' @export
linear_time <- function(chronon, granules = list()) {
  if (!all(vapply(granules, function(g) inherits(g, "mixtime::mt_unit"), logical(1L)))) {
    stop("All elements in granules must be time unit objects", call. = FALSE)
  }
  
  if (!inherits(chronon, "mixtime::mt_unit")) {
    stop("chronon must be a time unit object", call. = FALSE)
  }
  
  # TODO: Ensure c(granules, chronon) are in decreasing order of size

  function(.data, tz = tz_name(.data), discrete = TRUE) {
    # Attach timezone to chronon and granules
    if (!is.null(tz)) {
      chronon@tz <- tz
      granules <- lapply(granules, function(g) {g@tz <- tz; g})
    }

    # Cast from Date, POSIXct, etc.
    if (!is.numeric(.data) || !is.null(attributes(.data))) {
      .data <- chronon_convert(.data, chronon, discrete = discrete)
    }

    # if (!is.character(tz) || length(tz) != 1L) {
    #   cli::cli_abort("{tz} must be a length 1 string describing the timezone. Mixed timezones currently need to be combined separately.")
    # }

    mixtime(
      vctrs::new_vctr(
        .data, 
        class = c("mt_linear", "mt_time"),
        tz = tz, granules = granules, chronon = chronon
      )
    )
  }
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
  
  if (is_zoned <- tz_name(time_chronon(x)) != "UTC") {
    tz_ext <- tz_abbreviation(x)
  }
  is_discrete <- is.integer(x)
  
  # Apply timezone offset to produce local time
  x <- vec_data(x) + tz_offset(x)
  
  parts <- rep(list(numeric(length(x))), n_units <- length(units))
  parts[[n_units]] <- floor(x)
  
  # Compute fractional component of chronon
  if(is_discrete) {
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


#' @importFrom tsibble index_valid
#' @export
index_valid.mt_linear <- function(x) TRUE

#' @importFrom tsibble interval_pull
#' @export
interval_pull.mt_linear <- function(x) {
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
  as.Date(chronon_divmod(chronon, tu_day(1L), vec_data(x))$chronon)
}

#' @method vec_cast.POSIXct mt_linear
#' @export
vec_cast.POSIXct.mt_linear <- function(x, ...) {
  chronon <- time_chronon(x)
  .POSIXct(
    chronon_convert(x, tu_second(1L), discrete = FALSE)
  )
  # as.POSIXct(
  #   chronon_divmod(chronon, tu_second(1L), vec_data(x))$chronon,
  #   origin = "1970-01-01", tz = attr(x, "tz")
  # )
}
