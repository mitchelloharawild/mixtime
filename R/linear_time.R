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
#' @param chronon A time unit object representing the chronon (e.g., `day(1)`)
#' @param granules A list of time unit objects representing the granules (e.g., `list(year(1), month(1))`)
#' @param calendar A calendar used to find the time units (e.g., `cal_isoweek`)
#' 
#' @return An function used to create continuous time points.
#' 
#' @examples
#' 
#' # A year-month time representation with months as the chronon
#' ym <- linear_time(month(1L), list(year(1L)))
#' ym(Sys.Date())
#' 
#' # A year-quarter-month time representation with months as the chronon
#' yqm <- linear_time(month(1L), list(year(1L), quarter(1L)))
#' yqm(1:100)
#' yqm(Sys.Date())
#' 
#' # A year-day time representation with days as the chronon
#' yd <- linear_time(day(1L), list(year(1L)))
#' yd(Sys.Date())
#' 
#' ymd_h <- linear_time(hour(1L), list(year(1L), month(1L), day(1L)))
#' ymd_h(Sys.time())
#' 
#' @export
linear_time <- function(chronon, granules = list(), calendar = cal_gregorian) {
  # Add calendar data mask for evaluating chronon and cycle
  chronon <- eval_tidy(enquo(chronon), data = calendar)
  granules <- eval_tidy(enquo(granules), data = calendar)

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

    # Parse text data
    if (is.character(.data)) {
      .data <- anytime::utctime(.data)
    }

    # Cast from Date, POSIXct, etc.
    if (!is.numeric(.data) || !is.null(attributes(.data))) {
      .data <- chronon_convert(
        .data + tz_offset(.data, tz), 
        chronon,
        discrete = discrete
      )
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


#' Gregorian continuous time representations
#' 
#' Linear time representations for the Gregorian calendar system. These functions
#' create time objects measured in years, year-quarters, or year-months since the
#' Unix epoch (1970-01-01).
#' 
#' @param .data Another object to be coerced into the specified time.
#' @param tz Timezone, defaults to "UTC".
#' @param discrete If `TRUE`, the number of chronons since Unix epoch that
#' `.data` falls into is returned as an integer. If `FALSE`, a fractional number
#'  of chronons is returned (analagous to time using a continuous time model).
#' 
#' @details
#' - `year()`: Represents time in whole years since 1970. The chronon is one year.
#' - `yearquarter()`: Represents time in quarters, grouped by year. The chronon
#'   is one quarter, with years as the granule for display and grouping.
#' - `yearmonth()`: Represents time in months, grouped by year. The chronon is
#'   one month, with years as the granule for display and grouping.
#' 
#' @section Custom Gregorian time representations:
#' You can create custom time representations using [linear_time()] with any of
#' the supported Gregorian time units (see [calendar_gregorian]).
#' 
#' For example, to create a time representation in hours since epoch with day granules:
#' ```r
#' dayhour <- linear_time(
#'   granules = list(cal_gregorian$day(1L)),
#'   chronon = cal_gregorian$hour(1L)
#' )
#' ```
#' 
#' @examples
#' 
#' year(Sys.Date())
#' year(Sys.Date(), discrete = FALSE)
#' 
#' @name linear_gregorian
#' @export
year <- linear_time(
  chronon = cal_gregorian$year(1L)
)

#' @examples
#' 
#' yearquarter(Sys.Date())
#' yearquarter(Sys.Date(), discrete = FALSE)
#' 
#' @rdname linear_gregorian
#' @export
yearquarter <- linear_time(
  chronon = cal_gregorian$quarter(1L),
  granules = list(cal_gregorian$year(1L))
)

#' @examples
#' 
#' yearmonth(Sys.Date())
#' yearmonth(Sys.Date(), discrete = FALSE)
#' 
#' @rdname linear_gregorian
#' @export
yearmonth <- linear_time(
  chronon = cal_gregorian$month(1L),
  granules = list(cal_gregorian$year(1L))
)

#' ISO 8601 year-week time representation
#'
#' Create or coerce using `yearweek()`.
#'
#' @inheritParams yearmonth
#' @param .data Another object to be coerced into ISO 8601 year-weeks.
#'
#' @examples
#' 
#' yearweek(Sys.Date())
#' yearweek(0:52)
#' 
#' @export
#' @name linear_iso8601
yearweek <- linear_time(granules = list(cal_isoweek$year(1L)), chronon = cal_isoweek$week(1L))

#' @examples
#' 
#' yearmonthday(Sys.Date())
#' yearmonthday(Sys.Date(), discrete = FALSE)
#' 
#' @rdname linear_gregorian
#' @export
yearmonthday <- linear_time(
  chronon = cal_gregorian$day(1L),
  granules = list(cal_gregorian$year(1L), cal_gregorian$month(1L))
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
