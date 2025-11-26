#' Cyclical time representation
#' 
#' `cyclical_time()` creates a cyclical time representation using specified
#' cycle and chronon (and optionally display granules). The cycles is the 
#' larger time unit that defines the time period over which the chronon loops
#' over (e.g. a week, `tu_week(1L)`). The chronon is the smaller time unit that
#' iterates within each cycle (e.g. a day, `tu_day(1L)`). Combined, these two
#' granules form a cyclical time point (e.g. day of the week).
#' 
#' @param chronon A time unit object representing the chronon (e.g., `tu_month(1L)`)
#' @param cycle A time unit object representing the cycle (e.g., `tu_year(1L)`)
#' 
#' @return An function used to create cyclical time points.
#' 
#' @examples
#' 
#' day_of_week <- cyclical_time(tu_day(1L), tu_week(1L))
#' day_of_week(Sys.Date())
#' 
#' month_of_year <- cyclical_time(tu_month(1L), tu_year(1L))
#' month_of_year(Sys.Date())
#' 
#' @export
cyclical_time <- function(chronon, cycle) {
  # if (!all(vapply(granules, function(g) inherits(g, "mixtime::mt_unit"), logical(1L)))) {
  #   stop("All elements in granules must be time unit objects", call. = FALSE)
  # }
  
  if (!inherits(chronon, "mixtime::mt_unit")) {
    stop("`chronon` must be a time unit object", call. = FALSE)
  }
  if (!inherits(cycle, "mixtime::mt_unit")) {
    stop("`cycle` must be a time unit object", call. = FALSE)
  }
  
  # TODO: Ensure c(granules, chronon) are in decreasing order of size

  function(.data, tz = NULL, discrete = TRUE) {
    # Get timezone from .data
    if (is.null(tz)) {
      tz <- suppressWarnings(lubridate::tz(.data))
    }
    
    # Cast to continuous time from Date, POSIXct, etc.
    if (!is.numeric(.data) || !is.null(attributes(.data))) {
      .data <- chronon_convert(.data, chronon, discrete = discrete)
    }

    # Reduce to cyclical time using modulo arithmetic
    .data <- .data%%chronon_cardinality(cycle, chronon) + 1L

    if (!is.character(tz) || length(tz) != 1L) {
      cli::cli_abort("{tz} must be a length 1 string describing the timezone. Mixed timezones currently need to be combined separately.")
    }

    vctrs::new_vctr(
      .data, 
      class = c("mt_cyclical", "mt_time"),
      tz = tz, chronon = chronon, cycle = cycle
    )
  }
}

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

  cyclical_labels(chronon, cycle, vec_data(x))
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

#' @export
seq.mt_cyclical <- function(from, to,
                            # by = ((to - from)/(length.out - 1),
                             ...) {
  # Capture extra arguments
  args <- rlang::list2(...)
  
  # Convert mt_cyclical to numeric for seq() method
  if (!missing(from) && inherits(from, "mt_cyclical")) {
    ptype <- from
    args$from <- vctrs::vec_data(from)
  }
  if (!missing(to) && inherits(to, "mt_cyclical")) {
    # Require compatible cyclical time objects for from:to
    if (!is.null(args$from)) {
      vec_assert(to, from)
    } else {
      ptype <- to
    }
    args$to <- vctrs::vec_data(to)
  }

  # Call the usual numeric seq() method
  res <- rlang::inject(seq(!!!args))

  # Restore mt_cyclical attributes
  period <- chronon_cardinality(attr(ptype, "cycle"), attr(ptype, "chronon"))
  vec_restore((res - 1L) %% period + 1L, ptype)
}