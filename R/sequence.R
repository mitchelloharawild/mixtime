#' Generate sequences of mixtime values
#' 
#' Create regular sequences of time values. This method handles both linear
#' time sequences (dates, date-times) and cyclical time sequences (day of week,
#' month of year).
#' 
#' @param from Starting value of the sequence.
#' @param to End value of the sequence (if provided).
#' @param by Increment of the sequence. Can be:
#'   * An integer for the number of time units
#'   * A character string specifying the interval (e.g., "1 day", "2 weeks", 
#'     "1 month", "1 year")
#'   * A time unit object created with time unit functions (e.g., `cal_gregorian$year(1L)`,
#'     `cal_gregorian$month(1L)`, `cal_gregorian$day(1L)`)
#' @param length.out Desired length of the sequence (alternative to `to`).
#' @param along.with Take the length from this argument (alternative to `length.out`).
#' @param on_invalid How to handle time points that overflow the cycle when
#'   using a `by` argument with different time units than the sequence type.
#'   Options are:
#'   * `"nearest"` (default): Adjust overflowing time points to the nearest 
#'     valid time point within the cycle
#'   * `"overflow"`: Allow time points to overflow into the next cycle
#'   
#'   This is relevant when the starting time point has an offset that doesn't
#'   exist in all cycles. For example, starting on day 31 with `by = "1 month"`
#'   will overflow in months with fewer than 31 days (e.g., February). With
#'   `"nearest"`, these will be adjusted to the last day of the month (e.g.,
#'   Feb 28/29). With `"overflow"`, the extra days carry into the next month.
#'   
#'   If not explicitly specified and overflow occurs, a warning is issued with
#'   the default `"nearest"` behavior applied.
#' @param ... Additional arguments passed to the underlying sequence method.
#' 
#' @return A mixtime vector containing the sequence.
#' 
#' @details
#' For linear time types (Date, POSIXct, yearmonth, etc.), sequences progress
#' forward or backward in time. For cyclical time types (month_of_year, 
#' day_of_week, etc.), sequences wrap around cyclically.
#' 
#' @examples
#' # Linear time sequences with integer by
#' seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"))
#' seq(yearquarter("2020-01-01"), length.out = 5, by = 3)
#' 
#' # Linear time sequences with string intervals
#' seq(yearmonthday("2020-01-01"), yearmonthday("2020-12-31"), by = "1 month")
#' seq(yearmonth("2020-01-01"), yearmonth("2025-01-01"), by = "1 year")
#' seq(yearmonthday("2020-01-01"), length.out = 10, by = "2 weeks")
#' 
#' # Linear time sequences with time units
#' seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"), by = cal_gregorian$month(2L))
#' seq(yearmonthday("2020-01-01"), length.out = 5, by = cal_gregorian$year(1L))
#' seq(yearmonthday("2020-01-01"), yearmonthday("2020-01-31"), by = cal_gregorian$day(7L))
#' 
#' # Handling invalid dates with on_invalid
#' seq(yearmonthday("2020-01-31"), length.out = 3, by = "1 month")  # warns, uses nearest
#' seq(yearmonthday("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "nearest")
#' seq(yearmonthday("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "overflow")
#' 
#' # Cyclical time sequences
#' seq(month_of_year(0L), month_of_year(11L))
#' seq(month_of_year(5L), month_of_year(3L), by = cal_gregorian$month(2L))
#' seq(day_of_week(0L), day_of_week(6L), by = 1)
#' 
#' @rdname seq.mixtime
#' @export
seq.mixtime <- function(...) {
  # Strip mixtime vecvec wrapper
  arg <- lapply(
    rlang::list2(...), 
    function(x) if(is_mixtime(x)) vecvec::unvecvec(x) else x
  )
  
  # Call seq method with bare vectors
  mixtime(rlang::exec(seq, !!!arg))
}

#' @rdname seq.mixtime
#' @export
seq.mt_linear <- function(
  from, to, by, length.out = NULL, along.with = NULL, 
  on_invalid = c("nearest", "overflow"), ...
) {
  if (!is.null(along.with)) {
    length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
    if (length(length.out) != 1L) 
      stop(sprintf("'%s' must be of length 1", length.out))
    length.out <- ceiling(length.out)
  }

  missing_from <- missing(from)
  missing_to <- missing(to)
  missing_by <- missing(by)

  # Check chronon compatibility
  if (!missing_from && !missing_to) {
    from_chronon <- time_chronon(from)
    to_chronon <- time_chronon(to)
    
    # Check that from and to have the same time classes
    if (!identical(from_chronon, to_chronon)) {
      cli::cli_abort(c(
        "Incompatible time chronones in {.fn seq}.",
        "i" = "{.arg from} has {.cls {paste(from_chronon, time_unit_full(from_chronon))}} chronons.",
        "i" = "{.arg to} has {.cls {paste(to_chronon, time_unit_full(to_chronon))}} chronons."
      ))
    }
  }
  ptype <- if(missing_from) to else from
  
  if (!missing_by) {
    # If by is provided, exactly two of from/to/length.out must be specified
    n_provided <- sum(!missing_from, !missing_to, !is.null(length.out))
    if (n_provided != 2L) {
      cli::cli_abort("When {.arg by} is provided, exactly two of {.arg from}, {.arg to}, or {.arg length.out} must be specified.")
    }

    # Sequence chronon
    chronon <- time_chronon(ptype)

    # Parse `by` argument
    if (is.character(by)) by <- parse_time_unit(by)
    # Numeric `by` uses chronon
    if (is.numeric(by) && !S7::S7_inherits(by, mt_unit)) {
      attributes(by) <- attributes(chronon)
    }
    
    # Convert to `from`/`to` time chronons to `by` time chronons
    seq_part <- NULL
    # Increment by 1 since the time point chronon is rebased with `by`
    arg <- list(by = 1L)
    if (!missing_from) {
      # Convert `from` into `by` chronons
      divmod <- chronon_divmod(chronon, by, as.numeric(from))
      arg$from <- divmod$chronon

      # Left aligned sequencing
      seq_part <- divmod$remainder
    }
    if (!missing_to) {
      # Shift `to` left to account for `from` alignment
      if (!is.null(seq_part)) to <- to - seq_part

      # Convert `to` into `by` chronons
      divmod <- chronon_divmod(chronon, by, as.numeric(to))
      arg$to <- divmod$chronon
      
      # Right aligned sequencing if from isn't provided
      seq_part <- seq_part %||% divmod$remainder
    }
    if (!is.null(length.out)) arg$length.out <- length.out

    res <- rlang::exec(seq.int, !!!arg)
    # TODO: safely add the seq_part to handle invalid dates without overflow
    #       (essentially, check the cardinality to see if seq_part overflows it)
    # This should be the default (also add detection if it applies, then give a warning if the argument is not explicitly provided)
    
    missing_on_invalid <- is.character(on_invalid) && length(on_invalid) > 1L
    on_invalid <- match.arg(on_invalid)
    cycle_size <- chronon_cardinality(by, chronon, res)
    # Check if the sequence offset will overflow the cycle
    if (any(if (by < 0) cycle_size >= seq_part else cycle_size <= seq_part)) {
      if (missing_on_invalid) {
        cli::cli_warn(c(
          "The cycle offset ({seq_part + 1L} {time_unit_full(chronon)}{cli::qty(seq_part)}{?s}) has produced time points that overflow the {time_unit_full(by)} cycle.",
          "!" = "Using the nearest valid time points in the cycle, {.code on_invalid = \"nearest\"} (the default).",
          "i" = "Specify {.arg on_invalid} explicitly to suppress this warning."
        ))
      }
      if (on_invalid == "nearest") {
        seq_part <- (if (by < 0) pmax else pmin)(cycle_size - 1L, seq_part)
      }
    }
    
    res <- chronon_convert_impl(res, by, chronon, discrete = is.integer(res)) + seq_part

    # Restore integer type for discrete time input
    if (is.integer(ptype)) res <- as.integer(res)
  } else {
    # If by is missing, at least two of from/to/length.out must be specified
    n_provided <- sum(!missing_from, !missing_to, !is.null(length.out))
    if (n_provided < 2L) {
      cli::cli_abort("When {.arg by} is missing, at least two of {.arg from}, {.arg to}, or {.arg length.out} must be specified.")
    }

    arg <- list(length.out = length.out)
    if (!missing_from) arg$from <- as.numeric(from)
    if (!missing_to) arg$to <- as.numeric(to)
    
    res <- rlang::exec(seq.int, !!!arg)
  }
  
  vec_restore(res, ptype)
}

#' @rdname seq.mixtime
#' @export
seq.mt_cyclical <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
  if (!is.null(along.with)) {
    length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
    if (length(length.out) != 1L) 
      stop(sprintf("'%s' must be of length 1", length.out))
    length.out <- ceiling(length.out)
  }

  # Capture extra arguments
  arg <- rlang::list2(...)
  if (!missing(by)) arg$by <- by
  if (!is.null(length.out)) arg$length.out <- length.out
  # NB: Passing along.with = NULL to seq.int causes an overflow in R
  if (!is.null(along.with)) arg$along.with <- along.with
  
  # Parse `by` argument
  if (is.character(arg$by)) arg$by <- parse_time_unit(arg$by)

  # Convert mt_cyclical to numeric for seq() method
  if (!missing(from) && inherits(from, "mt_cyclical")) {
    ptype <- from
    arg$from <- vctrs::vec_data(from)
  }
  if (!missing(to) && inherits(to, "mt_cyclical")) {
    arg$to <- vctrs::vec_data(to)
    if (!is.null(arg$from)) {
      # Require compatible cyclical time objects for from:to
      vec_assert(to, from)
    } else {
      ptype <- to
    }
  }

  # Cyclical period
  period <- chronon_cardinality(attr(ptype, "cycle"), attr(ptype, "chronon"))

  # Adjust from:to for looping around cycle
  if (!is.null(arg$to) && !is.null(arg$from)) {
    if (arg$by%||%1 > 0 && to < from) {
      arg$to <- arg$to + period
    } else if (to > from && arg$by%||%1 < 0) {
      arg$to <- arg$to - period
    }
  }

  # Convert `by` to match `ptype` units
  if (!is.null(arg$by) && S7::S7_inherits(arg$by, mt_unit)) {
    arg$by <- chronon_cardinality(arg$by, time_chronon(ptype))
  }

  # Generate linear sequence
  res <- rlang::inject(seq.int(!!!arg))

  # Compute cyclical component
  res <- res %% period
  
  # Restore integer type for discrete time input
  if (is.integer(ptype)) res <- as.integer(res)
  
  # Restore cyclical time attributes
  vec_restore(res, ptype)
}