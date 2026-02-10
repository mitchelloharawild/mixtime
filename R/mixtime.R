#' Create a new mixtime
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The list of time values to become a mixtime.
#'
#' @importFrom rlang is_empty
#' @export
new_mixtime <- function(x = list()) {
  validate_x <- vapply(x, mixtime_valid, logical(1L))

  if (!all(validate_x)) {
    stop("A mixtime must contain only valid time points")
  }
  vecvec::new_vecvec(x, class = "mixtime")
}

#' @export
mixtime <- function(...) {
  new_mixtime(rlang::list2(...))
}

#' Convert time class into a mixtime
#'
#' @param x A time value to convert to a mixtime
#' @param ... Additional arguments for methods
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, new_mixtime())
}

#' Check if the object is a mixtime
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `mixtime` class.
#'
#' @examples
#' is_mixtime(Sys.Date())
#' is_mixtime(yearmonth(1))
#'
#' @export
is_mixtime <- function(x) {
  inherits(x, "mixtime")
}

#' Check if times can be used within mixtime
#' 
#' @param x A vector of times.
#' 
#' @return A logical vector.
#' 
#' @export
mixtime_valid <- function(x) {
  UseMethod("mixtime_valid")
}

#' @export
mixtime_valid.default <- function(x) {
  isTRUE(tsibble::index_valid(x))
}

#' @export
mixtime_valid.mixtime <- function(x) TRUE

#' @export
vec_ptype_full.mixtime <- function(x, ...) "mixtime"

#' @export
vec_ptype_abbr.mixtime <- function(x, ...) "mixtime"

vec_cast_to_mixtime <- function(x, to, ...) mixtime(x)

vec_cast_from_mixtime <- function(x, to, ...) {
  class(x) <- setdiff(class(x), "mixtime")
  vec_cast(x, to, ...)
}

## Custom vec cast methods since some time classes don't have cast methods

#' @export
#' @method vec_cast.character mixtime
vec_cast.character.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.character, ...)
  vecvec::unvecvec(x)
}

#' @export
#' @method vec_cast.double mixtime
vec_cast.double.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.double, ...)
  vecvec::unvecvec(x)
}

#' @export
vec_ptype2.mixtime <- function(x, y, ...) {
  x_is_time <- isTRUE(index_valid(x))
  y_is_time <- isTRUE(index_valid(y))

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

#' @export
#' @importFrom vctrs vec_proxy_order
vec_proxy_order.mixtime <- function(x, ...) {
  if (length(attr(x, "v")) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(attr(x, "v"), time_chronon)
    chronon_type <- chronon_common(!!!chronons)

    attr(x, "v") <- lapply(attr(x, "v"), function(v) {
      if (is.integer(v)) v <- v + 0.5
      mixtime::chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vctrs::vec_data(vecvec::unvecvec(x)))
}

#' @export
seq.mixtime <- function(from, to, by = 1L, length.out = NULL, along.with = NULL, ...) {
  if (!missing(along.with)) {
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
  ptype <- vecvec::unvecvec(if(missing_from) to else from)
  
  if (!missing_by) {
    # If by is provided, exactly two of from/to/length.out must be specified
    n_provided <- sum(!missing_from, !missing_to, !is.null(length.out))
    if (n_provided != 2L) {
      cli::cli_abort("When {.arg by} is provided, exactly two of {.arg from}, {.arg to}, or {.arg length.out} must be specified.")
    }

    # Parse by argument
    if (is.character(by)) by <- parse_time_unit(by)
    # Convert to `from`/`to` time chronons to `by` time chronons
    chronon <- time_chronon(ptype)
    seq_part <- NULL
    arg <- list(by = by, length.out = length.out)
    if (!missing_from) {
      divmod <- chronon_divmod(chronon, by, as.numeric(from))
      seq_part <- divmod$remainder
      arg$from <- divmod$chronon
    }
    if (!missing_to) {
      divmod <- chronon_divmod(chronon, by, as.numeric(to))
      seq_part <- seq_part %||% divmod$remainder
      arg$to <- divmod$chronon
    }

    res <- rlang::exec(seq.int, !!!arg)
    # TODO: safely add the seq_part to handle invalid dates without overflow
    #       (essentially, check the cardinality to see if seq_part overflows it)
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
    
    # TODO: This should call seq.mt_[linear/cyclical]
    res <- rlang::exec(seq.int, !!!arg)
  }
  
  # TODO: This should be done with vec_restore()
  attributes(res) <- attributes(ptype)
  res
}