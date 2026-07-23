# ----------------------------------------------------------------
# class_mixtime methods
# ----------------------------------------------------------------

#' @export
method(vec_ptype_full, class_mixtime) <- function(x, ...) "mixtime"

#' @export
method(vec_ptype_abbr, class_mixtime) <- function(x, ...) "mixtime"


time_valid <- function(x) {
  if (is_mixtime(x)) return(TRUE)
  !inherits(try(time_chronon(x), silent = TRUE), "try-error")
}

vec_ptype2_mixtime <- function(x, y, ...) {
  x_is_time <- time_valid(x)
  y_is_time <- time_valid(y)

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

vec_cast_to_mixtime <- function(x, to, ...) {
  mixtime(x)
}

#' @importFrom vctrs vec_proxy_order
method(vec_proxy_order, class_mixtime) <- function(x, ...) {
  if (length(x@x) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(x@x, function(v) attr(v, "chronon"))
    chronon_type <- chronon_common_impl(chronons)

    x@x <- lapply(x@x, function(v) {
      if (is.integer(v)) v <- v + 0.5
      chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vecvec::unvecvec(x))
}

#' @importFrom vctrs vec_proxy_equal
method(vec_proxy_equal, class_mixtime) <- function(x, ...) {
  data_frame(
    x = as.numeric(x),
    g = unvecvec(vecvec_apply(x, function(x) rep(rlang::hash(attr(x, "chronon")), length(x))))
  )
}

# ------------------------------------------------------------------------------
# vec_cast / vec_ptype2 methods for fundamental modes of time
# ------------------------------------------------------------------------------

# --- casts from a time vector to a base type ---
mt_cast_to_character <- function(x, to, ...) time_format_impl(x)
mt_cast_to_integer <- function(x, to, ...) vec_cast(vec_data(x), integer())
mt_cast_to_double <- function(x, to, ...) vec_cast(vec_data(x), double())
mt_cast_to_Date <- function(x, to, ...) {
  # Convert to naive time zone dates
  vec_restore(chronon_convert(x, cal_gregorian$day(1L, tz = NA_character_)), to)
}
mt_cast_to_POSIXct <- function(x, to, ...) {
  vec_restore(chronon_convert(x, cal_gregorian$second(1L, tz = "UTC"), discrete = FALSE), to)
}

# --- casts from a base type to a time vector ---
mt_cast_from_numeric <- function(x, to, ...) {
  attributes(x) <- attributes(to)
  x
}

# --- time-to-time cast (same kind) ---
mt_cast_time_time <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (S7_inherits(to, mt_duration)) {
    discrete <- is.integer(vec_data(to))
    x <- as.numeric(x) * chronon_cardinality(attr(to, "chronon"), attr(x, "chronon"))
    if (discrete) x <- as.integer(x)
    attributes(x) <- attributes(to)
    return(x)
  }
  x <- chronon_convert(x, attr(to, "chronon"), discrete = is.integer(to))
  attributes(x) <- attributes(to)
  x
}

# --- prototype2 between two time vectors of the same kind ---
mt_ptype2_time_time <- function(x, y, ..., x_arg = "", y_arg = "") {
  x_dur <- S7_inherits(x, mt_duration)
  y_dur <- S7_inherits(y, mt_duration)
  x_cyc <- S7_inherits(x, mt_cyclical)
  y_cyc <- S7_inherits(y, mt_cyclical)
  if (x_dur != y_dur || x_cyc != y_cyc) {
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
  if (x_cyc) {
    return(mt_cyclical(
      chronon = chronon_common_impl(list(attr(x, "chronon"), attr(y, "chronon"))),
      cycle = chronon_common_impl(list(attr(x, "cycle"), attr(y, "cycle")))
    ))
  }
  data <- vec_ptype2(vec_data(x), vec_data(y))
  chronon <- chronon_common_impl(list(attr(x, "chronon"), attr(y, "chronon")))
  if (x_dur) {
    mt_duration(data, chronon = chronon)
  } else {
    mt_linear(data, chronon = chronon)
  }
}

# --- duration <-> numeric prototypes (only durations combine with bare numbers) ---
mt_ptype2_duration_x <- function(x, y, ...) x
mt_ptype2_y_duration <- function(x, y, ...) y

# Register the `vec_cast`/`vec_ptype2` methods above on the package-namespaced time
# classes. Called from `.onLoad()`.
register_mt_vctrs <- function() {
  types <- c("mixtime::mt_linear", "mixtime::mt_cyclical", "mixtime::mt_duration")
  for (cls in types) {
    # time -> base type
    register_s3_method("vctrs", "vec_cast.character", cls, mt_cast_to_character)
    register_s3_method("vctrs", "vec_cast.integer", cls, mt_cast_to_integer)
    register_s3_method("vctrs", "vec_cast.double", cls, mt_cast_to_double)
    # base type -> time
    register_s3_method("vctrs", "vec_cast", paste0(cls, ".integer"), mt_cast_from_numeric)
    register_s3_method("vctrs", "vec_cast", paste0(cls, ".double"), mt_cast_from_numeric)
    # time -> time (same kind) and prototype2 (same kind)
    register_s3_method("vctrs", "vec_cast", paste0(cls, ".", cls), mt_cast_time_time)
    register_s3_method("vctrs", "vec_ptype2", paste0(cls, ".", cls), mt_ptype2_time_time)
  }

  # A linear time vector can be cast to Date / POSIXct.
  register_s3_method("vctrs", "vec_cast.Date", "mixtime::mt_linear", mt_cast_to_Date)
  register_s3_method("vctrs", "vec_cast.POSIXct", "mixtime::mt_linear", mt_cast_to_POSIXct)

  # Durations combine with bare numbers.
  dur <- "mixtime::mt_duration"
  register_s3_method("vctrs", "vec_ptype2", paste0(dur, ".double"), mt_ptype2_duration_x)
  register_s3_method("vctrs", "vec_ptype2", paste0("double.", dur), mt_ptype2_y_duration)
  register_s3_method("vctrs", "vec_ptype2", paste0(dur, ".integer"), mt_ptype2_duration_x)
  register_s3_method("vctrs", "vec_ptype2", paste0("integer.", dur), mt_ptype2_y_duration)
}
