
#' @importFrom tsibble index_valid
#' @export
index_valid.mixtime <- function(x) {
  TRUE
}

#' @importFrom tsibble interval_pull
#' @export
interval_pull.mixtime <- function(x) {
  intvls <- lapply(attr(x, "v"), function(x) interval_pull(x))
  x <- new_rcrd(
    vec_rbind(!!!lapply(intvls, new_data_frame)),
    .regular = any(vapply(intvls, attr, logical(1L), which = ".regular")),
    class = c("mixtime_interval", "interval")
  )
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

#' @export
format.mixtime_interval <- function(x, ...) {
  fmt <- vapply(
    vctrs::new_rcrd(unclass(x)),
    fmt_interval,
    # function(y) format(`class<-`(y, "interval")),
    character(1L)
  )
  # tsibble requires length 1 interval formatting
  paste(unique(fmt), collapse = ", ")
}

fmt_interval <- function (x, ...) {
  if (!attr(x, ".regular"))
    return("!")
  if (all(vec_proxy(x) == 0))
    return("?")
  n <- n_fields(x)
  micro <- ifelse(cli::is_utf8_output(), "\u00b5s", "us")
  defaults <- vec_c("Y", "Q", "M", "W", "D", "h", "m", "s",
                    "ms", micro, "ns", "")
  n_defaults <- vec_size(defaults)
  misc <- if (n > n_defaults)
    vec_slice(fields(x), (n_defaults + 1):n)
  else NULL
  fmt_names <- vec_c(defaults, misc)
  val <- unlist(unclass(x), use.names = FALSE)
  paste0(val[val != 0], fmt_names[val != 0], collapse = " ")
}

#' @export
print.mixtime_interval <- function(x, ...) cat(format(x, ...))

#' @export
vec_proxy.mixtime_interval <- function(x, ...) {
  # Bypass tsibble:::unknown_interval length 1 checks
  apply(new_data_frame(x), 2, sum, simplify = FALSE)
}

#' @export
vec_cast.mixtime.yearweek <- function(x, to, ...) {
  yearweek(as.Date(x))
}

#' @export
vec_cast.mixtime.yearmonth <- function(x, to, ...) {
  yearmonth(as.Date(x))
}

#' @export
vec_cast.mixtime.yearquarter <- function(x, to, ...) {
  yearquarter(as.Date(x))
}