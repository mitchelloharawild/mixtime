#' @method as.double mixtime_gregorian
#' @export
as.double.mixtime_gregorian <- function(x, ...) {
  as.double(vec_data(x))
}

#' @export
Ops.mixtime_gregorian <- function(e1, e2) {
  if(rlang::is_missing(e2)) {
    stop("Unary operations on 'mixtime_gregorian' objects are not supported.", call. = FALSE)
  }
  is_mixtime_e1 <- inherits(e1, "mixtime_gregorian")
  is_mixtime_e2 <- inherits(e2, "mixtime_gregorian")
  if (is_mixtime_e1 && is_mixtime_e2) {
    # stop("Operations between two 'mixtime_gregorian' objects are not supported.", call. = FALSE)
    # Temporarily allowable for ggtime development, to be formalised as `<duration>` methods later
    return(do.call(.Generic, list(as.double(e1), as.double(e2))))
  }

  if (is_mixtime_e1) {
    out <- do.call(.Generic, list(as.double(e1), e2))
    attributes(out) <- attributes(e1)
  } else {
    out <- do.call(.Generic, list(e1, as.double(e2)))
    attributes(out) <- attributes(e2)
  }
  out
}

#' Represent years
#'
#' Create or coerce using `year()`
#'
#' \lifecycle{experimental}
#'
#' @param x Another object to be coerced into years
#' @param ... Arguments for methods.
#'
#' @export
year <- function(x, ...){
  UseMethod("year")
}

#' @export
new_year <- function(x) {
  vctrs::new_vctr(x, class = c("mixtime_year", "mixtime_gregorian"))
}

#' @export
year.numeric <- function(x, ...) {
  mixtime(new_year(x - 1970L))
}

#' @export
year.default <- function(x, ...) {
  mixtime(new_year(as.integer(strftime(x, "%Y")) - 1970L))
}

#' @export
format.mixtime_year <- function(x, ...) {
  format(vec_data(x) + 1970L, ...)
}

#' @export
index_valid.mixtime_year <- function(x) TRUE

#' @export
interval_pull.mixtime_year <- function(x) {
  tsibble::new_interval(
    year = tsibble::gcd_interval(vec_data(x))
  )
}

#' Represent yearquarters
#'
#' Create or coerce using `yearquarter()`
#'
#' \lifecycle{experimental}
#'
#' @param x Another object to be coerced into yearquarters
#' @param ... Arguments for methods.
#'
#' @export
yearquarter <- function(x, ...){
  UseMethod("yearquarter")
}

#' @export
new_yearquarter <- function(x) {
  vctrs::new_vctr(x, class = c("mixtime_yearquarter", "mixtime_gregorian"))
}

#' @export
yearquarter.numeric <- function(x, ...) {
  mixtime(new_yearquarter(x))
}

#' @export
yearquarter.Date <- function(x, ...) {
  # Convert to yearquarters since Unix epoch (1970-01-01)
  year_val <- as.integer(strftime(x, "%Y"))
  quarter_val <- as.integer((as.integer(strftime(x, "%m")) - 1L) %/% 3L) + 1L
  yearquarters_since_epoch <- (year_val - 1970L) * 4L + (quarter_val - 1L)
  mixtime(new_yearquarter(yearquarters_since_epoch))
}

#' @export
yearquarter.POSIXt <- yearquarter.Date

#' @export
yearquarter.default <- function(x, ...) {
  yearquarter(as.Date(x), ...)
}

#' @export
format.mixtime_yearquarter <- function(x, ...) {
  x <- vec_data(x)
  paste(x %/% 4L + 1970L, " Q", x %% 4L + 1L, sep = "")
}

#' @export
index_valid.mixtime_yearquarter <- function(x) TRUE

#' @export
interval_pull.mixtime_yearquarter <- function(x) {
  tsibble::new_interval(
    quarter = tsibble::gcd_interval(vec_data(x))
  )
}

#' Represent yearmonths
#'
#' Create or coerce using `yearmonth()`
#'
#' \lifecycle{experimental}
#'
#' @param x Another object to be coerced into yearmonths
#' @param ... Arguments for methods.
#'
#' @export
yearmonth <- function(x, ...){
  UseMethod("yearmonth")
}

#' @export
new_yearmonth <- function(x) {
  
  vctrs::new_vctr(x, class = c("mixtime_yearmonth", "mixtime_gregorian"))
}

#' @export
yearmonth.numeric <- function(x, ...) {
  mixtime(new_yearmonth(x))
}

#' @export
yearmonth.Date <- function(x, ...) {
  # Convert to yearmonths since Unix epoch (1970-01-01)
  year_val <- as.integer(strftime(x, "%Y"))
  yearmonth_val <- as.integer(strftime(x, "%m"))
  yearmonths_since_epoch <- (year_val - 1970L) * 12L + (yearmonth_val - 1L)
  mixtime(new_yearmonth(yearmonths_since_epoch))
}

#' @export
yearmonth.POSIXt <- yearmonth.Date

#' @export
yearmonth.default <- function(x, ...) {
  yearmonth(as.Date(x), ...)

  # continuous_time(granules = list(tu_year(1L)), chronon = tu_month(1L))))
}

# 1970-01-01
# yearmonthday <- continuous_time(granules = list(tu_year(1L), tu_month(1L)), chronon = tu_day(1L))

#' @export
format.mixtime_yearmonth <- function(x, ...) {
  x <- vec_data(x)
  paste(x %/% 12L + 1970L, month.abb[x %% 12L + 1L])
}

#' @export
index_valid.mixtime_yearmonth <- function(x) TRUE

#' @export
interval_pull.mixtime_yearmonth <- function(x) {
  tsibble::new_interval(
    month = tsibble::gcd_interval(vec_data(x))
  )
}

#' Represent yearweeks
#'
#' Create or coerce using `yearweek()`
#'
#' \lifecycle{experimental}
#'
#' @param x Another object to be coerced into yearweeks
#' @param ... Arguments for methods.
#'
#' @export
yearweek <- function(x, ...){
  UseMethod("yearweek")
}

#' @export
new_yearweek <- function(x) {
  
  vctrs::new_vctr(x, class = c("mixtime_yearweek", "mixtime_gregorian"))
}

#' @export
yearweek.numeric <- function(x, ...) {
  mixtime(new_yearweek(x))
}

#' @export
yearweek.Date <- function(x, ...) {
  # Convert to yearweeks since Unix epoch (1970-01-01)
  # Using ISO week date system
  epoch_date <- as.Date("1970-01-01")
  weeks_since_epoch <- as.integer((x - epoch_date) / 7)
  mixtime(new_yearweek(weeks_since_epoch))
}

#' @export
yearweek.POSIXt <- yearweek.Date

#' @export
yearweek.default <- function(x, ...) {
  yearweek(as.Date(x), ...)
}

#' @export
format.mixtime_yearweek <- function(x, ...) {
  x <- vec_data(x)
  epoch_date <- as.Date("1970-01-01")
  week_dates <- epoch_date + (x * 7)
  year_val <- as.integer(strftime(week_dates, "%Y"))
  week_val <- as.integer(strftime(week_dates, "%U")) + 1L
  paste(year_val, " W", sprintf("%02d", week_val))
}

#' @export
index_valid.mixtime_yearweek <- function(x) TRUE

#' @export
interval_pull.mixtime_yearweek <- function(x) {
  tsibble::new_interval(
    week = tsibble::gcd_interval(vec_data(x))
  )
}
