#' Check the time type of values
#'
#' Test whether elements of a `mixtime` vector are linear, cyclical, or durations.
#'
#' These helpers return a logical vector the same length as `x` identifying the
#' type of time represented by each element.
#'
#' @param x A time object (typically a `mixtime` vector).
#' @param ... Additional arguments for methods.
#' 
#' @return A logical vector the same length as `x`.
#' 
#' @examples
#' t <- c(yearmonth(0), month_of_year(0), months(0L))
#' is_time_linear(t)
#' is_time_cyclical(t)
#' is_time_duration(t)
#' 
#' @name is_time
#' @export
is_time_linear <- S7::new_generic("is_time_linear", "x")
method(is_time_linear, class_mixtime) <- vecvec::vecvec_apply_fn(is_time_linear, logical())
method(is_time_linear, class_any) <- function(x) rep.int(FALSE, length(x))
method(is_time_linear, S7::new_S3_class("mt_linear")) <- function(x) rep.int(TRUE, length(x))

#' @rdname is_time
#' @export
is_time_cyclical <- S7::new_generic("is_time_cyclical", "x")
method(is_time_cyclical, class_mixtime) <- vecvec::vecvec_apply_fn(is_time_cyclical, logical())
method(is_time_cyclical, class_any) <- function(x) rep.int(FALSE, length(x))
method(is_time_cyclical, S7::new_S3_class("mt_cyclical")) <- function(x) rep.int(TRUE, length(x))

#' @rdname is_time
#' @export
is_time_duration <- S7::new_generic("is_time_duration", "x")
method(is_time_duration, class_mixtime) <- vecvec::vecvec_apply_fn(is_time_duration, logical())
method(is_time_duration, class_any) <- function(x) rep.int(FALSE, length(x))
method(is_time_duration, S7::new_S3_class("mt_duration")) <- function(x) rep.int(TRUE, length(x))