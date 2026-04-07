#' Extract locations from an object
#'
#' Generic function to extract the location from objects that have location information.
#' 
#' @param x An object with location information.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric value representing the location (e.g., longitude, latitude, etc.).
#'
#' @examples
#' t <- linear_time(
#'   1:3, 
#'   cal_time_solar_sunset$day(1L, lat = -37.8136, lon = 144.9631)
#' )
#' 
#' loc_longitude(t)
#' loc_latitude(t)
#' loc_altitude(t)
#' 
#' @name mixtime_location
#' @export
loc_latitude <- S7::new_generic("loc_latitude", "x")
S7::method(loc_latitude, mt_loc_unit) <- function(x) x@lat
S7::method(loc_latitude, S7::new_S3_class("mt_time")) <- function(x) {
  rep_len(loc_latitude(time_chronon(x)), length(x))
}
S7::method(loc_latitude, S7::new_S3_class("mixtime")) <- function(x) {
  as.numeric(vecvec::vecvec_apply(x, loc_latitude))
}
S7::method(loc_latitude, S7::class_any) <- function(x) NA_real_

#' @name mixtime_location
#' @export
loc_longitude <- S7::new_generic("loc_longitude", "x")
S7::method(loc_longitude, mt_loc_unit) <- function(x) x@lon
S7::method(loc_longitude, S7::new_S3_class("mt_time")) <- function(x) {
  rep_len(loc_longitude(time_chronon(x)), length(x))
}
S7::method(loc_longitude, S7::new_S3_class("mixtime")) <- function(x) {
  as.numeric(vecvec::vecvec_apply(x, loc_longitude))
}
S7::method(loc_longitude, S7::class_any) <- function(x) NA_real_

#' @name mixtime_location
#' @export
loc_altitude <- S7::new_generic("loc_altitude", "x")
S7::method(loc_altitude, mt_loc_unit) <- function(x) x@alt
S7::method(loc_altitude, S7::new_S3_class("mt_time")) <- function(x) {
  rep_len(loc_altitude(time_chronon(x)), length(x))
}
S7::method(loc_altitude, S7::new_S3_class("mixtime")) <- function(x) {
  as.numeric(vecvec::vecvec_apply(x, loc_altitude))
}
S7::method(loc_altitude, S7::class_any) <- function(x) NA_real_