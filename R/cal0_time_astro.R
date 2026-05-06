naive_loc <- naive(NA_real_)

# #' Location unit class with geographic coordinates
# #'
# #' A time unit class that extends mt_unit to include geographic location
# #' information for solar-based time calculations.
# #'
#' @param lat Numeric. Latitude in decimal degrees. Range: -90 to 90.
#'   Default: naive location (astronomical calculations require `lat`).
#' @param lon Numeric. Longitude in decimal degrees. Range: -180 to 180.
#'   Default: naive location (astronomical calculations require `lon`).
#' @param alt Numeric. Altitude in meters above sea level.
#'   Default: 0 (sea level).
#'
#' @rdname mt_unit
#' @export
mt_loc_unit <- S7::new_class(
  "mt_loc_unit", 
  parent = mt_unit,
  properties = list(
    lat = S7::new_property(S7::class_numeric, default = naive_loc),
    lon = S7::new_property(S7::class_numeric, default = naive_loc),
    alt = S7::new_property(S7::class_numeric, default = naive(0))
  ),
  # constructor = function (n = 1L, lat = naive_loc, lon = naive_loc, alt = naive(0)) {
  #   S7::new_object(mt_unit(n = n), lat = lat, lon = lon, alt = alt)
  # },
  validator = function(self) {
    NULL
  }
)

# Default formats
method(chronon_format_attr, mt_loc_unit) <- function(x) {
  if (!is.naive(x@lat) && !is.naive(x@lon)) " [{loc(.time)}]" else ""
}
