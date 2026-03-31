# #' Location unit class with geographic coordinates
# #'
# #' A time unit class that extends mt_unit to include geographic location
# #' information for solar-based time calculations.
# #'
#' @param lat Numeric. Latitude in decimal degrees. Range: -90 to 90.
#'   Default: 0 (equator).
#' @param lon Numeric. Longitude in decimal degrees. Range: -180 to 180.
#'   Default: 0 (Prime Meridian).
#' @param alt Numeric. Altitude in meters above sea level.
#'   Default: 0 (sea level).
#'
#' @rdname mt_unit
#' @export
mt_loc_unit <- S7::new_class(
  "mt_loc_unit", 
  parent = mt_unit,
  properties = list(
    lat = S7::new_property(S7::class_numeric, default = 0),
    lon = S7::new_property(S7::class_numeric, default = 0),
    alt = S7::new_property(S7::class_numeric, default = 0)
  ),
  validator = function(self) {
    NULL
  }
)