mt_unit_s3 <- S7::new_S3_class(
  "mixtime::mt_unit", 
  constructor = function(.data = 1L) .data,
  validator = function(self) {
    if (!typeof(self) %in% c("integer", "double")) {
      sprintf("Underlying data must be <integer> or <double>,  not <%s>", typeof(self))
    }
  }
)

#' Base S7 class for creating new time units
#' 
#' This class is the primative class for time units, and should 
#' be extended from when creating new time units. A new class
#' is typically created with S7 using: 
#' `tu_day <- S7::new_class("tu_day", parent = mt_unit)`
#' 
#' @param .data The number of time units
#' 
#' @return A time unit object of class `mt_unit`
#' 
#' @export
#' @rdname mt_unit
mt_unit <- S7::new_class("mt_unit", parent = mt_unit_s3)

#' @param tz The timezone name for the unit (valid units can be found with `[tzdb::tzdb_names()]`)
#' @rdname mt_unit
mt_tz_unit <- S7::new_class(
  "mt_tz_unit", 
  parent = mt_unit,
  properties = list(tz = S7::new_property(S7::class_character, default = "UTC")),
  validator = function(self) {
    check_tz_name(self@tz)
    NULL
  }
)