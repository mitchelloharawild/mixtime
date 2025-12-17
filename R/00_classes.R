mt_unit_s3 <- S7::new_S3_class(
  "mixtime::mt_unit", 
  constructor = function(.data = 1L) .data,
  validator = function(object) {
    if (!typeof(object) %in% c("integer", "double")) {
      sprintf("Underlying data must be <integer> or <double>,  not <%s>", typeof(object))
    }
  }
)

#' Base S7 class for time units
#' 
#' @param .data The number of time units
#' 
#' @return A time unit object of class `mt_unit`
#' 
#' @export
#' @rdname mt_unit
mt_unit <- S7::new_class("mt_unit", parent = mt_unit_s3)