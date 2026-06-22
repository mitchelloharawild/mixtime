.onLoad <- function(...) {
  # Register S7 methods
  S7::methods_register()

  # Register vecvec methods
  vecvec::vecvec_register(
    class_mixtime,
    f_ptype2 = vec_ptype2_mixtime,
    f_cast_to = vec_cast_to_mixtime
  )
  
  # Initialise tzdb package
  tzdb::tzdb_initialize()

  # Register tsibble methods
#   register_s3_method("tsibble", "index_valid", "mixtime", index_valid.mixtime)
#   register_s3_method("tsibble", "interval_pull", "mixtime", interval_pull.mixtime)
#   register_s3_method("tsibble", "index_valid", "mt_linear", index_valid.mt_linear)
  register_s3_method("tsibble", "interval_pull", "mt_time", interval_pull.mt_linear)

  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "mixtime", class = "mixtime", fun = dispatch_elements)
  invisible()
}
