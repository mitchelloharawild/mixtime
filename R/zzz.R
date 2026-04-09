.onLoad <- function(...) {
  # Register S7 methods
  S7::methods_register()
  
  # Initialise tzdb package
  tzdb::tzdb_initialize()
       
  # Register tsibble methods
  register_s3_method("tsibble", "index_valid", "mixtime", index_valid.mixtime)
  register_s3_method("tsibble", "interval_pull", "mixtime", interval_pull.mixtime)
  register_s3_method("tsibble", "index_valid", "mt_linear", index_valid.mt_linear)
  register_s3_method("tsibble", "interval_pull", "mt_linear", interval_pull.mt_linear)
       
  vctrs_exports <- getNamespaceExports(asNamespace("vctrs"))
  vec_cast_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_cast.")]
  # Register vec_cast.*.mixtime methods
  lapply(vec_cast_generics, register_s3_method,
         pkg = "vctrs", class = "mixtime::mixtime", fun = vec_cast_from_mixtime)
  # Register vec_cast.mixtime.* methods
  lapply(sub("^vec_cast", "mixtime::mixtime", vec_cast_generics), register_s3_method,
         pkg = "vctrs", generic = "vec_cast", fun = vec_cast_to_mixtime)

  vec_ptype2_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_ptype2.")]
  # Register vec_ptype2.*.mixtime methods
  lapply(vec_ptype2_generics, register_s3_method,
         pkg = "vctrs", class = "mixtime::mixtime", fun = `vec_ptype2.mixtime::mixtime`)
  # Register vec_ptype2.mixtime.* methods
  lapply(sub("^vec_ptype2", "mixtime::mixtime", vec_ptype2_generics), register_s3_method,
         pkg = "vctrs", generic = "vec_ptype2", fun = `vec_ptype2.mixtime::mixtime`)

  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "mixtime", class = "mixtime", fun = dispatch_elements)
  invisible()
}
