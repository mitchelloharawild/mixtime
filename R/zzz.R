.onLoad <- function(...) {
  vctrs_exports <- getNamespaceExports(asNamespace("vctrs"))

  vec_cast_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_cast.")]
  # Register vec_cast.*.mixtime methods
  # lapply(vec_cast_generics, register_s3_method,
  #        pkg = "vctrs", class = "mixtime", fun = vec_cast_from_mixtime)
  # Register vec_cast.mixtime.* methods
  lapply(sub("^vec_cast", "mixtime", vec_cast_generics), register_s3_method,
         pkg = "vctrs", generic = "vec_cast", fun = vec_cast_to_mixtime)

  vec_ptype2_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_ptype2.")]
  # Register vec_ptype2.*.mixtime methods
  lapply(vec_ptype2_generics, register_s3_method,
         pkg = "vctrs", class = "mixtime", fun = vec_ptype2.mixtime)
  # Register vec_ptype2.mixtime.* methods
  lapply(sub("^vec_ptype2", "mixtime", vec_ptype2_generics), register_s3_method,
         pkg = "vctrs", generic = "vec_ptype2", fun = vec_ptype2.mixtime)

  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "mixtime", class = "mixtime", fun = dispatch_elements)
  invisible()
}
