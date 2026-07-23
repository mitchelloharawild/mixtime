.onLoad <- function(...) {
  # Register S7 methods
  S7::methods_register()

  # Register vecvec methods
  vecvec::vecvec_register(
    class_mixtime,
    f_ptype2 = vec_ptype2_mixtime,
    f_cast_to = vec_cast_to_mixtime
  )

  # Register vec_cast / vec_ptype2 for the mt_time family on their package-namespaced
  # concrete classes (vctrs' custom double dispatch for these generics does not use
  # inheritance). Arithmetic is handled by native S7 operator methods (see arithmetic.R).
  register_mt_vctrs()

  # Initialise tzdb package
  tzdb::tzdb_initialize()

  # Register all methods
  # lapply(attr(methods(class = "default"), "info")$generic, register_s3_method,
  #        pkg = "mixtime", class = "mixtime", fun = dispatch_elements)
  invisible()
}
