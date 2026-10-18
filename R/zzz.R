.onLoad <- function(...) {
  # Register S7 methods
  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_load")()
  } else {
    S7::methods_register()
  }

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

  # Calendars created while mixtime was built are found in its namespace
  calendar_registry <<- new.env(parent = emptyenv())

  # Initialise tzdb package
  tzdb::tzdb_initialize()

  invisible()
}

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_unload")()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
