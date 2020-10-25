# nocov start
.onLoad <- function(...) {
  register_s3_method("tsibble", "index_valid", "moment")
  register_s3_method("tsibble", "interval_pull", "moment")
  register_s3_method("tsibble", "interval_pull", "moment_time_units")
  register_s3_method("tsibble", "interval_pull", "tu_day")
  register_s3_method("tsibble", "interval_pull", "tu_week")
  register_s3_method("tsibble", "interval_pull", "tu_month")
  register_s3_method("tsibble", "interval_pull", "tu_quarter")
  register_s3_method("tsibble", "interval_pull", "tu_year")
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
