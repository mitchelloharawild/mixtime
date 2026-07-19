scale_type <- S7::new_external_generic("ggplot2", "scale_type", "x")
method(scale_type, class_mixtime) <- function(x) {
  check_ggtime_attached("0.2.0.9000")
  "mixtime"
}

check_ggtime_attached <- function(version) {
  if ("package:ggtime" %in% search()) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "Plotting {.pkg mixtime} vectors requires the {.pkg ggtime} package.",
      i = if (!requireNamespace("ggtime", quietly = TRUE)) {
        "Install it with {.run install.packages(\"ggtime\")}."
      } else if (package_version(getNamespaceVersion("ggtime")) < version) {
        "Update to {.pkg ggtime} {version} or later with {.run install.packages(\"ggtime\")}."
      } else {
        "Attach it with {.run library(ggtime)}."
      }
    ),
    call = NULL
  )
}
