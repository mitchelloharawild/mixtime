S7_method_docs <- function() {
  calls <- sys.calls()
  fn_base <- "fn()" # fallback

  if (length(calls) > 1) {
    for (call in rev(calls[-length(calls)])) {
      call_str <- paste(deparse(call, width.cutoff = 500L), collapse = "")
      fn_part <- sub("\\s*\\(.*", "", call_str)
      fn_part <- trimws(fn_part)
      if (!nzchar(fn_part)) next

      # Remove namespace if present (pkg::fn.method -> fn.method)
      fn_name <- sub("^.*:::+", "", fn_part)
      fn_name <- gsub("`", "", fn_name)

      # If the function name contains a dot, take the left side (fn.method -> fn)
      if (grepl("\\.", fn_name)) {
        fn_base <- sub("\\..*$", "", fn_name)
        break
      }
    }
  }

  cli::cli_abort(
    "This function should not be called directly, instead use {.fn {fn_base}}.\n\nThis is a temporary solution for documenting S7 methods, which is not yet possible (https://github.com/RConsortium/S7/issues/315)",
    call = NULL
  )
}

check_tz_name <- function(zone) {
  if (!zone %in% tzdb::tzdb_names()) {
    cli::cli_abort(
      c(
        "Timezone {.val {zone}} not found in timezone database.",
        "i" = "Valid timezone names can be found with {.fn tzdb::tzdb_names}."
      ),
      call = NULL
    )
  }
  invisible(TRUE)
}