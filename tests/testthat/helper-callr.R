# Run `func` in a fresh R session with the mixtime under test attached, and
# return its result. Under pkgload::load_all() (e.g. devtools::test()) the
# source package is loaded, since library(mixtime) would load a possibly stale
# installed version. `func` is evaluated in the new session's global
# environment, so it must be self-contained apart from `args`.
r_mixtime <- function(func, args = list()) {
  skip_if_not_installed("callr")
  ns <- asNamespace("mixtime")
  src <- if (exists(".__DEVTOOLS__", envir = ns, inherits = FALSE)) {
    getNamespaceInfo(ns, "path")
  }
  environment(func) <- globalenv()
  callr::r(function(func, args, src) {
    if (is.null(src)) {
      library(mixtime)
    } else {
      pkgload::load_all(src, quiet = TRUE)
    }
    do.call(func, args)
  }, list(func = func, args = args, src = src))
}
