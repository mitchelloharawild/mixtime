test_that("scale_type() selects the mixtime scale", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggtime", minimum_version = "0.2.0.9000")
  library(ggtime)

  expect_identical(ggplot2::scale_type(yearmonth(36L + 0:11)), "mixtime")
})

test_that("scale_type() reports when ggtime is unavailable", {
  # search() is mocked, since the real search path cannot be manipulated
  # once ggtime has been attached by an earlier test in this session.
  local_mocked_bindings(search = function() c(".GlobalEnv", "package:ggtime"), .package = "base")
  expect_true(check_ggtime_attached("0.2.0.9000"))

  local_mocked_bindings(search = function() ".GlobalEnv", .package = "base")
  expect_error(check_ggtime_attached("0.2.0.9000"), "ggtime")
})
