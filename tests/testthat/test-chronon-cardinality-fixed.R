test_that("chronon_cardinality_fixed() returns the unit-granule constant", {
  expect_equal(chronon_cardinality_fixed(cal_time_civil$hour(1L), cal_time_civil$day(1L)), 24L)
  expect_equal(chronon_cardinality_fixed(cal_isoweek$day(1L), cal_isoweek$week(1L)), 7L)

  # Variable relationships are not registered as chronon_cardinality_fixed()
  expect_error(chronon_cardinality_fixed(cal_gregorian$day(1L), cal_gregorian$month(1L)))
})

test_that("chronon_cardinality() falls back to chronon_cardinality_fixed(), scaled by granule size", {
  expect_equal(chronon_cardinality(cal_time_civil$hour(1L), cal_time_civil$day(1L)), 24)
  expect_equal(chronon_cardinality(cal_time_civil$hour(2L), cal_time_civil$day(3L)), 24 * 3 / 2)

  # The inverse (coarser, finer) order is automatically derived
  expect_equal(chronon_cardinality(cal_gregorian$year(1L), cal_gregorian$month(1L)), 1 / 12)
})

test_that("only fixed cardinality (and direct divmod) relationships form divmod graph edges", {
  tu_fixed_test_a <- S7::new_class("tu_fixed_test_a", parent = mt_unit)
  tu_fixed_test_b <- S7::new_class("tu_fixed_test_b", parent = mt_unit)
  tu_variable_test_a <- S7::new_class("tu_variable_test_a", parent = mt_unit)
  tu_variable_test_b <- S7::new_class("tu_variable_test_b", parent = mt_unit)

  method(chronon_cardinality_fixed, list(tu_fixed_test_a, tu_fixed_test_b)) <- function(x, y) 5L
  method(chronon_cardinality, list(tu_variable_test_a, tu_variable_test_b)) <- function(x, y, at = NULL) {
    if (is.null(at)) cli::cli_abort("requires `at`")
    5L
  }

  edge_exists <- function(graph, a, b) {
    ia <- vec_match(S7_class_id(a), graph$chr_classes)
    ib <- vec_match(S7_class_id(b), graph$chr_classes)
    if (is.na(ia) || is.na(ib)) return(FALSE)
    any(graph$edge_from == ia & graph$edge_to == ib) ||
      any(graph$edge_from == ib & graph$edge_to == ia)
  }

  # Both relationships are visible to the cardinality graph, used for
  # ordering/topology by chronon_common() and time_parts().
  expect_true(edge_exists(chronon_cardinality_graph(), tu_fixed_test_a(1L), tu_fixed_test_b(1L)))
  expect_true(edge_exists(chronon_cardinality_graph(), tu_variable_test_a(1L), tu_variable_test_b(1L)))

  # Only the fixed relationship is safe to use for chronon_divmod() graph
  # traversal, since chronon_divmod_regular() calls chronon_cardinality()
  # without an `at`.
  expect_true(edge_exists(chronon_divmod_graph(), tu_fixed_test_a(1L), tu_fixed_test_b(1L)))
  expect_false(edge_exists(chronon_divmod_graph(), tu_variable_test_a(1L), tu_variable_test_b(1L)))
})
