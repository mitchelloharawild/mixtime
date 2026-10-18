card_up <- function(acc, from, to) acc * chronon_cardinality_fixed(from, to)
card_down <- function(acc, from, to) acc * chronon_cardinality_fixed(to, from)
traverse_values <- function(x) vapply(x, function(e) e$value, numeric(1L))
# Breadth-first order among a unit's neighbours follows method registration
# order, which other tests can change, so values are compared as a map.

test_that("chronon_traverse() folds fixed cardinalities to coarser units", {
  expect_mapequal(
    traverse_values(chronon_traverse(cal_gregorian$month(1L), card_up, 1)),
    c("mixtime::tu_quarter" = 3, "mixtime::tu_year" = 12)
  )
  expect_mapequal(
    traverse_values(chronon_traverse(cal_gregorian$day(1L), card_up, 1)),
    c("mixtime::tu_week" = 7)
  )
})

test_that("chronon_traverse() granules inherit the time zone of `x`", {
  traversal <- chronon_traverse(
    cal_gregorian$hour(1L, tz = "Australia/Melbourne"), card_up, 1
  )
  expect_mapequal(
    traverse_values(traversal),
    c("mixtime::tu_ampm" = 12, "mixtime::tu_day" = 24, "mixtime::tu_week" = 168)
  )
  for (e in traversal) {
    expect_equal(e$granule@tz, "Australia/Melbourne")
    expect_equal(e$granule@n, 1L)
  }
})

test_that("chronon_traverse() stops beyond granules satisfying `.until`", {
  visited <- character()
  traversal <- chronon_traverse(
    cal_gregorian$hour(1L),
    function(acc, from, to) {
      visited <<- c(visited, class(to)[[1L]])
      card_up(acc, from, to)
    },
    1,
    .until = function(acc, granule) acc >= 15
  )
  expect_mapequal(
    traverse_values(traversal),
    c("mixtime::tu_ampm" = 12, "mixtime::tu_day" = 24)
  )
  # Only reachable through a day, so never visited.
  expect_false("mixtime::tu_week" %in% visited)
})

test_that("chronon_traverse() walks to finer units", {
  traversal <- chronon_traverse(
    cal_gregorian$day(1L), card_down, 1,
    .until = function(acc, granule) acc > 15,
    direction = "finer"
  )
  expect_mapequal(
    traverse_values(traversal),
    c("mixtime::tu_hour" = 24, "mixtime::tu_ampm" = 2)
  )
})

test_that("chronon_traverse() accumulates non-numeric values", {
  traversal <- chronon_traverse(
    cal_gregorian$minute(1L),
    function(acc, from, to) c(acc, sub("mixtime::tu_", "", class(to)[[1L]])),
    "minute"
  )
  expect_equal(
    traversal[["mixtime::tu_week"]]$value,
    c("minute", "hour", "day", "week")
  )
})

test_that("chronon_traverse() can walk context-dependent relationships", {
  traversal <- chronon_traverse(
    cal_gregorian$day(1L), function(acc, from, to) acc + 1L, 0L,
    graph = "cardinality"
  )
  expect_equal(traversal[["mixtime::tu_month"]]$value, 1L)
  expect_equal(traversal[["mixtime::tu_year"]]$value, 1L)
  expect_equal(traversal[["mixtime::tu_quarter"]]$value, 2L)
  expect_false("mixtime::tu_month" %in% names(chronon_traverse(
    cal_gregorian$day(1L), card_up, 1
  )))
})

test_that("chronon_traverse() visits each unit once", {
  for (direction in c("coarser", "finer")) {
    for (graph in c("fixed", "cardinality")) {
      traversal <- chronon_traverse(
        cal_gregorian$second(1L), function(acc, from, to) acc + 1L, 0L,
        direction = direction, graph = graph
      )
      ids <- vapply(traversal, function(e) class(e$granule)[[1L]], character(1L))
      expect_false(anyDuplicated(ids) > 0L)
      expect_equal(names(traversal), unname(ids))
      expect_false("mixtime::tu_second" %in% ids)
    }
  }
})

test_that("chronon_traverse() gives nothing for unregistered units", {
  tu_unrelated <- new_time_unit("tu_unrelated")
  expect_equal(
    chronon_traverse(tu_unrelated(1L), card_up, 1),
    rlang::set_names(list())
  )
})

test_that("chronon_traverse() validates its inputs", {
  expect_error(chronon_traverse(1, card_up, 1), "time granule")
  expect_error(chronon_traverse(cal_gregorian$day(1L), "f", 1), "function")
  expect_error(
    chronon_traverse(cal_gregorian$day(1L), card_up, 1, direction = "up")
  )
})
