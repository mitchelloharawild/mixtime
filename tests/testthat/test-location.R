# ---------------------------------------------------------------------------
# loc_latitude / loc_longitude / loc_altitude
# ---------------------------------------------------------------------------

lat <- -37.8136
lon <- 144.9631
alt <- 100

# ---------------------------------------------------------------------------
# mt_loc_unit directly
# ---------------------------------------------------------------------------
test_that("loc_* accessors work on mt_loc_unit", {
  u <- mt_loc_unit(1L, lat = lat, lon = lon, alt = alt)
  expect_equal(loc_latitude(u),  lat)
  expect_equal(loc_longitude(u), lon)
  expect_equal(loc_altitude(u),  alt)
})

test_that("loc_altitude defaults to 0 when not supplied", {
  u <- mt_loc_unit(1L, lat = lat, lon = lon)
  # The default alt is naive(0); loc_altitude should return 0 (possibly naive)
  expect_equal(as.numeric(loc_altitude(u)), 0)
})

# ---------------------------------------------------------------------------
# mt_time (linear_time with a location-aware chronon)
# ---------------------------------------------------------------------------
test_that("loc_* accessors work on mt_time via solar day chronon", {
  t <- linear_time(1:3, cal_time_solar$day(1L, lat = lat, lon = lon))
  expect_equal(loc_latitude(t),  rep(lat, 3))
  expect_equal(loc_longitude(t), rep(lon, 3))
  expect_equal(loc_altitude(t),  rep(0, 3))   # default altitude
})

test_that("loc_* accessors work on mt_time via solar ampm chronon", {
  t <- linear_time(1:4, cal_time_solar$ampm(1L, lat = lat, lon = lon, alt = alt))
  expect_equal(loc_latitude(t),  rep(lat, 4))
  expect_equal(loc_longitude(t), rep(lon, 4))
  expect_equal(loc_altitude(t),  rep(alt, 4))
})

test_that("loc_* accessors return correct length matching input vector", {
  n <- 5L
  t <- linear_time(seq_len(n), cal_time_solar$day(1L, lat = lat, lon = lon))
  expect_length(loc_latitude(t),  n)
  expect_length(loc_longitude(t), n)
  expect_length(loc_altitude(t),  n)
})

# ---------------------------------------------------------------------------
# class_mixtime (mixed-chronon vector)
# ---------------------------------------------------------------------------
test_that("loc_* accessors work on class_mixtime", {
  t1 <- linear_time(1:2, cal_time_solar$day(1L, lat = lat,    lon = lon))
  t2 <- linear_time(1:2, cal_time_solar$day(1L, lat = 48.133, lon = 11.583))
  mt <- c(t1, t2)
  expect_equal(loc_latitude(mt),  c(lat, lat, 48.133, 48.133))
  expect_equal(loc_longitude(mt), c(lon, lon, 11.583, 11.583))
})

# ---------------------------------------------------------------------------
# class_any fallback — non-location objects return NA_real_
# ---------------------------------------------------------------------------
test_that("loc_* accessors return NA_real_ for non-location objects", {
  expect_identical(loc_latitude(42),       NA_real_)
  expect_identical(loc_longitude("hello"), NA_real_)
  expect_identical(loc_altitude(NULL),     NA_real_)
})

test_that("loc_* accessors return NA_real_ for non-astronomical mt_time", {
  t <- linear_time(1:3, cal_gregorian$day(1L))
  # No location info: each element maps to NA_real_ (length matches input)
  expect_equal(loc_latitude(t),  rep(NA_real_, 3))
  expect_equal(loc_longitude(t), rep(NA_real_, 3))
  expect_equal(loc_altitude(t),  rep(NA_real_, 3))
})

# ---------------------------------------------------------------------------
# Numeric correctness — west longitude / southern hemisphere / high altitude
# ---------------------------------------------------------------------------
test_that("loc_* return correct values for west longitude and high altitude", {
  hi_lat <- 21.3069; hi_lon <- -157.8583; hi_alt <- 350
  t <- linear_time(1:2, cal_time_solar$day(1L, lat = hi_lat, lon = hi_lon, alt = hi_alt))
  expect_equal(loc_latitude(t),  rep(hi_lat, 2))
  expect_equal(loc_longitude(t), rep(hi_lon, 2))
  expect_equal(loc_altitude(t),  rep(hi_alt, 2))
})
