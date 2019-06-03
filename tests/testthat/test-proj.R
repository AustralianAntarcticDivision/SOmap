test_that("family proj works", {
  expect_equal(family_proj(), "+proj=stere +lon_0=0 +lat_0=0 +datum=WGS84")
  expect_equal(family_proj(true_scale = -71),
               "+proj=stere +lon_0=0 +lat_0=0 +lat_ts=-71 +datum=WGS84")
  expect_equal(family_proj("laea"),"+proj=laea +lon_0=0 +lat_0=0 +datum=WGS84")

expect_equal(family_proj("laea", clon = 147, clat = -42),
             "+proj=laea +lon_0=147 +lat_0=-42 +datum=WGS84")


expect_equal(family_proj("lcc", clon = 147, clat = -42, secant_range = c(-70, -10)),
             "+proj=lcc +lon_0=147 +lat_0=-42 +lat_1=-70 +lat_2=-10 +datum=WGS84")


})
