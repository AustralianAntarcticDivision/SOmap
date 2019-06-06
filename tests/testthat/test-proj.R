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


test_that("mesh points works", {
  expect_equivalent(mesh_points(extent(-15.5, -0.5, 0, 15)),
  as.matrix(expand.grid(-15:-1, seq(14.5, 0.5, by = -1))))
})

#set.seed(2)
#dput(automap_nothing())

test_that("auto nothing works", {
  set.seed(2)
  expect_equivalent(automap_nothing(),
                    structure(c(-71.2769712005906, 74.9547382269224, 81.9319443961065,
                                -41.2274147906253, -48.3425192907426, -49.2859369685298),
                              .Dim = c(3L, 2L), .Dimnames = list(NULL, c("x", "y")))
                    tolerance = 0.001)
})


