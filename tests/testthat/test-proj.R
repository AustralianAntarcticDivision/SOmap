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
                    structure(c(-113.120607898551, -9.12177578104636, 22.8314257148454,
                                64.9031664206073, 148.363627839754, 165.527071137363, -52.1624040772778,
                                -68.0054312106626, -50.3706465095109, -52.1588634960823, -72.053785265492,
                                -53.2950798169202), .Dim = c(6L, 2L), .Dimnames = list(NULL,
                                                                                       c("x", "y"))),
                    tolerance = 0.001)
})
