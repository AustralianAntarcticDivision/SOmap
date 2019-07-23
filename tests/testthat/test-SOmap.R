context("test SOmap")
p <- SOmap(trim = -45, graticules = TRUE, fronts = TRUE)

test_that("SOmap returns a SOmap object", {
    expect_s3_class(p, "SOmap")
    expect_silent(print(p))
    ## things in p should be of expected classes
    expect_is(p$target, "Raster")
    expect_is(p$plot_sequence, "character")
    expect_is(p$projection, "character")
    expect_is(p$straight, "logical")
    expect_is(p$trim, "numeric")
    ## everything else in p should be a list of SO_plotter objects
    for (cmp in setdiff(names(p), c("target", "plot_sequence", "projection", "straight", "trim"))) {
        expect_is(p[[cmp]], "list")
        expect_true(all(vapply(p[[cmp]], inherits, "SO_plotter", FUN.VALUE = TRUE)))
    }
})

test_that("adding data to a plot is fine", {
  longitudes <- c(-180, -90, 0, 90)
  latitudes <- c(-50, -60, -50,-60)
  expect_message(SOplot(longitudes, latitudes, pch = 1:4))
  expect_silent(SOplot(SOmap_data$seaice_oct))

  expect_warning(SOplot(raster::crop(Bathy, raster::extent(1e6, 2e6, 1e6, 3e6))))
  expect_silent(SOplot(p$graticule$main$plotargs$x))
})
