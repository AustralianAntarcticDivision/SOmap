context("test SOmap")
p <- SOmap(trim = -45, graticules = TRUE, fronts = TRUE)

test_that("SOmap returns a SOmap object", {
    expect_s3_class(p, "SOmap")
    ##expect_silent(print(p))  ## spatial warnings 2020
    print(p)
    ## things in p should be of expected classes
    expect_is(p$target, "Raster")
    expect_is(p$plot_sequence, "character")
    expect_is(p$projection, "character")
    expect_is(p$straight, "logical")
    expect_is(p$trim, "numeric")
    expect_is(p$bathy_legend[[1]], "SOmap_legend")
    ## everything else in p should be a list of SO_plotter objects
    for (cmp in setdiff(names(p), c("target", "plot_sequence", "projection", "straight", "trim", "bathy_legend"))) {
        expect_is(p[[cmp]], "list")
        expect_true(all(vapply(p[[cmp]], inherits, "SO_plotter", FUN.VALUE = TRUE)))
    }
})

test_that("adding data to a plot is fine", {
    longitudes <- c(-180, -90, 0, 90)
    latitudes <- c(-50, -60, -50,-60)
    expect_silent(SOplot(longitudes, latitudes, pch = 1:4))
    ## expect_silent(SOplot(SOmap_data$seaice_oct))

    SOplot(raster::crop(Bathy, raster::extent(1e6, 2e6, 1e6, 3e6)))
    ##  expect_silent(SOplot(p$graticule$main$plotargs$x))
})

test_that("bathy_legend options work", {
    pT <- SOmap(bathy_legend = TRUE) ## yes, bathy legend
    expect_true(all(c("outer_mask", "bathy_legend") %in% names(pT)))
    expect_true(all(c("outer_mask", "bathy_legend") %in% pT$plot_sequence))
    pF <- SOmap(bathy_legend = FALSE) ## no bathy legend, and no space left for bathy legend
    expect_false(any(c("outer_mask", "bathy_legend") %in% names(pF)))
    expect_false(any(c("outer_mask", "bathy_legend") %in% pF$plot_sequence))
    ## bathy will be cropped differently if no bathy legend shown
    expect_false(identical(raster::extent(pF$target), raster::extent(pT$target)))
    ## space for legend but no actual legend
    pN <- SOmap(bathy_legend = NULL)
    ps <- SOmap(bathy_legend = "space")
    expect_identical(ps, pN)
    ## pT and pN should have their bathy cropped to the same extent
    expect_identical(raster::extent(pT$target), raster::extent(pN$target))
    expect_true("outer_mask" %in% names(pN))
    expect_false("bathy_legend" %in% names(pN))
    expect_true("outer_mask" %in% pN$plot_sequence)
    expect_false("bathy_legend" %in% pN$plot_sequence)
})
