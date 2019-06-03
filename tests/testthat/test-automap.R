context("test-automap")

disp_auto_map <- function() plot(SOauto_map(c(100:110), c(-70:-60)))

test_that("auto map works", {

  ## works with no input
   tst <- SOauto_map()
expect_s3_class(tst, "SOauto_map")
   ## works with sp input
   SOauto_map(SOmap_data$seaice_feb) %>% expect_s3_class("SOauto_map")

   SOauto_map(ice) %>% expect_s3_class("SOauto_map")
    x <- SOauto_map(c(100:110), c(-70:-60))
    expect_s3_class(x, "SOauto_map")
    nms <- sort(c("projection", "bathy", "bathyleg", "bathy_palette",
                  "coastline", "target", "lines_data", "points_data",
                  "ppch", "pcol", "pcex", "llty", "llwd", "lcol", "contours", "levels", "contour_colour", "graticule", "gratpos", "crs"))
    expect_identical(sort(names(x)), nms)


    expect_s3_class(rx <- reproj(x, "+proj=laea +lat_0=-40 +lon_0=110 +datum=WGS84"), "SOauto_map")
    expect_identical(sort(names(rx)), nms)
    expect_true(grepl("laea", rx$projection))

    expect_silent(print(rx))
    expect_message(SOplot(c(102, 105), c(-64, -68)))
    expect_silent(SOplot(SOmap_data$continent))
    expect_silent(SOplot(SOmap_data$seaice_oct))
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("Soauto_map basic", disp_auto_map)
})

disp_resblocks <- function() plot(SOauto_map(SOmap_data$CCAMLR_research_blocks[c(1, 4, 5), ]))
test_that("auto map plots polygons", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("Soauto_map research blocks", disp_resblocks)
})

#SOmap
disp_somap <- function() SOmap()
test_that("SOmap plots", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("Somap basemap",
                                disp_somap)
})

#SOmap2
disp_somap2 <- function() SOmap2(ccamlr = TRUE)
test_that("SOmap2 plots", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("Somap2 basemap",
                                disp_somap2)
})

#SOleg
disp_soleg <- function() {SOmap()
  SOleg(ticks=6, tlabs = seq(1:6))}
test_that("SOmap legends", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("SOmap legends",
                                disp_soleg)
})


#SOmanagement
disp_soman <- function() {SOmap()
    SOmanagement(ccamlr = TRUE)}
test_that("SOmap management", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("SOmap management",
                                disp_soman)
})
