context("test-automap")

disp_auto_map <- function() plot(SOmap_auto(c(100:110), c(-70:-60)))

test_that("auto map works", {

  ## works with no input
   tst <- SOmap_auto()
   expect_s3_class(tst, "SOmap_auto")
   ## works with sp input
   SOmap_auto(SOmap_data$seaice_feb) %>% expect_s3_class("SOmap_auto")

   SOmap_auto(ice) %>% expect_s3_class("SOmap_auto")
   x <- SOmap_auto(c(100:110), c(-70:-60))
   expect_s3_class(x, "SOmap_auto")
   nms <- sort(c("bathy", "coastline", "crs", "graticule", "ice", "init", "lines", "plot_sequence", "points", "projection", "target"))
   expect_identical(sort(names(x)), nms)


   expect_s3_class(rx <- reproj(x, "+proj=laea +lat_0=-40 +lon_0=110 +datum=WGS84"), "SOmap_auto")
   expect_identical(sort(names(rx)), nms)
   expect_true(grepl("laea", rx$projection))

   expect_silent(print(rx))
   expect_message(SOplot(c(102, 105), c(-64, -68)))
   #expect_silent(SOplot(SOmap_data$continent))
   #expect_silent(SOplot(SOmap_data$seaice_oct))
   skip("skipping vdiffr tests temporarily")
   vdiffr::expect_doppelganger("SOmap_auto basic", disp_auto_map)
})

test_that("dataframe and matrix input works",{
    #expect_silent(a <- SOmap_auto(data.frame(x = c(140, 150, 160), y = c(-30, -40, -60))))
    #expect_silent(a <- SOmap_auto(cbind(x = c(140, 150, 160), y = c(-30, -40, -60))))
})

test_that("check sp", {
    gr <- as(graticule::graticule(seq(100, 180, by = 5), seq(-70, -30, by = 8)), "SpatialPoints")
    SOmap_auto(gr)
})

disp_resblocks <- function() plot(SOmap_auto(SOmap_data$CCAMLR_research_blocks[c(1, 4, 5), ]))
test_that("auto map plots polygons", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("SOmap_auto research blocks", disp_resblocks)
})

## SOmap
disp_somap <- function() SOmap()
test_that("SOmap plots", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("Somap basemap", disp_somap)
})

## SOmap2
disp_somap2 <- function() SOmap2(ccamlr = TRUE)
test_that("SOmap2 plots", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("Somap2 basemap", disp_somap2)
})

## SOleg
disp_soleg <- function() {
    SOmap()
    SOleg(ticks=6, tlabs = seq(1:6))
}
test_that("SOmap legends", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("SOmap legends", disp_soleg)
})


## SOmanagement
disp_soman <- function() {
    SOmap()
    SOmanagement(ccamlr = TRUE)
}
test_that("SOmap management", {
    skip("skipping vdiffr tests temporarily")
    vdiffr::expect_doppelganger("SOmap management", disp_soman)
})
