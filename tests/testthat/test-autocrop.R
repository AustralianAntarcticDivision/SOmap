context("test-autocrop")
test_that("SOauto_crop works", {
    ## maps of different extent
    x30 <- SOmap(trim = -30)
    x60 <- SOmap(trim = -60)
    x85 <- SOmap(trim = -85)

    ## crop a SpatialPolygonsDataFrame
    chk <- SOauto_crop(SOmap_data$CCAMLR_SSRU, x30)
    ## Spatial by default
    expect_is(chk, "Spatial")
    ## in this case, nothing should have been cropped
    expect_equal(nrow(chk), nrow(SOmap_data$CCAMLR_SSRU))
    chk <- SOauto_crop(SOmap_data$CCAMLR_SSRU, x30, sp = FALSE)
    expect_is(chk, "sf")
    expect_true(all(sf::st_geometry_type(chk) == "POLYGON"))

    ## crop a SpatialLinesDataFrame
    chk <- SOauto_crop(SOmap_data$fronts_orsi, x60)
    ## removed some features
    expect_lt(nrow(chk), nrow(SOmap_data$fronts_orsi))
    ## as sf
    chk <- SOauto_crop(SOmap_data$fronts_orsi, x60, sp = FALSE)
    expect_true(all(grepl("LINESTRING", sf::st_geometry_type(chk))))

    ## crop a LINESTRING sf object
    expect_true(all(sf::st_geometry_type(SOmap_data$fronts_park) == "LINESTRING"))
    chk <- SOauto_crop(SOmap_data$fronts_park, x60)
    expect_lt(nrow(chk), nrow(SOmap_data$fronts_park))
    expect_is(chk, "SpatialLinesDataFrame")
    chk <- SOauto_crop(SOmap_data$fronts_park, x60, sp = FALSE)
    expect_is(chk, "sf")
    ## LINESTRING comes back as MULTILINESTRING
    expect_true(all(sf::st_geometry_type(chk) == "MULTILINESTRING"))

    ## cropping returns empty object
    chk <- SOauto_crop(SOmap_data$EEZ, x85, sp = FALSE)
    ## expect sf object with no rows
    expect_is(chk, "sf")
    expect_equal(nrow(chk), 0L)
    chk <- SOauto_crop(SOmap_data$EEZ, x85, sp = TRUE)
    ## expect NULL, because we can't coerce a zero-row sf object to Spatial
    expect_null(chk)

    ## cropping should also work with gg reference map
    chk <- SOauto_crop(SOmap_data$fronts_orsi, SOgg(x60))
    chk2 <- SOauto_crop(SOmap_data$fronts_orsi, x60)
    expect_equal(chk, chk2)

    ## and SOmap_auto
    amap <- SOmap_auto(c(-70,-80), c(-50,-60))
    chk <- SOauto_crop(SOmap_data$CCAMLR_statistical_areas, amap)
    chk2 <- SOauto_crop(SOmap_data$CCAMLR_statistical_areas, SOgg(amap))
    expect_equal(chk, chk2)
})
