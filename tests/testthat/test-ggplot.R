context("test ggplot stuff")

test_that("SOgg returns a ggplot object", {
    p <- SOmap(trim = -45)
    pg <- SOgg(p)
    expect_s3_class(pg, "SOmap_gg")
    expect_identical(sort(names(pg)), c("axis_labels", "bathy", "bathy_legend", "border", "coastline", "coord", "ice", "init", "plot_sequence", "projection", "scale_fill", "straight", "target", "theme", "trim"))
    expect_s3_class(plot(pg), "ggplot")

    pm <- SOmanagement(trim = -45, mpa = TRUE, mpa_labels = TRUE,
                ccamlr = TRUE, ccamlr_labels = TRUE, ssru = TRUE, ssru_labels = TRUE,
                ssmu = TRUE, ssmu_labels = TRUE, rb = TRUE, rb_labels = TRUE,
                sprfmorb = TRUE, eez = TRUE, eez_labels = TRUE,
                domains = TRUE, domains_labels = TRUE, iwc = TRUE, iwc_labels = TRUE)
    expect_warning(pg <- SOgg(pm)) ## warning because we aren't also supplying basemap or a SOmap object to get target etc from
    expect_s3_class(pg, "SOmanagement_gg")
    expect_identical(sort(names(pg)), c("ccamlr_ssmu", "ccamlr_ssru", "ccamlr_statistical_areas", "eez", "iwc", "mpa", "plot_sequence", "research_blocks", "sprfmo_research_blocks")) ##"ccamlr_planning_domains",

    ## things in pm should be of expected classes
    expect_is(pm$plot_sequence, "character")
    expect_is(pm$projection, "character")
    ## everything else in p should be a list of SO_plotter objects
    for (cmp in setdiff(names(pm), c("plot_sequence", "projection"))) {
        expect_is(pm[[cmp]], "list")
        expect_true(all(vapply(pm[[cmp]], inherits, "SO_plotter", FUN.VALUE = TRUE)))
    }

    ## gg-ify p together with pm
    pg <- SOgg(p, pm) ## no warning this time
    expect_s3_class(pg, "SOmap_gg")
    expect_identical(sort(names(pg)), c("axis_labels", "bathy", "bathy_legend", "border", "ccamlr_ssmu", "ccamlr_ssru", "ccamlr_statistical_areas", "coastline", "coord", "eez", "ice", "init", "iwc", "mpa", "plot_sequence", "projection", "research_blocks", "scale_fill", "sprfmo_research_blocks", "straight", "target", "theme", "trim")) ##"ccamlr_planning_domains",
    expect_s3_class(plot(pg), "ggplot")

    ## things in pg should be of expected classes
    expect_is(pg$target, "Raster")
    expect_is(pg$plot_sequence, "character")
    expect_is(pg$projection, "character")
    expect_is(pg$straight, "logical")
    expect_is(pg$trim, "numeric")
    ## everything else in p should be a list of SO_plotter objects
    for (cmp in setdiff(names(pg), c("target", "plot_sequence", "projection", "straight", "trim"))) {
        expect_is(pg[[cmp]], "list")
        expect_true(all(vapply(pg[[cmp]], inherits, "SO_plotter", FUN.VALUE = TRUE)))
    }

    ## should get the same thing from SOmap2 directly
    ## use the full set of management layer options here, just so all code is exercised
    p2 <- SOmap2(trim = -45, mpa = TRUE, mpa_labels = TRUE,
                ccamlr = TRUE, ccamlr_labels = TRUE, ssru = TRUE, ssru_labels = TRUE,
                ssmu = TRUE, ssmu_labels = TRUE, rb = TRUE, rb_labels = TRUE,
                sprfmorb = TRUE, eez = TRUE, eez_labels = TRUE,
                domains = TRUE, domains_labels = TRUE, iwc = TRUE, iwc_labels = TRUE)
    pg2 <- SOgg(p2)
    expect_setequal(names(pg2), names(pg))
})

test_that("SOgg works on SOmap_auto objects", {
    p <- SOmap_auto(c(0, 50), c(-70, -50))
    pg <- SOgg(p)
    expect_s3_class(pg, "SOmap_auto_gg")
    expect_s3_class(plot(pg), "ggplot")
})

