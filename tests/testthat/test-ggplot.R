context("test ggplot stuff")

test_that("SOgg returns a ggplot object", {
    ## use the full SOmap2 set of layer options here, just so all code is exercised
    p <- SOmap2(trim = -45, graticules = TRUE, fronts = TRUE, mpa = TRUE, mpa_labels = TRUE,
                ccamlr = TRUE, ccamlr_labels = TRUE, ssru = TRUE, ssru_labels = TRUE,
                ssmu = TRUE, ssmu_labels = TRUE, rb = TRUE, rb_labels = TRUE,
                sprfmorb = TRUE, eez = TRUE, eez_labels = TRUE,
                domains = TRUE, domains_labels = TRUE, iwc = TRUE, iwc_labels = TRUE)
    pg <- SOgg(p)
    expect_s3_class(pg, "ggplot")
})
