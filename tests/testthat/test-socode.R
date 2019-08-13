context("test SOcode")

socode_test1 <- function(mapcode) {
    expect_named(mapcode, c("code", "ddddata"))
    expect_is(mapcode$code, "character")
    expect_is(mapcode$ddddat, "list")
    ## eval the code, it should run without complaint
    blah <- with(mapcode, for (cd in code) eval(parse(text = cd)))
}


test_that("SOcode works with base plot objects", {
    p <- SOmap(trim = -45, graticules = TRUE, fronts = TRUE)
    mapcode <- SOcode(p, data_object_name = "ddddata")
    socode_test1(mapcode)
})

test_that("SOcode works with ggplot2 objects", {
    p <- SOmap(trim = -45, graticules = TRUE, fronts = TRUE)
    mapcode <- SOcode(SOgg(p), data_object_name = "ddddata")
    socode_test1(mapcode)
})
