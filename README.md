
[![Travis build status](https://travis-ci.org/AustralianAntarcticDivision/SOmap.svg?branch=master)](https://travis-ci.org/AustralianAntarcticDivision/SOmap) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/AustralianAntarcticDivision/SOmap?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/SOmap) [![Coverage status](https://codecov.io/gh/AustralianAntarcticDivision/SOmap/branch/master/graph/badge.svg)](https://codecov.io/github/AustralianAntarcticDivision/SOmap?branch=master) [![CRAN status](https://www.r-pkg.org/badges/version/SOmap)](https://cran.r-project.org/package=SOmap)

<!-- README.md is generated from README.Rmd. Please edit that file -->
SOmap <img src="man/figures/logo.png" align="right" />
======================================================

The goal of SOmap is to make publication quality round Southern Ocean maps in polar projections with little effort. This package is still very much a work in progress contact me with any questions or suggestions.

Installation
------------

The development version from [GitHub](https://github.com/AustralianAntarcticDivision/SOmap) with:

``` r
## install.packages("remotes") ## if needed
remotes::install_github("AustralianAntarcticDivision/SOmap")
```

Example
-------

To make a simple map you can use the following function; use `? SOmap` to see all the options for modifying layers.

``` r
library(SOmap)
SOmap()
```

<img src="man/figures/README-examplemap-1.png" width="100%" />

There is also `SOmanagement()` which provides management layers for the Southern Ocean and `SOleg()` which gives custom rounded legends for added map layers.

``` r
## custom colours
spiritedMedium <- colorRampPalette(c("#4D4140", "#596F7E", "#168B98", "#ED5B67", "#E27766", "#DAAD50", "#EAC3A6"))
spirited <- spiritedMedium(80)

SOmap(Trim = -40)
## add an example sea ice raster, which is bundled with SOmap
plot(ice, col = spirited, add = TRUE, legend = FALSE, alpha = 0.95)
SOleg(ice, position = "topright", col = spirited, ticks = 6,
      tlabs = c("0", "20", "40", "60", "80", "100"),
      Trim = -40, label = "Sea Ice", type = "continuous")

## add the exclusive economic zones management layer
SOmanagement(EEZ = TRUE)
```

<img src="man/figures/README-management-1.png" width="100%" />

An **automatic** plot function `SOauto_map()` will take any data in the form of longitude and latitude vectors and create a guess at a map.

``` r
ellie <- SOmap_data$mirounga_leonina

## construct and plot the map
SOauto_map(ellie$lon, ellie$lat)
```

<img src="man/figures/README-automap-1.png" width="100%" />

The `SOauto_map`, `SOmap`, and `SOmap2` functions return the data used to make the map so that further customization can be made. Plotting or printing the returned object will cause the map to be displayed in the graphics device.

``` r
data("albatross", package = "adehabitatLT")
## convert the albatross data to a single matrix of lon, lat points
albatrack <- do.call(rbind, lapply(albatross, function(z) rgdal::project(rbind(as.matrix(z[, c("x", "y")]), NA), "+proj=utm +zone=42 +south +datum=WGS84", inv = TRUE)))

## construct the map and return it, but don't plot it
alb_map <- SOauto_map(albatrack[, 1], albatrack[, 2])
```

Modifying this map object is currently a rather experimental process (proceed at your own risk!) but, for example, if we wished to change the points to be blue rather than red:

``` r
alb_map$pcol <- "blue"
## plot it
alb_map
```

<img src="man/figures/README-automap3-1.png" width="100%" />

Objects from `sf` or `sp` may also be used. (If a "raster" is given it is used only for its extent.)

``` r
## use the bundled fronts data as an example
mydata <- SOmap_data$fronts_orsi
SOauto_map(mydata, family = "laea", centre_lon = 147, input_points = FALSE)
```

<img src="man/figures/README-automap-spatial-1.png" width="100%" />

Please note that the SOmap project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
