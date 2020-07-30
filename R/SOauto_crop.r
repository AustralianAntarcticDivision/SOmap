#' Reproject and crop Spatial and sf objects to SOmap objects
#'
#' @param layer : an `sf` or `Spatial` (SpatialPolygonsDataFrame, SpatialLinesDataFrame, SpatialPointsDataFrame etc) object to reproject and crop
#' @param x : a SOmap or SOauto_map object
#' @param sp logical: if `TRUE`, return the cropped object in `Spatial` form, otherwise `sf`
#'
#' @return If successful, a reprojected and cropped version of `layer`. If the reprojection or cropping operations fail, the returned object will be of class `try-error`. If the cropping operations return an empty object (i.e. no parts of `layer` lie within the bounds of `x`) then the returned object will either be `NULL` (if `sp = TRUE`) or an `sf` object with no features if `sp = FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#'   a <- SOmap_auto(c(0, 50), c(-70, -50))
#'   x <- SOauto_crop(SOmap_data$fronts_orsi, a)
#'   plot(a)
#'   plot(x, add = TRUE)
#'
#'   a <- SOmap(trim = -60)
#'   x <- SOauto_crop(SOmap_data$EEZ, a)
#'   plot(a)
#'   plot(x, add = TRUE)
#' }
#'
SOauto_crop<-function(layer, x, sp = TRUE) {
    assert_that(inherits(x, c("SOmap", "SOmap_auto", "SOmap_gg", "SOmap_auto_gg")))
    layr <- SOproj(layer, target = x$projection)
    quietly(auto_crop(layr, x = x, sp = sp))
}

## private function to do the cropping
##  also used directly by some other functions, e.g. SOauto_map
auto_crop <- function(layr, x, sp = TRUE) {
    suppressWarnings(try({
        out <- sf::st_transform(sf::st_as_sf(layr), x$projection)
        check <- sf::st_buffer(out, 0)
        if (!all(sf::st_is_empty(check))) out <- check ## don't buffer e.g. points, because you get empty geometries back
        if (inherits(x, c("SOmap_auto", "SOmap_auto_gg"))) {
            ## use the extent of the bathymetry raster object
            extobj <- if (inherits(x, "SOmap_auto")) x$bathy else x$target
            ## SOmap_auto should be in SO_plotter form, but the gg version won't
            if (is.list(extobj) && length(extobj) > 0 && inherits(extobj[[1]], "SO_plotter")) {
                ## the bathy raster data itself is buried in the plotargs
                extobj <- extobj[[1]]$plotargs$x
            }
            ## crop by the projected extent. If the projection is rectangular, this should be fine. It might not work well with proejctions where the map doesn't have a rectangular visible extent though
            out <- sf::st_crop(out, xmin = raster::xmin(extobj), xmax = raster::xmax(extobj), ymin = raster::ymin(extobj), ymax = raster::ymax(extobj))
        } else {
            ## SOmap or SOmap_gg object
            ## we can't crop to the projected (rectangular) extent of the SOmap object, because it's a round map. The *visible* extent of the map doesn't fill the rectangular projected extent
            ## Note also that for SOmap objects, the extent of the bathy layer is probably NOT the visible extent of the map
            ## because the bathy extends further to provide space for the border and legend
            buf <- make_buf(x$trim, x$projection)
            out <- sf::st_intersection(out, buf)
        }
        if (sp) {
            if (nrow(out) > 0) as(out, "Spatial") else NULL
        } else {
            out
        }
    }, silent = TRUE))
}
