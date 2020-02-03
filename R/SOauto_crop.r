#' Reproject and crop Spatial Polygons to SOmap objects
#'
#' @param layer A SpatialPolygonsDataFrame.
#' @param x A SOmap or SOauto_map object.
#'
#' @return reprojected and cropped version of layer
#' @export
#'
#' @examples
#' \dontrun{
#' a<-SOauto_map(c(0, 50), c(-70, -50))
#' x<-SOauto_crop(SOmap_data$fronts_orsi,  a)
#' a
#' plot(x, add = TRUE)
#' }
#'
SOauto_crop<-function(layer, x){
    layr <- SOproj(layer, target = x$projection)
    ## use the extent of the bathymetry raster object
    extobj <- x$bathy
    if (is.list(x$bathy) && length(x$bathy) > 0 && inherits(x$bathy[[1]], "SO_plotter")) {
        ## the bathy raster data itself is buried in the plotargs
        extobj <- x$bathy[[1]]$plotargs$x
    }
    suppressWarnings(try({
        out <- sf::st_transform(sf::st_as_sf(layr), x$projection)
        check <- sf::st_buffer(out, 0)
        if (!all(sf::st_is_empty(check))) out <- check ## don't buffer e.g. points, because you get empty geometries back
        as(sf::st_crop(out, xmin = raster::xmin(extobj), xmax = raster::xmax(extobj), ymin = raster::ymin(extobj), ymax = raster::ymax(extobj)), "Spatial")
    }, silent = TRUE))
}
