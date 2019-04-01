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
  layr<-SOproj(layer, target = x$projection)

  suppressWarnings(try(as(sf::st_crop(sf::st_buffer(sf::st_transform(sf::st_as_sf(layr), x$projection), 0), xmin = raster::xmin(x$bathy), xmax = raster::xmax(x$bathy), ymin = raster::ymin(x$bathy), ymax = raster::ymax(x$bathy)), "Spatial"), silent = TRUE))
}
