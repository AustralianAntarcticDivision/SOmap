#' Latitude mask for polar rasters.
#'
#' @description
#' Latitude mask for polar projections; written by M.D. Sumner and part of the spex package.
#'
#' @param x
#' A raster layer.
#' @param latitude
#' maximum latitude (effectively a minimum latitude if southern = FALSE)
#' @param southern
#' flag for whether south-polar context is used, default is TRUE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## assumes that you have already defined a raster object called 'ice'
#' plot(latmask(ice, -60))
#' }
latmask <- function(x, latitude = 0, southern = TRUE) {
  if (raster::isLonLat(x))  {
    xy <- sp::coordinates(x)
  } else   {
    xy <- proj4::ptransform(sp::coordinates(x), raster::projection(x), proj_longlat())
  }
  if (southern) x[xy[,2] > (latitude * pi/180)] <- NA else x[xy[,2] < (latitude * pi/180)] <- NA
  x
}



