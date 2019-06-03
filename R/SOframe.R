## WIP

SOframe <- function(lon, lat) {
  assertthat::assert_that(length(lon) == 2L)
  assertthat::assert_that(length(lat) == 2L)
  suppressWarnings({
   out <- spex::spex(raster::extent(as.matrix(expand.grid(lon, lat))))
  })
  raster::projection(out) <- "+proj=longlat +datum=WGS84"
out
}

SOmask <- function(x, mask, ...) {
  UseMethod("SOmask")
}

SOmask.BasicRaster <- function(x, mask, ...)  {
  assertthat::assert_that(class(mask) == "SpatialPolygonsDataFrame")
  ## find width
  ## find height
  ## determine delta x, y
  ## segmentize the mask
  lon <- spex::xlim(mask)  ## assume longlat
  lat <- spex::ylim(mask)

  dy <- sp::spDistsN1(cbind(lon[1], lat[1]), cbind(lon[1], lat[2]), longlat = TRUE) / 111111.12
  dx <- sp::spDistsN1(cbind(lon[1], lat[2]), cbind(lon[2], lat[2]), longlat = TRUE) / 111111.12

  dx <- max(c(dy, dx)) * 1000 / 24
  raster::projection(mask) <- "+proj=longlat +datum=WGS84"
  mask <- sf::st_set_crs(sf::st_segmentize(sf::st_set_crs(sf::st_as_sf(mask), NA), dx), sf::st_crs(mask))

  raster::mask(x, SOproj(mask, target = raster::projection(x)))
}

