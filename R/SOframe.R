## WIP

SOframe <- function(lon, lat) {
  assertthat::assert_that(length(lon) == 2L)
  assertthat::assert_that(length(lat) == 2L)
  suppressWarnings({
   out <- spex::spex(raster::extent(as.matrix(expand.grid(lon, lat))))
  })
 #raster::projection(out) <- "+proj=longlat +datum=WGS84"
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

  raster::mask(x, SOproj(mask, target = raster::projection(x)))
}

