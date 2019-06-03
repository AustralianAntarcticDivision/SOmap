
#' Southern projection
#'
#' @description
#' Function for reprojecting data.
#'
#' @param x
#' longitude vector, or object with coordinates
#'
#' @param y
#' lattitude vector
#'
#' @param source
#' starting projection (default = longlat)
#'
#' @param target
#' target projection (default = stereo)
#'
#' @param ... arguments passed to [reproj::reproj()]
#' @param data
#' optional data to be included
#'
#' @return
#' Reprojects the given data object to polar projection. Works with Points, spatial, raster, SOmap, sf and sfc objects.
#'
#' @examples
#' \dontrun{
#'  x <- c(-70, -60,-50, -90)
#'  y <- c(-50, -75, -45, -60)
#'  pnts <- SOproj(x = y, y = x)
#'  SOmap2(CCAMLR = TRUE)
#'  plot(pnts, pch = 19, col = 3, add = TRUE)
#' }
#' @export
#' @importFrom reproj reproj
#' @importFrom raster projection<-
#' @importFrom sp coordinates<-
SOproj <- function(x, y = NULL, target = NULL, data, ..., source = NULL){
 if (is.character(y)) stop(sprintf("'y' is character, did you mean? \n\n  SOproj(%s, target = %s)",
                                   as.character(substitute(x)),
                                   as.character(substitute(y))))
  ## shortcut out, we have an object
  if (is.null(y) && !missing(x)) {
    if (is.null(target)) target <- SOcrs()
    source <- raster::projection(x)
    if (is.na(source)) {
      warning("assuming generic data is in longitude,latitude")
      source <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }
    #browser()
    return(reproj(x, target = target, source = source))

  }
  if (missing(x) || is.null(y)) {
      stop("x and y must be provided unless 'x' is an object")
  }
#  should never be needed
#   if (is.na(projection(x)) && is.null(source)) {
#     if (is.list(x) && !is.null(x$crs)) {
#       source <- x$crs
#     } else {
#      stop("no projection metadata on 'x'")
#     }
#   }
  if (is.null(target)) {
    target <-  SOcrs()
    if (is.null(target)) {
      message("No CRS provided or available, assuming SOmap default")
      target <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }
  }

  if (missing(data)) data <- 1

  if (is.numeric(x) && is.numeric(y)) {
    if ((missing(source) || is.null(source) || !nzchar(source))) {
      message("No projection provided, assuming longlat")
      source <- "+proj=longlat +datum=WGS84"
    }
    #browser()
    xy0 <- reproj::reproj(cbind(x, y), target = target, source = source)
    out <- data.frame(x = xy0[,1], y = xy0[,2], data = data)
    sp::coordinates(out) <- c("x", "y")
    raster::projection(out) <- target
  } else {
    stop("x and or y arguments malformed, should both be numeric vectors")
  }
  #  should never be needed
  #else {
  #out <- reproj(x, target = target)
  #}
  out
}

projection.SOmap <- function(x, asText = TRUE) {
 raster::projection(x$projection, asText = asText)
}
projection.SOmap_auto <- function(x, asText = TRUE) {
  raster::projection(x$projection, asText = asText)
}
#' Reproject SOmap
#'
#' Reproject a SOmap object by specifying a 'target' projection string (PROJ4)
#'
#' See [reproj::reproj()] for details.
#'
#' @section Warning:
#' So many ...
#' @seealso [reproj::reproj()]
#' @inheritParams reproj::reproj
#' @export
#' @export reproj
#' @aliases reproj reproj.SOmap_auto
#' @importFrom reproj reproj
#' @importFrom raster projectRaster raster
#' @examples
#' \dontrun{
#'   set.seed(27)
#'   amap <- SOmap_auto()
#'   reproj(amap, "+proj=moll")
#'   reproj(amap, "+proj=laea +lat_0=-55 +lon_0=154 +datum=WGS84")
#'
#'   bmap <- SOmap(trim = -35)
#'
#'   ## works great!
#'   reproj(bmap, "+proj=stere +lat_0=-90 +lon_0=147 +lat_ts=-71 +datum=WGS84")
#'
#'   ## these aren't exactly ideal
#'   reproj(bmap, "+proj=ortho +lat_0=-70")
#'   reproj(bmap, "+proj=laea +lat_0=-55 +lon_0=154 +datum=WGS84")
#' }
#' @name reproj
reproj.SOmap <- function(x, target, ..., source = NULL) {
  if (missing(target)) stop("'target' projection string required")
  if (!is.null(source)) warning("source ignored, should be NULL for SOmap objects")
  if (!is.null(x$bathy)) {
   rast <- try(reproj(x$bathy$plotargs$x, target = target), silent = TRUE)
    if (inherits(rast, "try-error")) {
     stop("unable to reproject raster sensibly")
   }
   x$bathy$plotargs$x <- rast
   x$target <- raster::raster(rast)
  }
  x$coastline$plotargs$x <- reproj(x$coastline$plotargs$x, target)
  x$projection <- target
  x
}
#' @export
#' @name reproj
reproj.SOmap_auto <- function(x, target, ..., source = NULL) {
  if (missing(target)) stop("'target' projection string required")
  if (!is.null(source)) warning("source ignored, should be NULL for SOmap objects")
  if (!is.null(x$bathy)) {
    rast <- try(reproj(x$bathy, target = target), silent = TRUE)
   if (inherits(rast, "try-error")) {
     stop("unable to reproject raster sensibly")
   }
   x$bathy <- rast
   x$target <- raster::raster(rast)
  }
  if (!is.null(x$coastline$data)) x$coastline$data <- reproj(x$coastline$data, target)
  if (!is.null(x$graticule)) x$graticule <- reproj(x$graticule, target)

  if (!is.null(x$lines_data)) x$lines_data <- reproj(x$lines_data, target, source = x$projection)
  if (!is.null(x$points_data)) x$points_data <- reproj(x$points_data, target, source = x$projection)

  x$projection <- target
  x
}
#' @name reproj
#' @export
reproj.BasicRaster <- function(x, target, ..., source = NULL) {
  targ <- raster::projectExtent(x, target)
  raster::projectRaster(x, targ)
}
#' @name reproj
#' @export
reproj.Spatial <- function(x, target, ..., source = NULL) {
  sp::spTransform(x, target)
}
#' @name reproj
#' @export
reproj.sf <- function(x, target, ..., source = NULL) {
  sf::st_transform(x, target)
}
#' @name reproj
#' @export
reproj.sfc <- function(x, target, ..., source = NULL) {
  sf::st_transform(x, target)
}
