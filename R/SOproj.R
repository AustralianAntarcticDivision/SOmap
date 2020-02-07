
#' Southern projection
#'
#' @description
#' Function for reprojecting data.
#'
#' @param x
#' longitude vector, or object with coordinates
#'
#' @param y
#' latitude vector
#'
#' @param source
#' starting projection (default = longlat)
#'
#' @param target
#' target projection (default = stereo)
#'
#' @param ... arguments passed to [reproj::reproj() function](reproj::reproj())
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
  if (is.null(target)) {
    suppressWarnings(target <- SOcrs())
    if (is.null(target)) {
      message("No CRS provided or available, assuming SOmap default")
      target <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }
  }

  ## shortcut out, we have an object
  if (is.null(y) && !missing(x)) {
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
#' @param x coordinates
#' @param source source specification (PROJ.4 string or epsg code)
#' @param target target specification (PROJ.4 string or epsg code)
#' @param ... arguments passed to the underlying projection engine, see [reproj::reproj()]
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
    do_SOmap_reproj(x = x, target = target, source = source)
}
#' @export
#' @name reproj
reproj.SOmap_auto <- function(x, target, ..., source = NULL) {
    if (missing(target)) stop("'target' projection string required")
    do_SOmap_reproj(x = x, target = target, source = source)
}

#' @export
#' @name reproj
reproj.SOmap_management <- function(x, target, ..., source = NULL) {
    if (missing(target)) stop("'target' projection string required")
    do_SOmap_reproj(x = x, target = target, source = source)
}

#' @export
#' @name reproj
reproj.SOmap_legend <- function(x, target, ..., source = NULL) {
    if (missing(target)) stop("'target' projection string required")
    do_SOmap_reproj(x = x, target = target, source = source)
}

## can use the same code for SOmap, SOmap_auto, and SOmap_management objects
## note that SOmap_management won't have a bathy component
do_SOmap_reproj <- function(x, target, source = NULL) {
    if (missing(target)) stop("'target' projection string required")
    if (!is.null(source)) warning("source ignored, should be NULL for SOmap objects")
    if (!is.null(x$bathy)) {
        rast <- try(reproj(x$bathy[[1]]$plotargs$x, target = target), silent = TRUE)
        if (inherits(rast, "try-error")) {
            stop("unable to reproject raster sensibly")
        }
        x$bathy[[1]]$plotargs$x <- rast
        x$target <- raster::raster(rast)
    }
    for (thing in setdiff(names(x), c("init", "plot_sequence", "projection", "target", "straight", "trim", "box", "crs", "lines", "points"))) {
        x[[thing]] <- reproj_SO_plotter_list(x[[thing]], target)
    }
    for (thing in intersect(names(x), c("lines", "points"))) {
        x[[thing]] <- reproj_SO_plotter_list(x[[thing]], target, source = x$projection)
    }
    x$projection <- target
    x
}

## each plottable element in an SOmap object should be a list of SO_plotter objects
## doesn't make sense to export this as a public reproj method, because we don't expect users to be
##  reprojecting SO_plotter objects themselves
reproj_SO_plotter_list <- function(thing, target, source = NULL) {
    if (inherits(thing, "SO_plotter")) {
        ## old code may have had just a single SO_plotter object, not a list of length 1
        thing <- reproj_SO_plotter(thing, target = target, source = source)
    } else if (is.list(thing)) {
        thing <- lapply(thing, reproj_SO_plotter, target = target, source = source)
    } else {
        stop("unexpected plotter object format")
    }
}
reproj_SO_plotter <- function(x, target, source) {
    if (inherits(x, "SOmap_legend")) {
        x <- reproj(x, target)
    } else {
        if (!is.null(x$plotargs$x)) x$plotargs$x <- reproj(x$plotargs$x, target, source = source)
    }
    x
}

#' @name reproj
#' @export
reproj.BasicRaster <- function(x, target, ..., source = NULL) {
  targ <- raster::projectExtent(x, target)
  if (raster::nlayers(x) == 3) {
    out <- raster::projectRaster(x, targ, method = "ngb")

  } else {
   out <-  raster::projectRaster(x, targ)
  }
  out
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
