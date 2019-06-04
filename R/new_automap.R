
family_proj <- function(family = NULL, clon = NULL, clat = NULL, true_scale = NULL,
                        secant_range = NULL) {
  if (is.null(family)) family <- "stere"
  if (is.null(clon)) clon <- 0
  if (is.null(clat)) clat <- 0
  ts <- ""
  secant <- ""
  if (family %in% c("aea", "lcc")) {
    if (is.null(secant_range)) secant_range <- clat + c(-10, 10)
    secant_range[1] <- max(c(secant_range[1], -90))
    secant_range[2] <- min(c(secant_range[2], 90))
    y0 <- secant_range[1]
    y1 <- secant_range[2]
    secant <- glue::glue(" +lat_1={y0} +lat_2={y1}")
  }
  ## need this leading space here
  if (!is.null(true_scale)) ts <- glue::glue(" +lat_ts={true_scale}")
  as.character(glue::glue("+proj={family} +lon_0={clon} +lat_0={clat}{ts}{secant} +datum=WGS84"))
}
mesh_points <- function(x) {
  sp::coordinates(raster::raster(raster::extent(x), nrows = 15, ncols = 15))
}

#' Internal SOmap
#'
#' @noRd
#' @keywords internal
#' @param sample_type create random input data from a 'polar' or 'lonlat' domain
automap_nothing <- function(sample_type = "polar") {
  stopifnot(sample_type %in% c("lonlat", "polar"))
    nsample <- runif(1, 15, 35)
    if (sample_type == "polar") {
      ## sample from Bathy

      Bathy <- NULL
      data("Bathy", package = "SOmap", envir = environment())

      rr <- raster(Bathy)
      raster::res(rr) <- c(runif(1, 16000, 1e6), runif(1, 16000, 1e6))
      xy <- rgdal::project(raster::xyFromCell(rr, sample(raster::ncell(rr), nsample)),
                           raster::projection(rr), inv = TRUE)
      xy <- xy[xy[,2] < -40, ]
      if (length(xy) == 2) xy <- jitter(rbind(xy, xy), amount = 10)
    }
    if (sample_type == "lonlat") {
      xlim <- sort(runif(2, -359, 359))
      ylim <- sort(runif(2, -89, -20))

      x <- runif(nsample, xlim[1], xlim[2])
      y <- runif(nsample, ylim[1], ylim[2])
      xy <- cbind(x, y)

    }
    xy <- xy[order(xy[, 1], xy[,2]), ]
    xy
}
crunch_bathy <- function(target_raster) {
  stars_to_raster(stars::st_warp(stars::st_as_stars(SOmap::Bathy),
                                                    stars::st_as_stars(target_raster)))

}
crunch_raster <- function(source_raster, target_raster) {
  stars_to_raster(stars::st_warp(stars::st_as_stars(source_raster),
                                      stars::st_as_stars(target_raster)))

}
#' @noRd
#' @param x a raster, stars, spatial sf, or numeric vector ('y' must also be present if 'x' is numeric, or NULL if x is a matrix)
#' @param target defaults to a projection family "stere", if set to NULL uses the projection of 'x'
automap_maker <-
  function(x, y = NULL, centre_lon = NULL, centre_lat = NULL, target = "stere",
           dimXY = c(300, 300), ...) {
    if (missing(x) && is.null(y)) {
      x <- automap_nothing(sample_type = "polar")
    }
    if (is.numeric(x) && !is.null(dim(x)) && is.null(y)) {
      x <- as.matrix(x)
      y <- x[,2, drop = TRUE]
      x <- x[,1, drop = TRUE]
    }
    ## check args
    if ("family" %in% names(list(...))) warning("'family' argument is defunct, please use 'target'")



    llproj <- "+init=epsg:4326"

    tgt_raster <- NULL
    tgt_prj <- NULL
    xy <- NULL
    llxy <- NULL
    if (!is.null(target) && substr(trimws(target, which = "left"), 1, 1) == "+") {
      ## whoa we have an actual PROJ.4 string
      if (!is.null(centre_lon) || !is.null(centre_lat)) {
        warning("'target' provided looks like a PROJ so centre_lon/centre_lat ignored")
        centre_lon <- centre_lat <- NULL
      }
      tgt_prj <- target
      target <- "notanyknownprojection"
    }

    if (inherits(x, "BasicRaster")) {

      src_extent <- raster::extent(x)
      src_prj <- raster::projection(x)
      dimXY <- dim(x)[1:2]

      ## if target family is the same as the projection we're using, then no auto-PROJ.4-string
      if (is.null(target) || grepl(target, src_prj)) {
        tgt_raster <- x
        tgt_prj <- raster::projection(x)

      }

      ## we gave it a grid, but also a target family
      if (!grepl("^\\+", target) && (length(c(centre_lon, centre_lat)) < 2)) {
        ## get the centre lon and lat from the input

        cpts <- spbabel::sptable(spex::spex(x))[-1, c("x_", "y_")]
        mp <- mid_point(reproj::reproj(as.matrix(cpts), target = 4326, source = raster::projection(x)))
        if (is.null(centre_lon)) centre_lon <- mp[1]
        if (is.null(centre_lat)) centre_lat <- mp[2]
      }
    }

    if (inherits(x, "sf") || inherits(x, "Spatial")) {
      pex <- spex::spex(x)
      src_extent <- raster::extent(pex)
      src_prj <- raster::projection(x)
      if (is.null(target)|| grepl(target, src_prj)) {
        tgt_raster <- raster::raster(pex)
        dim(tgt_raster) <- dimXY
        tgt_prj <- raster::projection(pex)
      }

    }
    tscale <- NULL;
    if (is.numeric(x)) {
      src_prj <- llproj
      llxy <- cbind(x, y)
      midp <- mid_point(llxy)
      if (is.null(centre_lon)) centre_lon <- midp[1]
      if (is.null(centre_lat)) centre_lat <- midp[2]
      if (!is.null(target) && target == "stere" && !grepl(target, src_prj)) tscale <- -71 * sign(centre_lat)


      src_extent <- raster::extent(range(llxy[,1], na.rm = TRUE), range(llxy[,2], na.rm = TRUE))

    }

    if (is.null(tgt_prj) && (!is.null(target) || (!is.null(centre_lon) || !is.null(centre_lat)))) {
      tgt_prj <- family_proj(target, centre_lon, centre_lat, true_scale = tscale)
    }

    if (is.null(tgt_raster)) {
      src_raster <- raster::raster(src_extent, nrows = dimXY[1], ncols = dimXY[2],
                                   crs = src_prj)
      tgt_raster <- raster::projectExtent(src_raster, tgt_prj)
      dim(tgt_raster)<- dimXY

    }
    if (!is.null(llxy)) xy <- reproj::reproj(llxy, tgt_prj, source = llproj)[, 1:2, drop = FALSE]
    bathymetry <- crunch_bathy(tgt_raster)
    #bathymetry <- raster::trim(bathymetry)
    SOcrs(raster::projection(bathymetry))

    return(list(target = bathymetry, xy = xy))
  }
