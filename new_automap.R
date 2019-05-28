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
    secant_range[2] <- min(c(secant_range[1], 90))
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
#' @param x a raster, stars, spatial sf, or numeric vector ('y' must also be present if 'x' is numeric, or NULL if x is a matrix)
#' @param target defaults to a projection family "stere", if set to NULL uses the projection of 'x'
new_automap <-
  function(x, y = NULL, centre_lon = NULL, centre_lat = NULL, target = "stere",
           expand = TRUE,
           dimXY = c(300, 300),
           bathy = TRUE, coast = TRUE, input_points = TRUE, input_lines = TRUE,
           graticule = TRUE, buffer = 0.05,
           contours = TRUE, levels = c(-500, -1000, -2000),
           trim_background = TRUE,
           mask = FALSE, ppch = 19, pcol = 2, pcex = 1, bathyleg = FALSE, llty = 1, llwd = 1, lcol = 1,
           gratlon = NULL, gratlat = NULL, gratpos="all",
           sample_type = sample(c("polar", "lonlat"), 1L), ...) {

    ## check args
    if ("family" %in% names(list(...))) warning("'family' argument is defunct, please use 'target'")

    llproj <- "+init=epsg:4326"

    # lonrange, latrange
    # lonvec, latvec
    # sp, sf object
    # raster, stars object
    # file

    ## mid_point in lon,lat
    ## target mesh

  tgt_raster <- NULL
  if (inherits(x, "BasicRaster")) {
    src_extent <- raster::extent(x)
    src_prj <- raster::projection(x)
    dimXY <- dim(x)[1:2]

    if (is.null(target)) {
      tgt_raster <- x
      tgt_prj <- raster::projection(x)
    }
  }

  if (inherits(x, "sf") || inherits(x, "Spatial")) {
    pex <- spex::spex(x)
    src_extent <- raster::extent(pex)
    src_prj <- raster::projection(x)
    if (is.null(target)) {
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
    if (!is.null(target) && target == "stere") tscale <- -71 * sign(centre_lat)


    src_extent <- raster::extent(range(llxy[,1]), range(llxy[,2]))
  }


if (!is.null(target) || (!is.null(centre_lon) || !is.null(centre_lat))) {
  tgt_prj <- family_proj(target, centre_lon, centre_lat, true_scale = tscale)
} else {
  tgt_prj <- src_prj
}


  if (is.null(tgt_raster)) {

browser()
      src_raster <- raster::raster(src_extent, nrows = dimXY[1], ncols = dimXY[2],
                                   crs = src_prj)
        tgt_raster <- raster::projectExtent(src_raster, tgt_prj)
        dim(tgt_raster)<- dimXY

    }

      #if (bathy) bathymetry <- raster::projectRaster(SOmap::Bathy, tgt_raster)
     if (bathy) bathymetry <- stars:::st_as_raster(stars::st_warp(stars::st_as_stars(SOmap::Bathy),
                                                                  stars::st_as_stars(tgt_raster)))
  ramp2 <- grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
  bluepal <- ramp2(45)

  return(list(bathymetry, dimXY))
structure(list(projection = raster::projection(target),
               bathy = bathymetry, bathyleg = bathyleg, bathy_palette = bluepal,
               coastline = list(data = coastline, fillcol = NA, linecol = "black"), target = target, ##data = xy,
               lines_data = if (input_lines) xy else NULL, points_data = if (input_points) xy else NULL,
               ppch = ppch, pcol = pcol, pcex = pcex,
               llty = llty, llwd = llwd, lcol = lcol,
               contours = contours, levels = levels, contour_colour = "black",
               graticule = graticule, crs = prj, gratpos=gratpos),
          class = "SOauto_map")
  }

# lonrange, latrange
# lonvec, latvec
# sp, sf object
# raster, stars object
# file
plot(new_automap(c(100, 120), c(-60, -30))[[1]])
plot(new_automap(c(100, 180), c(-60, -30))[[1]])
plot(new_automap(c(100, 180), c(-60, -30), target = NULL)[[1]])
plot(new_automap(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = "laea")[[1]])
plot(new_automap(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, centre_lon = 147)[[1]])

plot(new_automap(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = NULL)[[1]])

plot(new_automap(ice))
