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



# accept inputs like

#* nothing  TODO
#* lonrange, latrange
#* lonvec, latvec
#* sp, sf object
#* raster, stars object

#  then build *a template raster with projection and extent and dimensions*

# 1. *-* Create automap_maker() to build template raster from various inputs
# 2. Create automap_nothing()  for automap_maker() to use when x/y both NULL
# 3. Replace inner logic of SOauto_map() with automap_maker()
# 4. Profit.

## WIP copy logic from SOauto_map for no inputs to here
automap_nothing <- function(centre_lon = NULL, centre_lat = NULL, target = "stere",
                            sample_type = sample(c("polar", "lonlat"), 1L)) {

}
#' @param x a raster, stars, spatial sf, or numeric vector ('y' must also be present if 'x' is numeric, or NULL if x is a matrix)
#' @param target defaults to a projection family "stere", if set to NULL uses the projection of 'x'
automap_maker <-
  function(x, y = NULL, centre_lon = NULL, centre_lat = NULL, target = "stere",
           expand = TRUE,
           dimXY = c(300, 300)) {

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
    tgt_prj <- NULL
    if (inherits(x, "BasicRaster")) {
      src_extent <- raster::extent(x)
      src_prj <- raster::projection(x)
      dimXY <- dim(x)[1:2]

      ## if target family is the same as the projection we're using, then no auto-PROJ.4-string
      if (is.null(target) || grepl(target, src_prj)) {
        tgt_raster <- x
        tgt_prj <- raster::projection(x)

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


      src_extent <- raster::extent(range(llxy[,1]), range(llxy[,2]))
    }

    if (is.null(tgt_prj) && (!is.null(target) || (!is.null(centre_lon) || !is.null(centre_lat)))) {
      tgt_prj <- family_proj(target, centre_lon, centre_lat, true_scale = tscale)
    } else {
      tgt_prj <- src_prj
    }
    if (is.null(tgt_raster)) {
      src_raster <- raster::raster(src_extent, nrows = dimXY[1], ncols = dimXY[2],
                                   crs = src_prj)
      tgt_raster <- raster::projectExtent(src_raster, tgt_prj)
      dim(tgt_raster)<- dimXY

    }
    bathymetry <- stars:::st_as_raster(stars::st_warp(stars::st_as_stars(SOmap::Bathy),
                                                                 stars::st_as_stars(tgt_raster)))
    SOcrs(projection(bathymetry))

    return(list(bathymetry))
  }
