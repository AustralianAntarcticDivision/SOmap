#' Southern Ocean Map
#'
#' @description
#' Function for creating round Southern Ocean maps.
#'
#' @param bathy_legend logical: if \code{TRUE}, insert the bathymetry legend.
#' @param border logical: if \code{TRUE}, insert longitude border.
#' @param trim numeric: latitude to trim the map to. Set this to -10 for effectively no trim.
#' @param graticules logical: if \code{TRUE}, insert graticule grid.
#' @param border_col character: colours for longitude border.
#' @param border_width numeric: thickness (in degrees of latitude) of the border.
#' @param graticules_col string: colour for graticule grid.
#' @param straight logical: if \code{TRUE}, leave a blank space on the side for a straight legend.
#' @param land logical: if \code{TRUE}, plot coastline.
#' @param land_col character: colour to use for coastline.
#' @param ice logical: if \code{TRUE}, plot ice features (ice shelves, glacier tongues, and similar).
#' @param ice_col character: colour to use for ice features.
#' @param fronts logical: if \code{TRUE}, plot ocean fronts (Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front).
#' @param fronts_col character: colours for fronts.
#'
#' @return An object of class "SOmap", which represents a polar-stereographic map of the southern hemisphere. Printing or plotting this object will cause it to be displayed in the current graphics device.
#'
#' @examples
#' \dontrun{
#'   tfile <- tempfile("SOmap", fileext = ".png")
#'   png(tfile, width = 22, height = 20, units = "cm", res = 600)
#'   SOmap(trim = -45, graticules = TRUE)
#'   dev.off()
#'   unlink(tfile)
#'   SOmap(trim = -45, graticules = TRUE)
#' }
#' @export
#'

SOmap <- function(bathy_legend = TRUE, border = TRUE, trim = -45, graticules = FALSE, straight = FALSE, land = TRUE, land_col = "black", ice = TRUE, ice_col = "black", fronts = FALSE, fronts_col = c("hotpink", "orchid", "plum"), border_col = c("white", "black"), border_width = 2, graticules_col = "grey70") {
    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    ## Set up color palette for bathy
    ramp2 <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF","grey99", "grey90", "grey92", "grey94", "grey96", "white"))
    bluepal <- ramp2(100)
    bluepal2 <- ramp2(80)
    ## fix trim without legend
    if (!border) border_width <- 0

    #bathy legend
    if (bathy_legend) {
        ## White Mask
        mask_graticule <- graticule::graticule(lons = seq(-180, 180, by = 1),lats = c(trim+border_width+11.5, trim+border_width), tiles = TRUE, proj = raster::projection(Bathy))

        ## Legend
        ## Colored legend
        bleg <- graticule::graticule(lons = seq(185, 265, by = 1),lats = c(trim+border_width+1, trim+border_width+3), tiles = TRUE, proj = raster::projection(Bathy))

        btick <- graticule::graticule(lats = c(trim+border_width+2, trim+border_width+5), lons = seq(190, 260, by = 11.666), proj = raster::projection(Bathy), tiles = FALSE)

        spud <- graticule::graticule(lons = seq(-180, 180, by = 1), lats = c(trim+border_width+8, trim+border_width+4.75), tiles = TRUE, proj = raster::projection(Bathy))
        df2 <- data.frame(a = c("-8000", "-6000", "-4000", "-2000", "0", "2000", "4000"),
                          lon = seq(190, 260, by = 11.666),
                          lat=rep(trim+border_width+7, 7))
        sp::coordinates(df2) <- c("lon", "lat")
        raster::projection(df2) <- "+init=epsg:4326"
        lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy)))
    }
    ## Graticule dots #
    xx <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
    yy <- c(seq(from = -90, to = trim-1, by = 15), trim)
    grat <- graticule::graticule(xx, yy, proj = raster::projection(Bathy))
    gratlab <- graticule::graticule_labels(lons = 180,lats = c(-45, -30, -60, -75), xline = 180, yline = -15, proj = raster::projection(Bathy))

    ## crop bathy raster depending on legend yes or no
    q <- ifelse(bathy_legend, trim+border_width+11, trim+border_width)
    Bathy <- raster::trim(SOmap::latmask(Bathy, latitude = q))
    out <- list(projection = raster::projection(Bathy), target = raster::raster(Bathy), straight = straight, trim = trim)
    out$bathy <- SO_plotter(plotfun = if (straight) "plot" else "image", plotargs = list(x = Bathy, col = bluepal, yaxt = "n", xaxt = "n", asp = 1))
    if (straight) out$bathy[[1]]$plotargs$legend <- FALSE

    out$box <- SO_plotter(plotfun = "graphics::box", plotargs = list(col = "white"))
    out$plot_sequence <- c("bathy", "box")
    buf <- make_buf(trim+border_width, proj = out$projection)
    if (land) {
      xland <-sf::st_as_sf(SOmap::SOmap_data$continent)
      xland <- sf::st_buffer(xland, 0)
      out$coastline <- SO_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xland)$geometry), col = NA, border = land_col, add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "coastline")
    }

    if (ice) {
      xice <- sf::st_buffer(SOmap::SOmap_data$ant_coast_ice, 0)
      out$ice <- SO_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xice)$geometry), col = NA, border = ice_col, add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "ice")
    }

    ## fronts
    if (fronts) {
      xfront <-sf::st_as_sf(SOmap::SOmap_data$fronts_orsi)
      out$fronts <- SO_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), col = fronts_col, add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "fronts")
    }

    ## Graticule grid
    if (graticules) {
        out$graticule <- c(SO_plotter(plotfun = "plot", plotargs = list(x = grat, col = graticules_col, lty = 3, add = TRUE), name = "main"),
                           SO_plotter(plotfun = "text", plotargs = list(x = gratlab, labels = parse(text = gratlab$lab), col = graticules_col, cex = 0.5), name = "labels"))
        out$plot_sequence <- c(out$plot_sequence, "graticule")
    }

    ## Legend
    if (bathy_legend) {
        out$outer_mask <- SO_plotter(plotfun = "plot", plotargs = list(x = mask_graticule, border = FALSE, col = "white", add = TRUE))
        out$bathy_legend <- c(SO_plotter(plotfun = "plot", plotargs = list(x = btick, col = "black", add = TRUE), name = "ticks"),
                              SO_plotter(plotfun = "plot", plotargs = list(x = bleg, lwd = 2, add = TRUE), name = "legend_outer"),
                              SO_plotter(plotfun = "plot", plotargs = list(x = bleg, border = FALSE, col = bluepal2, add = TRUE), name = "legend_fill"),
                              SO_plotter(plotfun = "plot", plotargs = list(x = spud, border = FALSE, col = "white", add = TRUE), name = "graticules"),
                              SO_plotter(plotfun = "text", plotargs = list(x = lab_pos2, labels = lab_pos2$a, cex = 0.75, adj = 0.5), name = "labels"))
        out$plot_sequence <- c(out$plot_sequence, "outer_mask", "bathy_legend")
    }
    if (border) {
        bord <- graticule::graticule(lons = seq(-180, 180, by = 15), lats = c(trim+border_width, trim), tiles = TRUE, proj = raster::projection(Bathy))
        out$border <- SO_plotter(plotfun = "plot", plotargs = list(x = bord, col = border_col, border = "black", add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "border")
    }
    structure(out, class = "SOmap")
}

#' @method plot SOmap
#' @export
plot.SOmap <- function (x, y, ...) {
    print(x)
    invisible()
}

#' @method print SOmap
#' @export
print.SOmap <- function(x, ...) {
    op <- par(mar = rep(0.01, 4),  mai= rep(0.0, 4))#oma= rep(0.0, 4),
    #on.exit(par(op))
    ## record current CRS
    SOcrs(x$projection)
    ## iterate through plot_sequence
    plot_all(x)
    invisible(x)
}
