#' Southern Ocean Map
#'
#' @description
#' Function for creating round Southern Ocean maps.
#'
#' @param bathy_legend logical: if \code{TRUE}, insert the bathymetry legend. If \code{bathy_legend = NULL} or \code{bathy_legend = "space"}, then space will be left for the legend but no legend will actually be plotted. Use this if you plan to add a legend later.
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
#' @param fronts logical or string: if \code{TRUE} or "Orsi", plot Orsi et al., (1995) ocean fronts: Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front. If "Park" plot the Park & Durand (2019) fronts; Northern boundary, Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front and Southern Boundary.
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
    ## wrap in `quietly` to suppress unwanted warnings
    quietly(SOmap_inner(bathy_legend = bathy_legend, border = border, trim = trim, graticules =  graticules, straight = straight, land = land, land_col = land_col, ice = ice, ice_col = ice_col, fronts = fronts, fronts_col = fronts_col, border_col = border_col, border_width = border_width, graticules_col = graticules_col))

}

SOmap_inner <- function(bathy_legend, border, trim, graticules, straight, land, land_col, ice, ice_col, fronts, fronts_col, border_col, border_width, graticules_col) {

    if (is.character(fronts)) {
        fronts <- match.arg(tolower(fronts), c("park", "orsi"))
    } else {
        if (!(is.flag(fronts) && !is.na(fronts)))
            stop("'fronts' should be TRUE, FALSE, 'Park' (to use Park & Durand 2019 fronts) or 'Orsi' (to use Orsi et al. 1995 fronts)")
    }
    ## deal with bathy legend options
    if (is.null(bathy_legend)) bathy_legend <- "space"
    if (is.character(bathy_legend)) {
        bathy_legend <- match.arg(tolower(bathy_legend), c("space"))
        bathy_legend_space <- TRUE
        plot_bathy_legend <- FALSE
    } else if (is.logical(bathy_legend) && !is.na(bathy_legend)) {
        bathy_legend_space <- plot_bathy_legend <- bathy_legend
    } else {
        stop("bathy_legend should be TRUE, FALSE, NULL, or \"space\"")
    }

    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    ## Set up color palette for bathy
    ramp2 <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA", "#D6EFFF", "#EBFAFF","grey99", "grey90", "grey92", "grey94", "grey96", "white"))
    bluepal <- ramp2(100)
    bluepal2 <- ramp2(80)
    if (!border) border_width <- 0

    ## Graticule dots
    xx <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
    yy <- c(seq(from = -90, to = trim-1, by = 15), trim)
    grat <- suppressWarnings(graticule::graticule(xx, yy, proj = raster::projection(Bathy)))
    gratlab <- suppressWarnings(graticule::graticule_labels(lons = 180,lats = c(-45, -30, -60, -75), xline = 180, yline = -15, proj = raster::projection(Bathy)))

    ## crop bathy raster depending on whether we are leaving space for a legend or not
    q <- ifelse(bathy_legend_space, trim+border_width+11, trim+border_width)
    Bathy <- raster::trim(SOmap::latmask(Bathy, latitude = q))
    out <- list(projection = raster::projection(Bathy), target = raster::raster(Bathy), straight = straight, trim = trim)
    out$bathy <- SO_plotter(plotfun = if (straight) "plot" else "image", plotargs = list(x = Bathy, col = bluepal, yaxt = "n", xaxt = "n", asp = 1, zlim = c(-8000, 4000)))
    if (straight) out$bathy[[1]]$plotargs$legend <- FALSE

    out$box <- SO_plotter(plotfun = "graphics::box", plotargs = list(col = "white"))
    out$plot_sequence <- c("bathy", "box")
    buf <- make_buf(trim+border_width, proj = out$projection)
    if (land) {
      xland <-sf::st_as_sf(SOmap::SOmap_data$continent)
      xland <- sf::st_buffer(xland, 0)
      xland <- sf::st_set_crs(xland, sf::st_crs(buf))
      out$coastline <- SO_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xland)$geometry), col = NA, border = land_col, add = TRUE))

      out$plot_sequence <- c(out$plot_sequence, "coastline")
    }

    if (ice) {
      xice <- sf::st_buffer(SOmap::SOmap_data$ant_coast_ice, 0)
      ## make sure the crs are equal because god forbid we wouldn't want to be making things useable ...
      xice <- sf::st_set_crs(xice, sf::st_crs(buf))
      out$ice <- SO_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xice)$geometry), col = NA, border = ice_col, add = TRUE))

      out$plot_sequence <- c(out$plot_sequence, "ice")
    }

    ## fronts
    if (isTRUE(fronts) || fronts == "orsi") {
      xfront <-sf::st_as_sf(SOmap::SOmap_data$fronts_orsi)
      xfront <- sf::st_set_crs(xfront, sf::st_crs(buf))
      out$fronts <- SO_plotter(plotfun = "plot", plotargs = list(x = suppressWarnings(sf::st_intersection(buf, xfront)$geometry), col = fronts_col, add = TRUE))
      out$plot_sequence <- c(out$plot_sequence, "fronts")
    } else if (fronts == "park") {
      xfront <-suppressWarnings(SOproj(SOmap::SOmap_data$fronts_park, target = out$projection))
      #xfront <- sf::st_set_crs(xfront, sf::st_crs(buf))
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
    if (bathy_legend_space) {
        ## white mask over the part of the bathy raster extending beyond the plot border
        mask_graticule <- graticule::graticule(lons = seq(-180, 180, by = 1), lats = c(trim+border_width+11.5, trim+border_width), tiles = TRUE, proj = raster::projection(Bathy))
        out$outer_mask <- SO_plotter(plotfun = "plot", plotargs = list(x = mask_graticule, border = FALSE, col = "white", add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "outer_mask")
    }
    if (plot_bathy_legend) {
        ## construct the actual bathy legend
        solegx <- SOleg(position = "bottomleft", type = "continuous", breaks = c(-8000, -6000, -4000, -2000, 0, 2000, 4000), border_width = border_width, col = bluepal2, trim = trim)
        ## take the bits we need here and add them
##        out$bathy_legend <- list(ticks = solegx$ticks[[1]],
##                                 legend_outer = solegx$legend[[1]],
##                                 legend_fill = solegx$legend[[2]],
##                                 ##graticules = solegx$mask2[[1]],
##                                 labels = solegx$tick_labels[[1]])
        out$bathy_legend <- list(solegx) ## embed the legend object
        out$plot_sequence <- c(out$plot_sequence, "bathy_legend")
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
