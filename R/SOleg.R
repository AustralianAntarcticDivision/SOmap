#' Rounded legends for SOmap
#'
#' @param x numeric: object to obtain min and max values from for \code{type = "continuous"}.
#' @param position string: where you want the legend ("topleft", "topright", "bottomleft", or "bottomright").
#' @param col character: colours to use.
#' @param ticks numeric: number of ticks to include on the legend. Only used with \code{type = "continuous"}.
#' @param tlabs character: tick labels. Required for \code{type = "discrete"}, optional for \code{type = "continuous"} if \code{x} is given.
#' @param breaks numeric: vector of tick positions for \code{type = "continuous"} when \code{x} is given.
#' @param trim numeric: \code{trim} value that was used to create the SOmap object (see \code{\link{SOmap}}).
#' @param label string: legend label.
#' @param type string: type of legend ("discrete" or "continuous").
#' @param ladj numeric: distance to adjust the tick labels from the ticks.
#' @param lcex numeric: size of the tick labels.
#' @param lsrt numeric: angle of the tick labels.
#' @param tadj numeric: distance to adjust the title from the ticks.
#' @param tcex numeric: size of the title text.
#' @param rnd numeric: optional rounding factor for continuous legends using the \code{link{round}} function.
#' @param border_width numeric: thickness (in degrees of latitude) of the border.
#'
#' @return An object of class "SOmap_legend". Printing or plotting this object will cause it to be added to the SOmap in the current graphics device.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   SOmap()
#'
#'   ## Discrete Legend
#'   SOleg(position = "topleft", col = hcl.colors(5, "Viridis"),
#'         tlabs = c("a", "b", "c", "d", "e"), trim = -45, label = "Species")
#'
#'   ## Continuous Legend
#'   SOleg(x = runif(100), position = "topright", col = hcl.colors(80, "Viridis"),
#'         breaks = c(0.1, 0.2, 0.5, 0.9), trim = -45, label = "Species",
#'         rnd = 1, type = "continuous")
#' }

SOleg <- function(x = NULL, position = "topright", col = NULL, ticks = NULL, tlabs = NULL, breaks = NULL,
                  trim = -45, type = "discrete", label = "", ladj = 0.5, lsrt = 0, lcex = 0.75,
                  tadj = 0.5, tcex = 1, rnd = NULL, border_width = 2) {
    ## wrap in `quietly` to suppress unwanted warnings
    quietly(SOleg_inner(x = x, position = position, col = col, ticks = ticks, tlabs = tlabs, breaks = breaks,
                        trim = trim, type = type, label = label, ladj = ladj, lsrt = lsrt, lcex = lcex,
                        tadj = tadj, tcex = tcex, rnd = rnd, border_width = border_width))
}
SOleg_inner <-function(x, position, col, ticks, tlabs, breaks, trim, type, label, ladj, lsrt, lcex, tadj, tcex, rnd, border_width) {

    if (is.null(col)) col <- c("#440154FF", "#3E4A89FF", "#26828EFF", "#35B779FF", "#B4DE2CFF")

    ## data
    Bathy <- NULL
    data("Bathy", package = "SOmap", envir = environment())

    if (type == "continuous" && !is.null(ticks) && !is.null(breaks) && length(breaks) != ticks ) {
        stop("Number of ticks and breaks do not match. You do not need to use ticks if you have breaks")
    }

    if (type == "continuous" && !is.null(breaks)) {
        if (!inherits(breaks, c("numeric", "integer"))) {
            stop("breaks must be numeric or integer")
        }
    }

    if (type == "continuous" && is.null(ticks) && !is.null(breaks)) {
        ticks <- length(breaks)
    }

    if (type == "continuous" && is.null(ticks) && is.null(breaks)) {
        stop("Number of ticks needs to be set for continuous legends via the ticks parameter")
    }

    if (type == "continuous" && !is.null(ticks) && !is.null(tlabs) && length(tlabs) != ticks) {
        stop("Number of ticks and labels do not match")
    }

    if (type == "continuous" && is.factor(x) || is.character(x)) {
        stop("Discrete variable given to continuous legend. Try type='discrete'")
    }

    if (type == "discrete" && !is.discrete(col)) {
        stop("Continuous colors given for discrete variable")
    }

    if (type == "discrete") {
        qbins <- length(tlabs)
        qtadjust <- (80/length(tlabs))/2
    } ## how far in to move the tick marks each end
    qticks <-length(tlabs) ##(80-qtadjust)/(length(tlabs)-1)} #how far between ticks. Currently deprecated.
    cols <- col

    if (type == "continuous") {
        qbins <- 80
        qticks <- ticks
        qtadjust <- 0

        if (is.discrete(cols)) {
            ramp <- grDevices::colorRampPalette(col)
            cols <- ramp(80)
        } else {
            cols <- col(80)
        }
        if (!is.null(x) && is.null(tlabs) && !inherits(x, "BasicRaster")) {
            lmins <- min(x)
            lmax <- max(x)
            lbs <- seq(from = lmins, to = lmax, length.out = ticks)
            if (!is.null(rnd)) {
                lbs<-base::round(lbs, digits = rnd)
            }
            tlabs <- as.character(lbs)
        }
        if (!is.null(x) && is.null(tlabs) && inherits(x, c("BasicRaster", "RasterLayer"))) {
            lmins <- raster::cellStats(x, stat = "min", na.rm = TRUE)
            lmax <- raster::cellStats(x, stat = "max", na.rm = TRUE)
            lbs <- seq(from = lmins, to = lmax, length.out = ticks)
            if (!is.null(rnd)) {
                lbs<-base::round(lbs, digits = rnd)
            }
            tlabs <- as.character(lbs)
        }
        if (is.null(x) && is.null(tlabs) && !is.null(breaks)) {
            lmins <- min(breaks)
            lmax <- max(breaks)
            lbs <- breaks
            ##if (!is.null(rnd)) lbs<-base::round(lbs, digits = rnd)
            tlabs <- as.character(lbs)
        }
    }

    switch(position,
           "topright" = {
               jklons <- seq(4, 86, by = 1)
               bllons <- seq(5, 85, length.out = qbins+1)
               btlons <- seq(5+qtadjust, 85-qtadjust, length.out = qticks)
               lablon <- 45
               SRT <- -45
               strt <-  5
           },
           "bottomright" = {
               jklons <- seq(94, 176, by = 1)
               bllons <- seq(95, 175, length.out = qbins+1)
               btlons <- seq(95+qtadjust, 175-qtadjust, length.out = qticks)
               lablon <- 135
               SRT <- 45
               strt <- 95
           },
           "bottomleft" = {
               jklons <- seq(184, 266, by = 1)
               bllons <- seq(185, 265, length.out = qbins+1)
               btlons <- seq(185+qtadjust, 265-qtadjust, length.out = qticks)
               lablon <- 225
               SRT <- -45
               strt <- 185
           },
           "topleft" = {
               jklons <- seq(274, 356, by=1)
               bllons <- seq(275, 355, length.out = qbins+1)
               btlons <- seq(275+qtadjust, 355-qtadjust, length.out = qticks)
               lablon <- 315
               SRT <- 45
               strt <- 275
           }
           )

    if (type == "continuous" && !is.null(breaks)) {
        nms <- (breaks-lmins)/(lmax-lmins)
        btlons <- round(nms*80, 2) + strt
        tlabs <- as.character(breaks)
    }

    ## Graticule for colors
    bleg  <- graticule::graticule(lons = bllons,lats = c(trim+border_width+1, trim+border_width+3), tiles = TRUE, proj = raster::projection(Bathy))
    ## Graticule for ticks
    btick <- graticule::graticule(lons = btlons ,lats = c(trim+border_width+2, trim+border_width+5),  proj = raster::projection(Bathy), tiles = FALSE)
    just_ticks <- do.call(rbind, lapply(seq_along(btlons), function(z) {
        graticule::graticule(lons = btlons[z], lats = c(trim+border_width+3, trim+border_width+5), proj = raster::projection(Bathy), tiles = FALSE, nverts = 2)[1, ]
    }))
    ## NB nverts seems to have no effect here

    ## Graticule for masks
    k <- graticule::graticule(lons = jklons, lats = c(trim+border_width+8, trim+border_width+4.75), tiles = TRUE, proj = raster::projection(Bathy))
    j <- graticule::graticule(lons = jklons, lats = c(trim+15, trim+2), tiles = TRUE, proj = raster::projection(Bathy))

    ## Tick labels
    df2 <- data.frame(a = tlabs,lon = btlons, lat=rep(trim+9, length(tlabs))) ## Create dataframe with labels and locations.
    sp::coordinates(df2) <- c("lon", "lat") ## Assign the current coordinate type
    raster::projection(df2) <- proj_longlat() ## Assign the current projection type
    lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy))) ## Reproject to the polar map coordinates.

    ## Legend label
    df3 <- data.frame(a = label,lon = lablon, lat = rep(trim+12.5))
    sp::coordinates(df3) <- c("lon", "lat")
    raster::projection(df3) <- proj_longlat()
    lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))

    structure(list(
        projection = raster::projection(Bathy),
        plot_sequence = c("mask", "ticks", "legend", ##"mask2",
                          "tick_labels", "legend_labels"),
        mask = SO_plotter(plotfun = "raster::plot", plotargs = list(x = j, col = "white", border = FALSE, add = TRUE)),
        ##ticks = SO_plotter(plotfun = "raster::plot", plotargs = list(x = btick, col = "black", add = TRUE)),
        ticks = SO_plotter(plotfun = "plot", plotargs = list(x = just_ticks, col = "black", add = TRUE)),
        legend = c(SO_plotter(plotfun = "raster::plot", plotargs = list(x = bleg, lwd = 2, add = TRUE)),
                   SO_plotter(plotfun = "raster::plot", plotargs = list(x = bleg, border = FALSE, col = cols, add = TRUE))),
        ##mask2 = SO_plotter(plotfun = "raster::plot", plotargs = list(x = k, border = FALSE, col = "white", add = TRUE)),
        tick_labels = SO_plotter(plotfun = "text", plotargs = list(x = lab_pos2, labels = lab_pos2$a, cex = lcex, adj = ladj, srt = lsrt)),
        legend_labels = SO_plotter(plotfun = "text", plotargs = list(x = lab_pos3, labels = lab_pos3$a, cex = tcex, adj = tadj, srt = SRT))
    ), class = "SOmap_legend")
}

#' @method plot SOmap_legend
#' @export
plot.SOmap_legend <- function (x, y, ...) {
    print(x)
    invisible()
}

#' @method print SOmap_legend
#' @export
print.SOmap_legend <- function(x, ...) {
    plot_all(x)
    invisible(x)
}


#Notes for further development.
#If we use raster::plot(x$ticks$data[x$ticks$data$id %in% c(1:(length(x$ticks$data$id)-1))], add = TRUE, col = x$ticks$col) we should be able to remove the mask2 option.

#We should rewrite this using the curvy graticule function that Mike wrote which works better for low counts in discrete legends.

# curvy_g <- function(lons, lats, proj = NULL, incr = 1) {
#   if (is.null(proj)) stop("'proj' must be supplied")
#   r <- raster::raster(raster::extent(range(lons), range(lats)),
#                       ncols = length(lons) - 1, nrows = length(lats) - 1)
#   r[] <- 0
#   ll_crs <- "+proj=longlat +datum=WGS84 +no_defs"
#   out <- sf::as_Spatial(sf::st_segmentize( spex::polygonize(r), dfMaxLength = incr))
#   sp::proj4string(out) <- sp::CRS(ll_crs)
#   sp::spTransform(out, proj)
# }
#
# prj <- projection(Bathy)
# g <- curvy_g(lons = c(95, 135, 175),
#              lats = c(-42, -39),
#              proj = prj
# )
#plot(g, col=rainbow(10))
