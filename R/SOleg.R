
#' SOleg creating rounded legends for SOmap
#'
#' @param x
#' Object to obtain min and max values from for type='continuous' default=NULL
#' @param position
#' Where you want the legend ("topleft","topright", "bottomright")
#' @param col
#' Color pattern to use.
#' @param ticks
#' Number of ticks to include on the legend. Only used with type='continuous'
#' @param tlabs
#' Tick labels. Needed for type='discrete' optonal for type='continuous' if x is given
#' @param breaks
#' Numeric vector to create legend ticks for type='continuous' if x is given eg breaks<-c(1,2,3)
#' @param trim
#' Trim that was used to create the SOmap.
#' @param label
#' Legend label.
#' @param type
#' Type of legend to be plotted 'discrete' or 'continuous' default='discrete'
#' @param ladj
#' Distance to adjust the tick labels from the ticks. default = 0.5
#' @param lcex
#' Size of the tick labels. default = 0.75
#' @param lsrt
#' Angle of the tick labels. default = 0
#' @param tadj
#' Distance to adjust the title from the ticks. default = 0.5
#' @param tcex
#' Size of the title text. default=1
#' @param rnd
#' optional rounding factor for continuous legends using the round() function. default = NULL.
#' @param border_width numeric: thickness (in degrees of latitude) of the border
#'
#' @return
#' Creates rounded legends
#'
#' @export
#'
#' @examples
#' \dontrun{
#' SOmap()
#' #Discrete Legend
#'
#' SOleg(position="topleft",
#'       col=viridis::viridis(5),
#'       tlabs = c("a","b","c","d", "e"),
#'       trim=-45,
#'       label="Species")
#'
#' #Continuous Legend
#'
#' nums<-runif(100)
#'
#' brks<-c(0.1,0.2,0.5,0.9)
#'
#' SOleg(x=nums,
#'       position="topright",
#'       col=viridis::viridis(80),
#'       breaks=brks
#'       trim=-45,
#'       label="Species"
#'       rnd=1,
#'       type="continuous")

#' }

SOleg <-function(x = NULL,
                 position = "topright",
                 col = NULL,
                 ticks = NULL,
                 tlabs = NULL,
                 breaks = NULL,
                 trim = -45,
                 type = "discrete",
                 label = "",
                 ladj = 0.5,
                 lsrt = 0,
                 lcex = 0.75,
                 tadj = 0.5,
                 tcex = 1,
                 rnd = NULL,
                 border_width = 2) {

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
        if (!is.null(x) && is.null(tlabs) && inherits(x, "BasicRaster")) {
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
           "topleft" = {
               jklons <- seq(274, 356, by=1)
               bllons <- seq(275, 355, length.out = qbins+1)
               btlons <- seq(275+qtadjust, 355-qtadjust, length.out = qticks)
               lablon <- 315
               SRT <- 45
               strt <- 275
           },
           "bottomright" = {
               jklons <- seq(94, 176, by = 1)
               bllons <- seq(95, 175, length.out = qbins+1)
               btlons <- seq(95+qtadjust, 175-qtadjust, length.out = qticks)
               lablon <- 135
               SRT <- 45
               strt <- 95
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
    ## Graticule for masks

    k <- graticule::graticule(lons = jklons, lats = c(trim+border_width+8, trim+border_width+4.75), tiles = TRUE, proj = raster::projection(Bathy))
    j <- graticule::graticule(lons = jklons, lats = c(trim+15, trim+2), tiles = TRUE, proj = raster::projection(Bathy))

    ## Tick labels
    df2 <- data.frame(a = tlabs,lon = btlons, lat=rep(trim+9, length(tlabs))) ## Create dataframe with labels and locations.
    sp::coordinates(df2) <- c("lon", "lat") ## Assign the current coordinate type
    raster::projection(df2) <- "+init=epsg:4326" ## Assign the current projection type
    lab_pos2 <- sp::spTransform(df2, raster::crs(raster::projection(Bathy))) ## Reproject to the polar map coordinates.

    ## Legend label
    df3 <- data.frame(a = label,lon = lablon, lat = rep(trim+12.5))
    sp::coordinates(df3) <- c("lon", "lat")
    raster::projection(df3) <- "+init=epsg:4326"
    lab_pos3 <- sp::spTransform(df3, raster::crs(raster::projection(Bathy)))

    structure(list(
        mask = list(data = j, col = "white", border = FALSE),
        ticks = list(data = btick, col = "black"),
        legend = list(data = bleg, lwd = 2, border = FALSE, col = cols),
        mask2 = list(data = k, border = FALSE, col = "white"),
        tick_labels = list(data = lab_pos2, cex = lcex, adj = ladj, srt = lsrt),
        legend_labels = list(data = lab_pos3, cex = tcex, adj = tadj, srt = SRT)
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
    raster::plot(x$mask$data, border = x$mask$border, col = x$mask$col, add = TRUE) ## mask
    raster::plot(x$ticks$data, add = TRUE, col = x$ticks$col)
    raster::plot(x$legend$data, lwd = x$legend$lwd, add = TRUE)
    raster::plot(x$legend$data, border = x$legend$border,  col = x$legend$col, add = TRUE)
    raster::plot(x$mask2$data, border = x$mask2$border, col = x$mask2$col, add = TRUE)
    text(x$tick_labels$data, labels = x$tick_labels$data$a, cex =  x$tick_labels$cex, adj = x$tick_labels$adj, srt = x$tick_labels$srt)
    text(x$legend_labels$data, labels = x$legend_labels$data$a, cex =  x$legend_labels$cex, adj = x$legend_labels$adj, srt = x$legend_labels$srt)
    ## Need to set SRT during the position if statements.
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
#   ll_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
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
