#' Default Southern Ocean map
#'
#' Provide minimal input information to get a default map. The simplest case is
#' to run the function without any inputs at all and it will use random location data.
#'
#' To input your data, use input locations as `x` (longitude) and `y` (latitude) values. There must be at least two locations.
#'
#' Try `target` families such as 'lcc', 'laea', 'gnom', 'merc', 'aea' if feeling adventurous.
#'
#' @param x optional input data longitudes
#' @param y optional input data latitudes
#' @param centre_lon optional centre longitude (of the map projection, also used to for plot range if `expand = TRUE`)
#' @param centre_lat as per `centre_lon`
#' @param target optional projection family (default is `stere`ographic), or full PROJ string (see Details)
#' @param dimXY dimensions of background bathmetry (if used) default is 300x300
#' @param bathy logical: if \code{TRUE}, plot bathymetry. Alternatively, provide the bathymetry data to use as a \code{raster} object
#' @param land logical: if \code{TRUE}, plot coastline. Alternatively, provide the coastline data to use as a \code{Spatial} object
#' @param land_col character: colour to use for plotting the coastline
#' @param ice logical: if \code{TRUE}, plot ice features (ice shelves, glacier tongues, and similar)
#' @param ice_col character: colour to use for ice features
#' @param input_points add points to plot (of x, y)
#' @param input_lines add lines to plot   (of x, y)
#' @param graticule flag to add a basic graticule
#' @param expand fraction to expand plot range (default is 0.05, set to zero for no buffer, may be negative)
#' @param contours logical: add contours?
#' @param levels numeric: contour levels to use if \code{contours} is \code{TRUE}
#' @param ppch set point character (default=19)
#' @param pcol set point color (default=19)
#' @param pcex set point cex (default=1)
#' @param llty set line type
#' @param llwd set line width
#' @param lcol set line color
#' @param bathyleg optional bathymetry legend (default=FALSE)
#' @param gratlon longitude values for graticule meridians
#' @param gratlat latitude values for graticule parallels
#' @param gratpos positions (sides) of graticule labels
#' @return An object of class SOmap_auto, containing the data and other details required to generate the map. Printing or plotting the object will cause it to be plotted.
#' @param ... reserved, checked for defunct and deprecated usage
#' @export
#' @examples
#' \dontrun{
#'   SOmap_auto(c(0, 50), c(-70, -50))
#'   SOmap_auto(runif(10, 130, 200), runif(10, -80, -10))
#'   SOplot(c(147, 180), c(-42, -60), pch = 19, cex = 2,col = "firebrick")
#'   SOmap_auto(runif(10, 130, 200), runif(10, -85, -60))
#'
#'   ## save the result to explore later!
#'   protomap <- SOmap_auto(runif(10, 60, 160), runif(10, -73, -50))
#'
#'   SOmap_auto(runif(50, 40, 180), runif(50, -73, -10), family = "laea", centre_lat = -15,
#'                 input_lines = FALSE)
#' }
SOmap_auto <- function(x, y, centre_lon = NULL, centre_lat = NULL, target = "stere",
                       dimXY = c(300, 300),
                       bathy = TRUE, land = TRUE, land_col = "black", ice = TRUE, ice_col = "black",
                       input_points = TRUE, input_lines = TRUE,
                       graticule = TRUE, expand = 0.05,
                       contours = FALSE, levels = c(-500, -1000, -2000),
                       ppch = 19, pcol = 2, pcex = 1, bathyleg = FALSE, llty = 1, llwd = 1, lcol = 1,
                       gratlon = NULL, gratlat = NULL, gratpos="all", ...) {
    ## check inputs
    assert_that(is.flag(contours), !is.na(contours))
    assert_that(is.numeric(levels), length(levels) > 0)
    assert_that(is.numeric(expand), msg = "'expand' must be numeric, changed behaviour in SOmap > 0.2.1")

    dots <- list(...)
    if ("mask" %in% names(dots)) warning("'mask' argument to SOmap() is defunct")
    if ("trim_background" %in% names(dots)) warning("'trim_background' argument to SOmap() is defunct")


    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())
    if (missing(y)) y <- NULL
    ## automap_nothing ----
    if (missing(x) && is.null(y)) {
        ## generate some random data
        xy <- automap_nothing()
        x <- xy[,1]
        y <- xy[,2]
    }
    ## END automap_nothing ----

    ## automap_maker ----
    amap <- automap_maker(x, y = y, centre_lon = centre_lon, centre_lat = centre_lat, target = target, dimXY = dimXY)
    xy <- amap$xy
    target <- amap$target
    prj <- raster::projection(target)
    ## END automap_maker

    if (abs(expand) > 0) {
      xl <- spex::xlim(target)
      yl <- spex::ylim(target)

      xl <- xl + c(-1, 1) * expand * diff(xl)
      yl <- yl + c(-1, 1) * expand * diff(yl)
      raster::extent(target) <- raster::extent(xl, yl)
      dim(target) <- dimXY
      target <- crunch_bathy(target)

    }

    ## we have target, and we have performed expansion so xlim and ylim are ready
    xlim <- spex::xlim(target)
    ylim <- spex::ylim(target)


    bathymetry <- coastline <- NULL
    if (isTRUE(bathy)) {            ## insert your local bathy-getter here
        bathymetry <- target
    } else {
        if (inherits(bathy, "BasicRaster")) {
            bathymetry <- crunch_raster(bathy, target)
            bathy <- TRUE

        }
    }

    mycrop <- function(thing) {
        try({
            if (!inherits(thing, c("sf", "sfc"))) thing <- sf::st_as_sf(thing)
            as(sf::st_crop(sf::st_buffer(sf::st_transform(thing, prj), 0), xmin = raster::xmin(target), xmax = raster::xmax(target), ymin = raster::ymin(target), ymax = raster::ymax(target)), "Spatial")
        }, silent = TRUE)
    }

    if (isTRUE(land)) {
        suppressWarnings({
            coastline <- mycrop(SOmap_data$continent)
            if (inherits(coastline, "try-error")) {
                land <- FALSE
                warning("no coastline within region, cannot be plotted")
                coastline <- NULL
            }
        })
    } else {
        if (inherits(land, "Spatial")) {
            coastline <- sp::spTransform(land, prj)
            land <- TRUE
        }
    }

    if (isTRUE(ice)) {
        suppressWarnings({
            icedat <- mycrop(SOmap_data$ant_coast_ice)
            if (inherits(icedat, "try-error")) {
                ice <- FALSE
                warning("no ice data within region, cannot be plotted")
                icedat <- NULL
            }
        })
    }
    
    grat <- sf::st_graticule(c(raster::xmin(target), raster::ymin(target), raster::xmax(target), raster::ymax(target)),
                             crs = raster::projection(target), lon = gratlon, lat = gratlat)
    if (graticule) {
        graticule <- grat
    } else {
        graticule <- NULL
    }

    bluepal <- grDevices::colorRampPalette(c("#54A3D1", "#60B3EB", "#78C8F0", "#98D1F5", "#B5DCFF", "#BDE1F0", "#CDEBFA",
                                             "#D6EFFF", "#EBFAFF", "grey92", "grey94", "grey96", "white"))(45)
    ## need to construct breaks for plotting, so that colours always line up with the right values
    depthmin <- if (inherits(bathymetry, "BasicRaster")) raster::cellStats(bathymetry, "min", na.rm = TRUE) else -10353 ## fallback to this
    depthmax <- if (inherits(bathymetry, "BasicRaster")) raster::cellStats(bathymetry, "max", na.rm = TRUE) else 6050 ## fallback to this
    bathy_breaks <- c(seq(from = depthmin, to = 0, length.out = 33), seq(from = 0, to = depthmax, length.out = 14)[-1]) ## one more break than colour
    bathy_break_labels <- round(bathy_breaks)
    if (!exists("xy")) xy <- NULL
    ## new SOmap_auto object format
    out <- list(projection = raster::projection(target), target = target, plot_sequence = c("init"))
    out$init <- SO_plotter(plotfun = function(target, main = NULL) {
        base_plt <- c(0.1571257, 0.9195210, 0.1847547, 0.8514717)
        plt <- c(base_plt[c(1, 3)]/2, 1-(1-base_plt[c(2, 4)])/2)[c(1, 3, 2, 4)]
        aspect <- if (raster::isLonLat(target)) 1/cos(mean(c(raster::xmin(target), raster::xmax(target))) * pi/180) else 1
        if (!is.null(main)) {
            plt[4] <- min(plt[4], 0.90) ## room at top
        }
        aspectplot.default(c(raster::xmin(target), raster::xmax(target)), c(raster::ymin(target), raster::ymax(target)), asp = aspect, plt = plt)
        par(xpd = NA)
    }, plotargs = list(target = target)) ## TODO figure out how to pass main here
    add_bathy_legend_to_plotseq <- FALSE
    if (!is.null(bathymetry)) {
        ## check breaks and colours: need one more break than number of colours
        ##if (length(x$bathy_breaks) != (length(x$bathy_palette) + 1)) x$bathy_breaks <- NULL
        
        if (isTRUE(bathyleg)) {
            if (FALSE) {
                ## old code using raster::plot and relying on its own legend handling
                ## ##out$bathy <- SO_plotter(plotfun = "raster::plot", plotargs = list(x = bathymetry, add = TRUE, col = bluepal, breaks = bathy_breaks, axes = FALSE))
            } else if (FALSE) {
                ## attempt #2 to fix legend handling: use aspectplot.default and raster::image, and control the legend ourselves
                out$bathy <- SO_plotter(plotfun = "raster::image", plotargs = list(x = bathymetry, add = TRUE, col = bluepal, axes = FALSE))
                asp <- if (raster::isLonLat(target)) 1/cos(mean(c(raster::xmin(target), raster::xmax(target))) * pi/180) else 1
                r <- abs(asp * diff(c(raster::ymin(target), raster::ymax(target)))/diff(c(raster::xmin(target), raster::xmax(target))))
                ##out$bathy_legend <- SO_plotter(plotfun = "raster::plot", plotargs = list(x = bathymetry, legend.only = TRUE, col = bluepal, legend.width = 1, legend.shrink = 0.5, axis.args = list(at = bathy_breaks, labels = bathy_break_labels, cex.axis = 0.6), legend.args = list(text = "", side = if (r >= 1) 3 else 1, font = 2, line = 2.5, cex = 0.8), horizontal = r < 1))
                ## alternatively we can exert a bit more control over where the legend goes using 'smallplot'
                if (r < 1) {
                    ## legend at the bottom
                    legpos <- c(0.2, 0.8, 0.0, 0.05)
                    horiz <- TRUE
                } else {
                    legpos <- c(0.95, 1.0, 0.2, 0.8)
                    horiz <- FALSE
                }
                out$bathy_legend <- SO_plotter(plotfun = "raster::plot", plotargs = list(x = bathymetry, smallplot = legpos, col = bluepal, breaks = bathy_breaks, legend.only = TRUE, axes = FALSE, horizontal = horiz, axis.args = list(at = bathy_breaks, labels = bathy_break_labels, cex.axis = 0.6)))
                ## note also that we add bathy_legend to the very end of the plot_sequence (below) so that it doesn't muck up the plot aspect ratio
                add_bathy_legend_to_plotseq <- TRUE
            } else {
                ## rely on raster to set the plot up
                asp <- if (raster::isLonLat(target)) 1/cos(mean(c(raster::xmin(target), raster::xmax(target))) * pi/180) else 1
                r <- abs(asp * diff(c(raster::ymin(target), raster::ymax(target)))/diff(c(raster::xmin(target), raster::xmax(target))))
                out$bathy <- SO_plotter(plotfun = "raster::plot", plotargs = list(x = bathymetry, add = FALSE, box = FALSE, col = bluepal, axis.args = list(at = bathy_breaks, labels = bathy_break_labels), axes = FALSE, legend = TRUE, horizontal = r < 1))
                ## note that extra args here get passed down the stack to .rasterImagePlot, which takes:
                ## .rasterImagePlot <- function(x, col, add=FALSE, legend=TRUE, horizontal = FALSE, 
                ##    legend.shrink=0.5, legend.width=0.6, legend.mar = ifelse(horizontal, 3.1, 5.1),
                ##    legend.lab=NULL, graphics.reset=FALSE, bigplot = NULL, smallplot = NULL, legend.only = FALSE, 
                ##    lab.breaks=NULL, axis.args=NULL, legend.args = NULL, interpolate=FALSE, box=TRUE, breaks=NULL, 
                ##    zlim=NULL, zlimcol=NULL, fun=NULL, asp, colNA = NA, alpha=NULL, npretty=0, ...) {
                out$plot_sequence <- c() ## don't set up with aspectplot.default, just let raster take care of it
            }
        } else {
            if (!is.null(bathy_breaks)) {
                out$bathy <- SO_plotter(plotfun = "raster::image", plotargs = list(x = bathymetry, add = TRUE, col = bluepal, breaks = bathy_breaks, axes = FALSE))
            } else {
                ## image can't cope with NULL breaks?
                out$bathy <- SO_plotter(plotfun = "raster::image", plotargs = list(x = bathymetry, add = TRUE, col = bluepal, axes = FALSE))
            }
        }
        out$plot_sequence <- c(out$plot_sequence, "bathy")
    }
    if (contours && !is.null(bathymetry)) {
        out$contours <- SO_plotter(plotfun = "contour", plotargs = list(x = bathymetry, levels = levels, col = "black", add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "contours")
    }
    if (land) {
        out$coastline <- SO_plotter(plotfun = "plot", plotargs = list(x = coastline, col = NA, border = land_col, add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "coastline")
    }
    if (ice) {
        out$ice <- SO_plotter(plotfun = "plot", plotargs = list(x = icedat, col = NA, border = ice_col, add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "ice")
    }
    if (input_points) {
        out$points <- SO_plotter(plotfun = "points", plotargs = list(x = xy, pch = ppch, cex = pcex, col = pcol))
        out$plot_sequence <- c(out$plot_sequence, "points")
    }
    if (input_lines) {
        out$lines <- SO_plotter(plotfun = "lines", plotargs = list(x = xy, lty = llty, lwd = llwd, col = lcol))
        out$plot_sequence <- c(out$plot_sequence, "lines")
    }
    if (!is.null(graticule)) {
        out$graticule <- SO_plotter(plotfun = "SOmap:::plot_graticule", plotargs = list(x = graticule, GratPos = gratpos))
        out$plot_sequence <- c(out$plot_sequence, "graticule")
    }
    if (add_bathy_legend_to_plotseq) out$plot_sequence <- c(out$plot_sequence, "bathy_legend")
    out$crs <- prj
    structure(out, class = "SOmap_auto")

    ## old SOmap_auto object format
    ##structure(list(projection = raster::projection(target),
    ##               bathy = bathymetry, bathyleg = bathyleg, bathy_palette = bluepal,
    ##               bathy_breaks = bathy_breaks,
    ##               coastline = if (land) list(data = coastline, fillcol = NA, linecol = land_col) else NULL,
    ##               ice = if (ice) list(data = icedat, fillcol = NA, linecol = ice_col) else NULL,
    ##               target = target, ##data = xy,
    ##               lines_data = if (input_lines) xy else NULL, points_data = if (input_points) xy else NULL,
    ##               ppch = ppch, pcol = pcol, pcex = pcex,
    ##               llty = llty, llwd = llwd, lcol = lcol,
    ##               contours = contours, levels = levels, contour_colour = "black",
    ##               graticule = graticule, crs = prj, gratpos=gratpos),
    ##          class = "SOmap_auto")
}


#' @method plot SOmap_auto
#' @export
plot.SOmap_auto <- function (x, y, ...) {
    print(x, ...)
    invisible()
}

## #' @method print SOmap_auto
## #' @export
##print.SOmap_auto <- function(x,main=NULL, ..., set_clip = TRUE) {
##  base_mar <- c(5.1, 4.1, 4.1, 2.1)
##  aspect <- if (raster::isLonLat(x$target)) 1/cos(mean(c(raster::xmin(x$target), raster::xmax(x$target))) * pi/180) else 1
##  if (is.null(main)) {
##      margins <-base_mar/2.5
##  } else {
##      mars <- base_mar/2.5
##      mars[3] <- mars[3]+2
##      margins <- mars
##  }
##  pp <- aspectplot.default(c(raster::xmin(x$target), raster::xmax(x$target)), c(raster::ymin(x$target), raster::ymax(x$target)), asp = aspect, mar =margins)
##  ## reset par(pp) when we exit this function
##                                        #on.exit(par(pp))
##  ## record current crs
##  SOcrs(x$projection)
##
##  if(!is.null(main)){graphics::title(main = main)}
##  op <- par(xpd = FALSE)
##  if (!is.null(x$bathy)) {
##      ## check breaks and colours: need one more break than number of colours
##      ## if the user has overridden the palette but not the breaks, this may cause an error
##      if (length(x$bathy_breaks) != (length(x$bathy_palette) + 1)) x$bathy_breaks <- NULL
##      if (isTRUE(x$bathyleg)) {
##          raster::plot(x$bathy, add = TRUE, col = x$bathy_palette, breaks = x$bathy_breaks, axes = FALSE)
##      } else {
##          if (!is.null(x$bathy_breaks)) {
##              raster::image(x$bathy, add = TRUE, col = x$bathy_palette, breaks = x$bathy_breaks, axes = FALSE)
##          } else {
##              ## image can't cope with NULL breaks?
##              raster::image(x$bathy, add = TRUE, col = x$bathy_palette, axes = FALSE)
##          }
##      }
##  }
##  ## suggested param change: if levels is a scalar than pass it to nlevels
##  ## nlevels = 1
##  if (x$contours && !is.null(x$bathy)) contour(x$bathy, levels = x$levels, col = x$contour_colour, add = TRUE)
##
##  if (!is.null(x$coastline)) {
##      if (!is.null(x$coastline$data)) plot(x$coastline$data, col = x$coastline$fillcol, border = x$coastline$linecol, add = TRUE)
##  }
##
##  if (!is.null(x$ice)) {
##      if (!is.null(x$ice$data)) plot(x$ice$data, col = x$ice$fillcol, border = x$ice$linecol, add = TRUE)
##  }
##
##  if (!is.null(x$points_data)) points(x$points_data, pch = x$ppch, cex = x$pcex, col = x$pcol)
##  if (!is.null(x$lines_data)) lines(x$lines_data, lty = x$llty, lwd = x$llwd, col = x$lcol)
##
##  if (!is.null(x$graticule)) {
##      plot_graticule(x$graticule, GratPos=x$gratpos)
##  }
##  par(op)
##  if (set_clip) graphics::clip(raster::xmin(x$target), raster::xmax(x$target),
##                               raster::ymin(x$target), raster::ymax(x$target))
##
##  invisible(x)
##}

#' @method print SOmap_auto
#' @export
print.SOmap_auto <- function(x, main = NULL, ..., set_clip = TRUE) {
    toplot <- intersect(x$plot_sequence, names(x))
    ## must do init and bathy first
    opar <- par(no.readonly = TRUE)
    for (thing in intersect(toplot, c("init", "bathy"))) {
        ## iterate through all the components, which will be SO_plotter objects
        for (thispf in x[[thing]]) {
            thisfun <- thispf$plotfun
            this_plotargs <- thispf$plotargs
            if (thing == "init" && !is.null(main)) this_plotargs$main <- main
            if (is.character(thisfun)) do.call(eval(parse(text = thisfun)), this_plotargs) else do.call(thisfun, this_plotargs)
        }
    }
    toplot <- setdiff(toplot, c("init", "bathy"))
    ## record current crs
    SOcrs(x$projection)
    if (!is.null(main)) graphics::title(main = main)
    ## now plot everything else
    if (length(toplot) > 0) {
        temp <- x[toplot]
        temp$plot_sequence <- toplot
        plot_all(structure(temp, class = "SOmap_auto"))
    }
    if (set_clip) graphics::clip(raster::xmin(x$target), raster::xmax(x$target), raster::ymin(x$target), raster::ymax(x$target))
    ##par(opar)
    invisible(x)
}

## from ?sf::st_graticule
plot_graticule <- function(x, GratPos) {
    gratopts <- c("all", "left", "right", "top", "bottom")
    if (!GratPos[1] %in% gratopts) stop("gratpos must be one of: all, left, right, top, bottom.")
                                        #plot(sf::st_geometry(x), add = TRUE, col = 'grey', reset = FALSE)
    plot(sf::as_Spatial(x), add = TRUE, col = "grey")
                                        # points(x$x_start, x$y_start, col = 'red')
                                        #points(x$x_end, x$y_end, col = 'blue')
    op <- par(xpd = NA)
    invisible(lapply(seq_len(nrow(x)), function(i) {
        if (GratPos=="all" || "left" %in% GratPos){
            if (x$type[i] == "N" && x$x_start[i] - min(x$x_start) < 1000) {
                text(x[i,"x_start"], x[i,"y_start"], labels = parse(text = x[i,"degree_label"]),
                     srt = x$angle_start[i], pos = 2, cex = .7)}} #left

        if (GratPos=="all" || "bottom" %in% GratPos) {
            if (x$type[i] == "E" && x$y_start[i] - min(x$y_start) < 1000) {
                text(x[i,"x_start"], x[i,"y_start"], labels = parse(text = x[i,"degree_label"]),
                     srt = x$angle_start[i] - 90, pos = 1, cex = .7)}} #bottom

        if (GratPos=="all" || "right" %in% GratPos ) {
            if (x$type[i] == "N" && x$x_end[i] - max(x$x_end) > -1000) {
                text(x[i,"x_end"], x[i,"y_end"], labels = parse(text = x[i,"degree_label"]),
                     srt = x$angle_end[i], pos = 4, cex = .7)}} #right

        if (GratPos=="all" || "top" %in% GratPos ) {
            if (x$type[i] == "E" && x$y_end[i] - max(x$y_end) > -1000) {
                text(x[i,"x_end"], x[i,"y_end"], labels = parse(text = x[i,"degree_label"]),
                     srt = x$angle_end[i] - 90, pos = 3, cex = .7)}} #top
    }))
    par(op)
    invisible(NULL)
}


aspectplot.default <- function(xlim, ylim, asp, plt, ...) {
    plot.new()
    xlim <- sort(xlim)
    ylim <- sort(ylim)
    r <- abs(asp * abs(diff(ylim)/diff(xlim)))
    dv <- dev.size("px")
    dr <- dv[1]/dv[2]

    rd <- r * dr
    bm <- 0

    if (rd <= 1) {  # X = 0, 1
        recip <- (r * dr)/2
        viewport <- c(0, 1, 0.5 - recip, 0.5 + recip)
    } else {     # Y = 0, 1
        recip <- ((1/dr) * (1/r) )/2
        viewport <- c(0.5 - recip, 0.5 + recip, 0, 1)
    }
    if (viewport[1] < 0) viewport[1] <- 0
    if (viewport[2] > 1) viewport[2] <- 1
    if (viewport[3] < 0) viewport[3] <- 0
    if (viewport[4] > 1) viewport[4] <- 1
    p <- par(plt = viewport, fig = plt, new = FALSE, ...)
    plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", asp = asp)
    p
}

## old, kept here temporarily for easy reference
##aspectplot.default <- function(xlim, ylim, asp, ...) {
##  plot.new()
##  xlim <- sort(xlim)
##  ylim <- sort(ylim)
##  r <- abs(asp * abs(diff(ylim)/diff(xlim)))
##cat("r: ", r, "\n")  
##  if(r <= 1) {  # X = 0, 1
##    recip <- r / 2
##    figure <- c(0, 1, 0.5 - recip, 0.5 + recip)
##  } else {     # Y = 0, 1
##    recip <- (1/r) / 2
##    figure <- c(0.5 - recip, 0.5 + recip, 0, 1)
##  }
##  p <- par(fig = figure, new = FALSE, ...)
##  plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", asp = asp)
##  box(col = "red")
##  p
##}

fast_mask <- function(ras, poly) {
  cells <- tabularaster::cellnumbers(ras, sf::st_as_sf(poly))
  ras[setdiff(1:ncell(ras), cells$cell_)] <- NA
  ras
}

#' Defunct function
#'
#' Removed from SOmap
#' @param ... all arguments passed to new function
#'
#' @export
#' @name SOmap-defunct
default_somap <- function(...) {
  .Defunct("SOmap_auto")
}

#' @export
#' @name SOmap-defunct
SOauto_map <- function(...) {
  .Defunct("SOmap_auto")
}

