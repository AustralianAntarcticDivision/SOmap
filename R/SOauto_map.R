mid_point <- function (p, fold = FALSE)
  {
  gc <- "+proj=geocent +datum=WGS84"
  lc <- "+proj=longlat +datum=WGS84"
   reproj::reproj(matrix(colMeans(reproj::reproj(p, target = gc, source  = lc), na.rm = TRUE), 1L),target = lc, source = gc)[1L, 1:2, drop = FALSE]

  }


#' Default Southern Ocean map
#'
#' Provide minimal input information to get a default map. The simplest case is
#' to run the function without any inputs at all and it will provide a random default.
#'
#' To input your data, use input locations as `x` (longitude) and `y` (latitude) values, there must be at least two locations.
#'
#' Try families such as 'lcc', 'laea', 'gnom', 'merc', 'aea' if feeling adventurous.
#'
#' Using `mask = TRUE` does not work well when the pole is included, so it's `FALSE` by default.
#'
#' @param x optional input data longitudes
#' @param y optional input data latitudes
#' @param centre_lon optional centre longitude (of the map projection, also used to for plot range if `expand = TRUE`)
#' @param centre_lat as per `centre_lon`
#' @param family optional projection family (default is `stere`ographic), or full PROJ string (see Details)
#' @param expand re-compute range of plot to incorporate centre_lon and centre_lat with the data as a natural middle
#' @param dimXY dimensions of background bathmetry (if used) default is 300x300
#' @param bathy logical: if \code{TRUE}, plot bathymetry. Alternatively, provide the bathymetry data to use as a \code{raster} object
#' @param coast logical: if \code{TRUE}, plot coastline. Alternatively, provide the coastline data to use as a \code{Spatial} object
#' @param input_points add points to plot (of x, y)
#' @param input_lines add lines to plot   (of x, y)
#' @param graticule flag to add a basic graticule
#' @param buffer fraction to expand plot range from that calculated (either from data, or from centre_lon/centre_lat _and_ data if `expand = TRUE`)
#' @param contours logical: add contours?
#' @param levels numeric: contour levels to use if \code{contours} is \code{TRUE}
#' @param trim_background crop the resulting bathymetry to its margin of valid values
#' @param mask logical: if \code{TRUE}, mask the raster and coastline to the graticule
#' @param ppch set point character (default=19)
#' @param pcol set point color (default=19)
#' @param pcex set point cex (default=1)
#' @param llty set line type
#' @param llwd set line width
#' @param lcol set line color
#' @param bathyleg optional bathymetry legend (default=FALSE). Note when \code{bathyleg} is \code{FALSE}, plotting is done with \code{raster::image}, but when \code{bathyleg} is \code{TRUE} plotting uses \code{raster::plot}
#' @param gratlon longitude values for graticule meridians
#' @param gratlat latitude values for graticule parallels
#' @param sample_type create random input data from a 'polar' or 'lonlat' domain
#' @return An object of class SOauto_map, containing the data and other details required to generate the map. Printing or plotting the object will cause it to be plotted.
#' @export
#' @examples
#' SOauto_map(c(0, 50), c(-70, -50))
#' SOauto_map(runif(10, 130, 200), runif(10, -80, -10))
#' SOplot(c(147, 180), c(-42, -60), pch = 19, cex = 2,col = "firebrick")
#' SOauto_map(runif(10, 130, 200), runif(10, -85, -60))
#' ## save the result to explore later!
#' protomap <- SOauto_map(runif(10, 60, 160), runif(10, -73, -50))
#'
#' SOauto_map(runif(50, 40, 180), runif(50, -73, -10), family = "aea", centre_lat = -15,
#'               input_lines = FALSE)
SOauto_map <- function(x, y, centre_lon = NULL, centre_lat = NULL, family = "stere",
                       expand = TRUE,
                       dimXY = c(300, 300),
                       bathy = TRUE, coast = TRUE, input_points = TRUE, input_lines = TRUE,
                       graticule = TRUE, buffer = 0.05,
                       contours = TRUE, levels = c(-500, -1000, -2000),
                       trim_background = TRUE,
                       mask = FALSE, ppch = 19, pcol = 2, pcex = 1, bathyleg = FALSE, llty = 1, llwd = 1, lcol = 1,
                       gratlon = NULL, gratlat = NULL,
                       sample_type = sample(c("polar", "lonlat"), 1L)) {
    ## check inputs
    assert_that(is.flag(contours), !is.na(contours))
    assert_that(is.numeric(levels), length(levels) > 0)

    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    if (missing(x) && missing(y)) {
      stopifnot(sample_type %in% c("lonlat", "polar"))
      nsample <- runif(1, 15, 35)
      if (sample_type == "polar") {
       ## sample from Bathy
       rr <- raster(Bathy)
       raster::res(rr) <- c(runif(1, 16000, 1e6), runif(1, 16000, 1e6))
       xy <- rgdal::project(raster::xyFromCell(rr, sample(raster::ncell(rr), nsample)), raster::projection(rr), inv = TRUE)
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
        x <- xy[,1]
        y <- xy[,2]
    }

    if (is.numeric(x) && is.numeric(y)) {
        testx <- cbind(x, y)
    } else {
        testx <- x  ## assume we have some kind of object
    }
    ## ignore y
    if (is.matrix(testx)) {
        if (nrow(testx) ==2 ) {
            testx <- rbind(testx, testx)  ## because raster::extent(cbind(145:146, -42:-43))
        }
        if (!raster::couldBeLonLat(testx, warnings = FALSE)) {
            warning("'x' doesn't look like longlat data")
        }
    } else {
        do_midpoint <- FALSE

        if (inherits(x, "SpatialPoints")) {
          input_lines <- FALSE
        }
        if (inherits(x, "SpatialLines") || inherits(x, "SpatialPolygons")) {
          input_points <- FALSE
        }

        ## we have some kind of object
        if (inherits(testx, "BasicRaster")) {
            warning("input 'x' is a raster, converting to an extent for a simple plot of input_points/input_lines")
            x <- spex::spex(testx)
            do_midpoint <- TRUE
        }
        testx <- try(spbabel::sptable(x))  ##


        if (inherits(testx, "try-error")) stop("don't understand how to get lon,lat from 'x'")
        ## split on branch

        testx <- head(do.call(rbind, lapply(split(testx, paste(testx$object_, testx$branch_, sep = ":")), function(x) rbind(x, NA))), -1)
        testx <- as.matrix(testx[c("x_", "y_")])
        if (!raster::isLonLat(raster::projection(x))) {
            testx <- rgdal::project(testx, raster::projection(x), inv = TRUE)
            midpoint <- NULL
            if (do_midpoint) {
                midpoint <- cbind(mean(range(testx[,1L])), mean(range(testx[,2L])))
                midpoint <- rgdal::project(midpoint, raster::projection(x), inv = TRUE)
            }
            ## add the midpoint for good measure
            testx <- rbind(testx,midpoint)
        }
        x <- testx[,1]
        y <- testx[,2]
    }

    stopifnot(length(x) > 1)
    stopifnot(length(y) > 1)


    xlim <- range(x, na.rm = TRUE)
    ylim <- range(y, na.rm = TRUE)
    if (ylim[1] < -90) {ylim[1] <- -90}
    if (ylim[2] > 90) {ylim[2] <- 90}

    if (grepl("\\+proj", family)) {
      ## ignore the above and take the string as given
      prj <- family
      if (!is.null(centre_lon) || !is.null(centre_lat)) {
        warning("centre_lon and centre_lat are ignored if 'family' is a full PROJ string")
      }

    } else {
      mp <- mid_point(p=cbind(x, y))
    if (is.null(centre_lon)) {
        #centre_lon <- zapsmall(round(mean(xlim), digits = 2))
        centre_lon <- mp[1]
    }
    if (is.null(centre_lat)) {
        #centre_lat <-  zapsmall(round(mean(ylim), digits = 2))
      centre_lat <- mp[2]
    }


    template <- "+proj=%s +lon_0=%f +lat_0=%f +datum=WGS84"
    if (family == "stere") {
        ## won't generalize to northern hemisphere
        template <- "+proj=%s +lon_0=%f +lat_0=%f +lat_ts=-71 +datum=WGS84"
    }
    if (family %in% c("aea", "lcc")) {
        template <- paste("+proj=%s +lon_0=%f +lat_0=%f +datum=WGS84", sprintf("+lat_0=%f +lat_1=%f", ylim[1], ylim[2]))
    }
    prj <- sprintf(template, family, centre_lon, centre_lat)

    }

    target <- raster::projectExtent(raster::raster(raster::extent(xlim, ylim), crs = "+init=epsg:4326"), prj)
    if (trim_background) {
      #browser()
      target <- crop(target, extent(rgdal::project(cbind(x, y), prj)))
    }
    dim(target) <- dimXY
    ## extend projected bounds by the buffer
    xxlim <- c(raster::xmin(target), raster::xmax(target))
    xxlim <- xxlim + diff(range(xxlim)) * c(-buffer, buffer)
    yylim <- c(raster::ymin(target), raster::ymax(target))
    yylim <- yylim + diff(range(yylim)) * c(-buffer, buffer)
    middle <- rgdal::project(cbind(mean(xxlim), mean(yylim)), raster::projection(target),
                             inv = TRUE)
    if (is.null(centre_lon)) centre_lon <- middle[1L]
    if (is.null(centre_lat)) centre_lat <- middle[2L]
    target <- extend(target, extent(xxlim, yylim))
    if (expand) {

        centre_line <- rgdal::project(cbind(centre_lon, centre_lat), prj)
        ## we need the largest of the difference from centre to target boundary
        xhalf <- max(abs(centre_line[1] - c(raster::xmin(target), raster::xmax(target))))
        yhalf <- max(abs(centre_line[2] - c(raster::ymin(target), raster::ymax(target))))
        exp_xlim <- centre_line[1] + c(-xhalf, xhalf)
        exp_ylim <- centre_line[2] + c(-yhalf, yhalf)

        target <- extend(target, extent(exp_xlim[1], exp_xlim[2], exp_ylim[1], exp_ylim[2]))
    }
    dim(target) <- dimXY
    bathymetry <- coastline <- NULL
    if (isTRUE(bathy)) {            ## insert your local bathy-getter here
        ##if (!exists("topo")) topo <- raster::aggregate(raadtools::readtopo("etopo2", xylim = extent(-180, 180, -90, 0)), fact = 10)
        bathymetry <- raster::projectRaster(Bathy, target)
        if (trim_background) {
            bathymetry <- raster::trim(bathymetry)
            target <- crop(target, bathymetry)
        }
    } else {
        if (inherits(bathy, "BasicRaster")) {
            bathymetry <- raster::projectRaster(bathy[[1]], target, method = "ngb")
            bathy <- TRUE
            if (trim_background) {
                bathymetry <- raster::trim(bathymetry)
                target <- crop(target, bathymetry)
            }
        }

    }

    if (isTRUE(coast)) {
        suppressWarnings({
            coastline <- try(as(sf::st_crop(sf::st_buffer(sf::st_transform(sf::st_as_sf(SOmap_data$continent), prj), 0), xmin = raster::xmin(target), xmax = raster::xmax(target), ymin = raster::ymin(target), ymax = raster::ymax(target)), "Spatial"), silent = TRUE)
            if (inherits(coastline, "try-error")) {
                coast <- FALSE
                warning("no coastline within region, cannot be plotted")
            }
        })
    } else {
        if (inherits(coast, "Spatial")) {
            coastline <- sp::spTransform(coast, prj)
            coast <- TRUE
        }

    }

  # if (croptograt){
  # poly <- as(extent(target), "SpatialPolygons")
  # projection(poly) <- projection(target)
  # g <- graticule(xlim, ylim, proj = projection(target),nverts=10, tiles=TRUE)}

    if (mask) {
        gratmask <- graticule::graticule(seq(xlim[1], xlim[2], length = 30),
                                         seq(ylim[1], ylim[2], length = 5), proj = raster::projection(target), tiles = TRUE)
        if (bathy) {
            bathymetry <- fast_mask(bathymetry, gratmask)
        }
        if (coast) {
            suppressWarnings({
                coastline <- as(sf::st_union(sf::st_intersection(sf::st_as_sf(coastline), sf::st_buffer(sf::st_as_sf(gratmask), 0))), "Spatial")
            })
        }
    }
    if (input_points || input_lines) xy <- rgdal::project(cbind(x, y), prj)

    if (graticule) {
        graticule <- sf::st_graticule(c(raster::xmin(target), raster::ymin(target), raster::xmax(target), raster::ymax(target)),
                                      crs = raster::projection(target), lon = gratlon, lat = gratlat)
    } else {
        graticule <- NULL
    }

    ramp2 <- grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
    bluepal <- ramp2(45)
    ## bk <- c(-10353,-8000,-5000,-4000,-3000,-2000,-1500,-1000,-500,-1,0,1500, 5850)

  # if (croptograt){
  # plot(erase(poly, g), add = TRUE, col = "white")
  # invisible(list(bathy = bathymetry, coastline = coastline, target = target))
  # } else {
    if (!exists("xy")) xy <- NULL
    structure(list(projection = raster::projection(target),
                   bathy = bathymetry, bathyleg = bathyleg, bathy_palette = bluepal,
                   coastline = list(data = coastline, fillcol = NA, linecol = "black"), target = target, ##data = xy,
                   lines_data = if (input_lines) xy else NULL, points_data = if (input_points) xy else NULL,
                   ppch = ppch, pcol = pcol, pcex = pcex,
                   llty = llty, llwd = llwd, lcol = lcol,
                   contours = contours, levels = levels, contour_colour = "black",
                   graticule = graticule, crs = prj),
              class = "SOauto_map")

}


#' @method plot SOauto_map
#' @export
plot.SOauto_map <- function (x, y, ...) {
    print(x)
    invisible()
}

#' @method print SOauto_map
#' @export
print.SOauto_map <- function(x,main=NULL, ...) {
  base_mar <- c(5.1, 4.1, 4.1, 2.1)
    aspect <- if (raster::isLonLat(x$target)) 1/cos(mean(c(raster::xmin(x$target), raster::xmax(x$target))) * pi/180) else 1
    if (is.null(main)) {
       margins <-base_mar/2.5
    } else {
        mars <- base_mar/2.5
        mars[3] <- mars[3]+2
        margins <- mars
    }
    pp <- aspectplot.default(c(raster::xmin(x$target), raster::xmax(x$target)), c(raster::ymin(x$target), raster::ymax(x$target)), asp = aspect, mar =margins)
    ## reset par(pp) when we exit this function
    #on.exit(par(pp))
    ## record current crs
    SOcrs(x$projection)
    newextent <- raster::extent(par("usr"))

    if(!is.null(main)){graphics::title(main = main)}
    op <- par(xpd = FALSE)
    if (!is.null(x$bathy)) {
        if (isTRUE(x$bathyleg)) {
            raster::plot(x$bathy, add = TRUE, col = x$bathy_palette, axes = FALSE)
        } else {
            raster::image(x$bathy, add = TRUE, col = x$bathy_palette, axes = FALSE)#grey(seq(0, 1, length = 40)))
        }
    }
    ## suggested param change: if levels is a scalar than pass it to nlevels
    ## nlevels = 1
    if (x$contours && !is.null(x$bathy)) contour(x$bathy, levels = x$levels, col = x$contour_colour, add = TRUE)

    if (!is.null(x$coastline)) plot(x$coastline$data, col = x$coastline$fillcol, border = x$coastline$linecol, add = TRUE)

    if (!is.null(x$points_data)) points(x$points_data, pch = x$ppch, cex = x$pcex, col = x$pcol)
    if (!is.null(x$lines_data)) lines(x$lines_data, lty = x$llty, lwd = x$llwd, col = x$lcol)

    if (!is.null(x$graticule)) {
        plot_graticule(x$graticule)
    }
    par(op)
    invisible(x)
}

## from ?sf::st_graticule
plot_graticule <- function(g) {
  #plot(sf::st_geometry(g), add = TRUE, col = 'grey', reset = FALSE)
  plot(sf::as_Spatial(g), add = TRUE, col = "grey")
  # points(g$x_start, g$y_start, col = 'red')
  #points(g$x_end, g$y_end, col = 'blue')
op <- par(xpd = NA)
  invisible(lapply(seq_len(nrow(g)), function(i) {
    if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_start[i], pos = 2, cex = .7)
    if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_start[i] - 90, pos = 1, cex = .7)
    if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
      text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_end[i], pos = 4, cex = .7)
    if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
      text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_end[i] - 90, pos = 3, cex = .7)
  }))
  par(op)
  invisible(NULL)
}


aspectplot.default <- function(xlim, ylim, asp, ...) {
  plot.new()
  xlim <- sort(xlim)
  ylim <- sort(ylim)
  r <- asp * abs(diff(ylim)/diff(xlim))
  if(r <= 1) {  # X = 0, 1
    recip <- r / 2
    figure <- c(0, 1, 0.5 - recip, 0.5 + recip)
  } else {     # Y = 0, 1
    recip <- (1/r) / 2
    figure <- c(0.5 - recip, 0.5 + recip, 0, 1)
  }

  p <- par(fig = figure, new = FALSE, ...)
  plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", asp = asp)
  p
}

fast_mask <- function(ras, poly) {
  cells <- tabularaster::cellnumbers(ras, sf::st_as_sf(poly))
  ras[setdiff(1:ncell(ras), cells$cell_)] <- NA
  ras
}

#' Deprecated function
#'
#' Deprecated from SOmap
#' @param ... all arguments passed to new function
#'
#' @export
default_somap <- function(...) {
  .Deprecated("SOauto_map")
}
