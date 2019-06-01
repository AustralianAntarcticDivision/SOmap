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
#' Try `target` families such as 'lcc', 'laea', 'gnom', 'merc', 'aea' if feeling adventurous.
#'
#' Using `mask = TRUE` does not work well when the pole is included, so it's `FALSE` by default.
#'
#' @param x optional input data longitudes
#' @param y optional input data latitudes
#' @param centre_lon optional centre longitude (of the map projection, also used to for plot range if `expand = TRUE`)
#' @param centre_lat as per `centre_lon`
#' @param target optional projection family (default is `stere`ographic), or full PROJ string (see Details)
#' @param dimXY dimensions of background bathmetry (if used) default is 300x300
#' @param bathy logical: if \code{TRUE}, plot bathymetry. Alternatively, provide the bathymetry data to use as a \code{raster} object
#' @param coast logical: if \code{TRUE}, plot coastline. Alternatively, provide the coastline data to use as a \code{Spatial} object
#' @param input_points add points to plot (of x, y)
#' @param input_lines add lines to plot   (of x, y)
#' @param graticule flag to add a basic graticule
#' @param expand fraction to expand plot range (default is 0.05, set to zero for no buffer, may be negative)
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
#' @param gratpos positions (sides) of graticule labels
#' @return An object of class SOauto_map, containing the data and other details required to generate the map. Printing or plotting the object will cause it to be plotted.
#' @export
#' @examples
#' \dontrun{
#'   SOauto_map(c(0, 50), c(-70, -50))
#'   SOauto_map(runif(10, 130, 200), runif(10, -80, -10))
#'   SOplot(c(147, 180), c(-42, -60), pch = 19, cex = 2,col = "firebrick")
#'   SOauto_map(runif(10, 130, 200), runif(10, -85, -60))
#'
#'   ## save the result to explore later!
#'   protomap <- SOauto_map(runif(10, 60, 160), runif(10, -73, -50))
#'
#'   SOauto_map(runif(50, 40, 180), runif(50, -73, -10), family = "laea", centre_lat = -15,
#'                 input_lines = FALSE)
#' }
SOauto_map <- function(x, y, centre_lon = NULL, centre_lat = NULL, target = "stere",
                       dimXY = c(300, 300),
                       bathy = TRUE, coast = TRUE, input_points = TRUE, input_lines = TRUE,
                       graticule = TRUE, expand = 0.05,
                       contours = TRUE, levels = c(-500, -1000, -2000),
                       trim_background = TRUE,
                       mask = FALSE, ppch = 19, pcol = 2, pcex = 1, bathyleg = FALSE, llty = 1, llwd = 1, lcol = 1,
                       gratlon = NULL, gratlat = NULL, gratpos="all") {
    ## check inputs
    assert_that(is.flag(contours), !is.na(contours))
    assert_that(is.numeric(levels), length(levels) > 0)
    assert_that(is.numeric(expand), msg = "'expand' must be numeric, changed behaviour in SOmap > 0.2.1")
    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())
    if (missing(y)) y <- NULL
## automap_nothing ----
    if (missing(x) && missing(y)) {
        xy <- automap_nothing()
        x <- xy[,1]
        y <- xy[,2]
    }
## END automap_nothing ----

    ## automap_maker ----
    amap <- automap_maker(x, y = y,
                          centre_lon = centre_lon,
                          centre_lat = centre_lat,
                          target = target, dimXY = dimXY)
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
                coastline <- NULL
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

    grat <- sf::st_graticule(c(raster::xmin(target), raster::ymin(target), raster::xmax(target), raster::ymax(target)),
                             crs = raster::projection(target), lon = gratlon, lat = gratlat)

    if (mask) {
        #gratmask <- graticule::graticule(seq(xlim[1], xlim[2], length = 30),
        #                                 seq(ylim[1], ylim[2], length = 5), proj = raster::projection(target), tiles = TRUE)


        if (bathy) {
            bathymetry <- fast_mask(bathymetry, grat)
        }
        if (coast) {
            suppressWarnings({
                coastline <- as(sf::st_union(sf::st_intersection(sf::st_as_sf(coastline),
                                      sf::st_buffer(sf::st_as_sf(grat), 0))), "Spatial")
            })
        }
    }

    if (graticule) {
      graticule <- grat
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
                   graticule = graticule, crs = prj, gratpos=gratpos),
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
print.SOauto_map <- function(x,main=NULL, ..., set_clip = TRUE) {
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

    if (!is.null(x$coastline$data)) plot(x$coastline$data, col = x$coastline$fillcol, border = x$coastline$linecol, add = TRUE)

    if (!is.null(x$points_data)) points(x$points_data, pch = x$ppch, cex = x$pcex, col = x$pcol)
    if (!is.null(x$lines_data)) lines(x$lines_data, lty = x$llty, lwd = x$llwd, col = x$lcol)

    if (!is.null(x$graticule)) {
        plot_graticule(x$graticule, GratPos=x$gratpos)
    }
    par(op)
    if (set_clip) graphics::clip(raster::xmin(x$target), raster::xmax(x$target),
                                 raster::ymin(x$target), raster::ymax(x$target))

    invisible(x)
}

'%notin%'<-Negate('%in%')

## from ?sf::st_graticule
plot_graticule <- function(g, GratPos) {
  gratopts<-c("all", "left", "right", "top", "bottom")
  if(GratPos[1] %notin% gratopts) stop("gratpos must be one of: all, left, right, top, bottom.")
  #plot(sf::st_geometry(g), add = TRUE, col = 'grey', reset = FALSE)
  plot(sf::as_Spatial(g), add = TRUE, col = "grey")
  # points(g$x_start, g$y_start, col = 'red')
  #points(g$x_end, g$y_end, col = 'blue')
op <- par(xpd = NA)
  invisible(lapply(seq_len(nrow(g)), function(i) {
    if(GratPos=="all" || "left" %in% GratPos){
      if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000){
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_start[i], pos = 2, cex = .7)}} #left

    if(GratPos=="all" || "bottom" %in% GratPos){
      if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000){
      text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
            srt = g$angle_start[i] - 90, pos = 1, cex = .7)}} #bottom

    if(GratPos=="all" || "right" %in% GratPos ){
    if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000){
       text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
            srt = g$angle_end[i], pos = 4, cex = .7)}} #right

    if(GratPos=="all" || "top" %in% GratPos ){
    if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000){
      text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
           srt = g$angle_end[i] - 90, pos = 3, cex = .7)}} #top
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
