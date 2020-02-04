#' Add items to an existing SOmap
#'
#' Reproject and add an object to an existing `SOmap` or `SOmap_auto`.
#'
#' @param x : longitude vector, or an object with coordinates
#' @param y : latitude vector, or missing if x is an object
#' @param target : target projection. If not provided, it will default to the projection of the current map, and if that is not set it will use the default SOmap polar stereographic projection
#' @param add logical: if `TRUE`, add this object to an existing plot
#' @param ... : other parameters passed to the `plot` function
#' @param source : if `x` is not an object with a projection already set, specify its projection here (default = longlat)
#'
#' @examples
#' \dontrun{
#'   x <-c (-70, -60,-50, -90)
#'   y <-c (-50, -75, -45, -60)
#'   map <- SOmap_auto(x, y, input_lines = FALSE)
#'
#'   ## plot the map, with the x, y points already added
#'   map
#'   ## re-plot the points in a different colour and marker
#'   SOplot(x = x, y = y, pch = 0, cex = 2, col = 6)
#' }
#' @export
SOplot<-function(x, y = NULL, target = NULL, ..., source = NULL, add=TRUE) {
  SObj <- SOproj(x = x, y = y, target = target, source = source, ...)
  everything <- par(no.readonly = TRUE)
  if (add && (is.matrix(x) || (is.numeric(x) && is.numeric(y)))) {
    points(SObj, ...)
  } else {
      if (inherits(SObj, "BasicRaster")) {
          if (raster::nlayers(SObj) == 3) {
              ## assume raster is a RGB, and zap white with bgalpha
              raster::plotRGB(SObj, add = add, bgalpha = 0, ...)
          } else {
              plot(SObj, add=add, ...)
              ## calling plot.raster changes some par values, which will cause problems for subsequent
              ##   SOplot calls. See e.g. https://github.com/AustralianAntarcticDivision/SOmap/issues/67 and
              ##   https://github.com/AustralianAntarcticDivision/SOmap/issues/36
              par(fig = everything$fig) ## this seems to be the critical one
          }
      } else {
          plot(SObj, add=add, ...)
      }
  }
  #par(everything)
  invisible(NULL)
}


## not yet exported
## function to apply SOproj then construct object
## p <- SOmap()
## usrp <- SOify(x = c(-70, -60,-50, -90), y = c(-50, -75, -45, -60), target = p$projection, pch = 19, col = 6)
## p
## usrp
SOify <- function(x, y = NULL, target = NULL, ..., source = NULL, add = TRUE){
    SObj <- SOproj(x = x, y = y, target = target, source = source, ...)
    out <- list(target = target, projection = SObj$projection)
    ## we want a unique name for this object (unique within R session)
    ## this is a bit rubbish, must be a better way
    tf <- tempfile()
    objname <- basename(tf)
    writeLines("", tf) ## create that file so next call to tempfile() can't use the same file name
    out$plot_sequence <- objname
    if (!is.null(source)) {
        out$trim <- source$trim
        out$projection <- source$projection
    }
    if ((is.matrix(x) || (is.numeric(x) && is.numeric(y)))) {
        out[[objname]] <- SO_plotter(plotfun = "points", plotargs = list(SObj, ...))
    } else {
        out[[objname]] <- SO_plotter(plotfun = "plot", plotargs = list(SObj, add = add, ...))
    }
    structure(out, class = "SOthing")
}

# @method plot SOthing
# @export
plot.SOthing <- function (x, y, ...) {
    print(x)
    invisible()
}

# @method print SOthing
# @export
print.SOthing <- function(x, ...) {
    plot_all(x)
    invisible(x)
}
