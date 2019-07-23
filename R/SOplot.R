
#' Southern Ocean plot
#'
#' @description
#' Reproject and add point layers to either SOmap or SOmap_auto layers.
#'
#' @param x
#' longitude vector, or object with coordinates
#'
#' @param y
#' lattitude vector, or missing if x is an object
#'
#' @param source
#' starting projection (default = longlat)
#'
#' @param target
#' target projection (default = stereo)
#'
#' @param add
#' add layer to plot (default = TRUE)
#'
#' @param ...
#' other plot options
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#'  x<-c(-70, -60,-50, -90)
#'  y<-c(-50, -75, -45, -60)
#'  map<-SOmap_auto(x,y, input_lines = FALSE)
#'  map
#'  SOplot(x = x, y = y, target = map$projection,pch=19,col=6)
#' }
#' @export
#'


SOplot<-function(x, y = NULL, target = NULL, ..., source = NULL, add=TRUE){
  SObj <- SOproj(x = x, y= y, target = target, source = source, ...)
  #everything <- par(no.readonly = TRUE)
  if (add && (is.matrix(x) || (is.numeric(x) && is.numeric(y)))) {
    points(SObj, ...)
  } else {
    plot(SObj, add=add, ...)
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
