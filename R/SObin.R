
#' Bin longitude latitude points by count in a SOmap context
#'
#' This is intended as a worker, to be used as an option
#' to `SOauto_map()`, perhaps with `SObin = TRUE`.
#'
#' @param x longitudes
#' @param y latitudes
#' @param ... passed to plot if `add = TRUE`
#' @param col colours to use if `add = TRUE`
#' @param dim dimensions of raster to bin to
#' @param add if `TRUE` binned raster is added to plot
#' @param target target projection passed to SOproj
#' @param source source projection of data projection passed to SOproj
#'
#'
#' @return raster
#' @noRd
#' @keywords internal
#' @examples
#' library(SOmap)
#' a <- SOmap_auto()
#' a
#' pts <- geosphere::randomCoordinates(1e6)
#' bin <- SObin(pts[, 1], pts[, 2], add = TRUE)
SObin <- function(x, y = NULL, ..., col = viridisLite::viridis(26), dim = c(300, 300), add = TRUE, target = NULL, source = NULL, data.frame=FALSE) {
  SObj <- SOproj(x = x, y= y, target = target, source = source)

  if(missing(target)){crs <- SOcrs()}else(
    crs<-target
  )

  ex <- spex::spex(x=SObj,crs = crs)
  r <- raster::raster(ex, nrows = dim[1], ncols = dim[2])
  r <- raster::setValues(r, NA_integer_)
  tib <- tibble::tibble(cell = raster::cellFromXY(r, sp::coordinates(SObj)))
  summ <- dplyr::summarize(dplyr::group_by(tib, .data$cell), count = dplyr::n())
  summ <- dplyr::filter(summ, !is.na(.data$cell))
  r[summ$cell] <- summ$count
  if (add) plot(r, add = TRUE, ..., col = col)
  if (data.frame){
    rdf<-as.data.frame(r, xy=TRUE)
    return(rdf)
  }else(return(r))
}



