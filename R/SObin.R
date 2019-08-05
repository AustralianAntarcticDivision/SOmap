
#' Bin longitude latitude points by count in a SOmap context
#'
#' @param x matrix of 2-columns, longitude,latitude
#' @param ... passed to plot if `add = TRUE`
#' @param col colours to use if `add = TRUE`
#' @param dim dimensions of raster to bin to
#' @param add if `TRUE` binned raster is added to plot
#'
#' @return raster
#' @export
#'
#' @examples
#' library(SOmap)
#' a <- SOmap_auto()
#' a
#' pts <- geosphere::randomCoordinates(1e6)
#' bin <- SObin(pts, add = TRUE)
SObin <- function(x, ..., col = viridis::viridis(26), dim = c(300, 300), add = FALSE) {
  crs <- SOcrs()
  ex <- spex::spex(crs = crs)
  x <- suppressWarnings(reproj::reproj(x, crs)[,1:2])
  r <- raster::raster(ex, nrows = dim[1], ncols = dim[2])
  r <- raster::setValues(r, NA_integer_)
  tib <- tibble::tibble(cell = raster::cellFromXY(r, x))
  summ <- dplyr::summarize(dplyr::group_by(tib, .data$cell) ,count = dplyr::n())
  summ <- dplyr::filter(summ, !is.na(.data$cell))
  r[summ$cell] <- summ$count
  if (add) plot(r, add = TRUE, ..., col = col)
  r
}



