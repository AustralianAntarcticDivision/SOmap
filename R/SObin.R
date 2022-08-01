
#' Bin longitude latitude points by count in a SOmap context
#'
#' Creates a raster density layer from a set of points.
#'
#' @param x longitudes
#' @param y latitudes
#' @param baselayer optional spatial layer to get extent from
#' @param ... passed to plot if `add = TRUE`
#' @param col colours to use if `add = TRUE`
#' @param dim dimensions of raster to bin to
#' @param add if `TRUE`, the raster is added to the current plot. An error is thrown if there is no existing plot
#' @param target target projection passed to SOproj
#' @param source source projection of data projection passed to SOproj
#' @param data.frame if true return a data frame instead of a raster.
#'
#' @return A raster. If `add = TRUE`, it is returned invisibly.
#' @export
#' @examples
#' SOmap_auto()
#' pts <- geosphere::randomCoordinates(1e6)
#' bin <- SObin(pts[, 1], pts[, 2], add = TRUE)
SObin <- function(x, y = NULL, baselayer = NULL, ..., col = hcl.colors(26, "Viridis"), dim = c(512, 512), add = TRUE, target = NULL, source = NULL, data.frame = FALSE) {
    assert_that(is.flag(add), !is.na(add))
    if (dev.cur() == 1L && add) {
        stop("There is no existing plot to add to")
    }
    SObj <- SOproj(x = x, y = y, target = target, source = source)
    if (missing(target)) {
        crs <- SOcrs()
    } else {
        crs <- target
    }
    if (add) {
        ex <- spex::spex(crs = crs)
    }
    if (!missing(baselayer)) {
        ex <- spex::spex(x = baselayer, crs = crs)
    }
    if (missing(baselayer) && !add) {
        ex <- spex::spex(x=SObj,crs = crs)
    }

    r <- raster::raster(ex, nrows = dim[1], ncols = dim[2])
    r <- raster::setValues(r, NA_integer_)
    tib <- tibble::tibble(cell = raster::cellFromXY(r, sp::coordinates(SObj)))
    summ <- dplyr::summarize(dplyr::group_by(tib, .data$cell), count = dplyr::n())
    summ <- dplyr::filter(summ, !is.na(.data$cell))
    r[summ$cell] <- summ$count
    if (add && dev.cur() != 1L) plot(r, add = TRUE, ..., col = col)
    if (data.frame) r <- raster::as.data.frame(r, xy = TRUE)
    if (add) invisible(r) else r
}


