#' project like rgdal
#'
#' don't use this function
#' @param x matrix
#' @param proj target projection
#' @param inv  ignored,
#' @param ... ignored
#' @param source source projection
#'
#' @return matrix of x transformed from source to proj
#' @export
#'
#' @examples
#' gdal_project(cbind(0, 0), "+proj=laea +lon_0=147")
gdal_project <- function(x, proj, inv = FALSE, ..., source = NULL) {
  if (!inv) source <- "EPSG:4326"
  ## this actually makes no sense, proj and source should be flipped at this point, but we're not fixing that here
  if (inv && is.null(source)) stop("source crs  must be specified")
  terra::project(x, to = proj, from = source)
}
