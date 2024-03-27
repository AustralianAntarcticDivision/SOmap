# project like rgdal
gdal_project <- function(x, proj, inv = FALSE, ..., source = NULL) {
  if (!inv) source <- "EPSG:4326"
  ## this actually makes no sense, proj and source should be flipped at this point, but we're not fixing that here
  if (inv && is.null(source)) stop("source crs  must be specified")
  terra::project(x, to = proj, from = source)
}
