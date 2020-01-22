stars_to_raster <- function (x, ...)
{
  stopifnot(inherits(x, "stars"))
  if (length(dim(x)) > 3) {
    warning("folding all higher dimensions into the third dimension")
    x = stars::st_apply(x, 1:2, as.vector)
  }
  d = stars::st_dimensions(x)
  dxy = attr(d, "raster")$dimensions
  stopifnot(all(dxy %in% names(d)))
  bb = sf::st_bbox(x)
  if (length(dim(x)) == 2) {
    raster::raster(nrows = dim(x)[dxy[2]], ncols = dim(x)[dxy[1]],
                   xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4],
                   crs = sf::st_crs(x)$proj4string, vals = as.vector(x[[1]]))
  }
  else {
    third = setdiff(names(d), dxy)
    b = raster::brick(nrows = dim(x)[dxy[2]], ncols = dim(x)[dxy[1]],
                      xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4],
                      nl = dim(x)[third], crs = sf::st_crs(x)$proj4string)
    raster::values(b) = as.vector(x[[1]])
    z = seq(d[[third]])
    if (all(!is.na(z)))
      raster::setZ(b, z)
    b
  }
}

proj_longlat <- function() {
  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
}
