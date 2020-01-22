prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
src <- readAll(raadtools::readtopo("gebco_14"))  ##, xylim = extent(-180, 180, -90, 0))

r <- raster(projectExtent(raster(extent(-180, 180, -90, -5), crs  = proj_longlat()), prj))
## cleanup and rebuild
r <- raster(spex::buffer_extent(r, 16000), crs = prj)
res(r) <- 16000
Bathy <- projectRaster(src, r)
dataType(Bathy) <- "INT2S"
Bathy <- setValues(Bathy, as.integer(values(Bathy)))

devtools::use_data(Bathy, overwrite = TRUE)


if (FALSE) {
    ## not needed any more?
    ## bathy as raster, for use as a rasterGrob
    ## see gghelpers.R for usage example
    library(raster)
    library(magick)
    gebco_cmap <- function(n) {
        r <- seq(from = 60, to = 207, length.out=n)
        g <- seq(from = 155, to = 234, length.out=n)
        b <- seq(from = 207, to = 244, length.out=n)
        vapply(seq_len(n), function(k) sprintf("#%02X%02X%02XFF", round(r[k]), round(g[k]), round(b[k])), FUN.VALUE = "", USE.NAMES = FALSE)
    }
    cmap <- gebco_cmap(21)
    tn <- tempfile(fileext = ".png")
    png(filename = tn, width = 800, height = 800)
    plot(Bathy, axes = FALSE, box = FALSE, col = cmap, legend = FALSE)
    dev.off()
    im <- image_write(image_trim(image_read(tn)), path = tn) ## strip surrounding whitespace

    bathy_grob <- grid::rasterGrob(png::readPNG(tn), width = grid::unit(1,"npc"), height = grid::unit(1,"npc"))
    ## for convenience, add its extent as an attribute
    attr(bathy_grob, "extent") <- extent(Bathy)
    usethis::use_data(bathy_grob, overwrite = TRUE)
    unlink(tn)
}
