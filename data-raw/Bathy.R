prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
ex <- c(-11344000, 11344000, -11344000, 10800000 )
res <- c(16000, 16000)
src <- "/vsicurl/https://projects.pawsey.org.au/idea-gebco-tif/GEBCO_2024.tif"
library(terra)
r <- rast(ext(ex), crs = prj, res = res)
Bathy <- project(rast(src), r, method = "bilinear", by_util = TRUE)
Bathy <- raster::raster(Bathy) * 1

usethis::use_data(Bathy, overwrite = TRUE)


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
