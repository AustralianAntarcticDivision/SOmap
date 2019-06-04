## example plot of the pelagic regionalisation clusters

library(raster)
library(SOmap)

## download the pelagic regionalisation data
data_file <- tempfile(fileext = ".zip")
download.file("https://data.aad.gov.au/eds/4424/download", destfile = data_file, mode = "wb")
## unzip it into a temporary directory
data_dir <- tempfile()
dir.create(data_dir)
unzip(data_file, exdir = data_dir)
## read the csv file
x <- read.csv(file.path(data_dir, "pelagic_regionalisation", "pelagic_cluster_20.csv"))

## the colour map
cmap <- matrix(c(191, 152, 48,
                 166, 74, 0,
                 230, 68, 0,
                 236, 96, 89,
                 166, 4, 0,
                 255, 1, 10,
                 255, 199, 144,
                 158, 202, 225,
                 107, 174, 214,
                 33, 113, 181,
                 8, 48, 107,
                 238, 198, 239,
                 180, 0, 151,
                 117, 0, 98,
                 192, 244, 0,
                 154, 183, 46,
                 0, 201, 13,
                 0, 130, 9,
                 255, 186, 0,
                 255, 253, 0), nrow = 20, byrow = TRUE)
## convert this to hex colour strings
cmap <- apply(cmap, 1, function(z) sprintf("#%02x%02x%02x", z[1], z[2], z[3]))

## create raster object from the csv data
xr <- rasterFromXYZ(x)
projection(xr) <- "+proj=longlat +datum=WGS84"

## create the map object that we'll then modify
p <- SOmap(trim = -40, bathy_legend = FALSE, border_width = 0.5)
## replace the bathymetry data with the regionalisation
p$bathy$plotargs$x <- projectRaster(xr, crs = p$projection, method = "ngb")
## replace the colour map
p$bathy$plotargs$col <- cmap
plot(p)
## add a legend
legend(x = "right", legend = as.character(1:20), fill = cmap)
