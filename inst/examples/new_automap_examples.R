
# lonrange, latrange
# lonvec, latvec
# sp, sf object
# raster, stars object
# file

set.seed(1)
plot(automap_maker()[[1]])
set.seed(1)

aaargh
plot(automap_maker(target = "+proj=laea +lat_0=-20")[[1]])

llx <-  c(100, 120)
lly <- c(-60, -30)
plot(automap_maker(llx, lly)[[1]])
points(reproj(expand.grid(llx, lly), SOcrs()))
plot(automap_maker(llx, lly, target = NULL)[[1]])
points(reproj(expand.grid(llx, lly), SOcrs()))
plot(automap_maker(cbind(llx, lly), target = NULL)[[1]])
points(reproj(expand.grid(llx, lly), SOcrs()))

plot(automap_maker(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = "laea")[[1]])
points(reproj(cbind(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat), SOcrs()))

plot(automap_maker(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, centre_lon = 147)[[1]])
points(reproj(cbind(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat), SOcrs()))

plot(automap_maker(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = NULL)[[1]])
points(reproj(cbind(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat), SOcrs()))

plot(automap_maker(ice)[[1]])
image(ice, add = TRUE)

sst <- raadtools::readsst(xylim = extent(-180, 180, -90, -30))
plot(automap_maker(sst, target = "laea", centre_lon = 147, centre_lat = -42)[[1]])
SOplot(sst, col = palr::sstPal(100))
