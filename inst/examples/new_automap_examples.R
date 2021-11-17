  library(SOmap)

# lonrange, latrange
# lonvec, latvec
# sp, sf object
# raster, stars object
# file


set.seed(1)
SOmap_auto()

SOmap_auto(target = "+proj=laea +lat_0=-20")

llx <-  c(100, 120)
lly <- c(-60, -30)
SOmap_auto(llx, lly)

SOmap_auto(llx, lly, target = NULL)
SOmap_auto(cbind(llx, lly), target = NULL)

SOmap_auto(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = "laea")

SOmap_auto(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, centre_lon = 147)
SOmap_auto(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = NULL)
SOmap_auto(ice)

sst <- raadtools::readsst(xylim = extent(-180, 180, -90, -30))
SOmap_auto(sst, target = "laea", centre_lon = 147, centre_lat = -42)
SOplot(sst, col = palr::sstPal(100))

SOmap_auto(sst, centre_lon = 147, centre_lat = -42)
SOplot(sst, col = palr::sstPal(100))

SOmap_auto(sst, target = "merc", centre_lon = 147, centre_lat = -42)
SOplot(sst, col = palr::sstPal(100))

## FAIL
#SOmap_auto(sst, target = "longlat", centre_lon = 147, centre_lat = -42)
#SOplot(sst, col = palr::sstPal(100))

SOmap_auto(ice, target = "laea", centre_lon = 147, centre_lat = -42)
SOplot(ice, col = palr::ice_pal(100))

SOmap_auto(ice, centre_lon = 147, centre_lat = -42)
SOplot(ice, col = palr::ice_pal(100))

SOmap_auto(ice, target = "merc", centre_lon = 147, centre_lat = -42)
SOplot(ice, col = palr::ice_pal(100))

ramp2 <- grDevices::colorRampPalette(c("#54A3D1","#60B3EB","#78C8F0","#98D1F5","#B5DCFF","#BDE1F0","#CDEBFA","#D6EFFF","#EBFAFF","grey92","grey94","grey96", "white"))
bluepal <- ramp2(45)


set.seed(1)
plot(automap_maker()[[1]])
set.seed(1)


runfun <- function(input) {
  am <- automap_maker(input);
  plot(am$target, col = bluepal);
  points(am$xy);
  SOplot(input, col = viridis::viridis(20))
  invisible(am)
}

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

library(SOmap)
# lonrange, latrange
# lonvec, latvec
# sp, sf object
# raster, stars object
# file

set.seed(1)
plot(automap_maker()[[1]])
set.seed(1)


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


SOmap_auto(x=c(10,60), y=c(-40,-70), mask=T)

SOmap_auto(x=c(10,60), y=c(-40,-70), mask=T, trim_background = FALSE)



