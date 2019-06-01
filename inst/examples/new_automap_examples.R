
# lonrange, latrange
# lonvec, latvec
# sp, sf object
# raster, stars object
# file


set.seed(1)
SOauto_map()

SOauto_map(target = "+proj=laea +lat_0=-20")

llx <-  c(100, 120)
lly <- c(-60, -30)
SOauto_map(llx, lly)

SOauto_map(llx, lly, target = NULL)
SOauto_map(cbind(llx, lly), target = NULL)

SOauto_map(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = "laea")

SOauto_map(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, centre_lon = 147)
SOauto_map(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = NULL)
SOauto_map(ice)

sst <- raadtools::readsst(xylim = extent(-180, 180, -90, -30))
SOauto_map(sst, target = "laea", centre_lon = 147, centre_lat = -42)
SOplot(sst, col = palr::sstPal(100))

SOauto_map(sst, centre_lon = 147, centre_lat = -42)
SOplot(sst, col = palr::sstPal(100))

SOauto_map(sst, target = "merc", centre_lon = 147, centre_lat = -42)
SOplot(sst, col = palr::sstPal(100))

## FAIL
#SOauto_map(sst, target = "longlat", centre_lon = 147, centre_lat = -42)
#SOplot(sst, col = palr::sstPal(100))

SOauto_map(ice, target = "laea", centre_lon = 147, centre_lat = -42)
SOplot(ice, col = palr::icePal(100))

SOauto_map(ice, centre_lon = 147, centre_lat = -42)
SOplot(ice, col = palr::icePal(100))

SOauto_map(ice, target = "merc", centre_lon = 147, centre_lat = -42)
SOplot(ice, col = palr::icePal(100))

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



