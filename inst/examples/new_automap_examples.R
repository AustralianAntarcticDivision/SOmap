
# lonrange, latrange
# lonvec, latvec
# sp, sf object
# raster, stars object
# file
llx <-  c(100, 120)
lly <- c(-60, -30)
plot(mk_automap(llx, lly)[[1]])
points(reproj(expand.grid(llx, lly), SOcrs()))
plot(mk_automap(llx, lly, target = NULL)[[1]])
points(reproj(expand.grid(llx, lly), SOcrs()))
plot(mk_automap(cbind(llx, lly), target = NULL)[[1]])
points(reproj(expand.grid(llx, lly), SOcrs()))

plot(mk_automap(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = "laea")[[1]])
points(reproj(cbind(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat), SOcrs()))

plot(mk_automap(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, centre_lon = 147)[[1]])
points(reproj(cbind(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat), SOcrs()))

plot(mk_automap(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat, target = NULL)[[1]])
points(reproj(cbind(SOmap_data$mirounga_leonina$lon, SOmap_data$mirounga_leonina$lat), SOcrs()))

plot(mk_automap(ice)[[1]])
image(ice, add = TRUE)

sst <- raadtools::readsst(xylim = extent(-180, 180, -90, -30))
plot(mk_automap(sst, target = "laea", centre_lon = 147, centre_lat = -42)[[1]])
SOplot(sst, col = palr::sstPal(100))
