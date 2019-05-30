## TODO: attribution properly
# Ressource 59800 - Park Young-Hyang, Durand Isabelle (2019). Altimetry-drived Antarctic Circumpolar Current fronts. SEANOE. https://doi.org/10.17882/59800
#
# Requested file :
#   Nom	Id
# Geographical positions of five altimetry-derived fronts of the Antarctic Circumpolar Current	62985


f <- system.file("extdata/SEANOE/ACC_FRONTS_Locations.nc",
                 package = "SOmap")
grids <- ncmeta::nc_grids(f)
library(purrr)
x <- tidync::tidync(f)
purrr::map(grids$grid,
           ~(tidync::activate(x, .x) %>%
               tidync::hyper_tibble() %>% names()))

## rewrap to -180, 180
repivot <- function(x) {
  pivot <- max(which(x[, 1]  >= 0)) + 1
  print(pivot)
  rbind(x[c(pivot:nrow(x), 1:(pivot - 1)), ])
}

l <- sf::st_sfc(purrr::map(grids$grid,
           ~(tidync::activate(x, .x) %>%
               tidync::hyper_tibble())[2:1] %>% as.matrix() %>%
             repivot() %>%
             sf::st_linestring()), crs = 4326)

ACC_FRONTS <- sf::st_sf(name = c("Northern Boundary",
                   "Subantarctic",
                   "Polar",
                   "Southern Antarctic Circumpolar Current",
                   "Southern Boundary"),
          front = c("NB", "SAF", "PF", "SACCF", "SB"), geometry = l)
saveRDS(ACC_FRONTS, "data-raw/ACC_FRONTS.rds")

#ggplot(SOproj(ACC_FRONTS), aes(colour = name)) + geom_sf() + coord_sf()


