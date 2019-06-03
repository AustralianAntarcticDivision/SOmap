## by default we'll store everything in this projection
psproj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

## CCAMLR reference data
library(sp)
library(raster)
require(curl)

get_unzip_data <- function(this_url, fname = basename(this_url)) {
    working_dir <- tempfile()
    if (!dir.exists(working_dir)) dir.create(working_dir)
    curl::curl_download(this_url, destfile = file.path(working_dir, fname))
    unzip(file.path(working_dir, fname), exdir = working_dir)
}

## MPAs
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/mpa-shapefile-WGS84_0.zip") ## "https://data.ccamlr.org/sites/default/files/mpa-shapefile-EPSG102020_0.zip"
MPA1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

## fix non-ascii to avoid check warnings
MPA1$Descr <- NULL #gsub("\uc2\ub0", "degrees ", MPA1$Descr)
chk <- sapply(names(MPA1), function(z) length(tools::showNonASCII(MPA1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in MPA data")

## statistical areas
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/asd-shapefile-WGS84.zip")
CCAMLR1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
CCAMLR1$Descr <- NULL #gsub("\uc2\ub0", "degrees ", CCAMLR1$Descr)
chk <- sapply(names(CCAMLR1), function(z) length(tools::showNonASCII(CCAMLR1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in CCAMLR1 data")

## research blocks
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/rb-shapefile-WGS84_0.zip")
RB1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

## SSRUs
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/ssru-shapefile-WGS84.zip")
SSRU1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
SSRU1$Descr <- NULL ## gsub("\uc2\ub0", "degrees ", SSRU1$Descr)
chk <- sapply(names(SSRU1), function(z) length(tools::showNonASCII(SSRU1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in SSRU1 data")

## SSMUs
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/ssmu-shapefile-WGS84.zip")
SSMU1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
chk <- sapply(names(SSMU1), function(z) length(tools::showNonASCII(SSMU1[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in SSMU1 data")

## VMEs
## Defined areas of registered vulnerable marine ecosystems as defined under CM 22-09.
files <- get_unzip_data("https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:vme_polygons&outputFormat=shape-zip", fname = "vme_polygons.zip")
VME_polygons <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
VME_polygons$comment <- stringi::stri_enc_toascii(VME_polygons$comment)
chk <- sapply(names(VME_polygons), function(z) length(tools::showNonASCII(VME_polygons[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in VME_polygons data")

##<Title>VME Fine Scale Rectangles (FSR)</Title>
## VME Fine-Scale Rectangles identified under CM 22-07.
files <- get_unzip_data("https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:vme_fsr&outputFormat=shape-zip", fname = "vme_fsr.zip")
VME_fsr <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
chk <- sapply(names(VME_fsr), function(z) length(tools::showNonASCII(VME_fsr[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in VME_fsr data")

##<Title>VME Risk Areas</Title>
##<Abstract>VME Risk Areas declared under CM 22-07.</Abstract>
files <- get_unzip_data("https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:vme_risk_areas&outputFormat=shape-zip", fname = "vme_risk_areas.zip")
VME_risk_areas <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
chk <- sapply(names(VME_risk_areas), function(z) length(tools::showNonASCII(VME_risk_areas[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in VME_risk_areas data")

if (FALSE) {
    ## available but not yet included here
    ##<Title>VME lines</Title>
    ##Vulnerable Marine Ecosystem (VME) transects. VMEs notified under CM 22-06 (encounters with VMEs).
    files <- get_unzip_data("https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:vme_lines&outputFormat=shape-zip", fname = "vme_lines.zip")
    VME_lines <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
    chk <- sapply(names(VME_lines), function(z) length(tools::showNonASCII(VME_lines[[z]])) > 0)
    if (any(chk)) stop("non-ASCII chars in VME_lines data")
}

## planning domains
files <- get_unzip_data("https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:CCAMLR_MPA_Planning_Domains&outputFormat=shape-zip", fname = "planning_domains.zip")
planning_domains <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
chk <- sapply(names(planning_domains), function(z) length(tools::showNonASCII(planning_domains[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in planning_domains data")


## continent (was land1)
library(sf)
library(dplyr)
library(sp)
continent <- rnaturalearth::ne_countries(scale = 50, returnclass = "sp")
continent <- continent[coordinates(continent)[,2] < 20, ]

library(spbabel)
sptable(continent) <- sptable(continent) %>% dplyr::filter(y_ > -85, island_)

continent <- continent %>% st_as_sf() %>%  st_buffer(dist = 0) %>%
  group_by(continent) %>% summarize() %>% st_transform(psproj) %>% as("Spatial")

#plot(st_as_sf(continent[2, ]))

## alternative continent with actual ice shelves, eh!
notANT <- sf::st_as_sf(continent[continent$continent != "Antarctica",])
notANT <- sf::st_buffer(notANT, 0)
##buf <- sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_buffer(sf::st_point(cbind(0, 0)), 111111 * (90-abs(q+3)))), crs = psproj)
antonly <- quantarcticR::qa_get("ADD Coastlines (low)", verbose = TRUE, cache = "persistent")
antonly <- sf::st_as_sf(antonly)
ant_coast_ice <- antonly$geometry[antonly$SURFACE != "land"] ## ice shelves, tongues, rumples, etc
ant_coast_land <- sf::st_union(antonly$geometry[antonly$SURFACE == "land"]) ## collapse down
##coast_land <- sf::st_union(c(ant_coast_land, st_geometry(notANT))) ## join with the not-Antarctic land
## then we can plot ant_coast_ice with appropriate col (fill colour) and border (line colour) separately to coast_land with its own fill and border
## NB if we wanted to keep non-Antarctic land separate from Antarctic land, we could do that here, and then they could be potentially plotted in different colours (or handled differently in other ways)


g <- graticule::graticule(seq(-180, 180, by = 5), c(-84, 0), proj = psproj, tiles = TRUE)


chk <- sapply(names(continent), function(z) length(tools::showNonASCII(continent[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in continent data")

## fronts (was ocean1)
fronts_orsi <- spTransform(orsifronts::orsifronts, CRS(psproj))
chk <- sapply(names(fronts_orsi), function(z) length(tools::showNonASCII(fronts_orsi[[z]])) > 0)
if (any(chk)) stop("non-ASCII chars in fronts_orsi data")

## eez and eez_coast (was EEZ1)
files <- get_unzip_data("https://data.ccamlr.org/sites/default/files/eez-shapefile-WGS84.zip")
EEZ1 <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))

if (FALSE) {
    ## or to derive from the original files from marineregions.org
    ##devtools::install_github('SymbolixAU/geojsonsf')
    library(geojsonsf)
    eezlist <- lapply(c(25513, 8383, 8385, 8388, 8387, 8384, 8399), function(id) {
  key <- if (id %in% c(25513)) "eez_iho" else "eez"
  this_url <- paste0("http://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:", key, "&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Emrgid%3C%2FPropertyName%3E%3CLiteral%3E", id, "%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E") ## geojson

  tf <- tempfile(fileext = ".json")
  download.file(this_url, destfile = tf)
  geojson_sf(tf)
})

    common <- purrr::reduce(purrr::map(eezlist, names), intersect)
    eez_coast <- st_geometry(sf::st_transform(do.call(rbind, purrr::map(eezlist, ~.x[common])), psproj))

    ## kill the coast
    library(dplyr)
    eez <- st_cast(eez_coast, "POLYGON")
    ##g <- st_geometry(eez)
    ##eez <- st_set_geometry(eez, sf::st_sfc(lapply(g, function(x) sf::st_polygon(x[1])), crs = st_crs(g))) %>% dplyr::select(territory1, everything())
    eez <- sf::st_sfc(lapply(eez, function(x) sf::st_polygon(x[1])), crs = st_crs(eez))
    eez_coast <- as(eez_coast, "Spatial")
    EEZ1 <- as(eez, "Spatial")
}


## october and february ice extents
files <- get_unzip_data("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/shapefiles/shp_median/median_extent_S_10_1981-2010_polyline_v3.0.zip")
seaice_oct <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))
files <- get_unzip_data("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/shapefiles/shp_median/median_extent_S_02_1981-2010_polyline_v3.0.zip")
seaice_feb <- spTransform(raster::shapefile(files[grepl("shp$", files)]), CRS(psproj))


## elephant seal tracking
ellie_url <- "https://github.com/ianjonsen/bsam/raw/master/data/ellie.RData"
tfile <- tempfile(fileext = "RData")
curl::curl_download(ellie_url, destfile = tfile)
load(tfile)
mirounga_leonina <- ellie

## Antarctic Digital Database coastline medium poly
##cm_url <- "https://add.data.bas.ac.uk/repository/entry/get/Coastline_medium_res_polygon.zip?entryid=synth%3Af477219b-9121-44d6-afa6-d8552762dc45%3AL2NvYXN0bGluZXMvc2hwL0NvYXN0bGluZV9tZWRpdW1fcmVzX3BvbHlnb24uemlw"
##fname <- tempfile(fileext = ".zip")
##ok <- curl::curl_download(cm_url, destfile = fname)
##exdir <- tempfile()
##outfiles <- unzip(fname, exdir = exdir)
##
##ADD_coastline_med <- shapefile(outfiles[grepl("shp$", outfiles)])
##if (!identical(psproj, projection(ADD_coastline_med))) stop("ADD needs reprojecting")

## GSHHG data for non-Antarctica
gshhg_url <- "http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-shp-2.3.7.zip"
fname <- tempfile(fileext = ".zip")
ok <- curl::curl_download(gshhg_url, destfile = fname)
exdir <- tempfile()
outfiles <- unzip(fname, exdir = exdir)

GSHHS_i_L1 <- shapefile(outfiles[grepl("GSHHS_i_L1\\.shp$", outfiles)])
GSHHS_i_L1 <- crop(GSHHS_i_L1, extent(c(-180, 180, -90, 0)))
GSHHS_i_L1 <- spTransform(GSHHS_i_L1, psproj)

SOmap_data <- list(CCAMLR_MPA = MPA1, CCAMLR_statistical_areas = CCAMLR1, CCAMLR_research_blocks = RB1,
                   CCAMLR_SSRU = SSRU1, CCAMLR_SSMU = SSMU1,
                   CCAMLR_VME_polygons = VME_polygons, CCAMLR_VME_fsr = VME_fsr, CCAMLR_VME_risk_areas = VME_risk_areas,
                   CCAMLR_planning_domains = planning_domains,
                   continent = continent, fronts_orsi = fronts_orsi,
                   seaice_feb = seaice_feb, seaice_oct = seaice_oct,
                   mirounga_leonina = mirounga_leonina,
                   EEZ = EEZ1, ##ADD_coastline_med = ADD_coastline_med,
                   GSHHS_i_L1 = GSHHS_i_L1,
                   ant_coast_land = ant_coast_land, ant_coast_ice = ant_coast_ice)

usethis::use_data(SOmap_data, overwrite = TRUE, compress = "xz")

