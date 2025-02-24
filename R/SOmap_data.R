#' Contextual data for Southern Ocean maps
#'
#' Various spatial datasets that are commonly used on Southern Ocean maps.
#'
#' @docType data
#'
#' @usage data(SOmap_data)
#'
#' @format A list containing the following elements:
#' \itemize{
#'   \item CCAMLR_MPA
#'     \itemize{
#'       \item Description: current marine protected areas
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/marine-protected-areas
#'       \item License: not specified
#'     }
#'   \item CCAMLR_research_blocks
#'     \itemize{
#'       \item Description: A defined spatial area in which research fishing on toothfish is conducted under a research plan agreed by the Commission
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/research-blocks
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_SSMU
#'     \itemize{
#'       \item Description: Small-scale management units (SSMUs) are designed to be used as a basis for subdividing the precautionary catch limit for krill in Subareas 48.1, 48.2, 48.3 and 48.4, and in developing management procedures for the krill fishery that can adequately account for localised effects on krill predators (SC-CAMLR-XXI, paragraphs 3.16 to 3.18). The boundaries of the SSMUs are based on predator foraging ranges (refer SC-CAMLR-XXI, Annex 4 and Trathan et al, 2008).
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/small-scale-management-units
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_SSRU
#'     \itemize{
#'       \item Description: Small-scale research units (SSRUs) are designed to be used as a basis for subdividing the precautionary catch limit for toothfish in exploratory fisheries, and in condcuting research fishing and developing stock assessments. The boundaries of the SSRUs are defined in Conservation Measure 41-01 (2013). CCAMLR Secretariat (2013).
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/small-scale-research-units
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_statistical_areas
#'     \itemize{
#'       \item Description: Statistical areas, subareas and divisions are used globally for the purpose of reporting fishery statistics. CCAMLR's Convention Area in the Southern Ocean is divided, for statistical purposes, into Area 48 (Atlantic Antarctic) between 70W and 30E, Area 58 (Indian Ocean Antarctic) between 30 and 150E, and Area 88 (Pacific Antarctic) between 150E and 70W. These areas, which are further subdivided into subareas and divisions, are managed by CCAMLR.
#'       \item Source: CCAMLR
#'       \item URL: https://data.ccamlr.org/dataset/statistical-areas-subareas-and-divisions
#'       \item License: Public domain
#'     }
#'   \item CCAMLR_VME_polygons
#'     \itemize{
#'       \item Description: Defined areas of registered vulnerable marine ecosystems as defined under CM 22-09.
#'       \item Source: CCAMLR
#'       \item URL: https://gis.ccamlr.org/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_VME_fsr
#'     \itemize{
#'       \item Description: Vulnerable marine ecosystem fine-scale rectangles identified under CM 22-07.
#'       \item Source: CCAMLR
#'       \item URL: https://gis.ccamlr.org/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_VME_risk_areas
#'     \itemize{
#'       \item Description: Vulnerable marine ecosystem risk areas declared under CM 22-07.
#'       \item Source: CCAMLR
#'       \item URL: https://gis.ccamlr.org/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities
#'       \item License: Not specified
#'     }
#'   \item CCAMLR_planning_domains
#'     \itemize{
#'       \item Description: Nine planning domains were defined during the 2011 CCAMLR workshop on marine protected areas (SC-CAMLR-XXX, Annex 6). These planning domains provide comprehensive coverage of bioregions in the Southern Ocean and may be used as reporting and auditing units for work related to the development of MPAs and as a means to organise future activities related to this effort.
#'       \item Source: CCAMLR
#'       \item URL: https://gis.ccamlr.org/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities
#'       \item License: Not specified
#'     }
#'   \item continent
#'     \itemize{
#'       \item Description: Coastline, details TBA
#'       \item Source: TBA
#'       \item URL: TBA
#'       \item License: TBA
#'     }
#'   \item EEZ
#'     \itemize{
#'       \item Description: An exclusive economic zone (EEZ) is a sea zone prescribed by the United Nations Convention on the Law of the Sea over which a state has special rights regarding the exploration and use of marine resources.
#'       \item Source: Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). https://doi.org/10.14284/403
#'       \item URL: https://www.marineregions.org/
#'       \item License: CC-BY
#'     }
#'   \item fronts_orsi
#'     \itemize{
#'       \item Description: Southern Ocean fronts as defined by Orsi et al. 1995
#'       \item Source: orsifronts
#'       \item URL: https://github.com/AustralianAntarcticDivision/orsifronts
#'       \item License: see orsifronts
#'     }
#'   \item seaice_feb and seaice_oct
#'     \itemize{
#'       \item Description: median October and February sea ice extent
#'       \item Source: Fetterer, F., K. Knowles, W. Meier, M. Savoie, and A. K. Windnagel. 2017, updated daily. Sea Ice Index, Version 3. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center
#'       \item URL: https://doi.org/10.7265/N5K072F8
#'       \item License: Please cite
#'     }
#'    \item mirounga_leonina
#'     \itemize{
#'       \item Description: Example elephant seal Argos tracking data
#'       \item Source: Data were sourced from the Integrated Marine Observing System (IMOS) - IMOS is supported by the Australian Government through the National Collaborative Research Infrastructure Strategy and the Super Science Initiative
#'       \item URL: https://github.com/ianjonsen/bsam
#'       \item License: Please cite
#'     }
#'    \item ADD_coastline_med
#'     \itemize{
#'       \item Description: Medium-resolution coastline data from the SCAR Antarctic Digital Database. This coastline only covers continental Antarctica: see the GSHHS_i_L1 data for the remainder of the southern hemisphere
#'       \item Source: SCAR
#'       \item URL: https://add.data.bas.ac.uk/repository/entry/show?entryid=f477219b-9121-44d6-afa6-d8552762dc45
#'       \item License: CC-BY. Citation: SCAR Antarctic Digital Database (2018)
#'     }
#'    \item GSHHS_i_L1
#'     \itemize{
#'       \item Description: Coastline data (excluding Antarctica) from the Global Self-consistent, Hierarchical, High-resolution Geography Database. Only southern hemisphere, level 1 (boundary between land and ocean, except Antarctica), intermediate resolution data are included here
#'       \item Source: Wessel P, Smith WHF (1996) A Global Self-consistent, Hierarchical, High-resolution Shoreline Database. J. Geophys. Res. 101:8741-8743
#'       \item URL: http://www.soest.hawaii.edu/wessel/gshhg/
#'       \item License: LGPL
#'     }
#'     \item fronts_park
#'     \itemize{
#'       \item Description: Altimetry-derived Antarctic Circumpolar Current fronts
#'       \item Source: Park Young-Hyang, Durand Isabelle (2019). Altimetry-drived Antarctic Circumpolar Current fronts. SEANOE.
#'       \item URL: https://doi.org/10.17882/59800
#'       \item License: CC-BY-4.0, please cite in full as at DOI
#'     }
#' }
#' @keywords datasets
"SOmap_data"

#' Sea ice
#'
#' Example sea ice concentration data from the Southern Ocean (2018-10-15).
#' (See "data-raw/ice.R").
#' @docType data
#' @name ice
#' @examples
#' \dontrun{
#'   ll <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#'   xy <- coordinates(spTransform(as(SOmap_data$fronts_orsi, "SpatialPoints"), ll))
#'   ## just because you can doesn't mean you should ...
#'   SOmap_auto(xy[,1], xy[,2], bathy = ice, input_points = FALSE, levels = c(15, 30, 60, 90))
#' }
NULL

#' Bathymetric data for maps
#'
#' Bathymetric data reprocessed from the General Bathymetric Chart of the Ocean.
#'
#' These data providers don't publish the data in cloud-ready form, so we have a few versions convert to Cloud-Optimized GeoTIFF, always using the
#' 'ice surface elevation' form. See [R package sds](https://github.com/hypertidy/sds) which records the latest available source in `sds::gebco()` and
#' has some time-stamped versions of that function for older copies.
#'
#' The process for creating a COG is recorded here: [derived GEBCO reformat](https://github.com/AustralianAntarcticDivision/raad-deriv/issues/3).
#' @docType data
#'
#' @usage data(Bathy)
#'
#' @format An object of class \code{"RasterLayer"}
#'
#' @keywords datasets
#'
#' @references GEBCO - The General Bathymetric Chart of the Oceans, latest version at time of writing was 2024, with attribution
#' "GEBCO Compilation Group (2024) GEBCO 2024 Grid (doi:10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f)".
#'
#' @source \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO}
"Bathy"

