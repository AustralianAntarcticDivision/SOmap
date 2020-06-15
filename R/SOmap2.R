#' Southern Ocean Map 2
#'
#' @description
#' Function for creating round Southern Ocean maps with inbuild base layers.
#'
#' @param bathy_legend logical: if \code{TRUE}, insert the bathymetry legend.
#' @param ccamlr logical: if \code{TRUE}, insert the CCAMLR area boundaries.
#' @param ccamlr_labels logical: if \code{TRUE}, add labels for the CCAMLR areas.
#' @param ssru logical: if \code{TRUE}, insert the CCAMLR small scale research unit boundaries.
#' @param ssru_labels logical: if \code{TRUE}, add labels for the CCAMLR small scale research units.
#' @param ssmu logical: if \code{TRUE}, insert the CCAMLR small scale management unit boundaries.
#' @param ssmu_labels logical: if \code{TRUE}, add labels for the CCAMLR small scale management units.
#' @param rb logical: if \code{TRUE}, insert the CCAMLR research block boundaries.
#' @param rb_labels logical: if \code{TRUE}, add labels for the CCAMLR research blocks.
#' @param sprfmorb logical: if \code{TRUE}, insert the SPRFMO toothfish research block boundaries.
#' @param border logical: if \code{TRUE}, insert longitude border.
#' @param trim numeric: latitude to trim the map to. Set this to -10 for effectively no trim.
#' @param graticules logical: if \code{TRUE}, insert a graticule grid.
#' @param eez logical: if \code{TRUE}, insert Exclusive Economic Zones.
#' @param eez_labels logical: if \code{TRUE}, add labels for the Exclusive Economic Zones.
#' @param mpa logical: if \code{TRUE}, insert CCAMLR Marine Protected Areas.
#' @param mpa_labels logical: if \code{TRUE}, add labels for the CCAMLR Marine Protected Areas.
#' @param domains logical: if \code{TRUE}, insert CCAMLR Marine Protected Areas planning domains.
#' @param domains_labels logical: if \code{TRUE}, add labels for the CCAMLR Marine Protected Area planning domains.
#' @param iwc logical: if \code{TRUE}, insert International Whaling Commission boundaries.
#' @param iwc_labels logical: if \code{TRUE}, add labels for the International Whaling Commission areas.
#' @param rb_col character: colour for CCAMLR research blocks.
#' @param sprfmo_col character: colour for SPRFMO toothfish research blocks
#' @param ccamlr_col character: colour for CCAMLR boundaries
#' @param ssru_col character: colour for CCAMLR small scale research units.
#' @param ssmu_col character: colour for CCAMLR small scale management units.
#' @param eez_col character: colour for Exclusive Economic Zone boundaries.
#' @param mpa_col character: colour for CCAMLR Marine Protected Areas.
#' @param border_col character: colours for longitude border.
#' @param graticules_col character: colour for graticule grid.
#' @param iwc_col character: colour for IWC boundaries.
#' @param domains_col character: colour for the CCAMLR planning domains boundaries.
#' @param straight logical: if \code{TRUE}, leave a blank space on the side for a straight legend.
#' @param land logical: if \code{TRUE}, plot the coastline.
#' @param land_col character: colour to use for coastline.
#' @param ice logical: if \code{TRUE}, plot ice features (ice shelves, glacier tongues, and similar).
#' @param ice_col character: colour to use for ice features.
#' @param fronts logical or string: if \code{TRUE} or "Orsi", plot Osri et al., (1995) ocean fronts: Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front. If "Park" plot the Park & Durand (2019) fronts; Northern boundary, Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front and Southern Boundary.
#' @param fronts_col character: colours to use for fronts.
#'
#' @return An object of class "SOmap", which represents a polar-stereographic map of the southern hemisphere, with the chosen management layers added. Printing or plotting this object will cause it to be displayed in the current graphics device.
#'
#' @examples
#' \dontrun{
#'   SOmap2(ccamlr = TRUE, mpa = TRUE, trim = -45)
#' }
#' @export
#'

SOmap2 <- function(bathy_legend = TRUE, land = TRUE, ice = TRUE, ccamlr = FALSE, ccamlr_labels = FALSE, ssru = FALSE, ssru_labels = FALSE,
                   ssmu = FALSE, ssmu_labels = FALSE, rb = FALSE, rb_labels = FALSE, sprfmorb = FALSE, border = TRUE, trim = -45,
                   graticules = FALSE, eez = FALSE, eez_labels = FALSE, mpa = FALSE, mpa_labels = FALSE, domains = FALSE, domains_labels = FALSE,
                   iwc = FALSE, iwc_labels = FALSE, straight = FALSE, fronts = FALSE, fronts_col = c("hotpink","orchid","plum"),
                   land_col = "black", ice_col = "black", rb_col = 3, sprfmo_col = 'grey50', ccamlr_col = 2, ssru_col = "grey50", ssmu_col = "grey70", eez_col = "maroon",
                   mpa_col = "yellow", border_col = c("white","black"), graticules_col = "grey70", iwc_col = "blue", domains_col = "magenta") {

    out <- SOmap(bathy_legend = bathy_legend, border = border, trim = trim, graticules = graticules, straight = straight, land = land, land_col = land_col, ice = ice, ice_col = ice_col, fronts = fronts, fronts_col = fronts_col, border_col = border_col, graticules_col = graticules_col)
    ## data
    SOmap_data <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())

    ## get the management layer details from SOmanagement
    mx <- SOmanagement(ccamlr = ccamlr, ccamlr_labels = ccamlr_labels, ccamlr_col = ccamlr_col,
                       ssru = ssru, ssru_labels = ssru_labels, ssru_col = ssru_col,
                       ssmu = ssmu, ssmu_labels = ssmu_labels, ssmu_col = ssmu_col,
                       rb = rb, rb_labels = rb_labels, rb_col = rb_col,
                       sprfmorb = sprfmorb, sprfmo_col = sprfmo_col,
                       trim = trim,
                       eez = eez, eez_labels = eez_labels, eez_col = eez_col,
                       mpa = mpa, mpa_labels = mpa_labels, mpa_col= mpa_col,
                       domains = domains, domains_labels = domains_labels, domains_col = domains_col,
                       iwc = iwc, iwc_labels = iwc_labels, iwc_col = iwc_col)

    if (land && ccamlr) {
        ## change coastline data
        notANT <- sf::st_as_sf(SOmap_data$continent[SOmap_data$continent$continent != "Antarctica",])
        notANT <- sf::st_buffer(notANT, 0)
        buf <- make_buf(trim, proj = out$projection)
        out$coastline$plotargs$x <- suppressWarnings(sf::st_intersection(buf, notANT)$geometry)
    }

    ## copy management layers into out
    out[mx$plot_sequence] <- mx[mx$plot_sequence]
    out$plot_sequence <- insert_into_sequence(out$plot_sequence, ins = mx$plot_sequence, after = c("bathy", "box", "coastline", "ice", "fronts", "graticule"))
    out
}

