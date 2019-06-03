#' Southern Ocean Map 2
#'
#' @description
#' Function for creating round Southern Ocean maps with inbuild base layers.
#'
#' @param bathy_legend
#' Insert the bathymetry legend.
#' @param ccamlr
#' Insert the CCAMLR boundaries.
#' @param ccamlr_labels
#' Insert the CCAMLR labels.
#' @param ssru
#' Insert the CCAMLR small scale research unit boundaries.
#' @param ssru_labels
#' Insert the CCAMLR small scale research unit labels.
#' @param ssmu
#' Insert the CCAMLR small scale management unit boundaries.
#' @param ssmu_labels
#' Insert the CCAMLR small scale management unit labels.
#' @param rb
#' Insert the CCAMLR research block boundaries.
#' @param rb_labels
#' Insert the CCAMLR research block labels.
#' @param sprfmorb
#' Insert the SPRFMO toothfish research block boundaries.
#' @param border
#' Insert longitude border.
#' @param trim
#' Longitude to trim map to.
#' @param graticules
#' Insert graticule grid.
#' @param eez
#' Insert Exclusive Economic Zones.
#' @param eez_labels
#' Insert Exclusive Economic Zone labels.
#' @param mpa
#' Insert CCAMLR Marine Protected Areas.
#' @param mpa_labels
#' Insert CCAMLR Marine Protected Area labels.
#' @param domains
#' Insert CCAMLR Marine Protected Areas planning domains.
#' @param domains_labels
#' Insert CCAMLR Marine Protected Area planning domains labels.
#' @param iwc
#' Insert International Whaling Commission boundaries.
#' @param iwc_labels
#' Insert International Whaling Commission labels.
#' @param rb_col
#' Color for CCAMLR research blocks.
#' @param sprfmo_col
#' Color for SPRFMO toothfish research blocks
#' @param ccamlr_col
#' Color for CCAMLR boundaries
#' @param ssru_col
#' Color for CCAMLR small scale research units.
#' @param ssmu_col
#' Color for CCAMLR small scale management units.
#' @param eez_col
#' Color for Exclusive Economic Zone boundaries; Default is maroon.
#' @param mpa_col
#' Color for CCAMLR Marine Protected Areas; Default is yellow.
#' @param border_col
#' Colors for longitude border; Default is c("black","white").
#' @param graticules_col
#' Color for graticule grid; Default is grey.
#' @param iwc_col
#' Color for IWC boundaries; Default is blue.
#' @param domains_col
#' Color for the CCAMLR planning domains boundaries. Default is magenta.
#' @param straight
#' Do you need a blank space on the side for a straight legend.
#' @param land
#' Plot land boundary
#' @param fronts
#' Plot ocean fronts: Subantarctic Front, Polar Front, Southern Antarctic Circumpolar Current Front
#' @param fronts_col
#' colors for fronts
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#' SOmap2(ccamlr = TRUE, mpa = TRUE, trim = -45)
#' }
#' @export
#'

SOmap2<-function(bathy_legend=TRUE,
                 land=TRUE,
                 ccamlr= FALSE,
                 ccamlr_labels= FALSE,
                 ssru= FALSE,
                 ssru_labels = FALSE,
                 ssmu= FALSE,
                 ssmu_labels= FALSE,
                 rb= FALSE,
                 rb_labels= FALSE,
                 sprfmorb= FALSE,
                 border= TRUE,
                 trim= -45,
                 graticules= FALSE,
                 eez=FALSE,
                 eez_labels=FALSE,
                 mpa=FALSE,
                 mpa_labels=FALSE,
                 domains=FALSE,
                 domains_labels=FALSE,
                 iwc=FALSE,
                 iwc_labels=FALSE,
                 straight=FALSE,
                 fronts=FALSE,
                 fronts_col=c("hotpink","orchid","plum"),
                 rb_col=3,
                 sprfmo_col='grey50',
                 ccamlr_col=2,
                 ssru_col="grey50",
                 ssmu_col="grey70",
                 eez_col="maroon",
                 mpa_col= "yellow",
                 border_col=c("white","black"),
                 graticules_col="grey70",
                 iwc_col="blue",
                 domains_col="magenta") {

    out <- SOmap(bathy_legend = bathy_legend, border = border, trim = trim, graticules = graticules, straight = straight, land = land, fronts = fronts, fronts_col = fronts_col, border_col = border_col, graticules_col = graticules_col)
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

    if (land) {
        if (ccamlr) {
            ## change coastline data
            notANT <- sf::st_as_sf(SOmap_data$continent[SOmap_data$continent$continent != "Antarctica",])
            notANT <- sf::st_buffer(notANT, 0)
            buf <- make_buf(trim, proj = out$projection)
            out$coastline$plotargs$x <- suppressWarnings(sf::st_intersection(buf, notANT)$geometry)
        }
    }

    ## copy management layers into out
    out[mx$plot_sequence] <- mx[mx$plot_sequence]
    out$plot_sequence <- insert_into_sequence(out$plot_sequence, ins = mx$plot_sequence, after = c("bathy", "box", "coastline", "fronts", "graticule"))
    out
}

