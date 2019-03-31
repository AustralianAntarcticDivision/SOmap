#' Southern Ocean management map layers
#'
#' @description
#' Function for adding management layers to SOmap
#'
#' @param CCAMLR
#' Insert the CCAMLR boundaries.
#' @param CCAMLRlab
#' Insert the CCAMLR labels.
#' @param SSRU
#' Insert the CCAMLR small scale research unit boundaries.
#' @param SSRUlab
#' Insert the CCAMLR small scale research unit labels.
#' @param SSMU
#' Insert the CCAMLR small scale management unit boundaries.
#' @param SSMUlab
#' Insert the CCAMLR small scale management unit labels.
#' @param RB
#' Insert the CCAMLR research block boundaries.
#' @param RBlab
#' Insert the CCAMLR research block labels.
#' @param SPRFMORB
#' Insert the SPRFMO toothfish research block boundaries.
#' @param Trim
#' Longitude to trim map to.
#' @param EEZ
#' Insert Exclusive Economic Zones.
#' @param EEZlab
#' Insert Exclusive Economic Zone labels.
#' @param MPA
#' Insert CCAMLR Marine Protected Areas.
#' @param MPAlab
#' Insert CCAMLR Marine Protected Area labels.
#' @param Domains
#' Insert CCAMLR Marine Protected Area planning Domains.
#' @param Domainslab
#' Insert CCAMLR Marine Protected Area planning Domains labels.
#' @param IWC
#' Insert International Whaling Commission boundaries.
#' @param IWClab
#' Insert International Whaling Commission labels.
#' @param rbcol
#' Color for CCAMLR research blocks.
#' @param sprfmocol
#' Color for SPRFMO toothfish research blocks
#' @param ccamlrcol
#' Color for CCAMLR boundaries
#' @param ssrucol
#' Color for CCAMLR small scale research units.
#' @param ssmucol
#' Color for CCAMLR small scale management units.
#' @param eezcol
#' Color for Exclusive Economic Zone boundaries; Default is maroon.
#' @param mpacol
#' Color for CCAMLR Marine Protected Areas; Default is yellow.
#' @param iwccol
#' Color for IWC boundaries; Default is blue.
#' @param domcol
#' Color for the Domain boundaries. Default is magenta.
#' @param x
#' Optional SOmap or SOautomap object.
#'
#' @return
#' Produces at the very base a round bathymetry map of the southern hemisphere.
#'
#' @examples
#' \dontrun{
#' tfile <- tempfile("SOmap", fileext = ".png")
#' png(tfile, width=22, height=20, units='cm', res=600)
#' SOmap(Trim=-45)
#' SOmanagement(CCAMLR=T, CCAMLRlab=T, Trim=-45)
#' dev.off()
#' unlink(tfile)
#'   SOmap(Trim = -45)
#'   SOmanagement(CCAMLR = TRUE, CCAMLRlab = TRUE, Trim = -45)
#' }
#' @export
SOmanagement <- function(CCAMLR = FALSE,
                         CCAMLRlab = FALSE,
                         SSRU = FALSE,
                         SSRUlab = FALSE,
                         SSMU = FALSE,
                         SSMUlab = FALSE,
                         RB = FALSE,
                         RBlab = FALSE,
                         SPRFMORB = FALSE,
                         Trim = -45,
                         EEZ = FALSE,
                         EEZlab = FALSE,
                         MPA = FALSE,
                         MPAlab = FALSE,
                         IWC = FALSE,
                         IWClab = FALSE,
                         Domains = FALSE,
                         Domainslab = FALSE,
                         rbcol = "green",
                         sprfmocol = "grey50",
                         ccamlrcol = "red",
                         ssrucol = "grey50",
                         ssmucol = "grey70",
                         eezcol = "maroon",
                         mpacol = "yellow",
                         iwccol = "blue",
                         domcol = "magenta",basemap) {
    ## data
    SOmap_data <- NULL
    Bathy <- NULL
    data("SOmap_data", package = "SOmap", envir = environment())
    data("Bathy", package = "SOmap", envir = environment())

    ## CCAMLR Labels
    cclabs<-c("88.3", "48.4", "88.2", "48.2", "48.3", "58.4.3a", "58.4.3b", "58.5.2", "48.5", "48.6", "58.4.1", "88.1", "58.4.4a", "58.7", "58.6", "58.5.1", "58.4.4b")

    if(!missing(basemap)){out <- list(projection = basemap$projection, plot_sequence = NULL)}else(
    out <- list(projection = raster::projection(Bathy), plot_sequence = NULL))

    if (IWC) {
        out$iwc <- list(as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-170, Trim), c(-170, -78.40)), out$projection), col = iwccol)),
                        as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-120, Trim), c(-120, -73.844137)), out$projection), col = iwccol)),
                        as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-60, -65.168), c(-60, -75.146206)), out$projection), col = iwccol)),
                        as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-60, Trim), c(-60, -62.4505)), out$projection), col = iwccol)),
                        as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(0, Trim), c(0, -69.596701)), out$projection), col = iwccol)),
                        as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(70, Trim), c(70, -68.366691)), out$projection), col = iwccol)),
                        as_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(130, Trim), c(130, -66.295027)), out$projection), col = iwccol)))
        if (IWClab) {
            df3 <- data.frame(a = c("Area VI", "Area I", "Area II", "Area III", "Area IV", "Area V"),
                              lon = c(-145, -90, -30, 35, 100, 160),
                              lat=rep(-60, 6))
            sp::coordinates(df3) <- c("lon", "lat")
            raster::projection(df3) <- "+init=epsg:4326"
            lab_pos3 <- sp::spTransform(df3, raster::crs(out$projection))
            out$iwc$labels <- as_plotter(plotfun = "text", plotargs = list(x = lab_pos3, labels = lab_pos3$a, col = iwccol, cex = 0.4, pos = 1, offset = -0.05))
        }
        out$plot_sequence <- c(out$plot_sequence, "iwc")
    }

    if (RB) {
        #out$research_blocks <- list(data = SOmap_data$CCAMLR_research_blocks, border = rbcol)
        #if (RBlab) {
        #    out$research_blocks$labels <- list(data = SOmap_data$CCAMLR_research_blocks, labels = SOmap_data$CCAMLR_research_blocks$GAR_Short_, col = rbcol, cex = 0.4, pos = 4, offset = 0.3)
        #}
        out$research_blocks <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_research_blocks, border = rbcol, add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "research_blocks")
        if (RBlab) {
            out$research_blocks$labels <- as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_research_blocks, labels = SOmap_data$CCAMLR_research_blocks$GAR_Short_, col = rbcol, cex = 0.4, pos = 4, offset = 0.3))
        }
        out$plot_sequence <- c(out$plot_sequence, "research_blocks")
    }

    if (SPRFMORB) {
        sprfmoa <- graticule::graticule(lats = c(-59.9, -57.9), lons = c(-155.3333, -150), proj = out$projection)
        sprfmob <- graticule::graticule(lats = c(-59.0, -60.0),lons = c(-142.1666667, -145.833333), proj = out$projection)
        ##out$sprfmo_research_blocks <- list(data = list(sprfmoa, sprfmob), col = sprfmocol)
        out$sprfmo_research_blocks <- list(as_plotter(plotfun = "plot", plotargs = list(x = sprfmoa, col = sprfmocol, add = TRUE)),
                                           as_plotter(plotfun = "plot", plotargs = list(x = sprfmob, col = sprfmocol, add = TRUE)))
        out$plot_sequence <- c(out$plot_sequence, "sprfmo_research_blocks")
    }

    if (SSRU) {
        ##out$ccamlr_ssru <- list(data = SOmap_data$CCAMLR_SSRU, border = ssrucol)
        ##if (SSRUlab) {
        ##    out$ccamlr_ssru$labels <- list(data = SOmap_data$CCAMLR_SSRU, labels = SOmap_data$CCAMLR_SSRU$ShortLabel, col = ssrucol, cex = 0.4, pos = 1, offset = -0.05)
        ##}
        out$ccamlr_ssru <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_SSRU, border = ssrucol, add = TRUE))
        if (SSRUlab) {
            out$ccamlr_ssru$labels <- as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_SSRU, labels = SOmap_data$CCAMLR_SSRU$ShortLabel, col = ssrucol, cex = 0.4, pos = 1, offset = -0.05))
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssru")
    }

    if (SSMU) {
        ##out$ccamlr_ssmu <- list(data = SOmap_data$CCAMLR_SSMU, border = ssmucol)
        out$ccamlr_ssmu <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_SSMU, border = ssmucol, add = TRUE))
        if (SSMUlab) {
            ##out$ccamlr_ssmu$labels <- list(data = SOmap_data$CCAMLR_SSMU, labels = SOmap_data$CCAMLR_SSMU$ShortLabel, col = ssmucol, cex = 0.5, pos = 1, offset = 0.6)
            out$ccamlr_ssru$labels <- as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_SSMU, labels = SOmap_data$CCAMLR_SSMU$ShortLabel, col = ssmucol, cex = 0.5, pos = 1, offset = 0.6))
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssmu")
    }

    if (CCAMLR) {
        ##out$ccamlr_statistical_areas <- list(data = SOmap_data$CCAMLR_statistical_areas, border = ccamlrcol)
        out$ccamlr_statistical_areas <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas, border = ccamlrcol, add = TRUE))
        if (CCAMLRlab) {
            ##out$ccamlr_statistical_areas$labels <- list(
            ##                                            list(data = SOmap_data$CCAMLR_statistical_areas[!SOmap_data$CCAMLR_statistical_areas$LongLabel %in% c("48.1", "58.4.2"), ], labels = cclabs, col = ccamlrcol, cex = 0.5, pos = 1, offset = -0.3),
            ##                                            list(data = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "58.4.2", ], labels = "58.4.2", col = ccamlrcol,cex = 0.5, pos = 3, offset = 0.5),
            ##                                            list(data = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "48.1", ], labels = "48.1", col = ccamlrcol, cex = 0.5, pos = 2, offset = -0.1))
            out$ccamlr_statistical_areas$labels <- list(
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[!SOmap_data$CCAMLR_statistical_areas$LongLabel %in% c("48.1", "58.4.2"), ], labels = cclabs, col = ccamlrcol, cex = 0.5, pos = 1, offset = -0.3)),
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "58.4.2", ], labels = "58.4.2", col = ccamlrcol,cex = 0.5, pos = 3, offset = 0.5)),
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "48.1", ], labels = "48.1", col = ccamlrcol, cex = 0.5, pos = 2, offset = -0.1))
            )
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_statistical_areas")
    }

    if (EEZ) {
        ##out$eez <- list(data = SOmap_data$EEZ, border = eezcol)
        out$eez <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$EEZ, border = eezcol, col = NA, add = TRUE))
        if (EEZlab) {
            ##out$eez$labels <- list(data = SOmap_data$EEZ, labels = SOmap_data$EEZ$Name, col = eezcol, cex = 0.35, pos = 4, offset = 0.8)
            out$eez$labels <- as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$EEZ, labels = SOmap_data$EEZ$Name, col = eezcol, cex = 0.35, pos = 4, offset = 0.8))
        }
        out$plot_sequence <- c(out$plot_sequence, "eez")
    }

    if (MPA) {
        #out$mpa <- list(data = SOmap_data$CCAMLR_MPA, border = mpacol)
        out$mpa <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_MPA, border = mpacol, col = NA, add = TRUE))
        if (MPAlab) {
            ##out$mpa$labels <- list(data = SOmap_data$CCAMLR_MPA, labels = SOmap_data$CCAMLR_MPA$ShortLabel, col = mpacol, cex = 0.35, pos = 1, offset =0.2)
            out$mpa$labels <- as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_MPA, labels = SOmap_data$CCAMLR_MPA$ShortLabel, col = mpacol, cex = 0.35, pos = 1, offset =0.2))
        }
        out$plot_sequence <- c(out$plot_sequence, "mpa")
    }

    if (Domains) {
        ##out$ccamlr_planning_domains <- list(data = SOmap_data$CCAMLR_planning_domains, border = domcol)
        this <- SOmap_data$CCAMLR_planning_domains
        out$ccamlr_planning_domains <- as_plotter(plotfun = "plot", plotargs = list(x = this, border = domcol, col = NA, add = TRUE))
        if (Domainslab) {
            labs <- c("Domain  8", "Domain  9", "", "", "Domain  3", "", "Domain  4", "Domain  5", "Domain  6")
            labs1 <- c("", "", "Domain  1", "", "", "", "", "", "")
            labs2 <- c("", "", "", "", "", "Domain  2", "", "", "")
            labs7 <- c("", "", "", "Domain  7", "", "", "", "", "")
            ##out$ccamlr_planning_domains$labels <- list(
            ##                                           list(data = SOmap_data$CCAMLR_planning_domains, labels = labs, col = domcol, cex = 0.7, pos = 3, offset = 0.05),
            ##                                           list(data = SOmap_data$CCAMLR_planning_domains, labels = labs1, col = domcol, cex = 0.7, pos = 1, offset = 3.0),
            ##                                           list(data = SOmap_data$CCAMLR_planning_domains, labels = labs2, col = domcol, cex = 0.7, pos = 3, offset = 0.5),
            ##                                           list(data = SOmap_data$CCAMLR_planning_domains, labels = labs7, col = domcol, cex = 0.7, pos = 4, offset = 0.9)
            ##                                           )
            out$ccamlr_planning_domains$labels <- list(
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_planning_domains, labels = labs, col = domcol, cex = 0.7, pos = 3, offset = 0.05)),
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_planning_domains, labels = labs1, col = domcol, cex = 0.7, pos = 1, offset = 3.0)),
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_planning_domains, labels = labs2, col = domcol, cex = 0.7, pos = 3, offset = 0.5)),
                as_plotter(plotfun = function(x, ...) text(sp::coordinates(x), ...), plotargs = list(x = SOmap_data$CCAMLR_planning_domains, labels = labs7, col = domcol, cex = 0.7, pos = 4, offset = 0.9))
            )
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_planning_domains")
    }

    if (!missing(basemap)) {
        for (i in setdiff(names(out), c("projection", "plot_sequence"))) {
            ## out[[i]] is either a list containing plotfun and plotargs, or a list of such lists, in which case we need to iterate over its elements
            ## should perhaps enforce the latter format for all components of the object, to simplify things, but for now cope with both:
            if ("plotargs" %in% names(out[[i]])) {
                out[[i]]$plotargs$x <- SOauto_crop(layer = out[[i]]$plotargs$x, x = basemap)
            } else {
                for (subi in seq_along(out[[i]])) {
                    if ("plotargs" %in% names(out[[i]][[subi]])) {
                        out[[i]][[subi]]$plotargs$x <- SOauto_crop(layer = out[[i]][[subi]]$plotargs$x, x = basemap)
                    }
                }
            }
        }
    }
    structure(out, class = "SOmap_management")
}

#' @method plot SOmap_management
#' @export
plot.SOmap_management <- function (x, y, ...) {
    print(x)
    invisible()
}

#' @method print SOmap_management
#' @export
print.SOmap_management <- function(x, ...) {
    ## print the management layers
    ## expects that an existing SOmap has already been plotted
    op <- par(mar = rep(0.01, 4), oma= rep(0.0, 4), mai= rep(0.0, 4))
    on.exit(par(op))
    ## plot each layer
    plot_all(x)
    invisible(x)
}
