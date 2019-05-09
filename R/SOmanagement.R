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
#' @param basemap
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

    if (!missing(basemap)) {
        out <- list(projection = basemap$projection, plot_sequence = NULL)
    } else {
        out <- list(projection = raster::projection(Bathy), plot_sequence = NULL)
    }

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
            out$iwc$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = lab_pos3, labelcol = "a", col = iwccol, cex = 0.4, pos = 1, offset = -0.05))
        }
        out$plot_sequence <- c(out$plot_sequence, "iwc")
    }

    if (RB) {
        out$research_blocks <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_research_blocks, border = rbcol, add = TRUE))
        out$plot_sequence <- c(out$plot_sequence, "research_blocks")
        if (RBlab) {
            out$research_blocks$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_research_blocks, labelcol = "GAR_Short_", col = rbcol, cex = 0.4, pos = 4, offset = 0.3))
        }
        out$plot_sequence <- c(out$plot_sequence, "research_blocks")
    }

    if (SPRFMORB) {
        sprfmoa <- graticule::graticule(lats = c(-59.9, -57.9), lons = c(-155.3333, -150), proj = out$projection)
        sprfmob <- graticule::graticule(lats = c(-59.0, -60.0),lons = c(-142.1666667, -145.833333), proj = out$projection)
        out$sprfmo_research_blocks <- list(as_plotter(plotfun = "plot", plotargs = list(x = sprfmoa, col = sprfmocol, add = TRUE)),
                                           as_plotter(plotfun = "plot", plotargs = list(x = sprfmob, col = sprfmocol, add = TRUE)))
        out$plot_sequence <- c(out$plot_sequence, "sprfmo_research_blocks")
    }

    if (SSRU) {
        out$ccamlr_ssru <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_SSRU, border = ssrucol, add = TRUE))
        if (SSRUlab) {
            out$ccamlr_ssru$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_SSRU, labelcol = "ShortLabel", col = ssrucol, cex = 0.4, pos = 1, offset = -0.05))
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssru")
    }

    if (SSMU) {
        out$ccamlr_ssmu <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_SSMU, border = ssmucol, add = TRUE))
        if (SSMUlab) {
            out$ccamlr_ssru$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_SSMU, labelcol = "ShortLabel", col = ssmucol, cex = 0.5, pos = 1, offset = 0.6))
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssmu")
    }

    if (CCAMLR) {
        out$ccamlr_statistical_areas <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas, border = ccamlrcol, add = TRUE))
        if (CCAMLRlab) {
            if (!missing(basemap)) {
                ## basemap has been provided so we'll just use the same pos and offset for all labels
                out$ccamlr_statistical_areas$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas, labelcol = "LongLabel", col = ccamlrcol, cex = 0.5, pos = 1, offset = -0.3))
            } else {
                ## some trickery to get 58.4.2 and 48.1 in good positions
                out$ccamlr_statistical_areas$labels <- list(
                    as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[!SOmap_data$CCAMLR_statistical_areas$LongLabel %in% c("48.1", "58.4.2"), ], labelcol = "LongLabel", col = ccamlrcol, cex = 0.5, pos = 1, offset = -0.3)),
                    ## these two still need fixing to cope with autocropping
                    as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "58.4.2", ], labelcol = "LongLabel", col = ccamlrcol,cex = 0.5, pos = 3, offset = 0.5)),
                    as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "48.1", ], labelcol = "LongLabel", col = ccamlrcol, cex = 0.5, pos = 2, offset = -0.1))
                )
            }
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_statistical_areas")
    }

    if (EEZ) {
        out$eez <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$EEZ, border = eezcol, col = NA, add = TRUE))
        if (EEZlab) {
            out$eez$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$EEZ, labelcol = "Name", col = eezcol, cex = 0.35, pos = 4, offset = 0.8))
        }
        out$plot_sequence <- c(out$plot_sequence, "eez")
    }

    if (MPA) {
        out$mpa <- as_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_MPA, border = mpacol, col = NA, add = TRUE))
        if (MPAlab) {
            out$mpa$labels <- as_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_MPA, labelcol = "ShortLabel", col = mpacol, cex = 0.35, pos = 1, offset =0.2))
        }
        out$plot_sequence <- c(out$plot_sequence, "mpa")
    }

    if (Domains) {
        this <- SOmap_data$CCAMLR_planning_domains
        out$ccamlr_planning_domains <- as_plotter(plotfun = "plot", plotargs = list(x = this, border = domcol, col = NA, add = TRUE))
        if (Domainslab) {
            this$labs <- c("Domain  8", "Domain  9", "", "", "Domain  3", "", "Domain  4", "Domain  5", "Domain  6")
            this$labs1 <- c("", "", "Domain  1", "", "", "", "", "", "")
            this$labs2 <- c("", "", "", "", "", "Domain  2", "", "", "")
            this$labs7 <- c("", "", "", "Domain  7", "", "", "", "", "")
            out$ccamlr_planning_domains$labels <- list(
                ## surely this would be better done by plotting subsets, not plotting everything each time with selectively blank labels - BR
                as_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs", col = domcol, cex = 0.7, pos = 3, offset = 0.05)),
                as_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs1", col = domcol, cex = 0.7, pos = 1, offset = 3.0)),
                as_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs2", col = domcol, cex = 0.7, pos = 3, offset = 0.5)),
                as_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs7", col = domcol, cex = 0.7, pos = 4, offset = 0.9))
            )
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_planning_domains")
    }


    if (!missing(basemap)) {
        ## oh my this is horrible code
        ## first, a helper function to apply the autocropping
        ## this will cope with z being the actual SO_plotter object, or a list thereof
        do_autocrop <- function(z, basemap) {
            if (inherits(z, "SO_plotter") && "plotargs" %in% names(z)) {
                z$plotargs$x <- SOauto_crop(layer = z$plotargs$x, x = basemap)
                ## the SO_plotter object can also have a labels component
                if ("labels" %in% names(z)) {
                    z$labels <- do_autocrop(z$labels, basemap = basemap)
                }
            } else if (is.list(z)) {
                ## iterate over the list, calling this do_autocrop function on each element in turn
                for (subi in seq_along(z)) {
                    z[[subi]] <- do_autocrop(z[[subi]], basemap = basemap)

                }
            }
            z
        }
        for (i in setdiff(names(out), c("projection", "plot_sequence"))) {
            ## out[[i]] is either SO_plotter object (a list containing plotfun and plotargs), or a list of such objects, in which case we need to iterate over its elements
            ## should perhaps enforce the latter format for all components of the object, to simplify things, but for now cope with both
            out[[i]] <- do_autocrop(out[[i]], basemap = basemap)
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


#' Helper function for labels
#' This is basically a thin wrapper around \code{text}, that passes \code{x[[labelcol]]} to \code{text} as the \code{labels} parameter
#'
#' @param x data.frame or Spatial data.frame: data to pass to \code{text}
#' @param labelcol string: name of the column in \code{x} to use for text labels
#' @param ... other plot arguements
#'
#' @return as for \code{text}
#'
#' @seealso \code{\link{text}}
#'
#' @export
SOmap_text <- function(x, labelcol, ...) {
    ## labelcol defines the column to use for the label text
    omg <- function(x, ...) if (inherits(x, "Spatial")) text(sp::coordinates(x), ...) else text(x, ...)
    if (!missing(labelcol) && labelcol %in% names(x)) {
        omg(x, labels = x[[labelcol]], ...)
    } else {
        omg(x, ...)
    }
}
