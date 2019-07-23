#' Southern Ocean management map layers
#'
#' @description
#' Function for adding management layers to SOmap
#'
#' @param ccamlr logical: if \code{TRUE}, insert the CCAMLR area boundaries.
#' @param ccamlr_labels logical: if \code{TRUE}, add labels for the CCAMLR areas.
#' @param ssru logical: if \code{TRUE}, insert the CCAMLR small scale research unit boundaries.
#' @param ssru_labels logical: if \code{TRUE}, add labels for the CCAMLR small scale research units.
#' @param ssmu logical: if \code{TRUE}, insert the CCAMLR small scale management unit boundaries.
#' @param ssmu_labels logical: if \code{TRUE}, add labels for the CCAMLR small scale management units.
#' @param rb logical: if \code{TRUE}, insert the CCAMLR research block boundaries.
#' @param rb_labels logical: if \code{TRUE}, add labels for the CCAMLR research blocks.
#' @param sprfmorb logical: if \code{TRUE}, insert the SPRFMO toothfish research block boundaries.
#' @param trim numeric: latitude to trim the map to. Set this to -10 for effectively no trim.
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
#' @param iwc_col character: colour for IWC boundaries.
#' @param domains_col character: colour for the CCAMLR planning domains boundaries.
#' @param basemap SOmap or SOmap_auto: optional map object to extract extent, projection, and other information from.
#'
#' @return
#' An object of class "SOmap_management" containing the requested management layers. Printing or plotting this object will display those layers on the current map (note that an \code{SOmap} object needs to have been plotted first)
#'
#' @examples
#' \dontrun{
#'   tfile <- tempfile("SOmap", fileext = ".png")
#'   png(tfile, width=22, height=20, units='cm', res=600)
#'   SOmap(trim = -45)
#'   SOmanagement(ccamlr = TRUE, ccamlr_labels = TRUE, trim=-45)
#'   dev.off()
#'   unlink(tfile)
#'
#'   SOmap(trim = -45)
#'   SOmanagement(ccamlr = TRUE, ccamlr_labels = TRUE, trim = -45)
#' }
#' @export
SOmanagement <- function(ccamlr = FALSE,
                         ccamlr_labels = FALSE,
                         ssru = FALSE,
                         ssru_labels = FALSE,
                         ssmu = FALSE,
                         ssmu_labels = FALSE,
                         rb = FALSE,
                         rb_labels = FALSE,
                         sprfmorb = FALSE,
                         trim = -45,
                         eez = FALSE,
                         eez_labels = FALSE,
                         mpa = FALSE,
                         mpa_labels = FALSE,
                         iwc = FALSE,
                         iwc_labels = FALSE,
                         domains = FALSE,
                         domains_labels = FALSE,
                         rb_col = "green",
                         sprfmo_col = "grey50",
                         ccamlr_col = "red",
                         ssru_col = "grey50",
                         ssmu_col = "grey70",
                         eez_col = "maroon",
                         mpa_col = "yellow",
                         iwc_col = "blue",
                         domains_col = "magenta",basemap) {
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

    if (iwc) {
        out$iwc <- c(SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-170, trim), c(-170, -78.40)), out$projection), col = iwc_col)),
                     SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-120, trim), c(-120, -73.844137)), out$projection), col = iwc_col)),
                     SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-60, -65.168), c(-60, -75.146206)), out$projection), col = iwc_col)),
                     SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(-60, trim), c(-60, -62.4505)), out$projection), col = iwc_col)),
                     SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(0, trim), c(0, -69.596701)), out$projection), col = iwc_col)),
                     SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(70, trim), c(70, -68.366691)), out$projection), col = iwc_col)),
                     SO_plotter(plotfun = "lines", plotargs = list(x = rgdal::project(rbind(c(130, trim), c(130, -66.295027)), out$projection), col = iwc_col)))
        if (iwc_labels) {
            df3 <- data.frame(a = c("Area VI", "Area I", "Area II", "Area III", "Area IV", "Area V"),
                              lon = c(-145, -90, -30, 35, 100, 160),
                              lat=rep(-60, 6))
            sp::coordinates(df3) <- c("lon", "lat")
            raster::projection(df3) <- "+init=epsg:4326"
            lab_pos3 <- sp::spTransform(df3, raster::crs(out$projection))
            out$iwc <- c(out$iwc, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = lab_pos3, labelcol = "a", col = iwc_col, cex = 0.4, pos = 1, offset = -0.05), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "iwc")
    }

    if (rb) {
        out$research_blocks <- SO_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_research_blocks, border = rb_col, add = TRUE), name = "main")
        out$plot_sequence <- c(out$plot_sequence, "research_blocks")
        if (rb_labels) {
            out$research_blocks <- c(out$research_blocks, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_research_blocks, labelcol = "GAR_Short_", col = rb_col, cex = 0.4, pos = 4, offset = 0.3), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "research_blocks")
    }

    if (sprfmorb) {
        sprfmoa <- graticule::graticule(lats = c(-59.9, -57.9), lons = c(-155.3333, -150), proj = out$projection)
        sprfmob <- graticule::graticule(lats = c(-59.0, -60.0),lons = c(-142.1666667, -145.833333), proj = out$projection)
        out$sprfmo_research_blocks <- c(SO_plotter(plotfun = "plot", plotargs = list(x = sprfmoa, col = sprfmo_col, add = TRUE)),
                                        SO_plotter(plotfun = "plot", plotargs = list(x = sprfmob, col = sprfmo_col, add = TRUE)))
        out$plot_sequence <- c(out$plot_sequence, "sprfmo_research_blocks")
    }

    if (ssru) {
        out$ccamlr_ssru <- SO_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_SSRU, border = ssru_col, add = TRUE), name = "main")
        if (ssru_labels) {
            out$ccamlr_ssru <- c(out$ccamlr_ssru, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_SSRU, labelcol = "ShortLabel", col = ssru_col, cex = 0.4, pos = 1, offset = -0.05), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssru")
    }

    if (ssmu) {
        out$ccamlr_ssmu <- SO_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_SSMU, border = ssmu_col, add = TRUE), name = "main")
        if (ssmu_labels) {
            out$ccamlr_ssmu <- c(out$ccamlr_ssmu, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_SSMU, labelcol = "ShortLabel", col = ssmu_col, cex = 0.5, pos = 1, offset = 0.6), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssmu")
    }

    if (ccamlr) {
        out$ccamlr_statistical_areas <- SO_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas, border = ccamlr_col, add = TRUE), name = "main")
        if (ccamlr_labels) {
            if (!missing(basemap)) {
                ## basemap has been provided so we'll just use the same pos and offset for all labels
                out$ccamlr_statistical_areas <- c(out$ccamlr_statistical_areas, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas, labelcol = "LongLabel", col = ccamlr_col, cex = 0.5, pos = 1, offset = -0.3), name = "labels"))
            } else {
                ## some trickery to get 58.4.2 and 48.1 in good positions
                out$ccamlr_statistical_areas <- c(out$ccamlr_statistical_areas,
                    c(SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[!SOmap_data$CCAMLR_statistical_areas$LongLabel %in% c("48.1", "58.4.2"), ], labelcol = "LongLabel", col = ccamlr_col, cex = 0.5, pos = 1, offset = -0.3), name = "labels"),
                      ## these two still need fixing to cope with autocropping
                      SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "58.4.2", ], labelcol = "LongLabel", col = ccamlr_col,cex = 0.5, pos = 3, offset = 0.5), name = "labels"),
                      SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_statistical_areas[SOmap_data$CCAMLR_statistical_areas$LongLabel == "48.1", ], labelcol = "LongLabel", col = ccamlr_col, cex = 0.5, pos = 2, offset = -0.1), name = "labels")))
            }
        }
        out$plot_sequence <- c(out$plot_sequence, "ccamlr_statistical_areas")
    }

    if (eez) {
        out$eez <- SO_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$EEZ, border = eez_col, col = NA, add = TRUE), name = "main")
        if (eez_labels) {
            out$eez <- c(out$eez, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$EEZ, labelcol = "Name", col = eez_col, cex = 0.35, pos = 4, offset = 0.8), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "eez")
    }

    if (mpa) {
        out$mpa <- SO_plotter(plotfun = "plot", plotargs = list(x = SOmap_data$CCAMLR_MPA, border = mpa_col, col = NA, add = TRUE), name = "main")
        if (mpa_labels) {
            out$mpa <- c(out$mpa, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = SOmap_data$CCAMLR_MPA, labelcol = "ShortLabel", col = mpa_col, cex = 0.35, pos = 1, offset =0.2), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "mpa")
    }

    if (domains) {
        this <- SOmap_data$CCAMLR_planning_domains
        out$ccamlr_planning_domains <- SO_plotter(plotfun = "plot", plotargs = list(x = this, border = domains_col, col = NA, add = TRUE), name = "main")
        if (domains_labels) {
            this$labs <- c("Domain  8", "Domain  9", "", "", "Domain  3", "", "Domain  4", "Domain  5", "Domain  6")
            this$labs1 <- c("", "", "Domain  1", "", "", "", "", "", "")
            this$labs2 <- c("", "", "", "", "", "Domain  2", "", "", "")
            this$labs7 <- c("", "", "", "Domain  7", "", "", "", "", "")
            out$ccamlr_planning_domains <- c(out$ccamlr_planning_domains,
                ## surely this would be better done by plotting subsets, not plotting everything each time with selectively blank labels - BR
                                             SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs", col = domains_col, cex = 0.7, pos = 3, offset = 0.05), name = "labels"),
                                             SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs1", col = domains_col, cex = 0.7, pos = 1, offset = 3.0), name = "labels"),
                                             SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs2", col = domains_col, cex = 0.7, pos = 3, offset = 0.5), name = "labels"),
                                             SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this, labelcol = "labs7", col = domains_col, cex = 0.7, pos = 4, offset = 0.9), name = "labels"))
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
