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
#'   ## map with non-default latitudinal extent
#'   SOmap(trim = -55)
#'   ## either provide the same extent via 'trim'
#'   SOmanagement(ccamlr = TRUE, ccamlr_labels = TRUE, trim = -55)
#'
#'   ## or equivalently, pass the basemap to SOmanagement
#'   x <- SOmap(trim = -55)
#'   plot(x)
#'   SOmanagement(ccamlr = TRUE, ccamlr_labels = TRUE, basemap = x)
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
        if (missing(trim) && !is.null(basemap$trim)) trim <- basemap$trim
    } else {
        out <- list(projection = raster::projection(Bathy), plot_sequence = NULL)
    }
    if (!missing(basemap) && inherits(basemap, "SOmap_auto")) {
        ## SOmap_auto object, which can have arbitrary projection and extent
        ## but the bathy component of an SOmap_auto object matches the visible map extent, so
        ##  we can just use SOauto_crop (and ignore the geometry_only parm, which only matters
        ##  for the version of apply_buf used with SOmap objects)
        apply_buf <- function(thing, geometry_only) {
            SOauto_crop(thing, x = basemap)
        }
    } else {
        ## SOmap object
        ## the bathy data does not necessarily match the visible extent of the map, so we
        ##  need to make an appropriate buffer
        croptarget <- make_buf(trim, proj = out$projection)
        ## local convenience functions to crop/buffer objects to our projection and extent
        apply_buf <- function(thing, geometry_only = TRUE) {
            tryCatch({
                if (inherits(thing, "Spatial")) {
                    thing <- sf::st_as_sf(thing)
                    thing <- sf::st_buffer(thing, 0)
                    thing <- sf::st_set_crs(thing, sf::st_crs(croptarget))
                }
                out <- suppressWarnings(sf::st_intersection(croptarget, thing))
                if (geometry_only) out$geometry else out
            }, error = function(e) NULL)
        }
    }
    centroids <- function(z) suppressWarnings(sf::st_centroid(sf::st_as_sf(z)))

    if (iwc) {
        ## TODO: cope with trimmed or otherwise non-standard extents
        ## also TODO: do these areas have northern boundary lines?
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
            raster::projection(df3) <- proj_longlat()
            lab_pos3 <- sp::spTransform(df3, raster::crs(out$projection))
            out$iwc <- c(out$iwc, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = lab_pos3, labelcol = "a", col = iwc_col, cex = 0.4, pos = 1, offset = -0.05), name = "labels"))
        }
        out$plot_sequence <- c(out$plot_sequence, "iwc")
    }

    if (rb) {
        thisdat <- apply_buf(SOmap_data$CCAMLR_research_blocks)
        if (!is.null(thisdat)) {
            out$research_blocks <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = rb_col, add = TRUE), name = "main")
            if (rb_labels) {
                labdat <- apply_buf(centroids(SOmap_data$CCAMLR_research_blocks), geometry_only = FALSE)
                if (!is.null(labdat)) {
                    out$research_blocks <- c(out$research_blocks, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "GAR_Short_", col = rb_col, cex = 0.4, pos = 4, offset = 0.3), name = "labels"))
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "research_blocks")
        }
    }

    if (sprfmorb) {
        ## TODO: cope with trimmed or otherwise non-standard extents
        sprfmoa <- graticule::graticule(lats = c(-59.9, -57.9), lons = c(-155.3333, -150), proj = out$projection)
        sprfmob <- graticule::graticule(lats = c(-59.0, -60.0), lons = c(-142.1666667, -145.833333), proj = out$projection)
        out$sprfmo_research_blocks <- c(SO_plotter(plotfun = "plot", plotargs = list(x = sprfmoa, col = sprfmo_col, add = TRUE)),
                                        SO_plotter(plotfun = "plot", plotargs = list(x = sprfmob, col = sprfmo_col, add = TRUE)))
        out$plot_sequence <- c(out$plot_sequence, "sprfmo_research_blocks")
    }

    if (ssru) {
        thisdat <- apply_buf(SOmap_data$CCAMLR_SSRU)
        if (!is.null(thisdat)) {
            out$ccamlr_ssru <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = ssru_col, add = TRUE), name = "main")
            if (ssru_labels) {
                labdat <- apply_buf(centroids(SOmap_data$CCAMLR_SSRU), geometry_only = FALSE)
                if (!is.null(labdat)) {
                    out$ccamlr_ssru <- c(out$ccamlr_ssru, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "ShortLabel", col = ssru_col, cex = 0.4, pos = 1, offset = -0.05), name = "labels"))
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssru")
        }
    }

    if (ssmu) {
        thisdat <- apply_buf(SOmap_data$CCAMLR_SSMU)
        if (!is.null(thisdat)) {
            out$ccamlr_ssmu <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = ssmu_col, add = TRUE), name = "main")
            if (ssmu_labels) {
                labdat <- apply_buf(centroids(SOmap_data$CCAMLR_SSMU), geometry_only = FALSE)
                if (!is.null(labdat)) {
                    out$ccamlr_ssmu <- c(out$ccamlr_ssmu, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "ShortLabel", col = ssmu_col, cex = 0.5, pos = 1, offset = 0.6), name = "labels"))
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "ccamlr_ssmu")
        }
    }

    if (ccamlr) {
        thisdat <- apply_buf(SOmap_data$CCAMLR_statistical_areas)
        if (!is.null(thisdat)) {
            out$ccamlr_statistical_areas <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = ccamlr_col, add = TRUE), name = "main")
            if (ccamlr_labels) {
                sadata <- SOmap_data$CCAMLR_statistical_areas
                if (!missing(basemap)) {
                    ## basemap has been provided so we'll just use the same pos and offset for all labels
                    labdat <- apply_buf(centroids(sadata), geometry_only = FALSE)
                    if (!is.null(labdat)) {
                        out$ccamlr_statistical_areas <- c(out$ccamlr_statistical_areas, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "LongLabel", col = ccamlr_col, cex = 0.5, pos = 1, offset = -0.3), name = "labels"))
                    }
                } else {
                    ## some trickery to get 58.4.2 and 48.1 in good positions
                    labdat <- apply_buf(centroids(sadata[!sadata$LongLabel %in% c("48.1", "58.4.2"), ]), geometry_only = FALSE)
                    if (!is.null(labdat)) {
                        out$ccamlr_statistical_areas <- c(out$ccamlr_statistical_areas, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "LongLabel", col = ccamlr_col, cex = 0.5, pos = 1, offset = -0.3), name = "labels"))
                    }
                    labdat <- apply_buf(centroids(sadata[sadata$LongLabel == "58.4.2", ]), geometry_only = FALSE)
                    if (!is.null(labdat)) {
                        out$ccamlr_statistical_areas <- c(out$ccamlr_statistical_areas, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "LongLabel", col = ccamlr_col,cex = 0.5, pos = 3, offset = 0.5), name = "labels"))
                    }
                    labdat <- apply_buf(centroids(sadata[sadata$LongLabel == "48.1", ]), geometry_only = FALSE)
                    if (!is.null(labdat)) {
                        out$ccamlr_statistical_areas <- c(out$ccamlr_statistical_areas, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "LongLabel", col = ccamlr_col, cex = 0.5, pos = 2, offset = -0.1), name = "labels"))
                    }
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "ccamlr_statistical_areas")
        }
    }

    if (eez) {
        thisdat <- apply_buf(SOmap_data$EEZ)
        if (!is.null(thisdat)) {
            out$eez <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = eez_col, col = NA, add = TRUE), name = "main")
            if (eez_labels) {
                labdat <- apply_buf(centroids(SOmap_data$EEZ), geometry_only = FALSE)
                if (!is.null(labdat)) {
                    out$eez <- c(out$eez, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "Name", col = eez_col, cex = 0.35, pos = 4, offset = 0.8), name = "labels"))
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "eez")
        }
    }

    if (mpa) {
        thisdat <- apply_buf(SOmap_data$CCAMLR_MPA)
        if (!is.null(thisdat)) {
            out$mpa <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = mpa_col, col = NA, add = TRUE), name = "main")
            if (mpa_labels) {
                labdat <- apply_buf(centroids(SOmap_data$CCAMLR_MPA), geometry_only = FALSE)
                if (!is.null(labdat)) {
                    out$mpa <- c(out$mpa, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = labdat, labelcol = "ShortLabel", col = mpa_col, cex = 0.35, pos = 1, offset =0.2), name = "labels"))
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "mpa")
        }
    }

    if (domains) {
        thisdat <- apply_buf(SOmap_data$CCAMLR_planning_domains)
        if (!is.null(thisdat)) {
            out$ccamlr_planning_domains <- SO_plotter(plotfun = "plot", plotargs = list(x = thisdat, border = domains_col, col = NA, add = TRUE), name = "main")
            if (domains_labels) {
                this <- apply_buf(centroids(SOmap_data$CCAMLR_planning_domains), geometry_only = FALSE)
                if (!is.null(this) && nrow(this) > 0) {
                    this$labs <- paste0("Domain  ", this$Name)
                    ## plot subsets, adjusting layout parms for each
                    idx <- this$Name %in% c("8", "9", "3", "4", "5", "6")
                    if (any(idx)) {
                        out$ccamlr_planning_domains <- c(out$ccamlr_planning_domains, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this[idx, ], labelcol = "labs", col = domains_col, cex = 0.7, pos = 3, offset = 0.05), name = "labels"))
                    }
                    idx <- this$Name %in% c("1")
                    if (any(idx)) {
                        out$ccamlr_planning_domains <- c(out$ccamlr_planning_domains, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this[idx, ], labelcol = "labs1", col = domains_col, cex = 0.7, pos = 1, offset = 3.0), name = "labels"))
                    }
                    idx <- this$Name %in% c("2")
                    if (any(idx)) {
                        out$ccamlr_planning_domains <- c(out$ccamlr_planning_domains, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this[idx, ], labelcol = "labs2", col = domains_col, cex = 0.7, pos = 3, offset = 0.5), name = "labels"))
                    }
                    idx <- this$Name %in% c("7")
                    if (any(idx)) {
                        out$ccamlr_planning_domains <- c(out$ccamlr_planning_domains, SO_plotter(plotfun = "SOmap_text", plotargs = list(x = this[idx, ], labelcol = "labs7", col = domains_col, cex = 0.7, pos = 4, offset = 0.9), name = "labels"))
                    }
                }
            }
            out$plot_sequence <- c(out$plot_sequence, "ccamlr_planning_domains")
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
#' @param x data.frame, Spatial data.frame, or sfc: data to pass to \code{text}
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
    omg <- function(x, ...) {
        if (inherits(x, "Spatial")) {
            text(sp::coordinates(x), ...)
        } else if (inherits(x, c("sf", "sfc"))) {
            text(sf::st_coordinates(x), ...)
        } else {
            text(x, ...)
        }
    }
    if (!missing(labelcol) && labelcol %in% names(x)) {
        if (!is.null(x) && !is.null(x[[labelcol]]) && length(x[[labelcol]]) > 0) {
            omg(x, labels = x[[labelcol]], ...)
        }
    } else {
        omg(x, ...)
    }
}
