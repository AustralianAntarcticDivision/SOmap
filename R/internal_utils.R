## internal function to construct buffer to use for trimming
## e.g.
##  buf <- make_buf(trim+border_width, proj = raster::projection(Bathy))
##  suppressWarnings(sf::st_intersection(buf, some_object))
make_buf <- function(trim_to_latitude, proj) {
    bufrad <- 90-abs(trim_to_latitude) ## radius in degrees latitude
    tmp <- data.frame(lon = 0, lat = -90+bufrad)
    sp::coordinates(tmp) <- c("lon", "lat")
    raster::projection(tmp) <- proj_longlat()
    tmp <- sp::spTransform(tmp, sp::CRS(proj))
    sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_buffer(sf::st_point(cbind(0, 0)), sp::coordinates(tmp)[2])), crs = proj)
}

insert_into_sequence <- function(sequence, ins, after) {
    ## insert ins into sequence, so that it appears directly after the last element in after
    idx <- tail(which(sequence %in% after), 1)
    if (length(idx) < 1) {
        c(sequence, ins)
    } else {
        c(sequence[seq_len(idx)], ins, sequence[-seq_len(idx)])
    }
}


## internal plotting routine, called by SOmap and SOmanagement
## iterate through the object's plot_sequence vector, running the plotfun with plotargs for each
plot_all <- function(x) {
    assert_that(inherits(x, c("SOmap_management", "SOmap", "SOthing", "SOmap_legend")))
    ## interate through each plottable element in turn
    for (toplot in intersect(x$plot_sequence, names(x))) {
        allpf <- x[[toplot]] ## all the stuff to plot for this element
        ## either a SO_plotter object, or a list thereof
        ## if it's just one, put it in a list
        if (inherits(allpf, "SO_plotter")) allpf <- list(allpf)
        if (!all(vapply(allpf, inherits, "SO_plotter", FUN.VALUE = TRUE))) {
            warning("plotting behaviour for '", toplot, "' should be specified by an SO_plotter object or list of such objects, ignoring")
            next
        }
        for (thispf in allpf) {
            thisfun <- thispf$plotfun
            ##if (is.character(thisfun)) thisfun <- parse(text = thisfun)
            ##eval(thisfun, envir = x[[toplot]]$plotenv)
            this_plotargs <- thispf$plotargs
            if (is.character(thisfun)) do.call(eval(parse(text = thisfun)), this_plotargs) else do.call(thisfun, this_plotargs)
        }
        if (!is.null(thispf$labels)) {
            ## this should not be needed now: all label stuff should now be in the main list of plotfuns and be handled above
            ## but leave this here for the time being
            allpf <- thispf$labels ## all the stuff to plot for this element
            if (is.list(allpf) && length(allpf) > 1 && is.null(names(allpf))) {
                ## a list of plotfun/args to iterate over
            } else {
                allpf <- list(allpf)
            }
            for (thispf in allpf) {
                thisfun <- thispf$plotfun
                this_plotargs <- thispf$plotargs
                if (is.character(thisfun)) do.call(eval(parse(text = thisfun)), this_plotargs) else do.call(thisfun, this_plotargs)
            }
        }
    }
    invisible(NULL)
}

stars_to_raster <- function (x, ...) {
  stopifnot(inherits(x, "stars"))
  if (length(dim(x)) > 3) {
    warning("folding all higher dimensions into the third dimension")
    x = stars::st_apply(x, 1:2, as.vector)
  }
  d = stars::st_dimensions(x)
  dxy = attr(d, "raster")$dimensions
  stopifnot(all(dxy %in% names(d)))
  bb = sf::st_bbox(x)
  if (length(dim(x)) == 2) {
    raster::raster(nrows = dim(x)[dxy[2]], ncols = dim(x)[dxy[1]],
                   xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4],
                   crs = sf::st_crs(x)$proj4string, vals = as.vector(x[[1]]))
  }
  else {
    third = setdiff(names(d), dxy)
    b = raster::brick(nrows = dim(x)[dxy[2]], ncols = dim(x)[dxy[1]],
                      xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4],
                      nl = dim(x)[third], crs = sf::st_crs(x)$proj4string)
    raster::values(b) = as.vector(x[[1]])
    z = seq(d[[third]])
    if (all(!is.na(z)))
      raster::setZ(b, z)
    b
  }
}

proj_longlat <- function() {
  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
}

