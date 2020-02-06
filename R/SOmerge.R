#' Merge multiple SOmap or related objects
#'
#' The inputs must contain exactly one object of class `SOmap`.
#'
#' Note that objects of class `SOmap_auto` are not yet supported.
#'
#' @param ... : one or more objects of class `SOmap`, `SOmap_management`, or `SOmap_legend`, or a list of such objects
#' @param reproject logical: if `TRUE`, and any of the input objects are in a different projection to the input `SOmap` object, an attempt will be made to reproject
#'
#' @return A single object of class `SOmap`.
#'
#' @seealso \code{\link{SOmap}}
#'
#' @examples
#' \dontrun{
#'   mymap <- SOmap(bathy_legend = "space")
#'   mylegend <- SOleg(x = runif(100), position = "topright", col = viridisLite::viridis(80),
#'                     breaks = c(0.1, 0.2, 0.5, 0.9), trim = -45, label = "Thing",
#'                     rnd = 1, type = "continuous")
#'   mymgmt <- SOmanagement(eez = TRUE, basemap = mymap)
#'   merged <- SOmerge(mymap, mymgmt, mylegend)
#'   plot(merged)
#'
#'   ## note that you need to take some care in constructing the component objects
#'   ##  to ensure their visual consistency
#'
#'   ## e.g. this will work, but the EEZ layers will extend beyond the map bounds
#'   mymap <- SOmap(trim = -55)
#'   mymgmt <- SOmanagement(eez = TRUE, trim = -45) ## note different trim
#'   plot(SOmerge(mymap, mymgmt))
#'
#'   ## better to do
#'   mymap <- SOmap(trim = -55)
#'   mymgmt <- SOmanagement(eez = TRUE, basemap = mymap)
#'   plot(SOmerge(mymap, mymgmt))
#'
#'   ## we can reproject on the fly if needed
#'
#'   sw_atlantic <- SOmap_auto(c(-70, -20), c(-65, -45), input_points = FALSE, input_lines = FALSE)
#'   mymap_auto$projection
#'   ## the EEZs within this region
#'   sw_atlantic_mgmt <- SOmanagement(eez = TRUE, basemap = sw_atlantic)
#'
#'   mymap <- SOmap()
#'
#'   ## sw_atlantic_mgmt lies within the bounds of mymap, so we might want to combine them
#'   ##  even though their projections are different
#'
#'   merged <- SOmerge(mymap, sw_atlantic_mgmt, reproject = TRUE)
#'   plot(merged)
#' }
#'
#' @export
SOmerge <- function(..., reproject = FALSE) {
    supported_classes <- c("SOmap", "SOmap_management", "SOmap_legend")
    p <- list(...)
    if (length(p) == 1 && is.list(p[[1]]) && !inherits(p[[1]], supported_classes)) {
        ## we've been given a list of objects?
        p <- p[[1]]
    }
    if (!all(vapply(p, inherits, supported_classes, FUN.VALUE = TRUE))) {
        stop("SOmerge is expecting one or more objects (of class\"", paste0(supported_classes, collapse = ", "), "\") or a list of such objects")
    }
    somap_idx <- vapply(p, inherits, "SOmap", FUN.VALUE = TRUE)
    if (sum(somap_idx) != 1) {
        stop("The inputs to SOmerge must include exactly one object of class \"SOmap\"")
    }
    out <- p[[which(somap_idx)]]
    p <- p[!somap_idx]
    for (pnum in seq_along(p)) {
        thisp <- p[[pnum]]
        if ("projection" %in% names(thisp) && thisp$projection != out$projection) {
            if (reproject) {
                thisp <- reproj(thisp, target = out$projection)
            } else {
                stop("can't merge objects with different projections, try using `reproj = TRUE`")
            }
        }
        ## does it matter is thisp$target does not match out$target?
        out$plot_sequence <- c(out$plot_sequence, paste0(thisp$plot_sequence, "_", pnum))
        thisp <- thisp[setdiff(names(thisp), c("projection", "plot_sequence", "target"))]
        names(thisp) <- paste0(names(thisp), "_", pnum)
        out <- c(out, thisp)
    }
    structure(out, class = "SOmap")
}
