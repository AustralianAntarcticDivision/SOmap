
#' Construct a SO_plotter object
#'
#' `SOmap` and similar objects contain all of the data and code required to draw a map. This information is embedded in `SO_plotter` objects within the `SOmap` object.
#'
#' @param plotfun function or string: either the name of a function to use, or the function itself
#' @param plotargs list: arguments to pass to the function
#' @param name string: optional name for this element
#'
#' @return An object of class SO_plotter
#'
#' @seealso `SOmap`
#'
#' @examples
#' \dontrun{
#'   p <- SOmap()
#'   ## replace the `box` element with different plotting code
#'   p$box <- SO_plotter(plotfun = "graphics::box", plotargs = list(col = "red"))
#'
#'   ## you can also specify multiple plotting instructions for a single graphical element
#'   ##  of a map
#'   p$box <- c(SO_plotter(plotfun = "graphics::box", plotargs = list(col = "red")),
#'              SO_plotter(plotfun = "graphics::box", plotargs = list(lwd = 2)))
#' }
#' @export
SO_plotter <- function(plotfun, plotargs = NULL, name = NULL) {
    if (!is.string(plotfun)) {
        assert_that(is.function(plotfun))
    } else {
        assert_that(!is.na(plotfun), nzchar(plotfun))
    }
    if (!is.null(plotargs)) assert_that(is.list(plotargs))
    out <- list(structure(list(plotfun = plotfun, plotargs = plotargs), class = "SO_plotter"))
    if (!is.null(name)) {
        assert_that(is.string(name))
        names(out) <- name
    }
    out
}
