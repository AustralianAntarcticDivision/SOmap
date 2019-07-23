#' SOmap coordinate system
#'
#' Set or return the coordinate system currently in use.
#'
#' If argument `crs` is NULL, the function returns the current value (which may be `NULL`).
#' @param crs provide PROJ string to set the value
#' @export
#' @examples
#' \dontrun{
#' SOmap()
#' SOcrs()
#' }
SOcrs <- function(crs = NULL) {
  if (!is.null(crs)) {
    options(SOmap.crs.inuse = crs)
    return(crs)
  }
  crs <- getOption("SOmap.crs.inuse")
  if (is.null(crs)) warning("No SOmap.crs.inuse")
  crs
}
