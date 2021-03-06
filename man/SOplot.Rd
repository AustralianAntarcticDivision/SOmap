% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOplot.R
\name{SOplot}
\alias{SOplot}
\title{Add items to an existing SOmap}
\usage{
SOplot(x, y = NULL, target = NULL, ..., source = NULL, add = TRUE)
}
\arguments{
\item{x}{: longitude vector, or an object with coordinates}

\item{y}{: latitude vector, or missing if x is an object}

\item{target}{: target projection. If not provided, it will default to the projection of the current map, and if that is not set it will use the default SOmap polar stereographic projection}

\item{...}{: other parameters passed to the \code{plot} function}

\item{source}{: if \code{x} is not an object with a projection already set, specify its projection here (default = longlat)}

\item{add}{logical: if \code{TRUE}, add this object to an existing plot}
}
\description{
Reproject and add an object to an existing \code{SOmap} or \code{SOmap_auto}.
}
\examples{
\dontrun{
  x <-c (-70, -60,-50, -90)
  y <-c (-50, -75, -45, -60)
  map <- SOmap_auto(x, y, input_lines = FALSE)

  ## plot the map, with the x, y points already added
  map
  ## re-plot the points in a different colour and marker
  SOplot(x = x, y = y, pch = 0, cex = 2, col = 6)
}
}
