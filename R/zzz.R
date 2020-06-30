.onLoad <- function(libname, pkgname) {
    ## raster 3.3.6 and 3.3.7 had a bug with trim that causes our SOmap layout to fail
    if (packageVersion("raster") == "3.3.6" || packageVersion("raster") == "3.3.7") {
        warning("your version of the raster package (", packageVersion("raster"), ") has a bug, please upgrade it with install.packages(\"raster\")")
    }
    invisible()
}
