#' Extract plotting code from a SOmap
#'
#' This is thoroughly experimental!
#'
#' @param x : a map object as returned by [SOmap], [SOmanagement], [SOleg], or [SOgg]
#' @param data_object_name string: the name to use for the object that will hold the map data. See Examples, below
#'
#' @return A list with two elements: `code` contains R code that will draw the map, and `SOmap_data` (or whatever was passed as the `data_object_name` argument) contains any data required by that code
#'
#' @seealso [SOmap]
#'
#' @examples
#' \dontrun{
#'   p <- SOmap()
#'   mapcode <- SOcode(p, data_object_name = "SOmap_data")
#'
#'   ## write this code to a file
#'   my_R_file <- tempfile(fileext = ".R")
#'   writeLines(mapcode$code, con = my_R_file)
#'
#'   ## you can edit the code in that file if desired
#'
#'   ## save the data
#'   my_data_file <- tempfile(fileext = ".rds")
#'   saveRDS(mapcode$SOmap_data, my_data_file)
#'
#'   ## later on, we can re-load the data and execute the code
#'   SOmap_data <- readRDS(my_data_file)
#'   source(my_R_file)
#'
#'   ## or just to show that this works, evaluate the returned code directly against its data
#'   with(mapcode, for (codeline in code) eval(parse(text = codeline)))
#' }
#'
#' @export
SOcode <- function(x, data_object_name = "SOmap_data") {
    datout <- list()
    codeout <- character()
    if (!inherits(x, c("SOmap_management", "SOmap", "SOthing", "SOmap_legend", "SOmap_gg", "SOmap_auto_gg")))
        stop("x is not an object of a recognized SOmap class")
    is_gg <- inherits(x, c("SOmap_gg", "SOmap_auto_gg"))
    ## we expect each plottable element of x to be a list of SO_plotter objects
    ##  but with old code we could have had an SO_plotter object directly (not in a list)
    ##  catch these first for backwards compatibility
    x2 <- lapply(x[intersect(x$plot_sequence, names(x))], function(z) {
        if (inherits(z, "SO_plotter")) list(z) else z
    })
    x2$plot_sequence <- names(x2)
    ## now reconstruct x, un-nesting embedded objects
    ## as of v0.6, we can have a SOmap_legend object embedded in here, which is itself a list of SO_plotter objects
    ## so if this object is a SOmap_legend, we have to handle it differently
    x <- lapply(x2$plot_sequence, function(component) {
        ## x2[[component]] will be a list of SO_plotter objects that make up this component
        ## OR it will be a list with a SOmap_legend object in it
        this <- lapply(x2[[component]], function(z) {
            if (inherits(z, "SOmap_legend")) {
                ## unpack it
                SOcode_flatten(z)[[1]]
            } else {
                z
            }
        })
    })
    names(x) <- x2$plot_sequence
    x$plot_sequence <- x2$plot_sequence
    ## interate through each plottable element in turn
    for (toplot in intersect(x$plot_sequence, names(x))) {
        allpf <- x[[toplot]] ## all the stuff to plot for this element
        ## either a SO_plotter object, or a list thereof
        ## if it's just one, put it in a list (backwards compatibility)
        if (inherits(allpf, "SO_plotter")) allpf <- list(allpf)
        if (!all(vapply(allpf, inherits, c("SO_plotter", "SOmap_legend"), FUN.VALUE = TRUE))) {
            warning("plotting behaviour for '", toplot, "' should be specified by an SO_plotter object or list of such objects, ignoring")
            next
        }
        pfn <- 0 ## for disambiguating data object references associated with this plot element
        for (thispf in allpf) {
            pfn <- pfn+1
            ## function can be a bare function name OR a locally-defined function OR a function name as a string
            if (is.function(thispf$plotfun)) {
                thisfun <- paste0("(", as.character(substitute(thispf$plotfun)), ")")
            } else if (is.character(thispf$plotfun)) {
                thisfun <- thispf$plotfun
            } else {
                stop("expecting function to be passed as string or function")
            }
            ## arguments
            myargs <- thispf$plotargs
            if (!is.list(myargs)) stop("expecting plot arguments as a list object")
            if (length(myargs) < 1) {
                ## empty list
                recoded_args <- ""
            } else {
                ## want all entries in args list to be named
                if (is.null(names(myargs)) || any(!nzchar(names(myargs)))) {
                    stop("unnamed args not dealt with yet")
                }
                datelement <- paste0(toplot, ".", pfn) ## name of data element in the returned data object
                ## short numeric/character/logical arguments can be passed directly as part of the function call string
                is_short <- rep(FALSE, length(myargs))
                args_as_string_or <- rep(NA_character_, length(myargs))
                for (argi in seq_along(myargs)) {
                    thisarg <- myargs[[argi]]
                    if (inherits(thisarg, c("numeric", "logical", "character")) && length(thisarg) <= 10) {
                        is_short[argi] <- TRUE
                        args_as_string_or[[argi]] <- deparse(thisarg)
                    } else {
                        ## the value of this argument is too long to nicely embed in the function call string directly
                        ## so we will return this data as part of data_object_name and refer to it there in our function call string
                        args_as_string_or[[argi]] <- paste0(data_object_name, "$", datelement, "$", names(myargs)[argi])
                    }
                }
                if (!all(is_short)) {
                    datout[[datelement]] <- myargs[!is_short]
                }
                ## now construct a string of the whole function call
                recoded_args <- paste0(names(myargs), " = ", args_as_string_or)
                ## Yikes. Just for the record, let's acknowledge that this whole thing is spectacularly unattractive code.
                ## But it's working, so ... shhhhh.
            }
            codeout <- c(codeout, paste0(thisfun, "(", paste(recoded_args, collapse = ", "), ")"))
        }
    }
    if (is_gg && length(codeout) > 1) {
        ## need to add the plot elements together
        codeout <- paste0(codeout, c(rep(" +", length(codeout)-1), ""))
        codeout <- paste(codeout, collapse = "\n")
    }
    out <- list(code = codeout, datout)
    names(out)[2] <- data_object_name
    out
}

## internal function to flatten an object into a single list of SO_plotter calls
## used to un-nest a nested SOmap_legend object, above
SOcode_flatten <- function(x) {
    unlist(x[x$plot_sequence], recursive = FALSE, use.names = FALSE)
}
