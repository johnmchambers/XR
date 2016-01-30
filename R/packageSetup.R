#' Execute a Setup Step for a Package
#'
#' The R code in one or more files is evaluated to carry out setup computations that will
#' write some code, documentation or anything else into the source directory of an R package.
#' Designed for package authors who want to use techniques such as the proxy classes in the
#' XR set of interfaces, \code{compileAttributes()} in Rcpp or inline documentation in roxygen2.
#'
#' The computations will be carried out in an environment constructed by\code{packageSetup()}
#' with the namespace of the package as its parent, and a \code{.packageName} object to
#' associate it with the package.  By default, the setup step looks for the \code{DESCRIPTION} file in the
#' working directory to find the package name.  The package must have been installed in the
#' library path of this R session.
#'
#' @param file The name of the files to be parsed and evaluated for the setup step.  By default,
#' looks for \code{"setup.R"} in the inst/tools directory or the current directory.
#' @param dir Optional directory to use as the working directory for the evaluation. By default
#' the current working directory should be the source directory for the package.
#' @param needPackage The package for which the setup is intended.  Not needed if the working
#' directory (either currently or given by \code{dir} is the source directory for that package.
#' If supplied can be either the package name or \code{FALSE} if the setup is not intended for
#' a package.
packageSetup <- function(file = .findSetup(), dir = ".", needPackage = TRUE) {
    .findSetup <- function() {
        for(f in c("inst/setup.R", "inst/tools/setup.R", "./setup.R"))
            if(file.exists(f)) return(f)
        NULL
    }
    wd <- getwd()
    optT <- getOption("topLevelEnvironment")
    on.exit({setwd(wd); options(topLevelEnvironment = optT)})
    if(!missing(dir))
        setwd(dir)
    if(identical(needPackage, FALSE))
        env <- parent.frame()
    else {
        if(is.character(needPackage))
            package <- needPackage
        ## check that this is a package source directory
        else {
            if(!file.exists("DESCRIPTION"))
                stop(gettextf("directory (%s) should be a package source directory, but no DESCRIPTION file",
                          dQuote(getwd())))
            package <- read.dcf("DESCRIPTION")[,"Package"][[1]]
        }
        env <- new.env(parent = asNamespace(package))
        assign(".packageName", package, envir = env)
    }
    options(topLevelEnvironment = env) # metadata for classes, methods, goes here
    if(is.null(file))
        stop("No file argument, and couldn't find setup.R")
    eval(parse(file),env)
}
