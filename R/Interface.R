setClassUnion("ProxyObject", "name")

#' Reference class for all interface evaluators
#'
#' This class has the fields required for any specific interface and the methods that
#' are defined centrally in the XR structure.  As noted in the documentation for inidividual
#' methods, some methods must be redefined in the specific interface.
#' @field evaluatorId A character string (usually unique) giving the language and date when started
#' @field languageName The server language.  Does not have to be unique if multiple classes implement interfaces to the same language.
#' @field proxyCount Counter used to generate unique names for proxy objects.
#' @field propertyFormat C-style format string for access to properties and methods in this
#' evaluator.  Nearly always just the two names, separated by "."
#' @field proxyClassTable An environment for all proxy classes known currently for this
#' evaluator class.
#' @field prototypeObject The object representing any proxy class for this inteface.  Usually from
#' a class defined by the specific interface package, to distinguish its proxy classes. This
#' field is passed as the \code{prototype} argument in calls to \code{\link{asServerObject}}.
Interface <-
  setRefClass("Interface",
              fields = list(evaluatorId = "character",
              languageName = "character", proxyCount = "integer",
              prototypeObject = "ProxyObject",
              propertyFormat = "character", proxyClassTable = "environment",
              modules = "environment",
              serverPath = "character"))

#' Classes of objects representing errors or other conditions in a server language
#'
#' Errors and warnings generated in evaluating an expression in the server language will
#' be returned to R as objects from one of these classes.
#' The interface evaluator will normally throw an error for \code{"InterfaceError"} and
#' issue a warning for \code{"InterfaceWarning"}.
#' @aliases InterfaceError-class InterfaceWarning-class
setClass("InterfaceCondition",
         slots = c(message = "character", value = "ANY", expr = "character", evaluator = "Interface"))

setClass("InterfaceError",
         contains = "InterfaceCondition")

setClass("InterfaceWarning",
         contains = "InterfaceCondition")


## the scalar classes
integerScalar <- setClass("integerScalar", contains = c("integer"),
                          prototype = NA_integer_,
                          validity = function(object) length(object) == 1)

numericScalar <- setClass("numericScalar", contains = c("numeric"),
                          prototype = NA_real_,
                          validity = function(object) length(object) == 1)

characterScalar <- setClass("characterScalar", contains = c("character"),
                          prototype = NA_character_,
                          validity = function(object) length(object) == 1)

logicalScalar <- setClass("logicalScalar", contains = c("logical"),
                          prototype = NA,
                          validity = function(object) length(object) == 1)

complexScalar <- setClass("complexScalar", contains = c("complex"),
                          prototype = NA_complex_,
                          validity = function(object) length(object) == 1)

.scalars <- c("integerScalar", "numericScalar", "characterScalar", "logicalScalar", "complexScalar")
.vectors <- c("integer", "numeric", "character", "logical", "complex")
.vectorTypes <- c("integer", "double", "character", "logical", "complex")

typeToClass <- function(type) {
    tt <- match(type, .vectorTypes)
    if(is.na(tt))
        "list"
    else
        .vectors[[tt]]
}

setClassUnion("Scalar", .scalars)

#' Scalar objects: declarations and classes
#'
#' The generator function \code{Scalar} and the corresponding class union indicate to the
#' interface evaluator that the object should be a valid scalar.  These are not often needed
#' explicitly, if the default rule converting length-1 vectors to scalars is applied and if
#' the JSON scalar output is a legal constant in the server language.
#' @param object A valid object for a scalar; that is, a vector of length 1.
#' @return The same value but with a suitable scalar class `numericScalar` etc.
#' @aliases Scalar-class numericScalar-class integerScalar-class logicalScalar-class
#' @aliases characterScalar-class complexScalar-class
#' @aliases show,Scalar-method  show,characterScalar-method

Scalar <- function(object) {
    what <- match(typeof(object), .vectorTypes)
    if(is.na(what))
        stop(gettextf("Type %s object is not a valid scalar", nameQuote(typeof(object))))
    else if(length(object) != 1)
        stop(gettextf("Scalar object must be of length 1; got %d", length(object)))
    new(.scalars[[what]], object)
}

#' Force an object to be treated as a vector in the server language
#'
#' @return the object, but with the S4 bit turned on.
#' Relies on the convention that XR interfaces leave S4 objects
#' as vectors, not scalars, even when they are of length 1
#' @param object A vector object.  Calling with a non-vector is an error.
noScalar <- function(object) {
    if(is.vector(object))
        asS4(object)
    else
        stop(gettextf("Object of class %s is not a vector", nameQuote(class(object))))
}


setMethod("show", "Scalar", function(object) cat(format(object), "\n"))

setMethod("show", "characterScalar", function(object) cat(deparse(as.character(object)), "\n"))

setMethod("[<-", "Scalar",
    function (x, i, j, ..., value)
    {
        if(missing(i) && missing(j))
            as(value, class(x))
        else
            stop("Indexed replacement invalid with scalar objects")
    }
)

setMethod("[[<-", "Scalar",
    function (x, i, j, ..., value)
    {
            stop("Replacing elements invalid with scalar objects")
    }
)


for(i in seq_along(.scalars)) {
    cl <- .scalars[[i]]
    vcl <- .vectors[[i]]
    setMethod("initialize", cl, eval(substitute(function(.Object, data ) {
        if(!missing(data)) {
            if(length(data) != 1)
                stop(gettextf("Can't use a vector of length %s as a scalar",
                          length(data)))
            data <- as(data, VCL)
            class(data) <- class(.Object)
            .Object <- data
        }
        asS4(.Object)
    }, list(VCL = vcl))))
    setAs(vcl, cl, eval(substitute(function(from) {
        if(length(from) != 1)
            stop(gettextf("Can't coerce a vector of length %s to scalar",
                          length(from)))
        new(cl, as(from, VCL))
    },list(VCL = vcl))), function(from, value) {
        if(length(value) != 1)
            stop(gettextf("Can't coerce a vector of length %s to scalar",
                          length(value)))
        from[ ] <- value
        from
    })
    ## the setAs() code doesn't work well here, so we have set all the methods
    ## rather than counting on inheritance
    for(fromvcl in .vectors) {
        setAs(fromvcl, cl, eval(substitute(function(from) {
            if(length(from) != 1)
                stop(gettextf("Can't coerce a vector of length %s to scalar",
                              length(from)))
            new(CL, as(from, VCL))
        }, list(CL = cl, VCL = vcl))), eval(substitute(function(from, value) {
            if(length(value) != 1)
                stop(gettextf("Can't coerce a vector of length %s to scalar",
                              length(value)))
            from[ ] <- as(value, VCL)
            from
        }, list(VCL = fromvcl))))
    }
}

### ProxyObjects

#' Class for Assigned Proxy Objects and Related Mechanisms
#'
#' The `AssignedProxy` class is used by interface packages to return a reference
#' to an assigned server object.  The \R user can then supply this object anywhere
#' in later interface computations, just as one would use the name of an \R object
#' in a function call or other expression.
#'
#' The virtual class `ProxyObject` is a superclass, designed to allow
#' other mechanisms for proxy objects (none exists at this time).
#' @slot .Data The `AssignedProxy` class is a subclass of `character`; the actual character string
#' will be generated by the interface and is unique over the session, so long as the `XR`
#' package stays loaded.
#' @slot serverClass The server language class and module for the corresponding object.
#' @slot size The size (usually, length) of the server object, if that makes sense.
#' Can be used to make decisions about handling large objects.
#' @slot evaluator The evaluator object that returned the proxy.  Having this as a slot allows
#' interface computations to operate directly on the proxy, without a user-supplied evaluator.
AssignedProxy <- setClass("AssignedProxy",
                          slots = c(serverClass = "character", module = "character",
                          size = "integer", evaluator = "Interface"),
                          contains = c("character"))
#' @describeIn AssignedProxy
setMethod("show", "AssignedProxy",
          function(object) {
              ev <- object@evaluator
              cat(ev$languageName, "proxy object\n")
              cat("Server Class:", object@serverClass)
              module <- object@module
              if(length(module) == 1 && nzchar(module))
                  cat("; Module:", module)
              len <- object@size
              if(!is.null(len))
                  cat("; size:", len)
              cat("\n")
          })

setIs("AssignedProxy", "ProxyObject")

## a table for the evaluators defined in this session
## Objects are assigned with the evaluatorId as name and the sequential number
## as value (since these are only required to be distinct within a single R
## process)
.evaluatorTable <- new.env()

## returns or creates a sequential number for the given evaluator,
#' @describeIn getInterface Return the sequential number for this evalutor; used in \code{ProxyName()} method.
evaluatorNumber <- function(ev) {
    id <- ev$evaluatorId
    n <- .evaluatorTable[[id]]
    if(is.null(n)) {
        n <- length(objects(.evaluatorTable))+1
        assign(id, n, envir = .evaluatorTable)
    }
    n
}

## a table of the search paths for languages that subclass "Interface"
languagePaths <- new.env()

pathEl <- setClass("pathEl", slots = c(package = "character", pos = "numeric"),
                   contains = "character")

setMethod("initialize","pathEl",
          function(.Object, ..., package = NULL, pos = NA) {
              if(is.null(package))
                  package <- ""
              if(is.na(pos))
                  pos <- NA_real_
              callNextMethod()
          })



serverAddToPath <- function(Class, dir, package = utils::packageName(topenv(parent.frame())),
                          pos = NA) {
    if(!is.character(dir))
        stop(gettextf(
            "New path element must be a directory as a string, got %s",
            dQuote(class(dir))))
    if(is(Class, "classRepresentation"))
        className <- Class@className
    else if(is(Class, "character")) {
        className <- Class
        Class <- getClass(className)
    }
    else
        stop(gettextf(
            "Invalid object to define interface (got class %s)",
            class(Class)))
    if(!extends(Class, "Interface"))
        stop(gettextf(
            "Class for path must extend \"Interface\", %s does not",
            dQuote(className)))
    path <- languagePaths[[className]]
    el <- list(pathEl(dir, package = package, pos = pos))
    if(is.null(path))
        path <- el
    else if(is.na(match(dir, path)))
        path <- c(path, el)
    languagePaths[[className]] <- path
    invisible(path)
}

## a table of the evaluators for all languages that subclass "Interface"
## (see getInterface())
languageEvaluators <- new.env()

#' Get or start an evaluator for an interface
#'
#' @return \code{getInterface()} returns an  interface evaluator for this class, starting one if none exists.
#' @param Class the name of the interface class for this evaluator; by default, the class of the
#' current evaluator. Can also be the class definition object.
#' @param makeNew can be used to force or prevent starting a new evaluator, if passed as
#' a logical value.  Can also be passed as a function that tests the suitability of a
#' current evaluator, returning TRUE if this one won't do, and a new one should be
#' generated instead (consistent with the ... arguments, presumably).
#'
#' The default is NA, meaning that an existing evaluator is OK, but one should be generated
#' if none exists.  In contrast, FALSE means to return NULL if no matching evaluator exists.
#' @param ... arguments, if any, are passed to the generator for the evaluator
#' @details
#'Specific language interface packages usually supply a convenience function equivalent
#'to calling \code{getInterface()} for their class; e.g., \code{RPython()} in \code{'XRPython'}
#'
#'If no \code{Class} is given, the current (i.e., last active) evaluator is returned
getInterface <- function(Class, ..., .makeNew = NA, .select = NULL)  {
    if(nargs() == 0)
        return(languageEvaluators[[".Current"]])
    if(missing(Class)) {
        current <- languageEvaluators[[".Current"]]
        if(!length(current)) {
            if(identical(.makeNew, FALSE))
                return(NULL)
            else
                stop("No interface evaluator has been started")
        }
        className <- class(current[[1]])
        Class <- getClass(className)
    }
    else {
        if(is(Class, "classRepresentation"))
            className <- Class@className
        else if(is(Class, "refObjectGenerator")) {
            Class <- Class@generator$def
            className <- Class@className
        }
        else if(is.character(Class)) {
            className <- Class
            Class <- getClass(Class)
        }
        else
            stop(gettextf(
                "Invalid object to define interface (got class %s)",
                class(Class)))
        current <- languageEvaluators[[className]]
    }
    ## at this point, Class, className and current (list of corresponding eval'rs)
    ## should be defined
    if(is(.select, "function")) # test for properties of the interface
        currEval <- .select(current)
    else if(length(current))
        currEval <- current[[1]]
    else
        currEval <- NULL
    ## .makeNew should be NA (default), FALSE or TRUE
    if(identical(.makeNew, FALSE)) # return the latest one or NULL if none has been started
        return(currEval) # NULL or evaluator
    if(is.na(.makeNew) && (nargs() - !missing(.makeNew)) < 2 && # no ... args.
        ## return the current if there is one, else start one
            !is.null(currEval))
            return(currEval)
    ## start an interface. This may fail, otherwise add the resulting evaluator to the list
    value <- new(Class, ...)
    className <- class(value)
    current <- languageEvaluators[[className]]
    if(is.null(current))
        current <- list(value)
    else
        current <- c(list(value), current)
    languageEvaluators[[className]] <- current
    languageEvaluators[[".Current"]] <- value
    paths <- languagePaths[[className]]
    if(!is.na(paths)) {
        for(path in paths)
            value$AddToPath(path, path@package, path@pos)
    }
    value
}

#' @describeIn getInterface Remove an the specified  evaluator from the table of available interfaces.
rmInterface <- function(evaluator) {
    className <- class(evaluator)
    current <- languageEvaluators[[className]]
    if(!length(current))
        return(FALSE)
    id <- evaluator$evaluatorId
    for(i in seq_along(current)) {
        if(identical(id, current[[i]]$evaluatorId)) {
            languageEvaluators[[className]] <- current[-i]
            currEval <- languageEvaluators[[".Current"]]
            if(!is.null(currEval) && identical(id, currEval$evaluatorId))
                languageEvaluators[[".Current"]] <- NULL
            return(TRUE)
        }
    }
    return(FALSE)
}

.onUnload <- function(libPath) {
    ## close all active evaluators
    for(Class in objects(languageEvaluators)) {
        if(identical(Class, "current"))
            next #skip, this duplicates an actual class
        OK <- TRUE
        for(ev in languageEvaluators[[Class]]) {
            tryFinalize <- tryCatch(ev$Quit(), error = function(e)e)
            if(is(tryFinalize, "error"))
                OK <- FALSE
        }
        if(!OK)
            warning(gettextf("There were errors trying to quit the class %s evaluators",
                             nameQuote(Class)))
        languageEvaluators[[Class]] <- list() # nobody cares, but anyway.
    }
}


Interface$methods(
                  initialize = function(...) {
                      initFields(...) # allow overides to precede
                      if(!length(languageName)) # but should be set by a subclass method first
                          languageName <<- "<UnspecifiedLanguage>"
                      if(!length(proxyCount))
                          proxyCount <<- 0L
                      if(!(length(evaluatorId) == 1 && nzchar(evaluatorId)))
                          evaluatorId <<- paste(languageName, "Evaluator", format(Sys.time()))
                      if(!length(propertyFormat))
                          propertyFormat <<- "%s.%s"
                  },
                  finalize = function(...) {
                      'method called when the object is garbage collected.  A call to the $Quit() method
also calls this method (recalling it later then does nothing).  In case some server action
(like closing down a subprocess) is required, the $ServerQuit() method is called, and
the evaluator is then removed from the table of interface evaluators.'
        ServerQuit(...)
        rmInterface(.self)
    },
    Quit = function(...) { finalize(...) },
    ServerArglist = function(...) {
        if(nargs() == 0)
            return("")
        arglist <- list(...); args <- character(length(arglist))
        anames <- allNames(arglist)
        for(i in seq_along(args)) {
            namei <- anames[[i]]
            arg <- AsServerObject(arglist[[i]])
            if(nzchar(namei))
                arg <- paste(namei, "=", arg)
            args[[i]] <- arg
        }
        paste(args, collapse = ", ")
    },
    Call = function(fun, ...,
             .get = NA) {
        'Call the server language function `fun`.  Each of the `...` arguments will be
translated into a server language expression by the AsServerObject() method.'
        Eval(gettextf("%s(%s)",fun, ServerArglist(...)), .get = .get)
    },
    MethodCall = function(object, name, ..., .get = NA) {
        'Call the server language method `name` on `object`, with arguments `...`,
by default assuming a language in which the syntax is `object.name(...)`.
To override with a different syntax, define field propertyFormat in the evaluator.
Note that `name` must be a character string, not an evaluation in the server.'
        fun <- gettextf(propertyFormat, AsServerObject(object), name)
        Call(fun, ..., .get = .get)
    },
    FieldGet = function(object, name, .get = NA) {
        Eval(gettextf(propertyFormat, AsServerObject(object), name), .get = .get)
    },
    FieldAssign = function(object, name, value, .get = NA) {
        propName <- gettextf(propertyFormat, AsServerObject(object), name)
        expr <- gettextf("%s = %s", propName, "%s")
        Eval(expr, value, .returnValue = FALSE)
        invisible(object)
    },
    ProxyName = function(new = TRUE) {
        'A key for the next proxy object.  In the default strategy, this is a string
"R_i_j" where i is the sequence code for the evaluator and j is the proxy count,
incremented if `new` is TRUE'
        if(new)
            proxyCount <<- proxyCount + 1L
        seq <- evaluatorNumber(.self)
        paste0("R_",seq,"_",proxyCount)
    },
    Eval = function(expr, ..., .get = NA) {
        'Evaluate `expr` and return the value, possibly as a proxy.
Expressions are supplied as character strings to be parsed and
evaluated by the server language. If `expr` has "%s" fields, they are filled in
with the appropriate server language code equivalent to the `...` arguments.  If `expr` has
more than one element, all but the last are evaluated by $Command(), with `...` ignored.'
        n <- length(expr)
        if(n > 1) {
            for(i in seq(length.out = n-1))
                Command(expr[[i]])
            expr <- expr[[n]]
        }
        key <- ProxyName() # new proxy object key, may not be used
        ServerEval(ServerExpression(expr,...), key, .get)
    },
    Command = function(expr, ...) {
        'Like Eval(), but the value of the expression is ignored.  In particular, may be a command
in the server language that is not an expression.'
        invisible(ServerEval(ServerExpression(expr, ...), "", FALSE))
    },
    Get = function(what, ...) {
        'Return the value, always converted to an R object.  Usually gets a proxy object as the argument, but can be called like $Eval(), if ... is non-empty.'
        if(nargs() > 1)
            Eval(what, ..., .get = TRUE)
        else
            Eval("%s", what, .get = TRUE)
    },
    Send = function(object, serverClass = "", .key = NULL) {
        'Send the converted version of `object` to the server language.
If `.key` is specified, assign it under that name.  By default (and recommended)
a proxy object in R provides the name for the converted object.
If `serverClass` is supplied, there should be a corresponding asServerObject() method.'
        if(is.null(.key))
            key <- ProxyName()
        else
            key <- .key
        if(nzchar(serverClass)) {
            pclass <- ProxyClassName(serverClass) # implies the class exists
            prototype <- new(pclass)
            expr <- AsServerObject(object, prototype)
        }
        else
            expr <- AsServerObject(object)
        ## expr could be a JSONScalar or possibly something else from asServerObject
        ## The definition of ServerEval requires a character string as this argument
        ServerEval(as.character(expr), key, FALSE)
    },
    Remove = function(key) ServerRemove(key),
    Shell = function(endCode = "quit", prompt =  ">>>: ", cont = "+++: ") {
        'Starts an interactive shell.  Each line of input must be a complete expression
or statement in the server language. To continue over multiple lines, append an unescaped
backslash to all but the last line.'
        pp <- prompt
        repeat {
            expr <- character()
            repeat {
                cat(pp, file = stdout())
                line <- readLines(stdin(), n = 1)
                if(grepl("[\\]$", line) && !grepl("[\\][\\]$", line)) {
                    expr <- c(expr, sub(".$", "", line))
                    pp <- cont
                }
                else {
                    expr <- c(expr, line)
                    pp <- prompt
                    break
                }
            }
            if(length(expr) > 1)
                expr <- paste(expr, collapse = "\n")
            if(identical(expr, endCode))
                break
            tryCatch(ServerEval(expr), error = function(e) {
                message(gettextf("%s error: %s", languageName, e$message))
                })
        }
    },
    New = function(serverClass, serverModule = "", ...) {
        'Generate a new object from the specified server class.  The corresponding generator function
in the server is given by ServerGenerator(serverClass), by default just the class name.
Typically called from the $Initialize() method of the proxy class.'
        ## simplified version of $Call(), to avoid the asRObject() method for AssignedProxy,
        ## which would recursively try to generate a proxy class object.
        Call(ServerGenerator(serverClass, serverModule), ..., .get = FALSE)
    },
    Function = function(serverFun) { # <TODO> add args= that can specify arg list </TODO>
        'Returns an R function object that calls the specified sever language function,
specified by its name in the server language or by the proxy object returned by the
$Define() method of the evaluator'
        if(!is.character(serverFun))
            stop(gettextf("Expected the name of a server language function or the R proxy object for one; got an object of class %s",
                          nameQuote(class(serverFun)[[1]])))
        eval(substitute(function(...)
            Call(FUN, ...), list(FUN = as.character(serverFun))))
    },
    Help = function(topic) {
        getRefClass()$help(topic)
    },
    help = function(topic) {
        getRefClass()$help(topic)
    },
    ProxyClassName = function(serverClass) {
        'If there is a proxy class defined corresponding to this serverClass, return the name
of that class (typically pasted with the server langauge, separated by underscore).  If no
such class is defined, return NA.'
        pclass <- paste(serverClass,languageName, sep="_")
        def <- getClassDef(pclass)
        if(is.null(def))
             NA # assign this to avoid the class search on later calls
        else
            pclass
    },
    ProxyClassObject = function(object) {
        'If `object` is an assigned proxy, check whether the serverClass is a known proxy class
and if so, return an object from that class; otherwise return `object`.'
        if(!is(object, "AssignedProxy"))
            return(object) # probably an error, however
        object@evaluator <- .self
        ## and the size slot may be empty or set to a negative number (server
        ## languages lacking NA_integer_)
        n <- object@size
        if(length(n) > 1)
            stop("Invalid length attribute: should have been a scalar, was vector of length ",
                 length(n))
        if(length(n) == 0 || n < 0)
            object@size <- NA_integer_
        serverClass <- object@serverClass
        if(length(serverClass) == 1 && nzchar(serverClass))
            pclass <- proxyClassTable[[serverClass]]
        else
            return(object) # no serverClass, but this is unlikely
        ## if found, should be the R class with name == "class_language"
        if(is.null(pclass)) { # look for the class the first time
            pclass <- ProxyClassName(object@serverClass)
            # assign the class name (or NA) for next time
            base::assign(object@serverClass, pclass, envir = proxyClassTable)
        }
        if(is.na(pclass))
            object
        else
            methods::new(pclass, .serverObject = object)
    },
    AsRObject = function(object) {
        'Given an R object made up of vectors, lists and named tables, interpret that as a general R object,
using a convention that may be specialized to the server language by overriding
$AsRObject() or by methods for asRObject().  The argument will may be from a proxy class.'
        asRObject(object, .self)
    },
    AsServerObject = function(object, prototype = prototypeObject) {
        'Given an R object return a string that, when evaluated in the server language gives
a corresponding object in that language.  The default implementation uses the function
XR::objectAsJSON, which returns a JSON string and assumes a function `objectFromJSON(string)`
in the server. The conversion may be specialized to server language classes by methods for
asServerObject() or objectAsJSON().'
        asServerObject(object, prototype)
    },
    Serialize = function(object, file, append = FALSE) {
        'Use the server language serialization to serialize `object` to the specified `file`.
According to `append` either append to the file (default) or overwrite.
The supplied object should be a proxy for a server language object.'
        if(!is(object, "AssignedProxy"))
            stop(gettextf(
                "Only proxies for server language objects can be serialized; got class %s",
                nameQuote(class(object))))
        if(!append) { # truncate the file
            con <- base::file(file, "w")
            close(con)
        }
        ServerSerialize(as.character(object), file)
    },
    Unserialize = function(file, all = FALSE) {
        'Unserialize the objects previously written to `file` by $Serialize().
Returns a list of proxy objects (always a list even if only one object found).'
        ServerUnserialize(file, all) # will be a matching method to ServerSerialize()
    },
    AddToPath = function(directory = base::tolower(languageName),
                     package = utils::packageName(topenv(parent.frame())), pos = NA)     {
        'Add the directory to the systemPath
By default, appends to the path; if `pos` is given, inserts at that position.
If both directory and package are omitted, the method looks for the package name in the
calling function (suitable if the method call is from a package source file).'
        if(is.null(package) || identical(package,"")) # not from package or user supplied ""
          serverDirectory <- directory
        else {
          serverDirectory <- system.file(directory, package = package)
          if(!nzchar(serverDirectory))
            stop(gettextf("No directory %s found for package %s",
                          nameQuote(directory), nameQuote(package)))
        }
        if(serverDirectory %in% serverPath)
            return(invisible(FALSE))
        ServerAddToPath(serverDirectory, pos)
        serverPath <<- c(serverPath, serverDirectory)
        invisible(TRUE)
    },
    ServerAddToPath = function(serverDirectory, pos)
        stop(gettextf("No method $ServerAddToPath() found for evaluator class %s",
              nameQuote(class(.self)))),
    Import = function (module, ...) {
        'Import the module.  The "Interface" method assumes a command "import" in the server
language and does not handle any extra arguments (e.g., for importing specific members).'
        members <- list(...)
        if(length(members))
            stop(gettexttf(
             "The XR version of this method has no way to interpret %d extra arguments in addition to module name %s",
                           length(members), nameQuote(module)))
        imported <- base::exists(module, envir = modules)
        if (!imported) {
            Command(paste("import", module))
            base::assign(module, ".IMPORT", envir = modules)
        }
        module
    },
    Source = function(filename) {
        'Parse and evaluate the contents of the file.  This method is likely to be overriden for particular langauges
with a directive to include the contents of the file.  The `XR` version reads the file and processes the entire contents
as a single string, newlines inserted between lines of the file.'
        Command(paste(base::readLines(filename), collapse = "\n"))
    },
    ServerExpression = function(...) {
        'The arguments define an expression in the server language.
The first argument is a string; any others are objects to be substituted for %s fields
in the string.  These can include proxy objects or R data.'
        expr <- list(...)
        if(length(expr)>1)  { # expressions can have parameters via %s in a C format string
            for(i in 2:length(expr))
                expr[[i]] <- AsServerObject(expr[[i]])
            do.call(base::gettextf, expr, envir = parent.frame())
        }
        else
            expr[[1]]
    },
    ServerGenerator = function(serverClass, serverModule = "") {
        if(nzchar(serverModule))
            paste(serverModule, serverClass, sep=".")
        else
            serverClass
    },
    ServerClass = function(Class, module) {
        'If possible, return the class structure of Class, a class in the server language.
module= is the server module/package/library in which Class is defined, or "".
If no reflection information is available, return NULL (which this definition does).
Should return a list or reference object: "$fields" and "$methods" should be character
vectors or named lists of the server fields and metohds.'
        NULL
    },
    ## the methods that must be subclassed for an actual interface
    ServerEval = function(expr, key, get) {
        'Must be defined by the server language interface: evaluates `expr`(a text string).
If `key` is an empty string, `expr` is treated as a directive, with no defined value.
Otherwise, `key` is a non-empty string, and the server
object should be assigned with this name. The value returned is the R result, which may
be an AssignedProxy() object. If `get` is TRUE or the value judged simple enough,
it will be converted to an ordinary R object instead.'
        stop(gettextf("The ServerEval method has not been defined for evaluator %s",
                      evaluatorId))
    },
    ServerRemove  = function(key) {
        'Should be defined by the server language interface:
    The reference previously created for `key` should be removed.
What happens has no effect on the client side; the intent is to potentially recover memory.'
        FALSE
    },
    ServerQuit = function(...) {
         'If the server language needs to take some action when the evaluator is closed down,
this method should do it.  In connected interfaces, this is likely but not in embedded.
The method for the "Interface" class does nothing.'
    },
    ServerFunctionDef = function(name, module = "", ...) {
        'The XR method defines the proxy function with no special metadata information.
Server langugae metadata may be used by a method that overrides this one, and calls it.'
        ProxyFunction(name, module, ...)
    },
    ServerClassDef = function(Class, module, ...) {
        'Individual interface packages will define this to return a named list or other object such that value$fields and value$methods are the server fields and methods, character vectors
of names or named objects whose elements give further information.  This default version
returns NULL, indicating that no metadata is available.'
        NULL
    },
    ServerSerialize = function(key, file) {
        'Serialize the proxy function corresponding to `key` to the specified `file`.
Will normally be defined using the serialization supported by the particular server
language. The default gets the object and serializes in R, so only works if conversion does'
        con <- open(file, "a")
        base::serialize(Get(key), con)
        close(con)
    },
    ServerUnserialize = function(file, all) {
        'Unserialize the file, returning a proxy object for a list, or equivalent in the server
language, of all the objects serialized to this file.  Because open connections can not
generally be shared among languages, must unserialize the entire file.'
        con <- open(file, "r")
        value <- list()
        repeat {
            tryCatch( value <- append(value, base::unserialize(con)),
                     error = function(e) break
                     )
        }
        if(!all && length(value)) {
            if(length(value) == 1)
                value <- value[[1]]
            else
                warning(gettextf("Expected to unserialize one object, found %d",
                                 length(value)))
        }
        value
    }
)


prototypeObject <- function(evaluator = getInterface())
    evaluator$prototypeObject

#' Generate a Server Language Expression corresponding to an R Object
#'
#' Returns a string that can be inserted into a server language expression.
#' When parsed and evaluated by the server evaluator, the result
#' will be the appropriate object or data.
#'
#' Methods for proxy objects and proxy class objects
#' will produce the name under which they were assigned.
#' The default method uses JSON to encode the object as a string and expects the server side
#' interface to have a function objectFromJSON() to decode the string.
#' @param object The \R object.
#' @param prototype The proxy for a prototype of the server language object wanted.
#' When called from the `AsServerObject()` method of an evaluator, this argument is supplied
#' automatically from a class of objects for that evaluator, allowing methods to be defined
#' specialized to the various interface evalutor classes.
setGeneric("asServerObject",
           function(object, prototype) {
               jsonString <- objectAsJSON(object, prototype)
               if(is(jsonString, "JSONScalar")) # assume these are legal in server
                   jsonString
               else
                   gettextf("objectFromJSON(%s)",
                            typeToJSON(jsonString, prototype)) # add escapes in the string
           })

#' @describeIn asServerObject class "name" is used to pass unquoted strings, in case your
#' interface code did an explicit assign (but that's discouraged).
setMethod("asServerObject", "name",
          function(object, prototype) {
              as.character(object)
          })


#' @describeIn asServerObject Proxy objects are just passed as their character string, although
#' a particular interface class could do something different, like refer to a table.
setMethod("asServerObject", "AssignedProxy",
          function(object, prototype)
              as.character(object)
          )
## also a method in ProxyClass.R

#' A class that facilitates returning R vectors via a list in JSON
#'
#' Server language code will return a dictionary in which data= is a JSON-style list
#' and type= is the R vector type desired.  See the asRObject() method described below.
#' @slot data The actual vector data.
#' @slot type The string for the \R type intended. In spite of the slot name, this really the class;
#' for example, "numeric" rather than "double".
#' @slot missing The index of NA's in this vector.  Needed because most server languages only
#' treat `NaN` for doubles and have no mechanism for `NA` in other types.
vector_R <- setClass("vector_R",
                     slots = c(data = "vector", type = "character",
                     missing = "vector", serverClass = "character"))

#' @describeIn vector_R Expects usually to get only the `data` argument.
setMethod("initialize", "vector_R",
          function(.Object, data, type = typeToClass(typeof(.Object@data)), missing) {
              if(base::missing(data)) # the 0-argument case
                  return(.Object)
              .Object@data <- as(data, "vector")
              .Object@type <- type
              if(base::missing(missing))
                  .Object@missing <-
                      switch(type,
                             integer = , numeric = , complex = ,
                             logical = seq_along(data)[is.na(data)],
                             integer())
              else {
                  .Object@missing <- missing
                  if(length(missing))
                      .Object@data[missing] <- NA
              }
              .Object
          })

#' Specialize the R Object Returned from an Interface Evaluator
#'
#' The result of a server language expression is returned as a string, using the JSON
#' standard notation to represent a scalar, list or dictionary.  Methods for this function
#' get the simple R object obtained from deparsing and interpret it generally.
#'
#' The methods supplied with the `XR` pacakage handle the standard mechanisms for interpretation.
#' Additional methods are likely to interpret proxy class objects for which the standard
#' XR representation in terms of class and slots is not what's actually wanted.
#' @param object An object constructed from the explicit representation as a dictionary.
#' The elements of the dictionary will be converted into objects for the slots of the same
#' name.  Application-written methods will re-interpret the object into the intended \R form,
#' not necessarily from the same class.
#' @param evaluator This argument will be supplied as the evaluator object doing the conversion.
#' Therefore, methods may have one of the specific evaluator classes (e.g., \code{"PythonInterface"},
#' in their signature.
#' @section Writing Application Methods:
#' Application packages will typically write methods for special classes, and often for classes
#' themselves defined in the package.  One good reason is that the server language does not
#' naturally return the eventually intended object in a convenient form; for example, because
#' it does not have typed arrays.  Then a special class will be defined in \R.  The server code
#' will generate a dictionary with the \code{".RClass"} element having the class name, plus
#' whatever slots make sense.  The application method for \code{asRObject()} will take these
#' slots and construct whatever object is really intended.  For an example, see the method
#' for class \code{vector_R}.
setGeneric("asRObject", function(object, evaluator) {
        object
})

#' @describeIn asRObject When a proxy object appears, usually as an element of a list, it
#' is expanded, by using the `Get()` method of the evaluator and calling `asRObject()` on the
#' result.
setMethod("asRObject", "ProxyObject", # typically, an element of a list
          function(object, evaluator) {
              actual <- evaluator$Get(object)
              asRObject(actual, evaluator)
          }
          )

.formattedTypes <- c("complex") # others? raw?

.asRParsed <- function(data) {
    txt <- paste0("c(", paste(as.character(data), collapse = ", "), ")")
    eval(parse(text = txt))
}

#' @describeIn vector_R To distinguish typed R vectors from a general JSON list, encode the
#' desired data as an object from the "vector_R" class.
#' Vector types whose elements cannot be represented in JSON (e..g, "complex") should
#' be returned as a list of character strings in a format that R will parse as
#' elements of the suitable vector object. (e.g, ".5+3i")
setMethod("asRObject", "vector_R", # the class for representing R basic vector types
          function(object, evaluator) {
              ## types that must be formatted by server
              if(object@type %in% .formattedTypes)
                  value <- .asRParsed(object@data)
              else
                  value <- as(object@data, object@type)
              if(length(object@missing)) {
                  whichMissing <- unlist(object@missing)
                  value[whichMissing] <- NA
              }
              value
          }
          )


#' @describeIn asRObject Both lists and dictionaries will come here from the basic conversion.
#' `names(object)` will either be NULL or all non-empty, from a dictionary.
setMethod("asRObject", "list",
          function(object, evaluator) {
              ## apply conversion to the elements
              object <- lapply(object, asRObject, evaluator)
              if(".RClass" %in% names(object)) {
                  object <- makeNewObject(object, evaluator)
                  ## Classes designed for interfaces may need to apply
                  ## asRObject again, because a new() call is constrained to
                  ## produce the exact class specified; see vector_R for an example
                  asRObject(object, evaluator)
              }
              else
                  object
          })


## a table of generator functions that need to be called by makeNewObject in place
## of the initialize() method
.scanRaw <- function(data) {
    txt <- textConnection(as.character(data), "r")
    on.exit(close(txt))
    scan(txt, raw())
}

.scanComplex <- function(data) {
    txt <- textConnection(as.character(data), "r")
    on.exit(close(txt))
    scan(txt, complex())
}

.asRMethods <- list(matrix = base::array, data.frame = base::data.frame,
                    AssignedProxy = AssignedProxy, raw = .scanRaw, complex = .scanComplex
                    )

## these have to match the types serialized in typeToJSON(): not a satisfactory state
.serializedTypes <- c( "externalptr", "bytecode", "weakref", "promise",  "char", "...")

## the asRObject method for the above types
.unserial <- function(data) {
    txt <- textConnection(as.character(data), "r")
    on.exit(close(txt))
    unserialize(txt)
}

.cleanArgs <- function(args, doStruct = FALSE) {
    args[c(".RClass",".package", ".type", ".extends")] <- NULL
    slots <- names(args)
    if(doStruct) {
        unNamed <- slots == ".Data" | grepl("^[.][.][0-9]*$", slots)
        if(any(unNamed)) {
            ## sort by name, so the ..N arguments are put in order
            order <- base::order(slots)
            names(args)[unNamed] <- ""
            args <- args[order]
        }
    }
    args
}

makeNewObject <- function(object, evaluator, expandProxy = TRUE) {
    class <-  object[[".RClass"]]
    classAttr <- object[[".extends"]]
    if(is.null(classAttr))
        classAttr <- class
    package <- object[[".package"]]
    type <- object[[".type"]]
    dataPart <- object[[".Data"]] # may be NULL
    if(is.null(type)) # try to infer it
        type <- if(is.null(dataPart)) "S4" else typeof(dataPart)
    if(!is.null(type) && type != "S4" && type != typeof(dataPart)) {
        ## usually a type is basic but .Data is a list
        dataClass <- typeToClass(type)
        data <- dataPart
        if(is.null(data)) # no .Data, just slots but specifies .type (error?)
            data <- methods::new(dataClass)
        else
            data <- as(data, dataClass)
        object[[".Data"]] <- data
    }
    args <- as.list(object)
    generator <- .asRMethods[[class]]
    ## The following prohibits extensions of serialized types except via methods
    if(is.null(generator) && type %in% .serializedTypes)
        generator <- .unserial
    classDef <- getClassDef(class, package = package)
    doStruct <- is.null(classDef) || isVirtualClass(classDef) || !is.null(generator)
    args <- .cleanArgs(args, doStruct)
    if(is.null(generator))  { # generate a new object from the class
        if(doStruct)
            value <- makeStructureObject(args, class, package, classAttr)
        else {
            ## The representation is in terms of the slots, not the arguments
            ## to an initialization method.  Call new(class), then assign slots
            if(is.character(package))
                packageSlot(class) <- package
            value <- methods::new(classDef)
            slots <- names(args)
            for(i in seq_along(args))
                slot(value, slots[[i]]) <- args[[i]]
        }
    }
    else if(is(generator, "function")) # one of the classes that needs a special initializer
        value <- do.call(generator, args)
    else stop("something has corrupted the .asRMethod table:  call for help")
    value
}

.primitiveStructures <- c("matrix", "array")

makeStructureObject <- function(args, class, package, classAttr = class) {
    ## create a structure from the data part (element ".Data" or unnamed)
    ## with the other elements as attributes
    attrs <- names(args)
    dataPart <- attrs == ".Data" | !nzchar(attrs)
    if(any(dataPart))
        ## should warn if > 1?
        data <- args[dataPart][[1]]
    else
        data <- logical() # ? simplest vector; should warn?
    ## use individual attr(), because the dataPart may have attributes already
    for(i in seq_along(attrs))
        attr(data, attrs[[i]]) <- args[[i]]
    ## set class attribute, if not one of the primitives
    if(! class %in% .primitiveStructures)
        base::class(data) <- classAttr
    data
}

#' Construct a String in JSON Notation to Represent an R Object
#'
#' The XR structure requires a server-language function \code{objectFromJSON()} which
#' parses an object description in JSON.  Methods for generic function \code{objectAsJSON()}
#' should produce the appropriate string.
#'
#' This function is typically called from a method for \code{\link{asServerObject}}.
#' Methods for \code{objectAsJSON()} in turn often call one or both of two helper functions:
#' \code{\link{asJSONS4}()}, which produces the full description of the \R object; and
#' \code{\link{typeToJSON}()}, which produces the code for the basic \R types, ignoring all
#' class or attribute information.
#' @param object The object to be converted.
#' @param prototype The prototype server class; see \code{\link{asServerObject}}.
#' @param level Will be 1 for top-level call, incremented when recalled for an element.
#' Used to make choices about scalars.
#' @return A string that will be parsed according to JSON grammar.
setGeneric("objectAsJSON",
           function(object, prototype = prototypeObject(), level = 1) { # the default method
               if((isS4(object) && !is.null(attr(object, "class"))) ||
              ## exclude the S4 case where the bit is on to force a JSON array
                  is.list(attributes(object)) || # S3 class or structure
                  is.recursive(object)) # relying on a method existing for "list"
                  asJSONS4(object, prototype, level = level)
              else
                  typeToJSON(object, prototype)
           })

## We sidestep JSON for "numeric" because it leaves off trailing decimals points => integer
## The we need to have an explicit "integer" method so it does not inherit from "numeric"
## setMethod("objectAsJSON", "numeric",
##           function(object, prototype, level = 1) {
##               value <- as.character(as.numeric(object))
##               ints <- grepl("^[-]?[0-9]*$", value)
##               value[ints] <- paste0(value[ints], ".0")
##               if(length(value) != 1 || isS4(object))
##                   paste0("[", paste(value, collapse = ","), "]")
##               else
##                   JSONScalar(value)
##           })

## setMethod("objectAsJSON", "integer",
##           function(object, prototype, level = 1) {
##               value <- as.character(as.numeric(object))
##               if(length(value) != 1 || isS4(object))
##                   paste0("[", paste(value, collapse = ","), "]")
##               else
##                   JSONScalar(value)
##           })

#' @describeIn objectAsJSON treat matrix and array objects as legitimate S3 objects
setMethod("objectAsJSON", "array",
          function(object, prototype, level = 1) {
              attr(object, "class") <- if(is.matrix(object)) c("matrix", "array") else "array"
              asJSONS4(object, prototype)
          })
#' @describeIn objectAsJSON An environment is encoded with its class, in contrast to a named list
#' which may be a simple dictionary.
setMethod("objectAsJSON", "environment",
          function(object, prototype, level = 1) {
              ## Reference class objects won't get here, having their own method
              ## To distinguish named lists from environments in JSON, both are
              ## shown explicitly with their class
              asJSONS4(object, prototype, level = level)
          })

## setMethod("objectAsJSON", "NULL",
##           function(object, prototype, level = 1) {
##               "null"
##           })

#' @describeIn objectAsJSON A list will be encoded as a JSON list if it has no names, as a JSON
#' dictionary if it has all distinct, non-empty names, or with an explicit representation in
#' all other cases.
setMethod("objectAsJSON", "list",
          function(object, prototype, level = 1) {
              ## a totally plain list is a JSON array; one with distinct non-empty names
              ## is a dictionary;  anything else is expanded by asJSONS4()
              if(identical(class(object), "list")) {
                  nn <- methods::allNames(object)
                  empty <- !nzchar(nn)
                  if(all(empty))
                      dict <- FALSE
                  else if(any(duplicated(nn) | empty))
                      return(asJSONS4(object, prototype, level = level))
                  else
                      dict <- TRUE
                  jsonApply("list", object, prototype, level = level, dict)
              }
              else
                  asJSONS4(object, prototype, level = level)
          })

#' @describeIn objectAsJSON An explicit representation that includes the fields.
setMethod("objectAsJSON", "envRefClass",
          function(object, prototype, level = 1) {
              fieldNames <- names(getRefClass(class(object))$fields())
              fields <- lapply(fieldNames, function(what)object$field(what))
              names(fields) <- fieldNames
              asJSONS4(object, prototype, fields, level = level)
          })

#' @describeIn objectAsJSON An interface object is transmitted via its character string Id,
#'  enough to identify the object; the rest is too R-dependent to be useful.
setMethod("objectAsJSON", "Interface",
          function(object, prototype, level = 1) {
              ## the character string Id is enough to identify the object
              ## the engine is too R-dependent
              asJSONS4(object, prototype, exclude = "engine", level = level)
          })


#' @describeIn objectAsJSON Gets the object back from the server, then recalls the generic.
#' It's usually not a good idea to get here, because bringing the proxy back and then converting
#' it again is not foolproof; better to make direct use of the proxy.  But if a proxy object
#' is part of an ordinary list, environment or other R object, this method will be used.
setMethod("objectAsJSON", "AssignedProxy",
          function(object, prototype, level = 1) {
              ## <FIXME> We have a bind, in that prototype is usually ev$prototypeObject,
              ## which is nearly always a ProxyClassObject, but to require this
              ## gets us into a recursive bind in the definition of class "Interface". ???
              if(is(prototype, "ProxyClassObject"))
                  object <- prototype$.ev$Get(object)
              else if(is(prototype, "AssignedProxy"))
                  object <- prototype@evaluator$Get(object)
              else
                  stop(gettextf(
                      "something is wrong: expected a ProxyClassObject for prototype, got class %s",
                      nameQuote(class(prototype))))
              objectAsJSON(object, prototype, level)
          })

#' Generate the Explicit Dictionary form for an R Object
#'
#' The XR interface strategy uses an explicit named list (i.e., dictionary) to describe an R object
#' from a particular class.  This function creates the suitable form for such a dictionary, based on
#' the formal class or the contents of an object.  Used by some interface packages (e.g., XRJulia) but
#' likely only of information value otherwise, to tell you how to code an object in the server language.
#' @return a named list with the required entries, e.g., \code{".RClass"}.
#' @param object the object to use to infer the representation
#' @param exclude slots or the like that should \emph{not} be in the dictionary form.
objectDictionary <- function(object, exclude = character()) {
    slotList <- function() {
        value <- lapply(vnames, function(what) slot(object, what))
        names(value) <- vnames
        value
    }
    ## these three functions replace slotList for S3, environment, list objects
    attrList <- function() {
        obj <- object
        attributes(obj) <- NULL
        c(list(.Data = as.vector(obj)), attributes(object))
    }
    envList <- function() {
        value <- lapply(vnames, function(what) get(envir = object, what))
        names(value) <- vnames
        value
    }
    listList <- function() {
        value <- as.list(object)
        names(value) <- vnames
        value
    }
    simpleVector <- function() {
        list(.Data = object)
    }
    if(isS4(object)) {
        Rclass <- as.character(class(object)) # usually has a package slot, we use the def
        def <- getClassDef(Rclass)
        package <- def@package
        ext <- extends(Rclass)
        vnames <- names(def@slots)
    }
    else if(length(attr(object, "class"))) { #S3 class
        package <- ""
        ext <- class(object)
        Rclass <- ext[[1]]
        vnames <- c(".Data", names(attributes(object)))
        slotList <- attrList
    }
    else if(is.environment(object)) { # a non-S4 environment can't have structure
        Rclass <- ext <- "environment"
        package <- ""
        vnames <- objects(object, all.names = TRUE)
        slotList <- envList # substitute for the function returning slots
    }
    else if(is.list(object)) {
        vnames <- names(fillNames(object))
        slotList <- listList
        ext <- class(object)
        Rclass <- ext[[1]]
        package <- "" # this provides a test for an S3 class
        slotList <- listList
    }
    else { # plain
        package <- ""
        ext <- class(object)
        Rclass <- ext[[1]]
        slotList <- simpleVector
    }
    value <- list(
        .RClass = Rclass,
        .package = package,
        .type = typeof(object),
        .extends = ext)
    slots <- slotList()
    value[names(slots)] <- slots
    exclude <- c("class", exclude)
    ## with ext defined, we drop the actual class attribute from the dictionary
    value[names(value) %in% exclude] <- NULL
    value
}

## The general converter for an object with a formal class
## or an object to be described via its S3 class, data and attributes
asJSONS4 <- function(object, prototype, exclude = character(), level = 1) {
    value <- objectDictionary(object, exclude)
    ## now apply objectAsJSON to elements
    jsonApply(Rclass, value, prototype, level+1)
  }

jsonApply <- function(Rclass, value, prototype, level = 1, dict = TRUE) {
    if(length(value))
        coded <- sapply(value, objectAsJSON, prototype = prototype, level = level)
    else # sapply would return an empty list
        coded <- character()
    if(!is.character(coded) || length(coded) != length(value)) #sanity check
        stop(gettextf(
            "Internal error: Something went wrong in applying objectAsJSON to an object of class %s:",
            nameQuote(Rclass)))
    if(dict)
        paste("{", paste(nameQuote(names(value)), ":", coded, collapse = ", "), "}")
    else
        paste("[", paste(coded, collapse =", "), "]")
}


## create an explicit .RClass representation for some type of data
## The object should not come from a subclass of the type--in that case
## asJSONS4 should have been called.
asJSONData <- function(Class, type, data,  prototype) {
  classDef <- getClassDef(Class)
  if(is.null(classDef)) # no class for this type: does this ever happen?
    { package <- "base"; extends <- Class}
  else
    { package <- classDef@package; extends <- methods::extends(Class)}
  fields <- list(.RClass = Class, .type = type, .package = package, .extends = extends,
       .Data = data)
  ## the JSON dictionary
  jsonApply(Class, fields, prototpye, level + 1)
}
## a utility to fill in blank names in list elements to make it valid for a dictionary
fillNames <- function(object, noNamesOK = FALSE) {
    onames <- allNames(object)
    empty <- !nzchar(onames)
    if(noNamesOK && all(empty))
        return(object)
    if(sum(empty) == 1)
        onames[empty] <- ".Data"
    else
        onames[empty] <- paste0(".Data", 1:sum(empty))
    dups <- duplicated(onames)
    if(any(dups)) # bad form but allowed by list()
        onames[dups] <- paste0(onames[dups],".dup", 1:sum(dups))
    names(object) <- onames
    object
}



## the classes specializing JSON character strings
JSONScalar <- setClass("JSONScalar", contains = "character")

.Digits <- ceiling(.Machine$double.digits*log10(.Machine$double.base))

#' Convert a Simple Object to JSON String
#'
#' Convert a simple object (should have no attributes or formal class) to JSON string.
#' Called from some specific interface packages, usually to put quotes and escapes into a string.
#' @param object the object to convert; only its type will be used to select format
#' @param prototype the prototype object, supplied from the evaluator calling this function
typeToJSON <- function(object, prototype) {
    unbox = TRUE; digits = .Digits # these were arguments  but never used other than the defaults
    switch(typeof(object),
           ## the standard vector types
           ## NAs for numeric become NaN; other types are made explicit as vector_R objects
           double =  {
               if(any(is.na(object)))
                   object[is.na(object)] <- NaN
               }, integer = , character = , logical =  {
                   if(any(is.na(object))) {
                       obj <- vector_R(obj)
                       obj[is.na(obj)] <- TRUE
                       return(objectAsJSON(obj, prototype))
                   }
               },
           raw = , complex = { # format the vector in an explicit representation
               return(asJSONData(class(object), typeof(object),
                                 format(object, digits = digits), prototype))
           },
           environment = { object <- as.list(object) }, # will be a dictionary
           NULL = return("null"), #now, types that generate literal text, not to be deparsed
           symbol = return(object), # the unquoted string (but shouldn't get here; cf asServerObject)
           closure = , language = , special = , builtin = {
               return(asJSONData(class(object), typeof(object), deparse(object), prototype))
               },
       {# and finally, serialize it if we don't know anything else to do
           txt <- textConnection("txtC", "w")
           on.exit(close(txt))
           serialize(object, txt)
           close(txt)
           txt <- textConnection(txtC, "r")
           return(asJSONData(class(object), typeof(object), readLines(txt), prototype))
       })
    if(typeof(object) == "double") { ## avoid JSON ambiguity re. float/int
              value <- as.character(as.numeric(object))
              ints <- grepl("^[-]?[0-9]*$", value)
              value[ints] <- paste0(value[ints], ".0")
              if(length(value) != 1)
                  value <- paste0("[", paste(value, collapse = ","), "]")
    }
    else
        ## the auto_unbox arg turns length 1 vectors into scalars; undo that
        value <- as.vector(jsonlite::toJSON(object, auto_unbox = unbox, digits = digits))
    if(grepl("^ *\\[", value))
        value
    else if(isS4(object))
        paste("[", value, "]") # force a vector
    else
        JSONScalar(value)
}

#' Convert the String Returned by a Server Language Interface to an R Object.
#'
#' This is the conversion mechanism for results returned from a server language interface.
#' By default, JSON is used to decode the string.  Otherwise the result of the basic
#' decoding should be provided as argument \code{object}.
#' Should be called by the implementation of ServerEval for individual interface classes.
#' @return the R object implied by the server result.
#' @param string the string to be passed to JSON.
#' @param key the key if a proxy was allowed, otherwise the empty string.
#' @param get the logical controling whether a proxy or a converted value was wanted.
#' @param evaluator the evaluator object that issued the server language expression.
#' @param object If JSON is not used, the call from the server language method should provide the
#' elementary conversion of the result (without using \code{asRObject()} and the string argument
#' should be omitted.  If JSON is used, \code{object} should be computed by default from JSON.
valueFromServer <- function(string, key, get, evaluator,
    object = jsonlite::fromJSON(string, simplifyVector = FALSE, flatten = FALSE)
                          ) {
    ## if key is empty any non-null value is presumably an exception
    if(!nzchar(key)) {
        if(is.null(object))
            return(object)
        ## else go on to decode the presumed exception
    }
    ## honor assigned proxy representations here (other .Rclass entries done in asRObject())
    else if(is.recursive(object)) {
        ns <- names(object)
        if(is.character(ns) && identical(object$.RClass, "AssignedProxy")) {
            object <- new("AssignedProxy", object$.Data, serverClass = object$serverClass,
                         size = object$size)
        }
    }
    ## ServerEval is defined to return a converted value, an assigned proxy
    ## or a dictionary with the R object class information
    if(is(object, "AssignedProxy")) { # check for a proxy class
        object <- evaluator$ProxyClassObject(object)
        ## at this point, unless we are trying to force conversion, just return
        ## Note that error conditions will never return a proxy object so we
        ## couldn't have got here if an error occurred
        if(!identical(get, TRUE))
            return(object)
    }
    ## now interpret the result as an R object.
    object <- asRObject(object, evaluator)
    if(is(object, "InterfaceCondition"))
        doCondition(object)
    else
        object
}

doCondition <- function(object) {
    evaluator <- object@evaluator
    msg <-  object@message
    value <- object@value
    cond <- class(object)[[1]]
    switch(cond,
           InterfaceError = , InterfaceWarning = {}, # standard
           msg <- gettextf("(%s) %s", cond, msg))
    if(is(object, "InterfaceError"))
        stop(msg, call.=FALSE)
    else if(is(object, "InterfaceWarning"))
        warning(msg, call.=FALSE)
    else {
        message(msg)
        base::signalCondition(cond)
    }
    value
}

#' Plain Double Quote for Names
#'
#' Utility to surround names of classes, etc with double quotes (not the single quotes of
#' shQuote or the fancy quotes of dQuote)
#' @return the string with quotes.  But empty strings stay empty.
#' @param what the input string.  Should be a name or something without quotes at least.
nameQuote <- function(what) {
    if(length(what))
        paste0('"', what, '"')
    else
        character()
}

## produce a quoted list of names separated by commas
quoteList <- function(what, more) {
    if(missing(more))
        paste(nameQuote(what), collapse = ", ")
    else
        paste(what, nameQuote(more), sep = " = ", collapse = ", ")
}

