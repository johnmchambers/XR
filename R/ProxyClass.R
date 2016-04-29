## Classes that are proxies for interfaces

## a description of a class in the server language
ProxyClass <- setRefClass("ProxyClass",
                          fields = c(ServerClass = "character",
                          ServerModule =  "character",
                          language = "character",
                          evaluatorClass = "character"))
## want to prevent adding non-proxy methods and fields
ProxyClass$lock("language")
#' A Class for Objects that are Proxies for Specific Server Class Objects
#'
#' This class is extended by all specific proxy classes for a particular language.
#' If a proxy object is returned from the server language whose server class matches a
#' defined proxy class, then an object from that class is generated.
#' @field .proxyObject the actual proxy reference
#' @field .proxyClass the description of the server language class (name, module, langauge)
#' @field .ev the evaluator that produced this proxy object.
#' @template reference
ProxyClassObject <- setRefClass("ProxyClassObject",
                                fields = c(.proxyObject =  "AssignedProxy",
                                .proxyClass = "ProxyClass", .ev = "Interface"),
                                contains = "ProxyObject",
                                )

## a convenience method, to give a more informative error message
#' Miscellaneous methods
#'
#' Convenience methods are provided for operator \code{$} to give more informative error messages
#' if \code{AssignedProxy} objects are assumed
#' incorrectly to have a proxy class definition.
#'
#' @param x,name,value Arguments to the operator.
#' @name MiscMethods
#' @template reference
NULL

#' @rdname MiscMethods
setMethod("$", "AssignedProxy",
          function(x, name) {
              stop(gettextf("No proxy class defined for server class %s",
                            x@serverClass))
          })

#' @rdname MiscMethods
setMethod("$<-", "AssignedProxy",
          function(x, name, value) {
              stop(gettextf("No proxy class defined for server class %s",
                            x@serverClass))
          })


#' @describeIn objectAsJSON Gets the object back from the server, then recalls the generic.
#' See the comments under the \code{"AssignedProxy"} method.
setMethod("objectAsJSON", "ProxyClassObject",
          function(object, prototype, level = 1) {
              object <- object$.proxyObject # will retrieve the AssignedProxy
              objectAsJSON(object, prototype, level)
          })

ProxyClassObject$methods(
    show = function() {
        cat(gettextf("R Object of class %s, for ",
                     dQuote(class(.self))))
        methods::show(.proxyObject)
    })


#' Create a Proxy Class
#'
#' Creates a proxy class of a given name and other requirements.
#' Usually infers fields and methods from server language metadata, but can also
#' use explicitly supplied values.  Particular interface packages will typically have
#' a specialized version that calls this function.
#'
#' A proxy class has fields and methods that are created to use the corresponding fields
#' and methods of the server language, through an interface evaluator.  This function
#' normally expects information about the class to be returned by the \code{$ServerClassDef()}
#' method of the evaluator, specialized to the language. It can also be called with explicit
#' lists for the fields and methods.  The actual fields and methods will use the interface to
#' access or call the corresponding code in the server language.
#'
#' @return a generator object for the R class, along with the side effect of setting the class definition.
#' @param Class the name of the class to be used in the proxy, usually just the server language class.
#' @param module the name of the server langauge module if it needs to be imported.
#' @param fields,methods explicit field and method information if this cannot be found by inspection. Normally omitted.
#' @param ServerClass the name of the server language class, normall defaults to \code{Class}.
#' @param where the environment for the class definition.  By default, and nearly always, the namespace of the
#' package in which the call to \code{setProxyClass()} occurs.
#' @param contains explicitly needed superclasses if any.
#' @param evaluatorClass the evaluator class to identify the evaluator, e.g.  \code{"PythonInterface"} for Python.
#' By default, the current evaluator class.
#' @param proxyObjectClass The general class for proxy objects in this interface.  Typically
#' obtained automatically from the \code{prototypObject} field of the evaluator.
#' @param language the server language, taken from the evaluator if one is found.
#' @param readOnly character vector of any field names that should be marked read-only.
#' @param save If the proxy class is being defined in an application package, supply this as
#' an environment for a load action or use it to write to a source file (see Ch. 12 of Extending R)
#'   Default \code{FALSE}, if the proxy class is being used in this session only.
#' @param objName When using the \code{save=} argument to write R code, use this name in the
#' assignment expression for the generator object.  By default, the name of the class.
#' @param ... extra arguments to pass on to \code{setRefClass()}.
#' @template reference
setProxyClass <- function(Class, module = "",
                          fields = character(), methods = NULL,
                          ServerClass = Class,
                          where = topenv(parent.frame()),
                          contains = character(),
                          evaluatorClass,
                          proxyObjectClass = "ProxyClassObject",
                          language = if(is.null(evaluator)) "" else evaluator$languageName,
                          readOnly = NULL,
                          ...,
                          save = FALSE,
                          objName = Class,
                          docText = NULL,
                          docFunction = createRoxygen) {
    ## in the case everything is specified (usually after a dumpProxyClass())
    ## construct the reference class with no server side computation
    if(!(is.null(fields) || is.null(methods) || missing(language))) {
        Class_R <- paste0(Class,"_",language)
        generator <- setRefClass(Class_R,
                                 contains = c(contains, proxyObjectClass),
                                 fields = fields, methods = methods,
                                 where = where)
    }
    else {
        if(missing(evaluatorClass)) {
            evaluator <-  XR::getInterface()
            if(is.null(evaluator))
                stop("No server language interface has been started.")
            evaluatorClass <- class(evaluator)
        }
        else
            evaluator <- XR::getInterface(evaluatorClass)
        if(!(nzchar(language) && nzchar(evaluatorClass)))
            stop("evaluatorClass and language must be non-empty strings")
        Class_R <- paste0(Class, "_", language)
        if(is.null(methods)) { # methods must be given as a list to suppress metadata
            if(is.null(evaluator))
                stop(gettextf("No server language interface of class %s has been started.",
                              nameQuote(evaluatorClass)))
            str <- evaluator$ServerClassDef(ServerClass, module, ...)
            methods <- inferXMethods(str$methods, language)
            getString <- evaluator$propertyFormat; setString <- gettextf("%s = %s", getString, "%s")
            if(is.null(readOnly) && length(str$readOnly))
                readOnly <- str$readOnly
            fields <- inferXFields(c(str$fields, fields), readOnly, getString, setString)
            if(missing(proxyObjectClass)) # may be language-specific, for asServerObject() methods
                proxyObjectClass <- class(evaluator$prototypeObject)
        }
        noSet <- !identical(save, FALSE) && isClass(Class_R, where = where) &&
                 classDefIsLocked(getClass(Class_R, where = where))
        if(noSet){ # just dump the current version
            generator <- classGeneratorFunction(getClass(Class_R, where = where), where = where)
        }
        else {
            generator <- setRefClass(Class_R,
                                     contains = c(contains, proxyObjectClass),
                                     fields = fields,
                                     where = where)
            ## construct the $initialize() method.
            ## "language" is specified before user-supplied args and locked
            if(nzchar(module))
                importModule <- substitute(evaluator$Import(SERVERMODULE), list(SERVERMODULE = module))
            else
                importModule <- NULL
            initMethod <- eval(substitute(
                function(..., evaluator,
                         .serverObject) {
                    if(missing(evaluator))
                        evaluator <- XR::getInterface(ICLASS)
                    if(missing(.serverObject)) {
                        ## (can't use default in arg list, substitute doesn't find it)
                        IMPORT
                        .serverObject <- evaluator$New(SERVERCLASS, SERVERMODULE, ...)
                    }
                    if(is(.serverObject, "ProxyClassObject"))
                        proxy <- .serverObject$.proxyObject
                    else
                        proxy <- .serverObject ## had better be an AssignedProxy
                    .proxyObject <<- proxy
                    .ev <<- evaluator
                }, list(LANGUAGE = language, SERVERCLASS = ServerClass,
                        ICLASS = evaluatorClass, SERVERMODULE = module,
                        IMPORT = importModule)))
            ## construct the ServerClassInfo() method.  Must be executable as a pure
            ## function (used by dumpProxyClasses)
            infoMethod <- function() list()
            ## the list to be returned by the method
            body(infoMethod) <- list(ServerClass = ServerClass, ServerModule = module,
                                     language = language, evaluatorClass = evaluatorClass,
                                     proxyFields = names(fields), proxyMethods = c("initialize", "ServerClassInfo", names(methods)),
                                     proxyContains = contains, proxyObjectClass = proxyObjectClass)
            ## enter the R methods for the server language methods & fields
            methods <- c(list(initialize = initMethod, ServerClassInfo = infoMethod),
                         methods)
            if(noSet) {} # just the proxy
            else
                do.call(generator$methods, methods)
            ## assign in this evaluator's proxyClassTable, in case a previous search
            ## for the proxy class had failed, causing NA to be stored there.
            if(is(evaluator, "Interface"))
                base::assign(Class, Class_R, envir = evaluator$proxyClassTable)
        }
        if(identical(save, FALSE)) {}
        else {
            dumpProxyClass(generator, save, Class_R, contains, fields, name = objName, docText = docText, docFunction = docFunction)
        }
    }
    generator
}



inferXMethods <- function(methodDefs, language) {
    if(length(methodDefs) == 0)
        return(list())
      methodNames <- allNames(methodDefs)
      named <- nzchar(methodNames)
      if(!all(named)) {
          ## if not named, element better be a character string
          bad <- any(sapply(methodDefs[!named], function(xMethod)is(xMethod, "character") && length(xMethod) == 1))
          if(bad) # a more informative error message would be nice
              stop("Any unnamed xMethods should be single character strings")
          methodNames[!named] <- methodDefs[!named]
          methodDefs[!named] <- list(NULL)
      }
      for(i in seq_along(methodDefs))
          methodDefs[[i]] <- inferX(methodNames[[i]], methodDefs[[i]], language)
      methodDefs
  }

## create a method from:  just the name, the name and arguments, or a function
inferX <- function(what, xMethod, language) {
    methodName <- what
    ## the stored element can be a function, if ServerClass computes the proxy
    ## method.
    ## Otherwise, the function is defined with "..." for args.  If xMethod is
    ## a character string for the argument list (esp. character() for no arguments),
    ## this information is inserted into the docString for the R method.
    if(is(xMethod, "function")) {
        ## possibly a ProxyFunction
        inferDoc(xMethod, language)
    }
    else {
        if(is.null(xMethod))
            docString <- gettextf("%s method %s(...)",language, methodName)
        else if(is(xMethod, "character")) {
            args <- xMethod
            docString <- gettextf("%s method %s(%s)", language, methodName,
                              paste(args, collapse = ", "))
        }
        else
            stop(gettextf("Expected NULL, an arg list or the method as a function: got class %s",
                          dQuote(class(xMethod))))
        eval(substitute(function(..., .get = NA) {
            DOCSTRING
            .ev$MethodCall(.proxyObject, WHAT, ..., .get = .get)
        }, list(WHAT = methodName,
                DOCSTRING = docString)))
    }
}

## Insert a server documentation string into a method definition
inferDoc <- function(fun, language) {
    if(is(fun, "ProxyFunction") && length(fun@serverDoc)) {
        doc <- fun@serverDoc
        doc[[1]] <- gettextf("%s Documentation: %s", language, doc[[1]])
        ll <- as.list(body(fun))
        if(identical(ll[[1]], as.name("{")))
            ll <- base::append(ll, doc, 1)
        else
            ll <- list(as.name("{"), doc, body(fun))
        body(fun) <- as.call(ll)
    }
    fun
}

inferXFields <- function(xFields = character(), readOnly = NULL,
                         getString = "%s.%s", setString = "%s.%s = %s") {
    ## input is a vector with elements xname=class or just xname
    ## where xname is the server field name and class is the R
    ## class to which it should be coerced
    ##
    ## output is a list of functions, the field accessor functions for a reference class.
    if(!length(xFields))
        return(list())
    fieldNames <- allNames(xFields)
    typed <- nzchar(fieldNames)
    fieldNames[!typed]  <- xFields[!typed]
    if(any(duplicated(fieldNames))) {
        ## this presumably means both from metadata and specified; we give preference
        ## to the first occuring, which is to the metadata as currently implemented
        w <- -which(duplicated(fieldNames))
        xFields <- xFields[-w]
        fieldNames <- fieldNames[-w]
    }
    fieldFuns <- vector("list", length(xFields))
    if(length(readOnly))
        readOnly <- fieldNames %in% readOnly
    else
        readOnly <- rep(FALSE, length(fieldNames))
    for(i in seq_along(xFields)) {
        namei <- fieldNames[[i]]
        geti <- substitute(.ev$Eval(GET, proxy),
                           list(GET = gettextf(getString, "%s", namei)))
        if(readOnly[[i]])
            seti <- substitute(stop(
                gettextf("Server field \"%s\" of server class \"%s\" is read-only",
                         WHAT, proxy@serverClass), call.=FALSE),
                               list(WHAT = namei))
        else {
            seti <- substitute(.ev$Command(SET, proxy, value),
                               list(SET = gettextf(setString, "%s", namei, "%s")))
            if(typed[[i]]) { # can't coerce if the field is returned as proxy
                ## geti <- substitute({ value <- WHAT; as(value, CLASS)},
                ##               list(WHAT = geti, CLASS = xFields[[i]]))
                seti <- substitute({ value <- as(value, CLASS); WHAT},
                                   list(WHAT = seti, CLASS = xFields[[i]]))
            }
        }
        fieldFuns[[i]] <- eval(substitute(function(value) {
            proxy <- get(".proxyObject", envir = .self)
            if(missing(value))
                GETI
            else {
                SETI
                invisible(value)
            }
        }, list(GETI = geti, SETI = seti)))
    }
    names(fieldFuns) <- fieldNames
    fieldFuns
}


.asMethodList <- function(methods) {
    if(is.character(methods)) {
        value <- vector("list", length(methods))
        names(value) <- methods # return a vector of NULLs with the names
        value
    }
    else if(is.list(methods)) { # a list, with functions or argument lists
        bad <- rep(FALSE, length(methods))
        mnames <- allNames(methods)
        nullList <- list(NULL)
        for(i in seq_along(methods)[!nzchar(mnames)]) { # the unnamed elements better be strings
            mi <- methods[[i]]
            if(is.character(mi) && length(mi) ==1) {
                methods[i] <- nullList
                mnames[[i]] <- mi
            }
            else
                bad[[i]] <- TRUE
        }
        if(any(bad))
            stop(gettextf("Element(s) %s of the methods list were not named and not single strings",
                          paste(seq_along(methods)[bad], collapse=", ")))
        names(methods) <- mnames
        methods
    }
}


#' The Definition of a Server Language Class
#'
#' @field fields,methods named lists of the server language fields and methods to be exported
#' @field operators named list of the server class methods that are "operator overloading" of functions.
#' @field readOnly the names of any fields that should be made read-only in the R class
#' @template reference
ServerClassDef <- setRefClass("ServerClassDef",
                           fields  = c( methods = "namedList", fields = "namedList",
                           operators = "namedList", readOnly = "character"))

ServerClassDef$methods("initialize" =
          function(reflection = list(...), methods = NULL, fields = NULL, ...) {
              callSuper(...)
              resolveProxyMethods(.self, reflection$methods, methods)
              resolveProxyFields(.self, reflection$fields, fields)
          })

## resolve xmethods, from reflectance if any, with user-supplied methods
resolveProxyMethods <- function(.Object, xmethods, methods) {
    if(length(methods))
        methods <- .asMethodList(methods)
    if(is.null(xmethods)) { #no reflectance
        xmethods <- methods
    }
    else { # user supplies a list of which methods should be exported
        xnames <- names(xmethods)
        unames <- names(methods)
        if(any(is.na(match(unames, xnames))))
            warning(gettextf("Methods to be exported (%s) were not found in reflectance",
                             paste(dQuote(unames[is.na(match(unames, xnames))]), collapse = ", ")))
        xpt <- xnames %in% unames
        xmethods <- xmethods[xpt]
        if(length(xmethods) == 0)
            message("Proxy class will have no server language methods exported")
    }
    .Object$methods <- as(xmethods, "namedList")
}

resolveProxyFields <- function(.Object, xfields, fields) {
    if(length(fields))
        fields <- .asMethodList(fields)
    if(is.null(xfields)) { #no reflectance
        xfields <- fields
    }
    else { # user supplies a list of which fields should be exported
        xnames <- names(xfields)
        unames <- names(fields)
        if(any(is.na(match(unames, xnames))))
            warning(gettextf("Fields to be exported (%s) were not found in reflectance",
                             paste(dQuote(unames[is.na(match(unames, xnames))]), collapse = ", ")))
        xpt <- xnames %in% unames
        xfields <- xfields[xpt]
    }
    .Object$fields <- as(xfields, "namedList")
}

#' A Class for Proxy Functions
#'
#' A class for functions in R that call functions in a server language.  The arguments in a call are converted to
#' equivalent server language objects, via \code{\link{asServerObject}()}.  These usually include proxy
#' objects in R for results previously computed through the same interface evaluator.
#'
#' This class is always subclassed for a particular server language.  Proxy functions for that
#' language will use a corresponding evaluator to find metadata about the server function.
#'
#' @slot .Data the function
#' @slot name the name of the server language function
#' @slot module the name of the module, if that needs to be imported
#' @slot evaluatorClass the class for the evaluator, identifying which server lanaguage is involved.
#' @slot serverDoc documentation for the server language function
#' @slot serverArgs the formal arguments of the server language function, if known
#' @template reference
ProxyFunction <- setClass("ProxyFunction",
                          slots = c(name = "character", module = "character", evaluatorClass = "character",
                                    serverDoc = "character", serverArgs = "character"),
                          contains = c("function", "ProxyObject"))

setMethod("initialize", "ProxyFunction",
          function(.Object, name = "", module = "", prototype = function(...) NULL, evaluator = getInterface(), ..., .get = NA, .Data = NULL, save = FALSE, objName = name, docText = NULL, docFunction = createRoxygen) {
              ## .Data is set either directly from code in a setup step
              ## or at the end of an initialize() method specialized to a server language
              if(is.null(.Data)) {
                  args <- as.list(formals(prototype))
                  anames <- paste(allNames(args), collapse = ", ")
                  if(is(evaluator, "character") && length(evaluator) == 1) # the class for the evaluator or ""
                  {}
                  else if(is(evaluator, "Interface"))
                      evaluator <- class(evaluator)
                  else
                      stop(gettextf(
                          "The evaluator= argument should be an evaluator or its class; got %s",
                          dQuote(class(evaluator))))
                  deflt <- substitute(XR::getInterface(WHAT), list(WHAT = evaluator))
                  formals(prototype) <- c(args, list(.evaluator = deflt, .get = .get))
                  text <- gettextf('.evaluator$Call(%s,%s, .get = .get)', nameQuote(name), anames)
                  body(prototype) <- parse(text = text)[[1]]
                  .Object@.Data <- prototype
                  .Object@name <- name
                  .Object@module <- module
                  .Object@evaluatorClass <- class(evaluator)
              }
              else {
                  ## set the slots directly
                  .Object@name <- name
                  .Object@module <- module
                  .Object@.Data <- .Data
              }
              if(!identical(save, FALSE))
                  evaluator$SaveProxyFunction(save, .Object, objName, docText, docFunction)
              callNextMethod(.Object, ...)
          })

#' @describeIn asServerObject
#' a proxy function just turns into its server language name.
setMethod("asServerObject", "ProxyFunction",
    function (object, prototype)
    {
        object@name
    }
)

#' @describeIn asServerObject
#' an object from a proxy class will be replaced by the name of the referenced object
setMethod("asServerObject", "ProxyClassObject",
    function (object, prototype)
    {
        as.character(object$.proxyObject)
        ## this would be more independent of implementation:
        ## asServerObject(object$.prototypeObject, prototype)
    }
)

#' Dump Explicit Definition of Proxy Classes and Functions
#'
#' To avoid getting server language class and function information at load time,
#' which may not work with narrow-minded
#' package repositories such as CRAN, these functions generate R code to define the proxy classes
#' and functions explicitly.
#' Normally not called directly.
#'
#' The functions will usually be called from  \code{\link{setProxyClass}}
#' or an initializing method for a subclass of \code{\link{ProxyFunction}},
#' with an argument for the file or open connection to which the output will be sent.
#'
#' If called with the working directory set to the package source directory,
#' the files go into the application package's R source, defining the same
#' proxy functions or classes explicitly.
#' @param gen   If \code{save} is an environment, the generator object for the class. ignored otherwise.
#' @param save where to write the generated R code.  If simply \code{TRUE}, a name is constructed from the function or  class names.
#' May also be a connection.  If the file or connection is not open, it is opened and then
#' closed on exit.  Allowed to be an environment (not currently used).
#' @param ProxyClass the server language class name.
#' @param docText,docFunction  \code{docText} optional
#' documentation text for roxygen-style comments to be inserted when the definition is
#' being saved; \code{docFunction}, the function to generate the documentation.  By default uses roxygen-style comments.
#' @template reference
dumpProxyClass <- function(gen, save, ProxyClass, contains, fields, name, docText = NULL, docFunction = createRoxygen) {
    if(identical(save, TRUE))
        save <- .dumpFileName(ProxyClass)
    if(is(save, "connection") && isOpen(save))
        con <- save
    else {
        if(is(save, "connection"))
            con <- open(save, "w")
        else
            con <- base::file(save, "w")
        on.exit(close(con))
    }
    if(is(save, "environment")) {
        assign(name, gen, envir = save)
        return(NULL)
    }
    classDef <- getClass(ProxyClass)
    if(!extends(ProxyClass, "ProxyClassObject"))
        stop(gettextf("Class %s is not a proxy class; should extend \"ProxyClassObject\"",
                      dQuote(ProxyClass)))
    if(length(docText)) { # write some documentation comments (default, roxygen) with a preliminary class defn if needed.
        classExpr <- gettextf("%s <- setRefClass(%s, contains = c(%s), fields = c(%s))", name, dQuote(ProxyClass),
                              paste(dQuote(c(contains, "proxyObject")), collapse = ", "),
                              paste(dQuote(fields), collapse = ", "))
        docFunction(con, docText, classExpr)
    }
    rmethods <- classDef@refMethods
    info <- rmethods[["ServerClassInfo"]]
    pc <- info()
    text <- gettextf("%s <- XR::setProxyClass(%s, module = %s,", name, nameQuote(pc$ServerClass), nameQuote(pc$ServerModule))
    text <- c(text, gettextf("    evaluatorClass = %s, language = %s, proxyObjectClass = %s,",
                             nameQuote(pc$evaluatorClass), nameQuote(pc$language), nameQuote(pc$proxyObjectClass)))
    contains <- pc$proxyContains
    if(length(contains))
        text <- c(text, gettextf("    contains = %s,", deparse(contains)))
    fields <- pc$proxyFields
    if(length(fields))
        text <- c(text, "    methods = list(),", .dumpProxyFields(fields))
    else
        text <- c(text, "    methods = list(), fields = character()")
    text <- c(text, "    )")
    methods <- pc$proxyMethods
    if(length(methods))
        text <- c(text, "",  .dumpLocalMethods(ProxyClass, methods))
    text <- c(text, "")
    writeLines(text, con)
}

.dumpFileName <- function(classes,thing = "Class") {
    if(length(classes) == 1)
        what <- classes
    else
        what <- paste0(classes[[1]], "---")
    file <- gettextf("Proxy_%s_%s.R", what, thing)
    ## check for wd being a package source directory, if so, use the R/ subdirectory
    if(file.exists("DESCRIPTION") && identical(file.info("R")$isdir, TRUE))
        file <- paste0("R/", file)
    file
}

.dumpOneClass <- function(ProxyClass, gen, con, name = ProxyClass, docText = NULL, docFunction = createRoxygen) {
}

.dumpProxyFields <- function(fieldnames) {
    lfuns <- inferXFields(fieldnames)
    ld <- lapply(lfuns, deparse)
    nf <- length(fieldnames)
    for(i in seq_along(fieldnames)) {
        defi <- ld[[i]]
        defi[[1]] <- paste0(fieldnames[i], " = ", defi[[1]])
        if(i < nf) {
            n <- length(defi)
            defi[[n]] <- paste0(defi[[n]], ",")
        }
        ld[[i]] <- defi
    }
    c("    fields = list(", paste0("      ",unlist(ld)), "      )")
}


.dumpLocalMethods <- function(ProxyClass, methods) {
    text <- gettextf("%s$methods(", ProxyClass)
    mdefs <- getClass(ProxyClass)@refMethods
    lastChar <- rep(",", length(methods))
    lastChar[[length(methods)]] <- ")"
    for( i in seq_along(methods)) {
        what <- methods[[i]]
        fun <- mdefs[[what]]
        def <- deparse(fun@.Data) # without the refMethodDef slots
        n <- length(def)
        def[[1]] <- paste(what, def[[1]], sep = " = ")
        def[[n]] <- paste0(def[[n]], lastChar[[i]])
        text <- c(text, def, "")
    }
    text
}

## augment documentation title with metadata
ProxyDoc <- setClass("ProxyDoc",
                        slots = c(title = "character", description = "character",
                        details = "character", usage = "character", sections = "list"))

setGeneric("makeProxyDoc",
           function(object, docText, language = "Proxy", ...) {
               doc <- ProxyDoc(...)
               docText <- unlist(strsplit(docText, "\n"))
               doc@title <- if(length(docText)) {
                   title <- docText[1]
                   docText <- docText[-1]
                   title
               } else ""
               while(length(docText)  && !nzchar(docText[[1]]))
                   docText <- docText[-1]
               doc@description <- c(doc@description, docText)
               doc
           })

setMethod("makeProxyDoc", "ProxyFunction",
          function(object, docText, language = "Proxy", ...) {
              if(length(object@serverDoc))
                  docText <- c(docText, "", gettextf("[%s Documentation]", language),object@serverDoc)
              doc <- callNextMethod()
              doc@usage <- gettextf("%s(%s) [%s]",object@name, paste(object@serverArgs, collapse = ", "),
                                        language)
              doc
          })

createRoxygen <- function(con, docText, setText, evaluator = getInterface()) {
    ## create a doc object
    doc <- makeProxyDoc(object, docText, evaluator$languageName)
    rox <- c(doc@title,"", doc@description)
    if(length(doc@details))
        rox <- c(rox, "", doc@details)
    if(length(doc@usage))
        rox <- c(rox, "@section Proxy Function:", doc@usage)
    if(length(doc@sections)) {
        sections <- doc@sections
        what <- names(sections)
        for(i in seq_along(sections))
            rox <- c(rox, gettextf("@section %s:", what[[i]]), sections[[i]])
    }
    ## make all lines roxygen-style comments
    which <- !grepl("^#' ", rox)
    if(any(which))
        rox[which] <- paste("#'", rox[which])
    writeLines(c(rox, setText), con)
}

## a generic function to write a dummy or other version of an object
## to correspond to the documentation comments from createRoxygen
setGeneric("writeFakeObject", function(object, con) NULL)

setMethod("writeFakeObject", "refClassRepresentation",
          function(object, con) {
              name <- object@className
              ## todo: get the proxy fields from object@fieldClasses
              writeLines(c(gettextf("%s <- setRefClass(%s, fields = %s)",
                                    name, dQuote(name), "character()"),
                           ""), con)
          })


#' @rdname dumpProxyClass
#'
#' @param object the proxy object, constructed by the initialization method for one of the proxy
#' function classes, such as \code{"PythonFunction"}.
dumpProxyFunction <-
function(file, object, objName = object@name, docText, writeDoc = createRoxygen) {
    if(identical(file, TRUE))
        file  <- .dumpFileName(object@name, "Function")
    if(is(file, "connection") && isOpen(file))
        con <- file
    else {
        if(is(file, "connection"))
            con <- open(file, "w")
        else
            con <- base::file(file, "w")
        on.exit(close(con))
    }
    if(length(docText))
        writeDoc(con, docText, object)
    cat(gettextf("%s <- ", objName), file = con)
    dput(object, con)
}

## proxyName defined earlier (in Interface.R)
setMethod("proxyName", "ProxyClassObject",
          function(x)
              callGeneric(x$.proxyObject))


## temporary definition of two functions until the methods package has exported versions
classDefIsLocked <- function(Class) {
    methods:::.classDefIsLocked(Class)
}

classGeneratorFunction <- function (classDef, env = topenv(parent.frame()))
{
    if (is(classDef, "classRepresentation")) {
    }
    else if (is(classDef, "character")) {
        if (is.null(packageSlot(classDef)))
            classDef <- getClass(classDef, where = env)
        else classDef <- getClass(classDef)
    }
    else stop("argument 'classDef' must be a class definition or the name of a class")
    fun <- function(...) NULL
    body(fun) <- substitute(new(CLASS, ...), list(CLASS = classDef@className))
    environment(fun) <- env
    fun <- as(fun, "classGeneratorFunction")
    fun@className <- classDef@className
    fun@package <- classDef@package
    fun
}
