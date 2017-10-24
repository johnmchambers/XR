## Classes that are proxies for interfaces

#' A Class to Describe Classes in the Server Language
#'
#' Used by initialize() methods for proxy class objects and therefore exported for the sake of
#' packages using the XR model.  Not typically needed by end users.
#' @field ServerClass the name of the corresponding server language class.
#' @field language the name of the server language (locked)
#' @field evaluatorClass the class for an interface evaluator for this proxy class.
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
                                           .proxyClass = "ProxyClass", .ev = "Interface",
                                           serverObject = function(value) {
                                               if(missing(value))
                                                   .ev$ProxyClassObject(.proxyObject)
                                               else {
                                                   if(is(value, "ProxyClassObject"))
                                                       value <- value$.proxyObject
                                                   if(!is(value, "AssignedProxy"))
                                                       stop(gettextf("The server object to assign must be a proxy; got class %s",
                                                                     XR::nameQuote(class(value))))
                                                   .proxyObject <<- value
                                               }
                                           }
                                           ),
                                contains = "ProxyObject"
                                )


ProxyClassObject$methods(
    size = function() {
        'returns the server-language size of the object, possibly NA'
        .proxyObject@size
       },
    proxyName = function() {
        'the character string under which the server language object is assigned.
         Useful to examine the object in a shell for the server language.'
        as.character(.proxyObject)
    }
    )

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

.proxyObjectFields <- names(ProxyClassObject$fields())
## to assist in extending proxy classes, the show() method is
## defined as a functional method, with access to the XR namespace
setMethod("show", "ProxyClassObject",
          function(object) {
              thisClass <- class(object)
              cat(gettextf("R Object of class %s, for ",
                           XR::nameQuote(thisClass)))
              methods::show(object$.proxyObject)
              if(thisClass != "ProxyClassObject") { # a subclass
                  fields <- names(object$.refClassDef@fieldClasses)
                  fields <- fields[!fields %in% .proxyObjectFields]
                  for (fi in fields) {
                      cat("Field \"", fi, "\":\n", sep = "")
                      methods::show(object$field(fi))
                  }
              }
          })
                  

ProxyClassObject$methods(
    show = function() {
        cat(gettextf("R Object of class %s, for ",
                     nameQuote(class(.self))))
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
#' @param save If the proxy class is being defined in an application package,
#' use this to write to a source file (see Ch. 12 of Extending R)
#'   Default \code{FALSE}, if the proxy class is being assigned in the installation or load of a package.
#' @param objName When using the \code{save=} argument to write R code, use this name in the
#' assignment expression for the generator object.  By default, the name of the class.
#' @param ... extra arguments to pass on to \code{setRefClass()}.
#' @param docText metadata information supplied by the interface for a particular
#' server language.
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
                          docText = NULL) {
    ## in the case everything is specified (usually after a call with save=file)
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
        ## construct the $initialize() method.
        if(nzchar(module))
            importModule <- substitute(.evaluator$Import(SERVERMODULE), list(SERVERMODULE = module))
        else
            importModule <- NULL
        initMethod <- eval(substitute(
            function(..., .evaluator,
                     .serverObject) {
                ## $initialize() method generated in XR::setProxyClass()
                ## (can't use default in arg list, substitute doesn't find it)
                if(missing(.evaluator)) {
                    if(missing(.serverObject))
                        .evaluator <- XR::getInterface(ICLASS, .makeNew = FALSE)
                    else
                        .evaluator <- XR::proxyEvaluator(.serverObject)
                }
                if(!nargs() && is.null(.evaluator))
                    return() # allow prototype objects without a call to the server language
                if(missing(.serverObject)) {
                    if(is.null(.evaluator)) # this was the first use (unlikely!)
                        .evaluator <- XR::getInterface(ICLASS)
                    IMPORT
                    ## Note that this interprets ... as arguments to the server initializer, NOT as fields
                    ## in a superclass.  You need a specialized $initialize() method to mix args, fields
                    .serverObject <- .evaluator$New(SERVERCLASS, SERVERMODULE, ...)
                }
                else if(!missing(...)) # Specifying .serverobject= allows superclass and/or fields in ...
                        initFields(...)
                if(is(.serverObject, "ProxyClassObject"))
                    proxy <- .serverObject$.proxyObject
                else
                    proxy <- .serverObject # had better be an AssignedProxy
                .proxyObject <<- proxy
                .ev <<- .evaluator
            }, list(LANGUAGE = language, SERVERCLASS = ServerClass,
                    ICLASS = evaluatorClass, SERVERMODULE = module,
                    IMPORT = importModule)))
        ## construct the ServerClassInfo() method.  Must be executable as a pure
        ## function (used by dumpProxyClass)
        infoMethod <- function() list()
        ## the list to be returned by the method
        body(infoMethod) <- list(ServerClass = ServerClass, ServerModule = module,
                                 language = language, evaluatorClass = evaluatorClass,
                                 proxyFields = names(fields), proxyMethods = c("initialize", "ServerClassInfo", names(methods)),
                                 proxyContains = contains, proxyObjectClass = proxyObjectClass)
        ## enter the R methods for the server language methods & fields
        methods <- c(list(initialize = initMethod, ServerClassInfo = infoMethod),
                     methods)
        if(identical(save, FALSE)) {  # do the assignment of the class now
            generator <- setRefClass(Class_R,
                                     contains = c(contains, proxyObjectClass),
                                     fields = fields,
                                     where =  where)
            do.call(generator$methods, methods)
            ## assign in this evaluator's proxyClassTable, in case a previous search
            ## for the proxy class had failed, causing NA to be stored there.
            if(is(evaluator, "Interface"))
                base::assign(Class, Class_R, envir = evaluator$proxyClassTable)
            generator
        }
        else {
            dumpProxyClass( save, Class_R, contains, fields, methods, name = objName, docText = docText)
        }
    }
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
inferX <- function(methodName, xMethod, language) {
    ## the stored element can be a function, if ServerClass computes the proxy
    ## method.
    ## Otherwise, the function is defined with "..." for args.  If xMethod is
    ## a character string for the argument list (esp. character() for no arguments),
    ## this information is inserted into the docString for the R method.
    if(is(xMethod, "function")) {
        ## possibly a ProxyFunction
        inferDoc(methodName, xMethod, language)
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
                          nameQuote(class(xMethod))))
        eval(substitute(function(..., .get = NA) {
            DOCSTRING
            .ev$MethodCall(.proxyObject, WHAT, ..., .get = .get)
        }, list(WHAT = methodName,
                DOCSTRING = docString)))
    }
}

## Insert a server documentation string into a method definition
inferDoc <- function(methodName, fun, language) {
    if(is(fun, "ProxyFunction")) {
        doc <- fun@serverDoc
        if(!length(doc))
            doc <- ""
        args <- fun@serverArgs
        usage <- gettextf("%s Method: %s(%s)", language, methodName,
                          paste(args, collapse = ", "))
        doc[[1]] <- paste(usage, doc[[1]], sep ="\n")
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
                             paste(nameQuote(unames[is.na(match(unames, xnames))]), collapse = ", ")))
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
                             paste(nameQuote(unames[is.na(match(unames, xnames))]), collapse = ", ")))
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
          function(.Object, name = "", module = "", prototype = function(...) NULL, evaluator = getInterface(), ..., .get = NA, .Data = NULL, save = FALSE, objName = name, docText = NULL) {
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
                          nameQuote(class(evaluator))))
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
                  evaluator$SaveProxyFunction(save, .Object, objName, docText)
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

dumpProxyClass <- function(save, ProxyClass, contains, fields, methods, name, docText = NULL) {
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
    info <- methods[["ServerClassInfo"]]
    pc <- info()
    fields <- pc$proxyFields
    if(length(docText)) { # write some documentation comments (default, roxygen) with a preliminary class defn if needed.
        ## Bug in methods::setRefClass: can't include 0 length fields arg. (R3.3.0)
        ## Until fixed, following workaround
        if(length(fields))
            classExpr <- gettextf("%s <- setRefClass(%s, contains = c(%s), fields = c(%s))", ProxyClass, nameQuote(ProxyClass),
                              paste(nameQuote(c(contains, "ProxyClassObject")), collapse = ", "),
                                  paste(nameQuote(fields), collapse = ", "))
        else
            classExpr <- gettextf("%s <- setRefClass(%s, contains = c(%s))", ProxyClass, nameQuote(ProxyClass),
                              paste(nameQuote(c(contains, "ProxyClassObject")), collapse = ", "))
        createRoxygen(new("ProxyClass"), con, docText, classExpr)
    }
    text <- gettextf("%s <- XR::setProxyClass(%s, module = %s,", ProxyClass, nameQuote(pc$ServerClass), nameQuote(pc$ServerModule))
    text <- c(text, gettextf("    evaluatorClass = %s, language = %s, proxyObjectClass = %s,",
                             nameQuote(pc$evaluatorClass), nameQuote(pc$language), nameQuote(pc$proxyObjectClass)))
    contains <- pc$proxyContains
    if(length(contains))
        text <- c(text, gettextf("    contains = %s,", deparse(contains)))
    if(length(fields))
        text <- c(text, "    methods = list(),", .dumpProxyFields(fields))
    else
        text <- c(text, "    methods = list()")
    text <- c(text, "    )")
    if(length(methods))
        text <- c(text, "",  .dumpLocalMethods(ProxyClass, methods))
    text <- c(text, "")
    writeLines(text, con)
    TRUE
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
    mnames <- names(methods)
    lastChar <- rep(",", length(methods))
    lastChar[[length(methods)]] <- ")"
    for( i in seq_along(methods)) {
        what <- mnames[[i]]
        def <- deparse(methods[[i]])
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
               if(length(docText)) {
                   doc@title <- docText[1]
                   docText <- docText[-1]
               }
               else
                   doc@title <- ""
               while(length(docText)  && !nzchar(docText[[1]])) # throw leading empty lines
                   docText <- docText[-1]
               doc@description <- c(doc@description, docText)
               doc
           })

setMethod("makeProxyDoc", "ProxyFunction",
          function(object, docText, language = "Proxy", ...) {
              if(length(object@serverDoc))
                  docText <- c(docText, "", gettextf("[%s Documentation]", language),object@serverDoc)
              doc <- callNextMethod()
              doc@usage <- gettextf("%s(%s)",object@name, paste(object@serverArgs, collapse = ", "))
              doc
          })

createRoxygen <- function(object, con, docText, setText = character(), evaluator = getInterface()) {
    ## create a doc object
    language <- evaluator$languageName
    doc <- makeProxyDoc(object, docText, language)
    rox <- c(doc@title,"", doc@description)
    if(length(doc@details))
        rox <- c(rox, "", doc@details)
    if(length(doc@usage))
        rox <- c(rox, gettextf("@section %s Functions:", language),
                 doc@usage)
    if(length(doc@sections)) {
        sections <- doc@sections
        what <- names(sections)
        for(i in seq_along(sections))
            rox <- c(rox, gettextf("@section %s:", what[[i]]), sections[[i]])
    }
    ## export:  this should be optional?
    rox <- c(rox, "@export")
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
                                    name, nameQuote(name), "character()"),
                           ""), con)
          })

#' Write A Proxy Function to a File or Connection
#'
#' This function is called by the \code{$SaveProxyFunction()} method of an evaluator object.  It is exported
#' for the convenience of packages inheriting from XR and would not normally be called by a user.
#' @param file the file or connection for writing the function text.
#' @param object,objName,docText arguments supplied by the evaluator method.
dumpProxyFunction <-
function(file, object, objName = object@name, docText) {
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
    if(length(docText)) {
        .get <- as.list(object)$.get
        if(is.null(.get)) .get <- "NA" else .get <- deparse(.get)
        settext <- c(gettextf("%s <- function(..., .ev = XR::getInterface(), .get = %s)",
                              objName, .get), "    NULL", "")
        createRoxygen(object, con, docText, settext)
    }
    cat(gettextf("%s <- ", objName), file = con)
    dput(object, con)
    cat("\n", file = con)
}

## proxyName defined earlier (in Interface.R)
#' @describeIn proxyName this class has a proxy object as a field.
setMethod("proxyName", "ProxyClassObject",
          function(x)
              callGeneric(x$.proxyObject))

#' The Evaluator Function Object Referred to from a Proxy Objec
#'
#' Any proxy for a server language object contains a reference to the interface evaluator object
#' used to create the proxy object.  This function retrieves the evaluator (whether or not there is
#' a proxy class for this object).  The function is called from specialized methods for particular
#' server langauge classes, as part of a package using the XR package.  End users will not typically
#' need to call it directly; it is exported to simplify life for the extending package.
#' @param object any proxy object
proxyEvaluator <- function(object) {
    if(is(object, "AssignedProxy"))
        object@evaluator
    else
        object$.ev
}

#' Test if an Object is a Proxy
#'
#' Returns \code{TRUE} if \code{object} is either a simple proxy or an object from a proxy class.
#' @param object Any object.
isProxy <- function(object)
    is(object, "AssignedProxy") || is(object, "ProxyClassObject")
