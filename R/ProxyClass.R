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
ProxyClassObject <- setRefClass("ProxyClassObject",
                                fields = c(.proxyObject =  "AssignedProxy",
                                .proxyClass = "ProxyClass", .ev = "Interface"),
                                contains = "ProxyObject",
                                )

## a convenience method, to give a more informative error message
#' @describeIn AssignedProxy
setMethod("$", "AssignedProxy",
          function(x, name) {
              stop(gettextf("No proxy class defined for server class %s",
                            x@serverClass))
          })

#' @describeIn AssignedProxy
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
#' @param language the server language, taken from the evaluator if one is found.
#' @param readOnly character vector of any field names that should be marked read-only.
#' @param ... extra arguments to pass on to \code{setRefClass()}.
setProxyClass <- function(Class, module = "",
                          fields = character(), methods = NULL,
                          ServerClass = Class,
                          where = topenv(parent.frame()),
                          contains = character(),
                          evaluatorClass,
                          proxyObjectClass = "ProxyClassObject",
                          language = if(is.null(evaluator)) "" else evaluator$languageName,
                          readOnly = NULL,
                          ...) {
    ## in the case everything is specified (usually after a dumpProxyClasses())
    ## construct the reference class with no server side computation
    if(!(is.null(fields) || is.null(methods) || missing(language)))
        generator <- setRefClass(paste0(Class,"_",language),
                                 contains = c(contains, proxyObjectClass),
                                 fields = fields, methods = methods,
                                 where = where)
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
        do.call(generator$methods, c(list(initialize = initMethod, ServerClassInfo = infoMethod),
                                     methods))
        ## assign in this evaluator's proxyClassTable, in case a previous search
        ## for the proxy class had failed, causing NA to be stored there.
        if(is(evaluator, "Interface"))
            base::assign(Class, Class_R, envir = evaluator$proxyClassTable)
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
    ## Otherwise, the function is defined with "..." for args.  If xMethod is,
    ## a character string for the argument list (esp. character() for no arguments),
    ## this information is inserted into the docString for the R method.
    if(is(xMethod, "function")) { }
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
        xMethod <- eval(substitute(function(..., .get = NA) {
            DOCSTRING
            .ev$MethodCall(.proxyObject, WHAT, ..., .get = .get)
        }, list(WHAT = methodName,
                DOCSTRING = docString)))
    }
    xMethod
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

#' Set a Load Action for Proxy Classes
#'
#' Set up a load action to define one or more proxy classes for a package
#' The call to loadProxyClasses() usually comes in the source for an application package
#' that uses an interface to one or more server langauges.
#' The alternative, usually preferred, is to use \code{dumpProxyClass()} to generate an explicit
#' definition in the source for the package.
#' @param classes names of one or more classes in the server language
#' @param modules modules (aka libraries, packages) in which to find the classes
#' @param evaluator the evaluator to use, by default the current evaluator.
#' @param ... arguments to pass on to the setProxyClass() call that will be generated at load tome.
loadProxyClasses <- function(classes, module ="", evaluator = XR::getInterface(), ...) {
    args <- list(...)
    if(length(module) != 1)
        stop("There should be one module specified or \"\" for no import needed.  Got length ",length(module))
    language <- class(evaluator)
    ## build the body of the function to be called at load time
    body <- quote({})
    body[[2]] <- substitute(ev <- XR::getInterface(LANG),
                            list(LANG = language))
    if(nzchar(module))
        body[[3]] <- substitute(ev$Import(MODULE, CLASSES),
                                list(MODULE = module, CLASSES = classes))
    for(i in seq_along(classes)) {
        call <- substitute(setProxyClass(CLASS, module = MOD, where = ns, evaluator = ev),
                           list(CLASS = classes[[i]], MOD = module))
        if(length(args))
            call <- as.call(c(as.list(call), args))
        body[[length(body)+1]] <- call
    }
    f <- function(ns){}
    body(f) <- body
    where <- topenv(parent.frame())
    if(exists(".packageName", envir = where, inherits = FALSE))
        setLoadAction(f, paste0("Proxy.",classes[[1]], if(length(classes) > 1) "..." else ""), where = where)
    else
        evalq(f(where))
    invisible(f)
}

#' The Definition of a Server Language Class
#'
#' @field fields,methods named lists of the server language fields and methods to be exported
#' @field operators named list of the server class methods that are "operator overloading" of functions.
#' @field readOnly the names of any fields that should be made read-only in the R class
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
#' @slot name the name of the server language function
#' @slot the name of the module, if that needs to be imported
#' @slot the class for the evaluator, identifying which server lanaguage is involved.
ProxyFunction <- setClass("ProxyFunction",
                          slots = c(name = "character", module = "character", evaluatorClass = "character"),
                          contains = "function")
#' @describeIn ProxyFunction
#'
setMethod("initialize", "ProxyFunction",
          function(.Object, name = "", module = "", prototype = function(...) NULL, evaluator = getInterface(), ..., .get = NA) {
              ## an escape for the case that dumpProxyFunctions()
              ## was used to avoid the need for server evaluation:
              if(methods::hasArg(".Data")) {
                  ## set the slots directly.  Should really
                  ## do a callNextMethod(), but it's known to be the deffault
                  ## method & there is a bug in setting .Data in that method (R3.2.2)
                  .Object@name <- name
                  .Object@module <- module
                  args <- list(...)
                  for(sl in names(args))
                      slot(.Object, sl) <- args[[sl]]
                  return(.Object)
              }
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
#' which won't work with narrow-minded
#' package repositories such as CRAN, these functions generate R code to define the proxy classes
#' and functions explicitly.
#'
#' The package creator should create the desired proxy functions and/or classes from an interactive session.
#' Then call \code{dumpProxyFunctions()} and/or \code{dumpProxyClasses()} to write
#' files of R source.  The files go into the application package's source, defining the same
#' proxy functions or classes explicitly.
#' @param Classes the names of the server language classes for which proxy classes should have been created.
#' @param file where to write the generated R code.  By default makes up a name from the function or  class names.
#' @param where where to find the function or class definitions; by deafault and usually the global environment.
dumpProxyClasses <- function(Classes, file = .dumpFileName(Classes), where = .GlobalEnv) {
    classes <- getClasses(where)
    ## Find corresponding proxy classes
    ambig <- none <- rep(FALSE, length(Classes))
    for(i in seq_along(Classes)[!grepl("_",Classes, fixed = TRUE)]) {
        proxy <- grep(paste0(Classes[[i]], "_"), classes, fixed = TRUE)
        if(length(proxy) == 1)
            Classes[[i]] <- classes[[proxy]]
        else if(length(proxy) > 1)
            ambig[[i]] <- TRUE
        else
            none[[i]] <- TRUE
    }
    if(any(ambig | none)) {
        msg <- (if(any(none)) gettextf("Proxy classes not found for %s",
                                       quoteList(Classes[none]))
                else character())
        if(any(ambig))
            msg <- c(msg, gettextf("More than one possible proxy for %s",
                                   quoteList(Classes[ambig])))
        stop(paste(msg, collapse = "; "))
    }
    if(is(file, "connection") && isOpen(file))
        con <- file
    else {
        if(is(file, "connection"))
            con <- open(file, "w")
        else
            con <- base::file(file, "w")
        on.exit(close(con))
    }
    for(i in seq_along(Classes))
        .dumpOneClass(Classes[[i]], con)
    file
}

.dumpFileName <- function(classes,thing = "Class") {
    if(length(classes) == 1)
        what <- classes
    else
        what <- paste0(classes[[1]], "---")
    gettextf("Proxy_%s_%s.R", what, thing)
}

.dumpOneClass <- function(ProxyClass, con) {
    classDef <- getClass(ProxyClass)
    if(!extends(ProxyClass, "ProxyClassObject"))
        stop(gettextf("Class %s is not a proxy class; should extend \"ProxyClassObject\"",
                      dQuote(ProxyClass)))
    rmethods <- classDef@refMethods
    info <- rmethods[["ServerClassInfo"]]
    pc <- info()
    text <- gettextf("%s <- XR::setProxyClass(%s, module = %s,", ProxyClass, nameQuote(pc$ServerClass), nameQuote(pc$ServerModule))
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

#' @describeIn dumpProxyClasses
#' @param functions the names of the server language functions for which proxies have been created.
dumpProxyFunctions <-
    function(functions,
             file = .dumpFileName(functions, "Function"),
             where = topenv(parent.frame())) {
    if(is.character(file)) {
        con <- base::file(file, "w")
        on.exit(close(con))
    }
    else
        con <- file
    for(what in functions) {
        def <- get(what, envir = where)
        if(!is(def, "ProxyFunction"))
            stop(gettextf("Expected a ProxyFunction for object %s, got %s",
                          nameQuote(what), nameQuote(class(def))))
        snames <- methods::slotNames(class(def))
        call <- lapply(snames, function(x) methods::slot(def, x))
        names(call) <- snames
        call <- as.call(c(quote(new), class(def), call))
        expr <- substitute(WHAT <- CALL,
                           list(WHAT = as.name(what),
                                CALL = call))
        dput(expr, con)
        cat("\n", file = con)
    }
    invisible(file)
}
