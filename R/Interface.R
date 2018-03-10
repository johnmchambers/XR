#' Class Union to Represent Proxy Objects
#'
#' A virtual class to include all classes that can represent proxy objects
#' in the server language.  May be extended by the interface for a particular language.
#' @name ProxyObject-class
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
#' @field simplify Should lists whose elements are each basic scalars be unlisted?
#' Default FALSE.  May also be a function that takes a possibly simplifiable
#' list as argument and returns the vector/list result.
#' Use this to apply a customized test; e.g., all scalars must have same type.
#' @field propertyFormat The C-style format for a property (i.e., field) in the server language.
#' The default assumes \code{"."} is the field operator, as in all likely server languages so far.
#' @field proxyClassTable Used to keep track of proxy classes encountered
#' @field modules The evaluator's table of currently imported modules.
#' @field serverPath The evaluator's current server language path for importing.
#' @template reference
Interface <-
  setRefClass("Interface",
              fields = list(evaluatorId = "character",
              languageName = "character", proxyCount = "integer",
              prototypeObject = "ProxyObject",
              propertyFormat = "character", proxyClassTable = "environment",
              modules = "environment",
              serverPath = "character", simplify = "ANY"))

#' Classes of objects representing errors or other conditions in a server language
#'
#' Errors and warnings generated in evaluating an expression in the server language will
#' be returned to R as objects from one of these classes.
#' The interface evaluator will normally throw an error for \code{"InterfaceError"} and
#' issue a warning for \code{"InterfaceWarning"}.
#' @aliases InterfaceError-class InterfaceWarning-class
#' @slot message The character string message from the server language evaluator.
#' @slot value In the case of a warning, the object to return after issuing the condition.
#' @slot expr The expression sent to the server language that produced the condition.
#' @slot evaluator The interface evaluator object receiving the condition.
#' @template reference
setClass("InterfaceCondition",
         slots = c(message = "character", value = "ANY", expr = "character", evaluator = "Interface"))

setClass("InterfaceError",
         contains = "InterfaceCondition")

setClass("InterfaceWarning",
         contains = "InterfaceCondition")

.vectors <- c("integer", "numeric", "character", "logical", "complex")
.vectorTypes <- c("integer", "double", "character", "logical", "complex")

typeToClass <- function(type) {
    tt <- match(type, .vectorTypes)
    if(is.na(tt))
        "list"
    else
        .vectors[[tt]]
}

#' Send a Non-scalar Version of an Object
#'
#' Ensures that an object is interpreted as a vector (array) when sent to the server language.
#' The default strategy is to send length-1 vectors as scalars.
#'
#' @return the object, but with the S4 bit turned on.
#' Relies on the convention that XR interfaces leave S4 objects
#' as vectors, not scalars, even when they are of length 1
#' @param object A vector object.  Calling with a non-vector is an error.
#' @template reference
noScalar <- function(object) {
    if(is.vector(object))
        asS4(object)
    else
        stop(gettextf("Object of class %s is not a vector", nameQuote(class(object))))
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
#' @slot serverClass,module The server language class and module for the corresponding object.
#' @slot size The size (usually, length) of the server object, if that makes sense.
#' Can be used to make decisions about handling large objects.
#' @slot evaluator The evaluator object that returned the proxy.  Having this as a slot allows
#' interface computations to operate directly on the proxy, without a user-supplied evaluator.
#' @template reference
AssignedProxy <- setClass("AssignedProxy",
                          slots = c(serverClass = "character", module = "character",
                          size = "integer", evaluator = "Interface"),
                          contains = c("character"))
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

## a table, storing for each interface class a set of actions
## to be taken when an evaluator from that class is initialized
evaluatorActions <- new.env()

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

#' Carry Out an Evaluator Initialization Action
#'
#' This function is called from the Startup() method of an evalautor and is not
#' useful to be called directly.  It is exported to make it visible from within
#' a subclass of "Interface".
#'
#' @param action the action from the table.  Must be an expression or some special
#' class, typically a path element to add to the server path.
#' @param ev the evaluator.
setGeneric("evaluatorAction", function(action, ev)
    stop("No default method for evaluatorAction(), must be a language object or special class"))

#' @describeIn evaluatorAction a language object, just evaluate it.
setMethod("evaluatorAction", "language",
          function(action, ev)
              base::eval(action, ev))

#' @describeIn evaluatorAction a "pathEl" object to add to the server search path.
setMethod("evaluatorAction", "pathEl",
          function(action, ev)
              ev$AddToPath(action@.Data, action@package, action@pos))




#' Add to Table of Search Paths and Import Commands
#'
#' Utilities to add to the table of search paths, import commands and tasks for all evalutors of the
#' specified class.  Called only from analogous functions in packages for specific languages.
#'
#' The server-specific information is added to the table stored by the XR package.  All future
#' evaluators for the specified interface class will have these directories in their search
#' path, will import the module information specified and carry out any other tasks supplied.
#'
#' If a current evaluator for this
#' class exists, it applies all the commands, but \emph{previous} evalutors
#' for this class are not modified.
#'
#' Commands are evaluated in the order of the calls to these functions.  For example, the application
#' package should execute a call to add to the search path before any calls to import modules form
#' the corresponding directory.
#'
#' @param Class the class of the server-specific evalutor.
#' @param onLoad,where used to set up a load action; should be omitted if called from a package source
#' @template reference
#' @name evaluatorActions
NULL

#' @describeIn evaluatorActions
#'
#' Add the directory to the search path of all evaluators of this class.
#'
#' @param directory the directory to add to the search path table.
#' @param package the name of the server-specific interface package.
#' @param pos where in the list of directories to insert this one.  Defaults to the end.
serverAddToPath <- function(Class, directory, package = utils::packageName(topenv(parent.frame())),
                            pos = NA, onLoad = NA, where = topenv(parent.frame())) {
    ## note:  directory is not allowed to be missing.  The server language specializations will
    ## supply a default of the language name, as per $AddToPath().
    el <- pathEl(directory, package = package, pos = pos)
    if(is.na(onLoad)) # set load action if from installation phase
        onLoad <- nzchar(el@package) && # use slot, iniitalize() method will have set it.
            !environmentIsLocked(where) # else can't do evalOnLoad()
    if(onLoad) {
        action <- as.call(list(quote(XR::serverAddToPath),Class, directory,package, pos))
        action$onLoad <- FALSE
        evalOnLoad(action, where = where)
        return(TRUE)
    }
    if(!is.character(directory))
        stop(gettextf(
            "New path element must be a directory as a string, got %s",
            nameQuote(class(directory))))
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
            nameQuote(className)))
    env <- XR::evaluatorActions
    ## add el to the actions -- evalAction() has a method for this class
    actions <- env[[className]]
    if(is.null(actions))
        actions <- list(el) 
    else
        actions <- c(actions, list(el))
    env[[className]] <- actions
    value <- getInterface(className, .makeNew = FALSE)
    if(!is.null(value))
        value$AddToPath(directory, package, pos)
    invisible(TRUE)
}


#' @describeIn evaluatorActions
#'
#' An import command with these arguments will be executed for each new evaluator of this
#' interface class, and for the current evaluator if one exists.
#'
#' @param ... arguments to pass to the evaluator's \code{$Import()} method
serverImport <- function(Class, ...,
                         onLoad = nzchar(packageName(where)) && !environmentIsLocked(where),
                         where = topenv(parent.frame())) {
    ## if from a source package, set load action
    if(onLoad) {
        action <- as.call(list(quote(XR::serverImport),Class, ...))
        action$onLoad <- FALSE
        evalOnLoad(action, where = where)
        return(TRUE)
    }
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
            nameQuote(className)))
    env <- XR::evaluatorActions
    actions <- env[[className]] ### use [[ to get NULL if not there
    expr <- as.call(list(quote(.self$Import), ...))
    if(is.null(actions))
        actions <- list(expr)
    else {
        for(el in actions)
            if(identical(el, expr))  ### catches the EXACT same import request
                return(invisible(FALSE))
        actions <- c(actions, list(expr))
    }
    assign(className, actions, envir = env)
    value <- getInterface(className, .makeNew = FALSE)
    if(!is.null(value))
        eval(expr, envir = value)
    invisible(TRUE)
}

#' @describeIn evaluatorActions
#'
#' An unevalated command or expression for the interface is supplied, typically using
#' \code{quote()} or \code{substitute}.  When an evaluator from the class is created, this
#' command will be evaluated.
#'
#' @param command an \emph{unevaluated} command or expression for the evaluator.
serverTask <- function(Class, command, onLoad = nzchar(packageName(where)), where = topenv(parent.frame())) {
    ## if from a source package, set load action
    if(onLoad) {
        action <- sys.call()
        action[[1]] <- quote(XR::serverTask)
        action$onLoad <- FALSE
        evalOnLoad(action, where = where)
        return(TRUE)
    }
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
            nameQuote(className)))
    env <- XR::evaluatorActions
    ## TODO: should check here for the command being a suitable expression
    action <- command
    action[[1]] <- substitute(.self$WHAT, list(WHAT = command[[1]]))
    actions <- env[[className]] ### use [[ to get NULL if not there
    if(is.null(actions))
        actions <- list(action)
    else
        actions <- c(actions, list(action))
     assign(className, actions, envir = env)
    value <- getInterface(className, .makeNew = FALSE)
    if(!is.null(value)) {
        eval(action, envir = value)
    }
    invisible(TRUE)
}


## a table of the evaluators for all languages that subclass "Interface"
## (see getInterface())
languageEvaluators <- new.env()

#' Get or start an evaluator for an interface
#'
#' Utility functions to manage a table of evaluators, indexed by the evaluator class, typically
#' one class per server language. All are typically hidden by functions or methods for the particular
#' class. \code{rmInterface} and \code{evaluatorNumber} are used by methods and exported so that
#' subclasses of interface evaluators will have access to them.
#'
#' @aliases evaluatorTable
#' @return \code{getInterface()} returns an  interface evaluator for this class, starting one if none exists.
#' @param Class the name of the interface class for this evaluator; by default, the class of the
#' current evaluator. Can also be the class definition object.
#' @param ... arguments, if any, are passed to the generator for the evaluator
#' @param .makeNew can be used to force or prevent starting a new evaluator, if passed as
#' a logical value.  Can also be passed as a function that tests the suitability of a
#' current evaluator, returning TRUE if this one won't do, and a new one should be
#' generated instead (consistent with the ... arguments, presumably).
#'
#' The default is NA, meaning that an existing evaluator is OK, but one should be generated
#' if none exists.  In contrast, FALSE means to return NULL if no matching evaluator exists.
#' @param .select Can be supplied as a function of one argument, which will be called for
#' each evaluator of this class and which should return \code{TRUE}/\code{FALSE} according to
#' whether the evaluator should be accepted.  Allows applications to select, for example, a
#' particular evaluator corresponding to a known connection.
#' @details
#'Specific language interface packages usually supply a convenience function equivalent
#'to calling \code{getInterface()} for their class; e.g., \code{RPython()} in \code{'XRPython'}
#'
#'If no \code{Class} is given, the current (i.e., last active) evaluator is returned
#' @examples
#' ## the current evaluator, or NULL if none exists
#' getInterface()
#' ## this will always be NULL, because no evaluator has this class
#' getInterface("Interface", .makeNew = FALSE)
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
    ## apply the path and import directives previously specified for this class
    ## by serverAddToPath() and serverImport()
    ## paths <- languagePaths[[className]]
    ## if(!is.null(paths)) {
    ##     for(path in paths)
    ##         value$AddToPath(path, path@package, path@pos)
    ## }
    ## imports <- languageImports[[className]]
    ## if(!is.null(imports)) {
    ##     for(expr in imports)
    ##         eval(expr)
    ## }
    value
}

#' @describeIn getInterface Remove the specified  evaluator from the table of available interfaces.
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

## returns or creates a sequential number for the given evaluator,
#' @describeIn getInterface Return the sequential number for this evalutor; used in \code{ProxyName()} method.
#' If not there: if \code{add}, add the evaluator to the table; else return \code{NA}.
#' @param evaluator any evaluator object.
#' @param add if this evaluator is not in the table, add it.  Default \code{TRUE}.
evaluatorNumber <- function(evaluator, add = length(id) > 0) {
    id <- evaluator$evaluatorId
    n <-  if(length(id)) .evaluatorTable[[id]] else NULL
    if(is.null(n)) {
        if(add) {
            n <- length(objects(.evaluatorTable))+1
            assign(id, n, envir = .evaluatorTable)
        }
        else
            n <- NA_integer_
    }
    n
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
#' Make a Help Topic an Explicit Character String
#'
#' A helper function to pass on a help topic (specifically for a reference-class method) to
#' another help-style function; i.e., so the user could have supplied either a name or a
#' general expression that evaluates to a character string.
#'
#' @param topic The \emph{expression} supplied by your user.  If this was a name, it will be
#' taken literally, otherwise evaluated two levels up the call stack, which should return a character string.
#' @template reference
fixHelpTopic <- function(topic) {
    if(is.name(topic))
        as.character(topic)
    else
        eval(topic, parent.frame(2))
}

Interface$methods(
                      initialize = function(...) {
                      'initializes the evaluator in a language-independent sense.'
                      initFields(...) # allow overides to precede
                      usingMethods(finalize, ServerQuit) # in case of garbage collection
                      saveInTable <- TRUE
                      if(!length(languageName)) {# should be set by a subclass method first
                          languageName <<- "<UnspecifiedLanguage>"
                          saveInTable <- FALSE
                      }
                      if(!length(proxyCount))
                          proxyCount <<- 0L
                      if(!(length(evaluatorId) == 1 && nzchar(evaluatorId)))
                          evaluatorId <<- paste(languageName, "Evaluator", format(Sys.time()))
                      if(!length(propertyFormat))
                          propertyFormat <<- "%s.%s"
                      if(is(simplify, "uninitializedField"))
                          simplify <<- FALSE
                      ## get an evaluator number, save the object in table
                      if(saveInTable) # but not for dummy objects
                          evaluatorNumber(.self, TRUE)
                      ## path <- languagePaths[[className]]
                      ## for(dir in path) # requires all elements to be of class pathEl
                      ##     AddToPath(dir, dir@package, dir@pos)
                      ## imports <- languageImports[[className]]
                      ## for(expr in imports)
                      ##     eval(expr, envir = .self)
                  },
    startupActions = function() {
        'Perform the evaluator actions specified for this object, typically additions to search path and imports'
        ## initialize the system path, imports and potentially other actions
        className <- class(.self)
        actions <- evaluatorActions[[className]]
        for(el in actions)
            evaluatorAction(el, .self)
    },

                  finalize = function(...) {
                      'method called when the object is garbage collected.  A call to the $Quit() method
also calls this method (recalling it later then does nothing).  In case some server action
(like closing down a subprocess) is required, the $ServerQuit() method is called, and
the evaluator is then removed from the table of interface evaluators.'
        ServerQuit(...)
        rmInterface(.self)
    },
    show = function() {
        n <- evaluatorNumber(.self, FALSE)
        n_txt <- if(is.na(n)) "<Not in the evaluator table>" else
             gettextf("Evaluator number: %d", as.integer(n))
        cat(gettextf("%s evaluator; Id: %s; %s\n", languageName, nameQuote(evaluatorId), n_txt))
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
    ProxyName = function(x, new = TRUE) {
'Called without arguments, returns a key for the next proxy object.  In the default strategy, this is a string
"R_i_j" where i is the sequence code for the evaluator and j is the proxy count,
incremented if `new` is TRUE.  If `x` is supplied as an existing proxy object,
returns the key for that object.'
        if(!missing(x))
            return(XR::proxyName(x))
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
backslash to all but the last line.

A line typed to the shell starting with "$" is an escape back to the evaluator, and
can be used to call evaluator methods, e.g., "ProxyName(x)".  See the example in
the documentation for class "Interface" in XR'
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
            if(grepl("^[$]",expr))
                MethodEval(expr, TRUE, TRUE)
            else
                tryCatch(ServerEval(expr), error = function(e) {
                    message(gettextf("%s error: %s", languageName, e$message))
                    })
        }
    },
    MethodEval = function(string, catch = FALSE, print = FALSE) {
        'The string is a method call for the evaluator, or the name of a field.
Evaluated as the expression ev$string.'
        if(!grepl("^[$]",string))
            string <- paste0("$",string)
        expr <- parse(text = paste0(".self", string))
        value <- (if(catch)
               tryCatch(eval(expr), error=function(e)
                   {
                       message(gettextf("error in R method: %s", e$message))
                   })
        else
            eval(expr))
        if(print) methods::show(value)
        value
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
        getRefClass()$help(fixHelpTopic(substitute(topic)))
    },
    help = function(topic) {
        getRefClass()$help(fixHelpTopic(substitute(topic)))
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
        if(!append) { # truncate the file
            con <- base::file(file, "w")
            close(con)
        }
        ServerSerialize(ProxyName(object), file)
    },
    Unserialize = function(file, all = FALSE) {
        'Unserialize a list of objects previously written to `file` by $Serialize().\nReturns a list of proxy objects. If all=FALSE, returns a single object if exactly one object found.'
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
            try <- tryCatch( append(value, base::unserialize(con)),
                     error = function(e) e
                     )
            if(is(try, "condition")) # test for actual error?
                break
            else
              value <- try
        }
        if(!all && length(value) > 0) {
            if(length(value) == 1)
                value <- value[[1]]
            else
                warning(gettextf("Expected to unserialize one object, found %d",
                                 length(value)))
        }
        value
    },
    SaveProxyFunction = function(save, object, objName = obj@name, docText = NULL) {
        'The object is an expanded function definition, provided by the initialize method for this class.
`save` should be either an environment in which to assign it or a place to dump the R source, either an
open connection or a file name.'
        if(is(save, "environment"))
            assign(objName, object, envir = save)
        else
            dumpProxyFunction(save, object, objName, docText)
    },
    copy = function (shallow = FALSE) 
    {
        'An interface evaluator is always copied shallow to avoid infinite recursion.'
        def <- .refClassDef
        value <- new(def)
        vEnv <- as.environment(value)
        selfEnv <- as.environment(.self)
        for (field in names(def@fieldClasses))
            assign(field, get(field, envir = selfEnv), envir = vEnv)
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
#' @template reference
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
#' and type= is the R vector type desired.  See the asRObject() method documentation.
#' Objects from this class can also be generated in R, usually supplying just \code{data}
#' and generating the other slots from that object's properties.
#' @slot data The actual vector data.
#' @slot type The string for the \R type intended. In spite of the slot name, this really the class;
#' for example, "numeric" rather than "double".
#' @slot missing The index of NA's in this vector.  Needed because most server languages only
#' treat `NaN` for doubles and have no mechanism for `NA` in other types.
#' @examples
#' x <- c(1:2,NA,4:5)
#' vector_R(x)
#' @template reference
vector_R <- setClass("vector_R",
                     slots = c(data = "vector", type = "character",
                     missing = "vector"))

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
#' @template reference
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

#' @describeIn asRObject To distinguish typed R vectors from a general JSON list, encode the
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

#' @describeIn asRObject Assume this has been done via .RClass; avoid inheriting the list method
setMethod("asRObject", "data.frame",
          function(object, evaluator)
              object
          )

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

.asRMethods <- list(matrix = base::array,
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
    ##S4 objects always have a nonempty package
    if(nzchar(package))
        classDef <- getClassDef(class, package = package)
    else
        classDef <- NULL
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

.primitiveStructures <- c("matrix", "array", "structure")
.specialAttrs <- c("names", "row.names")

makeStructureObject <- function(args, class, package, classAttr = class) {
    ## create a structure from the data part (element ".Data" or unnamed)
    ## with the other elements as attributes
    attrs <- names(args)
    dataPart <- attrs == ".Data" | !nzchar(attrs)
    if(any(dataPart)) {
        ## should warn if > 1?
        data <- args[dataPart][[1]]
        attrs <- attrs[!dataPart]
        args <- args[!dataPart]
    }
    else
        data <- logical() # ? simplest vector; should warn?
    ## flag the attributes that are constrained by the base code
    specials <- attrs %in% .specialAttrs
    for(i in seq_along(attrs)) {
        el <- args[[i]]
        if(specials[[i]] && is.list(el))
            el <- unlist(el)
        ## use individual attr(), because the dataPart may have attributes already
        attr(data, attrs[[i]]) <- el
    }
    if(nzchar(package)) { # S4 class
        attr(classAttr, "package") <- package
        base::class(data) <- classAttr
        asS4(data)
    }
    else {
        ## set class attribute, if not one of the primitives
        if(! class %in% .primitiveStructures)
            base::class(data) <- classAttr
        data
    }
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
#' @template reference
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
#' @template reference
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
    else if(length(attributes(object))) { # a structure w/o class attr.
        Rclass <- ext <- "structure"
        package <- ""
        slotList <- attrList
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

#' Convert an Object to a Dictionary or Array in JSON Notation
#'
#' The general converter for an object with a formal class
#' or an object to be described via its S3 class, data and attributes.
#' Not usually called directly, but from \code{\link{objectAsJSON}} or method in an interface package.
#' @param object The object to convert.
#' @param prototype The prototype object (supplied from the evaluator).
#' @param exclude Slots to exclude from the dictionary.
#' @param level The level of expansion of objects within the original object.
#' @template reference
asJSONS4 <- function(object, prototype, exclude = character(), level = 0) {
    value <- objectDictionary(object, exclude)
    ## now apply objectAsJSON to elements
    jsonApply(attr(object, "class"), value, prototype, level+1)
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
asJSONData <- function(Class, type, data,  prototype, level=1) {
  classDef <- getClassDef(Class)
  if(is.null(classDef)) # no class for this type: does this ever happen?
    { package <- "base"; extends <- Class}
  else
    { package <- classDef@package; extends <- methods::extends(Class)}
  fields <- list(.RClass = Class, .type = type, .package = package, .extends = extends,
       .Data = data)
  ## the JSON dictionary
  jsonApply(Class, fields, prototype, level + 1)
}

#' Utilities for Server-Language Specific Use
#'
#'  A utility to fill in blank names in list elements to make it valid for a dictionary.
#' @param object a list object, possibly with empty or duplicate names.
#' @param noNamesOK what to do with a list having no names---leave it alone or fill them all in?
#' @template reference
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
#' @template reference
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
                       obj <- vector_R(object)
                       obj@missing <- which(is.na(object))
                       obj@data[obj@missing] <- .notNA(typeof(object))
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
           txtC <- character() # just so CMD check doesn't screw up
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

## something to subsitute for NA in vecor_R representation
.notNA <- function(type) {
    switch(type,
           character = "",
           logical = FALSE,
           integer = 999L,
           NULL)
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
#' @template reference
valueFromServer <-
    function(string, key, get, evaluator,
             object)
    {
        if(missing(object)) {
            simplify <- evaluator$simplify
            postSimplify <- is(simplify, "function")
            object <- jsonlite::fromJSON(string,
             simplifyVector = !postSimplify && simplify, flatten = FALSE,
             simplifyDataFrame=FALSE, simplifyMatrix = FALSE)
            if(postSimplify)
                object <- simplify(object)
        }
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
#' @template reference
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

#' Return the Server Language Name Corresponding to a Proxy Object
#'
#' Interface evaluators pass constructed names to the server language evaluator, arranged
#' to be unique within and between evaluators.  This function returns the name when given a
#' proxy object.
#'
#' @param x an object returned from some computation in the server  as a proxy
#' for that server object.  May be from a proxy class, but doesn't need to be.
setGeneric("proxyName", function(x)
    stop(gettextf("No proxy name contained in objects of class %s",
                  nameQuote(class(x)))))

#' @describeIn proxyName for this class, the name is the object (which extends class
#' "character")
setMethod("proxyName", "AssignedProxy",
          function(x)
              as(x, "character"))
## and a method for ProxyClassObject in ProxyClass.R

#' Class Union for Describing Server Language Fields
#'
#' May be extended by the interface for a particular language.
#' @name serverFields-class
setClassUnion("serverFields", c("environment", "list", "namedList", "NULL"))

noServerData <- new.env() # a special value for the data slot of from_Server

.badNames <- function(ns) {
    if(is.null(ns))
        "No names"
    else {
        blank <- !nzchar(ns)
        if(any(blank))
            gettextf("Empty names: %s", paste(which(blank), collapse = ", "))
        else
            gettextf("Duplicate names: %s", paste(nameQuote(ns[duplicated(ns)]), collapse = ", "))
    }
}

.checkFields <- function(fields) {
    if(is.null(fields) || is.environment(fields))
        return(fields)
    ns <- names(fields)
    if(is.character(ns) && all(nzchar(ns)) && !anyDuplicated(ns))
        as(fields, "environment")
    else
        stop("Field names must be unique and non-empty: %s", .badNames(ns))
}

#' A Class to Describe General Server Objects
#'
#' Classes that inherit from this class are used to convert to R a server language
#' object that is composed of named fields.  The corresponding R object will have
#' conversions for the fields that can be accessed with the "$" operator.
#'
#' @slot serverClass the name of the server language class
#' @slot module the name of the server language module
#' @slot language the name of the server language
#' @slot fields the names of the server language fields
#' @slot data the converted data for the server fields
from_Server <- setClass("from_Server",
                        slots = c(serverClass = "character", module = "character",
                                  language = "character", fields = "serverFields", data = "ANY"),
                        prototype = list(serverClass = "", module = "", language = "",
                        fields = NULL, data = noServerData))
#' @describeIn from_Server performs some checks on the fields
#'
#' @param .Object,referenceClass arguments supplied automatically.
#' @param ... possible slots for subclasses
setMethod("initialize", "from_Server",
          function(.Object, ..., referenceClass = TRUE) {
              obj <- callNextMethod(.Object, ...)
              fields <- obj@fields
              if(referenceClass && !is.null(fields))
                  obj@fields <- .checkFields(fields)
              obj
          })

#' @describeIn from_Server extract a field.  The name must match a field in the
#' data part.
#'
#' @param x,name the object and the field name
setMethod("$", "from_Server",
          function(x, name) {
              i <- match(name, names(x@data))
              if(is.na(i))
                  stop(gettextf("%s object of class %s has no %s field",
                                x@language, nameQuote(x@serverClass), nameQuote(name)))
              x@data[[i]]
          })

#' @describeIn from_Server automatic printing, currently just a list of field names.
#'
#' @param object an object from some server class, converted by the \code{$Get()} method
#' or an equivalent computation, such as supplying \code{.get = TRUE} to a proxy function
#' call.
setMethod("show", "from_Server",
          function(object) {
              cat(gettextf("R conversion of %s object of class %s\n\nConverted fields:\n",
                           object@language, nameQuote(object@serverClass)))
              methods::show(as.list(object@fields))
          })


#' Unconverted Server Language Objects
#'
#' Objects from this class represent server language objects whose conversion was requested
#' but which are judged (by the server side of the interface) to have no equivalent R form.
#' Rather than generating an error, the interface returns an object of this class, which can
#' have convertible attributes.  Fields of a convertible object may be unconvertible without
#' preventing conversion of the rest of the parent object.
#' @slot serverClass,serverModule The names of the class and module in the server language.
#' @slot language The language name (for communicating with users), not the interface class name.
#' @slot attributes A list with names that should be interpreted as properties of the object.
#' @template reference
setClass("Unconvertible",
         slots = c( serverClass = "character", serverModule = "character",
         language = "character", attributes = "list"))

setMethod("initialize", "Unconvertible",
          function(.Object, ...) {
              value <- callNextMethod()
              if(!length(value@serverModule))
                  value@serverModule <- ""
              if(!length(value@language))
                  value@language <- "<Unspecified>"
              if(!length(value@serverClass))
                  value@serverClass <- "<Unspecified>"
              value
          })

setMethod("show", "Unconvertible",
          function(object) {
              cat(gettextf("Unconvertible %s object of class %s",
                  object@language, nameQuote(object@serverClass)))
              if(nzchar(object@serverModule))
                  cat(gettextf(", module = %s", nameQuote(object@serverModule)))
              cat("\n")
              if(length(object@attributes)) {
                  cat("Attributes:\n")
                  show(object@attributes)
              }
          })
