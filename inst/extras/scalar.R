### some class definitions for scalars.
### These might be needed for a server langauge that did not understand
### JSON scalar formats or for which a special control was needed to ensure
### scalars of the correct type.


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
