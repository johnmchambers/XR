% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Interface.R
\docType{class}
\name{Interface-class}
\alias{Interface}
\alias{Interface-class}
\title{Reference class for all interface evaluators}
\description{
This class has the fields required for any specific interface and the methods that
are defined centrally in the XR structure.  As noted in the documentation for inidividual
methods, some methods must be redefined in the specific interface.
}
\section{Fields}{

\describe{
\item{\code{evaluatorId}}{A character string (usually unique) giving the language and date when started}

\item{\code{languageName}}{The server language.  Does not have to be unique if multiple classes implement interfaces to the same language.}

\item{\code{proxyCount}}{Counter used to generate unique names for proxy objects.}

\item{\code{propertyFormat}}{C-style format string for access to properties and methods in this
evaluator.  Nearly always just the two names, separated by "."}

\item{\code{proxyClassTable}}{An environment for all proxy classes known currently for this
evaluator class.}

\item{\code{asRMethods}}{A table of specialized methods for interpreting R objects returned from the server
encoded in a dictionary with element ".RClass".}

\item{\code{proxyObject}}{The object representing any proxy class for this inteface.  Usually from
a class defined by the specific interface package, to distinguish its proxy classes. This
field is passed as the \code{prototype} argument in calls to \code{\link{asServerObject}}.}
}}
\section{Methods}{

\describe{
\item{\code{AsRObject(object)}}{Given an R object made up of vectors, lists and named tables, interpret that as a general R object,
using a convention that may be specialized to the server language by overriding
$AsRObject() or by methods for asRObject().  The argument will may be from a proxy class.}

\item{\code{AsServerObject(object, prototype = proxyObject)}}{Given an R object return a string that, when evaluated in the server language gives
a corresponding object in that language.  The default implementation uses the function
XR::objectAsJSON, which returns a JSON string and assumes a function `objectFromJSON(string)`
in the server. The conversion may be specialized to server language classes by methods for
asServerObject() or objectAsJSON().}

\item{\code{Call(fun, ..., .proxy = NA)}}{Call the server language function `fun`.  Each of the `...` arguments will be
translated into a server language expression by the AsServerObject() method.}

\item{\code{Command(...)}}{Like Eval(), but the value of the expression is ignored.  In particular, may be a command
in the server language that is not an expression.}

\item{\code{Convert(object, serverClass = "", .key = NULL)}}{Assign the converted version of `object` in the server language.
If `.key` is specified, assign it under that name.  By default (and recommended)
a proxy object in R provides the reference to the converted object.
If `serverClass` is supplied, there should be a corresponding asServerObject() method.}

\item{\code{Eval(expr, ..., .proxy = NA)}}{Evaluate `expr` and return the value, possibly as a proxy.
Expressions are supplied as character strings to be parsed and
evaluated by the server language. If `expr` has "%s" fields, they are filled in
with the appropriate server language code equivalent to the `...` arguments.  If `expr` has
more than one element, all but the last are evaluated by $Command(), with `...` ignored.}

\item{\code{Function(serverFun)}}{Returns an R function object that calls the specified sever language function,
specified by its name in the server language or by the proxy object returned by the
$Define() method of the evaluator}

\item{\code{Get(what, ...)}}{Return the value, always converted to an R object.  Usually gets a proxy object as the argument, but can be called like $Eval(), if ... is non-empty.}

\item{\code{Import(module, ...)}}{Import the module.  The "Interface" method assumes a command "import" in the server
language and does not handle any extra arguments (e.g., for importing specific members).}

\item{\code{MethodCall(object, name, ..., .proxy = NA)}}{Call the server language method `name` on `object`, with arguments `...`,
by default assuming a language in which the syntax is `object.name(...)`.
To override with a different syntax, define field propertyFormat in the evaluator.
Note that `name` must be a character string, not an evaluation in the server.}

\item{\code{New(serverClass, serverModule = "", ...)}}{Generate a new object from the specified server class.  The corresponding generator function
in the server is given by ServerGenerator(serverClass), by default just the class name.
Typically called from the $Initialize() method of the proxy class.}

\item{\code{ProxyClassObject(object)}}{If `object` is an assigned proxy, check whether the serverClass is a known proxy class
and if so, return an object from that class; otherwise return `object`.}

\item{\code{ProxyName(new = TRUE)}}{A key for the next proxy object.  In the default strategy, this is a string
"R_i_j" where i is the sequence code for the evaluator and j is the proxy count,
incremented if `new` is TRUE}

\item{\code{Serialize(object, file, append = FALSE)}}{Use the server language serialization to serialize `object` to the specified `file`.
According to `append` either append to the file (default) or overwrite.
The supplied object should be a proxy for a server language object.}

\item{\code{ServerClass(Class, module)}}{If possible, return the class structure of Class, a class in the server language.
module= is the server module/package/library in which Class is defined, or "".
If no reflection information is available, return NULL (which this definition does).
Should return a list or reference object: "$fields" and "$methods" should be character
vectors or named lists of the server fields and metohds.}

\item{\code{ServerEval(expr, key, keepValue)}}{Must be defined by the server language interface: evaluates `expr`(a text string).
If `key` is an empty string, `expr` is treated as a directive, with no defined value.
Otherwise, `key` is a non-empty string, and the server
object should be assigned with this name. The value returned is the R result, which may
be an AssignedProxy() object. If `keepValue` is FALSE or the value judged simple enough,
it will be converted to an ordinary R object instead.}

\item{\code{ServerExpression(...)}}{The arguments define an expression in the server language.
The first argument is a string; any others are objects to be substituted for %s fields
in the string.  These can include proxy objects or R data.}

\item{\code{ServerRemove(key)}}{Should be defined by the server language interface:
The reference previously created for `key` should be removed.
What happens has no effect on the client side; the intent is to potentially recover memory.}

\item{\code{ServerSerialize(key, file)}}{Serialize the proxy function corresponding to `key` to the specified `file`.
Will normally be defined using the serialization supported by the particular server
language. The default gets the object and serializes in R, so only works if conversion does}

\item{\code{ServerUnserialize(file, all)}}{Unserialize the file, returning a proxy object for a list, or equivalent in the server
language, of all the objects serialized to this file.  Because open connections can not
generally be shared among languages, must unserialize the entire file.}

\item{\code{Unserialize(file, all = FALSE)}}{Unserialize the objects previously written to `file` by $Serialize().
Returns a list of proxy objects (always a list even if only one object found).}
}}
