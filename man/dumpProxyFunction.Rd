% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProxyClass.R
\name{dumpProxyFunction}
\alias{dumpProxyFunction}
\title{Write A Proxy Function to a File or Connection}
\usage{
dumpProxyFunction(file, object, objName = object@name, docText)
}
\arguments{
\item{file}{the file or connection for writing the function text.}

\item{object, objName, docText}{arguments supplied by the evaluator method.}
}
\description{
This function is called by the \code{$SaveProxyFunction()} method of an evaluator object.  It is exported
for the convenience of packages inheriting from XR and would not normally be called by a user.
}
