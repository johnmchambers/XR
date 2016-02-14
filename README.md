# XR - Structure for Interfaces from R

This package provides a central structure for interfaces from R to
other languages that share the concept of evaluating calls to
functions or methods and a system of classes for objects.

The functions and other software in this package will not generally be
used directly by applications. Instead, other packages will provide
specialized interfaces to particular languages built on the common
structure but with a programming interface and implementation
customized to that language.
Examples included in this repository are `XRPython` and `XRJulia`.

The structure consists of a class of evaluator objects, generic
functions to customize communication with the server language and a
set of tools to manage evaluators in the session.

The interface structure is described in the forthcoming book
*Extending R* (John M. Chambers, 2016, Chapman & Hall).
A pdf version of the XR chapter from the book is included with the
documentation of this package.
