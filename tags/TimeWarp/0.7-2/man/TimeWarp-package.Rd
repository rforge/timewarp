\name{TimeWarp-package}
\alias{TimeWarp-package}
\alias{TimeWarp}
\docType{package}
\title{
Date sequence and manipulation with business days and holidays.
}
\description{

A package for manipulating vectors of class Date. Support for other
vectors and classes may be added in the future.

The general idea with the behavior of functions from this package is
that they should return an object of the same class as they are given,
e.g., \code{dateWarp()} applied to a vector of dates in character format
should return a vector of dates in character format, and
\code{dateWarp()} applied to a \code{Date} class vector should return a
\code{Date} class vector.

This is not have the functions are implemented! Presently, the functions
always return a \code{Date} vector.

}
\author{
Jeffrey Horner, Lars Hansen

Maintainer: Lars Hansen <lhansen@blackmesacapital.com>
}
\keyword{ package }
\seealso{
\code{\link{dateWarp}}, \code{\link{dateSeq}}, \code{\link{dateMatch}}, and \code{\link{holidays}}
}
