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

This functionality is currently implemented for \code{Date},
\code{character}, \code{POSIXct} and \code{POSIXlt} classes.  For other
classes, functions from this package will currently return a \code{Date}
vector, but that may change as other classes are added.

}
\author{
Jeffrey Horner, Lars Hansen, Tony Plate

Maintainer: Tony Plate \email{tplate@acm.org}
}
\keyword{ package }
\seealso{
\code{\link{dateWarp}}, \code{\link{dateAlign}}, \code{\link{dateSeq}}, \code{\link{dateMatch}}, and \code{\link{holidays}}
}
\examples{
library(Holidays)
# Find US option expiration dates in 2011 (The pricing day, usually a Friday)

# Technically speaking, standardized options expire on the Saturday
# following the third Friday of the month. The reason that equity and
# index options expire on this day is due to the fact that this day offers
# the least number of scheduling conflicts, i.e. holidays.

# When an options expiration date falls on a holiday, all trading dates
# are moved forward. For example, in 2008, options expiration date falls
# on Good Friday. In this situation, options will still expire on
# Saturday following Good Friday -- however the last trading day for
# Equity options will be the Thursday preceding the Good Friday trading
# holiday. The last trading date for many Broad-Based Index products
# will be Wednesday -- with A.M. settlement values determined based on
# an index component's opening price on Thursday.

yy <- 2011
(d1 <- dateSeq(paste(yy, '-01-01', sep=''), len=12, by='months'))
(d2 <- dateAlign(d1, by='months', dir=-1))
(d3 <- dateAlign(d2, by='weeks', week.align=5))
(d4 <- dateWarp(d3, 14, by='days'))
(d5 <- dateAlign(d4, by='bizdays@NYSEC', dir=-1))

# Find option expiration dates that have been shifted because they would have
# occured on a holiday
yy <- 1990
d1 <- dateSeq(paste(yy, '-01-01', sep=''), len=288, by='months')
d2 <- dateAlign(d1, by='months', dir=-1)
d3 <- dateAlign(d2, by='weeks', week.align=5)
d4 <- dateWarp(d3, 14, by='days')
d5 <- dateAlign(d4, by='bizdays@NYSEC', dir=-1)
data.frame(holiday=d4, option.expiration=d5)[which(d4 != d5), ]
}
