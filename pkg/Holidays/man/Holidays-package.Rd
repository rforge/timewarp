\name{Holidays-package}
\alias{Holidays-package}
\alias{Holidays}
\docType{package}
\title{
  Supplies holiday data to the TimeWarp package.
}
\description{

  This package is a data-only package that registers holidays with the
  TimeWarp package.  The package has no visible functions or data; it
  works by calling \code{\link[TimeWarp]{registerHolidays}()} from its
  \code{.onLoad()} function.

}
\author{
Jeffrey Horner, Lars Hansen, Tony Plate

Maintainer: Tony Plate \email{tplate@acm.org}
}
\details{
  Accurate holidays are available for the following:
  \itemize{
    \item{NYSE}{1970 - 2050: All scheduled holidays on the New York
      stock exchange.  Since 2000, NYSE holidays have followed published
      rules.}
    \item{NYSE.Closings}{1900 - 2011: Special closings as of 2018, the most recent
      special closing was for Hurricane Sandy in 2011.  NYSE has unscheduled special
    closing for the funerals of US presidents and for other
    extraordinary events.}
    \item{NYSEC}{1970 - 2050: All scheduled holidays and special closings.}
    \item{NYSE.Halfdays}{2000 - 2050: Early closing days,}
    \item{NYSEC.Long}{1900 - 2050: All scheduled holidays and special
      closings that I could find from 1900 through 2050.}
    \item{USFed}{1971 - 2050: US Federal holidays}
    \item{AUSEX}{1990 - 2012: Australian stock exchange holidays}
    \item{GBREX}{1990 - 2009: London (Great Britain) stock exchange holidays}
    \item{HKGEX}{1996 - 2020: Hong Kong stock exchange holidays}
    \item{JPNEX}{1994 - 2020: Hong Kong stock exchange holidays}
    \item{KOREX}{1994 - 2020: South Korea stock exchange holidays}
  }
  There are formulas for various NYSE holidays which have been follewed
  since around 2000, see
  \code{\link{holiday.US_NYSE}}.  Many of the holiday series are hardcoded.
}
\note{
  To create a package with custom holidays (or updated holidays for the
  holiday series defined in here), either modify the source code of this
  package, or copy commands from R/holidays.R into a new package.
  }
\keyword{ package }
\seealso{
From the TimeWarp package: \code{\link[TimeWarp]{holidays}},
\code{\link[TimeWarp]{registerHolidays}}, \code{\link[TimeWarp]{allHolidays}}.
}
\examples{
# View counts of registered holidays by year
sapply(as.character(1998:2012), function(y)
       sapply(allHolidays(), function(h) length(holidays(y, h, silent=TRUE))))
}
