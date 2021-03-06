\name{dateShift}
\alias{dateShift}
\title{Date shifting}
\description{
  Shift a date vector a number of days, bizdays, months, weeks or years.
}
\usage{
dateShift(x, by = "days", k.by = 1, direction = 1, holidays = NULL, silent = FALSE)
}
\arguments{
  \item{x}{
    \code{\link{Date}} vector, or a character vector that can be
    converted to \code{\link{Date}} by \code{\link{dateParse}}.
  }
  \item{by}{
    character string with the time unit of the shifts.
    Can be one of \code{"days"}, \code{"bizdays"},\code{"weeks"},
    \code{"months"}, or \code{"years"}.
  }
  \item{k.by}{
    positive integer with the number of \code{by} time units to shift.
  }
  \item{direction}{
    integer with the direction to shift. A value of \code{1} for the future,
    and \code{-1} for the past.
  }
  \item{holidays}{
    character string naming the holiday series (see \code{\link{holidays}}).
  }
  \item{silent}{
    logical indicating whether or not to suppress warnings about arguments.
  }
}
\value{
  \code{\link{Date}} vector that is a time shifted version of the input
  dates. If shifting by \code{"bizdays"}, weekends and holidays will be skipped.
}
\author{Lars Hansen}

\seealso{
  \code{\link{dateAlign}},
  \code{\link{dateWarp}},
  \code{\link{dateMatch}},
  \code{\link{dateParse}},
  \code{\link{dateSeq}}
}
\examples{
dateShift("2007/12/06", by = "days", k.by = 7, direction = -1)
date <- as.Date("2009/1/1") + -5:5
dateShift(date, by = "days", silent = TRUE)
library(Holidays)
dateShift(date, by = "bizdays", k.by = 5, holidays = "NYSEC")
dateShift(date, by = "weeks", k.by = 2)
dateShift(date, by = "months", k.by = "3", direction = "-1")
dateShift(date, by = "years", k.by = 1, direction = 1)
}
\keyword{utilities}
