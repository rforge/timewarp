\name{isWeekday}
\alias{isWeekday}
\alias{nearestWeekday}
\alias{filterWeekdays}
\title{Functions for identifying and working with weekdays}
\description{
  Functions for identifying and working with weekdays
}
\usage{
nearestWeekday(x, return.dow=FALSE)
filterWeekdays(x, return.dow=FALSE)
isWeekday(x, return.dow=FALSE)
}

\arguments{
	\item{x}{ vector of dates, either character, Date, or POSIXct or
	POSIXlt.}
	\item{return.dow}{ if TRUE, an attribute 'dow' on the returned
	value will hold the day-of-week of x, with 0 being Sunday and 6
	being Saturday. }
}
\value{
  \itemize{
    \item{nearestWeekday()}{returns a vector of same type and length as
	x holidng the nearest weekday.  The returned value can contain
	duplicates, and the order is the same as x.}
    \item{isWeekday()}{returns a logical vector the length as x.}
    \item{filterWeekdays()}{returns a vector of same type as x, but with
	non-weekdays and duplicates removed.}
  }
}
\keyword{utilities}
