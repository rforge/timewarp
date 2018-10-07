\name{holiday.formula}
\alias{holiday.formula}
\alias{holiday.US_NEWYEAR_NO_FRI}
\alias{holiday.US_NEWYEAR_BOTH}
\alias{holiday.US_MLK}
\alias{holiday.US_WASHINGTONS_BIRTHDAY}
\alias{holiday.US_MEMORIAL}
\alias{holiday.US_INDEPENDENCE}
\alias{holiday.US_LABOR}
\alias{holiday.US_COLUMBUS}
\alias{holiday.US_VETERANS_2ND_MONDAY}
\alias{holiday.US_VETERANS_NEAREST_WEEKDAY}
\alias{holiday.US_THANKSGIVING}
\alias{holiday.US_CHRISTMAS}
\alias{holiday.EASTER_SUNDAY}
\alias{holiday.GOOD_FRIDAY}
\alias{holiday.US_NYSE}
\alias{holiday.US_FED}
\alias{holiday.US_HALFDAY}
\title{
  Formulas for various holidays
}
\description{
  Calculate holidays based on formula, e.g., US Thanksgiving is the
  fourth Thursday in November; the holiday for Christmas is December 25
  or the nearest weekday if December 25 falls on a weekend.
}
\usage{
holiday.US_NEWYEAR_NO_FRI(year)
holiday.US_NEWYEAR_BOTH(year)
holiday.US_MLK(year)
holiday.US_WASHINGTONS_BIRTHDAY(year)
holiday.US_MEMORIAL(year)
holiday.US_INDEPENDENCE(year)
holiday.US_LABOR(year)
holiday.US_COLUMBUS(year)
holiday.US_VETERANS_2ND_MONDAY(year)
holiday.US_VETERANS_NEAREST_WEEKDAY(year)
holiday.US_THANKSGIVING(year)
holiday.US_CHRISTMAS(year)
holiday.EASTER_SUNDAY(year)
holiday.GOOD_FRIDAY(year)
holiday.US_NYSE(year)
holiday.US_FED(year)
holiday.US_HALFDAY(year)
}
\arguments{
  \item{year}{A vector of integers specifying the years to return
    holidays for.  In almost all cases, these will be four-digit
    positive integers.}
}
\value{
  A vector of class Date, containing all the holidays for the supplied
  years, in order, with no duplicates.  The dates of the returned
  holidays will be in the supplied years.  The returned value can be a
  zero-length vector, e.g., holiday.US_NEWYEAR_BOTH(2000) (because New
  Year's day in 2000 fell on a Saturday, so the holiday was in 1999, and
  New Year's day in 2001 fell on a Monday, so there was no New Years's
  holiday in 2000).  The returned value can be of length more than one even when only
  one year is supplied, e.g., holiday.US_NEWYEAR_BOTH(1999) (because
  1999 contained two holidays for New Years: Jan 1st and Dec 31st.)
}
\author{
Tony Plate
Maintainer: Tony Plate \email{tplate@acm.org}
}
\keyword{ package }
