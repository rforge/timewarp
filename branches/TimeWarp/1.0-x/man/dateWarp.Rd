\name{dateWarp}
\alias{dateWarp}
\alias{dateWarpAppend}
\title{Date vector shifting and alignment}
\description{
  Perform multiple shifts and alignments on \code{\link{Date}} vectors.
  \code{dateWarp()} and \code{dateWarpAppend()} are generics, with
  methods for \code{character}, \code{Date}, \code{POSIXct}, and \code{POSIXlt}.
}
\usage{
dateWarp(date, spec, holidays = NULL, by = "bizdays", direction = 1, duplicates.keep = TRUE)
dateWarpAppend(date, ..., where = c("sorted", "start", "end"), empty.ok
= FALSE, duplicates.ok = FALSE)
}

\arguments{
        \item{date}{
          a \code{\link{Date}} vector, or a character vector that can be
          converted to \code{\link{Date}} by \code{\link{dateParse}}.
        }
        \item{spec}{
          a specification of shifts and alignment tranformations. See \sQuote{Details}.
        }
        \item{holidays}{
          a character string naming the holiday series (see \code{\link{holidays}}).
        }
        \item{by}{
          how to warp. Can be one of \code{"days"}, \code{"bizdays"},
          \code{"weeks"}, \code{"months"}, or \code{"years"}.
          \code{"bizdays"} can contain a holiday specification like:
          \code{"bizdays@NYSEC"}
        }
        \item{direction}{
          which direction to warp. a numeric value of 1 for the future,
          and -1 for the past (be careful about using variables for this
          value, for if it's value is negative and you place a minus
          sign in front of the variable, you'll go back to the future).
        }
        \item{duplicates.keep}{
          logical indicating whether or not to keep duplicate dates.
        }
        \item{\dots}{
          arguments to pass to \code{dateWarp}.
        }
        \item{where}{
          character string. can be \code{"sorted"} to sort the result instead of append, \code{"start"} to prepend, or \code{"end"} to append.
        }
        \item{empty.ok}{
          is it okay for the \code{dates} argument to be empty?
        }
        \item{duplicates.ok}{
          logical indicating whether or not to keep duplicate dates.
        }
}
\details{
  \code{spec} specify tranformations in several ways:
  \itemize{
    \item integer vector by which to shift the current Date object. The
    units of the shift are specified by the \code{by} argument.
    If the shift contains more than one element, this will
    transform a single Date object into a multiple-element Date
    object. It is an error to apply a multiple-element shift to
    a multiple element Date object.
    \item character data of the form "+3 bizdays@NYSEC", "+3 bizdays",
    or "+3". 'by' and 'holidays' specifications are extracted from
    the string as appropriate, and override any previously specified
    or given in arguments.
    \item a named list of elements. The elements can be a mix of
    list or scalars. The scalar items can be named:
    \describe{
      \item{unique}{the item value can be either numeric or
        logical. No matter the value, the dates will be made unique}
      \item{latest}{the item is a \code{Date} or a character that can
        be converted to a \code{Date} with
        \code{\link{dateParse}}. The dates will be clipped at
        the specified \code{Date}.}
      \item{earliest}{the item is a \code{Date} or a character that can
        be converted to a \code{Date} with
        \code{\link{dateParse}}. The dates will be clipped at
        the specified \code{Date}.}
    }
    The list items can be named:
    \describe{
      \item{shift}{the list will be used as arguments to a call
        of \code{\link{dateShift}}}
      \item{align}{the list will be used as arguments to a call
        of \code{\link{dateAlign}}}
    }
    If not all arguments are not provided, the {dateWarp}
    arguments will be used instead.
  }
}
\value{
  A date vector that is a transformed version of the input
  dates. Multiple shift or alignment transfations can be specified. If
  more that one is given, each vill be applied in turn to the result of
  the previous one.
  The class of the returned value is the same as the class of \code{date}
  for \code{character}, \code{Date}, \code{POSIXct}, and
  \code{POSIXlt}.  For \code{date} of other classes, the class of the
  returned value is \code{Date}, but this may change in the future.
}
\author{Lars Hansen}
\seealso{
  \code{\link{dateAlign}},
  \code{\link{dateShift}},
  \code{\link{dateMatch}},
  \code{\link{dateParse}},
  \code{\link{dateSeq}}
}
\examples{
library(Holidays)
dates <- dateSeq("2001/12/20", by = 'bizdays', len = 9, holidays = "NYSEC")
dateWarp(dates, -1:1, by = "bizdays", holidays = "NYSEC", duplicates.keep =
FALSE)
dateWarp(dates, "+1 bizdays@NYSEC")
dateWarp(dates, list(0:6), by = "bizdays@NYSEC")
dateWarp(dates, list(-1:1, unique = TRUE), by = "bizdays")
dateWarp(dates, list(latest = "2001/12/25"))
x <- dateSeq("2001/01/01", len = 4, by = "weeks")
dateWarp(dates, list(align = list(to = x, how = "after")))
dateWarp(dates, list(shift = list(by = "bizdays", k.by = 2, direction =
1, holidays = "NYSEC"), shift = 2), by = "days", direction = -1,
holidays = "NONE")
dateWarp(dates, hol = "NYSEC", list(shift = list(k.by = 1, by = "months"),
shift=list(k.by = -1, by = "bizdays")))
dateWarp(dates, list(align = list(by = "bizdays"), shift = list(by =
"months", k.by = 2), holidays = "JPNEX"))
}

\keyword{utilities}
