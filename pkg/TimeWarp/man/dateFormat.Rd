\name{dateFormat}
\alias{dateFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Converts a Date object to character data }
\description{
  ??
}
\usage{
dateFormat(date, format = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{ ~~Describe \code{date} here~~ }
  \item{format}{ ~~Describe \code{format} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (date, format = NULL) 
{
    if (is.null(format)) {
        if (Sys.info()[["sysname"]] == "Linux") 
            format <- "\%02m/\%02d/\%04Y"
        else format <- "\%m/\%d/\%Y"
    }
    if (is(date, "character")) 
        date <- dateParse(date)
    if (is(date, "dates")) 
        format(as.POSIXct(date), format)
    else if (is(date, "Date") || is(date, "POSIXt")) 
        format(date, format)
    else stop("unknow date format: '", class(date), "'")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

