nearestWeekday <- function(x, return.dow=FALSE) {
    xu <- unique(x)
    xi <- match(x, xu)
    origClass <- class(x)[1]
    if (origClass=='character') {
        xud <- dateParse(xu)
        xult <- as.POSIXlt(xud, tz='UTC')
    } else if (origClass=='Date') {
        xud <- xu
        xult <- as.POSIXlt(xud, tz='UTC')
    } else if (origClass=='POSIXlt') {
        xud <- dateParse(xu)
        xult <- as.POSIXlt(xud, tz='UTC')
    } else if (origClass=='POSIXct') {
        xult <- as.POSIXlt(xud, tz='UTC')
        xud <- as.Date(xult, tz='UTC', origin=TimeWarp:::.dateParse.origin)
    } else {
        stop('x must be character, Date, POSIXct or POSIXlt')
    }
    # dow: 6 is Saturday, 0 is Sunday
    dow <- xult$wday
    nwd <- xud + ifelse(dow==6, -1, ifelse(dow==0, 1, 0))
    if (origClass=='character') {
        nwdoc <- format(nwd, '%Y-%m-%d')
    } else if (origClass=='Date') {
        nwdoc <- nwd
    } else if (origClass=='POSIXlt') {
        nwdoc <- as.POSIXlt(nwd, tz='UTC')
    } else if (origClass=='POSIXct') {
        nwdoc <- as.POSIXct(nwd, tz='UTC')
    }
    y <- nwdoc[i]
    if (return.dow)
        attr(y, 'dow') <- dow[i]
    return(y)
}
holiday.US_NEWYEAR <- function(year) {
    x <- dateParse(sprintf('%04d-01-01', floor(year)))
    y <- nearestWeekday(x, return.dow=TRUE)
    dow <- attr(y, 'dow')
    attr(y, dow) <- NULL
    return(y[dow!=6])
}
holiday.US_MLK <- function(year) {
    x <- dateParse(sprintf('%04d-01-15', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_WASH <- function(year) {
    x <- dateParse(sprintf('%04d-02-15', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_MEMORIAL <- function(year) {
    x <- dateParse(sprintf('%04d-05-31', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=-1))
}
holiday.US_GOOD_FRIDAY <- function(year) {
    x <- dateParse(sprintf('%04d-05-31', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=-1))
}
holiday.US_INDEP <- function(year) {
    x <- dateParse(sprintf('%04d-07-04', floor(year)))
    return(nearestWeekday(x))
}
holiday.US_LABOR <- function(year) {
    x <- dateParse(sprintf('%04d-09-01', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_THANKSGIVING <- function(year) {
    x <- dateParse(sprintf('%04d-11-25', floor(year)))
    return(dateAlign(x, by='weeks', week.align=4, direction=1))
}
holiday.US_CHRISTMAS <- function(year) {
    x <- dateParse(sprintf('%04d-12-24', floor(year)))
    return(nearestWeekday(x))
}
holiday.EASTER_SUNDAY <- function(year) {
    # Using algorithm from http://aa.usno.navy.mil/faq/docs/easter.php
    # Cites this as the original source:
    # J.-M. Oudin (1940) reprinted in Richards, E.G. 2012, "Calendars," Explanatory
    # Supplement to the Astronomical Almanac, 3rd ed., S.E. Urban and P.K. Seidelmann eds., 600-601.
    if (any(year < 1900))
        stop('only works for year >= 1583')
    if (any(year > 2099))
        stop('only works for year <= 2099')
    y <- year
    c <- y %/% 100
    n <- y - 19 * ( y %/% 19 )
    k <- ( c - 17 ) %/% 25
    i <- c - c %/% 4 - ( c - k ) %/% 3 + 19 * n + 15
    i <- i - 30 * ( i %/% 30 )
    i <- i - ( i %/% 28 ) * ( 1 - ( i %/% 28 ) * ( 29 %/% ( i + 1 ) ) * ( ( 21 - n ) %/% 11 ) )
    j <- y + y %/% 4 + i + 2 - c + c %/% 4
    j <- j - 7 * ( j %/% 7 )
    l <- i - j
    m <- 3 + ( l + 40 ) %/% 44
    d <- l + 28 - 31 * ( m %/% 4 )
    return(dateParse(sprintf('%04d-%02d-%02d', y, m, d)))
}
holiday.GOOD_FRIDAY <- function(year) {
    es <- holiday.EASTER_SUNDAY(year)
    return(es - 2)
}
