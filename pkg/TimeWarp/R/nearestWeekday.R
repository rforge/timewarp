# Returns a vector of nearest weekdays in x, retaining duplicates and order of the original
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
        xud <- as.Date(xult, tz='UTC', origin=.dateParse.origin)
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
    y <- nwdoc[xi]
    if (return.dow)
        attr(y, 'dow') <- dow[xi]
    return(y)
}

# Returns a list of unique weekdays in x, possibly of different length than x
filterWeekdays <- function(x, return.dow=FALSE) {
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
        xud <- as.Date(xult, tz='UTC', origin=.dateParse.origin)
    } else {
        stop('x must be character, Date, POSIXct or POSIXlt')
    }
    # dow: 6 is Saturday, 0 is Sunday
    dow <- xult$wday
    dow.use <- dow >= 1 & dow <= 5
    y <- xu[dow.use]
    if (return.dow)
        attr(y, 'dow') <- dow
    return(y)
}

# Return logical vector same length as x
isWeekday <- function(x, return.dow=FALSE) {
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
        xud <- as.Date(xult, tz='UTC', origin=.dateParse.origin)
    } else {
        stop('x must be character, Date, POSIXct or POSIXlt')
    }
    # dow: 6 is Saturday, 0 is Sunday
    dow <- xult$wday
    dow.use <- dow >= 1 & dow <= 5
    y <- dow.use[xi]
    if (return.dow)
        attr(y, 'dow') <- dow[xi]
    return(y)
}

