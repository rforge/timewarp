holiday.US_NEWYEAR_NO_FRI <- function(year) {
    x <- dateParse(sprintf('%04d-01-01', floor(year)))
    y <- nearestWeekday(x, return.dow=TRUE)
    dow <- attr(y, 'dow')
    attr(y, 'dow') <- NULL
    return(y[dow!=6])
}

holiday.US_NEWYEAR_BOTH <- function(year) {
    # See if we get any New Year pushed back from the following years
    year <- floor(year)
    x <- nearestWeekday(dateParse(sprintf('%04d-01-01', unique(c(year, year+1)))))
    yy <- as.numeric(dateFormat(x, '%Y'))
    return(sort(unique(x[yy %in% year])))
}
holiday.US_MLK <- function(year) {
    x <- dateParse(sprintf('%04d-01-15', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_WASHINGTONS_BIRTHDAY <- function(year) {
    x <- dateParse(sprintf('%04d-02-15', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_MEMORIAL <- function(year) {
    x <- dateParse(sprintf('%04d-05-31', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=-1))
}
holiday.US_INDEPENDENCE <- function(year) {
    x <- dateParse(sprintf('%04d-07-04', floor(year)))
    return(nearestWeekday(x))
}
holiday.US_LABOR <- function(year) {
    x <- dateParse(sprintf('%04d-09-01', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_COLUMBUS <- function(year) {
    x <- dateParse(sprintf('%04d-10-08', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_VETERANS_2ND_MONDAY <- function(year) {
    x <- dateParse(sprintf('%04d-11-08', floor(year)))
    return(dateAlign(x, by='weeks', week.align=1, direction=1))
}
holiday.US_VETERANS_NEAREST_WEEKDAY <- function(year) {
    x <- dateParse(sprintf('%04d-11-11', floor(year)))
    return(nearestWeekday(x))
}
holiday.US_THANKSGIVING <- function(year) {
    x <- dateParse(sprintf('%04d-11-22', floor(year)))
    return(dateAlign(x, by='weeks', week.align=4, direction=1))
}
holiday.US_CHRISTMAS <- function(year) {
    x <- dateParse(sprintf('%04d-12-25', floor(year)))
    return(nearestWeekday(x))
}
holiday.EASTER_SUNDAY <- function(year) {
    # Using algorithm from http://aa.usno.navy.mil/faq/docs/easter.php
    # Works from 1583 thru 2099
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
holiday.US_NYSE <- function(year) {
    # no Columbus day
    # Never gets the New Years day pushed back from next year (when Jan 1st YYYY is a Saturday, Fri Dec 31st YYYY-1 is not a NYSE holiday)
    h <- c(holiday.US_NEWYEAR_NO_FRI(year),
           holiday.US_MLK(year),
           holiday.US_WASHINGTONS_BIRTHDAY(year),
           holiday.GOOD_FRIDAY(year),
           holiday.US_MEMORIAL(year),
           holiday.US_INDEPENDENCE(year),
           holiday.US_LABOR(year),
           holiday.US_THANKSGIVING(year),
           holiday.US_CHRISTMAS(year))
    return(sort(h))
}
holiday.US_FED <- function(year) {
    # no Good Friday
    h <- c(holiday.US_NEWYEAR_BOTH(year),
           holiday.US_MLK(year),
           holiday.US_WASHINGTONS_BIRTHDAY(year),
           holiday.US_MEMORIAL(year),
           holiday.US_INDEPENDENCE(year),
           holiday.US_LABOR(year),
           holiday.US_COLUMBUS(year),
           holiday.US_VETERANS_NEAREST_WEEKDAY(year),
           holiday.US_THANKSGIVING(year),
           holiday.US_CHRISTMAS(year))
    return(sort(h))
}
holiday.US_HALFDAY <- function(year) {
    # US early closings (at 1pm since around 2000) on days before Independence and Christmas day, and day after Thanksgiving
    h <- c(dateWarp(holiday.US_INDEPENDENCE(year), '-1 days'),
           dateWarp(holiday.US_THANKSGIVING(year), '+1 days'),
           dateWarp(holiday.US_CHRISTMAS(year), '-1 days'))
    return(sort(filterWeekdays(h)))
}
