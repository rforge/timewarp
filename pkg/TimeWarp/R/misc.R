years <- function(x) {
    # Optimize to only convert unique values in x
    y <- unique(x)
    if (length(x) > 10) {
        y <- unique(x)
        if (length(y) >= length(x))
            y <- NULL
    }
    if (!is.null(y)) {
        i <- match(x, y)
        return(as.POSIXlt(y)$year[i] + 1900)
    } else {
        return(as.POSIXlt(x)$year + 1900)
    }
}

emptyDate <- function() .dateParse.origin[0]

is.wholenumber <- function(x) any(floor(x) == x)

