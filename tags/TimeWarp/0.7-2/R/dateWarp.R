dateWarp <- function(date, spec, holidays = NULL, by = "bizdays",
                     direction = 1, duplicates.keep = TRUE)
{
    ### BEGIN ARGUMENT PROCESSING ###

    if (!hasArg(date))
        stop("'date' argument missing.")

    if (!hasArg(spec))
        stop("'spec' argument missing.")

    if (!inherits(date, "Date"))
    {
        date <- dateParse(date)
        if (is.null(date))
            stop("'date' argument must inherit from the 'Date' class.")
    }

    if (!is.null(by))
    {
        if (!is.character(by))
            stop("'by' must be a character vector.")

        if (length(by) > 1)
        {
            by <- by[1]
            warning("only the first element of 'by' will be used.")
        }

        if ((atPos <- regexpr("@", by)[1]) > 0)
        {
            byStr <- by
            by <- substring(byStr, 1, atPos - 1)

            if (!is.null(holidays))
                stop("double specification of holidays.")

            holidays <- substring(byStr, atPos + 1)
            if (holidays == "")
                stop("could not parse holiday name out of '", byStr, "'.")

            if (by == "" ||
                !(by %in% c('days', 'bizdays', 'weeks', 'months', 'years')))
                stop("could not parse 'by' out of '", byStr, "'.")
        }
        else
            if (!(by %in% c('days', 'bizdays', 'weeks', 'months', 'years')))
                stop("'by' must contain only the values 'days', 'bizdays','weeks', 'months' or 'years'.")
    }

    if (!is.null(holidays))
    {
        if ((!is.null(by) && by != "bizdays") &&
            !("bizdays" %in% unlist(spec, use.names = FALSE)))
        {
            warning("ignoring holidays argument. Only relevant when 'by = \"bizdays\"'.")
            holidays <- NULL
        }
        else if (!all(holidays %in% allHolidays()))
        {
            stop(paste('no', holidays[!(holidays %in% allHolidays())][1],'holidays exist.'))
        }
    }

    ## 'direction' is +1 for after, -1 for before
    direction <- as.integer(direction)
    if (!(direction == -1 || direction == 1))
        stop("'direction' must be -1 or 1.")

    duplicates.keep <- as.logical(duplicates.keep)
    if (!is.finite(duplicates.keep) || length(duplicates.keep) != 1)
        stop("'duplicates.keep' must be either TRUE or FALSE.")

    ### END ARGUMENT PROCESSING ###

    if (is.numeric(spec) || is.character(spec))
        spec <- list(spec)

    if (is.list(spec))
    {
        for (i in seq_along(spec))
        {
            byUse <- NULL
            holidaysUse <- NULL

            name <- if (is.null(names(spec))) "" else names(spec)[i]

            if ((name == "latest" || name == "earliest") && is(spec[[i]], "character"))
                op <- dateParse(spec[[i]])
            else
                op <- spec[[i]]

            if (is(op, "character") && (name == "" || name == "shift"))
            {
                ## Parse something like "+3 bizdays@NYSEC", "+3 bizdays", or "+3".
                ## (It's necessary to supply 'by' to dateWarp(), but it can be picked
                ## up from elsewhere.)
                opStr <- op

                if ((spacePos <- regexpr(" ", opStr)[1]) != -1)
                {
                    byStr <- substring(opStr, spacePos + 1)
                    opStr <- substring(opStr, 1, spacePos - 1)

                    if ((atPos <- regexpr("@", byStr)[1]) > 0)
                    {
                        byUse <- substring(byStr, 1, atPos - 1)
                        holidaysUse <- substring(byStr, atPos + 1)
                        if (holidaysUse == "")
                            stop("could not parse holiday name out of '", byStr, "'.")
                    }
                    else
                    {
                        byUse <- byStr
                    }
                    if (byUse == "" ||
                        !(byUse %in% c('days', 'bizdays', 'weeks', 'months', 'years')))
                        stop("could not parse 'by' out of '", byStr, "'.")
                }
                # all that is left in "a.str" should a number like "-1" or "+3"
                op <- as.numeric(opStr)

                if (any(is.na(op)))
                    stop("could not parse some elements of 'spec' vector to numbers: ",
                         paste(opStr[is.na(op)][seq(len=min(3, sum(is.na(op))))], collapse=", "),
                         if (sum(is.na(op))>3) " ...")
            }

            if (is(op, "numeric"))
            {
                if (name == "" || name == "shift")
                {
                    if (is.null(byUse) && !is.null(by))
                        byUse <- by
                    if (is.null(byUse))
                        stop("must supply 'by'")

                    if (is.null(holidaysUse) && !is.null(holidays))
                        holidaysUse <- holidays

                    res <- emptyDate()
                    for (i in seq_along(op))
                    {
                        if (op[i] == 0)
                            tmp <- dateAlign(date, by = byUse, direction = direction,
                                             holidays = holidaysUse, silent = TRUE)
                        else if (op[i] < 0)
                            tmp <- dateShift(date, by = byUse, k.by = -op[i], direction = -direction,
                                             holidays = holidaysUse, silent = TRUE)
                        else
                            tmp <- dateShift(date, by = byUse, k.by = op[i], direction = direction,
                                             holidays = holidaysUse, silent = TRUE)

                        res <- c(res, tmp)
                    }

                    if (!duplicates.keep)
                        res <- unique(res)

                    date <- res

                }
                else if (name == "unique")
                {
                    date <- unique(date)
                }
                else
                {
                    stop("names of integer operations can be '', 'shift' or 'unique'.")
                }

            }
            else if (is(op, "logical") && name == "unique")
            {
                date <- unique(date)
            }
            else if (inherits(op, "Date"))
            {
                if (name == "latest")
                    date <- pmin(date, op)
                else if (name=="earliest")
                    date <- pmax(date, op)
                else
                    stop("names of Date can be \"latest\" or \"earliest\"")
            }
            else if (is.list(op))
            {
                ## A list element is treated as a list of arguments for dateAlign or dateShift.
                if (name == "align" && any(is.element(c("to", "table"), names(op))))
                {
                    names(op)[names(op) == "to"] <- "table"
                    date <- op$table[do.call("dateMatch", c(list(x = date), op))]
                }
                else if (name == "align")
                {
                    ## For arguments "by", "direction", and "holidays", substitute a default if not supplied.
                    if (all(is.na(pmatch(names(op), "by"))))
                    {
                        if (is.null(by))
                            stop("must supply 'by'.")
                        op$by <- by
                    }
                    if ((atPos <- regexpr("@", op$by)[1]) > 0)
                    {
                        if (any(!is.na(i <- pmatch(names(op), "holidays"))))
                            stop("double specification of holidays in '", name, "' component.")
                        op$holidays <- substring(op$by, atPos+1)
                        if (op$holidays == "")
                            stop("could not parse holiday name out of '", op$by, "'.")
                        op$by <- substring(op$by, 1, atPos-1)
                    }
                    if (all(is.na(pmatch(names(op), "direction"))))
                        op$direction <- direction

                    if (all(is.na(pmatch(names(op), "holidays"))))
                        op$holidays <- holidays

                    date <- do.call("dateAlign", c(list(x = date, silent = TRUE), op))
                }
                else if (name == "shift")
                {
                    ## For arguments "by" and "holidays", substitute a default if not supplied
                    if (all(is.na(pmatch(names(op), "by"))))
                    {
                        if (is.null(by))
                            stop("must supply 'by'.")
                        op$by <- by
                    }
                    if ((atPos <- regexpr("@", op$by)[1]) > 0)
                    {
                        if (any(!is.na(i <- pmatch(names(op), "holidays"))))
                            stop("double specification of holidays in '", name, "' component.")
                        op$holidays <- substring(op$by, atPos+1)
                        if (op$holidays == "")
                            stop("could not parse holiday name out of '", op$by, "'.")
                        op$by <- substring(op$by, 1, atPos-1)
                    }
                    ## Check if unnamed item is intended to be a k.by argument.
                    if (all(is.na(pmatch(names(op), "k.by"))) &&
                        length(i <- which(names(op) == "")))
                    {
                        if (is.numeric(op[[i]]))
                            names(op)[i] <- "k.by"
                    }
                    if (all(is.na(pmatch(names(op), "direction"))))
                        op$direction <- direction

                    if (all(is.na(pmatch(names(op), "holidays"))))
                        op$holidays <- holidays

                    if ("k.by" %in% names(op) && op$k.by < 0)
                    {
                        op$k.by <- -op$k.by
                        op$direction <- -op$direction
                    }

                    date <- do.call("dateShift", c(list(x = date, silent = TRUE), op))
                }
                else
                    stop("names of lists must be 'align' or 'shift'.")
            }
        }
    }

    if (!duplicates.keep)
        date <- unique(date)

    date
}

dateWarpAppend <- function(dates, ..., where=c("sorted", "start", "end"), empty.ok=FALSE, duplicates.ok=FALSE) {
    where <- match.arg(where)
    if (!inherits(dates, 'Date'))
        dates <- dateParse(dates)
    new.dates <- dateWarp(dates, ...)
    if (!empty.ok && length(new.dates)==0)
        stop("no new dates to add")
    if (where=="sorted") {
        if (!all(diff(dates) >= 0))
            stop("input dates are not sorted")
        dates <- sort(c(new.dates, dates))
    } else if (where=="start") {
        dates <- c(new.dates, dates)
    } else {
        dates <- c(dates, new.dates)
    }
    if (!duplicates.ok){
        dups <- duplicated(dates)
        if (any(dups)) dates <- dates[!dups]
    }
    dates
}
