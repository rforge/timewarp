##
## Unfortunately, 'format.POSIXct' is not compatible between
## Linux and Windows R. Here are a few for the difference I have found:
## 1) Windows version does not recognize "%y" format.
## 2) Windows version does not recognize width arguments like
##    "%02d".
## 3) Windows and Linux does not agree on the meaning of "%Y".
##    Under Windows it means "%04Y"; under Linux it print with
##    minimal width.
##
## This function formats any kind of data objects to character
## strings with the default format "%02m/%02d/%04Y" under
## both Linux and Windows.
##
dateFormat <- function(date, format = NULL)
{
    if (is.null(format))
    {
        if (Sys.info()[["sysname"]] == "Linux")
            format <- "%02m/%02d/%04Y"
        else
            format <- "%m/%d/%Y"
    }

    if (is(date, "character"))
        date <- dateParse(date)

    if (is(date, "dates"))
        format(as.POSIXct(date), format)
    else if (is(date, "Date") || is(date, "POSIXt"))
        format(date, format)
    else
        stop("unknow date format: '", class(date), "'")
}
