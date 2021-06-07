## code from package 'datetimeutils'

.year <- function(x)
    as.POSIXlt(x)$year + 1900

.month <- function(x)
    as.POSIXlt(x)$mon + 1

.week <- function(x, WKST = 1) {

    ## RFC 5545, 3.3.10.
    ## 
    ## Week number one of the calendar year is the first
    ## week that contains at least four (4) days in that
    ## calendar year.

    if (!inherits(x, "Date"))
        x <- as.Date(as.POSIXlt(x))

    x <- (x + 4 - ((.weekday(x) - WKST) %% 7 +1))
    y1 <- x - as.POSIXlt(x)$yday
    as.numeric(x - y1) %/% 7+1
}

.start_of_week <- function(x, WKST = 1, Date = TRUE) {

    ## map a given date to the start of its week
    offset <- if (WKST == 0)
                  3
              else
                  WKST - 4

    if (Date)
        .Date(7 * (unclass(x - offset) %/% 7) + offset)
    else
              7 * (unclass(x - offset) %/% 7) + offset
}

