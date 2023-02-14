## copied from datetimeutils https://github.com/enricoschumann/datetimeutils

.mday <- function(x)
    as.POSIXlt(x)$mday

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

.weekday <- function(x)
    ## maps Date to a number (0 Sunday .. 6 Saturday)
    unclass(x + 4) %% 7

.wday <- c(
    "SU" = 0,
    "MO" = 1,
    "TU" = 2,
    "WE" = 3,
    "TH" = 4,
    "FR" = 5,
    "SA" = 6)

.weekdayS <- function(dates)
        c("SU",
          "MO",
          "TU",
          "WE",
          "TH",
          "FR",
          "SA")[unclass(dates + 4) %% 7 + 1]

.next_weekday <- function(wday, start, count = 1, interval = 1) {
    ## wday : 0 SU, 1 MO ... => target day
    ## start: a date
    if (count == 0)
        return(numeric(0L))
    ans <- (wday - .weekday(start)) %% 7 +
        rep(7 * interval * seq(0, count - 1), each = length(wday))
    ans <- sort(ans) + start
    ans[seq_len(count)]
}

.date2char <- function(d)
    format(d, format = "%Y%m%d")

.char2date <- function(s)
    as.Date(s, format = "%Y%m%d")
