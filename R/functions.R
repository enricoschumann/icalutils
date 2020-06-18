.eol.strict     <- "\r\n"
.fold.re        <- "\n[ \t]"
.fold.re.strict <- "\r\n[ \t]"

.wday <- c(
    "SU" = 0,
    "MO" = 1,
    "TU" = 2,
    "WE" = 3,
    "TH" = 4,
    "FR" = 5,
    "SA" = 6)

.fold <- function(s) {
    i <- nchar(s) > 75

}

.unfold <- function(s, strict.eol) {
    s <- gsub(if (strict.eol) .fold.re.strict else .fold.re, "", s)
    strsplit(s, "\r?\n")[[1L]]
}

.read_one_line <- function(filename, nchars = 200) {
    zz <- file(filename, "rb")
    on.exit(close(zz))
    vcal <- list()
    i <- 0
    while(length(txt <- readChar(zz, nchars))) {
        i <- i + 1
        vcal[[i]] <- txt
    }
    paste(unlist(vcal), collapse = "")
}

read_icalendar <-
function(file,
         strict.eol = TRUE,
         use.OlsonNames = TRUE,
         uid.names = FALSE,
         keep.source = TRUE,
         components = c("VEVENT", "VTODO", "VJOURNAL",
                        "VFREEBUSY", "VTIMEZONE"),
         ...) {

    if (use.OlsonNames)
        tz.names <- OlsonNames()
    else
        tz.names <- character(0)


    cal.txt <- .read_one_line(file)
    cal.txt <- .unfold(cal.txt, strict.eol)


    ## TODO: text operations -- remove \, etc?
    cal <- .properties(cal.txt)
    if (keep.source)
        for (i in seq_along(cal))
            attr(cal[[i]], "source") <- cal.txt[i]
    cal <- .expand_properties(cal, tz.names = tz.names)


    ans <- list()
    for (C in components) {
        ## grep the original content lines
        begin <- grep(paste0("^BEGIN:", C), cal.txt, ignore.case = TRUE)
        end <- grep(paste0("^END:", C),     cal.txt, ignore.case = TRUE)
        for (i in seq_along(begin)) {
            ans.i <- cal[seq(begin[i] + 1L, end[i] - 1L)]
            class(ans.i) <- tolower(C)
            ans <- c(ans, list(ans.i))
        }
    }
    if (uid.names)
        names(ans) <-
            sapply(ans, function(x)
                if (is.null(x[["UID"]])) NA else x[["UID"]])
    class(ans) <- c("icalendar")
    ans

}

print.icalendar <-
function(x, ...) {

    len <- length(x)
    cat("An icalendar file with", len, "components.\n")
    invisible(x)
}

as.data.frame.icalendar <-
function(x,
         row.names = NULL,
         optional = FALSE,
         adjust.allday = TRUE,
         recur.expand = FALSE,
         recur.until = NULL,
         recur.count = NULL,
         use.OlsonNames = TRUE,
         all.timestamps.POSIXct = TRUE,
         all.timestamps.Date = FALSE,
         timestamps.tz = "",
         components = c("VEVENT", "VTODO"),
         ...) {

    x <- x[unname(unlist(lapply(x, "class"))) %in% tolower(components)]

    res.df <- data.frame()
    fields <- c("UID", "SUMMARY", "DESCRIPTION", "LOCATION")
    for (f in fields) {
        tmp <- lapply(x, `[[`, f)
        if (f == fields[1L])
            res.df <- data.frame(unlist(
                lapply(tmp, function(x) if (is.null(x)) NA else x)),
                stringsAsFactors = FALSE)
        else
            res.df <- cbind(res.df,
                            unlist(
                                lapply(tmp, function(x) if (is.null(x)) NA else x)),
                            stringsAsFactors = FALSE)
    }
    names(res.df) <- tolower(fields)

    start <- lapply(x, function(x) if (is.null(x[["DTSTART"]])) NA else x[["DTSTART"]])
    end   <- lapply(x, function(x) if (is.null(x[["DTEND"]]))   NA else x[["DTEND"]])

    all.day <- unlist(lapply(start, function(x) inherits(x, "Date")))
    res.df <- cbind(res.df, all.day = all.day, stringsAsFactors = FALSE)

    if (adjust.allday)
        end[all.day] <- lapply(end[all.day], function(x) x[[1L]] - 1)

    recurring <- unlist(lapply(x, function(x) "RRULE" %in% names(x)))
    res.df <- cbind(res.df, recurring = recurring, stringsAsFactors = FALSE)

    ## compute recurrances from original _start_ and _end_
    ## but allow for all.day adjustments
    if (recur.expand) {
        recurring.events <- list()
        RRULE <- lapply(x[recurring], `[[`, "RRULE")
        for (r in seq_along(RRULE)) {
            ## if ("EXDATE" %in% names(x[recurring][[r]]))

            created <- .expand_rrule(start[recurring][[r]],
                                     end  [recurring][[r]],
                                     RRULE = RRULE[[r]],
                                     UNTIL = recur.until,
                                     COUNT = recur.count)
            if (isFALSE(created))
                message("cannot parse RRULE",
                        attr(RRULE[[r]], "RRULE"))
            recurring.events[[r]] <- created
        }
    }

    if (all.timestamps.POSIXct) {

        ## TODO: add original dates?
        ## attr(res.df, "start.Date") <- .Date(unlist(start[all.day]))
        ## attr(res.df, "end.Date") <- .Date(unlist(end[all.day]))

        start[all.day] <- lapply(start[all.day],
                                 function(x) as.POSIXct(paste(x, "00:00:00"),
                                                        tz = timestamps.tz))
        start <- .POSIXct(unlist(start))
        end[all.day] <- lapply(end[all.day],
                               function(x) as.POSIXct(paste(x, "00:00:00"),
                                                      tz = timestamps.tz))
        end <- .POSIXct(unlist(end))

        res.df <- cbind(res.df, start = start, end = end,
                        stringsAsFactors = FALSE)


    } else if (all.timestamps.Date) {
        message ("not implemented")
    } else {
        message ("not implemented")
        ## TODO create two columns
    }


    ## with all fields filled, add recurrences
    if (recur.expand) {
        res.df <- cbind(res.df, uid.parent = NA_character_,
                        stringsAsFactors = FALSE)
        ri <- which(recurring)
        for (r in seq_along(ri)) {
            if (isFALSE(recurring.events[[r]]) ||  ## RRULE could not be expanded
                nrow(recurring.events[[r]]) <= 1L)
                next
            copy <- res.df[rep(ri[r], nrow(recurring.events[[r]])-1), ]
            copy$start <- recurring.events[[r]]$DTSTART[-1L]
            copy$end <- recurring.events[[r]]$DTEND[-1L]
            copy$uid.parent <- res.df$uid[1L]
            copy$uid <- NA
            res.df <- rbind(res.df, copy)
        }
    }
    res.df
}


.weekday <- function(dates)
    unclass(dates + 4) %% 7

.weekdayS <- function(dates)
        c("SU",
          "MO",
          "TU",
          "WE",
          "TH",
          "FR",
          "SA")[unclass(dates + 4) %% 7 + 1]

.next_weekday <- function(wday, start, count = 1, interval = 1)
    start + wday - unclass(start + 4) %% 7 +
        rep(interval*7L*(seq_len(count) - 1L), each = length(wday))

.is_ical_date <- function(s, ...)
    grepl("^[0-9]{8}$", s)

.date <- function(s, ...)
    as.Date(s, "%Y%m%d")

.utc_dt <- function(s, ...) {
    ## trailing Z does not have to be removed
    as.POSIXct(s, format = "%Y%m%dT%H%M%S", tz = "UTC")
}

.local_dt <- function(s) {
    ans <- as.POSIXct(s, format = "%Y%m%dT%H%M%S", tz = "UTC")
    attr(ans, "icalutils_localtime") <- TRUE
    ans
}

## format time into character representation of UTC time
## (basic format 8601: YmdHMSZ)
.z <- function(t)
    strftime(t, format = "%Y%m%dT%H%M%SZ", tz = "UTC")

.parse_rrule <- function(RRULE, ...) {

    ## takes a vector of one or more rrules (character) and
    ## returns a list of lists

    rules <- strsplit(RRULE, ";", fixed = TRUE)
    rules <- lapply(rules, strsplit, "=", fixed = TRUE)
    r.names <- lapply(rules, function(x) lapply(x, `[[`, 1))
    r.names <- lapply(r.names, unlist)
    rules <- lapply(rules, function(x) lapply(x, `[`, -1))

    rules <- mapply(`names<-`, rules, r.names, SIMPLIFY = FALSE)

    rules <- lapply(rules,
                    function(x) {
        nm <- names(x)
        if ("INTERVAL" %in% nm)
            x[["INTERVAL"]] <- as.numeric(x[["INTERVAL"]])
        else
            x[["INTERVAL"]] <- 1

        if ("COUNT" %in% nm)
            x[["COUNT"]] <- as.numeric(x[["COUNT"]])

        if ("UNTIL" %in% nm)
            if (.is_ical_date(x[["UNTIL"]]))
                x[["UNTIL"]] <- .date(x[["UNTIL"]])
            else
                x[["UNTIL"]] <- .utc_dt(x[["UNTIL"]])

        if ("BYMONTH" %in% nm) {
            tmp <- strsplit(x[["BYMONTH", exact = TRUE]], ",", fixed = TRUE)[[1]]
            tmp <- as.numeric(tmp)
            x[["BYMONTH"]] <- tmp
        }
        if ("BYDAY" %in% nm) {
            BYDAY <- list()
            tmp <- toupper(strsplit(x$BYDAY, ",", fixed = TRUE)[[1]])
            if (any(grepl("[+-]?\\d+[A-Z]+", tmp))) {
                BYDAY[["wday"]] <- sub("[+-]?\\d+([A-Z]+)", "\\1", tmp)
                BYDAY[["n"]] <- as.numeric(sub("([+-]?\\d+)[A-Z]+", "\\1", tmp))
            } else {
                BYDAY[["wday"]] <- tmp
                BYDAY[["n"]] <- rep(0, length(tmp))
            }
            x[["BYDAY"]] <- BYDAY
        }

        if ("BYMONTHDAY" %in% nm) {
            tmp <- strsplit(x$BYMONTHDAY, ",", fixed = TRUE)[[1]]
            tmp <- as.numeric(tmp)
            x[["BYMONTHDAY"]] <- tmp
        }
        if ("BYSETPOS" %in% nm) {
            tmp <- strsplit(x$BYSETPOS, ",", fixed = TRUE)[[1]]
            tmp <- as.numeric(tmp)
            x[["BYSETPOS"]] <- tmp
        }

        class(x) <- "RRULE"
        x
    })

    for (i in seq_along(rules))
        attr(rules[[i]], "RRULE") <- RRULE[i]
    rules
}

.expand_rrule <-
function(DTSTART, DTEND,
         RRULE, RDATE, EXDATE,
         UNTIL = NULL, COUNT = NULL) {

    RRULE.text <- attr(RRULE, "RRULE")
    FREQ <- toupper(RRULE$FREQ)
    if (is.null(RRULE$INTERVAL))
        INTERVAL <- 1 else INTERVAL <- RRULE$INTERVAL

    DTSTART.isdate <- inherits(DTSTART, "Date")
    DTSTARTlt <- as.POSIXlt(DTSTART)

    BYSECOND   <- RRULE[["BYSECOND",   exact = TRUE]]
    BYMINUTE   <- RRULE[["BYMINUTE",   exact = TRUE]]
    BYHOUR     <- RRULE[["BYHOUR",     exact = TRUE]]
    BYDAY      <- RRULE[["BYDAY",      exact = TRUE]]
    BYMONTHDAY <- RRULE[["BYMONTHDAY", exact = TRUE]]
    BYYEARDAY  <- RRULE[["BYYEARDAY",  exact = TRUE]]
    BYWEEKNO   <- RRULE[["BYWEEKNO",   exact = TRUE]]
    BYMONTH    <- RRULE[["BYMONTH",    exact = TRUE]]
    BYSETPOS   <- RRULE[["BYSETPOS",   exact = TRUE]]
    WKST       <- RRULE[["WKST",       exact = TRUE]]

    violations <- NULL

    if (!is.null(RRULE$UNTIL)) {

        ## use UNTIL if UNTIL is 1) specified and 2) smaller than RRULE$UNTIL,
        if (is.null(UNTIL) || as.POSIXct(UNTIL) < as.POSIXct(RRULE$UNTIL))
            UNTIL <- RRULE$UNTIL
        if (!inherits(UNTIL, class(DTSTART))) {
            ## message("DTSTART and UNTIL are not of the same type [3.3.10.]")
            violations <- c(violations,
                            "DTSTART and UNTIL are not of the same type [RFC 5545, 3.3.10.]")
            if (!inherits(DTSTART, "Date"))
                UNTIL <- as.Date(UNTIL)
        }
    } else if (is.null(COUNT) && !is.null(RRULE$COUNT)) {
        COUNT <- RRULE$COUNT
    } else if (is.null(RRULE$UNTIL) &&
               is.null(RRULE$COUNT) &&
               is.null(UNTIL) &&
               is.null(COUNT)) {
        ## neither COUNT nor UNTIL are specified in RRULE,
        ## and recur.count/recur.until are NULL

        if (DTSTART.isdate)
            UNTIL <- Sys.Date() + 365*5
        else {
            UNTIL <- as.POSIXct(Sys.Date() + 365*5)
        }
        message("no UNTIL/COUNT specified: repeat until ", UNTIL)
    }
    if (DTSTART.isdate && inherits(UNTIL, "POSIXct"))
        UNTIL <- as.Date(as.POSIXlt(UNTIL))
    else if (!DTSTART.isdate && inherits(UNTIL, "Date"))
        UNTIL <- as.POSIXct(as.POSIXlt(UNTIL))

    ans <- FALSE
    if (FREQ == "YEARLY") {

        if        ( is.null(BYSECOND)   &&
                    is.null(BYMINUTE)   &&
                    is.null(BYHOUR)     &&
                    is.null(BYDAY)      &&
                    is.null(BYMONTHDAY) &&
                    is.null(BYYEARDAY)  &&
                    is.null(BYWEEKNO)   &&
                    is.null(BYMONTH)    &&
                    is.null(BYSETPOS)   &&
                    is.null(WKST)) {

            ## rules work for Date and POSIXct
            if (!is.null(UNTIL))
                DTSTARTs <- seq(DTSTART, to = UNTIL, by = paste(INTERVAL, "year"))
            else
                DTSTARTs <- seq(DTSTART, length.out = COUNT, by = paste(INTERVAL, "year"))
            if (!is.null(DTEND)) {
                DTENDs <- DTSTARTs + unclass(DTEND - DTSTART)
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND   = DTENDs)
            } else {
                ans <- data.frame(DTSTART = DTSTARTs)
            }

        } else if ( is.null(BYSECOND)   &&
                    is.null(BYMINUTE)   &&
                    is.null(BYHOUR)     &&
                   !is.null(BYDAY)      &&
                 ## is.null(BYMONTHDAY) &&
                    is.null(BYYEARDAY)  &&
                    is.null(BYWEEKNO)   &&
                   !is.null(BYMONTH)    &&
                    is.null(BYSETPOS)   &&
                    is.null(WKST)) {

            dates <- seq(as.Date(DTSTART, "%Y%m%d"),
                         as.Date(UNTIL, "%Y%m%d"), by = "1 day")
            dates <- dates[month(dates) %in% RRULE$BYMONTH]

            if (!is.null(RRULE$BYMONTHDAY)) {
                if (any(RRULE$BYMONTHDAY < 0))
                    warning("negative BYMONTHDAY in ", attr(RRULE, "RRULE") )
                dates <- dates[mday(dates) %in% RRULE$BYMONTHDAY]

            }

            dates <- dates[.weekday(dates) %in% .wday[RRULE$BYDAY$wday]]
            dates <- .Date(tapply(dates, year(dates), `[`, RRULE$BYDAY$n))

            if (inherits(DTSTART, "POSIXct")) {
                DTSTARTs <- as.POSIXct(paste(as.character(dates),
                                             DTSTARTlt$hour,
                                             DTSTARTlt$min,
                                             DTSTARTlt$sec),
                                       format = "%Y-%m-%d %H %M %S",
                                       tz = "UTC")
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND = NA)
            }

        } else if ( is.null(RRULE$BYSECOND)   &&
                    is.null(RRULE$BYMINUTE)   &&
                    is.null(RRULE$BYHOUR)     &&
                    !is.null(RRULE$BYDAY)      &&
                    !is.null(RRULE$BYMONTHDAY) &&
                    is.null(RRULE$BYYEARDAY)  &&
                    is.null(RRULE$BYWEEKNO)   &&
                    !is.null(RRULE$BYMONTH)    &&
                    is.null(RRULE$BYSETPOS)   &&
                    is.null(RRULE$WKST)) {

            NA
        } else {

            ## ORDER: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
            ## BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL a
            dates <- seq(as.Date(DTSTART), as.Date(UNTIL), by = "1 day")

            ## BY MONTH
            if (!is.null(RRULE$BYMONTH))
                dates <- dates[month(dates) %in% RRULE$BYMONTH]
            else
                dates <- dates[month(dates) %in% month(DTSTART)]

            ## TODO BYWEEKNO
            ## TODO BYYEARDAY
            ## TODO BYMONTHDAY
            if (!is.null(RRULE$BYMONTHDAY))
                dates <- dates[mday(dates) %in% RRULE$BYMONTHDAY]
            else
                dates <- dates[mday(dates) %in% mday(DTSTART)]

            ## TODO BYDAY



            if (inherits(DTSTART, "POSIXct")) {
                DTSTARTs <- as.POSIXct(paste(as.character(dates),
                                             DTSTARTlt$hour,
                                             DTSTARTlt$min,
                                             DTSTARTlt$sec),
                                       format = "%Y-%m-%d %H %M %S",
                                       tz = "UTC")
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND = NA)
            }

        }

    } else if (FREQ == "MONTHLY") {
        ## browser()


        if (##is.null(BYSECOND)   &&
            ##is.null(BYMINUTE)   &&
            ##is.null(BYHOUR)     &&
            ## is.null(BYDAY)      &&
            ## is.null(BYMONTHDAY) &&
            is.null(BYYEARDAY)  &&
            is.null(BYWEEKNO)   &&
            ##is.null(BYMONTH)    &&
            ## is.null(BYSETPOS)   &&
            is.null(WKST)) {

            if (is.null(UNTIL) && !is.null(COUNT))
                dates <- seq(as.POSIXct(as.character(DTSTART), tz = "UTC"),
                             by = "1 day",
                             length.out = COUNT*31)
            else
                dates <- seq(as.POSIXct(as.character(DTSTART), tz = "UTC"),
                             as.POSIXct(as.character(UNTIL), tz = "UTC"),
                             by = "1 day")

            if (!is.null(BYMONTHDAY))
                dates <- dates[mday(dates) %in% BYMONTHDAY]
            if (!is.null(BYMONTH))
                dates <- dates[ month(dates) %in% RRULE[["BYMONTH", exact = TRUE]] ]

            if (!is.null(BYDAY)) {
                if (all(BYDAY$n == 0)) {
                    dates <- dates[.weekdayS(as.Date(dates)) %in% BYDAY$wday]

                    if (!is.null(BYSETPOS)) {
                        dates <- tapply(dates, format(dates, "%Y-%m"),
                                        function(x) if (length(x) < max(BYSETPOS))
                                                        NULL else x[BYSETPOS])
                        if (is.list(dates))
                            dates <- unlist(dates)
                        dates <- unname(.POSIXct(dates, tz = "UTC"))
                    }
                }
            }
            if (!is.null(COUNT))
                dates <- dates[1:COUNT]

            DTSTARTs <- dates
            if (!is.null(DTEND))
                DTENDs  <- DTSTARTs + (DTEND - DTSTART)
            if (!DTSTART.isdate) {
                DTSTARTs <- as.POSIXct(as.character(DTSTARTs),
                                       tz = attr(DTSTART, "tzone"))
                if (!is.null(DTEND))
                    DTENDs   <- as.POSIXct(as.character(DTENDs),
                                           tz = attr(DTSTART, "tzone"))
            } else {
                DTSTARTs <- as.Date(as.POSIXlt(DTSTARTs))
                if (!is.null(DTEND))
                    DTENDs   <- as.Date(as.POSIXlt(DTENDs))
            }
            if (!is.null(DTEND))
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND   = DTENDs)
            else
                ans <- data.frame(DTSTART = DTSTARTs)


        } else {

            ## ORDER: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
            ## BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL
            dates <- seq(as.Date(DTSTART), as.Date(UNTIL), by = "1 day")

            ## TODO BY MONTH
            ## TODO BYWEEKNO
            ## TODO BYYEARDAY
            ## TODO BYMONTHDAY
            if (!is.null(RRULE$BYMONTHDAY))
                dates <- dates[mday(dates) %in% RRULE$BYMONTHDAY]
            else
                dates <- dates[mday(dates) %in% month(DTSTART)]

            ## TODO BYDAY

            ## TODO BYHOUR
            datetimes <- as.POSIXlt(dates, tz = attr(DTSTART, "tzone"))
            if (!is.null(RRULE$BYHOUR))
                NA  ## FIXME
            else
                datetimes$hour <- DTSTARTlt$hour

            ## TODO BYMINUTE
            if (!is.null(RRULE$BYMINUTE))
                NA  ## FIXME
            else
                datetimes$min <- DTSTARTlt$min

            ## TODO BYSECOND
            ## TODO BYSETPOS

            ## ans <- data.frame(DTSTART = DTSTARTs,
            ##                   DTEND = DTENDs)





        }
    } else if (FREQ == "WEEKLY") {
        if        ( is.null(RRULE$BYSECOND)   &&
                    is.null(RRULE$BYMINUTE)   &&
                    is.null(RRULE$BYHOUR)     &&
                   !is.null(RRULE$BYDAY)      &&
                    is.null(RRULE$BYMONTHDAY) &&
                    is.null(RRULE$BYYEARDAY)  &&
                    is.null(RRULE$BYWEEKNO)   &&
                    is.null(RRULE$BYMONTH)    &&
                    is.null(RRULE$BYSETPOS)   &&
                    is.null(RRULE$WKST)) {

            if (DTSTART.isdate) {
                if (is.null(COUNT))
                    COUNT <- ceiling(as.numeric(as.Date(UNTIL) - DTSTART)/(7*INTERVAL))

                ans <- .next_weekday(.wday[RRULE$BYDAY$wday],
                                     start = DTSTART,
                                     count = COUNT,
                                     interval = INTERVAL)
                DTSTARTs <- ans[ans <= as.Date(UNTIL)]
                DTENDs <- DTSTARTs + unclass(DTEND - DTSTART)
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND = DTENDs)

            } else {
                if (is.null(COUNT))
                    COUNT <- ceiling((as.numeric(UNTIL) - as.numeric(DTSTART))/(7*INTERVAL)/86400)
                dates <- .next_weekday(.wday[RRULE$BYDAY$wday],
                                       start = as.Date(DTSTART),
                                       count = COUNT,
                                       interval = INTERVAL)
                DTSTARTs <- as.POSIXct(paste(dates,
                                             DTSTARTlt$hour,
                                             DTSTARTlt$min,
                                             DTSTARTlt$sec),
                                       format = "%Y-%m-%d %H %M %S",
                                       tzone = attr(DTSTART, "tzone"))

                ## FIXME: the difference between DTSTART and DTEND
                ## should be independent of timezone changes
                DTENDs <- DTSTARTs + unclass(DTEND) - unclass(DTSTART)
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND = DTENDs)
            }
        }
    } else if (FREQ == "DAILY") {
        if        ( is.null(RRULE$BYSECOND)   &&
                    is.null(RRULE$BYMINUTE)   &&
                    is.null(RRULE$BYHOUR)     &&
                    is.null(RRULE$BYDAY)      &&
                    is.null(RRULE$BYMONTHDAY) &&
                    is.null(RRULE$BYYEARDAY)  &&
                    is.null(RRULE$BYWEEKNO)   &&
                    is.null(RRULE$BYMONTH)    &&
                    is.null(RRULE$BYSETPOS)   &&
                    is.null(RRULE$WKST)) {

            if (DTSTART.isdate) {
                ans <- FALSE

            } else {
                if (is.null(COUNT))
                    COUNT <- ceiling((as.numeric(UNTIL) - as.numeric(DTSTART))/(INTERVAL)/86400)

                dates <- as.Date(DTSTART) + INTERVAL*(seq_len(COUNT)-1)
                DTSTARTs <- as.POSIXct(paste(dates,
                                             DTSTARTlt$hour,
                                             DTSTARTlt$min,
                                             DTSTARTlt$sec),
                                       format = "%Y-%m-%d %H %M %S",
                                       tzone = attr(DTSTART, "tzone"))

                ## FIXME: the difference between DTSTART and DTEND
                ## should be independent of timezone changes
                if (is.null(DTEND))
                    ans <- data.frame(DTSTART = DTSTARTs)
                else {
                    DTENDs <- DTSTARTs + (DTEND - DTSTART)
                    ans <- data.frame(DTSTART = DTSTARTs,
                                      DTEND = DTENDs)
                }
            }
        }
    }

    ## TODO: add RDATEs

    ## TODO: remove EXDATEs


    attr(ans, "rfc.violations") <- violations
    ans

}

.properties <- function(s, ...) {

    ## receives a character vector
    ## (*unfolded* content lines of iCalendar stream),
    ## and returns named list (names = properties).
    ## Attached (as attributes)
    ## to these properties may be "parameters" as
    ## specified by [RFC5545:3.2.]. The property values
    ## are all character.

    p <- "^([^:;]+?)[:;](.*)"
    ans <- gsub(p, "\\2", s, perl = TRUE)
    names(ans) <- gsub(p, "\\1", s, perl = TRUE)
    ans <- as.list(ans)

    ## check for property parameters [RFC5545:3.2.]
    param <- grep("[^:]+;.*:", s)
    for (i in seq_along(param)) {
        attr(ans[[ param[i] ]], "parameters") <-
            sub("(.*?):(.*)", "\\1", ans[[ param[i] ]])
        ans[[ param[i] ]] <-
            sub("(.*?):(.*)", "\\2", ans[[ param[i] ]])
    }
    ans
}

.expand_properties <- function(p, tz.names = character(0)) {

    ## receives a named list (names = properties) with
    ## character entries, and returns a list. The values
    ## should be evaluated to proper R objects
    ## (e.g. datetime becomes POSIXct, etc).

    ans <- p
    txt <- unlist(p)  ## all character, so unlist is safe
    param <- unlist(lapply(ans, function(x)
        if (!is.null(A <- attr(x, "parameters")))
            A else ""))

    nm <- names(ans)
    done <- logical(length(ans))



    ## ----------- DATETIME FIELDS -----------
    dtfields <- c("CREATED",
                  "LAST-MODIFIED",
                  "DTSTAMP",
                  "DTSTART",
                  "DTEND",
                  "DUE",
                  "EXDATE",
                  "RDATE")
    is.dt <- nm %in% dtfields

    if (any(is.dt)) {
        i <- !done & is.dt & endsWith(txt, "Z")
        if (any(i)) {
            i.utc <- which(i)
            v.utc <- .utc_dt(txt[i])
            done[i] <- TRUE
        } else
            i.utc <- NULL

        i <- !done & is.dt & grepl("VALUE=DATE", param, ignore.case = TRUE)
        if (any(i)) {
            i.date <- which(i)
            v.date <- as.Date(txt[i], format = "%Y%m%d")
            done[i] <- TRUE
        } else
            i.date <- NULL

        i <- !done & is.dt & grepl("TZID", param, ignore.case = TRUE)
        if (any(i)) {
            i.tz <- which(i)
            v.tz <- vector("list", length = length(i.tz)) ## a list to preserve  tz

            tz <- sub('TZID="?([^;"]+)"?', "\\1", param[i])

            for (j in seq_along(i.tz)) {

                if (!tz[j] %in% tz.names)
                    if (tz1 <- match(tz[j], .tznames$Windows, nomatch = 0)) {
                        ## TODO collect such messages
                        ## message("map timezone  ",
                        ##         .tznames[["Windows"]][tz1], " => ",
                        ##         .tznames[["Olson"]][tz1])
                        tz1 <- .tznames[["Olson"]][tz1]
                    } else {
                        warning("Timezone not found: ", tz[j], ". Using current tz instead.")
                        tz1 <- ""
                    }
                else
                    tz1 <- tz[j]

                v.tz[[j]] <- as.POSIXct(ans[[i.tz[j]]],
                                        format = "%Y%m%dT%H%M%S",
                                        tz = tz1)
            }

            done[i] <- TRUE
        } else
            i.tz <- NULL

        i <- !done & is.dt & grepl("^[0-9]{8}T[0-9]{6}$", param)
        if (any(i)) {
            i.localtime <- which(i)
            v.localtime <- .local_dt(txt[i])
            done[i] <- TRUE
        } else
            i.localtime <- NULL

    }

    for (j in c(i.utc, i.date, i.tz, i.localtime)) {
        ans[[j]] <- if (j %in% i.utc)
                        v.utc[j == i.utc]
                    else if (j %in% i.date)
                        v.date[j == i.date]
                    else if (j %in% i.localtime)
                        v.localtime[j == i.localtime]
                    else if (j %in% i.tz)
                        v.tz[[which(j == i.tz)]]
        attr(ans[[j]], "source") <- attr(p[[j]], "source")
        if (param[j] != "")
            attr(ans[[j]], "parameters") <- attr(p[[j]], "parameters")
    }



    ## ----------- RRULE -----------
    i <- which(nm %in% "RRULE")
    for (j in i) {
        ans[[j]] <- .parse_rrule(ans[[j]])[[1]]
        attr(ans[[j]], "source") <- attr(p[[j]], "source")
        if (param[j] != "")
            attr(ans[[j]], "parameters") <- attr(p[[j]], "parameters")
    }
    ans
}

ical_structure <- function(file, ..., strict.eol = TRUE) {

    cal <- .read_one_line(file)
    cal <- .unfold(cal, strict.eol)

    n <- length(cal)
    component <- character(n)
    level <-start <- end <- numeric(n)
    current.level <- 0
    for (i in seq_along(cal)) {
        if (grepl("^BEGIN:", cal[i])) {
            current.level <- current.level + 1
            cat(rep("  ", current.level-1), cal[i], "\n", sep = "")
            level[i] <- current.level
            start[i] <- i
            component[i] <- cal[i]
        }
        if (grepl("^END:", cal[i])) {
            current.level <- current.level - 1
            end[i] <- i
        }
    }
    data.frame(component, level, start, end)
}

## copy of https://github.com/enricoschumann/mailtools/blob/master/R/msgID.R
.msgID <- function() {
    digits <- function(k, b) {
        if (all(k == 0L))
            return(rep(0, length(k)))
        nd <- 1 + floor(log(max(k), b))
        ans <- numeric(length(k) * nd)
        dim(ans) <- c(length(k), nd)
        for (i in nd:1) {
            ans[ ,i] <- k %% b
            if (i > 1L)
                k <- k%/%b
        }
        ans
    }
    ab <- c(1:9, 0,letters[1:26])
    n <- c(floor(runif(1)*1e16),
           floor(10000*as.numeric(Sys.time())))
    paste(c(ab[digits(n[1L], 36) + 1], ".",
            ab[digits(n[2L], 36) + 1]), sep = "", collapse="")
}


## copied from datetimeutils https://github.com/enricoschumann/datetimeutils
year <- function(x)
    as.POSIXlt(x)$year + 1900

month <- function(x)
    as.POSIXlt(x)$mon + 1

mday <- function(x)
    as.POSIXlt(x)$mday

rrule <-
function(dtstart,
         dtend = NULL,
         freq,
         until = NULL,
         count = NULL,
         interval = 1,
         bysecond = NULL,
         byminute = NULL,
         byhour = NULL,
         byday = NULL,
         bymonthday = NULL,
         byyearday = NULL,
         byweekno = NULL,
         bymonth = NULL,
         bysetpos = NULL,
         wkst = NULL,
         rdate = NULL,
         exdate = NULL,
         text = NULL) {

    if (!is.null(count) && !is.null(until))
        stop("specify either ", sQuote("count"),
             " or ", sQuote("until"), ", but not both")
    if (!is.null(text)) {
        text <- sub("^RRULE:", "", text, ignore.case = TRUE)
        rrule <- .parse_rrule(text)[[1L]]
    } else {
        rrule <- list(
            FREQ        = freq,
            UNTIL       = until,
            COUNT       = if (!is.null(count)) as.numeric(count),
            INTERVAL    = as.numeric(interval),
            BYSECOND    = bysecond,
            BYMINUTE    = byminute,
            BYHOUR      = byhour,
            BYDAY       = byday,
            BYMONTHDAY  = bymonthday,
            BYYEARDAY   = byyearday,
            BYWEEKNO    = byweekno,
            BYMONTH     = bymonth,
            BYSETPOS    = bysetpos,
            WKST        = wkst
        )
    }

    ans <- list()
    ans$text <- if (!is.null(text))
                    text
                else {
                    rrule1 <- rrule[!unlist(lapply(rrule, is.null))]
                    paste0(paste0(names(rrule1), "=", toupper(rrule1)), collapse = ";")
                }

    ans$recurrence_set <-
        .expand_rrule(DTSTART = dtstart,
                      DTEND   = dtend,
                      RRULE   = .parse_rrule(ans$text)[[1L]],
                      RDATE   = rdate,
                      EXDATE  = exdate,
                      UNTIL   = until,
                      COUNT   = count)
    ans
}

## TODO no default => .default must raise error
to_vevent <- function(x, ...)
    UseMethod("to_vevent")



vevent <-
function(dtstart,
         summary,
         dtend = NULL,
         all.day = inherits(dtstart, "Date"),
         description = NULL,
         uid = NULL,
         categories = NULL,
         ...,
         vcalendar = TRUE,
         file,
         fold = TRUE) {

    DTSTART <- if (inherits(dtstart, "Date"))
                   format(dtstart, "%Y%m%d")
               else
                   .z(dtstart)


    UID <- if (is.null(uid))
               .msgID()
           else
               uid

    event <- c(paste0("UID:", paste0(UID, ".", Sys.info()["nodename"])),
               paste0("SUMMARY:", summary),
               paste0("DTSTAMP:", .z(Sys.time())),
               paste0("DTSTART:", DTSTART))

    if (!is.null(dtend)) {
        DTEND <- if (inherits(dtend, "Date")) {
                     if (all.day)
                         dtend <- dtend + 1
                     format(dtend, "%Y%m%d")
                 } else
                     .z(dtend)
        event <- c(event,
                   paste0("DTEND:", DTEND))
    } else if (all.day) {
        DTEND <- dtstart + 1
        DTEND <- format(DTEND, "%Y%m%d")
        event <- c(event,
                   paste0("DTEND:", DTEND))
    }

    event <- c("BEGIN:VEVENT",
               event,
               "END:VEVENT")

    if (vcalendar) {
        head <- c("BEGIN:VCALENDAR",
                  "VERSION:2.0",
                  "PRODID:-//http://enricoschumann.net/R/packages/icalutils//NONSGML icalutils %%version%%//EN")

        VERSION <- packageVersion("icalutils")
        head <- gsub("%%version%%", VERSION, head)

        foot <- "END:VCALENDAR"
        event <- c(head, event, foot)
    }

    ans <- as.list(event)
    class(ans) <- "vevent"

    if (!missing(file)) {
        ## TODO FOLD

        writeLines(paste(paste0(event, .eol.strict), collapse = ""), file)
        invisible(event)
    } else
        event

}
