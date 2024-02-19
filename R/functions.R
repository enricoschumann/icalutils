.eol.strict <-     "\r\n"
.fold.re <-        "\n[ \t]"
.fold.re.strict <- "\r\n[ \t]"

.unquote <- function(s) {
    i <- startsWith(s, "\"")
    s[i] <- substr(s[i], 2L, nchar(s[i]))
    i <- endsWith(s, "\"")
    s[i]   <- substr(s[i], 1L, nchar(s[i]) - 1L)
    s
}

.fold <- function(s, max.bytes = 75) {
    ## RFC ==> max 75 octets
    .NotYetImplemented()
    i <- nchar(s, type = "bytes") > max.bytes
}

.unfold <- function(s, strict.eol) {
    re <- if (is.character(strict.eol))
              strict.eol
          else if (strict.eol)
              .fold.re.strict
          else
              .fold.re
    s <- gsub(re, "", s)
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


    ## TODO: two or more files might be specified
    cal.txt <- .read_one_line(file)
    cal.txt <- .unfold(cal.txt, strict.eol)


    ## TODO text operations -- remove \, etc
    cal <- .properties(cal.txt, keep.source = keep.source)
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
            ## TODO check for repeated names?
        }
    }
    if (uid.names)
        names(ans) <-
            sapply(ans, function(x)
                if (is.null(x[["UID"]])) NA else x[["UID"]])
    class(ans) <- c("icalendar")
    ans
}

print.vevent <- function(x, ...) {
    time <- as.character(x$DTSTART)
    all.day <- inherits(x$DTSTART, "Date")
    if (!is.null(x$DTEND)) {
        if (all.day && x$DTEND > x$DTSTART + 1)
            time <- paste0(time, " -- ", x$DTEND - 1L)
        if (!all.day && as.Date(x$DTSTART) == as.Date(x$DTEND))
            time <- paste0(time, " -- ", format(x$DTEND, "%H:%M:%S"))
    }
    if (!is.null(x$RRULE)) {
        time <- paste(time, "(recurring)")
    }
    cat(time, "  ", x$SUMMARY, "\n", sep = "")
    invisible(x)
}

print.icalendar <-
function(x, ..., max = NULL) {

    if (is.null(max))
        max <- getOption("max.print", 99999L)

    len <- length(x)
    cat("An icalendar file with ", len,
        if (len == 1L) " component" else " components",
        ".\n", sep = "")
    if (len == 0L)
        cat("  [Not a valid iCalendar: at least one component is required.]\n")
    invisible(x)
}

`[.icalendar` <-
function(x, i, ...) {
    ans <- unclass(x)[i]
    class(ans) <- "icalendar"
    ans
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
            message(start[recurring][[r]])
            created <- try(.expand_rrule(start[recurring][[r]],
                                         end  [recurring][[r]],
                                         RRULE = RRULE[[r]],
                                         UNTIL = recur.until,
                                         COUNT = recur.count), silent = TRUE)
            if (inherits(created, "try-error") ||
                isFALSE(created)) {
                message("cannot parse RRULE",
                        attr(RRULE[[r]], "RRULE"))
                created <- FALSE
            }
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


.parse_rrule <- function(RRULE, ...) {

    ## takes a vector of one or more rrules (character)
    ## and returns a list of lists. Note that the
    ## rrules must *not* start with "RRULE:" anymore.

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
        if (is.null(UNTIL) || as.POSIXct(UNTIL) > as.POSIXct(RRULE$UNTIL))
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

        ## RFC 5545, 3.3.10: The WKST rule part specifies the day
        ##     on which the workweek starts.  Valid values are
        ##     MO, TU, WE, TH, FR, SA, and SU.  This is
        ##     significant when a WEEKLY "RRULE" has an interval
        ##     greater than 1, and a BYDAY rule part is
        ##     specified.  This is also significant when in a
        ##     YEARLY "RRULE" when a BYWEEKNO rule part is
        ##     specified.  The default value is MO.

        if (!is.null(BYWEEKNO) && !is.null(WKST) && WKST != "MO")
            warning(RRULE.text, ": ",
                    sQuote("WKST"), " is currently ignored")

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

            if (is.null(BYMONTHDAY))
                BYMONTHDAY <- as.POSIXlt(DTSTART)$mday
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

        ## RFC 5545, 3.3.10: The WKST rule part specifies the day
        ##     on which the workweek starts.  Valid values are
        ##     MO, TU, WE, TH, FR, SA, and SU.  This is
        ##     significant when a WEEKLY "RRULE" has an interval
        ##     greater than 1, and a BYDAY rule part is
        ##     specified.  This is also significant when in a
        ##     YEARLY "RRULE" when a BYWEEKNO rule part is
        ##     specified.  The default value is MO.

        if (INTERVAL > 1 && !is.null(BYDAY) && !is.null(WKST) && WKST != "MO")
            warning(RRULE.text, ": ",
                    sQuote("WKST"), " is currently ignored")

        if        ( is.null(BYSECOND)   &&
                    is.null(BYMINUTE)   &&
                    is.null(BYHOUR)     &&
                   !is.null(BYDAY)      &&
                    is.null(BYMONTHDAY) &&
                    is.null(BYYEARDAY)  &&
                    is.null(BYWEEKNO)   &&
                    is.null(BYMONTH)    &&
                    is.null(BYSETPOS) # &&
                   #is.null(WKST)
                   ) {


            if (DTSTART.isdate) {
                if (is.null(COUNT))
                    COUNT <- ceiling(as.numeric(as.Date(UNTIL) - DTSTART)/(7*INTERVAL))

                ans <- .next_weekday(.wday[RRULE$BYDAY$wday],
                                     start = DTSTART,
                                     count = COUNT,
                                     interval = INTERVAL)
                if (!is.null(UNTIL))
                    DTSTARTs <- ans[ans <= as.Date(UNTIL)]
                else
                    DTSTARTs <- ans
                if (is.null(DTEND))
                    ans <- data.frame(DTSTART = DTSTARTs)
                else
                    ans <- data.frame(DTSTART = DTSTARTs,
                                      DTEND = DTSTARTs + unclass(DTEND - DTSTART))

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

## .properties <- function(s, keep.source = TRUE, ...) {

##     ## receives a character vector
##     ## (*unfolded* content lines of iCalendar stream),
##     ## and returns named list (names = properties).
##     ## Attached (as attributes)
##     ## to these properties may be "parameters" as
##     ## specified by [RFC5545:3.2.]. The property values
##     ## are all character.

##     p <- "^([^:;]+?)[:;](.*)"
##     ans <- gsub(p, "\\2", s, perl = TRUE)
##     names(ans) <- gsub(p, "\\1", s, perl = TRUE)
##     ans <- as.list(ans)

##     ## check for property parameters [RFC5545:3.2.]
##     param <- grep("[^:]+;.*:", s)
##     for (i in seq_along(param)) {
##         attr(ans[[ param[i] ]], "parameters") <-
##             sub("(.*?):(.*)", "\\1", ans[[ param[i] ]])
##         ans[[ param[i] ]] <-
##             sub("(.*?):(.*)", "\\2", ans[[ param[i] ]])
##     }
##     ans
## }

.properties <- function(s, keep.source = TRUE, ...) {

    ## receives a character vector (*unfolded* content
    ## lines of iCalendar stream), and returns named
    ## list (names = "properties").  Attached (as
    ## attributes) to these properties may be
    ## "parameters" as specified by [RFC5545:3.2.]. The
    ## property and parameter values are all character.


    ## check for property parameters [RFC5545:3.2.]
    no.param <- grepl("^[a-zA-Z0-9-][a-zA-Z0-9-]*?:", s, perl = TRUE)
    has.param <- !no.param

    ans <- vector("list", length(s))

    p <- "^([^:;]+?)[:;].*"
    names(ans) <- gsub(p, "\\1", s, perl = TRUE)

    ## property values -- if there are NO PARAMETERS
    p <- "^[^:][^:]*?:(.*)"
    ans[no.param] <- gsub(p, "\\1", s[no.param], perl = TRUE)


    ## property values -- if THERE ARE PARAMETERS
    ## ==> look for pattern ;<param-name>=<param-value>
    has.param <- which(has.param)
    m <- gregexpr(";[^=]+=\"[^\"]+?\"(?=[;:])|;[^=]+=[^\"]+?(?=[;:])",
                  s[has.param], perl = TRUE)
    rm <- regmatches(s[has.param], m)

    for (i in seq_along(has.param)) {
        ans[[ has.param[i] ]] <-
            substr(s[ has.param[i] ],
                   1L +
                   m[[ i ]][1L] + sum(attr(m[[ i ]], "match.length")),
                   nchar( s[has.param[i] ]))
        params <- strsplit(rm[[i]], "=", fixed = TRUE)
        for (j in seq_along(params)) {
            attr(ans[[ has.param[i] ]],
                 substr(params[[j]][1L], 2L, nchar(params[[j]][1L]))) <-
                paste(.unquote(params[[j]][-1L]), collapse = "=")
        }
    }

    if (keep.source)
        for (i in seq_along(s))
            attr(ans[[i]], "source") <- s[i]
    ans
}

.expand_properties <- function(p, tz.names = character(0),
                               localtime.chron = FALSE,
                               copy.attributes = TRUE) {

    ## receives a named list (names = properties) with
    ## character entries, and returns a list. The values
    ## should be evaluated to proper R objects
    ## (e.g. datetime becomes POSIXct, etc).

    ans <- p
    txt <- unlist(p)  ## all character, so unlist is safe

    ## param <- unlist(lapply(ans, function(x)
    ##     if (!is.null(A <- attr(x, "parameters")))
    ##         A else ""))

    nm <- names(ans)
    done <- logical(length(ans))
    processed <- logical(length(ans))


    ## ----------- *SINGLE* DATETIME FIELDS -----------
    dtfields <- c("CREATED",
                  "LAST-MODIFIED",
                  "DTSTAMP",
                  "DTSTART",
                  "DTEND",
                  "DUE",
                  "EXDATE",
                  "RDATE")

    prop <- nm %in% dtfields & !processed & !grepl(",", txt, fixed = TRUE)

    if (any(prop)) {

        i <- prop & !processed & endsWith(txt, "Z")
        if (any(i)) {
            ans[i] <- as.list(.utc_dt(txt[i]))
            processed[i] <- TRUE
        }

        i <- prop & !processed & sapply(p, attr, "VALUE") == "DATE"
        if (any(i)) {
            ans[i] <- as.list(as.Date(txt[i], format = "%Y%m%d"))
            processed[i] <- TRUE
        }

        i <- prop & !processed & as.character(sapply(p, attr, "TZID")) != "NULL"
        if (any(i)) {

            tz <- sapply(p[i], attr, "TZID")
            uniq.tz <- unique(tz)

            for (j in seq_along(uniq.tz)) {

                if (!uniq.tz[j] %in% tz.names)
                    if (tz1 <- match(uniq.tz[j], .tznames$Windows, nomatch = 0)) {
                        message("map timezone  ",
                                .tznames[["Windows"]][tz1], " => ",
                                .tznames[["Olson"]][tz1])
                        tz1 <- .tznames[["Olson"]][tz1]
                    } else {
                        warning("Timezone not found: ", uniq.tz[j],
                                ". Using current tz instead.")
                        tz1 <- ""
                    }
                else
                    tz1 <- uniq.tz[j]

                ij <- which(i)[tz == uniq.tz[j]]
                ans[ij] <- as.list(as.POSIXct(txt[ij], tz = tz1,
                                              format = "%Y%m%dT%H%M%S"))
            }
            processed[i] <- TRUE
        }

        i <- prop & !processed & grepl("^[0-9]{8}T[0-9]{6}$", txt)
        if (any(i)) {
            if (localtime.chron && requireNamespace("chron")) {
                ans[i] <- as.list(chron(substr(txt[i], 1, 8),
                                        substr(txt[i], 10, 15),
                                        format = c(dates = "ymd", time = "hms")))
            } else {
                ans[i] <- as.list(as.POSIXct(txt[i], tz = "",
                                             format = "%Y%m%dT%H%M%S"))
            }
            processed[i] <- TRUE

        }
    }



    ## ----------- *MULTI-VALUE* DATETIME FIELDS -----------
    dtfields <- c("EXDATE",
                  "RDATE")

    prop <- nm %in% dtfields & !processed &  grepl(",", txt, fixed = TRUE)

    if (any(prop)) {
        message("multi-value datetime fields not yet implemented")
    }



    ## ----------- RRULE -----------
    i <- !processed & nm %in% "RRULE"
    for (j in which(i)) {
        ans[[j]] <- .parse_rrule(ans[[j]])[[1]]
    }
    processed[i] <- TRUE

    ## copy attributes
    if (copy.attributes) {
        for (i in which(processed)) {
            if (!is.null(attributes(p[[i]]))) {
                ## must be icalendar attributes,
                ## because "p" is a list of strings
                if (any(names(attributes(p[[i]])) %in% names(attributes(ans[[i]]))))
                    warning("attribute clash in property ", i)
                attributes(ans[[i]]) <- c(attributes(ans[[i]]),
                                          attributes(  p[[i]]))  ## tz etc. now in R data
            }
        }
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
.msgID <- function(n = 1) {
    ans <- character(n)
    for (i in seq_along(ans)) {
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
        m <- c(floor(runif(1)*1e16),
               floor(10000*as.numeric(Sys.time())))
        ans[i] <- paste(c(ab[digits(m[1L], 36) + 1], ".",
                          ab[digits(m[2L], 36) + 1]),
                        sep = "", collapse="")
    }
    ans
}



to_vevent <- function(x, ...)
    UseMethod("to_vevent")

to_vevent.default <- function(x, ...) {
    cl <- .class2(x)
    stop("no 'to_vevent' method for an object of class",
         if (length(cl) > 1) "(es)",
         " ",
         paste(sQuote(cl), collapse = ", "))
}

vevent <-
function(dtstart,
         summary,
         dtend = NULL,
         all.day = inherits(dtstart, "Date"),
         description = NULL,
         uid = NULL,
         categories = NULL,
         location = NULL,
         ...,
         vcalendar = TRUE,
         file,
         fold = TRUE) {

    len <- max(length(dtstart),
               length(dtend),
               length(summary),
               length(description))

    dtstart <- if ((n <- len/length(dtstart)) == 1) dtstart else rep(dtstart, n)
    dtend <- if ((n <- len/length(dtend)) == 1) dtend else rep(dtend, n)
    summary <- if ((n <- len/length(summary)) == 1) summary else rep(summary, n)
    description <- if ((n <- len/length(description)) == 1) description else rep(description, n)

    DTSTART <- if (inherits(dtstart, "Date"))
                   format(dtstart, "%Y%m%d")
               else
                   .z(dtstart)

    if (is.null(uid)) {
        UID <- paste0(.msgID(len), ".", Sys.info()["nodename"])
    } else if (isTRUE(uid)) {
        if (requireNamespace("uuid")) {
            UID <- uuid::UUIDgenerate(n = len)
        } else {
            warning("package uuid not available")
            UID <- paste0(.msgID(len), ".", Sys.info()["nodename"])
        }
    } else
            UID <- uid

    events <- vector("list", len)
    for (i in seq_len(len)) {
        event <- c(paste0("UID:", UID[i]),
                   paste0("SUMMARY:", summary[i]),
                   paste0("DTSTAMP:", .z(Sys.time())),
                   paste0("DTSTART:", DTSTART[i]))

        if (!is.null(dtend)) {
            DTEND <- if (inherits(dtend, "Date")) {
                         if (all.day)
                             dtend[i] <- dtend[i] + 1
                         format(dtend[i], "%Y%m%d")
                     } else
                         .z(dtend[i])
            event <- c(event,
                       paste0("DTEND:", DTEND))
        } else if (all.day) {
            DTEND <- dtstart[i] + 1
            DTEND <- format(DTEND, "%Y%m%d")
            event <- c(event,
                       paste0("DTEND:", DTEND))
        }
        if (!is.null(categories))
            event <- c(event,
                       paste0("CATEGORIES:",
                              paste0(categories, collapse = ",")))
        if (!is.null(location))
            event <- c(event,
                       paste0("LOCATION:",
                              paste0(location, collapse = ",")))
        event <- c("BEGIN:VEVENT",
                   event,
                   "END:VEVENT")
        class(event) <- "vevent"
        events[[i]] <- event
    }

    if (vcalendar) {
        head <- c("BEGIN:VCALENDAR",
                  "VERSION:2.0",
                  "PRODID:-//http://enricoschumann.net/R/packages/icalutils//NONSGML icalutils %%version%%//EN")

        VERSION <- packageVersion("icalutils")
        head <- gsub("%%version%%", VERSION, head)

        foot <- "END:VCALENDAR"
        events <- c(head, events, foot)

    }

    ans <- as.list(events)
    class(ans) <- "vevent"
    if (!missing(file)) {
        ## TODO FOLD

        writeLines(paste(paste0(unlist(events), .eol.strict), collapse = ""), file)
        invisible(events)
    } else
        events

}

save_attachments <- function(file, out.dir,
                             strict.eol = TRUE) {

    cal.txt <- .read_one_line(file)
    cal.txt <- .unfold(cal.txt, strict.eol)
    cal <- .properties(cal.txt)

    if (length(i <- grep("^ATTACH$", names(cal), ignore.case = TRUE))) {
        attachments <- cal[i]
        filenames <- rep("", length(attachments))

        spec.fn <- unlist(lapply(attachments, attr, "parameters"))
        i <- grep("x-filename", spec.fn, ignore.case = TRUE)
        spec.fn <- gsub(".*X-FILENAME=([^;]*)", "\\1", spec.fn)
        filenames[i] <- spec.fn
        filenames <- make.unique(filenames)

        base64 <- TRUE
        if (!requireNamespace("base64enc")) {
            warning("save attachments, but cannot decode them")
            base64 <- FALSE
        }
        for (i in seq_along(attachments)) {
            fn <- tempfile()
            writeLines(attachments[[i]], fn)
            if (base64)
                base64enc::base64decode(file = fn, output = file.path(out.dir, filenames[i]))
        }
        filenames
    } else
        invisible(NULL)
}

.parse_datetime <- function(dt, local.chron = FALSE, ...) {
    done <- logical(length(dt))
    ans <- vector("list", length(dt))

    ## UTC
    i <- !done & endsWith(dt, "Z")
    if (any(i)) {
        tmp <- as.POSIXct(dt[i], format = "%Y%m%dT%H%M%S", tz = "UTC")
        ans[i] <- as.list(tmp)
        done[i] <- TRUE
    }

    ## local time
    i <- !done & endsWith(dt, "Z")


}

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
    attr(ans, "localtime") <- TRUE
    ans
}

## format time into character representation of UTC time
## (basic format 8601: YmdHMSZ)
.z <- function(t)
    strftime(t, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
