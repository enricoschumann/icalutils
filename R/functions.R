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

.fold <- function(s) {}

.unfold <- function(s, strict.eol) {}

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

read_icalendar <- function(file, ...) {
    c(read_vevent(file),
      read_vtimezone(file))
}

read_vevent <- function(file, ...,
                        strict.eol = TRUE,
                        adjust.allday = TRUE,
                        return.class = "list",
                        recur.expand = FALSE,
                        recur.until = NULL,
                        recur.count = NULL,
                        use.OlsonNames = TRUE,
                        timestamps.POSIXct = TRUE,
                        timestamps.Date = FALSE,
                        timestamps.tz = "") {


    cal <- .read_one_line(file)
    cal <- gsub(if (strict.eol) .fold.re.strict else .fold.re, "", cal)
    cal <- strsplit(cal, "\r?\n")[[1]]

    ## RFC5545: VEVENTs cannot be nested
    begin <- grep("^BEGIN:VEVENT", cal)
    end <- grep("^END:VEVENT", cal)

    res <- list()
    if (use.OlsonNames)
        tz.names <- OlsonNames()
    else
        tz.names <- character(0)

    for (i in seq_along(begin)) {
        event <- cal[seq(begin[i] + 1, end[i] - 1)]
        res[[i]] <- .expand_properties(.properties(event),
                                       tz.names = tz.names)
    }


    if (return.class == "data.frame") {

        start <- lapply(res, `[[`, "DTSTART")
        end <- lapply(res, `[[`, "DTEND")
        all.day <- unlist(lapply(start, function(x) inherits(x, "Date")))

        recurring <- unlist(lapply(res, function(x) "RRULE" %in% names(x)))
        recurring.events <- list()
        if (recur.expand) {
            RRULE <- lapply(res[recurring], `[[`, "RRULE")
            for (r in seq_along(RRULE)) {
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


        uid <- lapply(res, `[[`, "UID")
        uid <- unlist(lapply(uid,
                                 function(x) if (is.null(x)) NA else x))

        summary <- lapply(res, `[[`, "SUMMARY")
        summary <- unlist(lapply(summary,
                                 function(x) if (is.null(x)) NA else x))

        description <- lapply(res, `[[`, "DESCRIPTION")
        description <- unlist(lapply(description,
                                     function(x) if (is.null(x)) NA else x))

        location <- lapply(res, `[[`, "LOCATION")
        location <- unlist(lapply(location,
                                  function(x) if (is.null(x)) NA else x))




        start[all.day] <- lapply(start[all.day],
                                 function(x) as.POSIXct(paste(x, "00:00:00"),
                                                        tz = timestamps.tz))
        start <- .POSIXct(unlist(start))

        end[all.day] <- lapply(end[all.day],
                               function(x) as.POSIXct(paste(x, "00:00:00"),
                                                      tz = timestamps.tz))
        end <- .POSIXct(unlist(end))

        res <- data.frame(uid,
                          summary,
                          description,
                          location,
                          start,
                          end,
                          all.day,
                          recurring,
                          stringsAsFactors = FALSE)
        if (recur.expand) {
            res <- cbind(res, uid.parent = NA_character_,
                         stringsAsFactors = FALSE)
            ri <- which(recurring)
            for (r in seq_along(ri)) {
                if (nrow(recurring.events[[r]]) == 1L)
                    next
                copy <- res[rep(ri[r], nrow(recurring.events[[r]])-1), ]
                copy$start <- recurring.events[[r]]$DTSTART[-1]
                copy$end <- recurring.events[[r]]$DTEND[-1]
                copy$uid.parent  <- res$uid[1]
                copy$uid <- NA
                res <- rbind(res, copy)
            }

        }
    }
    res
}

read_vtimezone <- function(file, ..., strict.eol = TRUE) {
    cal <- .read_one_line(file)
    cal <- gsub(if (strict.eol) .fold.re.strict else .fold.re, "", cal)
    cal <- strsplit(cal, "\r?\n")[[1]]

    ## rfc5545: VTIMEZONE cannot be nested
    begin <- grep("^BEGIN:VTIMEZONE", cal)
    end <- grep("^END:VTIMEZONE", cal)

    res <- list()
    for (i in seq_along(begin)) {
        event <- cal[seq(begin[i]+1, end[i]-1)]
        res[[i]] <- .expand_properties(.properties(event))
    }
    res
}

.expand_rrule <- function(DTSTART, DTEND,
                          RRULE, RDATE, EXDATE,
                          UNTIL = NULL, COUNT = NULL,
                          msg.violations = TRUE) {
    RRULE.text <- attr(RRULE, "RRULE")
    FREQ <- toupper(RRULE$FREQ)
    if (is.null(RRULE$INTERVAL))
        INTERVAL <- 1 else INTERVAL <- RRULE$INTERVAL

    DTSTART.isdate <- inherits(DTSTART, "Date")
    DTSTARTlt <- as.POSIXlt(DTSTART)



    if (!is.null(RRULE$UNTIL)) {
        UNTIL <- RRULE$UNTIL
        if (!class(DTSTART) %in% class(UNTIL) && msg.violations) {
            message("DTSTART and UNTIL are not of the same type [3.3.10.]")
            if (!inherits(DTSTART, "Date"))
                UNTIL <- as.Date(UNTIL)
        }
    } else if (!is.null(RRULE$COUNT)) {
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
        message("no UNTIL specified: use ", UNTIL)
    }
    if (DTSTART.isdate && inherits(UNTIL, "POSIXct"))
        UNTIL <- as.Date(as.POSIXlt(UNTIL))
    else if (!DTSTART.isdate && inherits(UNTIL, "Date"))
        UNTIL <- as.POSIXct(as.POSIXlt(UNTIL))
    ## if (inherits(DTSTART, "DATE"))
    ##     UNTIL <- as.Date("2025-1-1")
    ## else if (inherits(DTSTART, "POSIXct"))
    ##     UNTIL <- as.POSIXct("2025-1-1 10:00:00")

    ans <- FALSE
    if (FREQ == "YEARLY") {

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
                DTSTARTs <- seq(DTSTART, to = UNTIL, by = paste(INTERVAL, "year"))
                DTENDs <- DTSTARTs + unclass(DTEND - DTSTART)
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND = DTENDs)
            } else {
                NA
            }

        } else if ( is.null(RRULE$BYSECOND)   &&
                    is.null(RRULE$BYMINUTE)   &&
                    is.null(RRULE$BYHOUR)     &&
                    !is.null(RRULE$BYDAY)      &&
                    ## is.null(RRULE$BYMONTHDAY) &&
                    is.null(RRULE$BYYEARDAY)  &&
                    is.null(RRULE$BYWEEKNO)   &&
                    !is.null(RRULE$BYMONTH)    &&
                    is.null(RRULE$BYSETPOS)   &&
                    is.null(RRULE$WKST)) {

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


        ##then COUNT and UNTIL

        if        ( is.null(RRULE$BYSECOND)   &&
                    is.null(RRULE$BYMINUTE)   &&
                    is.null(RRULE$BYHOUR)     &&
                    is.null(RRULE$BYDAY)      &&
                    is.null(RRULE$BYMONTHDAY) &&
                    is.null(RRULE$BYYEARDAY)  &&
                    is.null(RRULE$BYWEEKNO)   &&
                    !is.null(RRULE$BYMONTH)    &&
                    is.null(RRULE$BYSETPOS)   &&
                    is.null(RRULE$WKST)) {



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
                DTENDs <- DTSTARTs + unclass(DTEND) - unclass(DTSTART)
                ans <- data.frame(DTSTART = DTSTARTs,
                                  DTEND = DTENDs)
            }
        }
    }

    ## TODO: remove EXDATEs
    ans

}


.weekday <- function(dates)
    unclass(dates + 4) %% 7

.next_weekday <- function(wday, start, count = 1, interval = 1)
    start + wday - unclass(start + 4) %% 7 +
        interval*7L*(seq_len(count) - 1L)


.utc_dt <- function(s, ...) {
    ## trailing Z does not have to be removed
    as.POSIXct(s, format = "%Y%m%dT%H%M%S", tz = "UTC")
}

.local_dt <- function(s) {
    ans <- as.POSIXct(s, format = "%Y%m%dT%H%M%S", tz = "UTC")
    attr(ans, "icalutils_localtime") <- TRUE
    ans
}

.parse_rrule <- function(RRULE, ...) {

    ## takes a vector of one or more rrules and
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

        if ("UNTIL" %in% nm)  ## FIXME: if DATE?
            x[["UNTIL"]] <- .utc_dt(x[["UNTIL"]])
        class(x) <- "icalutils_rrule"

        if ("BYMONTH" %in% nm) {
            tmp <- strsplit(x$BYMONTH, ",", fixed = TRUE)[[1]]
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



        x
    })
    for (i in seq_along(rules))
        attr(rules[[i]], "RRULE") <- RRULE[i]
    rules
}

.properties <- function(s, ...) {
    ## receives a character vector (lines of iCalendar),
    ## and returns named list (names = properties)

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
    ## character entries,  and returns a list
    ans <- p
    nm <- names(ans)
    dtfields <- c("CREATED",
                  "LAST-MODIFIED",
                  "DTSTAMP",
                  "DTSTART",
                  "DTEND",
                  "DUE")
    for (field in dtfields) {
        if (field %in% nm) {
            if (endsWith(ans[[field]], "Z")) {

                ans[[field]] <- .utc_dt(ans[[field]])

            } else if (length(
                       grep("^VALUE=DATE",
                            attr(ans[[field]], "parameters"),
                            ignore.case = TRUE))) {

                ans[[field]] <- as.Date(ans[[field]],
                                        format = "%Y%m%d")

            } else if (length(
                       grep("^TZID",
                            attr(ans[[field]], "parameters"),
                            ignore.case = TRUE))) {
                ## TZID may use double-quotes
                tz <- sub('TZID="?([^;"]+)"?', "\\1", attr(ans[[field]],"parameters"))
                if (!tz %in% tz.names)
                    if (tz.i <- match(tz, .tznames$Windows, nomatch = 0)) {
                        ## TODO collect such messages
                        ## message("map timezone  ",
                        ##         .tznames[["Windows"]][tz.i], " => ",
                        ##         .tznames[["Olson"]][tz.i])
                        tz <- .tznames[["Olson"]][tz.i]
                    } else {
                        warning("Timezone not found: ", tz, ". Using current tz instead.")
                        tz <- ""
                    }

                ans[[field]] <- as.POSIXct(ans[[field]],
                                           format = "%Y%m%dT%H%M%S",
                                           tz = tz)
            } else {

                ## local time (no timezone, not UTC)
                ans[[field]] <- .local_dt(ans[[field]])

            }
        }
    }
    if ("RRULE" %in% nm) {
        ans[["RRULE"]] <- .parse_rrule(ans[["RRULE"]])[[1]]

    }
    ans
}

ical_structure <- function(file, ..., strict.eol = TRUE) {
    cal <- .read_one_line(file)
    cal <- gsub(if (strict.eol) .fold.re.strict else .fold.re, "", cal)
    cal <- strsplit(cal, "\r?\n")[[1]]

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

allday_event <- function(date, summary, description = "", file) {

    UID <- paste0(.msgID(), ".", Sys.info()["nodename"])
    DTSTAMP <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y%m%dT%H%M%SZ")

    DTSTART <- format(date, "%Y%m%d")

    version <- packageVersion("icalutils")
    tmp <-
        "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//enricoschumann.net/R/packages/icalutils//NONSGML icalutils {version}//EN
BEGIN:VEVENT
UID:{UID}
DTSTAMP:{DTSTAMP}
DTSTART;VALUE=DATE:{DTSTART}
SUMMARY:{SUMMARY}
DESCRIPTION:{DESCRIPTION}
END:VEVENT
END:VCALENDAR
"

    tmp <- fill_in(tmp,
                   UID = UID,
                   DTSTAMP = DTSTAMP,
                   DTSTART = DTSTART,
                   SUMMARY = summary,
                   DESCRIPTION = description,
                   version = version)
    tmp <- strsplit(tmp, "\n", fixed = TRUE)[[1L]]
    tmp <- paste(tmp, collapse = .eol.strict)
    if (!missing(file)) {
        writeLines(tmp, file)
        invisible(tmp)
    } else
        tmp

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
year <- function(x, as.character = FALSE) {
    as.POSIXlt(x)$year + 1900
}

month <- function(x, as.character = FALSE) {
    as.POSIXlt(x)$mon + 1
}

mday <- function(x) {
    as.POSIXlt(x)$mday
}
