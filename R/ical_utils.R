.eol.strict     <- "\r\n"
.fold.re        <- "\n[ \t]"
.fold.re.strict <- "\r\n[ \t]"

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
                        timestamps.POSIXct = TRUE,
                        timestamps.Date = FALSE,
                        timestamps.tz = "") {


    cal <- .read_one_line(file)
    cal <- gsub(if (strict.eol) .fold.re.strict else .fold.re, "", cal)
    cal <- strsplit(cal, "\r?\n")[[1]]


    ## rfc5545: VEVENTs cannot be nested
    begin <- grep("^BEGIN:VEVENT", cal)
    end <- grep("^END:VEVENT", cal)

    res <- list()
    for (i in seq_along(begin)) {
        event <- cal[seq(begin[i] + 1, end[i] - 1)]
        res[[i]] <- .expand_properties(.properties(event))
    }

    if (return.class == "data.frame") {
        uid <- unlist(lapply(res, `[[`, "UID"))

        start <- lapply(res, `[[`, "DTSTART")
        all.day <- unlist(lapply(start, function(x) inherits(x, "Date")))
        start[all.day] <- lapply(start[all.day],
                                 function(x) as.POSIXct(paste(x, "00:00:00"),
                                                        tz = timestamps.tz))
        start <- .POSIXct(unlist(start))

        end <- lapply(res, `[[`, "DTEND")
        all.day <- unlist(lapply(end, function(x) inherits(x, "Date")))
        end[all.day] <- lapply(end[all.day],
                               function(x) as.POSIXct(paste(x, "00:00:00"),
                                                      tz = timestamps.tz))
        end <- .POSIXct(unlist(end))

        summary <- lapply(res, `[[`, "SUMMARY")
        summary <- unlist(lapply(summary,
                              function(x) if (is.null(x)) "" else x))

        description <- lapply(res, `[[`, "DESCRIPTION")
        description <- unlist(lapply(description,
                              function(x) if (is.null(x)) "" else x))

        recurring <- unlist(lapply(res, function(x) "RRULE" %in% names(x)))
        res <- data.frame(uid,
                          summary,
                          description,
                          start,
                          end,
                          all.day,
                          recurring,
                          stringsAsFactors = FALSE)
    }
    res
}

read_vtimezone <- function(file, ...,
                        strict.eol = TRUE) {
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

seq_rrule <- function(start, event, until = NULL, count = NULL) {

}

.seq_rrule <- function(DTSTART, DTEND, RRULE) {
    ## FREQ, ## MUST
    ## UNTIL,
    ## COUNT,
    ## INTERVAL = 1,
    ## BYSECOND,
    ## BYMINUTE,
    ## BYHOUR,
    ## BYDAY,
    ## BYMONTHDAY,  ## 1 -- 31    -1 -- -31
    ## BYYEARDAY,   ## 1 -- 366   -1 -- 366
    ## BYWEEKNO,    ## 1 -- 53
    ## BYMONTH,  ## 1 - 12
    ## BYSETPOS,
    ## WKST   ## weekstart: MO, TU, WE, TH, FR, SA, and SU

    FREQ <- toupper(RRULE$FREQ)

    if (is.null(RRULE$INTERVAL))
        INTERVAL <- 1
    else
        INTERVAL <- RRULE$INTERVAL

    if (is.null (RRULE$UNTIL) &&
        is.null (RRULE$COUNT) &&
        inherits(DTSTART, "Date")) {
        UNTIL <- Sys.Date() + 365*10
        message("no UNTIL specified: use ", UNTIL)
    }

    ## "the last work day of the month"
    ## FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1

    ## "last day of a month"
    ## FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR,SA,SU;BYSETPOS=-1


    ## "SECONDLY" / "MINUTELY" / "HOURLY" / "DAILY"
    ## / "WEEKLY" / "MONTHLY" / "YEARLY"
    ## => each can be of length > 1


    ## BYDAY "SU" / "MO" / "TU" / "WE" / "TH" / "FR" / "SA"
    ## +1SU first sunday, -1SU last sunday ## MONTHLY and YEARLY

    .wday <- c(
        "SU" = 0,
        "MO" = 1,
        "TU" = 2,
        "WE" = 3,
        "TH" = 4,
        "FR" = 5,
        "SA" = 6)

    start <- DTSTART
    end <- DTEND
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

            if (inherits(DTSTART, "DATE"))
                ans <- seq(DTSTART, to = UNTIL, by = paste(INTERVAL, "year"))
            else if (inherits(DTSTART, "POSIXct"))
                message("nothing done")


        } else if ( is.null(RRULE$BYSECOND)   &&
                    is.null(RRULE$BYMINUTE)   &&
                    is.null(RRULE$BYHOUR)     &&
                   !is.null(RRULE$BYDAY)      &&
                    is.null(RRULE$BYMONTHDAY) &&
                    is.null(RRULE$BYYEARDAY)  &&
                    is.null(RRULE$BYWEEKNO)   &&
                   !is.null(RRULE$BYMONTH)    &&
                    is.null(RRULE$BYSETPOS)   &&
                    is.null(RRULE$WKST)) {

            message("nothing done")
        }
    } else if (FREQ == "MONTHLY") {
        if        ( is.null(BYSECOND)   &&
                    is.null(BYMINUTE)   &&
                    is.null(BYHOUR)     &&
                    is.null(BYDAY)      &&
                    is.null(BYMONTHDAY) &&
                    is.null(BYYEARDAY)  &&
                    is.null(BYWEEKNO)   &&
                   !is.null(BYMONTH)    &&
                    is.null(BYSETPOS)   &&
                    is.null(WKST)) {

            message("nothing done")
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
            if (inherits(DTSTART, "Date"))
                ans <- .next_weekday(.wday[RRULE$BYDAY],
                                     start = DTSTART, count = RRULE$COUNT)
            else {
                lt <- as.POSIXlt(DTSTART)
                dates <- .next_weekday(.wday[RRULE$BYDAY],
                                       start = as.Date(DTSTART),
                                       count = RRULE$COUNT)
                ans <- as.POSIXct(paste(dates, lt$hour, lt$min, lt$sec),
                                  format = "%Y-%m-%d %H %M %S",
                                  tzone = attributes(DTSTART, "tzone"))
            }

        }
    }

    ans
}


.next_weekday <- function(wday, start, count = 1, frequency = 1)
    start + wday - unclass(start + 4) %% 7 +
        frequency*7L*(seq_len(count) - 1L)


.utc_dt <- function(x, ...) {
    ## trailing Z does not have to be removed
    as.POSIXct(strptime(x,
                        format = "%Y%m%dT%H%M%S",
                        tz = "UTC"))
}

.parse_rrule <- function(rrule, ...) {

    ## takes a vector of one or more rrules and
    ## returns a list
    rules <- strsplit(rrule, ";", fixed = TRUE)
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

        if ("UNTIL" %in% nm)
            x[["UNTIL"]] <- .utc_dt(x[["UNTIL"]])
        class(x) <- "ical_utils_rrule"
        x
    })

    rules
}

.properties <- function(s, ...) {
    ## receives a character vector (lines of iCalendar),
    ## and returns named character vector (names = properties)
    p <- "^([^:;]+?)[:;](.*)"
    ans <- gsub(p, "\\2", s, perl = TRUE)
    names(ans) <- gsub(p, "\\1", s, perl = TRUE)
    ans
}

.expand_properties <- function(p) {
    ## receives a named character vector (names = properties),
    ## and returns a list
    ans <- as.list(p)
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
                ans[[field]] <- as.POSIXct(
                    strptime(ans[[field]],
                             format = "%Y%m%dT%H%M%S",
                             tz = "UTC"))
            } else if (grepl("^VALUE=DATE",
                             ans[[field]],
                             ignore.case = TRUE)) {
                n <- nchar(ans[[field]])
                ans[[field]] <- as.Date(substr(ans[[field]], n - 7, n),
                                        format = "%Y%m%d")
            } else if (grepl("^TZID",
                             ans[[field]],
                             ignore.case = TRUE)) {
                n <- nchar(ans[[field]])
                tz <- substr(ans[[field]], 6, n - 16)
                ans[[field]] <- as.POSIXct(
                    strptime(substr(ans[[field]], n - 14, n),
                             format = "%Y%m%dT%H%M%S",
                             tz = tz))
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

    version <- packageVersion("ical.utils")
tmp <-
"BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//enricoschumann.net/R/packages/ical.utils//NONSGML ical.utils {version}//EN
BEGIN:VEVENT
UID:{UID}
DTSTAMP:{DTSTAMP}
DTSTART;VALUE=DATE:{DTSTART}
SUMMARY:{SUMMARY}
DESCRIPTION:{DESCRIPTION}
END:VEVENT
END:VCALENDAR"

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
