RRULE <-
function(DTSTART,
         DTEND = NULL,
         FREQ,
         UNTIL = NULL,
         COUNT = NULL,
         INTERVAL = 1,
         BYSECOND = NULL,
         BYMINUTE = NULL,
         BYHOUR = NULL,
         BYDAY = NULL,
         BYMONTHDAY = NULL,
         BYYEARDAY = NULL,
         BYWEEKNO = NULL,
         BYMONTH = NULL,
         BYSETPOS = NULL,
         WKST = "MO",
         RDATE = NULL,
         EXDATE = NULL,
         RRULE = NULL,
         source = NULL) {

    ## RFC 5545, 3.3.10.
    ## https://tools.ietf.org/html/rfc5545#page-44
    ##
    ## If multiple BYxxx rule parts are specified, then
    ## after evaluating the specified FREQ and INTERVAL
    ## rule parts, the BYxxx rule parts are applied to the
    ## current set of evaluated occurrences in the
    ## following order: BYMONTH, BYWEEKNO, BYYEARDAY,
    ## BYMONTHDAY, BYDAY, BYHOUR, BYMINUTE, BYSECOND and
    ## BYSETPOS; then COUNT and UNTIL are evaluated.

    if (!is.null(source)) {
        source <- toupper(source)
        if (grepl("^RRULE:", source))
            source <- sub("^RRULE:", "", source)
        RRULE <- icalutils:::.parse_rrule(source)[[1]]
    }
    if (!is.null(RRULE)) {
        ## TODO if other arguments are specified, give them priority?
        FREQ <- RRULE$FREQ
        UNTIL <- RRULE$UNTIL
        COUNT <- RRULE$COUNT
        INTERVAL <- RRULE$INTERVAL
        BYSECOND <- RRULE$BYSECOND
        BYMINUTE <- RRULE$BYMINUTE
        BYHOUR <- RRULE$BYHOUR
        BYDAY <- RRULE$BYDAY
        BYMONTHDAY <- RRULE$BYMONTHDAY
        BYYEARDAY <- RRULE$BYYEARDAY
        BYWEEKNO <- RRULE$BYWEEKNO
        BYMONTH <- RRULE[["BYMONTH", exact = TRUE]]
        BYSETPOS <- RRULE$BYSETPOS
        WKST <- RRULE$WKST
    }

    if (is.null(INTERVAL))
        INTERVAL <- 1

    if (is.null(WKST))
        WKST <- "MO"

    datetime <- !inherits(DTSTART, "Date")

    if (is.null(UNTIL)) {
        if (datetime) {
            UNTIL <- end_of_year(Sys.Date(), 10)
            x <- paste(year(UNTIL),
                       month(UNTIL),
                       mday(UNTIL),
                       hour(UNTIL),
                       minute(UNTIL),
                       round(second(UNTIL)),
                       sep = "-")
            UNTIL <- as.POSIXct(strptime(x, "%Y-%m-%d-%H-%M-%S",
                                         tz = ""), 
                                tz = "")            
        } else
            UNTIL <- Sys.Date() +
                (as.POSIXct(end_of_year(Sys.Date(), 10)) + 86400)
    }


    ## FIXME if COUNT is specified, compute
    ## guess for UNTIL from COUNT


    if (FREQ == "SECONDLY") {

    } else if (FREQ == "MINUTELY") {

    } else if (FREQ == "HOURLY") {

    } else if (FREQ == "DAILY") {
        ## #############################

        if (is.null(BYSECOND) &&
            is.null(BYMINUTE) &&
            is.null(BYHOUR) &&
            is.null(BYDAY) &&
            is.null(BYMONTHDAY) &&
            is.null(BYYEARDAY) &&
            is.null(BYWEEKNO) &&
            is.null(BYMONTH) &&
            is.null(BYSETPOS)) {

            tmp <- as.POSIXlt(DTSTART)
            ## BYMONTH <- tmp$mon + 1
            ## BYMONTHDAY <- tmp$mday

        }

        rset <- seq(DTSTART,
                    UNTIL,
                    by = paste(INTERVAL, "day"))
        if (INTERVAL > 1L) {
            NULL
        }

        if (!is.null(BYMONTH)) {
            rset <- rset[.month(rset) %in% BYMONTH]
        }
        if (!is.null(BYWEEKNO)) {
        }
        if (!is.null(BYYEARDAY)) {
        }
        if (!is.null(BYMONTHDAY)) {
            rset <- rset[mday(rset) %in% BYMONTHDAY]
        }
        if (!is.null(BYDAY)) {
            num.wday <- .wday[BYDAY$wday]
            rset <- rset[.weekday(rset) %in% num.wday]
        }
        if (datetime) {
            if (!is.null(BYHOUR)) {
            }
            if (!is.null(BYMINUTE)) {
            }
            if (!is.null(BYSECOND)) {
            }
        }
        if (!is.null(BYSETPOS)) {


            rset <- unname(unlist(tapply(rset, format(rset, "%Y%m"),
                   function(x) {

                lenx <- length(x)
                if (any(BYSETPOS < 0))  ## FIXME check for in positions outside length
                    BYSETPOS[BYSETPOS < 0] <- lenx + BYSETPOS[BYSETPOS < 0] + 1
                x[BYSETPOS]
            })))
            class(rset) <- class(DTSTART)

        }
        rset <- rset[rset >= DTSTART & rset <= UNTIL]



    } else if (FREQ == "WEEKLY") {

        ## 3.3.10 The BYMONTHDAY rule part MUST NOT be
        ##        specified when the FREQ rule part is set
        ##        to WEEKLY.

        if (is.null(BYDAY)) {
            BYDAY <- list(wday = names(.wday)[.wday == .weekday(DTSTART)],
                          n = 0)
        }

        rset <- seq(DTSTART, UNTIL, by = "1 day")
        if (INTERVAL > 1L) {
            ## map every timestamp to the start of the week
            tmp <- unique(.start_of_week(rset, WKST = .wday[WKST]))
            tmp <- tmp [seq(1, length(tmp), by = INTERVAL)]
            rset <- rset[.start_of_week(rset, WKST = .wday[WKST]) %in% tmp]
        }

        if (!is.null(BYMONTH)) {
            rset <- rset[.month(rset) %in% BYMONTH]
        }
        if (!is.null(BYWEEKNO)) {
        }
        if (!is.null(BYYEARDAY)) {
        }
        if (!is.null(BYMONTHDAY)) {
            rset <- rset[mday(rset) %in% BYMONTHDAY]
        }
        if (!is.null(BYDAY)) {
            num.wday <- .wday[BYDAY$wday]
            rset <- rset[.weekday(rset) %in% num.wday]
        }
        if (datetime) {
            if (!is.null(BYHOUR)) {
            }
            if (!is.null(BYMINUTE)) {
            }
            if (!is.null(BYSECOND)) {
            }
        }
        if (!is.null(BYSETPOS)) {


            rset <- unname(unlist(tapply(rset, format(rset, "%Y%m"),
                   function(x) {

                lenx <- length(x)
                if (any(BYSETPOS < 0))  ## FIXME check for in positions outside length
                    BYSETPOS[BYSETPOS < 0] <- lenx + BYSETPOS[BYSETPOS < 0] + 1
                x[BYSETPOS]
            })))
            class(rset) <- class(DTSTART)

        }
        rset <- rset[rset >= DTSTART & rset <= UNTIL]




    } else if (FREQ == "MONTHLY") {
        ##     #################

        if (is.null(BYSECOND) &&
            is.null(BYMINUTE) &&
            is.null(BYHOUR) &&
            is.null(BYDAY) &&
            is.null(BYMONTHDAY) &&
            is.null(BYYEARDAY) &&
            is.null(BYWEEKNO) &&
            is.null(BYMONTH) &&
            is.null(BYSETPOS)) {

            tmp <- as.POSIXlt(DTSTART)
            BYMONTHDAY <- tmp$mday

        }

        rset <- seq(first_of_month(DTSTART),
                    end_of_month(UNTIL),
                    by = "1 day")
        if (INTERVAL > 1L) {
            tmp <- .month(DTSTART)
            tmp <- c(tmp:12, if (tmp > 1L) 1:(tmp - 1L)) [seq(1, 12, by = INTERVAL)]
            ## tmp$mon <- seq.int(tmp$mon, 12*(.year(UNTIL) - .year(DTSTART)) + tmp$mon,
            ##                    by = INTERVAL)
            ## (2:24  -1 )%% 12 +1
            rset <- rset[.month(rset) %in% tmp]
        }

        if (!is.null(BYMONTH)) {
            rset <- rset[.month(rset) %in% BYMONTH]
        }
        if (!is.null(BYWEEKNO)) {
        }
        if (!is.null(BYYEARDAY)) {
        }
        if (!is.null(BYMONTHDAY)) {
            rset <- rset[mday(rset) %in% BYMONTHDAY]
        }
        if (!is.null(BYDAY)) {
            num.wday <- .wday[BYDAY$wday]
            rset <- rset[.weekday(rset) %in% num.wday]
        }
        if (datetime) {
            if (!is.null(BYHOUR)) {
            }
            if (!is.null(BYMINUTE)) {
            }
            if (!is.null(BYSECOND)) {
            }
        }
        if (!is.null(BYSETPOS)) {


            rset <- unname(unlist(tapply(rset, format(rset, "%Y%m"),
                   function(x) {

                lenx <- length(x)
                if (any(BYSETPOS < 0))  ## FIXME check for in positions outside length
                    BYSETPOS[BYSETPOS < 0] <- lenx + BYSETPOS[BYSETPOS < 0] + 1
                x[BYSETPOS]
            })))
            class(rset) <- class(DTSTART)

        }
        rset <- rset[rset >= DTSTART & rset <= UNTIL]


    } else if (FREQ == "YEARLY") {
        ## #############################

        if (is.null(BYSECOND) &&
            is.null(BYMINUTE) &&
            is.null(BYHOUR) &&
            is.null(BYDAY) &&
            is.null(BYMONTHDAY) &&
            is.null(BYYEARDAY) &&
            is.null(BYWEEKNO) &&
            is.null(BYMONTH) &&
            is.null(BYSETPOS)) {

            tmp <- as.POSIXlt(DTSTART)
            BYMONTH <- tmp$mon + 1
            BYMONTHDAY <- tmp$mday

        } else if (is.null(BYDAY) &&
            is.null(BYMONTHDAY) &&
            is.null(BYYEARDAY) &&
            is.null(BYWEEKNO) &&
            is.null(BYSETPOS)) {

            tmp <- as.POSIXlt(DTSTART)
            BYMONTHDAY <- tmp$mday

        }
        if (datetime) {
            rset <- seq(as.Date(DTSTART),
                        as.Date(UNTIL),
                        by = "1 day")
            rset <- ISOdatetime(year(rset),
                                month(rset),
                                mday(rset),
                                hour(DTSTART),
                                minute(DTSTART),
                                second(DTSTART))

        } else {
            rset <- seq(DTSTART, UNTIL, by = "1 day")
        }
        
        if (INTERVAL > 1L) {
            rset <- rset[.year(rset) %in%
                         seq(.year(DTSTART), .year(UNTIL), by = INTERVAL)]
        }

        if (!is.null(BYMONTH)) {
            rset <- rset[.month(rset) %in% BYMONTH]
        }
        if (!is.null(BYWEEKNO)) {
        }
        if (!is.null(BYYEARDAY)) {
        }
        if (!is.null(BYMONTHDAY)) {
            rset <- rset[mday(rset) %in% BYMONTHDAY]
        }
        if (!is.null(BYDAY)) {
            num.wday <- .wday[BYDAY$wday]
            rset <- rset[.weekday(rset) %in% num.wday]
        }
        if (datetime) {
            if (!is.null(BYHOUR)) {
            }
            if (!is.null(BYMINUTE)) {
            }
            if (!is.null(BYSECOND)) {
            }
        }
        if (!is.null(BYSETPOS)) {


            rset <- unname(unlist(tapply(rset, format(rset, "%Y"),
                   function(x) {

                lenx <- length(x)
                if (any(BYSETPOS < 0))  ## FIXME check for in positions outside length
                    BYSETPOS[BYSETPOS < 0] <- lenx + BYSETPOS[BYSETPOS < 0] + 1
                x[BYSETPOS]
            })))
            class(rset) <- class(DTSTART)

        }
        rset <- rset[rset >= DTSTART & rset <= UNTIL]

    }

    if (!is.null(COUNT)) {
        if (length(rset) < COUNT) {
            warning("fewer recurrences than COUNT")
            COUNT <- length(rset)
        }
        rset <- rset[1:COUNT]
    }

    list(text = if (!is.null(source))
                    source else {
                               ""
                               ## rrule1 <- rrule[!unlist(lapply(rrule, is.null))]
                               ## paste0(paste0(names(rrule1), "=", toupper(rrule1)), collapse = ";")
                           },
         recurrence_set = data.frame(DTSTART = rset))

}


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
