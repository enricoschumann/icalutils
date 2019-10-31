## RRULE:FREQ=YEARLY;BYMONTH=5;BYDAY=1MO;UNTIL=19420504T000000Z

## .seq_rrule(DTSTART, DTEND, RRULE, UNTIL, COUNT,
##                        msg.violations = TRUE) {


## .parse_rrule <- function(rrule, ...) {


## DTSTART:19410505T010000
## RRULE:FREQ=YEARLY;BYMONTH=5;BYDAY=1MO;UNTIL=19420504T000000Z
DTSTART <- as.POSIXct("19410505T010000",
                      format = "%Y%m%dT%H%M%S",
                      tz = "UTC")
RRULE <- icalutils:::.parse_rrule("FREQ=YEARLY;BYMONTH=5;BYDAY=1MO;UNTIL=19420504T000000Z")[[1]]
icalutils:::.seq_rrule(DTSTART, NULL, RRULE)




## dates <- seq(as.Date(DTSTART, "%Y%m%d"),
##              as.Date(UNTIL, "%Y%m%d"), by = "1 day")
## dates <- dates[month(dates) == BYMONTH]
## dates <- dates[.weekday(dates) == 1L]
## dates <- .Date(tapply(dates, year(dates), `[`, 1))






## .seq_rrule(start[recurring][[r]],
##            end  [recurring][[r]],
##            RRULE[[r]],
##            UNTIL = recur.until,
##            COUNT = recur.count)



## DTSTART:19411006T020000
## RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=1MO;UNTIL=19421005T000000Z

## DTSTART:19810927T030000
## RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=9;UNTIL=19950924T010000Z
## DTSTART <- "19810927T030000"
## UNTIL <- "19950924T010000Z"
## BYMONTH <- 9
## dates <- seq(as.Date(DTSTART, "%Y%m%d"),
##              as.Date(UNTIL, "%Y%m%d"), by = "1 day")
## dates <- dates[month(dates) == BYMONTH]
## dates <- dates[.weekday(dates) == 0L]
## dates <- .Date(tapply(dates, year(dates), function(x) x[length(x)]))


## ## DTSTART:19170416T020000
## ## RRULE:FREQ=YEARLY;BYMONTH=4;BYMONTHDAY=15,16,17,18,19,20,21;BYDAY=1MO;UNTIL=19180415T010000Z
## DTSTART <- "19170416T020000"
## UNTIL <- "19180415T010000Z"
## BYMONTH <- 4
## BYMONTHDAY <- c(15,16,17,18,19,20,21)
## dates <- seq(as.Date(DTSTART, "%Y%m%d"),
##              as.Date(UNTIL, "%Y%m%d"), by = "1 day")
## dates <- dates[month(dates) == BYMONTH]
## dates <- dates[mday(dates) %in% c(15,16,17,18,19,20,21)]
## dates <- dates[.weekday(dates) == 1L]
## dates <- .Date(tapply(dates, year(dates), `[`, 1))




    ## "the last work day of the month"
    ## FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1

    ## "last day of a month"
    ## FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR,SA,SU;BYSETPOS=-1

    ## "SECONDLY" / "MINUTELY" / "HOURLY" / "DAILY"
    ## / "WEEKLY" / "MONTHLY" / "YEARLY"
    ## => each can be of length > 1

    ## BYDAY "SU" / "MO" / "TU" / "WE" / "TH" / "FR" / "SA"
    ## +1SU first sunday, -1SU last sunday ## MONTHLY and YEARLY
