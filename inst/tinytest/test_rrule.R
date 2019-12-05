## Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = TRUE)
## library("icalutils")
## library("tinytest")

## .local_time <- function(s) {
##     ans <- as.POSIXct(s,
##                       format = "%Y%m%dT%H%M%S", tz = "UTC")
##     attr(ans, "icalutils_localtime") <- TRUE
##     ans
## }

## ------------------ time zones




## ------------------------------------------

## DTSTART;VALUE=DATE:20140307
## DTEND;VALUE=DATE:20140308
## RRULE:FREQ=YEARLY;INTERVAL=1
DTSTART <- as.Date("20140307", format = "%Y%m%d")
DTEND <- as.Date("20140308", format = "%Y%m%d")
RRULE <- .parse_rrule("FREQ=YEARLY;INTERVAL=1")
ans <- .expand_rrule(DTSTART=DTSTART, DTEND=DTEND, RRULE=RRULE[[1]],
                     UNTIL = as.Date("2022-1-1"))
expect_true(inherits(ans, "data.frame"))
expect_equal(colnames(ans), c("DTSTART", "DTEND"))
expect_equal(ans$DTSTART,
             structure(c(16136, 16501, 16867, 17232,
                         17597, 17962, 18328, 18693),
                       class = "Date"))
expect_equal(ans$DTEND,
             structure(c(16137, 16502, 16868, 17233,
                         17598, 17963, 18329, 18694),
                       class = "Date"))


## DTSTART;VALUE=DATE:20160514
## DTEND;VALUE=DATE:20160516
## RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA;UNTIL=20160707T215959Z
DTSTART <- as.Date("20160514", format = "%Y%m%d")
DTEND <- as.Date("20160516", format = "%Y%m%d")
RRULE <- .parse_rrule("FREQ=WEEKLY;INTERVAL=2;BYDAY=SA;UNTIL=20160707T215959Z")
ans <- .expand_rrule(DTSTART=DTSTART, DTEND=DTEND, RRULE=RRULE[[1]],
                     UNTIL = as.Date("2022-1-1"))
expect_true(inherits(ans, "data.frame"))
expect_equal(colnames(ans), c("DTSTART", "DTEND"))
expect_equal(ans$DTSTART,
             structure(c(16935, 16949, 16963, 16977), class = "Date"))
expect_equal(ans$DTEND,
             structure(c(16937, 16951, 16965, 16979), class = "Date"))




## DTSTART;VALUE=DATE:20160716
## DTEND;VALUE=DATE:20160718
## RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA
DTSTART <- as.Date("20160716", format = "%Y%m%d")
DTEND <- as.Date("20160718", format = "%Y%m%d")
RRULE <- .parse_rrule("FREQ=WEEKLY;INTERVAL=2;BYDAY=SA")
ans <- .expand_rrule(DTSTART=DTSTART, DTEND=DTEND, RRULE=RRULE[[1]],
                     UNTIL = as.Date("2017-1-1"))
expect_true(inherits(ans, "data.frame"))
expect_equal(colnames(ans), c("DTSTART", "DTEND"))
expect_equal(ans$DTSTART,
             structure(c(16998, 17012, 17026, 17040,
                         17054, 17068, 17082, 17096,
                         17110, 17124, 17138, 17152,
                         17166), class = "Date"))
expect_equal(ans$DTEND,
             structure(c(17000, 17014, 17028, 17042,
                         17056, 17070, 17084, 17098,
                         17112, 17126, 17140, 17154,
                         17168), class = "Date"))




if ("Europe/Berlin" %in% OlsonNames()) {
    ## DTSTART;TZID=Europe/Berlin:20170926T160000
    ## DTEND;TZID=Europe/Berlin:20170926T180000
    ## RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU

    DTSTART <- as.POSIXct("20170926T160000", format = "%Y%m%dT%H%M%S", tz = "Europe/Berlin")
    DTEND <- as.POSIXct("20170926T180000", format = "%Y%m%dT%H%M%S", tz = "Europe/Berlin")
    RRULE <- .parse_rrule("FREQ=WEEKLY;INTERVAL=1;BYDAY=TU")
    ans <- .expand_rrule(DTSTART=DTSTART, DTEND=DTEND, RRULE=RRULE[[1]],
                         UNTIL = as.POSIXct("2017-11-1"))
    expect_true(inherits(ans, "data.frame"))
    expect_equal(colnames(ans), c("DTSTART", "DTEND"))

    expect_equal(ans,
                 structure(
                     list(DTSTART = structure(c(1506434400, 1507039200,
                                                1507644000, 1508248800,
                                                1508853600, 1509462000),
                                              class = c("POSIXct", "POSIXt"), tzone = ""),
                          DTEND = structure(c(1506441600, 1507046400,
                                              1507651200, 1508256000,
                                              1508860800, 1509469200),
                                            tzone = "Europe/Berlin", class = c("POSIXct", "POSIXt"))),
                     class = "data.frame", row.names = c(NA, -6L)))


    ## ---
    ## DTSTART;TZID=Europe/Berlin:20181022T134500
    ## DTEND;TZID=Europe/Berlin:20181022T152000
    ## RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO;UNTIL=20190624T114500Z
    DTSTART <- as.POSIXct("20181022T134500", format = "%Y%m%dT%H%M%S", tz = "Europe/Berlin")
    DTEND <- as.POSIXct("20181022T152000", format = "%Y%m%dT%H%M%S", tz = "Europe/Berlin")
    RRULE <- .parse_rrule("FREQ=WEEKLY;INTERVAL=2;BYDAY=MO;UNTIL=20190624T114500Z")
    ans <- .expand_rrule(DTSTART=DTSTART, DTEND=DTEND, RRULE=RRULE[[1]])
    expect_true(inherits(ans, "data.frame"))
    expect_equal(colnames(ans), c("DTSTART", "DTEND"))

    expect_equal(ans,
                 structure(list(DTSTART = structure(c(1540208700, 1541421900,
                                                      1542631500, 1543841100, 1545050700, 1546260300, 1547469900, 1548679500,
                                                      1549889100, 1551098700, 1552308300, 1553517900, 1554723900, 1555933500,
                                                      1557143100, 1558352700, 1559562300, 1560771900), class = c("POSIXct",
                                                                                                                 "POSIXt"), tzone = ""), DTEND = structure(c(1540214400, 1541427600,
                                                                                                                                                             1542637200, 1543846800, 1545056400, 1546266000, 1547475600, 1548685200,
                                                                                                                                                             1549894800, 1551104400, 1552314000, 1553523600, 1554729600, 1555939200,
                                                                                                                                                             1557148800, 1558358400, 1559568000, 1560777600), tzone = "Europe/Berlin", class = c("POSIXct",
                                                                                                                                                                                                                                                 "POSIXt"))), class = "data.frame", row.names = c(NA, -18L)))

}




## ------------------------------------------
## RRULE examples in RFC5545
## 3.3.10. : the last work day of the month
## FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1

## 3.8.5.3.  Recurrence Rule
## ---
