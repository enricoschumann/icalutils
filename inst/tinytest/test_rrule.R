res <- rrule(dtstart = as.Date("2019-12-31"),
             dtend = as.Date("2019-12-31"),
             text = "FREQ=YEARLY;INTERVAL=1;UNTIL=20221231")

## text argument should be copied verbatim
expect_equal(res$text, "FREQ=YEARLY;INTERVAL=1;UNTIL=20221231")

## recurrence_set should be a vector of four rows
expect_equal(res$recurrence_set,
             structure(
                 list(DTSTART = structure(c(18261, 18627, 18992, 19357), class = "Date"),
                      DTEND   = structure(c(18261, 18627, 18992, 19357), class = "Date")),
                 class = "data.frame", row.names = c(NA, -4L)))



## ------------------------------------------------------

res <- rrule(dtstart = as.Date("2019-12-31"),
      dtend = as.Date("2019-12-31"),
      freq = "yearly", count = 4)

expect_equal(res$text, "FREQ=YEARLY;COUNT=4;INTERVAL=1")

## recurrence_set should be a vector of four rows
expect_equal(res$recurrence_set,
             structure(
                 list(DTSTART = structure(c(18261, 18627, 18992, 19357), class = "Date"),
                      DTEND   = structure(c(18261, 18627, 18992, 19357), class = "Date")),
                 class = "data.frame", row.names = c(NA, -4L)))



## ------------------------------------------------------

## DTSTART;VALUE=DATE:20140307
## DTEND;VALUE=DATE:20140308
## RRULE:FREQ=YEARLY;INTERVAL=1
DTSTART <- as.Date("20140307", format = "%Y%m%d")
DTEND <- as.Date("20140308", format = "%Y%m%d")

ans <- rrule(DTSTART, DTEND,
             text = "RRULE:FREQ=YEARLY;INTERVAL=1")$recurrence_set
expect_true(inherits(ans, "data.frame"))
expect_equal(colnames(ans), c("DTSTART", "DTEND"))
expect_equal(ans$DTSTART[1:5],
             structure(c(16136, 16501, 16867, 17232, 17597),
                       class = "Date"))
expect_equal(ans$DTEND[1:5],
             structure(c(16137, 16502, 16868, 17233, 17598),
                       class = "Date"))



## ------------------------------------------------------

## DTSTART;VALUE=DATE:20160514
## DTEND;VALUE=DATE:20160516
## RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA;UNTIL=20160707T215959Z
DTSTART <- as.Date("20160514", format = "%Y%m%d")
DTEND <- as.Date("20160516", format = "%Y%m%d")
ans <- rrule(DTSTART, DTEND,
             text = "RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA;UNTIL=20160707T215959Z")$recurrence_set

expect_true(inherits(ans, "data.frame"))
expect_equal(colnames(ans), c("DTSTART", "DTEND"))
expect_equal(ans$DTSTART,
             structure(c(16935, 16949, 16963, 16977), class = "Date"))
expect_equal(ans$DTEND,
             structure(c(16937, 16951, 16965, 16979), class = "Date"))



## ------------------------------------------------------

## DTSTART;VALUE=DATE:20160716
## DTEND;VALUE=DATE:20160718
## RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA
DTSTART <- as.Date("20160716", format = "%Y%m%d")
DTEND <- as.Date("20160718", format = "%Y%m%d")
ans <- rrule(DTSTART, DTEND,
             text = "RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA")$recurrence_set

expect_true(inherits(ans, "data.frame"))
expect_equal(colnames(ans), c("DTSTART", "DTEND"))
expect_equal(ans$DTSTART[1:13],
             structure(c(16998, 17012, 17026, 17040,
                         17054, 17068, 17082, 17096,
                         17110, 17124, 17138, 17152,
                         17166), class = "Date"))
expect_equal(ans$DTEND[1:13],
             structure(c(17000, 17014, 17028, 17042,
                         17056, 17070, 17084, 17098,
                         17112, 17126, 17140, 17154,
                         17168), class = "Date"))



## ------------------------------------------------------

if ("Europe/Berlin" %in% OlsonNames()) {
    ## DTSTART;TZID=Europe/Berlin:20170926T160000
    ## DTEND;TZID=Europe/Berlin:20170926T180000
    ## RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU

    DTSTART <- as.POSIXct("20170926T160000", format = "%Y%m%dT%H%M%S", tz = "Europe/Berlin")
    DTEND <- as.POSIXct("20170926T180000", format = "%Y%m%dT%H%M%S", tz = "Europe/Berlin")
    ans <- rrule(DTSTART, DTEND,
                 text = "RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=TU")$recurrence_set
    expect_true(inherits(ans, "data.frame"))
    expect_equal(colnames(ans), c("DTSTART", "DTEND"))

    expect_equal(ans[1:6, ],
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

    ans <- rrule(DTSTART, DTEND,
                 text = "RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO;UNTIL=20190624T114500Z")$recurrence_set

    expect_true(inherits(ans, "data.frame"))
    expect_equal(colnames(ans), c("DTSTART", "DTEND"))

    expect_equal(ans[1:18, ],
                 structure(
                     list(
                         DTSTART = structure(c(1540208700,
                                               1541421900,
                                               1542631500,
                                               1543841100,
                                               1545050700,
                                               1546260300,
                                               1547469900,
                                               1548679500,
                                               1549889100,
                                               1551098700,
                                               1552308300,
                                               1553517900,
                                               1554723900,
                                               1555933500,
                                               1557143100,
                                               1558352700,
                                               1559562300,
                                               1560771900),
                                             class = c("POSIXct", "POSIXt"), tzone = ""),
                         DTEND = structure(c(1540214400,
                                             1541427600,
                                             1542637200,
                                             1543846800,
                                             1545056400,
                                             1546266000,
                                             1547475600,
                                             1548685200,
                                             1549894800,
                                             1551104400,
                                             1552314000,
                                             1553523600,
                                             1554729600,
                                             1555939200,
                                             1557148800,
                                             1558358400,
                                             1559568000,
                                             1560777600),
                                           tzone = "Europe/Berlin", class = c("POSIXct", "POSIXt"))),
                     class = "data.frame", row.names = c(NA, -18L)))

}


## ------------------------------------------------------
## RRULE examples in RFC5545
## 3.3.10. : the last work day of the month
## FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1

## TODO
res <- rrule(dtstart = as.Date("2019-12-31"),
             dtend = as.Date("2019-12-31"),
             text = "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1")


## ------------------------------------------------------

## dtend is OPTIONAL
expect_equal(ncol(rrule(dtstart = as.Date("2019-12-31"),
                   text = "FREQ=YEARLY")$recurrence_set),
             1)
expect_equal(colnames(rrule(dtstart = as.Date("2019-12-31"),
                   text = "FREQ=YEARLY")$recurrence_set),
             "DTSTART")

expect_equal(rrule(dtstart = as.Date("2019-12-31"),
                   text = "FREQ=YEARLY")$recurrence_set,
             rrule(dtstart = as.Date("2019-12-31"),
                   freq = "yearly")$recurrence_set)


