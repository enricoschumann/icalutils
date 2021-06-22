## DAILY
txt <- "FREQ=DAILY;COUNT=10"
DTSTART <- as.Date("2020-10-26")
ans <- RRULE(DTSTART, source = txt)
expect_equal(ans$recurrence_set$DTSTART,
             seq(DTSTART, by = "1 day", length.out = 10))

txt <- "FREQ=DAILY;COUNT=10;INTERVAL=2"
DTSTART <- as.Date("2020-10-26")
ans <- RRULE(DTSTART, source = txt)
expect_equal(ans$recurrence_set$DTSTART,
             seq(DTSTART, by = "2 days", length.out = 10))


## MONTHLY
txt <- "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1;UNTIL=20211210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=1;UNTIL=20211210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)


txt <- "FREQ=YEARLY;UNTIL=20251210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;BYMONTH=10;UNTIL=20251210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;BYMONTH=10,11,12;UNTIL=20251210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;UNTIL=20251210;INTERVAL=2"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;UNTIL=20251210;INTERVAL=10"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=1;UNTIL=20251210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1;UNTIL=20251210"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)


## WEEKLY
txt <- "FREQ=WEEKLY;COUNT=10"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=WEEKLY;COUNT=10;INTERVAL=2"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=WEEKLY;COUNT=10;INTERVAL=2;BYDAY=MO,TU"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)

txt <- "FREQ=WEEKLY;COUNT=10;INTERVAL=2;BYDAY=MO,TU;WKST=TU"
DTSTART <- as.Date("2020-10-26")
RRULE(DTSTART, source = txt)



## YEARLY
txt <- "FREQ=YEARLY;INTERVAL=1"
DTSTART <- as.Date("1978-08-19")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;INTERVAL=2;COUNT=10"
DTSTART <- as.Date("1978-08-19")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY;COUNT=10"
DTSTART <- as.Date("1978-08-19")
RRULE(DTSTART, source = txt)

txt <- "FREQ=YEARLY"
DTSTART <- as.Date("1978-08-19")
RRULE(DTSTART, source = txt)
