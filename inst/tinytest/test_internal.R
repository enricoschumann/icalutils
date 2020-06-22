


## ---------------- [ .next_weekday ] ----------------

start <- as.Date("2020-1-1") ## Wednesday

## 'start' is a Wednesday
## icalutils:::.wday["WE"]
## ==>  WE 
##       3 
expect_equal(icalutils:::.next_weekday(3, start),    start)  



## start is a Wednesday, count is 1
expect_equal(icalutils:::.next_weekday(3, start, interval = 1), start)
expect_equal(icalutils:::.next_weekday(3, start, interval = 2), start)
expect_equal(icalutils:::.next_weekday(3, start, interval = 3), start)

expect_equal(icalutils:::.next_weekday(3, start, interval = 1, count = 2),
             start + c(0, 7))

expect_equal(icalutils:::.next_weekday(3, start, interval = 2, count = 2),
             start + c(0, 14))


## start is Wednesday 2020-01-01; next Sunday: 2020-01-05
expect_equal(icalutils:::.next_weekday(0, start), as.Date("2020-01-05"))

## count is 1: only a single date is returned
expect_equal(icalutils:::.next_weekday(3:4, start),
             start)

expect_equal(icalutils:::.next_weekday(3:4, start, count = 2),
             start + 0:1)




start <- as.Date("2020-01-01") ## wednesday
expect_equal(icalutils:::.next_weekday(c(1,3,4), start, count = 3),
             start + c(0,1,5))

ans <- rrule(start, text="RRULE:FREQ=WEEKLY;BYDAY=MO,WE,TH;INTERVAL=1;COUNT=3")
expect_equal(ans$recurrence_set[["DTSTART"]], start + c(0,1,5))
