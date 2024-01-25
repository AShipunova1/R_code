# Use trip end date to calculate the usable date 30 days later

tic("USABLE_DATE")
logbooks_notoverridden <-
  logbooks_notoverridden |>
  mutate(USABLE_DATE =
           format(
             as.Date(logbooks_notoverridden$TRIP_END_DATE, '%Y-%m-%d') + 30,
             format = "%Y-%m-%d"
           ))

# Append a time to the due date since the submission data has a date and time

add_time = "23:59:59"

logbooks_notoverridden$USABLE_DATE <-
  as.POSIXct(paste(as.Date(
    logbooks_notoverridden$USABLE_DATE, '%Y-%m-%d'
  ),
  add_time),
  format = "%Y-%m-%d %H:%M:%S")
toc()

# USABLE_DATE: 5.56 sec elapsed

tic("USABLE_DATE_TIME")
logbooks_notoverridden <-
  logbooks_notoverridden |>
  mutate(USABLE_DATE_TIME =
           TRIP_END_DATE +
           days(30) +
           hours(23) +
           minutes(59) +
           seconds(59)) |>
  mutate(USABLE_DATE_TIME =
           with_tz(USABLE_DATE_TIME,
                   Sys.timezone()))
toc()
# USABLE_DATE_TIME: 0.25 sec elapsed

# ymd_hms("2009-03-08 01:59:59", tz = "America/Chicago")

aa <-
logbooks_notoverridden |>
  select(starts_with("USABLE_DAT"), TRIP_END_DATE) |>
  distinct() |>
  filter(!USABLE_DATE == USABLE_DATE_TIME)
# |>
  # str()

unlist(aa$TRIP_END_DATE[1])
unlist(aa$USABLE_DATE[1])
unlist(aa$USABLE_DATE_TIME[1])
unlist(Logbooks$TRIP_END_DATE[1])

# > unlist(aa$USABLE_DATE[1])
# [1] "2022-08-06 23:59:59 EDT"
# > unlist(aa$USABLE_DATE_TIME[1])
# [1] "2022-08-06 18:59:59 EST"

# 'data.frame':	365 obs. of  3 variables:
#  $ USABLE_DATE     : POSIXct, format: "2022-08-06 23:59:59" "2022-03-24 23:59:59" ...
#  $ USABLE_DATE_TIME: POSIXlt, format: "2022-08-06 23:59:59" "2022-03-24 23:59:59" ...
#  $ TRIP_END_DATE   : Date, format: "2022-07-07" "2022-02-22" ...


