# doesn't work
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Oracle in instantclient_12_2};", timeout = 10)

Driver = "Oracle in instantclient_12_2"
Host   = "nova.nefsc.noaa.gov"
SVC    = "nova"
UID    = "ashipunova"
PWD    = getPass(msg="Enter Password: ")
Port   = 1526
PWD    = rstudioapi::askForPassword("Database password")
