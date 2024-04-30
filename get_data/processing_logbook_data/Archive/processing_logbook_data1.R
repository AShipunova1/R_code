# processing_logbook_data

#This code processes logbook data from Oracle, then cleans it up, so that we can use it in any logbook data analysis:
  #(1) pull all logbook and compliance data from Oracle
  #(2) clean up logbook data set, using Metrics Tracking and SRHS list
       #(a) remove records from SRHS vessels
       #(b) remove records from vessels without a GOM SEFHIER permit
       #(c) remove records where start date/time is after end date/time
       #(d) remove records for trips lasting more than 10 days
       #(e) only keep A, H and U logbooks for GOM permitted vessels (U because VMS allows Unknown Trip Type)
  #(3) remove all trips that were received > 30 days after trip end date, by using compliance data and time of submission

#general set up:

#load required packages
library(tidyr) #analysis of dataframes
library(readxl) #to read in XL files and by sheet
library(ROracle)
library(readr)
library(dplyr)
library(xlsx)
library(lubridate)
library(tidyverse)

library(tictoc) # Functions for timing

#set working and output directory - where do you keep the data and analysis folder on your computer?
# example: Path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Logbook Processing (Do this before all Logbook Analyses)/"

# annasPath <-
#   r"(C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\processing_logbook_data/)"

Path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessingLogbookData/"
Inputs <- "Inputs/"
Outputs <- "Outputs/"

#set the date ranges for the logbook and compliance data you are pulling

#To run the file as a whole, you can type this in the console: source('Processing Logbook Data.R') and hit enter.

# Caveats:
# 1) The way COMP_WEEK is calculated could get messed up depending on a given year time frame. It's due to something
# internal called the ISO number and how the function calculates the start of a week. If you are running this on a
# new data set, check your weeks to make sure it's calculating correctly after running that line of code.

# Auxiliary methods ----
# 1) A function to use every time we want to read a ready file or query the database if no files exist. Pressing F2 when the function name is under the cursor will show the function definition.

# The read_rds_or_run function is designed to read data from an RDS file if it exists or run a specified function to generate the data if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section
read_rds_or_run <- function(my_file_path,
                            my_data = as.data.frame(""),
                            my_function,
                            force_from_db = NULL) {

    # Check if the file specified by 'my_file_path' exists and 'force_from_db' is not set.
    if (file.exists(my_file_path) &
        is.null(force_from_db)) {
        # If the file exists and 'force_from_db' is not set, read the data from the RDS file.
        my_result <- readr::read_rds(my_file_path)
    } else {
        # If the file doesn't exist or 'force_from_db' is set, perform the following steps:
        # 1. Generate a message indicating the date and the purpose of the run.
        msg_text <- paste(today(), "run for", basename(my_file_path))
        tictoc::tic(msg_text)  # Start timing the operation.

        # 2. Run the specified function 'my_function' on the provided 'my_data' to generate the result.
        my_result <- my_function(my_data)

        tictoc::toc()  # Stop timing the operation.

        # 3. Save the result as an RDS binary file to 'my_file_path' for future use.
        # try is a wrapper to run an expression that might fail and allow the user's code to handle error-recovery.
        try(
        readr::write_rds(my_result,
                         my_file_path)
        )
    }

    # Return the generated or read data.
    return(my_result)
}


#------------------------------------------------------------------------------#

#### (1) pull all logbook and compliance data from Oracle ####

#Grab logbooks from Oracle (comment out and just read in file in output folder if you don't have Roracle)--------------------------------------------------------------------------
#Connect to Oracle ------------------------------------------------------------------
# must be on VPN!!!
#to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

#
# #set up where to call the data from
# con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
#                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]),
#                 dbname = "SECPR")
#
#
# #create DF, by calling from the table in Oracle - here you can specify what chunk of that data you want using "where"
# dat = dbGetQuery(con, "SELECT * FROM srh.mv_safis_trip_download@secapxdv_dblk
#                    WHERE trip_start_date >= '01-JAN-2022'")
#
# # #for a specified time interval:
# # compliance_data = dbGetQuery(con, "SELECT * FROM srh.mv_safis_trip_download@secapxdv_dblk
# #                    WHERE trip_start_date >= '01-JAN-2022'
# #                    and
# #                    trip_start_date < '01-JAN-2023'")
#
# View(dat)
#
#
# #save output
# write.csv(dat, file = paste(Path, Outputs, "SAFIS_TripsDownload_1.1.22-12.01.23.csv",
#               sep =""), row.names = FALSE) #use row.names = false to avoid a col of #s



# Grab compliance file from Oracle (or Comment this out if you don't have Roracle)------------------------------------------------------
#Connect to Oracle ------------------------------------------------------------------
# must be on VPN!!!
#to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

#
#set up where to call the data from
 con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]),
                 dbname = "SECPR")

#create variable with table to call data from, define year
compl_err_query <-
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '2020'"

# override_data_file_path is the same as compl_err_query_file
#   "//ser-fs1/sf/LAPP-DM Documents\\Ostroff\\SEFHIER\\Rcode\\ProcessingLogbookData\\Inputs\\compl_err_db_data_raw.rds"

# Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
compl_err_query_file <-
  file.path(Path, Inputs, "Compliance_raw_data_Year.rds")

# Check if the file path is correct, optional
# file.exists(compl_err_query_file)

compl_err_fun <-
  function(compl_err_query) {
    compliance_data <- dbGetQuery(con, compl_err_query)
    return(compliance_data)
  }

# create data frame using query to call the table above
# use the pre-defined method to check if there is a file saved already,
# read it or run the query and write the file fr future use

compliance_data <-
  read_rds_or_run(compl_err_query_file,
                  compl_err_query,
                  compl_err_fun
                  )

#### (2) clean up logbook data set, using Metrics Tracking and SRHS list ####


#### import and prep the permit data ####
#use Metrics Tracking report
#remove SRHS vessels from the list
#remove SA vessels from the list

#import the permit data
SEFHIER_MetricsTracking <- read.csv(
  paste(
    Path,
    Inputs,
    "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_2022.csv",
    sep = ""
  )
) #here I am using paste to combine the path name with the file, sep is used to say there are no breaks "" (or if breaks " ") in the paste/combining
#rename column headers
SEFHIER_MetricsTracking <-
  SEFHIER_MetricsTracking %>% rename(PERMIT_REGION = `Permit.Grouping.Region`, VESSEL_OFFICIAL_NUMBER = `Vessel.Official.Number`)

#import the list of SRHS vessels
#this is a single spreadsheet with all vessels listed, as opposed to the version where they are separated by region (bothregions_asSheets)
SRHSvessels <-
  read_csv(paste(Path, Inputs, "2022SRHSvessels.csv", sep = ""))
#reformat and rename column
colnames(SRHSvessels)[5] <- ("VESSEL_OFFICIAL_NUMBER")
SRHSvessels$VESSEL_OFFICIAL_NUMBER <-
  as.character(SRHSvessels$VESSEL_OFFICIAL_NUMBER)

#remove SRHSvessels from SEFHIER_MetricsTracking list
SEFHIER_PermitInfo <-
  anti_join(SEFHIER_MetricsTracking, SRHSvessels, by = 'VESSEL_OFFICIAL_NUMBER')

#remove the columns you don't need
SEFHIER_PermitInfo <- SEFHIER_PermitInfo[ ,c(1,8)]
# useful stat, not needed for processing
# NumSEHFIERPermits <- nrow(SEFHIER_PermitInfo)
# 3469

#### import and prep the logbook data ####
#delete logbook records where start date/time is after end date/time
#delete logbooks for trips lasting more than 10 days
#only keep A, H and U logbooks for GOM permitted vessels (U because VMS allows Unknown Trip Type)

# import the logbook data or download from Oracle db
logbooks_file_path <-
  paste(Path, Inputs, "SAFIS_TripsDownload_1.1.22-12.01.23.rds",
        sep = "")
#here I am using paste to combine the path name with the file, sep is used to say there are no breaks "" (or if breaks " ") in the paste/combining

# Logbooks <- read_rds(logbooks_file_path)

# Instead, we can read a file if exists or pull data from the Oracle db
logbooks_download_query <-
  "SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_start_date >= '01-JAN-2022'
  AND trip_start_date <= '01-DEC-2023'
"

# Define a function 'mv_safis_trip_download_fun' to retrieve data from the database using a specified query.
logbooks_download_fun <-
  function(logbooks_download_query) {
  # Use 'dbGetQuery' to execute the query on the database connection 'con' and return the result.
  result <- dbGetQuery(con, logbooks_download_query)

  # Return the result of the database query.
  return(result)
}

# Use 'read_rds_or_run' defined above to either read permit information from an RDS file or execute a query to obtain it.
Logbooks <-
  read_rds_or_run(logbooks_file_path,
                  logbooks_download_query,
                  logbooks_download_fun)

# not needed for processing
# dim(Logbooks)
# 484413    149

# Save column number for fututre use
logbooks_col_num <- ncol(Logbooks)
# 149

#rename column
# Was:
# colnames(Logbooks)[6]
# "VESSEL_OFFICIAL_NBR"

colnames(Logbooks)[6] <- ("VESSEL_OFFICIAL_NUMBER")

# reformat trip start/end date/time
# 1)
# Was:
# Logbooks$TRIP_START_DATE |>
#   head(1)
# "2022-07-07 01:00:00 EDT"

Logbooks$TRIP_START_DATE <-
  as.Date(Logbooks$TRIP_START_DATE)

# Now:
# Logbooks$TRIP_START_DATE |>
#   head(1)
# "2022-07-07"

# 2)
# Was:
# Logbooks$TRIP_START_TIME |>
  # head(1)
# "0800"

Logbooks$TRIP_START_TIME <-
  as.character(sprintf("%04d", as.numeric(Logbooks$TRIP_START_TIME)))

# Now:
# Logbooks$TRIP_START_TIME |>
#   head(1)
# "0800"

# 3)
Logbooks$TRIP_END_DATE <-
  as.Date(Logbooks$TRIP_END_DATE)

# 4)
Logbooks$TRIP_END_TIME <-
  as.character(sprintf("%04d", as.numeric(Logbooks$TRIP_END_TIME)))

# filter out just 2022 logbook entries
Logbooks <-
  Logbooks %>% filter(TRIP_START_DATE >= "2022-01-01" &
                        TRIP_START_DATE <= "2022-12-31")

#check logbook records for cases where start date/time is after end date/time, delete these records
#create column for start date & time
Logbooks$STARTDATETIME <- as.POSIXct(paste(Logbooks$TRIP_START_DATE,
                                           Logbooks$TRIP_START_TIME),
                                           format = "%Y-%m-%d %H%M")

# not needed for processing
# Logbooks$STARTDATETIME |>
#   head(1)
# "2022-07-07 08:00:00 EDT"

#create column for end date & time
Logbooks$ENDDATETIME <- as.POSIXct(paste(Logbooks$TRIP_END_DATE,
                                         Logbooks$TRIP_END_TIME),
                                         format = "%Y-%m-%d %H%M")

#the Time Stamp Error is true if start date/time is greater than or equal to end date/time, false if not
Logbooks['TimeStampError'] <-
  ifelse(Logbooks$STARTDATETIME >= Logbooks$ENDDATETIME, "true", "false")

# how many logbooks were thrown out because of a time stamp error?
# Logbooks_TimeStampError <-
#   Logbooks %>% filter(TimeStampError == "true") #useful stat, not needed for processing

# NumLogbooks_TimeStampError <-
  # nrow(Logbooks_TimeStampError) #useful stat, not needed for processing
# 857

#only keep the rows where there is no error between start & end date & time
Logbooks <-
  Logbooks %>% filter(TimeStampError == "false")

# useful stat, not needed for processing
# nrow(Logbooks)
# 94835

#For trips lasting more than 10 days, delete the records. The assumption is there is an
#error in either start or end date and time and the trip didn't really last that long.
Logbooks['TripLength'] <-
  as.numeric(difftime(Logbooks$ENDDATETIME,
                      Logbooks$STARTDATETIME,
                      units = "hours"))

# output trips with length > 240 into data frame
# LogbooksTooLong = Logbooks %>% filter(TripLength > 240) #useful stat, not needed for processing
# NumLogbooksTooLong = nrow(LogbooksTooLong) #useful stat, not needed for processing
# 74

#only keep trips with a length less than or equal to 10 days (240 hours)
Logbooks <-
  Logbooks %>% filter(TripLength <= 240)

#get rid of new columns, don't need them anymore. Use the number of column in original dataset (= 149)
Logbooks <-
  Logbooks[, c(1:logbooks_col_num)]

# only keep A, H and U logbooks (U means Unknown Trip Type, a VMS issue)
SEFHIER_logbooks <-
  left_join(SEFHIER_PermitInfo,
            Logbooks,
            by = c("VESSEL_OFFICIAL_NUMBER")) #joins permit info and trip info together

SEFHIER_logbooksAHU <-
  subset(SEFHIER_logbooks, (SEFHIER_logbooks$TRIP_TYPE %in% c("A", "H", "U"))) # subsets the data to charter and headboat logbook entries only

#subsets the data with no logbook entries, useful stat, not needed for processing
VesselsNoLogbooks <-
  subset(SEFHIER_logbooks, (SEFHIER_logbooks$TRIP_TYPE %in% c(NA)))

# the same as
# VesselsNoLogbooks <-
#   subset(SEFHIER_logbooks, is.na(SEFHIER_logbooks$TRIP_TYPE))

# nrow(VesselsNoLogbooks)
# 1636

#### (3) remove all trips that were received > 30 days after trip end date, by using compliance data and time of submission ####

#### prep the compliance data ####

# Use compliance data uploaded before
OverrideData <- compliance_data

# stat, not needed for processing
# dim(OverrideData)
# 458071     19

#filter out year 2022
OverrideData <- OverrideData %>% filter(COMP_YEAR == 2022)

#only keep the columns you need
# OverrideData <- OverrideData[,c(4,8,13)]

#change column name
OverrideData_1 <-
OverrideData |>
  dplyr::rename(VESSEL_OFFICIAL_NUMBER = "VESSEL_OFFICIAL_NBR",
                OVERRIDDEN = "IS_COMP_OVERRIDE")

# I changed it to the `rename()` syntax above so we know what columns are renamed (AS)
# colnames(OverrideData)[1] <- ("VESSEL_OFFICIAL_NUMBER")
# colnames(OverrideData)[3] <- ("OVERRIDDEN")

#change data type this column if needed
#OverrideData$VESSEL_OFFICIAL_NUMBER <- as.character(OverrideData$VESSEL_OFFICIAL_NUMBER)

#### determine what weeks were overridden, and exclude those logbooks ####

#assign each logbook a week designation (first day of the reporting week is a Monday)
#use the end date to calculate this, it won't matter for most trips, but for some trips that
#happen overnight on a Sunday, it might affect what week they are assigned to
#https://stackoverflow.com/questions/60475358/convert-daily-data-into-weekly-data-in-r

# not necessary now, already done (AS)
SEFHIER_logbooksAHU$TRIP_END_DATE2 <-
  as.Date(SEFHIER_logbooksAHU$TRIP_END_DATE, '%Y-%m-%d') #change format to a date

# Calculate the ISO week number for each date in the 'TRIP_END_DATE2' column.
# lubridate package has following methods:
# week() returns the number of complete seven day periods that have occurred between the date and January 1st, plus one.
#
# isoweek() returns the week as it would appear in the ISO 8601 system, which uses a reoccurring leap week.
#
# epiweek() is the US CDC version of epidemiological week. It follows same rules as isoweek() but starts on Sunday. In other parts of the world the convention is to start epidemiological weeks on Monday, which is the same as isoweek.
#
# The `lubridate::ymd` function is used to parse the dates from the 'TRIP_END_DATE2' column with year, month, and day components

SEFHIER_logbooksAHU <-
  SEFHIER_logbooksAHU %>%
  mutate(COMP_WEEK = isoweek(TRIP_END_DATE), # puts it in week num
         TRIP_END_YEAR = isoyear(TRIP_END_DATE)) # adds a year

# To see the result, note week 52 of 2021 (AS):
SEFHIER_logbooksAHU |>
  select(TRIP_END_DATE, TRIP_END_YEAR, COMP_WEEK) |>
  distinct() |>
  arrange(TRIP_END_DATE) |>
  head(3)
#   TRIP_END_DATE TRIP_END_YEAR COMP_WEEK
# 1    2022-01-01          2021        52
# 2    2022-01-02          2021        52
# 3    2022-01-03          2022         1

SEFHIER_logbooksAHU <-
  SEFHIER_logbooksAHU %>%
  filter(TRIP_END_YEAR == 2022)

# stat, not needed for processing
# nrow(SEFHIER_logbooksAHU)
# 318706

# to see the respective data in OverrideData
# not needed for processing
# OverrideData |>
#   select(COMP_YEAR,
#          COMP_WEEK_END_DT,
#          COMP_WEEK) |>
#   distinct() |>
#   arrange(COMP_WEEK_END_DT) |>
#   head(3)
#   COMP_YEAR COMP_WEEK_END_DT COMP_WEEK
# 1      2022       2022-01-09         1
# 2      2022       2022-01-16         2
# 3      2022       2022-01-23         3

# if a week for a vessel was overridden (OverrideData), remove the trip reports from the corresponding week in the logbook data

# add override data to df
SEFHIER_logbooksAHU_overr <-
  left_join(SEFHIER_logbooksAHU,
            OverrideData,
            join_by(TRIP_END_YEAR == COMP_YEAR,
                    VESSEL_OFFICIAL_NUMBER == VESSEL_OFFICIAL_NBR,
                    COMP_WEEK),
            relationship = "many-to-many"
            )
# Was:
# by = c("VESSEL_OFFICIAL_NUMBER", "COMP_WEEK"))
# added a year (AS)

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU_overr)
# 318751    170

SEFHIER_logbooksAHU_overridden <-
  filter(SEFHIER_logbooksAHU_overr, IS_COMP_OVERRIDE == 1) #data frame of logbooks that were overridden

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU_overridden)
# 9435  170

# To see if we need to remove overridden trips (AS)
# SEFHIER_logbooksAHU1 |>
#   filter(IS_COMP_OVERRIDE == 1)  |>
#   select(all_of(contains("TRIP")),
#          all_of(contains("over"))) |>
#   View()

SEFHIER_logbooksAHU_notoverridden <-
  filter(SEFHIER_logbooksAHU_overr, IS_COMP_OVERRIDE == 0) #data frame of logbooks that weren't overridden

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU_notoverridden)
# 308745    170

SEFHIER_logbooksAHU_NA <-
  filter(SEFHIER_logbooksAHU_overr, is.na(IS_COMP_OVERRIDE)) #logbooks with an Overridden value of NA, because they were
# 1) submitted by a vessel that is missing from the Compliance report and therefore has no associated override data, or
# 2) submitted by a vessel during a period in which the permit was inactive, and the report was not required

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU_NA)
# 571 170

#SEFHIER vessels missing from the Compliance report
SEFHIERVesselsMissing <- anti_join(SEFHIER_PermitInfo[,1], OverrideData, by='VESSEL_OFFICIAL_NUMBER')
#SEFHIER AH logbooks from vessels missing from the Compliance report
SEFHIERVesselsMissingAHUlogbooks <- inner_join(SEFHIERVesselsMissing, SEFHIERlogbooksAHU_NA, by='VESSEL_OFFICIAL_NUMBER')
#add missing logbooks back to the not overridden data frame
SEFHIERlogbooksAHU_notoverridden <- rbind(SEFHIERlogbooksAHU_notoverridden, SEFHIERVesselsMissingAHUlogbooks)
#remove missing logbooks from NA dataset, the NA dataset is now only those that were submitted when not needed
SEFHIERlogbooksAHU_NA <- anti_join(SEFHIERlogbooksAHU_NA, SEFHIERVesselsMissingAHUlogbooks)

#only keep the logbooks from non overridden weeks
SEFHIER_logbooksAHU <- SEFHIER_logbooksAHU_notoverridden
# NumSEFHIERlogbooksAHU <- nrow(SEFHIER_logbooksAHU) #useful stat, not needed for processing
# dim(SEFHIER_logbooksAHU_notoverridden)
# 308745 170

#We have decided to throw out logbooks that were submitted when the permit was inactive, the logic
#being we shouldn't include logbooks that weren't required in the first place. Alternatively,
#deciding to keep in the NAs means we would be keeping reports that were submitted by a vessel
#during a period in which the permit was inactive, and the report was not required.
#rbind(SEFHIER_logbooksAHU_notoverridden, SEFHIER_logbooksAHU_NA) this is the alternative

# unique list of vessels that submitted logbooks, useful stat, not needed for processing (doesn't work, AS)
# SEFHIER_logbooksAHU_vessels <-
  # unique(rbind(SEFHIER_logbooksAHU[, 1], SEFHIER_logbooksAHU_overridden[, 1]))
# NumSEFHIER_logbooksAHU_vessels <- nrow(SEFHIER_logbooksAHU_vessels)


#### determine which logbooks were turned in within 30 days, making them usable for analyses ####

#use trip end date to calculate the usable date 30 days later
SEFHIER_logbooksAHU <-
  SEFHIER_logbooksAHU %>%
  mutate(USABLE_DATE =
           format(
             as.Date(SEFHIER_logbooksAHU$TRIP_END_DATE, '%Y-%m-%d') + 30,
             format = "%Y-%m-%d"
           ))

#append a time to the due date since the submission data has a date and time
add_time <- "23:59:59" # 24 hr clock
SEFHIER_logbooksAHU$USABLE_DATE <-
  as.POSIXct(paste(
    as.Date(SEFHIER_logbooksAHU$USABLE_DATE, '%Y-%m-%d'),
    add_time
  ),
  format = "%Y-%m-%d %H:%M:%S")

#format the submission date (TRIP_DE)
SEFHIER_logbooksAHU$TRIP_DE <-
  as.POSIXct(SEFHIER_logbooksAHU$TRIP_DE, format = "%Y-%m-%d %H:%M:%S")

#subtract the usable date from the date of submission
#value is true if the logbook was submitted within 30 days, false if the logbook was not
SEFHIER_logbooksAHU['USABLE'] <-
  ifelse(SEFHIER_logbooksAHU$USABLE_DATE >= SEFHIER_logbooksAHU$TRIP_DE, "true", "false")

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU)
# [1] 308745    171

#data frame of logbooks that were usable
SEFHIER_logbooksAHU_usable <-
  SEFHIER_logbooksAHU %>% filter(USABLE == "true")

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU_usable)
# [1] 269713    171

# SEFHIER_logbooksAHU_usable <- SEFHIER_logbooksAHU_usable[,c(1:150)] #gets rid of columns used for processing
# I commented it out to keep compliance and override information in (AS)

# NumSEFHIER_logbooksAHU_usable = nrow(SEFHIER_logbooksAHU_usable) #useful stat, not needed for processing
# 269713

# NumVessels_usablelogbooks = length(unique(SEFHIER_logbooksAHU_usable[,1])) #useful stat, not needed for processing
# NumVessels_usablelogbooks
# 1617

#data frame of logbooks that were not usable, useful stats, not needed for processing
# SEFHIER_logbooksAHU_unusable <- SEFHIER_logbooksAHU %>% filter(USABLE == "false")
# NumSEFHIER_logbooksAHU_unusable <- nrow(SEFHIER_logbooksAHU_unusable) # how many logbooks were unusable?
# 39032

# NumVessels_unusablelogbooks <- length(unique(SEFHIER_logbooksAHU_unusable[,1])) #how many vessels had an unusable logbook?
# 1053

# Separate GOM only, SA only or dual using PERMIT_GROUP ----
# Data example:
# SEFHIER_logbooksAHU_usable %>%
#   select(PERMIT_GROUP) |>
#   distinct() |>
#   tail(3)
# PERMIT_GROUP
# (CDW)CDW, (CHG)1615, (CHS)CHS, (SC)SC
# (CHG)1034, (RCG)982
# (CHG)589, (RCG)567

# Note, PERMIT_REGION column has only GOM or SA

# Auxiliary: how to find the column name
#
# grep("permit",
#      names(SEFHIER_logbooksAHU_usable),
#      value = TRUE,
#      ignore.case = TRUE)

# Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group

# Check if 'permit_group_field_name' doesn't contain 'RCG', 'HRCG', 'CHG', or 'HCHG'; assign "sa_only" if true
# Check if 'permit_group_field_name' doesn't contain 'CDW', 'CHS', or 'SC'; assign "gom_only" if true
# For all other cases, assign "dual"

SEFHIER_logbooksAHU_usable_p_regions <-
  SEFHIER_logbooksAHU_usable %>%
  mutate(
    permit_sa_gom =
      dplyr::case_when(
        !grepl("RCG|HRCG|CHG|HCHG", PERMIT_GROUP) ~
          "sa_only",
        !grepl("CDW|CHS|SC", PERMIT_GROUP) ~ "gom_only",
        .default = "dual"
      )
  )

# Explanation:
#
# 1. **Create New Dataframe:**
#    - `SEFHIER_logbooksAHU_usable_regions <- SEFHIER_logbooksAHU_usable %>% ...`: Create a new dataframe 'SEFHIER_logbooksAHU_usable_regions' based on the 'SEFHIER_logbooksAHU_usable' dataframe.
#
# 2. **Use 'mutate' to Add Column:**
#    - `mutate(permit_sa_gom = dplyr::case_when(...))`: Utilize the 'mutate' function to add a new column 'permit_sa_gom' with values determined by the conditions specified in the 'case_when' function.
#
# 3. **Conditions with 'case_when':**
#    - `!grepl("RCG|HRCG|CHG|HCHG", PERMIT_GROUP) ~ "sa_only"`: If 'PERMIT_GROUP' does not contain the specified patterns, assign "sa_only".
#    - `!grepl("CDW|CHS|SC", PERMIT_GROUP) ~ "gom_only"`: If 'PERMIT_GROUP' does not contain the specified patterns, assign "gom_only".
#    - `.default = "dual"`: For any other case, assign "dual".
#
# 4. **'dplyr::' Prefix:**
#    - `dplyr::case_when(...)`: Prefix 'dplyr::' is used to explicitly specify that the 'case_when' function is from the 'dplyr' package, ensuring there is no ambiguity if other packages also have a 'case_when' function.

# stat, not needed for processing
# dim(SEFHIER_logbooksAHU_usable_p_regions)
# 269713    172

# export usable logbooks
#write.csv(GOMlogbooksAHU_usable, "//ser-fs1/sf/LAPP-DM Documents\\Ostroff\\SEFHIER\\Rcode\\ProcessingLogbookData\\Outputs\\UsableLogbooks2022.csv", row.names=FALSE)
#write.xlsx(GOMlogbooksAHU_usable, 'UsableLogbooks2022.xlsx', sheetName="2022Logbooks", row.names=FALSE)

# annas_file_path <-
  # file.path(Path, "Outputs", "SEFHIER_usable_logbooks_2022.rds")

jennys_file_path <-
  paste(Path, Outputs, "SEFHIER_usable_logbooks_2022.rds",
        sep = "")

write_rds(
  SEFHIER_logbooksAHU_usable_p_regions,
  file = jennys_file_path
)



