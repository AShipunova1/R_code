# db_compliance
# Throughout the code
# "Declaration" == "trip notification"
# "Logbook"     == "trip"

# Assumptions
# 1) Disregard the time zone for trips and trip notifications

# It would be really interesting to see an after the fact analysis done for compliance with another point of view, you can query the source of the activity from the tables we download from ACCSP and the permit materialized view mv_sero_fh_permits_his and then apply the latest compliance rules (all the fields needed for compliance are in these tables), it is very different to write an analysis report after the fact than build something for day-to-day activity that it has to be assigned to multiple users step by step.
# CATCHES
# EFFORTS
# TRIPS
# TRIPS_NEG
# TRIP_NOTIFICATIONS
# VESSELS

# setup ----
library(tictoc)
library(zoo)
# install.packages("sqldf")
library(sqldf)
library(gridExtra)
library(readxl)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "db_compliance"
con <- connect_to_secpr()

# get data ----
get_data_file_path <- file.path(
  my_paths$git_r,
  current_project_name,
  paste0("get_data_",
         current_project_name,
         ".R")
)
source(get_data_file_path)

# 2022 year interval ----
interval_2022 <- lubridate::interval(as.Date('2022-01-01'),
                                    as.Date('2022-12-31'))

# vessels to use (from FHIER metric trascking) ----
# dim(fhier_reports_metrics_tracking)
# [1] 3629   13

vessel_official_number_fhier_metrics <-
  fhier_reports_metrics_tracking |>
  select(vessel_official_number) |>
  distinct()

dim(vessel_official_number_fhier_metrics)
# [1] 3629    1

# vessels_permits_2022 ----
dim(vessels_permits_2022)
# [1] 40474    51

## weird headers ----
# print_df_names(vessels_permits_2022)
vessels_permits_2022_c <-
  vessels_permits_2022 |>
  rename("PERMIT_VESSEL_ID" = "QCSJ_C000000000300000") |>
  rename("VESSEL_VESSEL_ID" = "QCSJ_C000000000300001")

dim(vessels_permits_2022_c)
# [1] 40474    51

## fhier_metrics vessels only ----
vessels_permits_2022_c_me <-
  vessels_permits_2022_c |>
  filter(PERMIT_VESSEL_ID %in% vessel_official_number_fhier_metrics$vessel_official_number)

dim(vessels_permits_2022_c_me)
# [1] 30306    51

## region permit groups ----
vessels_permits_2022_r <-
  vessels_permits_2022_c_me |>
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

# print_df_names(vessels_permits_2022_r)

## add my_end_date ----
tic("add my_end_date")
vessels_permits_2022_r_end_date <-
  vessels_permits_2022_r |>
  rowwise() |>
  mutate(my_end_date =
           case_when((END_DATE < EFFECTIVE_DATE) &
                       (EXPIRATION_DATE > EFFECTIVE_DATE)
                     ~ EXPIRATION_DATE,
                     .default =
                       max(END_DATE,                                     EXPIRATION_DATE,
                           na.rm = T)
           )) %>%
  # select(-c(END_DATE,
            # EXPIRATION_DATE)) %>%
  dplyr::ungroup() |>
  distinct()
toc()
# add my_end_date: 25.6 sec elapsed
# add my_end_date: 37 sec elapsed
# add my_end_date: 20.71 sec elapsed

dim(vessels_permits_2022_r_end_date)
# [1] 20231    53
# [1] 15152    53

## combine v ids ----

id_names <- c(
  "COAST_GUARD_NBR",
  "PERMIT_VESSEL_ID",
  "SERO_OFFICIAL_NUMBER",
  "STATE_REG_NBR",
  "SUPPLIER_VESSEL_ID",
  "VESSEL_ALT_NUM",
  "VESSEL_VESSEL_ID")

tic("uid")
vessels_permits_2022_r_end_date_uid <-
  vessels_permits_2022_r_end_date |>
  rowwise() |>
  mutate(all_ids = list(
    c(
      COAST_GUARD_NBR,
      PERMIT_VESSEL_ID,
      SERO_OFFICIAL_NUMBER,
      STATE_REG_NBR,
      SUPPLIER_VESSEL_ID,
      VESSEL_ALT_NUM,
      VESSEL_VESSEL_ID
    )
  )) |>
  mutate(unique_all_vessel_ids = list(na.omit(unique(all_ids)))) |>
  ungroup()
toc()
# uid: 1.63 sec elapsed

dim(vessels_permits_2022_r_end_date_uid)
# [1] 20231    55
# [1] 15152    55

### fewer fields ----
vessels_permits_2022_r_end_date_uid_short <-
  vessels_permits_2022_r_end_date_uid |>
  select(
    VESSEL_VESSEL_ID,
    PERMIT_VESSEL_ID,
    EFFECTIVE_DATE,
    END_DATE,
    EXPIRATION_DATE,
    permit_sa_gom,
    my_end_date,
    unique_all_vessel_ids
  ) |>
  distinct()

dim(vessels_permits_2022_r_end_date_uid_short)
# [1] 9442    8
# [1] 6207    8

## get the earliest and the latest permit dates ----
# print_df_names(vessels_permits_2022_r_end_date_uid_short)
vessels_permits_2022_r_end_date_uid_short_mm <-
  vessels_permits_2022_r_end_date_uid_short |>
  group_by(unique_all_vessel_ids, permit_sa_gom) |>
  mutate(
    min_permit_eff_date = min(EFFECTIVE_DATE),
    max_permit_end_date = max(my_end_date)
  ) |>
  ungroup()

dim(vessels_permits_2022_r_end_date_uid_short_mm)
# [1] 9442   8
# [1] 6207   10

# vessels_permits_2022_r_end_date_uid_short_mm |>
#   filter(grepl('FL8701TB', unique_all_vessel_ids)) |> View()
# 2023 is here, ok

# vessels_permits_2022_r_end_date_uid_short_mm |>
#   filter(grepl('FL9004NX', unique_all_vessel_ids)) |>
#   View()
# diff sa / gom


## add weeks and months ----

vessels_permits_2022_r_end_date_uid_short_mm_w_y <-
  vessels_permits_2022_r_end_date_uid_short_mm |>
    mutate(
    EFFECTIVE_DATE_week_num =
      strftime(EFFECTIVE_DATE, format = "%U"),
    my_end_week_num =
      strftime(my_end_date, format = "%U"),
    EFFECTIVE_DATE_y =
      year(EFFECTIVE_DATE),
    my_end_y =
      year(my_end_date),
    EFFECTIVE_DATE_m =
      zoo::as.yearmon(EFFECTIVE_DATE),
    my_end_m =
      zoo::as.yearmon(my_end_date)
  ) %>%
  mutate(
    EFFECTIVE_DATE_week_num =
      as.double(EFFECTIVE_DATE_week_num),
    my_end_week_num =
      as.double(my_end_week_num)
  )

dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y)
# [1] 9433   16
# [1] 9442   14
# [1] 6207   16

## get permit periods ----
tic("get permit periods")
# glimpse(vessels_permits_2022_r_end_date_uid_short_mm_w_y)
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y %>%
  group_by(permit_sa_gom, unique_all_vessel_ids) |>
  mutate(eff_int =
           lubridate::interval(min_permit_eff_date,
                               max_permit_end_date)) |>
  mutate(permit_2022_int =
           lubridate::intersect(eff_int,
                                interval_2022)) |>

  ungroup()
toc()
# get permit periods: 46.29 sec elapsed
# get permit periods: 48.8 sec elapsed
# get permit periods: 96.22 sec elapsed
# get permit periods: 111.64 sec elapsed
# get permit periods: 68.61 sec elapsed

## mark dual ----
# Dual only if GOM and SA for the same period
# dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv)

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |>
  # for each vessel and permit in effect interval
  group_by(unique_all_vessel_ids, eff_int, permit_2022_int) |>
  # create a list of all permit regions
  mutate(all_permit_sa_gom = list(na.omit(unique(permit_sa_gom)))) |>
  # get the length of each list of permits
  mutate(all_permit_sa_gom_size = lengths(all_permit_sa_gom)) |>
  # if there are both sa and gom mark as dual,
  # otherwise keep the original permit region
  mutate(permit_sa_gom_dual =
           case_when(all_permit_sa_gom_size > 1 ~                                              "dual",
                     .default = permit_sa_gom)) |>
  # remove temporary columns
  select(-c(all_permit_sa_gom, all_permit_sa_gom_size)) |>
  ungroup()

### check ----
dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual)
# [1] 9442   16
# [1] 6207   19

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  filter(grepl("FL8701TB|FL3610NF|FL9004NX", unique_all_vessel_ids)) |>
  select(unique_all_vessel_ids,
         permit_sa_gom_dual) |>
  distinct() |>
  glimpse()
# $ unique_all_vessel_ids <list> <"FL3610NF", "328460">, <"FL8701TB", …
# $ permit_sa_gom_dual    <chr> "dual", "sa_only", "gom_only", "sa_only"
# FL9004NX in both, but not dual, bc intervals do not overlap

new_dual_ids <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  filter(permit_sa_gom_dual == "dual") |>
  select(unique_all_vessel_ids) |>
  distinct()

dim(new_dual_ids)
# [1] 275   1
# [1] 272   1

#### why not in new? ----

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  filter(grepl("FL2995SR", unique_all_vessel_ids)) |>
  select(eff_int, permit_sa_gom_dual) |>
  head()
  # eff_int                            permit_sa_gom_dual
#   <Interval>                                       <chr>
# 1 2022-03-02 23:00:00 EST--2023-01-30 23:00:00 EST gom_only
# 2 2022-10-19 00:00:00 EDT--2024-01-30 23:00:00 EST sa_only
# gom and sa periods overlap


vessels_permits_2022_r |>
  # filter(PERMIT_VESSEL_ID == "FL9004NX") |>
  filter(PERMIT_VESSEL_ID == "TX6550AU") |>
  distinct() |>
  glimpse()
# $ TOP                   <chr> "CDW", "CHS", "CHG", "RCG"
# $ END_DATE              <dttm> 2022-07-31, 2022-07-31, 2023-07-31, 2023-07-31
# $ permit_sa_gom         <chr> "sa_only", "sa_only", "gom_only", "gom_only"

## split permits by region ----
# the same vessel can be in both sa_only and gom only in diff time
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual %>%
  split(as.factor(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual$permit_sa_gom_dual))

map_df(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list, dim)
#    dual gom_only sa_only
# 1   635     1879    6308
# 2    33       33      33
# 1   653     1940    6356
# 2    37       37      37
# 2    26       26      26
# 2    30       30      30
# 1   917     2066    6459 (the same vessel in diff cat)
# 2    16       16      16
# 2    19       19      19
# 1   911     1781    3515
# 2    19       19      19

# TODO: compare vessel_permits from db and v_permits by overlapping with interval 2022
# adjust the query

## check if vessels are duplicated in a region
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual$unique_all_vessel_ids |>
  unique() |>
  length()
# 911
  # unique()
# 272

names(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list)

reg_cnts <-
  # "dual"     "gom_only" "sa_only"
  names(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list) |>
  map(function(reg_name) {
    curr_ids <-
      vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list[[reg_name]]$unique_all_vessel_ids
    l <- length(curr_ids)
    lu <- length(unique(curr_ids))
    res <- c(reg_name, l, lu)
    return(res)
  }
)

glimpse(reg_cnts)
 # $ : chr [1:3] "dual" "917" "275"
 # $ : chr [1:3] "gom_only" "2066" "1322"
 # $ : chr [1:3] "sa_only" "6459" "3956"
# FHIER metrics vsls
 # $ : chr [1:3] "dual" "911" "272"
 # $ : chr [1:3] "gom_only" "1781" "1079"
 # $ : chr [1:3] "sa_only" "3515" "2302"

### what makes them duplicates ----
#### in dual ----
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |> dim()
# 911

# FL3610NF 4

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |>
  select(-starts_with("EXPIRATION_DATE"),
    -starts_with("END_DATE"),
    -permit_sa_gom
  ) |>
  distinct() |> dim()
# [1] 469  16

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |>
  filter(grepl("FL3610NF", unique_all_vessel_ids)) |>
  glimpse()
# $ permit_sa_gom           <chr> "sa_only", "gom_only", "sa_only", "gom_only"
# $ permit_2022_int         <Interval> 2021-12-31 19:00:00 EST--2022-12-30 19:00:00 EST,…
# $ permit_sa_gom_dual      <chr> "dual", "dual", "dual", "dual"

## check if the same vessel in dual and not ----
dual_gom <-
  intersect(
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual$unique_all_vessel_ids,
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$gom_only$unique_all_vessel_ids
)

length(dual_gom)
# 0

dual_sa <-
  intersect(
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual$unique_all_vessel_ids,
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only$unique_all_vessel_ids
)
length(dual_sa)
# 0

gom_sa <-
  intersect(
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$gom_only$unique_all_vessel_ids,
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only$unique_all_vessel_ids
)
length(gom_sa)
# 92
# ASK?: these will be counted in both sa and gom compliance

# end vessel_permits preparations ----

# Trips, trips info, trips neg ----
# Trip data (= logbooks) ----

## add trip interval ----

trips_info_2022_int <-
  trips_info_2022 %>%
  mutate(trip_int =
           lubridate::interval(
             lubridate::floor_date(TRIP_START_DATE,
                                   unit = "day"),
             lubridate::floor_date(TRIP_END_DATE,
                                   unit = "day")
           ))

### trips durations:
trips_info_2022_int_dur <-
  trips_info_2022_int |>
  mutate(trip_dur =
           lubridate::as.duration(trip_int)) |>
  mutate(trip_dur_days =
           as.numeric(trip_dur, "days"))

# write_csv(trips_info_2022_int_dur, "trips_info_2022_int_dur.csv")

### check trips_info_2022_int ----
trips_info_2022_int %>%
  select(TRIP_START_DATE, TRIP_END_DATE, trip_int) %>%
  dim()
# [1] 98528     3

## trip types A and H trips ----
trips_info_2022_int_ah <-
  trips_info_2022_int %>%
  filter(TRIP_TYPE %in% c("A", "H"))

## Keep only SERO permitted ----
trips_info_2022_int_ah_sero <-
  trips_info_2022_int_ah |>
  filter(!is.na(SERO_VESSEL_PERMIT)) |>
  distinct()

# Trip notifications (= declarations) ----
# Jenny:
# The declaration's notification type ID = 6 (hail-out)
# The declaration has an intended fishing flag = Yes
# The declaration has not been canceled (i.e., we cannot find another declaration
# record that has the same SAFIS vessel ID, trip start date, and trip start time
# as the declaration we are currently checking where this other record's notification type ID = 5)

## trip types A and H trip_notif ----
trips_notifications_2022 %>%
   # select(TRIP_TYPE) %>% distinct()
   count(TRIP_TYPE)
#   TRIP_TYPE     n
# 1         A 55328
# 2         C   202
# 3         H 12410
# 4         R  2116

trips_notifications_2022_ah <-
  trips_notifications_2022 %>%
  filter(TRIP_TYPE %in% c("A", "H"))

dim(trips_notifications_2022)
# [1] 70056    33

dim(trips_notifications_2022_ah)
# [1] 67738    33

## not cancelled ----
trips_notifications_2022_ah |>
  count(NOTIFICATION_TYPE_ID)
#   NOTIFICATION_TYPE_ID     n
# 1                    5   109
# 2                    6 63549
# 5 = cancellation
# 6 = hail out/declaration
#   NOTIFICATION_TYPE_ID     n
# 1                    5   111
# 2                    6 67627

# print_df_names(trips_notifications_2022)
# 5/6
trips_notifications_2022_ah_5_6 <-
  trips_notifications_2022_ah |>
  group_by(
    TRIP_TYPE,
    VESSEL_ID,
    TRIP_START_DATE,
    TRIP_START_TIME,
    TRIP_END_DATE,
    TRIP_END_TIME
  ) |>
  mutate(NOTIFICATION_TYPE_IDs =
              toString(unique(NOTIFICATION_TYPE_ID))) |>
  ungroup()

dim(trips_notifications_2022_ah)
# [1] 63658    33
# [1] 67738    33

dim(trips_notifications_2022_ah_5_6)
# [1] 63658    34

trips_notifications_2022_ah_6 <-
  trips_notifications_2022_ah_5_6 |>
  filter(NOTIFICATION_TYPE_IDs == '6')

dim(trips_notifications_2022_ah_6)
# [1] 63535    34
# [1] 67613    34

trips_notifications_2022_ah_6 |>
  count(NOTIFICATION_TYPE_ID)
# 1                    6 67613

# # rm extra cols ----
# t_names_to_rm <-
#   c("ACTIVITY_TYPE",
#     "ADDDITIONAL_FISHERMEN",
#     "APP_VERSION",
#     "APPROVAL_DATE",
#     "APPROVED_BY",
#     "BAIT_WEIGHT",
#     "CAPT_NAME_FIRST",
#     "CAPT_NAME_LAST",
#     "CF_ID",
#     "CF_ISS_AGENCY",
#     "CF_PERMIT_ID",
#     "CONFIRMATION_SIGNATURE",
#     "CONFIRMED_VALIDATING_AGENCY",
#     "COST_BAIT",
#     "COST_FOOD",
#     "COST_ICE",
#     "COST_IFQ",
#     "COST_LIGHT",
#     "COST_MISC",
#     "DAYS_AT_SEA",
#     "DC",
#     "DE",
#     "DEA_PERMIT_ID",
#     "END_PORT",
#     "EVENT_ID",
#     "FORM_VERSION",
#     "FUEL_DIESEL_GALLON_PRICE",
#     "FUEL_DIESEL_GALLONS",
#     "FUEL_GALLON_PRICE",
#     "FUEL_GALLONS",
#     "FUEL_GAS_GALLON_PRICE",
#     "FUEL_GAS_GALLONS",
#     "ICE_MAKER",
#     "NBR_OF_CREW",
#     "NBR_PAYING_PASSENGERS",
#     "NUM_ANGLERS",
#     "OWNER_ABOARD",
#     "PARTNER_VTR",
#     "PAY_PERCENT_TO_CAPT",
#     "PAY_PERCENT_TO_CREW",
#     "PAY_PERCENT_TO_OWNER",
#     "PAY_TO_CAPT_CREW",
#     "PORT",
#     "REPORTING_SOURCE",
#     "REVENUE_TOTAL",
#     "SEA_TIME",
#     "SPLIT_TRIP",
#     "START_PORT",
#     "STATE",
#     "STATUS",
#     "SUB_TRIP_TYPE",
#     "SUBMIT_METHOD",
#     "SUBMITTED_BY_PARTICIPANT",
#     "SUPPLIER_TRIP_ID",
#     "TICKET_TYPE",
#     "TRANSMISSION_DATE",
#     "TRIP_END_TIME",
#     "TRIP_FEE",
#     "TRIP_NBR",
#     "TRIP_START_TIME",
#     "UC",
#     "UE",
#     "VALIDATING_AGENCY",
#     "VENDOR_APP_NAME",
#     "VENDOR_PLATFORM",
#     "VTR_NUMBER")
#
# # print_df_names(trips_info_2022_short)
# # print_df_names(trip_neg_2022_short)
#
# trips_info_2022_short <-
#   trips_info_2022 |>
#   select(-any_of(t_names_to_rm)) |>
#   distinct()
#
# trip_neg_2022_short <-
#   trip_neg_2022 |>
#   select(-any_of(t_names_to_rm)) |>
#   distinct()
#
# trips_notifications_2022_short <-
#   trips_notifications_2022 |>
#   select(-any_of(t_names_to_rm)) |>
#   distinct()
#
# add week num ----
## to trips ----
# strftime(c("2022-05-27", "2022-05-28", "2022-05-29", "2022-05-30", "2022-05-31", "2022-06-01", "2022-06-04", "2022-06-05"), format = "%V")
# [1] "21" "21" "21" "22" "22" "22" "22" "22"
# >
#   > strftime(c("2022-05-27", "2022-05-28", "2022-05-29", "2022-05-30", "2022-05-31", "2022-06-01", "2022-06-04", "2022-06-05"), format = "%U")
# [1] "21" "21" "22" "22" "22" "22" "22" "23"

# grep(
#   "WEEK",
#   names(vessels__trips_22_l$sa_only),
#   ignore.case = T,
#   value = T
# )
# 0
# Yanet: For the weeks between 2 months, both months are affected by the non-compliant status.

trips_info_2022_int_ah_sero_w_y <-
  trips_info_2022_int_ah_sero %>%
  mutate(
    TRIP_START_week_num =
      strftime(TRIP_START_DATE, format = "%U"),
    TRIP_END_week_num =
      strftime(TRIP_END_DATE, format = "%U"),
    TRIP_START_y =
      year(TRIP_START_DATE),
    TRIP_END_y =
      year(TRIP_END_DATE),
    TRIP_START_m =
      zoo::as.yearmon(TRIP_START_DATE),
    TRIP_END_m =
      zoo::as.yearmon(TRIP_END_DATE)
  ) %>%
  mutate(
    TRIP_START_week_num =
      as.double(TRIP_START_week_num),
    TRIP_END_week_num =
      as.double(TRIP_END_week_num)
  )

trips_info_2022_int_ah_sero_w_y |>
  filter(TRIP_START_week_num == 0 &
           TRIP_START_m == "Jan 2022") |>
  dim()
# TRIP_START_m == "Jan 2022"
# [1] 104  15
# 77 15 (sero)
# 77 79
trips_info_2022_int_ah_sero_w_y |>
  filter(TRIP_START_week_num == 52 &
           TRIP_START_m == "Jan 2022") |>
  dim()
# [1] 80 79

## to trip notifications ----
trips_notifications_2022_ah_6_w_y <-
  trips_notifications_2022_ah_6 %>%
  mutate(
    TRIP_START_week_num =
      strftime(TRIP_START_DATE, format = "%U"),
    TRIP_END_week_num =
      strftime(TRIP_END_DATE, format = "%U"),
    TRIP_START_y =
      year(TRIP_START_DATE),
    TRIP_END_y =
      year(TRIP_END_DATE),
    TRIP_START_m =
      zoo::as.yearmon(TRIP_START_DATE),
    TRIP_END_m =
      zoo::as.yearmon(TRIP_END_DATE)
  ) %>%
  mutate(
    TRIP_START_week_num =
      as.double(TRIP_START_week_num),
    TRIP_END_week_num =
      as.double(TRIP_END_week_num)
  )

trips_notifications_2022_ah_6_w_y |>
  filter(TRIP_START_week_num == 0) |>
  dim()
# [1] 32 33
# [1] 32 40

trips_notifications_2022_ah_6_w_y |>
  filter(TRIP_START_week_num == 52) |>
  dim()
# [1] 1132   39
# [1] 1063   40 not cancelled

## to negative trips ----

tic("trip_neg_2022_w_y")
trip_neg_2022_w_y <-
  trip_neg_2022 %>%
  mutate(
    TRIP_week_num =
      strftime(TRIP_DATE, format = "%U"),
    TRIP_DATE_y =
      year(TRIP_DATE),
    TRIP_DATE_m =
      zoo::as.yearmon(TRIP_DATE)
  ) %>%
  mutate(TRIP_week_num =
           as.double(TRIP_week_num)) |>
  mutate()
toc()
# trip_neg_2022_w_y: 2.56 sec elapsed

# check
trip_neg_2022_w_y |>
  filter(TRIP_week_num == 0 & TRIP_DATE_m == "Jan 2022") |>
  dim()
# [1] 2101    15

trip_neg_2022_w_y |>
  filter(TRIP_week_num == 52 & TRIP_DATE_m == "Jan 2022") |>
  dim()
# [1] 2077

# results:
map_df(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   917     2066    6459
# 1   911     1781    3515
# 2    19       19      19

dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual)
# [1] 6207   19

dim(trips_info_2022_int_ah_sero_w_y)
# [1] 80967    79
dim(trip_neg_2022_w_y)
# [1] 747173      15
dim(trips_notifications_2022_ah_6_w_y)
# [1] 67738    39
# [1] 67613    40

# add all weeks to each df ----

#### check if there are earlier reports with an end date in 2022 and start in 2021 ----
trips_info_2022_int_ah_sero_w_y |>
  # filter(TRIP_START_y == 2021) |>
  filter(TRIP_START_week_num < 52 &
           TRIP_START_y == 2021 &
           TRIP_END_y == 2022) |>
  dim()
# 4 79

trips_notifications_2022_ah_6_w_y |>
  # filter(TRIP_START_y == 2021) |>
  filter(TRIP_START_week_num < 52 &
           TRIP_START_y == 2021 &
           TRIP_END_y == 2022) |>
  dim()
# 8

### adjust dates_2022 ----

dates_2022_yw0 <-
  dates_2022 |>
  mutate(date_y_m = as.yearmon(COMPLETE_DATE))

dates_2022_yw0 |>
  head(28) |>
  tail()
# 24 2021  12  52 2021-12-30 23:00:00 Dec 2021
# 25 2022   1  52 2021-12-31 23:00:00 Jan 2022!
# 26 2022   1  52 2022-01-01 23:00:00 Jan 2022!
# 27 2022   1  1  2022-01-03 23:00:00 Jan 2022
# 28 2022   1  1  2022-01-05 23:00:00 Jan 2022

# dates_2022_yw1 <-
#   dates_2022_yw0 |>
#   # remove all before the last week of 2021
#   filter(!(MONTH_OF_YEAR == 12 &
#              YEAR == 2021 &
#              WEEK_OF_YEAR < 52))

### rename 52 to 0, bc that's how %U works ?
dates_2022_yw <-
  dates_2022_yw0 |>
  mutate(WEEK_OF_YEAR =
    case_when(
      YEAR == 2022 &
      MONTH_OF_YEAR == 1 &
        date_y_m == "Jan 2022" &
        WEEK_OF_YEAR == 52
      ~ WEEK_OF_YEAR == 0,
      .default = WEEK_OF_YEAR
    )
  )

# check
trips_info_2022_int_ah_sero_w_y |>
  filter(TRIP_START_y %in% c("2021", "2022") &
           TRIP_START_m == "Jan 2022") |>
  select(TRIP_START_y,
         TRIP_START_m,
         TRIP_START_week_num) |>
  distinct() |>
  arrange(TRIP_START_y,
         TRIP_START_m,
         TRIP_START_week_num) |>
  head()
#   TRIP_START_y TRIP_START_m TRIP_START_week_num
# 1         2021     Jan 2022                  52
# 2         2022     Jan 2022                   0
# 3         2022     Jan 2022                   1
# 4         2022     Jan 2022                   2
# 5         2022     Jan 2022                   3
# 6         2022     Jan 2022                   4
# vs.
# dates:
# YEAR MONTH_OF_YEAR WEEK_OF_YEAR COMPLETE_DATE date_y_m
# 2021  12  52  2021-12-28 23:00:00 Dec 2021
# 2021  12  52  2021-12-30 23:00:00 Dec 2021
# 2022   1  52  2021-12-31 23:00:00 Jan 2022
# 2022   1  52  2022-01-01 23:00:00 Jan 2022
# 2022   1   1  2022-01-03 23:00:00 Jan 2022
# 2022   1   1  2022-01-05 23:00:00 Jan 2022

dim(dates_2022_yw)
# [1] 427   5

dates_2022_w <-
  dates_2022_yw |>
  select(-COMPLETE_DATE) |>
  distinct()

dim(dates_2022_w)
# [1] 74  4

# join with dates ----
## t by start & dates22 ----

t_dates_by <-
  join_by(YEAR == TRIP_START_y,
          date_y_m     == TRIP_START_m,
          WEEK_OF_YEAR == TRIP_START_week_num)

tic("t_d_w")
t_d_w <-
   full_join(
     dates_2022_w,
     trips_info_2022_int_ah_sero_w_y,
     t_dates_by,
     relationship = "many-to-many"
   )
toc()
# t_d_w: 0.48 sec elapsed

t_d_w |>
    filter(WEEK_OF_YEAR == 52) |>
  select(YEAR,
         MONTH_OF_YEAR,
         date_y_m) |>
  distinct() |>
  glimpse()
# [1] 7948   16
# $ YEAR          <dbl> 2021, 2022, 2022, 2023, 2021
# $ MONTH_OF_YEAR <dbl> 12, 1, 12, 1, NA
# $ date_y_m      <yearmon> Dec 2021, Jan 2022, Dec 2022, Jan 2023, Jan 2022
# Rows: 4
# $ YEAR          <dbl> 2021, 2022, 2023, 2021
# $ MONTH_OF_YEAR <dbl> 12, 12, 1, NA
# $ date_y_m      <yearmon> Dec 2021, Dec 2022, Jan 2023, Jan 2022

dim(t_d_w)
# [1] 627414     17
# [1] 626904     17
# TODO: check the difference, what was in the full_join? (no dates)
# [1] 523465     17 left_join
# [1] 523877     16 full_join
# [1] 80978    80

t_d_w |>
  filter(is.na(YEAR)) |>
  dim()
# 0

### tne ----
tne_dates_by <-
  join_by(YEAR == TRIP_DATE_y,
          date_y_m     == TRIP_DATE_m,
          WEEK_OF_YEAR == TRIP_week_num)

tic("tne_d_w")
tne_d_w <-
   full_join(
     dates_2022_w,
     trip_neg_2022_w_y,
     tne_dates_by,
     relationship = "many-to-many"
   )
toc()
# tne_d_w: 0.61 sec elapsed
dim(tne_d_w)
# [1] 747185      17

### tn by start ----

# tn_dates_by <-
#    join_by(date_y_m     == TRIP_START_m,
#            WEEK_OF_YEAR == TRIP_START_week_num
# )

tic("tn_d_w")
tn_d_w <-
   full_join(
     dates_2022_w,
     trips_notifications_2022_ah_6_w_y,
     t_dates_by,
     relationship = "many-to-many"
   )
toc()
# tn_d_w: 0.75 sec elapsed
dim(tn_d_w)
# [1] 62752    34
# [1] 67748    40
# [1] 67623    41 not cancelled

#### check for week 52 in Jan 22 ----
tn_d_w |>
    filter(date_y_m == "Jan 2022" &
               WEEK_OF_YEAR == 52) |>
  dim()
# [1] 142  40
# [1] 74 41 not canc

trips_notifications_2022_ah_6_w_y |>
    filter(TRIP_START_m == "Jan 2022",
               TRIP_START_week_num == 52) |>
  dim()
# [1] 142  39
# [1] 74 40 not canc

trips_info_2022_int_ah_sero_w_y |>
    filter(TRIP_START_m == "Jan 2022",
               TRIP_START_week_num == 52) |>
  dim()
# [1] 80 79

trip_neg_2022_w_y |>
    filter(TRIP_DATE_m == "Jan 2022" &
               TRIP_week_num == 52) |>
  dim()
# [1] 2077    15

### add weeks per permit 22 ----

v_p_d_w_22 <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  mutate(permit_weeks_amnt_22 =
           round(permit_2022_int / lubridate::dweeks(1)))

dim(v_p_d_w_22)
# [1] 6459   20
# [1] 9442   20
# [1] 6207   20

# Count distinct weeks per vessel ----

# ## neg trip weeks ----
#
# trip_neg_2022_w_y_cnt_u <-
#   trip_neg_2022_w_y |>
#   group_by(VESSEL_ID) %>%
#   mutate(distinct_weeks_ne = n_distinct(TRIP_week_num)) |>
#   ungroup()
#
# dim(trip_neg_2022_w_y_cnt_u)
# # [1] 1709    5
# # [1] 3414    2 summarize
# # [1] 747078     44 mutate
# # [1] 747173      7
#
# ## trip_notif weeks count per vessel ----
# trips_notifications_2022_ah_6_w_y_cnt_u <-
#   trips_notifications_2022_ah_6_w_y |>
#   group_by(VESSEL_ID) |>
#   mutate(
#     distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
#     distinct_end_weeks_tn = n_distinct(TRIP_END_week_num)
#   )
#
# dim(trips_notifications_2022_ah_6_w_y_cnt_u)
# # [1] 914   3 summarize
# # [1] 67738    35
#
# trips_notifications_2022_ah_6_w_y_cnt_u %>%
#    filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
#    dim()
# # [1] 0 6
# # ok
# # [1] 6318   35
#
# ## trips weeks count per vessel ----
# trips_info_2022_int_ah_sero_w_y_weeks_cnt_u <-
#   trips_info_2022_int_ah_sero_w_y %>%
#     group_by(VESSEL_ID) %>%
#       mutate(
#         distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
#         distinct_end_weeks_t = n_distinct(TRIP_END_week_num)
#       ) %>%
#       mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))
#
# dim(trips_info_2022_int_ah_sero_w_y_weeks_cnt_u)
# # [1] 1110    7
# # [1] 1934    4
# # [1] 1933    4 summarize
# # [1] 96990   110
# # [1] 97003    18
# # [1] 80967    18 sero
#
# trips_info_2022_int_ah_sero_w_y_weeks_cnt_u |>
#   filter(!distinct_start_weeks_t == distinct_end_weeks_t) |>
#   dim()
# # 2196 sero
#
# rm dates, leave w, m, y ----
## v_p ----

v_p_d_w_22_short <-
  v_p_d_w_22 |>
  select(
    VESSEL_VESSEL_ID,
    PERMIT_VESSEL_ID,
    permit_sa_gom_dual,
    # EFFECTIVE_DATE_y,
    # EFFECTIVE_DATE_m,
    # EFFECTIVE_DATE_week_num,
    # my_end_y,
    # my_end_m,
    # my_end_week_num,
    # eff_int,
    permit_2022_int,
    permit_weeks_amnt_22
  ) |>
  distinct()

dim(v_p_d_w_22_short)
# [1] 8939   12
# [1] 5554    5
# [1] 3654    5

## t_d ----
# remove fields
t_d_w_short <-
  t_d_w |>
  select(
    -c(
      # TRIP_START_DATE,
      # TRIP_END_DATE,
      TRIP_ID,
      # TRIP_TYPE,
      # SERO_VESSEL_PERMIT,
      GARFO_VESSEL_PERMIT,
      TRIP_TIME_ZONE,
      # trip_int
    )
  ) |>
  distinct() |>
  # add a table report type for future counting
  mutate(rep_type = "trips")

dim(t_d_w_short)
# [1] 97014    11
# [1] 38447     9 (no trip_id)
# [1] 37990     9
# [1] 32375     9
# [1] 32379     9
# [1] 80978    76

## tne_d ----

# print_df_names(tne_d_w)
tne_d_w_short <-
  tne_d_w |>
  select(-c(TRIP_DATE, TRIP_ID)) |>
  distinct() |>
  mutate(rep_type = "trips_neg")

dim(tne_d_w_short)
# [1] 136329      6
# [1] 136333      6
# [1] 230311     15

## tn_d ----
# remove fields
tn_d_w_short <-
  tn_d_w |>
  select(-c(
    ARRIVAL_PORT,
    CANCEL_FLAG,
    DEA_PERMIT_SOLD_NOTIFICATION,
    DEPARTURE_PORT,
    EMAIL_SENT,
    GEAR_NOTIFICATION,
    # INTENDED_FISHING_FLAG,
    LANDING_LOCATION_CITY,
    LANDING_LOCATION_COUNTY,
    LANDING_LOCATION_NAME,
    LANDING_LOCATION_STATE,
    LANDING_LOCATION,
    NOTIFICATION_SEQ,
    NOTIFICATION_TIME_ZONE,
    NOTIFICATION_TYPE_ID,
    PERMIT_ID,
    PROCESSED_TIMESTAMP,
    PROCESSED,
    RAW_INPUT_ID,
    STAT_ZONE,
    SYSTEM_ID,
    # TRIP_END_DATE,
    TRIP_ID
    # ,
    # TRIP_START_DATE_TIME,
    # TRIP_START_DATE,
    # TRIP_TYPE
  )
  ) |>
  distinct() |>
  # add a table report type for future counting
  mutate(rep_type = "trips_notif")

dim(tn_d_w_short)
# [1] 21211    10
# [1] 21179     9 (no permit_id)
# [1] 20466     9
# [1] 66710    19
# [1] 66585    21 not canc

# join with dates_22 by week ----
## t & tne ----

tic("t__tne_d_weeks")
t__tne_d_weeks <-
  full_join(
    t_d_w_short,
    tne_d_w_short,
    join_by(YEAR,
            MONTH_OF_YEAR,
            WEEK_OF_YEAR,
            date_y_m,
            VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".t", ".tne")
  )
toc()
dim(t__tne_d_weeks)
# [1] 160791     10
# [1] 160795     10
# [1] 314492     88

## t & tn ----
intersect(names(t_d_w_short),
          names(tn_d_w_short)) |>
  paste(collapse = ", ")
# YEAR, MONTH_OF_YEAR, WEEK_OF_YEAR, date_y_m, TRIP_TYPE, DE, UE, DC, UC, VESSEL_ID, TRIP_START_DATE, TRIP_END_DATE, TRIP_END_TIME, TRIP_START_TIME, TRIP_END_week_num, TRIP_END_y, TRIP_END_m, rep_type

t__tn_join_by <- join_by(
  YEAR,
  MONTH_OF_YEAR,
  WEEK_OF_YEAR,
  date_y_m,
  TRIP_TYPE,
  VESSEL_ID,
  TRIP_START_DATE,
  TRIP_END_DATE,
  TRIP_END_week_num,
  TRIP_END_y,
  TRIP_END_m
)

tic("t__tn_d_weeks")
t__tn_d_weeks <-
  full_join(
    t_d_w_short,
    tn_d_w_short,
    t__tn_join_by,
    relationship = "many-to-many",
    suffix = c(".t", ".tn")
  )
toc()

dim(t_d_w_short)[1] +
dim(tn_d_w_short)[1]
# [1] 147688
# 147563
dim(t__tn_d_weeks)
# [1] 120876     86
# [1] 120754     88

length(unique(t__tn_d_weeks$WEEK_OF_YEAR))
# 53
length(unique(t__tn_d_weeks$VESSEL_ID))
# 1991

## t_tne & v_p ----

tic("v_p__t__tne_d_weeks")
v_p__t__tne_d_weeks <-
  full_join(
    v_p_d_w_22_short,
    t__tne_d_weeks,
    join_by(VESSEL_VESSEL_ID == VESSEL_ID),
    relationship = "many-to-many"
  )
toc()

dim(v_p__t__tne_d_weeks)
# [1] 165834     14
# [1] 165838     14
# [1] 320962     92

# VESSEL_VESSEL_ID     6913
# PERMIT_VESSEL_ID     5462
# permit_sa_gom_dual      4
# permit_2022_int       357
# permit_weeks_amnt_22   54
# YEAR                    4
# MONTH_OF_YEAR          13
# WEEK_OF_YEAR           54
# date_y_m               18

v_p__t__tne_d_weeks |>
  filter(VESSEL_VESSEL_ID == "248316") |>
  dim()
# [1] 58 14 correct
# [1] 77 92

## t_tn & v_p ----
tic("v_p__t__tn_d_weeks")
v_p__t__tn_d_weeks <-
  full_join(
    v_p_d_w_22_short,
    t__tn_d_weeks,
    join_by(VESSEL_VESSEL_ID == VESSEL_ID),
    relationship = "many-to-many"
  )
toc()

dim(v_p__t__tn_d_weeks)
# [1] 46329    18
# [1] 45530    17
# [1] 128369     90
# [1] 128243     92

### check ----
# 1)
v_p__t__tne_d_weeks |>
  filter(PERMIT_VESSEL_ID == "VI5498TB") |>
  count(YEAR) |>
  glimpse()
# now has TRIP_DATE_y
# 58
# $ TRIP_DATE_y <dbl> 2021, 2022
# $ n           <int> 1, 57
# ok
# $ n           <int> 1, 76


# 2) check change of year weeks
v_p__t__tne_d_weeks_21 <-
  v_p__t__tne_d_weeks |>
  # filter(date_y_m %within% permit_2022_int)
  # exclude the last weeks of 2021 before 52
  filter(date_y_m == 'Dec 2021' &
             WEEK_OF_YEAR < 52 &
             is.na(rep_type.t) &
             is.na(rep_type.tne)
         )

dim(v_p__t__tne_d_weeks_21)
# 0 (change 52/1 0)
# ok

# Run to get SA compliance
# source(file.path(my_paths$git_r,
#                  current_project_name,
#                  "sa_22_db_compliance.R"))

# GOM + dual compl by year ----
# GOM must have both (1) a logbook for any fishing intended declaration, and (2) an intended fishing declaration for any logbook

# There should be a declaration for every logbook.
# There should be a logbook for every declaration of a charter or headboat intending to fish.
# Noncompliant + overridden are compliant

## get gom and dual vsls ----
v_p__t__tn_d_weeks_gom <-
  v_p__t__tn_d_weeks |>
  filter(permit_sa_gom_dual %in% c("gom_only", "dual"))

dim(v_p__t__tn_d_weeks_gom)
# [1] 75524    91
# [1] 75403    92

## check activity type ----
v_p__t__tn_d_weeks_gom |>
  count(ACTIVITY_TYPE, INTENDED_FISHING_FLAG)
#   ACTIVITY_TYPE     n
#           <dbl> <int>
# 1             0 45834
# 2             3     1
# 3            80   488
# 4            81     2
# 5            NA 29199
# 0, 'TRIP WITH EFFORT',
# 80, 'TRIP UNABLE TO FISH',
# 81, 'TRIP NO INTENTION OF FISHING'

#    ACTIVITY_TYPE INTENDED_FISHING_FLAG     n
#            <dbl> <chr>                 <int>
#  1             0 N                       488
#  2             0 Y                     43375
#  3             0 NA                     1971
#  4             3 Y                         1
#  5            80 N                        21
#  6            80 Y                       425
#  7            80 NA                       42
#  8            81 N                         1
#  9            81 Y                         1
# 10            NA N                      3562
# 11            NA Y                     24375
# 12            NA NA                     1262

v_p__t__tn_d_weeks_gom |>
  filter(ACTIVITY_TYPE == "3") |>
  # head(2) |>
  # select(all_of(starts_with("UE"))) |>
  glimpse()
# $ UE.t  <chr> "KCSPORTFISHING                "
# $ UE.tn <chr> "KCSPORTFISHING"
# $ VESSEL_VESSEL_ID            <dbl> 328032
# $ PERMIT_VESSEL_ID            <chr> "FL9452SM"

v_p__t__tn_d_weeks_gom |>
  filter(ACTIVITY_TYPE == "81") |>
  select(PERMIT_VESSEL_ID,
         ACTIVITY_TYPE,
         INTENDED_FISHING_FLAG,
         all_of(starts_with("rep_type"))) |>
  glimpse()
# $ PERMIT_VESSEL_ID      <chr> "1114447", "FL6430PK"
# $ ACTIVITY_TYPE         <dbl> 81, 81
# $ INTENDED_FISHING_FLAG <chr> "Y", "N"
# $ rep_type.t            <chr> "trips", "trips"
# $ rep_type.tn           <chr> "trips_notif", "trips_notif"

# TODO: check for INTENDED_FISHING_FLAG again

## rm extra cols ----
### find empty columns ----
names(v_p__t__tn_d_weeks_gom) |>
  length()
# 92

empty_cols <-
  v_p__t__tn_d_weeks_gom |>
  map_df(function(x) {
    if (length(unique(x)) == 1) {
      return(unique(x))
    }
  })
# dim(empty_cols)[2]
# 32

t_names_to_rm <-
  c("ADDDITIONAL_FISHERMEN",
    "APP_VERSION",
    "APPROVAL_DATE",
    "APPROVED_BY",
    "BAIT_WEIGHT",
    "CANCEL_FLAG",
    "CAPT_NAME_FIRST",
    "CAPT_NAME_LAST",
    "CF_ID",
    "CF_ISS_AGENCY",
    "CF_PERMIT_ID",
    "CONFIRMATION_SIGNATURE",
    "CONFIRMED_VALIDATING_AGENCY",
    "COST_BAIT",
    "COST_FOOD",
    "COST_ICE",
    "COST_IFQ",
    "COST_LIGHT",
    "COST_MISC",
    "DAYS_AT_SEA",
    "DC",
    "DE",
    "DEA_PERMIT_ID",
    "END_PORT",
    "FORM_VERSION",
    "FUEL_DIESEL_GALLON_PRICE",
    "FUEL_DIESEL_GALLONS",
    "FUEL_GALLON_PRICE",
    "FUEL_GALLONS",
    "FUEL_GAS_GALLON_PRICE",
    "FUEL_GAS_GALLONS",
    "ICE_MAKER",
    "NBR_OF_CREW",
    "NBR_PAYING_PASSENGERS",
    "NUM_ANGLERS",
    "OWNER_ABOARD",
    "PARTNER_VTR",
    "PAY_PERCENT_TO_CAPT",
    "PAY_PERCENT_TO_CREW",
    "PAY_PERCENT_TO_OWNER",
    "PAY_TO_CAPT_CREW",
    "PORT",
    "REPORTING_SOURCE",
    "REVENUE_TOTAL",
    "SEA_TIME",
    "SPLIT_TRIP",
    "START_PORT",
    "STATE",
    "SUB_TRIP_TYPE",
    "SUBMITTED_BY_PARTICIPANT",
    "SUPPLIER_TRIP_ID",
    "TICKET_TYPE",
    "TRANSMISSION_DATE",
    "TRIP_FEE",
    "TRIP_NBR",
    "TRIP_START_DATE_TIME",
    "UC",
    "UE",
    "VALIDATING_AGENCY",
    "VENDOR_APP_NAME",
    "VENDOR_PLATFORM",
    "VTR_NUMBER"
  )

# do not rm:
# "ACTIVITY_TYPE",
# "EVENT_ID",
# "STATUS",
# "SUBMIT_METHOD",
# "TRIP_END_TIME",
# "TRIP_START_TIME",

v_p__t__tn_d_weeks_gom_short <-
  v_p__t__tn_d_weeks_gom |>
  select(-any_of(t_names_to_rm)) |>
  distinct()

dim(v_p__t__tn_d_weeks_gom)
# [1] 75524    91
# [1] 75403    92

dim(v_p__t__tn_d_weeks_gom_short)
# [1] 75524    35
# [1] 75403    36

data_overview(v_p__t__tn_d_weeks_gom_short) |>
  head(2)
# VESSEL_VESSEL_ID      1351
# PERMIT_VESSEL_ID      1351

v_p__t__tn_d_weeks_gom_short |>
  count(permit_sa_gom_dual)
# 1 dual               15875
# 2 gom_only           59649
# 1 dual               15853
# 2 gom_only           59550

v_p__t__tn_d_weeks_gom_short |>
  count(TRIP_TYPE)
#   TRIP_TYPE     n
#   <chr>     <int>
# 1 A         63121
# 2 H         11935
# 3 NA          468
# 1 A         63009
# 2 H         11926
# 3 NA          468
# Activity type NA - declaration only

v_p__t__tn_d_weeks_gom_short |>
  count(INTENDED_FISHING_FLAG)

# by <- join_by(unique_all_vessel_ids,
# #               overlaps(x$EFFECTIVE_DATE,
# #                        x$my_end_date,
# #                        y$EFFECTIVE_DATE,
# #                        y$my_end_date,
# #                        bounds = "[)"))

length(unique(v_p__t__tn_d_weeks_gom$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     1351

# TODO: check if permit_id in tn mean the same as in p_v

## wrong TRIP_START_TIME.tn ----
# ℹ In argument: `TRIP_START_TIME_tn_hm = parse_date_time(TRIP_START_TIME.tn,
#   "HM")`.
# Caused by warning:
# !  2 failed to parse.

v_p__t__tn_d_weeks_gom_short |>
  select(TRIP_START_TIME.tn) |>
  distinct() |>
  arrange(desc(TRIP_START_TIME.tn)) |>
  rowwise() |>
  head(3) |>
  # count number of digits, should be 4
  # mutate(l = floor(log10(as.numeric(TRIP_START_TIME.tn))) + 1)
  mutate(l = nchar(TRIP_START_TIME.tn)) |>
  ungroup()
#   <chr>              <int>
# 1 601                    3
# 2 600                    3
# 3 2359                   4

v_p__t__tn_d_weeks_gom_short |>
  filter(!(nchar(TRIP_START_TIME.tn) == 4) |
           !(nchar(TRIP_START_TIME.t) == 4)
         ) |>
  glimpse()
# $ TRIP_START_TIME.t    <chr> "0700", "0601", NA
# $ TRIP_START_TIME.tn   <chr> "601", "601", "600"

# assume the missing leading zero, can't be 6010 etc.
#### restore ----
v_p__t__tn_d_weeks_gom_short <-
  v_p__t__tn_d_weeks_gom_short |>
  mutate(TRIP_START_TIME.tn =
           case_when(
           nchar(TRIP_START_TIME.tn) < 4 ~
           paste0("0", TRIP_START_TIME.tn),
         .default = TRIP_START_TIME.tn
           )
         )

## match logbooks and declarations ----

# There should be a declaration for every logbook. - no
# There should be a logbook for every declaration of a charter or headboat intending to fish. - yes
# decl trip start < or > 1h logbooks trip start

  # mutate(isFilter = case_when(Time == "T0" & Value > 5 ~ 1, TRUE ~ 0)) %>%
# df <- df %>% group_by(levelled) %>%
# filter(any(direction == "down"))

### at least one pair of matching declarations per week ----
# $ rep_type.t            <chr> "trips", "trips"
# $ rep_type.tn           <chr> "trips_notif", "trips_notif"

filter_logb_for_decl_fish <- rlang::quo(
  rep_type.tn == "trips_notif" &
  INTENDED_FISHING_FLAG == 'Y'
  &
  rep_type.t == "trips"
)

v_p__t__tn_d_weeks_gom_short |>
  filter(!!filter_logb_for_decl_fish) |>
  dim()
# [1] 43802    35
# [1] 43767    36

#### decl trip start < or > 1h logbooks trip start ----

tic("v_p__t__tn_d_weeks_gom_short_matched")
v_p__t__tn_d_weeks_gom_short_matched <-
  v_p__t__tn_d_weeks_gom_short |>
  group_by(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |>
  # convert time to a Date format
  mutate(
    TRIP_START_TIME_t_hm =
      parse_date_time(TRIP_START_TIME.t, "HM"),
    TRIP_START_TIME_tn_hm =
      parse_date_time(TRIP_START_TIME.tn, "HM")
  ) |>
  # count the difference between start times t and tn
  mutate(time_diff1 = abs(TRIP_START_TIME_t_hm - (TRIP_START_TIME_tn_hm))) |>
  # less than an hour difference between trip and trip notif start time
  mutate(matched_reports =
           case_when(time_diff1 < 3600 ~ "matched",
                     .default = "not_matched"
           )) |>
  distinct() |>
  ungroup()
toc()

dim(v_p__t__tn_d_weeks_gom_short_matched)
# [1] 35662    41
# [1] 35664    38 filter with restored time
# [1] 44312    39 add matched col
# [1] 75524    39 without filter out not matched
# [1] 75403    40

v_p__t__tn_d_weeks_gom_short_matched |>
  count(matched_reports)
# 1 matched         35664
# 2 not_matched     39860
# 1 matched         35641
# 2 not_matched     39762

v_p__t__tn_d_weeks_gom_short_matched |>
  select(PERMIT_VESSEL_ID, matched_reports) |>
  distinct() |>
  count(matched_reports)
# 1 matched           595
# 2 not_matched      1300

length(unique(v_p__t__tn_d_weeks_gom_short_matched$PERMIT_VESSEL_ID))
# [1] 1351

v_p__t__tn_d_weeks_gom_short_matched |>
  filter(rep_type.tn == "trips_notif" &
           INTENDED_FISHING_FLAG == 'Y' &
             matched_reports == "not_matched" &
             !is.na(rep_type.t)) |>
  head() |>
  dim()
# 6 40

## strict compl vessels per week ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w")
v_p__t__tn_d_weeks_gom_short_matched_compl_w <-
  v_p__t__tn_d_weeks_gom_short_matched |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(is_compliant_w =
           case_when(any(matched_reports == "matched") ~
                       "yes",
                     .default = "no")) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w: 5.39 sec elapsed

# filter(PERMIT_VESSEL_ID %in% c("FL4459MW", "FL4459PW")) |>

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  count(INTENDED_FISHING_FLAG)
# 1 N                      4072
# 2 Y                     68177
# 3 NA                     3275

  # INTENDED_FISHING_FLAG is_compliant_w     n
# 1 N                     no              2739
# 2 N                     yes             1333
# 3 Y                     no             23597
# 4 Y                     yes            44580
# 5 NA                    no              2917
# 6 NA                    yes              358

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  count(# VESSEL_VESSEL_ID,
    #     PERMIT_VESSEL_ID,
    # WEEK_OF_YEAR,
    date_y_m,
    INTENDED_FISHING_FLAG,
    is_compliant_w) |>
  glimpse()
  # write_csv("compliant_fishing_month.csv")

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         matched_reports,
         INTENDED_FISHING_FLAG,
         is_compliant_w) |>
  distinct() |>
  group_by(date_y_m,
           matched_reports,
           INTENDED_FISHING_FLAG,
           is_compliant_w) |>
  mutate(cnt_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  glimpse()
# disregard not_matched & yes, that means there are more than 1 decl

### a good example of 2 decl per week with compliant ----
v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  filter(date_y_m == "Feb 2022") |>
         # ,
         # matched_reports == "not_matched"
         # INTENDED_FISHING_FLAG == "N"
         # ,
         # is_compliant_w == "yes") |>
         filter(PERMIT_VESSEL_ID == "1093374") |>
           select(
             VESSEL_VESSEL_ID,
             PERMIT_VESSEL_ID,
             WEEK_OF_YEAR,
             date_y_m,
             TRIP_TYPE,
             TRIP_START_DATE,
             TRIP_START_TIME.t,
             ACTIVITY_TYPE,
             SERO_VESSEL_PERMIT,
             trip_int,
             TRIP_START_TIME.tn,
             INTENDED_FISHING_FLAG,
             time_diff1,
             matched_reports,
             is_compliant_w
           ) |>
           distinct() |>
           View()

v_p__t__tn_d_weeks_gom_short_matched_compl_w_cnt_vsls_w <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         WEEK_OF_YEAR,
         is_compliant_w) |>
  distinct() |>
  group_by(date_y_m,
           WEEK_OF_YEAR,
           is_compliant_w) |>
  mutate(cnt_vsls =
           case_when(
             any(is_compliant_w == "yes") ~
               n_distinct(PERMIT_VESSEL_ID),
             .default = 0
           )) |>
  # n_distinct(PERMIT_VESSEL_ID)) |>
  select(-PERMIT_VESSEL_ID) |>
  distinct()
# disregard not_matched & yes, that means there are more than 1 decl

glimpse(v_p__t__tn_d_weeks_gom_short_matched_compl_w_cnt_vsls_w)

# subset(df, levelled %in% levelled[direction == 'down'])

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         WEEK_OF_YEAR,
         is_compliant_w) |>
  distinct() |>
  group_by(date_y_m,
           WEEK_OF_YEAR,
           is_compliant_w) |>
  filter(date_y_m == 'Mar 2022') |>
  subset(PERMIT_VESSEL_ID %in% PERMIT_VESSEL_ID[is_compliant_w == 'yes']) |>
  mutate(cnt_c1 = n_distinct(PERMIT_VESSEL_ID)) |>
  ungroup() |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  View()

# no matched declarations, but compliant? ----
# There should be a logbook for every declaration of a charter or a headboat intending to fish.

# print_df_names(v_p__t__tn_d_weeks_gom_short_matched_compl_w)

print_df_names(v_p__t__tn_d_weeks_gom_short_matched_compl_w)

## There are only a not matched not fishing intended declarations per week ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_2")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(not_fish_compl =
           case_when(
             is_compliant_w == "no" &
               # all reports are not fishing declarations
               all(INTENDED_FISHING_FLAG == "N" &
                     !is.na(rep_type.tn)) ~ "yes",
             .default = "no"
           )) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_2: 8.93 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  count(not_fish_compl)
# 1 no             73009
# 2 yes             2394

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  count(not_fish_compl)

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  # filter(not_fish_compl == "yes") |>
  filter(PERMIT_VESSEL_ID == "FL4459PW") |>
  arrange(WEEK_OF_YEAR) |>
  glimpse()

## a week with no reports ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_3")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
    no_rep_compl =
      case_when(
        is_compliant_w == "no" &
          not_fish_compl == "no" &
          # no reports
          all(is.na(rep_type.t)) &
          all(is.na(rep_type.tn)) ~ "yes",
        .default = "no"
      )
  ) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_3: 7.41 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  count(no_rep_compl)
# 1 no           74935
# 2 yes            468

#   filter(PERMIT_VESSEL_ID == "FL3627NN") |>
  # View()
#
# > v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
#   filter(PERMIT_VESSEL_ID == "FL3627NN") |>
#   View()
# > v_p__t__tn_d_weeks |>
#   filter(PERMIT_VESSEL_ID == "FL3627NN") |>
#   View()
# > trips_info_2022 |>
#   filter(VESSEL_ID == "330659") |>
#   View()
# 0
# > trips_notifications_2022 |>
#   filter(VESSEL_ID == "330659") |>
#   View()
# 0

## a week with a logb and no decl ----
# err, but is compliant

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
#   filter(VESSEL_VESSEL_ID == 72359,
#          PERMIT_VESSEL_ID == "933533")

tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_4")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
    cnt_t  = sum(!is.na(rep_type.t)),
    cnt_tn = sum(!is.na(rep_type.tn)),
    no_decl_compl =
      case_when(
        is_compliant_w == "no" &
          not_fish_compl == "no" &
          no_rep_compl == "no" &
          # no decl for a lgb
          cnt_t > cnt_tn ~ "yes",
        .default = "no"
      )
  ) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4: 11.17 sec elapsed
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4: 16.42 sec elapsed


v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |> 
  count(no_decl_compl)
# 1 no            75400 (w 42)
# 2 yes               3 (w 25)
# 1 no            73790
# 2 yes            1613 (not NA)


### the same in a different way ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
    # cnt_t  = sum(!is.na(rep_type.t)),
    # cnt_tn = sum(!is.na(rep_type.tn)),
    no_decl_compl =
      case_when(
        is_compliant_w == "no" &
          not_fish_compl == "no" &
          no_rep_compl == "no" &
          # no decl for a lgb
          !is.na(rep_type.t) &
          is.na(rep_type.tn)
          # cnt_t > cnt_tn 
        ~ "yes",
        .default = "no"
      )
  ) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a: 5.85 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a |> 
  count(no_decl_compl)
# 1 no            73738
# 2 yes            1665

### TODO find the differnece between 4 and 4a ----
v_yes <-
  list(
    v_p__t__tn_d_weeks_gom_short_matched_compl_w_4,
    v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a
  ) |>
  map(function(curr_df) {
    # browser()
    curr_df |>
      filter(no_decl_compl == "yes") |>
      select(PERMIT_VESSEL_ID) |>
      distinct() %>%
      return()
  })

str(v_yes)
 # $ : tibble [82 × 1] (S3: tbl_df/tbl/data.frame)
 #  ..$ PERMIT_VESSEL_ID: chr [1:82] "FL4482RC" "1162015" "1162266" "FL6786PB" ...
 # $ : tibble [115 × 1] (S3: tbl_df/tbl/data.frame)
 #  ..$ PERMIT_VESSEL_ID: chr [1:115] "FL4482RC" "FL3326MC" "1271163" "1271879" ...

in4 <-
  setdiff(v_yes[[1]]$PERMIT_VESSEL_ID,
          v_yes[[2]]$PERMIT_VESSEL_ID)
length(in4)
# 0

in4a <-
  setdiff(v_yes[[2]]$PERMIT_VESSEL_ID,
          v_yes[[1]]$PERMIT_VESSEL_ID)

length(in4a)
# 33
#  [1] "FL3326MC" "1271163"  "1271879"  "1174344"  "1211890"  "1174689" 
#  [7] "1109181"  "1087799"  "1217823"  "1264525"  "FL6017NC" "LA2117GM"
# [13] "FL8511RT" "FL9207ST" "FL8845ML" "FL1640RJ" "FL0957RW" "AL0364RL"
# [19] "AL1172RG" "1076615"  "FL1383SN" "1023529"  "1310120"  "1313218" 
# [25] "1299573"  "980844"   "FL2564MC" "FL1751SN" "FL2305NU" "FL1909RG"
# [31] "573252"   "555530"   "592015"  

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a |> 
#   filter(PERMIT_VESSEL_ID %in%
#            v_yes[[2]]$PERMIT_VESSEL_ID) |> 
#   View()

FL4482RC_4 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  filter(PERMIT_VESSEL_ID == "FL3326MC") |> 
  select(-c(cnt_t, cnt_tn))

FL4482RC_4a <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a |>
  filter(PERMIT_VESSEL_ID == "FL3326MC")

all.equal(FL4482RC_4, FL4482RC_4a)
# [1] "Component “no_decl_compl”: 1 string mismatch"

# library(arsenal)
# summary(comparedf(FL4482RC_4, FL4482RC_4a))
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'object' in selecting a method for function 'summary': Incompatible classes: <numeric> + <Interval>

library(diffdf)
diffdf(FL4482RC_4, FL4482RC_4a)
# All rows are shown in table below

  # =============================================
  #    VARIABLE     ..ROWNUMBER..  BASE  COMPARE 
  # ---------------------------------------------
  #  no_decl_compl       37         no     yes   
  # ---------------------------------------------

glimpse(FL4482RC_4[37,])
glimpse(FL4482RC_4a[37,])

v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  filter(WEEK_OF_YEAR == 26,
         PERMIT_VESSEL_ID == "FL3326MC") |> 
  glimpse()
# $ cnt_t                 <int> 1, 1
# $ cnt_tn                <int> 1, 1
# 2 entries, 1 has t, another has a tn!


# no report, but a decl is a "no fish" - compl ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_5")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
    no_lgb_compl =
      case_when(
        is_compliant_w == "no" &
          not_fish_compl == "no" &
          no_rep_compl == "no" &
          INTENDED_FISHING_FLAG == "N" &
          # no lgb for a not fish decl
          is.na(rep_type.t) &
          !is.na(rep_type.tn)
        # there is a decl!is.na(rep_type.tn)
        ~ "yes",
        .default = "no"
      )
  ) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 13.05 sec elapsed
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 3032.68 sec elapsed rowwise

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |> 
  count(no_lgb_compl)
# 1 no           75060
# 2 yes            343

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |> 
  arrange(desc(no_lgb_compl)) |> 
  glimpse()

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  arrange(desc(no_lgb_compl)) |>
  filter(no_decl_compl == "yes" & no_lgb_compl == "yes") |>
  count(unique(PERMIT_VESSEL_ID))
# # A tibble: 1 × 2
#   `unique(PERMIT_VESSEL_ID)`     n
#   <chr>                      <int>
# 1 FL3495RT                       5

