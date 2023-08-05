# db_compliance
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

dim(trips_notifications_2022_ah_5_6)
# [1] 63658    34

trips_notifications_2022_ah_6 <-
  trips_notifications_2022_ah_5_6 |>
  filter(NOTIFICATION_TYPE_IDs == '6')

dim(trips_notifications_2022_ah_6)
# [1] 63535    34

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
# [1] 80 15

## to trip notifications ----
trips_notifications_2022_ah_w_y <-
  trips_notifications_2022_ah %>%
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

trips_notifications_2022_ah_w_y |>
  filter(TRIP_START_week_num == 0) |>
  dim()
# [1] 32 33
# [1] 32 39

trips_notifications_2022_ah_w_y |>
  filter(TRIP_START_week_num == 52) |>
  dim()
# [1] 1132   39

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
dim(trips_notifications_2022_ah_w_y)
# [1] 67738    39

# add all weeks to each df ----

#### check if there are earlier reports with an end date in 2022 and start in 2021 ----
trips_info_2022_int_ah_sero_w_y |>
  # filter(TRIP_START_y == 2021) |>
  filter(TRIP_START_week_num < 52 &
           TRIP_START_y == 2021 &
           TRIP_END_y == 2022) |>
  dim()
# 4

trips_notifications_2022_ah_w_y |>
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
     trips_notifications_2022_ah_w_y,
     t_dates_by,
     relationship = "many-to-many"
   )
toc()
# tn_d_w: 0.75 sec elapsed
dim(tn_d_w)
# [1] 62752    34
# [1] 67748    40

#### check for week 52 in Jan 22 ----
tn_d_w |>
    filter(date_y_m == "Jan 2022" &
               WEEK_OF_YEAR == 52) |>
  dim()
# [1] 142  40

trips_notifications_2022_ah_w_y |>
    filter(TRIP_START_m == "Jan 2022",
               TRIP_START_week_num == 52) |>
  dim()
# [1] 142  39

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
# trips_notifications_2022_ah_w_y_cnt_u <-
#   trips_notifications_2022_ah_w_y |>
#   group_by(VESSEL_ID) |>
#   mutate(
#     distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
#     distinct_end_weeks_tn = n_distinct(TRIP_END_week_num)
#   )
#
# dim(trips_notifications_2022_ah_w_y_cnt_u)
# # [1] 914   3 summarize
# # [1] 67738    35
#
# trips_notifications_2022_ah_w_y_cnt_u %>%
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
dim(t__tn_d_weeks)
# [1] 120876     86

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

# # SA 2022 compliance ----
# # There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# # DNFs should not be submitted more than 30 days in advance
#
# # all weeks of 2022 * all vessels
# # SA: each can have:
# # 1) a permit
# # 2) a trip
# # 3) a negative report
# # 1 only
# # 1,2
# # 1,3
# # 2 only
# # 3 only
# # 2,3?

# SA compliance by year ----
## get sa only vsls ----
v_p__t__tne_d_weeks_sa <-
  v_p__t__tne_d_weeks |>
  filter(permit_sa_gom_dual == "sa_only")
dim(v_p__t__tne_d_weeks_sa)
# [1] 90766    15
# [1] 194697     92

## reports_exists filter ----
reports_exists_filter <- rlang::quo(
  !(is.na(rep_type.t) & is.na(rep_type.tne))
)

## mark weekly compliance ----
tic("v_p__t__tne_d_weeks_sa_compl")
v_p__t__tne_d_weeks_sa_compl_w <-
  v_p__t__tne_d_weeks_sa |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m,
           YEAR) |>
  # not compliant if both reports (trips and t negative) are absent
  mutate(sa_compl_week = case_when(!!reports_exists_filter ~
                                "yes",
                              .default = "no")) |>
  ungroup()
toc()
# v_p__t__tne_d_weeks_sa_compl: 28.39 sec elapsed
# v_p__t__tne_d_weeks_sa_compl: 22.39 sec elapsed
# v_p__t__tne_d_weeks_sa_compl_w: 33.11 sec elapsed

dim(v_p__t__tne_d_weeks_sa_compl_w)
# [1] 90766    16
# [1] 194697     93

## count compl weeks ----
# Do not group by year, the last week of 2021 should be counted together with 2022

v_p__t__tne_d_weeks_sa_compl_cnt_w <-
  v_p__t__tne_d_weeks_sa_compl_w |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID) |>
  mutate(compl_w_cnt = n_distinct(WEEK_OF_YEAR)) |>
  ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w)
# [1] 90766    16
# [1] 194697     94

### check compl week count ----
v_p__t__tne_d_weeks_sa_compl_cnt_w |>
  filter(PERMIT_VESSEL_ID == "FL4430NN") |>
  select(WEEK_OF_YEAR, date_y_m, all_of(starts_with("rep_type")), compl_w_cnt) |>
    distinct() |>
    View()
# 14 distinct weeks
# 17 rows bc some weeks are in 2 month, e.g. 48 in Nov 2022 and Dec 2022

## compliance per year ----
v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w |>
  mutate(compl_2022 =
           case_when(
    !!reports_exists_filter &
      compl_w_cnt >= permit_weeks_amnt_22 ~ "yes",
           .default = "no")
  ) |>
  ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22)
# [1] 90766    17
# [1] 194697     95

v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 |> 
  count(ACTIVITY_TYPE)
# 1             0  64007
# 2             2      7
# 3             8     22
# 4            80    229
# 5            NA 130432

# v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 |>
#   # select(PERMIT_VESSEL_ID, ACTIVITY_TYPE, all_of(starts_with("UE"))) |>
#   select(PERMIT_VESSEL_ID, ACTIVITY_TYPE, UE.t) |>
#   distinct() |>
#   filter(ACTIVITY_TYPE %in% c("2", "8")) |>
#   head(10)

### fewer columns ----
v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 |>
  select(
    PERMIT_VESSEL_ID,
    permit_2022_int,
    permit_weeks_amnt_22,
    YEAR,
    compl_w_cnt,
    compl_2022,
    rep_type.t,
    rep_type.tne
  ) |>
  distinct()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short)
# [1] 5275    6
# [1] 6627    8
# [1] 4934    8 (metrics vsls)

v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |>
  filter(compl_2022 == "yes") |>
  head() |>
  glimpse()
# $ PERMIT_VESSEL_ID     <chr> "FL4430NN", "FL2698TE", "FL2698TE", "FL2720R…
# $ permit_2022_int      <Interval> 2022-10-11 00:00:00 EDT--2022-12-30 19:…
# $ permit_weeks_amnt_22 <dbl> 12, 32, 32, 52, 52, 52
# $ YEAR                 <dbl> 2022, 2022, 2021, 2022, 2021, 2022
# $ compl_w_cnt          <int> 14, 53, 53, 53, 53, 52
# $ compl_2022           <chr> "yes", "yes", "yes", "yes", "yes", "yes"
# $ rep_type.t           <chr> NA, NA, NA, NA, NA, "trips"
# $ rep_type.tne         <chr> "trips_neg", "trips_neg", "trips_neg", "trip…


v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |>
filter(PERMIT_VESSEL_ID == "FL2698TE") |>
  glimpse()
# $ permit_weeks_amnt_22 <dbl> 32, 32
# $ YEAR                 <dbl> 2022, 2021
# $ compl_w_cnt          <int> 53, 1
# $ compl_2022           <chr> "yes", "no"
# w/o join_by YEAR:
# $ compl_w_cnt          <int> 53, 53
# $ compl_2022           <chr> "yes", "yes"

# TODO: check year = NA

## plot SA year ----
length(unique(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     3956
# 2302 (from metrics)

sa_compl_cnts <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |>
  select(PERMIT_VESSEL_ID,
         compl_2022) |>
  distinct() |>
  add_count(compl_2022, name = "total_compl_y")

sa_compl_cnts |>
  select(compl_2022, total_compl_y) |>
  distinct()
# 2 no                  2700
# 1 yes                 1257
#   compl_2022 total_compl_y
#   <chr>              <int>
# 1 yes                 1234
# 2 no                  1069

# compl
# 1262 * 100 / (3956)
# 32%
# 1257 * 100 / (3956)
# 31.77452
1234 * 100 / (1234 + 1069)
# [1] 53.58228

# no
# 2695 * 100 / (3956)
# 68%
# 2700 * 100 / (3956)
# [1] 68.25076
1069 * 100 / (1234 + 1069)
# [1] 46.41772

sa_compl_cnts_perc <-
  sa_compl_cnts |>
  mutate(total_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  group_by(compl_2022) |>
  mutate(compl_perc =
           total_compl_y * 100 / (total_vsls)) |>
  ungroup()

# (was 41% yes vs. 59% no from 2178 vessels)
# print_df_names(sa_compl_cnts_perc)
sa22_title = "SA Only Permitted Vessels (Total permitted: {sa_compl_cnts_perc$total_vsls})"

compl_2022_ord <- factor(sa_compl_cnts_perc$compl_2022,
                         levels = c("yes", "no"))
year_plot_sa <-
  sa_compl_cnts_perc %>%
  ggplot(aes(x = compl_2022_ord,
             y = compl_perc,
             fill = compl_2022)) +
  # geom_col(position = "dodge") +
  geom_col() +
  ylim(0, 100) +
  labs(title = str_glue(sa22_title),
       x = "",
       # x = "Compliant",
       y = "") +
  geom_text(aes(label = paste0(round(compl_perc, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values =
      c("yes" = "turquoise1",
        "no" = "yellow"),
    name = "Is compliant?",
    labels = c("no", "yes")
  )

# year_plot_sa

# GOM + dual compl by year ----
# There should be a declaration for every logbook.
# There should be a logbook for every declaration of a charter or headboat intending to fish.
# Noncompliant + overridden are compliant

## get gom and dual vsls ---- 
v_p__t__tn_d_weeks_gom <-
  v_p__t__tn_d_weeks |>
  filter(permit_sa_gom_dual %in% c("gom_only", "dual"))

dim(v_p__t__tn_d_weeks_gom)
# [1] 75524    91

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
# 90

empty_cols <-
  v_p__t__tn_d_weeks_gom |>
  map_df(function(x) {
    if (length(unique(x)) == 1) {
      return(unique(x))
    }
  })
# print_df_names(empty_cols) 
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

dim(v_p__t__tn_d_weeks_gom_short)
# [1] 75524    35

data_overview(v_p__t__tn_d_weeks_gom_short) |> 
  head(2)
# VESSEL_VESSEL_ID      1351
# PERMIT_VESSEL_ID      1351

v_p__t__tn_d_weeks_gom_short |>
  count(permit_sa_gom_dual)
# 1 dual               15875
# 2 gom_only           59649

v_p__t__tn_d_weeks_gom_short |>
  count(TRIP_TYPE)
#   TRIP_TYPE     n
#   <chr>     <int>
# 1 A         63121
# 2 H         11935
# 3 NA          468

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
# There should be a logbook for every declaration of a charter or headboat intending to fish.
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

#### decl trip start < or > 1h logbooks trip start ----

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
  # filter(time_diff1 < 3600) |>
  distinct() |> 
  ungroup()

dim(v_p__t__tn_d_weeks_gom_short_matched)
# [1] 35662    41
# [1] 35664    38 filter with restored time
# [1] 44312    39 add matched col
# [1] 75524    39 without filter out not matched

v_p__t__tn_d_weeks_gom_short_matched |> 
  count(matched_reports)
# 1 matched         35664
# 2 not_matched     39860

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
# 6 39

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

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  filter(date_y_m == "Feb 2022",
         # matched_reports == "not_matched"
         INTENDED_FISHING_FLAG == "N",
         is_compliant_w == "yes"
         )


# ## count separately amount of trips and trip_n for each vsl ----
# v_p__t__tn_d_weeks_gom_short_compl_y <-
#   v_p__t__tn_d_weeks_gom_short |>
#   group_by(VESSEL_VESSEL_ID,
#            PERMIT_VESSEL_ID,
#            permit_2022_int) |>
#   mutate(non_na_count.t = sum(!is.na(rep_type.t)),
#             non_na_count.tn = sum(!is.na(rep_type.tn))) |>
#   ungroup()
# 
# dim(v_p__t__tn_d_weeks_gom_short_compl_y)
# # [1] 22090    13
# # [1] 21470    13
# 
# # Number of unmatched declarations for logbooks, and unmatched logbooks for declarations
# # write_csv(v_p__t__tn_d_weeks_gom_short_compl, "cnt_t_n_tn__v_p__t__tn_d_weeks_gom_short_compl.csv")
# 
# # v_p__t__tn_d_weeks_gom_short_compl |>
# #   filter(VESSEL_VESSEL_ID == 72359) |>
# #   select(rep_type.t, rep_type.tn) |>
# #   glimpse()
# # NA both
# 
# # ## rm more columns ----
# # v_p__t__tn_d_weeks_gom_short_compl_y_short <-
# #   v_p__t__tn_d_weeks_gom_short_compl_y |>
# #   select(-c(MONTH_OF_YEAR,
# #             WEEK_OF_YEAR,
# #             date_y_m,
# #             rep_type.t,
# #             rep_type.tn)) |>
# #   distinct()
# # dim(v_p__t__tn_d_weeks_gom_short_compl_y_short)
# # # [1] 1643    8
# # # 1641
# #
# ## gom is_compliant by year ----
# dim(v_p__t__tn_d_weeks_gom_short_compl_y)
# # [1] 21470    13
# 
# ### t(logbooks) < tn(declarations) w fishing intention, A, H ----
# v_p__t__tn_d_weeks_gom_short_compl_short_compl_y <-
#   v_p__t__tn_d_weeks_gom_short_compl_y |>
#   group_by(PERMIT_VESSEL_ID,
#            permit_2022_int) |>
#   # View()
#   mutate(is_compliant_y =
#            case_when(non_na_count.t < non_na_count.tn
#                      ~ "no",
#                      .default = "yes")) |>
#   ungroup()
# 
# dim(v_p__t__tn_d_weeks_gom_short_compl_short_compl_y)
# # [1] 21470    14
# 
# # v_p__t__tn_d_weeks_gom_short_compl_short_compl_y <-
# #   v_p__t__tn_d_weeks_gom_short_compl_short |>
# #   group_by(PERMIT_VESSEL_ID, permit_2022_int) |>
# #   mutate(is_compliant_y =
# #            case_when(non_na_count.t == non_na_count.tn ~
# #                        "yes",
# #                      .default = "no")) |>
# #   ungroup()
# 
# # dim(v_p__t__tn_d_weeks_gom_short_compl_short_compl_y)
# # [1] 1643    9
# # [1] 22090    14
# # [1] 21470    14
# 
# v_p__t__tn_d_weeks_gom_short_compl_short_compl_y |>
#   filter(PERMIT_VESSEL_ID == 'TX9211DE') |>
#   dim()
# # 40
# # filter(VESSEL_VESSEL_ID == 72359) |>
# # $ permit_sa_gom_dual   <chr> "dual"
# # $ permit_weeks_amnt_22 <dbl> 52
# # $ YEAR                 <dbl> NA
# # $ non_na_count.t       <int> 0
# # $ non_na_count.tn      <int> 0
# # $ is_compliant_y       <chr> "yes"
# # same in fhier
# 
# ## check total compl per year ----
# v_p__t__tn_d_weeks_gom_short_compl_short_compl_y |>
#   select(PERMIT_VESSEL_ID) |>
#   distinct() |>
#   dim()
# # 1597
# 
# v_p__t__tn_d_weeks_gom_short_compl_short_compl_y |>
#   select(PERMIT_VESSEL_ID, is_compliant_y) |>
#   distinct() |>
#   count(is_compliant_y)
# # 1 no               625
# # 2 yes              972
# # new rule
# # 1 no               564
# # 2 yes             1033
# # fish intend
# # 1 no               438
# # 2 yes             1159
# 
# # yes
# 972 * 100 / (625 + 972)
# # [1] 60.86412
# # 1033 * 100 / (625 + 972)
# # [1] 64.68378
# 1159 * 100 / (438 + 1159)
# # [1] 72.57358 %
# 
# # no
# 625 * 100 / (1597)
# # [1] 39.13588
# 564 * 100 / (1597)
# # [1] 35.31622
# 438 * 100 / (438 + 1159)
# # [1] 27.42642
# 
# # Was 78% yes and 20% no
# # TODO: why?
# 
# 
# ## plot gom/dual year ----
# gom_compl_cnts <-
#   v_p__t__tn_d_weeks_gom_short_compl_short_compl_y |>
#   select(PERMIT_VESSEL_ID, is_compliant_y) |>
#   distinct() |>
#   add_count(is_compliant_y, name = "total_compl_y_GOM")
# 
# # print_df_names(gom_compl_cnts)
# 
# gom_compl_cnts_perc <-
#   gom_compl_cnts |>
#   mutate(total_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
#   select(-PERMIT_VESSEL_ID) |>
#   distinct() |>
#   group_by(is_compliant_y) |>
#   mutate(compl_perc =
#            total_compl_y_GOM * 100 / (total_vsls)) |>
#   ungroup()
# 
# 
# # print_df_names(sa_compl_cnts_perc)
# gom22_title = "GOM + Dual Permitted Vessels (Total permitted: {gom_compl_cnts_perc$total_vsls})"
# 
# compl_2022_ord_gom <- factor(gom_compl_cnts_perc$is_compliant_y,
#                          levels = c("yes", "no"))
# 
# year_plot_gom <-
#   gom_compl_cnts_perc %>%
#   ggplot(aes(x = compl_2022_ord_gom,
#              y = compl_perc,
#              fill = is_compliant_y)) +
#   # geom_col(position = "dodge") +
#   geom_col() +
#   ylim(0, 100) +
#   labs(title = str_glue(gom22_title),
#        x = "",
#        y = "") +
#   geom_text(aes(label = paste0(round(compl_perc, 1), "%")),
#             position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(
#     values =
#       c("yes" = "turquoise1",
#         "no" = "yellow"),
#     name = "Is compliant?",
#     labels = c("no", "yes")
#   )
# 
# # both year plots ----
# super_title = "2022: Percent Compliant vs. Noncompliant SEFHIER Vessels (Active Permits Only)"
# 
# year_plots <- list(year_plot_sa, year_plot_gom)
# grid.arrange(
#   grobs = year_plots,
#   top = super_title,
#   # left = my_legend,
#   ncol = 1
# )
# 
# 
# # Compare results with FHIER ----
# fhier_compl_file_name = "FHIER_Compliance_2022__08_01_2023.csv"
# FHIER_Compliance_2022_file_0 <-
#   read_csv(
#     file.path(
#       r"(~\R_files_local\my_inputs\from_Fhier\FHIER Compliance)",
#       fhier_compl_file_name
#     ),
#     name_repair = fix_names
#   )
# 
# dim(FHIER_Compliance_2022_file_0)
# # [1] 147654     17
# # [1] 147683     17
# 
# FHIER_Compliance_2022_file <-
#   FHIER_Compliance_2022_file_0 |>
#   select(
#     vessel_official_number,
#     permitgroup,
#     gom_permitteddeclarations__,
#     captainreports__,
#     negativereports__,
#     compliant_,
#     overridden_
#   ) |>
#   distinct()
# 
# dim(FHIER_Compliance_2022_file)
# # [1] 18475     7
# # [1] 18479     7
# 
# FHIER_Compliance_2022_file_short_reg <-
#   FHIER_Compliance_2022_file |>
#   separate_permits_into_3_groups(permit_group_field_name = "permitgroup")
# 
# # glimpse(FHIER_Compliance_2022_file_short_reg)
# FHIER_Compliance_2022_file_short_reg_gom <-
#   FHIER_Compliance_2022_file_short_reg |>
#   filter(permit_sa_gom %in% c("gom_only", "dual"))
# 
# dim(FHIER_Compliance_2022_file_short_reg_gom)
# # [1] 9233    8
# # [1] 9231    8
# 
# fhier_db_compl_gom_join <-
#   full_join(
#     FHIER_Compliance_2022_file_short_reg_gom,
#     v_p__t__tn_d_weeks_gom_short_compl_short_compl_y,
#     join_by(vessel_official_number == PERMIT_VESSEL_ID),
#     relationship = "many-to-many"
#   )
# 
# dim(fhier_db_compl_gom_join)
# # [1] 9815   16
# # [1] 234462     21
# 
# fhier_db_compl_gom_join |>
#   filter(!tolower(compliant_) == tolower(is_compliant_y)) |>
#   dim()
# # Rows: 5,557
# # [1] 91141    21
# 
# trips_info_2022 |>
#   filter(VESSEL_ID == "328219") |>
#   dim()
# # [1] 139  72
# 
# trips_info_2022 |>
#   filter(VESSEL_ID == "328219",
#          TRIP_TYPE %in% c("A", "H")) |>
#   dim()
# # [1] 133  72
# 
# trips_info_2022 |>
#   filter(VESSEL_ID == "328219",
#          TRIP_TYPE %in% c("A", "H")) |>
#     select(SERO_VESSEL_PERMIT) |>
#     distinct() |>
#   dim()
# # 1
# 
# # TODO: ask Michelle
# # why is it compl in FHIER compl?
# fhier_db_compl_gom_join |>
#   filter(VESSEL_VESSEL_ID == "328219") |>
#   filter(gom_permitteddeclarations__ > captainreports__) |>
#   dim()
# # [1] 40 21
# # 2 decl, 1 logb
# # [1] 480  21
# 
# # compare with FHIER metrics ----
# fhier_metrics_path <- r"(~\R_files_local\my_inputs\from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)03012022_12312022.csv)"
# # TODO: change to 12/31/2022 -- 12/31/2023
# 
# fhier_metrics <- read_csv(fhier_metrics_path,
#                           guess_max = 21474836,
#                           name_repair = fix_names) |>
#   filter(!is.na(vessel_official_number))
# 
# dim(fhier_metrics)
# # Rows: 3526 Columns: 13
# 
# fhier_metrics |>
#   select(vessel_official_number) |>
#   distinct() |>
#   count()
# # 1  3526
# 
# # View(fhier_metrics)
# fhier_metrics |>
#   select(vessel_official_number,
#          permit_grouping_region) |>
#   distinct() |>
#   count(permit_grouping_region)
# #   permit_grouping_region     n
# #   <chr>                  <int>
# # 1 GOM                     1323
# # 2 SA                      2203
# # 3 NA                         1
# 
# fhier_metrics_r <-
#   fhier_metrics |>
#   separate_permits_into_3_groups(permit_group_field_name = "permits")
# 
# # print_df_names(fhier_metrics_r)
# 
# fhier_metrics_r |>
#   select(vessel_official_number,
#          permit_sa_gom) |>
#   distinct() |>
#   count(permit_sa_gom)
# # 1 dual            304
# # 2 gom_only       1019
# # 3 sa_only        2203
# 
# fhier_metrics_r_ids <-
#   fhier_metrics_r |>
#   select(vessel_official_number) |>
#   distinct()
# dim(fhier_metrics_r_ids)
# # 3526
# 
# #### in_db_only ----
# in_db_only <- setdiff(
#   vessels_permits_2022_r$PERMIT_VESSEL_ID,
#   fhier_metrics_r_ids$vessel_official_number
# )
# length(in_db_only)
# # [1] 1962
# # TODO: WHY?
# # [1] "FL4450PT" "FL2694HA" "FL3320HK" "FL2619KK" "FL2632PW" "FL3250EM"
# # vessels_permits_2022_r |>
# #   filter(PERMIT_VESSEL_ID == "FL4450PT") |>
# #   View()
# # 2022-02-27 23:00:00
# 
# # trips_info_2022 |>
# # trip_neg_2022 |>
# # trips_notifications_2022 |>
# #   filter(VESSEL_ID == "317460")
#  # 0
# str(in_db_only)
# 
# vessel_info_in_db_only <-
#   vessels_permits_2022_r |>
#   filter(PERMIT_VESSEL_ID %in% in_db_only) |>
#   select(PERMIT_VESSEL_ID, EFFECTIVE_DATE, END_DATE, PERMIT_STATUS, VESSEL_VESSEL_ID, UE, permit_sa_gom) |>   distinct()
# 
# dim(vessel_info_in_db_only)
# # [1] 5239   52
# # [1] 3382    7
# 
# # PERMIT_VESSEL_ID 1962
# # EFFECTIVE_DATE    518
# # END_DATE          264
# # PERMIT_STATUS      11
# # VESSEL_VESSEL_ID 1962
# # UE                  8
# # permit_sa_gom       2
# 
# vessel_info_in_db_only_vsl_ids <-
#   vessel_info_in_db_only |>
#   filter(END_DATE > as.Date('2022-01-03') &
#            END_DATE < as.Date('2022-12-31')) |>
#   select(VESSEL_VESSEL_ID) |>
#   distinct()
# 
# trip_neg_2022 |>
#   filter(VESSEL_ID %in% vessel_info_in_db_only_vsl_ids$VESSEL_VESSEL_ID) |>
#   dim()
# # 8928
# 
# trips_notifications_2022 |>
#   filter(VESSEL_ID %in% vessel_info_in_db_only_vsl_ids$VESSEL_VESSEL_ID) |>
#   dim()
# # [1] 1355   33
# 
# trips_info_2022_int_ah |>
#   filter(VESSEL_ID %in% vessel_info_in_db_only_vsl_ids$VESSEL_VESSEL_ID) |>
#   dim()
# # [1] 1561    9
# # [1] 1029   15 trips_info_2022_int_ah_sero_w_y
# 
# vessel_info_in_db_only |>
#   filter(VESSEL_VESSEL_ID == 280672)
# #   PERMIT_VESSEL_ID      EFFECTIVE_DATE            END_DATE
# # 1          1206187 2021-01-31 23:00:00 2022-01-30 23:00:00
# 
# ### in processed data from db ----
# #### gom ----
# in_db_gom_only <- setdiff(
#   v_p__t__tn_d_weeks_gom_short_compl_short_compl_y$PERMIT_VESSEL_ID,
#   fhier_metrics_r_ids$vessel_official_number
# )
# length(in_db_gom_only)
# # 251
# # 1597-257 = 1340
# # old 1495
# 
# # head(in_db_gom_only)
# 
# #### sa ----
# in_db_sa_only <- setdiff(
#   v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22$PERMIT_VESSEL_ID,
#   fhier_metrics_r_ids$vessel_official_number
# )
# length(in_db_sa_only)
# # [1] 1711
# # 3956-1711
# # [1] 2245, old 2178
# 
# # vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
# #   filter(PERMIT_VESSEL_ID %in% in_db_sa_only) |>
# #   View()
# 
# ### why not in metrics? ----
# # vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
# #   filter(PERMIT_VESSEL_ID %in% in_db_gom_only) |>
# #   View()
# 
# # print_df_names(vessels_permits_2022_r_end_date_uid)
# 
# vessels_permits_2022_r_end_date_uid_not_in_metrics_permit_statuses <-
#   vessels_permits_2022_r_end_date_uid |>
#   filter(PERMIT_VESSEL_ID %in% in_db_only) |>
#   select(
#     PERMIT_VESSEL_ID,
#     EFFECTIVE_DATE,
#     END_DATE,
#     PERMIT_STATUS,
#     VESSEL_VESSEL_ID,
#     UE,
#     permit_sa_gom
#   ) |>
#   distinct() |>
#   count(PERMIT_STATUS, name = "permit_status_cnts") |>
#   mutate(
#     total_permit_status_cnts =
#       sum(permit_status_cnts),
#     perc_permit_status_cnts =
#       permit_status_cnts * 100 / total_permit_status_cnts
#   )
# # glimpse(vessels_permits_2022_r_end_date_uid_not_in_metrics_permit_statuses)
# 
# # in metrics
# vessels_permits_2022_r_end_date_uid_in_metrics_permit_statuses <-
#   vessels_permits_2022_r_end_date_uid |>
#   filter(!PERMIT_VESSEL_ID %in% in_db_only) |>
#   select(
#     PERMIT_VESSEL_ID,
#     EFFECTIVE_DATE,
#     END_DATE,
#     PERMIT_STATUS,
#     VESSEL_VESSEL_ID,
#     UE,
#     permit_sa_gom
#   ) |>
#   distinct() |>
#   count(PERMIT_STATUS, name = "permit_status_cnts") |>
#   mutate(total_permit_status_cnts =
#            sum(permit_status_cnts),
#          perc_permit_status_cnts =
#            permit_status_cnts * 100 / total_permit_status_cnts)
# 
# glimpse(vessels_permits_2022_r_end_date_uid_in_metrics_permit_statuses)
# # ~the same proportion
# # data_overview()
# # PERMIT_VESSEL_ID 1962
# # EFFECTIVE_DATE    518
# # END_DATE          264
# # PERMIT_STATUS      11
# # VESSEL_VESSEL_ID 1962
# # UE                  8
# # permit_sa_gom       2
# 
# 
# #### in_metrics_only ----
# in_metrics_only <- setdiff(
#   fhier_metrics_r_ids$vessel_official_number,
#   vessels_permits_2022_r$PERMIT_VESSEL_ID
# )
# 
# length(in_metrics_only)
# # 27
# 
# head(in_metrics_only)
# # [1] "1139674"  "AL0727MT" "LA3183FS" "1304296"  "AL4269AW" "653939"
# 
# in_metrics_only_ids_str <- paste(in_metrics_only, collapse = "', '")
# 
# vessel_info_in_metrics_only_query <-
#   "SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = sero_official_number )
# WHERE sero_official_number in ('{in_metrics_only_ids_str}')
#   "
# 
# vessel_info_in_metrics_only_res <-
#   dbGetQuery(con, str_glue(vessel_info_in_metrics_only_query)
#   )
# # [1] 443  51
# 
# names(vessel_info_in_metrics_only_res) <-
#     names(vessel_info_in_metrics_only_res) |>
#   make.names(unique = T)
# 
# # print_df_names(vessel_info_in_metrics_only_res)
# vessel_info_in_metrics_only_res |>
#   select(
#     VESSEL_ID,
#     VESSEL_ID.1,
#     EFFECTIVE_DATE,
#     EXPIRATION_DATE,
#     END_DATE,
#     PERMIT_STATUS,
#     SERO_OFFICIAL_NUMBER,
#     UE
#   ) |>
#   filter(EXPIRATION_DATE >=
#            as.Date('2022-01-03') &
#            EFFECTIVE_DATE <=
#            as.Date('2022-12-31')) |>
#   # select(VESSEL_ID, VESSEL_ID.1) |>
#   distinct() |>
#   glimpse()
# # $ VESSEL_ID   <chr> "1139674", "1176885", "1195318"
# # $ VESSEL_ID.1 <dbl> 393431, 326994, 307565
# 
# trips_info_2022 |>
#   filter(VESSEL_ID %in%
#            c(393431, 326994, 307565)) |>
#   count(VESSEL_ID)
# #     VESSEL_ID n
# # 1    326994 1
# # 2    393431 6
# 
# trip_neg_2022 |>
#   filter(VESSEL_ID %in%
#            c(393431, 326994, 307565)) |>
#   count(VESSEL_ID)
# #   VESSEL_ID   n
# # 1    326994 304
# # 2    393431 177
# 
# # this 2 should be in my db results.
# 
# vessels_permits_2022_r |>
#   filter(VESSEL_VESSEL_ID %in%
#            c(393431, 326994, 307565)) |>
#   dim()
# # 0
# 
# trips_notifications_2022 |>
#   filter(VESSEL_ID %in%
#            c(393431, 326994, 307565)) |>
#   count(VESSEL_ID)
# 0
# 
# # ===
# # GOM compliance each week ----
# 
# dim(v_p__t__tn_d_weeks_gom_short)
# # [1] 21470    11
# 
# ## remove odd dates ----
# v_p__t__tn_d_weeks_gom_short_in_p <-
#   v_p__t__tn_d_weeks_gom_short |>
#   # convert yearmon to date format (the first of month), compare with permit
#   filter(as.Date(date_y_m) %within% permit_2022_int)
# dim(v_p__t__tn_d_weeks_gom_short_in_p)
# # [1] 20349    11
# 
# ## count separately amount of trips and trip_n for each vsl ----
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts <-
#   v_p__t__tn_d_weeks_gom_short_in_p |>
#   group_by(VESSEL_VESSEL_ID,
#            PERMIT_VESSEL_ID,
#            permit_2022_int,
#            WEEK_OF_YEAR,
#            date_y_m) |>
#   mutate(non_na_count.t = sum(!is.na(rep_type.t)),
#          non_na_count.tn = sum(!is.na(rep_type.tn))) |>
#   ungroup()
# 
# dim(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts)
# # [1] 20349    13
# 
# ### db err views ----
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts |>
#   filter(
#     PERMIT_VESSEL_ID %in% c(
#       "FL1885MS",
#       "1196390",
#       "FL8446RP",
#       "1294141",
#       "980514",
#       "FL0188RM",
#       "1041927",
#       "689131",
#       "FL6786PB"
#     )
#   ) |>
#   # select(PERMIT_VESSEL_ID) |>
#   dim()
# # 10 w FL6786PB
# 
# ## gom compl ----
# tic()
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w <-
#   v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts |>
#   group_by(VESSEL_VESSEL_ID,
#            PERMIT_VESSEL_ID,
#            permit_2022_int,
#            WEEK_OF_YEAR,
#            date_y_m) |>
#   mutate(is_compliant_w =
#            case_when(non_na_count.t < non_na_count.tn ~
#                        "no",
#                      .default = "yes")) |>
#   ungroup()
# toc()
# # 5.12 sec elapsed
# 
# dim(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w)
# # [1] 20349    14
# 
# ## GOM compl per month from weeks ----
# tic("GOM compl per month from weeks")
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_m <-
#   v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w |>
#   group_by(VESSEL_VESSEL_ID,
#            PERMIT_VESSEL_ID,
#            permit_2022_int,
#            # WEEK_OF_YEAR,
#            date_y_m) |>
#   mutate(is_compliant_m =
#            case_when(any(is_compliant_w == "no") ~
#                        "no",
#                      .default = "yes")) |>
#   ungroup() |>
#   distinct()
# toc()
# # GOM compl per month from weeks: 1.7 sec elapsed
# 
# dim(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_m
#     )
# # [1] 20349    15
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_m |>
#   # filter(!is_compliant_m == is_compliant_w) |>
#   filter(PERMIT_VESSEL_ID == 'FL8981NK') |>
#   filter(date_y_m == "Apr 2022") |>
#   View()
# # [1] 462  15
# # vsl_id 328370
# 
# fhier_metrics |>
#   filter(vessel_official_number == 'FL5809RN') |>
#   dim()
# 
# trips_notifications_2022 |>
#   filter(VESSEL_ID == '328370' &
#            INTENDED_FISHING_FLAG == 'Y'
#            # TRIP_START_DATE > ''
#          ) |>
#   View()
# 
# # grep("trips_info", ls(), value = T)
# 
# trips_info_2022_int_ah_sero_w_y |>
#   filter(VESSEL_ID == '328370' &
#            TRIP_START_m == "Apr 2022"
#          ) |>
#   View()
# 
# ## if we know which are compliant - divide non compliant by buckets ----
# # print_df_names(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w)
# 
# ### add month interval ----
# # glimpse(dates_2022)
# dates_2022_m_int <-
#   dates_2022_yw |>
#   group_by(YEAR, MONTH_OF_YEAR) |>
#   mutate(month_int =
#            lubridate::interval(
#              min(COMPLETE_DATE),
#              max(COMPLETE_DATE)
#            )) |>
#   ungroup()
# 
# dates_2022_m_int_short <-
#   dates_2022_m_int |>
#   select(-COMPLETE_DATE)
# 
# dates_2022_m_int_short |>
#   # select(YEAR, MONTH_OF_YEAR, month_int) |>
#   # distinct() |>
#   glimpse()
# 
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_m_int <-
#   right_join(
#     dates_2022_m_int_short,
#     v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w,
#     join_by(YEAR, MONTH_OF_YEAR, WEEK_OF_YEAR, date_y_m),
#     relationship = "many-to-many"
#   )
# 
# dim(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_m_int)
# # [1] 125598     15
# # dim(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w)
# 
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_perm <-
#   v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w |>
#   mutate(m_first_day = day(date_y_m),
#          m_last_day = day(as.Date(date_y_m, frac = 1)))
# 
# # View(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_m_days)
# 
# 
# ### add total weeks per permit per month ----
# tic("v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_cnt_w")
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_cnt_w <-
#   v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w |>
#   group_by(VESSEL_VESSEL_ID,
#            PERMIT_VESSEL_ID,
#            permit_2022_int,
#            date_y_m) |>
#   mutate(permit_weeks_month_amnt_22 =
#            round(permit_2022_int / lubridate::dweeks(1))) |>
#   ungroup()
# toc()
# # v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_cnt_w: 3.94 sec elapsed
# 
# View(v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_cnt_w)
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_non_compl <-
#   v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w |>
#   filter(is_compliant_w == "no")
# 
# ### test jan ----
# 
# v_p__t__tn_d_weeks_gom_short_in_p_t_tn_cnts_compl_w_non_compl |>
#   filter(MONTH_OF_YEAR == 4) |>
#   data_overview()
# # VESSEL_VESSEL_ID     205
# # PERMIT_VESSEL_ID     205
# # permit_sa_gom_dual     2
# # MONTH_OF_YEAR          1
# # WEEK_OF_YEAR           5
