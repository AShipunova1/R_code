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

# err msg if no connection, but keep running
try(con <- connect_to_secpr())
# con <- connect_to_secpr()

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

# vessels to use (from FHIER metric tracking, except SRHS) ----
# dim(fhier_reports_metrics_tracking_not_srhs_ids)
# 2981
dim(metricks_not_srhs_ids_2022)
# 3571

# vessels_permits_2022 ----
dim(vessels_permits_2022)
# [1] 40474    51

## fhier_metrics vessels only ----
vessels_permits_2022_c_me <-
  vessels_permits_2022_c |>
  filter(
    PERMIT_VESSEL_ID %in% metricks_not_srhs_ids_2022$vessel_official_number
  )

dim(vessels_permits_2022_c_me)
# [1] 30306    51
# [1] 22937    51 tracking no srsh
# [1] 29656    51

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
# [1] "2023-08-24"
# new comp
# add my_end_date: 14.16 sec elapsed

dim(vessels_permits_2022_r_end_date)
# [1] 20231    53
# [1] 15152    53
# [1] 14827    53 metr

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
# uid: 0.75 sec elapsed

dim(vessels_permits_2022_r_end_date_uid)
# [1] 20231    55
# [1] 15152    55
# [1] "2023-08-22"
# [1] 14798    55
# [1] 14827    55

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
# [1] 6073    8
# [1] 6089    8

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
# [1] 6073   10
# [1] 6089   10

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
# [1] 6073   16
# [1] 6089   16

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
# new lapt
# get permit periods: 41.16 sec elapsed

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
# [1] 6089   19

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
# [1] 271   1

#### why not in new? ----

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  filter(grepl("FL2995SR", unique_all_vessel_ids)) |>
  select(eff_int, permit_sa_gom_dual) |>
  head()
  # eff_int                            permit_sa_gom_dual
#   <Interval>                                       <chr>
# 1 2022-03-02 23:00:00 EST--2023-01-30 23:00:00 EST gom_only
# 2 2022-10-19 00:00:00 EDT--2024-01-30 23:00:00 EST sa_only

### gom and sa periods overlap ----
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
# 1   907     1769    3397 (excl. srhs)
# 1   907     1778    3404

# TODO: compare vessel_permits from db and v_permits by overlapping with interval 2022
# adjust the query

## check if vessels are duplicated in a region
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual$unique_all_vessel_ids |>
  unique() |>
  length()
# 911
  # unique()
# 272
# 271

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
# excl srhs
 # $ : chr [1:3] "dual" "907" "271"
 # $ : chr [1:3] "gom_only" "1769" "1072"
 # $ : chr [1:3] "sa_only" "3397" "2239"
# new metricks no srhs
  # $ : chr [1:3] "dual" "907" "271"
  # $ : chr [1:3] "gom_only" "1778" "1077"
  # $ : chr [1:3] "sa_only" "3404" "2241"

### what makes them duplicates ----
#### in dual ----
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |> dim()
# [1] 907  19

# FL3610NF 4

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |>
  select(-starts_with("EXPIRATION_DATE"),
    -starts_with("END_DATE"),
    -permit_sa_gom
  ) |>
  distinct() |> dim()
# [1] 469  16
# [1] 467  16

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
# 89

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
# [1] 67738    34

trips_notifications_2022_ah_6 <-
  trips_notifications_2022_ah_5_6 |>
  filter(NOTIFICATION_TYPE_IDs == '6')

dim(trips_notifications_2022_ah_6)
# [1] 63535    34
# [1] 67613    34

trips_notifications_2022_ah_6 |>
  count(NOTIFICATION_TYPE_ID)
# 1                    6 67613

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
# [1] 32 40

trips_notifications_2022_ah_6_w_y |>
  filter(TRIP_START_week_num == 52) |>
  dim()
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
# 1   907     1769    3397 (excl. srhs)
# 1   907     1778    3404

dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual)
# [1] 6207   19
# [1] 6073   19
# [1] 6089   19

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
# t_d_w: 0.05 sec elapsed

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
# [1] 747185      16

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
# [1] 6073   20 excl srhs
# [1] 6089   20

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
# [1] 3583    5 escl srhs
# [1] 3590    5

## t_d ----
# remove fields
t_d_w_short <-
  t_d_w |>
  select(
    -c(
      # TRIP_START_DATE,
      # TRIP_END_DATE,
      # TRIP_ID,
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
# [1] 80978    79

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
    # TRIP_ID
    # ,
    TRIP_START_DATE_TIME, #empty
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
# [1] 66621    21 excl. srhs

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
# [1] 314492     89

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
# [1] 147599

dim(t__tn_d_weeks)
# [1] 120876     86
# [1] 120754     88
# [1] 120795     89

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
# [1] 320800     93 (excl. srhs)
# [1] 320898     93

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
# [1] 77 93

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
# [1] 128284     93
# [1] 127865     93 excl srhs
# [1] 128056     93

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

dim(v_p__t__tne_d_weeks_21)[1]
# 0 (change 52/1 0)
# ok

# Source SA and GOM compliance ----

# Run to get SA compliance
source(file.path(my_paths$git_r,
                 current_project_name,
                 "sa_22_db_compliance.R"))
# result:
# compliance per year per vessel
# v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short

source(file.path(my_paths$git_r,
                 current_project_name,
                 "gom_22_db_compliance.R"))
# results:
# compliance per week
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short
# per year
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y
# per month
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m


# Non compliant percentage ----

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_nc <-
#   v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short |>
#   filter(compl_w_total == "no")

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_nc |>
#   dim()
# [1] 7635   12
# [1] 7288   10 overr

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short |>
  group_by(date_y_m,
           WEEK_OF_YEAR,
           compl_w_total) |>
  mutate(vsls_nc_w = n_distinct(PERMIT_VESSEL_ID)) |>
  ungroup()

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt)
# [1] 21551    11

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
  filter(PERMIT_VESSEL_ID == "FL4459MW") |>
  glimpse()
# $ WEEK_OF_YEAR         <dbl> 13, 14
# $ date_y_m             <yearmon> Apr 2022, Apr 2022
# $ compl_w_total        <chr> "no", "no"
# $ vsls_nc_w            <int> 97, 143

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
    filter(WEEK_OF_YEAR == 13) |>
    head() |>
    glimpse()
# $ PERMIT_VESSEL_ID     "FL4459MW", "FL4459PW", "FL4482NJ", "FL4482NJ", "FL3…
# $ MONTH_OF_YEAR        4, 3, 3, 4, 3, 4
# $ WEEK_OF_YEAR         13, 13, 13, 13, 13, 13
# $ date_y_m             Apr 2022, Mar 2022, Mar 2022, Apr 2022, Mar 202…
# $ compl_w_total        "no", "yes", "no", "no", "yes", "yes"
# $ vsls_nc_w            97, 251, 126, 97, 251, 151

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
    filter(WEEK_OF_YEAR == 13 &
             date_y_m == "Mar 2022") |>
    select(PERMIT_VESSEL_ID) |>
    distinct() |>
    count()
# 373

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
  select(PERMIT_VESSEL_ID,
         # date_y_m,
         WEEK_OF_YEAR,
         compl_w_total) |>
  distinct() |>
  group_by(
    # date_y_m,
           WEEK_OF_YEAR,
           compl_w_total) |>
  filter(WEEK_OF_YEAR == 13) |>
  # subset(PERMIT_VESSEL_ID %in% PERMIT_VESSEL_ID[matched_compl == 'yes']) |>
  mutate(cnt_vsls_w = n_distinct(PERMIT_VESSEL_ID)) |>
  # glimpse()
  # Rows: 418
  ungroup() |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  glimpse()
# $ WEEK_OF_YEAR  <dbl> 13, 13
# $ compl_w_total <chr> "no", "yes"
# $ cnt_vsls_w    <int> 144, 274
# 144 + 274 = 418

# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present in all compliant ----

weeks_per_vsl_permit_year_compl_cnt <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
  dplyr::add_count(permit_2022_int,
                   VESSEL_VESSEL_ID,
                   PERMIT_VESSEL_ID,
                   compl_w_total,
                   name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(permit_2022_int,
                   VESSEL_VESSEL_ID,
                   PERMIT_VESSEL_ID,
                   name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

weeks_per_vsl_permit_year_compl_cnt |>
  filter(PERMIT_VESSEL_ID == "FL4463MX") |>
  View()

weeks_per_vsl_permit_year_compl_cnt |>
  filter(!weeks_per_vessel_per_compl == total_weeks_per_vessel) |>
  dim()
# [1] 8709   13

## test 1a ----
weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(PERMIT_VESSEL_ID == "1000042") |>
  dplyr::select(YEAR,
                compl_w_total,
                # compl_m,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  distinct()
# 1 2022 yes 25 40
# 2 2022 no  15 40

# was
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

# HERE
nc_2022_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(
    # year_permit == "2022 sa_only",
    # compl_w_total == "no",
    PERMIT_VESSEL_ID %in% c(
      "VA9236AV",
      "VA6784AD",
      "VA4480ZY",
      "SC9207BX",
      "SC8907DF",
      "SC8298DH"
    )
  ) %>%
  dplyr::select(PERMIT_VESSEL_ID,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique() |>
  arrange(desc(total_weeks_per_vessel))

glimpse(nc_2022_sa_only_test)
# 0

# sa only
v_p__t__tn_d_weeks |>
  dplyr::filter(
    # year_permit == "2022 sa_only",
    # compl_w_total == "no",
    PERMIT_VESSEL_ID %in% c(
      "VA9236AV",
      "VA6784AD",
      "VA4480ZY",
      "SC9207BX",
      "SC8907DF",
      "SC8298DH"
    )
  ) %>%
  View()

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

glimpse(count_weeks_per_vsl_permit_year_compl_p)

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

count_weeks_per_vsl_permit_year_n_compl_p_short_m <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(tolower(compl_w_total) == "no") %>%
  dplyr::select(
    PERMIT_VESSEL_ID,
    permit_2022_int,
    date_y_m,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  distinct() |>
  unique()

count_weeks_per_vsl_permit_year_n_compl_p_short_y <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(tolower(compl_w_total) == "no") %>%
  dplyr::select(
    PERMIT_VESSEL_ID,
    # permit_2022_int,
    # date_y_m,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  distinct() |>
  unique()

dim(count_weeks_per_vsl_permit_year_n_compl_p_short_y)
# [1] 453   4
dim(count_weeks_per_vsl_permit_year_n_compl_p_short_m)
# [1] 2293    6 (with month)

## 2b) get percentage "buckets" ----
# percent buckets
get_p_buckets <- function(my_df, field_name) {
  my_df %>%
    dplyr::mutate(
      percent_n_compl_rank =
        dplyr::case_when(
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) &
            !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) &
            !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>%
    return()
}

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_m <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short_m,
                "percent_compl")

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short_y,
                "percent_compl")

dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y)
# [1] 453   6
dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_m)
# [1] 2293    7 w month

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y |>
  glimpse()

### test 2 ----
# count in one bucket

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y %>%
  dplyr::filter(percent_n_compl_rank == '75<= & <=100%') %>%
  # dplyr::filter(date_y_m == "2022 sa_only") %>%
  dplyr::count(percent_compl,
               # date_y_m,
               name = "amount_of_occurences") %>%
  # glimpse()
#   $ percent_compl        <dbl> 75.00000, 77.27273, 77.35849, 78.26087, 78.787…
# $ amount_of_occurences <int> 4, 11, 12, 9, 9, 2, 9, 9, 9, 9, 20, 5, 5, 21, …
  dplyr::arrange(desc(percent_compl)) %>%
  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 1848 m
# 262 y

# 3) count how many in each bucket ----

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_m <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_m %>%
  dplyr::add_count(percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_y <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y %>%
  dplyr::add_count(percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_y %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                cnt_v_in_bucket) %>%
  distinct() %>%
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
 # $ cnt_v_in_bucket     : int [1:4] 127 29 35 262
 # $ total_per_y_r       : int [1:4] 453 453 453 453

# 127+29+35+262
# 453
# correct

# 4) cnt percents of (3) ----

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_y %>%
  # cnt vessels per year, permit region and compliance
  dplyr::add_count(name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_m <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_m %>%
  # cnt vessels per year, permit region and compliance
  dplyr::add_count(date_y_m,
    name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y)
# 453
dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_m)
# 2293

### check 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1

# GOM new
# 1 0<= & <25%                         28.0
# 2 25<= & <50%                         6.40
# 3 50<= & <75%                         7.73
# 4 75<= & <=100%                      57.8

# 5) blue plots by year ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# print_df_names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# "2022: % Non-Compliant GOM + Dual Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this]  "(Total Non-Compliant = 304 Vessels; Active Permits = 1192 Vessels)"
# "2022: % Non-Compliant SA Only Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this] "(Total Non-Compliant = 1289 Vessels; Active Permits = 1707 Vessels)"
# For plot 4:
# "2023: SA + Dual Permitted SEFHIER Vessels (Total Permitted: 2235 ; Total Noncompliant: 1628; Expired Permits: 1)"

blue_year_plot_titles <-
  data.frame(
    year_permit = c("2022 sa_only",
                    "2022 gom_dual",
                    "2023 sa_dual"),
    first_part = c(
      "SA Only Permitted Vessels\n(",
      "GOM + Dual Permitted Vessels\n(",
      "2023: SA + Dual Permitted SEFHIER Vessels\n(Total Permitted = 2235 Vessels; "
    )
  )

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y
    # %>%
      # dplyr::filter(year_permit == curr_year_permit)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # See the function definition F2
    curr_title_y_p <- make_year_permit_label(curr_year_permit)

    y_p_title <- "GOM 22"

    curr_blue_year_plot_title <-
      blue_year_plot_titles %>%
      filter(year_permit == curr_year_permit)

    y_p_title <-
      paste0(
        curr_blue_year_plot_title$first_part,
        "Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        " Vessels; Acitve permits = ",
        active_permits$exp_y_tot_cnt,
        # "; Expired permits: ",
        # expired_permits$exp_y_tot_cnt,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 12))

    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]]

## plot 2022 ----
ndash <- "\u2013"
super_title = paste0(
  "2022: % Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)

# footnote = textGrob(
#   "X axes is % of missing reports for non-compliant vessels",
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -1.5,
#   just = "right",
#   x = 0.01, y = 0.99,
#   vjust = 1
# )

### common y axes ----
yleft <- textGrob("% per permit region",
                  # rotate
                  rot = 90,
                  gp = gpar(fontsize = 10))

p <-
  list(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[1:2])[[1]] %>%
  # remove individual x and y labels for each plot
  map( ~ .x + labs(x = NULL, y = NULL))

plot_perc_22 <- gridExtra::grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)
