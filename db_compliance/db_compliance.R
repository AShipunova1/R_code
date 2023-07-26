# db_compliance
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

# vessels_permits_2022 ----
# dim(vessels_permits_2022)
# [1] 40474    51

## weird headers ----
# print_df_names(vessels_permits_2022)
vessels_permits_2022_c <-
  vessels_permits_2022 |>
  rename("PERMIT_VESSEL_ID" = "QCSJ_C000000000300000") |>
  rename("VESSEL_VESSEL_ID" = "QCSJ_C000000000300001")

## region permit groups ----
vessels_permits_2022_r <-
  vessels_permits_2022_c |>
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

dim(vessels_permits_2022_r_end_date)
# [1] 20231    53

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
# 
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |>
#   filter(grepl('FL9004NX', unique_all_vessel_ids)) |>
#   View()

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

# old_dual_v_ids <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual |>
#   filter(permit_sa_gom == "dual") |>
#   select(unique_all_vessel_ids) |>
#   distinct()

# dim(old_dual_v_ids)
# [1] 357   1

# intersect(new_dual_ids$unique_all_vessel_ids,
#           old_dual_v_ids$unique_all_vessel_ids) |>
#   length()
# 357
# 275

# in_new_dual_only <-
#   setdiff(vessels_permits_2022_r_end_date_uid_short_mm_w_y_dual__dual_ids$unique_all_vessel_ids,
#           old_dual_v_ids$unique_all_vessel_ids)
# glimpse(in_new_dual_only)
# 10

# in_new_dual_only1 <-
#   setdiff(new_dual_ids$unique_all_vessel_ids,
#           old_dual_v_ids$unique_all_vessel_ids)
# length(in_new_dual_only1)
# 0
# in_old_only <-
#   setdiff(old_dual_v_ids$unique_all_vessel_ids,
#         new_dual_ids$unique_all_vessel_ids)
# glimpse(in_old_only)
# 82

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

#### why not in old? ----
# glimpse(in_new_dual_only)
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_dual |>
#   filter(grepl("FL9004NX", unique_all_vessel_ids)) |>
#   glimpse()
# $ permit_sa_gom           <chr> "sa_only", "gom_only"

# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual |>
#   filter(grepl("FL9004NX", unique_all_vessel_ids)) |>
#   glimpse()
# $ permit_sa_gom               <chr> "gom_only", "sa_only"

# vessels_permits_2022_r |>
#   # filter(PERMIT_VESSEL_ID == "FL9004NX") |>
#   filter(PERMIT_VESSEL_ID == "TX6550AU") |>
#   distinct() |>
#   glimpse()

# vessels_dual_maybe <- c("1074262",
# "1145285",
# "1152092",
# "567241",
# "609460",
# "960433",
# "FL7422NT",
# "FL7812SM",
# "FL9004NX",
# "TX6550AU")

# vessels_permits_2022_r |>
#   filter(PERMIT_VESSEL_ID %in% vessels_dual_maybe) |>
#   View()

# vessels_permits_2022_r_10_dual_maybe <-
#   vessels_permits_2022_r |>
#   filter(PERMIT_VESSEL_ID %in% vessels_dual_maybe) |>
#   select(PERMIT_VESSEL_ID, EFFECTIVE_DATE,
#          END_DATE,
#          PERMIT_STATUS,
#          permit_sa_gom) |>
#   distinct()

# write_csv(vessels_permits_2022_r_10_dual_maybe,
#           "vessels_permits_2022_dual_maybe.csv")

## split by permit to get dual in another way ----
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y %>%
#   split(as.factor(vessels_permits_2022_r_end_date_uid_short_mm_w_y$permit_sa_gom))
#
# map_df(vessels_permits_2022_r_end_date_uid_short_mm_w_y_l, dim)
# #   gom_only sa_only
# # 1     2525    6908
# # 2       16      16
# # 1     2528    6914
# # 2       14      14
#
### join by overlap of gom and sa (= dual) ----
# # From Help:
# # It is common to have right-open ranges with bounds like `[)`, which would
# # mean an end value of `415` would no longer overlap a start value of `415`.
# # Setting `bounds` allows you to compute overlaps with those kinds of ranges.
# # View(vessels_permits_2022)
# # View(vessels_permits_2022_r_end_date_l$gom_only)
# by <- join_by(unique_all_vessel_ids,
#               overlaps(x$EFFECTIVE_DATE,
#                        x$my_end_date,
#                        y$EFFECTIVE_DATE,
#                        y$my_end_date,
#                        bounds = "[)"))
#
# tic("vessels_permits_2022_r_end_date_overlap_join")
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join <-
#   full_join(
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l$gom_only,
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l$sa_only,
#   by,
#   suffix = c(".gom", ".sa")
# )
# toc()
# # permit_info_r_l_overlap_join1: 0.66 sec elapsed
#
# dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y)
# # [1] 20918   112
# # short
# # [1] 8939   30
# # [1] 8949   34
# # [1] 9442   14
#
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join %>%
#   select(unique_all_vessel_ids) %>%
#   distinct() %>%
#   dim()
# # [1] 5461    1
#
# vessels_permits_2022 %>%
#   # select(QCSJ_C000000000300000
#   # [1] 5461    1
#   # select(QCSJ_C000000000300001
#   # [1] 5461    1
#   select(QCSJ_C000000000300000,
#          QCSJ_C000000000300001,
#          VESSEL_ALT_NUM) %>%
#   distinct() %>%
#   dim()
# # [1] 5462    3 (+NA)
#
 ### add "dual" to intervals ----
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join %>%
#   mutate(permit_sa_gom =
#            case_when(
#              !is.na(permit_sa_gom.sa) &
#                !is.na(permit_sa_gom.gom) ~ "dual",
#              .default =
#                dplyr::coalesce(permit_sa_gom.sa,
#                                permit_sa_gom.gom)
#
#            )) |>
#   ungroup()
#
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual %>%
#   select(permit_sa_gom) %>%
#   distinct()
# # all 3
#
# dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual)
# # [1] 20918   114
# # [1] 8939   31
# # [1] 8949   35
# # [1] 8949   28
#
# # View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual)
#
# # to get dual in the overlapping period:
# # filter(!is.na(permit_sa_gom.sa))
#
# tic("vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22")
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22 <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual %>%
#   mutate(
#     eff_int_gom =
#       lubridate::interval(EFFECTIVE_DATE.gom,
#                           my_end_date.gom),
#     eff_int_sa =
#       lubridate::interval(EFFECTIVE_DATE.sa,
#                           my_end_date.sa)
#   ) %>%
#   filter(int_overlaps(eff_int_gom,
#                       interval_2022) |
#            int_overlaps(eff_int_sa,
#                       interval_2022)
#          )
# toc()
# # vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22: 0.24 sec elapsed
#
#### check ----
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22 %>%
#   select(permit_sa_gom) %>%
#   distinct()
# # all 3
#
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22 %>%
#   filter(permit_sa_gom == "dual") %>%
#   select(unique_all_vessel_ids) %>%
#   distinct() %>%
#   dim()
# # 357

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

# TODO: compare vessel_permits from db and v_permits by overlapping with interval 2022
# adjust the query

## check if vessels are duplicated in a region
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual$unique_all_vessel_ids |>
  unique() |>
  length()
# 917
  # unique() |>
# 275

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

### what makes them duplicates ----
#### in dual ----
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |> dim()
# 917

# FL3610NF 4

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |>
  select(-starts_with("EXPIRATION_DATE"),
    -starts_with("END_DATE"),
    -permit_sa_gom
  ) |>
  distinct() |> dim()
  # filter(grepl("FL3610NF", unique_all_vessel_ids)) |>
  # View()
# [1] 472  13


# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |>
#   filter(grepl("FL3610NF", unique_all_vessel_ids)) |>
#   View()

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
## rm extra cols ----
t_names_to_rm <-
  c("ACTIVITY_TYPE",
    "ADDDITIONAL_FISHERMEN",
    "APP_VERSION",
    "APPROVAL_DATE",
    "APPROVED_BY",
    "BAIT_WEIGHT",
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
    "EVENT_ID",
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
    "STATUS",
    "SUB_TRIP_TYPE",
    "SUBMIT_METHOD",
    "SUBMITTED_BY_PARTICIPANT",
    "SUPPLIER_TRIP_ID",
    "TICKET_TYPE",
    "TRANSMISSION_DATE",
    "TRIP_END_TIME",
    "TRIP_FEE",
    "TRIP_NBR",
    "TRIP_START_TIME",
    "UC",
    "UE",
    "VALIDATING_AGENCY",
    "VENDOR_APP_NAME",
    "VENDOR_PLATFORM",
    "VTR_NUMBER")

# print_df_names(trips_info_2022_short)
# print_df_names(trip_neg_2022_short)

trips_info_2022_short <-
  trips_info_2022 |>
  select(-any_of(t_names_to_rm)) |>
  distinct()

trip_neg_2022_short <-
  trip_neg_2022 |>
  select(-any_of(t_names_to_rm)) |>
  distinct()

trips_notifications_2022_short <-
  trips_notifications_2022 |>
  select(-any_of(t_names_to_rm)) |>
  distinct()


# Trip data (= logbooks) ----
## add trip interval ----

trips_info_2022_int <-
  trips_info_2022_short %>%
  mutate(trip_int =
           lubridate::interval(
             lubridate::floor_date(TRIP_START_DATE,
                                   unit = "day"),
             lubridate::floor_date(TRIP_END_DATE,
                                   unit = "day")
           ))

### check trips_info_2022_int ----
trips_info_2022_int %>%
  select(TRIP_START_DATE, TRIP_END_DATE, trip_int) %>%
  dim()
# [1] 98528     3

## trip types A and H trips ----
trips_info_2022_int_ah <-
  trips_info_2022_int %>%
  filter(TRIP_TYPE %in% c("A", "H"))

# Keep only SERO permitted ----
trips_info_2022_int_ah_sero <-
  trips_info_2022_int_ah |>
  filter(!is.na(SERO_VESSEL_PERMIT)) |>
  distinct()

# Trip notifications (= declarations) ----

## trip types A and H trip_notif ----
trips_notifications_2022 %>%
   select(TRIP_TYPE) %>% distinct()
#     TRIP_TYPE
# 1           H
# 3           A
# 383         R
# 697         C

trips_notifications_2022_ah <-
  trips_notifications_2022_short %>%
  filter(TRIP_TYPE %in% c("A", "H"))

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
  filter(TRIP_START_week_num == 0) |> 
  dim()
# TRIP_START_m == "Jan 2022"
# [1] 104  15
# 77 15 (sero)

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


## to negative trips ----

trip_neg_2022_w_y <-
  trip_neg_2022_short %>%
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

# check 
trip_neg_2022_w_y |> 
  filter(TRIP_week_num == 0 & TRIP_DATE_m == "Jan 2022") |> 
  dim()
# [1] 2101    6

# results:
map_df(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   917     2066    6459
# 2    19       19      19
dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual)
# [1] 9442   19

dim(trips_info_2022_int_ah_sero_w_y)
# [1] 80967    15
dim(trip_neg_2022_w_y)
# [1] 747173      6
dim(trips_notifications_2022_ah_w_y)
# [1] 67738    33

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

# trip_neg_2022_w_y_cnt_u |> 
#   # filter(TRIP_START_y == 2021) |>
#   filter(TRIP_week_num < 52 &
#            TRIP_DATE_y == 2021) |>
#   dim()
# 0
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

dates_2022_yw <-
  dates_2022_yw0 |>
  # remove all before the last week of 2021
  filter(!(MONTH_OF_YEAR == 12 &
             YEAR == 2021 &
             WEEK_OF_YEAR < 52)) |>
  # has to rename to 0, bc that's how %U works
  mutate(WEEK_OF_YEAR =
    case_when(
      MONTH_OF_YEAR == 1 &
        YEAR == 2022 &
        WEEK_OF_YEAR == 52
      ~ WEEK_OF_YEAR == 0,
      .default = WEEK_OF_YEAR
    )
  )

# View(dates_2022_yw)
# [1] 401   5

dates_2022_w <-
  dates_2022_yw |>
  select(-COMPLETE_DATE)

glimpse(dates_2022_w)

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

dim(t_d_w)
# [1] 627414     17
# [1] 626904     17
# TODO: check the difference, what was in the full_join? (no dates)
# [1] 523465     17 left_join
# [1] 523877     16 full_join

# t_d_w |> 
#   filter(is.na(YEAR)) |> 
#   dim()
# 0

### tne ----
# tne_d_w |>
#   filter(WEEK_OF_YEAR == 0) |>
#   glimpse()
# Rows: 4,202

print_df_names(trip_neg_2022_w_y_cnt_u)

tne_dates_by <-
  join_by(YEAR == TRIP_DATE_y,
          date_y_m     == TRIP_DATE_m,
          WEEK_OF_YEAR == TRIP_week_num)

tic("tne_d_w")
tne_d_w <-
   full_join(
     dates_2022_w,
     trip_neg_2022_w_y_cnt_u,
     tne_dates_by,
     relationship = "many-to-many"
   )
toc()
# tne_d_w: 0.96 sec elapsed
dim(tne_d_w)
# [1] 4806915       8
# [1] 4806913       8

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
# [1] 435779     35
# [1] 435640     35 (SERO), 34 by Year

### add weeks per permit 22 ----

v_p_d_w_22 <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  mutate(permit_weeks_amnt_22 =
           round(permit_2022_int / lubridate::dweeks(1)))

dim(v_p_d_w_22)
# [1] 6459   20
# [1] 9442   20

# Count distinct weeks per vessel ----

## neg trip weeks ----

trip_neg_2022_w_y_cnt_u <-
  trip_neg_2022_w_y |>
  group_by(VESSEL_ID) %>%
  mutate(distinct_weeks_ne = n_distinct(TRIP_week_num)) |> 
  ungroup()

dim(trip_neg_2022_w_y_cnt_u)
# [1] 1709    5
# [1] 3414    2 summarize
# [1] 747078     44 mutate
# [1] 747173      7

## trip_notif weeks count per vessel ----
trips_notifications_2022_ah_w_y_cnt_u <-
  trips_notifications_2022_ah_w_y |>
  group_by(VESSEL_ID) |>
  mutate(
    distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
    distinct_end_weeks_tn = n_distinct(TRIP_END_week_num)
  )

dim(trips_notifications_2022_ah_w_y_cnt_u)
# [1] 914   3 summarize
# [1] 67738    35

trips_notifications_2022_ah_w_y_cnt_u %>%
   filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
   dim()
# [1] 0 6
# ok
# [1] 6318   35

## trips weeks count per vessel ----
trips_info_2022_int_ah_sero_w_y_weeks_cnt_u <-
  trips_info_2022_int_ah_sero_w_y %>%
    group_by(VESSEL_ID) %>%
      mutate(
        distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
        distinct_end_weeks_t = n_distinct(TRIP_END_week_num)
      ) %>%
      mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))

dim(trips_info_2022_int_ah_sero_w_y_weeks_cnt_u)
# [1] 1110    7
# [1] 1934    4
# [1] 1933    4 summarize
# [1] 96990   110
# [1] 97003    18
# [1] 80967    18 sero

trips_info_2022_int_ah_sero_w_y_weeks_cnt_u |>
  filter(!distinct_start_weeks_t == distinct_end_weeks_t) |>
  dim()
# 2196 sero

# rm dates, leave w, m, y ----
## v_p_d ----

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

# v_p_d_w_sa_22_short
# [1] 38031     9
# [1] 6423   10 V-P with no dates yet
# [1] 6423   11 (weeks per permit)
dim(v_p_d_w_22_short)
# [1] 8939   12
# [1] 5554    5

## t_d ----

t_d_w_short <-
  t_d_w |>
  select(
    -c(
      TRIP_START_DATE,
      TRIP_END_DATE,
      TRIP_ID, 
      TRIP_TYPE,
      SERO_VESSEL_PERMIT,
      GARFO_VESSEL_PERMIT,
      TRIP_TIME_ZONE,
      trip_int
    )
  ) |> 
  distinct() |> 
  mutate(rep_type = "trips")

dim(t_d_w_short)
# [1] 97014    11
# [1] 38447     9 (no trip_id)
# [1] 37990     9
# [1] 32375     9

## tne_d ----

# print_df_names(tne_d_w)
tne_d_w_short <-
  tne_d_w |>
  select(-c(TRIP_DATE, TRIP_ID)) |>
  distinct() |> 
  mutate(rep_type = "trips_neg")

dim(tne_d_w_short)
# [1] 136333      6
# [1] 136329      6

## tn_d ----
# print_df_names(tn_d_w)
tn_d_w_short <-
  tn_d_w |>
  select(
    -c(
      NOTIFICATION_TYPE_ID,
      SYSTEM_ID,
      TRIP_TYPE,
      TRIP_START_DATE,
      ARRIVAL_PORT,
      DEPARTURE_PORT,
      NOTIFICATION_SEQ,
      CANCEL_FLAG,
      TRIP_END_DATE,
      TRIP_ID,
      TRIP_START_DATE_TIME,
      EMAIL_SENT,
      TRIP_ID,
      INTENDED_FISHING_FLAG,
      LANDING_LOCATION,
      DEA_PERMIT_SOLD_NOTIFICATION,
      GEAR_NOTIFICATION,
      LANDING_LOCATION_NAME,
      STAT_ZONE,
      LANDING_LOCATION_STATE,
      LANDING_LOCATION_COUNTY,
      LANDING_LOCATION_CITY,
      PROCESSED,
      PROCESSED_TIMESTAMP,
      RAW_INPUT_ID,
      NOTIFICATION_TIME_ZONE
    )
  ) |> 
  distinct() |> 
  mutate(rep_type = "trips_notif")

dim(tn_d_w_short)
# [1] 21211    10

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
# [1] 160791     11

## t & tn ----
tic("t__tn_d_weeks")
t__tn_d_weeks <-
  full_join(
    t_d_w_short,
    tn_d_w_short,
    join_by(YEAR,
            MONTH_OF_YEAR,
            WEEK_OF_YEAR,
            date_y_m,
            VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".t", ".tn")
  )
toc()
dim(t__tn_d_weeks)
# [1] 41248    14
# length(unique(t__tn_d_weeks$WEEK_OF_YEAR))         
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

# 2)
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
# 
# ### check ----
# v_p__t__tne_d_weeks |>
#     filter(VESSEL_VESSEL_ID == "326929") |>
# # [1] 17 14
#   select(PERMIT_VESSEL_ID, WEEK_OF_YEAR) |>
#   distinct() |>
#   # 14
#   # count(PERMIT_VESSEL_ID)
# # 1 FL4430NN            14
#   group_by(PERMIT_VESSEL_ID) |>
#   mutate(compl_weeks =
#            n_distinct(WEEK_OF_YEAR)) |>
#   dim()
# # 14
# # compl_weeks >= permit_weeks_amnt_22
# 
# v_p__t__tne_d_weeks |>
#   filter(VESSEL_VESSEL_ID == "326929") |>
#   group_by(VESSEL_VESSEL_ID) |>
#   summarise(n_distinct(WEEK_OF_YEAR))
# # # A tibble: 1 × 2
# #   VESSEL_VESSEL_ID `n_distinct(WEEK_OF_YEAR)`
# #              <dbl>                      <int>
# # 1           326929                         14
# # correct
# 
# # TODO: add which weeks
# 
# 
# not_compliant_sa_vsl_ids <-
#   v_p__t__tne_d_weeks_sa_compl |> 
#   filter(compliant == "no") |> 
#   select(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |> 
#   distinct()
# dim(not_compliant_sa_vsl_ids)
# # [1] 2269    2
# # [1] 2268    2
# # [1] 2845    2 ok
# 
# v_p__t__tne_d_weeks_compl1 <-
#   v_p__t__tne_d_weeks_sa_compl |> 
#   filter(compliant == "yes") |> 
#   select(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |> 
#   distinct()
# 
# dim(v_p__t__tne_d_weeks_compl1)
# # [1] 1112    2
# 
# intersect(not_compliant_sa_vsl_ids$PERMIT_VESSEL_ID,
#           v_p__t__tne_d_weeks_compl1$PERMIT_VESSEL_ID) |> 
#   head()
# # 0
# # 2
# # [1] "FL2310RW" NA        
# # FL2310RW - duplicate in vessels on FHIER
# 
# vessel_ids_both_compl_and_not <-
#   intersect(not_compliant_sa_vsl_ids$VESSEL_VESSEL_ID,
#           v_p__t__tne_d_weeks_compl1$VESSEL_VESSEL_ID) 
# 
# length(vessel_ids_both_compl_and_not)
# # 61
# # [1]  94753  98483 326294 248489 291474 248785
# # 0
# 
# v_p__t__tne_d_weeks_sa_compl |> 
#   filter(VESSEL_VESSEL_ID == "248785") |> 
#   dim()
# # [1] 45 15
# 
# 
# v_p__t__tne_d_weeks_sa_compl |>   
#   filter(VESSEL_VESSEL_ID == "283991") |> 
#   View()
# # has reports, check permit
# 
# v_p__t__tne_d_weeks_sa_compl |> 
#   filter(PERMIT_VESSEL_ID == "1255890") |> 
#   View()
# # [1]  5 15
# 
# # FHIER
# # 1255890................. KNOT READY  - DARRELL R BESSINGER            (352) 2227202
# 
# v_p__t__tne_d_weeks_sa_compl_w_cnt <-
#   v_p__t__tne_d_weeks_sa_compl |> 
#   group_by(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID, YEAR, compliant) |> 
#   mutate(compl_weeks = if_else(compliant == "yes",  n_distinct(WEEK_OF_YEAR), 0)) |> 
#   ungroup()
# 
# dim(v_p__t__tne_d_weeks_sa_compl_w_cnt)
# # [1] 23704    16
# 
# v_p__t__tne_d_weeks_sa_compl_w_cnt_short <-
#   v_p__t__tne_d_weeks_sa_compl_w_cnt |>
#   select(VESSEL_VESSEL_ID,
#          PERMIT_VESSEL_ID,
#          YEAR,
#          permit_weeks_amnt_22,
#          compl_weeks,
#          compliant) |> 
#   distinct()
# 
# dim(v_p__t__tne_d_weeks_sa_compl_w_cnt_short)
# # [1] 7547    5
# # [1] 3958    6
# 
# ### check is.na(YEAR)? 381155 1024989 ----
# v_p__t__tne_d_weeks_sa_compl_w_cnt_short |> 
#   filter(VESSEL_VESSEL_ID == "381155") |> 
#   glimpse()
# # $ VESSEL_VESSEL_ID     <dbl> 381155
# # $ PERMIT_VESSEL_ID     <chr> "1024989"
# # $ YEAR                 <dbl> 2022
# # $ permit_weeks_amnt_22 <dbl> 32
# # $ compl_weeks          <int> 10
# # $ compliant            <chr> "yes"
# 
# # TODO: change compliant case_when - default - no, if there is a report - yes? Look for permit_weeks_amnt_22 <= compl_weeks?
# 
# # compl_weeks < permit_weeks_amnt_22, why it is in compliant? Where are the other weeks (147361 1036367)
# v_p__t__tne_d_weeks_sa_compl_w_cnt_short |> 
#   filter(VESSEL_VESSEL_ID == "147361") |> 
#   glimpse()
# 
# # $ permit_weeks_amnt_22 <dbl> 52
# # $ compl_weeks          <int> 4
# # $ compliant            <chr> "yes"
# 
# v_p__t__tne_d_weeks_sa_compl_w_cnt |> 
#   filter(VESSEL_VESSEL_ID == "147361") |> 
#   glimpse()
# 
# 
# ### check is.na(YEAR) ----
# 
# t_d_w |> 
#   filter(is.na(YEAR)) |> 
#   dim()
# # [1] 412  17
# # 0 with left join
# 
## count SA year compliance by weeks comparison with permit ----
# print_df_names(v_p__t__tne_d_weeks_sa_compl_w_cnt)

# SA compliance by year ----

v_p__t__tne_d_weeks_sa <-
  v_p__t__tne_d_weeks |>
  filter(permit_sa_gom_dual == "sa_only")
dim(v_p__t__tne_d_weeks_sa)
# [1] 90766    15

tic("v_p__t__tne_d_weeks_sa_compl")
v_p__t__tne_d_weeks_sa_compl <-
  v_p__t__tne_d_weeks_sa |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m,
           YEAR) |>
  mutate(sa_compl_week = case_when(is.na(rep_type.t) &
                                is.na(rep_type.tne) ~
                                "no",
                              .default = "yes")) |> 
  ungroup()
toc()
# v_p__t__tne_d_weeks_sa_compl: 28.39 sec elapsed

View(v_p__t__tne_d_weeks_sa_compl)
# [1] 90766    16

## count compl weeks ----
v_p__t__tne_d_weeks_sa_compl_cnt_w <-
  v_p__t__tne_d_weeks_sa_compl |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           # YEAR,
           # WEEK_OF_YEAR,
           # date_y_m,
           sa_compl_week) |>
  mutate(compl_w_cnt = n_distinct(WEEK_OF_YEAR)) |> 
  ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w)
# [1] 90766    17

v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w |> 
  mutate(compl_2022 = case_when(
           compl_w_cnt >= permit_weeks_amnt_22 ~ "yes",
           .default = "no")
  ) |>
  ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22)
# [1] 90766    18

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

View(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short)
# [1] 5275    6

# TODO: check year = NA

## plot SA year ----
# length(unique(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     3956

v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |> 
  select(PERMIT_VESSEL_ID,
         compl_2022) |> 
  distinct() |> 
  count(compl_2022)
# 1 no          2695
# 2 yes         1262

# compl
1262 * 100 / (3956)
# 32%

# no
2695 * 100 / (3956)
# 68%

# (was 41% yes vs. 59% no from 2178 vessels)
  # pivot_longer(cols = c(percent_compl,
  #                       percent_non_compl),
  #              names_to = "is_compliant",
  #              values_to = "percent")
  # 
  #   y_r_title = curr_year_region
  #   plots_for_c_vs_nc_vsls(curr_df, y_r_title)

# GOM + dual compl by year ----

v_p__t__tn_d_weeks_gom <-
  v_p__t__tn_d_weeks |> 
  filter(permit_sa_gom_dual %in% c("gom_only", "dual"))

dim(v_p__t__tn_d_weeks_gom)
# [1] 22613    18

# length(unique(v_p__t__tn_d_weeks_gom$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     1597

# TODO: check if permit_id in tn mean the same as in p_v
# 
# ===
v_p__t__tn_d_weeks_gom_short <-
  v_p__t__tn_d_weeks_gom |>
  select(
    -c(
      TRIP_END_week_num.t,
      TRIP_END_y.t,
      TRIP_END_m.t,
      PERMIT_ID,
      TRIP_END_week_num.tn,
      TRIP_END_y.tn,
      TRIP_END_m.tn,
    )
  ) |>
  distinct()

dim(v_p__t__tn_d_weeks_gom_short)
# [1] 22090    11

# # change na rep to 0?
# v_p__t__tn_d_weeks_gom_short_rep0 <-
#   v_p__t__tn_d_weeks_gom_short |>
#   # mutate(across(c(rep_type.t,
#   #                 rep_type.tn), tidyr::replace_na, 0))
#   mutate(across(c(rep_type.t,
#                   rep_type.tn), ~ replace(., is.na(.), 0)))

# dim(v_p__t__tn_d_weeks_gom_short_rep0)

v_p__tn__t_d_weeks_compl <-
  v_p__t__tn_d_weeks_gom_short |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           permit_2022_int) |>
  mutate(
    rep_t_cnts_notna = sum(!is.na((rep_type.t))),
    rep_t_cnts_na = sum(is.na((rep_type.t))),
    rep_tn_cnts_notna = sum(!is.na((rep_type.tn))),
    rep_tn_cnts_na = sum(is.na((rep_type.tn)))
  )

View(v_p__tn__t_d_weeks_compl)
  add_count(rep_type.t, name = "rep_t_cnts") |> 
  add_count(rep_type.tn, name = "rep_tn_cnts") |> 
  # mutate(rep_t_cnts = n(rep_type.t),
  #        rep_tn_cnts = n(rep_type.tn)) |> 
  ungroup()
# Error in `mutate()`:
# ℹ In argument: `rep_t_cnts = n(rep_type.t)`.
# ℹ In group 1: `VESSEL_VESSEL_ID = 72359`, `PERMIT_VESSEL_ID = "933533"`,
#   `permit_2022_int = 2021-12-31 19:00:00 EST--2022-12-30 19:00:00 EST`.

View(v_p__tn__t_d_weeks_compl)
# [1] 22090    13

# v_p__t__tn_d_weeks_gom_short_rep0 |> 
#   filter(VESSEL_VESSEL_ID == 72359) |> 
#   select(rep_type.t) |> 
#   glimpse()
   
  
  # mutate(is_compliant = case_when(
  #   is.na(rep_type.t) & is.na(rep_type.t) ~
  #     "no",
  #   .default = "yes"
  # ))

v_p__tn__t_d_weeks_compl |> 
  select(PERMIT_VESSEL_ID) |> 
  distinct() |> 
  dim()
# 1597    

v_p__tn__t_d_weeks_compl_cnt <-
  v_p__tn__t_d_weeks_compl |> 
  group_by(PERMIT_VESSEL_ID, is_compliant) |>
  dplyr::add_count(is_compliant, name = "compl_cnt") |> 
  mutate(compl_y_22 =
           case_when(permit_weeks_amnt_22 == compl_cnt ~
                       "yes",
                     .default = "no"
    
  )) |> 
  ungroup()

# View(v_p__tn__t_d_weeks_compl_cnt)

v_p__tn__t_d_weeks_compl_cnt |> 
  select(PERMIT_VESSEL_ID, compl_y_22) |>
  distinct() |>
  count(compl_y_22)
# 1 no          1588
# 2 yes            9

# yes
823 * 100 / (1597)
# [1] 51.53413

# no
774 * 100 / (1597)
# [1] 48.46587

# Was 78% yes and 20% no
# TODO: why?
# 