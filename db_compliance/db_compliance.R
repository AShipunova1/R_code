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

# View(trips_info_2022_int_ah)

trips_info_2022_int_ah_w_y <-
  trips_info_2022_int_ah %>%
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

# str(trips_info_2022_int_ah_w_y)
trips_info_2022_int_ah_w_y |> 
  filter(TRIP_START_week_num == 0) |> 
  dim()
# TRIP_START_m == "Jan 2022"
# [1] 104  15

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
  View()
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
length(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list)
dim(trips_info_2022_int_ah_w_y)
# [1] 97003    15
dim(trip_neg_2022_w_y)
# [1] 747173      6
dim(trips_notifications_2022_ah_w_y)
# [1] 67738    33

## add all weeks to each df ----

#### check if there are earlier reports with an end date in 2022 and start in 2021 ----
trips_info_2022_int_ah_w_y |>
  # filter(TRIP_START_y == 2021) |>
  filter(TRIP_START_week_num < 52 &
           TRIP_START_y == 2021 &
           TRIP_END_y == 2022) |>
  View()
# 5

trips_notifications_2022_ah_w_y |>
  # filter(TRIP_START_y == 2021) |>
  filter(TRIP_START_week_num < 52 &
           TRIP_START_y == 2021 &
           TRIP_END_y == 2022) |>
  View()
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

# str(dates_2022_w)

### t by start ----

t_dates_by <-
   join_by(date_y_m     == TRIP_START_m,
           WEEK_OF_YEAR == TRIP_START_week_num
)

tic("t_d_w")  
t_d_w <-
   full_join(
     dates_2022_w,
     trips_info_2022_int_ah_w_y,
     t_dates_by,
     relationship = "many-to-many"
   )
toc()
# t_d_w: 0.38 sec elapsed

# dim(t_d_w)
# [1] 627414     17

### tne ----
# tne_d_w |> 
#   filter(WEEK_OF_YEAR == 0) |> 
#   glimpse()

tne_dates_by <-
   join_by(date_y_m     == TRIP_DATE_m,
           WEEK_OF_YEAR == TRIP_week_num
)

tic("tne_d_w")  
tne_d_w <-
   full_join(
     dates_2022_w,
     trip_neg_2022_w_y,
     tne_dates_by,
     relationship = "many-to-many"
   )
toc()
# tne_d_w: 0.96 sec elapsed
dim(tne_d_w)
# [1] 4806915       8

### tn by start ----

tn_dates_by <-
   join_by(date_y_m     == TRIP_START_m,
           WEEK_OF_YEAR == TRIP_START_week_num
)

tic("tn_d_w")  
tn_d_w <-
   full_join(
     dates_2022_w,
     trips_notifications_2022_ah_w_y,
     tn_dates_by
     # ,
     # relationship = "many-to-many"
   )
toc()
# tn_d_w: 0.75 sec elapsed
dim(tn_d_w)
# [1] 435779     35

## v_p ----
# ### short
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa_short <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only |>
#   select(
#     VESSEL_VESSEL_ID,
#     PERMIT_VESSEL_ID,
#     unique_all_vessel_ids,
#     min_permit_eff_date,
#     max_permit_end_date,
#     EFFECTIVE_DATE_week_num,
#     my_end_week_num,
#     EFFECTIVE_DATE_y,
#     my_end_y,
#     EFFECTIVE_DATE_m,
#     my_end_m,
#     eff_int,
#     permit_sa_gom_dual,
#     permit_2022_int
#     # weeks_perm_2022_amnt
#   )

# vp_dates_by <-
#    join_by(date_y_m     >= EFFECTIVE_DATE_m,
#            date_y_m     <= my_end_m,
#            WEEK_OF_YEAR >= EFFECTIVE_DATE_week_num,
#            WEEK_OF_YEAR <= my_end_week_num
#               # overlaps(x$EFFECTIVE_DATE,
#               #          x$my_end_date,
#               #          y$EFFECTIVE_DATE,
#               #          y$my_end_date,
#               #          bounds = "[)"))
# 
#    )     
 
# tic("v_p_d_w")  
# v_p_d_w_sa_22 <-
#    full_join(
#      dates_2022_w,
#      vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa_short,
#      vp_dates_by
#    )
# toc()
# v_p_d_w: 0.44 sec elapsed

# dim(v_p_d_w_sa_22)
# [1] 204865     19
# VESSEL_VESSEL_ID        3956

### add weeks per permit 22 ----

v_p_d_w_sa_22 <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only |>
  mutate(permit_weeks_amnt_22 =
           round(permit_2022_int / lubridate::dweeks(1)))

dim(v_p_d_w_sa_22)
# [1] 6459   20

# end of data preparations ----

# Count distinct weeks per vessel ----

## neg trip weeks ----
# TODO ?do we need a join?

# print_df_names(v_trip_neg_2022_w_y)
trip_neg_2022_w_y_cnt_u <-
  trip_neg_2022_w_y |>
  group_by(VESSEL_ID) %>%
  mutate(distinct_weeks_ne = n_distinct(TRIP_week_num))

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
# [1] 57  3
# TODO: why - long trip
# [1] 6318   35

## trips weeks count per vessel ----
# View(v_trips_info_2022_int_ah_w_y)
trips_info_2022_int_ah_w_y_weeks_cnt_u <-
  trips_info_2022_int_ah_w_y %>%
    group_by(VESSEL_ID) %>%
      mutate(
        distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
        distinct_end_weeks_t = n_distinct(TRIP_END_week_num)
      ) %>%
      mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))

dim(trips_info_2022_int_ah_w_y_weeks_cnt_u)
# [1] 1110    7
# [1] 1934    4
# [1] 1933    4 summarize
# [1] 96990   110
# [1] 97003    18

trips_info_2022_int_ah_w_y_weeks_cnt_u |>
  filter(!distinct_start_weeks_t == distinct_end_weeks_t) |>
  dim()
# 27
# 63
# [1] 64  4
# [1] 3628  110 - long trips
# [1] 3552   18

# Keep only SERO permitted ----
trips_info_2022_int_ah_w_y_sero <-
  trips_info_2022_int_ah_w_y |>
  filter(!is.na(SERO_VESSEL_PERMIT)) |>
  distinct()

# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance

# print_df_names(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only)
# [1] "EFFECTIVE_DATE, END_DATE, EXPIRATION_DATE, permit_sa_gom, my_end_date, unique_all_vessel_ids, min_permit_eff_date, max_permit_end_date, EFFECTIVE_DATE_week_num, my_end_week_num, EFFECTIVE_DATE_y, my_end_y, EFFECTIVE_DATE_m, my_end_m, eff_int, permit_sa_gom_dual"

## all weeks of 2022 * all vessels ----
# SA: each can have:
# 1) a permit
# 2) a trip
# 3) a negative report
# 1 only
# 1,2
# 1,3
# 2 only
# 3 only
# 2,3?

## SA: compliant vessels per year ----

### rm dates, leave w, m, y ----
#### v_p_d ----
# print_df_names(v_p_d_w_sa_22)
v_p_d_w_sa_22_short <-
  v_p_d_w_sa_22 |>
  select(
    VESSEL_VESSEL_ID,
    PERMIT_VESSEL_ID,
    EFFECTIVE_DATE_y,
    EFFECTIVE_DATE_m,
    EFFECTIVE_DATE_week_num,
    my_end_y,
    my_end_m,
    my_end_week_num,
    eff_int,
    permit_2022_int,
    permit_weeks_amnt_22
  ) |> 
  distinct()

# v_p_d_w_sa_22_short |> 
#   filter(!YEAR == EFFECTIVE_DATE_y) |> 
#   glimpse()
# Rows: 8,470
# Columns: 12

dim(v_p_d_w_sa_22_short)
# [1] 38031     9
# [1] 6423   10 V-P with no dates yet
# [1] 6423   11 (weeks per permit)
# print_df_names(v_p_d_w_sa_22_short)

#### t_d ----
# print_df_names(t_d_w)
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
  distinct()

t_d_w |> 
  filter(!YEAR == TRIP_START_y) |> 
  glimpse()
# Rows: 198

dim(t_d_w_short)
# [1] 97014    11
# [1] 38447     9 (no trip_id)

#### tne_d ----

# print_df_names(tne_d_w)
tne_d_w_short <-
  tne_d_w |>
  select(-c(TRIP_DATE, TRIP_ID)) |>
  distinct()

# check year
tne_d_w |> 
  filter(!YEAR == TRIP_DATE_y) |> 
  glimpse()
# Rows: 4,154
# Columns: 8

# dim(tne_d_w_short)
# [1] 136333      6

## join by week ----
# v_p_d_w_sa_22_short

tic("v_p__tne_d_weeks")
v_p__tne_d_weeks <-
  full_join(
    v_p_d_w_sa_22_short,
    tne_d_w_short,
    join_by(VESSEL_VESSEL_ID == VESSEL_ID),
    relationship = "many-to-many"
  )
toc()
# v_p__tne_d_weeks: 0.42 sec elapsed
dim(v_p__tne_d_weeks)
# [1] 206877     15
# [1] 162614     14 WEEK_OF_YEAR
# [1] 190268     15 by vessel_only

tic("v_p__tne__t_d_weeks")
v_p__tne__t_d_weeks <-
  full_join(
    v_p__tne_d_weeks,
    t_d_w_short,
    join_by(date_y_m,
            YEAR,
            WEEK_OF_YEAR,
            MONTH_OF_YEAR,
            VESSEL_VESSEL_ID == VESSEL_ID),
    relationship = "many-to-many"
  )
toc()
# v_p__tne__t_d_weeks: 0.45 sec elapsed

dim(v_p__tne__t_d_weeks)
# [1] 287374     21
# [1] 190794     19 +WEEK_OF_YEAR
# [1] 192748     18 +MONTH_OF_YEAR
# [1] 220442     19
# 220438     20

# data_overview(v_p__tne__t_d_weeks)
# VESSEL_VESSEL_ID        6265
# PERMIT_VESSEL_ID        3957

# v_p__tne__t_d_weeks_22 <-
#   v_p__tne__t_d_weeks |>
#   # it is not 2021
#   filter(!YEAR %in% c(2021, 2023) &
#            # there is a report
#            (!is.na(TRIP_DATE_y)) & !is.na(TRIP_START_y))
# 
# View(v_p__tne__t_d_weeks_22)
# # [1] 8387   19
# # data_overview(v_p__tne__t_d_weeks_22)
# # VESSEL_VESSEL_ID        1062
# # PERMIT_VESSEL_ID         309
# 
# str(v_p__tne__t_d_weeks)

v_p__tne__t_d_weeks_21 <-
  v_p__tne__t_d_weeks |>
  # filter(date_y_m %within% permit_2022_int)
  # exclude the last weeks of 2021 before 52
  filter(date_y_m == 'Dec 2021' & 
             WEEK_OF_YEAR < 52 &
             is.na(TRIP_DATE_y) & 
             is.na(TRIP_START_y)
         )

# 22
# [1] 189986     19

dim(v_p__tne__t_d_weeks_21)
# [1] 336  19
# [1]  0 20 (52/1 0)

# v_p__tne__t_d_weeks |>
#   filter(date_y_m == 'Dec 2021' & 
#            (!is.na(TRIP_DATE_y) |
#              !is.na(TRIP_START_y))
#          ) |> 
#   glimpse()
# 1
# $ WEEK_OF_YEAR            <dbl> 52
# $ TRIP_START_y            <dbl> 2021
# $ TRIP_END_y              <dbl> 2022
# $ TRIP_END_m              <yearmon> Feb 2022

# FL3612RX = compl in FHIER
# trips_info_2022_int_ah_w_y_weeks_cnt_u |> 
#   filter(VESSEL_ID == "252980") |> 
#   dim()
# # 0
# 
# trip_neg_2022_w_y_cnt_u |> 
#   filter(VESSEL_ID == "252980") |> 
#   View()
# # [1] 215   7
# 
# v_p__tne__t_d_weeks |> 
#   filter(VESSEL_VESSEL_ID == "252980") |> 
#   View()
# # 59
# 
# v_p__tne__t_d_weeks |> 
#   filter(VESSEL_VESSEL_ID == "252980") |> 
#   select(MONTH_OF_YEAR) |> 
#     distinct() |> 
#     arrange(MONTH_OF_YEAR)
# 
v_p__tne__t_d_weeks |>
  filter(WEEK_OF_YEAR == 0) |>
    filter(!is.na(TRIP_START_y)) |>
  View()
# TODO: get read of 0 week
# 
# ===
# SA compliance by year ----
non_compliant_filter <- quo(# no reports
  is.na(TRIP_DATE_y) & is.na(TRIP_START_y))

not_compliant <-
  v_p__tne__t_d_weeks |>
  # use the saved filter
  filter(!!non_compliant_filter) |> 
  distinct() |>
  arrange(PERMIT_VESSEL_ID) 

dim(not_compliant)
# [1] 3698   20

# distinct
# Rows: 2,269
# TODO: add which weeks

not_compliant_vsl_ids <-
  not_compliant |> 
  select(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |> 
  distinct()
dim(not_compliant_vsl_ids)
# [1] 2269    2

v_p__tne__t_d_weeks_compl1 <-
  v_p__tne__t_d_weeks |> 
  filter(!(!!non_compliant_filter))

dim(v_p__tne__t_d_weeks_compl1)
# [1] 216740     20

v_p__tne__t_d_weeks_compl1_ids <-
  v_p__tne__t_d_weeks_compl1 |>
  select(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |>
  distinct()

dim(v_p__tne__t_d_weeks_compl1_ids)
# [1] 4841    2

intersect(not_compliant_vsl_ids$PERMIT_VESSEL_ID,
          v_p__tne__t_d_weeks_compl1_ids$PERMIT_VESSEL_ID) |> 
  head()
# 2
# [1] "FL2310RW" NA        
# FL2310RW - duplicate in vessels on FHIER

intersect(not_compliant_vsl_ids$VESSEL_VESSEL_ID,
          v_p__tne__t_d_weeks_compl1_ids$VESSEL_VESSEL_ID) |> 
  length()
# 61