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
interval_2022 = lubridate::interval(as.Date('2022-01-01'),
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
  ungroup()
toc()
# get permit periods: 46.29 sec elapsed
# get permit periods: 48.8 sec elapsed

# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |>
#   filter(grepl('FL9004NX', unique_all_vessel_ids)) |>
#   View()

## mark dual ----
# Dual only if GOM and SA for the same period
# dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv)

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |>
  # for each vessel and permit in effect interval
  group_by(unique_all_vessel_ids, eff_int) |>
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

# Trip data (= logbooks) ----
## rename duplicate columns ---- 
new_col_names <- make.unique(names(vessels_trips_info_2022), sep = "_")
names(vessels_trips_info_2022) <- new_col_names

## add trip interval ----

v_trips_info_2022_int <-
  vessels_trips_info_2022 %>%
  mutate(trip_int =
           lubridate::interval(
             lubridate::floor_date(TRIP_START_DATE,
                                   unit = "day"),
             lubridate::floor_date(TRIP_END_DATE,
                                   unit = "day")
           ))

### check trips_info_2022_int ----
v_trips_info_2022_int %>%
  select(TRIP_START_DATE, TRIP_END_DATE, trip_int) %>%
  dim()
# [1] 98528     3
# [1] 98514     3 + v

## trip types A and H trips ----
v_trips_info_2022_int_ah <-
  v_trips_info_2022_int %>%
  filter(TRIP_TYPE %in% c("A", "H"))

# Trip notifications (= declarations) ----
## rename duplicate columns ---- 
new_col_names <- make.unique(names(vessels_trip_notifications_2022), sep = "_")
names(vessels_trip_notifications_2022) <- new_col_names

## trip types A and H trip_notif ----
vessels_trip_notifications_2022 %>%
   select(TRIP_TYPE) %>% distinct()
#     TRIP_TYPE
# 1           H
# 3           A
# 383         R
# 697         C

v_trip_notifications_2022_ah <-
  vessels_trip_notifications_2022 %>%
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

v_trips_info_2022_int_ah_w_y <-
  v_trips_info_2022_int_ah %>%
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

# str(v_trips_info_2022_int_ah_w_y)

## to trip notifications ----
v_trip_notifications_2022_ah_w_y <-
  v_trip_notifications_2022_ah %>%
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

## to negative trips ----
## rename duplicate columns ---- 
v_trip_neg_2022_w_y <-
  vessels_trip_neg_2022 %>%
  mutate(
    TRIP_week_num =
      strftime(TRIP_DATE, format = "%U"),
    TRIP_DATE_y =
      year(TRIP_DATE),
    TRIP_DATE_m =
      zoo::as.yearmon(TRIP_DATE)
  ) %>%
  mutate(TRIP_week_num =
           as.double(TRIP_week_num))

# results:
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list
# v_trips_info_2022_int_ah_w_y
# v_trip_neg_2022_w_y
# v_trip_notifications_2022_ah_w_y
# end of data preparations ----

# Count distinct weeks per vessel ----

## neg trip weeks ----
# TODO ?do we need a join?

# print_df_names(v_trip_neg_2022_w_y)
v_trip_neg_2022_w_y_cnt_u <-
  v_trip_neg_2022_w_y |>
  group_by(VESSEL_ID) %>%
  mutate(distinct_weeks_ne = n_distinct(TRIP_week_num))

dim(v_trip_neg_2022_w_y_cnt_u)
# [1] 1709    5
# [1] 3414    2 summarize
# [1] 747078     44 mutate 

## trip_notif weeks count per vessel ----
v_trip_notifications_2022_ah_w_y_cnt_u <-
  v_trip_notifications_2022_ah_w_y |>
  group_by(VESSEL_ID) |>
  mutate(
    distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
    distinct_end_weeks_tn = n_distinct(TRIP_END_week_num)
  )

dim(v_trip_notifications_2022_ah_w_y_cnt_u)
# [1] 914   3 summarize
# [1] 67738    69 

v_trip_notifications_2022_ah_w_y_cnt_u %>%
   filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
   dim()
# [1] 0 6
# ok
# [1] 57  3
# TODO: why - long trip
# [1] 6318   69

## trips weeks count per vessel ----
# View(v_trips_info_2022_int_ah_w_y)
v_trips_info_2022_int_ah_w_y_weeks_cnt_u <-
  v_trips_info_2022_int_ah_w_y %>%
    # browser()
    group_by(VESSEL_ID) %>%
      mutate(
        distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
        distinct_end_weeks_t = n_distinct(TRIP_END_week_num)
      ) %>%
      mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))

dim(v_trips_info_2022_int_ah_w_y_weeks_cnt_u)
# [1] 1110    7
# [1] 1934    4
# [1] 1933    4 summarize
# [1] 96990   110

v_trips_info_2022_int_ah_w_y_weeks_cnt_u %>%
   filter(!distinct_start_weeks_t == distinct_end_weeks_t) %>%
   dim()
# 27
# 63
# [1] 64  4
# [1] 3628  110 - long trips

# Keep only sero permitted ----
v_trips_info_2022_int_ah_w_y_sero <-
  v_trips_info_2022_int_ah_w_y |>
  filter(!is.na(SERO_VESSEL_PERMIT)) |>
  distinct()

# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance

# print_df_names(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only)
# [1] "EFFECTIVE_DATE, END_DATE, EXPIRATION_DATE, permit_sa_gom, my_end_date, unique_all_vessel_ids, min_permit_eff_date, max_permit_end_date, EFFECTIVE_DATE_week_num, my_end_week_num, EFFECTIVE_DATE_y, my_end_y, EFFECTIVE_DATE_m, my_end_m, eff_int, permit_sa_gom_dual"

## get eff_int and 2022 intersection ----
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only |>
  mutate(permit_eff_int_2022 =
           lubridate::intersect(eff_int,
                                interval_2022)) |>
  mutate(weeks_perm_2022_amnt =
           (permit_eff_int_2022 / lubridate::dweeks(1)) |>
           round()) |>
  distinct()

# [1] 3956    4
# [1] 6459   19

min(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa$weeks_perm_2022_amnt, na.rm = T)
# [1] 0.1190476

max(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa$weeks_perm_2022_amnt, na.rm = T)
# 52
# 0-109 (without interval_2022)
# 0-52

# data_overview(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53
# VESSEL_ID                3888
# today() [1] "2023-07-17"
# VESSEL_VESSEL_ID           3181
# weeks_perm_2022_amnt         54
# ---
# unique_all_vessel_ids 3956
# eff_int               2072
# eff_int_22             320
# week_amnt              318
# ---
# unique_all_vessel_ids   3956
# eff_int                 2072
# permit_eff_int_2022      320
# weeks_perm_2022_amnt      53

## remove vessel specific fields (get them from v_p if needed) and extra trip field----
v_names_to_rm <- 
  c("SER_ID",
    "UPDATED_FLAG",
    "SERO_HOME_PORT_CITY",
    "SERO_HOME_PORT_COUNTY",
    "SERO_HOME_PORT_STATE",
    "SERO_OFFICIAL_NUMBER",
    "COUNTY_CODE",
    "STATE_CODE",
    "ENTRY_DATE",
    "SUPPLIER_VESSEL_ID",
    "PORT_CODE",
    "HULL_ID_NBR",
    "COAST_GUARD_NBR",
    "STATE_REG_NBR",
    "REGISTERING_STATE",
    "VESSEL_NAME",
    "PASSENGER_CAPACITY",
    "VESSEL_TYPE",
    "YEAR_BUILT",
    "UPDATE_DATE",
    "PRIMARY_GEAR",
    "OWNER_ID",
    "EVENT_ID_V",
    "DE_V",
    "UE_V",
    "DC_V",
    "UC_V",
    "STATUS_V"
  )

t_names_to_rm <- 
  c("ACTIVITY_TYPE",
    "ADDDITIONAL_FISHERMEN",
    "APP_VERSION",
    "APPROVAL_DATE",
    "APPROVED_BY",
    "BAIT_WEIGHT",
    "CAPT_NAME_FIRST",
    "CAPT_NAME_LAST",
    "CF_ISS_AGENCY",
    "CF_PERMIT_ID.t",
    "CONFIRMATION_SIGNATURE",
    "CONFIRMED_VALIDATING_AGENCY",
    "COST_BAIT",
    "COST_FOOD",
    "COST_ICE",
    "COST_IFQ",
    "COST_LIGHT",
    "COST_MISC",
    "DAYS_AT_SEA",
    "DC_1",
    "DC.t",
    "DE_1",
    "DE.t",
    "DEA_PERMIT_ID",
    "END_PORT",
    "EVENT_ID_1",
    "EVENT_ID.t",
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
    "STATUS.t",
    "SUB_TRIP_TYPE",
    "SUBMIT_METHOD.t",
    "SUBMITTED_BY_PARTICIPANT",
    "SUPPLIER_TRIP_ID",
    "TICKET_TYPE",
    "TRANSMISSION_DATE",
    "TRIP_END_TIME",
    "TRIP_FEE",
    "TRIP_NBR",
    "TRIP_START_TIME",
    "UC_1",
    "UC.t",
    "UE_1",
    "UE.t",
    "VALIDATING_AGENCY",
    "VENDOR_APP_NAME",
    "VENDOR_PLATFORM",
    "VTR_NUMBER") 
  
names_to_keep <-  
  c("eff_int",
    "EFFECTIVE_DATE",
    "EFFECTIVE_DATE_m",
    "EFFECTIVE_DATE_week_num",
    "EFFECTIVE_DATE_y",
    "END_DATE",
    "EXPIRATION_DATE",
    "GARFO_VESSEL_PERMIT",
    "max_permit_end_date",
    "min_permit_eff_date",
    "my_end_date",
    "my_end_m",
    "my_end_week_num",
    "my_end_y",
    "permit_eff_int_2022",
    "permit_sa_gom",
    "permit_sa_gom_dual",
    "PERMIT_VESSEL_ID",
    "SERO_VESSEL_PERMIT",
    "TRIP_END_DATE",
    "TRIP_END_week_num",
    "TRIP_ID.t",
    "trip_int",
    "TRIP_START_DATE",
    "TRIP_START_week_num",
    "TRIP_TIME_ZONE",
    "TRIP_TYPE",
    "unique_all_vessel_ids",
    "VESSEL_VESSEL_ID",
    "weeks_perm_2022_amnt")

# print_df_names(v_trip_neg_2022_w_y_cnt_u)
# print_df_names(v_trips_info_2022_int_ah_w_y_weeks_cnt_u)

v_trip_neg_2022_w_y_cnt_u_short <-
  v_trip_neg_2022_w_y_cnt_u |> 
  select(-any_of(v_names_to_rm)) |> 
  select(-any_of(t_names_to_rm)) |> 
  distinct()

v_trips_info_2022_int_ah_w_y_weeks_cnt_u_short <-
  v_trips_info_2022_int_ah_w_y_weeks_cnt_u |> 
  select(-any_of(v_names_to_rm)) |> 
  select(-any_of(t_names_to_rm)) |> 
  distinct()

## combine trips and trip_negative week cnts (logbooks + DNFs) ----
# intersect(names(v_trips_info_2022_int_ah_w_y),
#          names(v_trip_neg_2022_w_y)) |> 
#   paste(sep = ", ") |> cat()

tic("t__tne_22")
t__tne_22 <-
  full_join(
    v_trips_info_2022_int_ah_w_y_weeks_cnt_u_short,
    v_trip_neg_2022_w_y_cnt_u_short,
    join_by(VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".t", ".tne")
  )
toc()
# t__tne_22: 35.96 sec elapsed
# t__tne_22: 38.8 sec elapsed
# t__tne_22: 14.22 sec elapsed fewer cols

dim(t__tne_22)
# [1] 838720     99
# [1] 823051     99 (sero t only)
# [1] 844068    119
# [1] 8785739     103
# [1] 8785739      41

# 
# t__tne__dates_w_cnt_t_tne_short <-
#   t__tne__dates_w_cnt_t_tne |>
#   select(-c
#     YEAR,
#     MONTH_OF_YEAR,
#     WEEK_OF_YEAR,
#     COMPLETE_DATE,
#     TRIP_ID.t,
#     VESSEL_ID,
#     TRIP_END_DATE,
#     SERO_VESSEL_PERMIT,
#     trip_int,
#     TRIP_START_week_num,
#     TRIP_END_week_num,
#     TRIP_START_y,
#     TRIP_END_y,
#     TRIP_START_m,
#     TRIP_END_m,
#     TRIP_ID.tne,
#     TRIP_week_num,
#     TRIP_DATE_y,
#     TRIP_DATE_m,
#     distinct_start_weeks_t,
#     distinct_end_weeks_t,
#     max_weeks_cnt_t,
#     distinct_weeks_ne
#   ) |>
#   distinct()

# dim(t__tne__dates)
# [1] 838720     95
# [1] 823051     95

# dim(t__tne__dates_w_cnt_t_tne)
# [1] 838720     99
# [1] 823051     99

# dim(t__tne__dates_w_cnt_t_tne_short)
# [1] 838720     23
# [1] 823051     23

# View(t__tne__dates_w_cnt_t_tne_short)

## join v_p and t_tne ----
sa_v_p_t_tne_by <-
  dplyr::join_by(VESSEL_VESSEL_ID == VESSEL_ID)

# print_df_names(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa)
tic("sa_v_p_t_tne")
sa_v_p_t_tne <-
  full_join(
    vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_sa,
    t__tne_22,
    sa_v_p_t_tne_by,
    relationship = "many-to-many",
    suffix = c(".v_p", ".t_tne")
  )
toc()
# sa_v_p_t_tne: 86.07 sec elapsed
# sa_v_p_t_tne: 26.5 sec elapsed fewer cols

dim(sa_v_p_t_tne)
# [1] 825558     42
# [1] 825335     28
# [1] 51984236       50
# [1] 14730564      60

# print_df_names(sa_v_p_t_tne)

## Count weeks ----
### permit, but no t/tne ----

sa_v_p_t_tne |> 
  head() |> View()


# v_p_t_tne_dates |> 
sa_v_p_t_tne2 |> 
  filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |> 
  dim()
# [1] 2567   42

v_p_t_tne_dates_cnts__no_t__no_tne <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID)) |>
  # no t or tne
  filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
  group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  mutate(no_t_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR))

dim(v_p_t_tne_dates_cnts__no_t__no_tne)
# [1] 2402   43

### permit, t xor tne ----

v_p_t_tne_dates_cnts__t_xor_tne <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID)) |>
  # t xor tne
  filter((is.na(TRIP_ID.t) | is.na(TRIP_ID.tne)) &
           # but not both are absent
           !(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne))) |>
           group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
           mutate(t_xor_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR))
         
# a <- NA
# a0 <- NA
# a1 <- 1
# a2 <- 2
# 
# a1 | a2 T
# a | a1 T
# a | a0 NA

# is.na(a) | is.na(a0) T
# is.na(a) | is.na(a1) T
# is.na(a1) | is.na(a2) F

dim(v_p_t_tne_dates_cnts__t_xor_tne)
# [1] 819818     43
# [1] 817416     43 (rm both are NAs)

# v_p_t_tne_dates_cnts__t_xor_tne |> 
#   filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |> dim()
# [1] 2402   43


### permit, t and tne ----

v_p_t_tne_dates_cnts__t_and_tne <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID)) |>
  # t and tne
  filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
  group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  mutate(t_and_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR))

View(v_p_t_tne_dates_cnts__t_and_tne)
# [1] 5575   43

# df2 <- 
  # df %>%
  # mutate(x = if_else(B > 100, A, A),
  #        Xi = if_else(B > 100,  x*0.1 + A, A),
  #        Xii = if_else(B > 100,  x*0.5 + A, A),
  #        Xiii = if_else(B > 100,  x*0.9 + A, A))

# all 3 conditions together ----
  # left_join(dt1,
  #           dt1 %>% 
  #                filter(x2==1) %>%
  #                group_by(x) %>%
  #                summarise(a=mean(y)), by='x') %>%
  #                mutate(z=y/a)%>%
  #                head()

v_p_t_tne_dates_w_v_p <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID))

# 1) # no t or tne
v_p_t_tne_dates_w_v_p__cnts__t_tne <-
  left_join(
    v_p_t_tne_dates_w_v_p,
    v_p_t_tne_dates_w_v_p |>
      # no t or tne
      filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
      group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
      mutate(no_t_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR)) |>
      ungroup()
  )
dim(v_p_t_tne_dates_w_v_p__cnts__t_tne)
# [1] 825393     43

# 2) t xor tne
v_p_t_tne_dates_w_v_p__cnts__t_tne <-
  left_join(
    v_p_t_tne_dates_w_v_p__cnts__t_tne,
    v_p_t_tne_dates_w_v_p__cnts__t_tne |>
      # t xor tne, # but not both are absent
      filter(
        (is.na(TRIP_ID.t) | is.na(TRIP_ID.tne)) &
        !(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne))
        ) |>
      group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
      mutate(t_xor_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR)) |>
      ungroup()
  )

dim(v_p_t_tne_dates_w_v_p__cnts__t_tne)
# [1] 825393     44

# 3) t and tne
v_p_t_tne_dates_w_v_p__cnts__t_tne <-
  left_join(
    v_p_t_tne_dates_w_v_p__cnts__t_tne,
    v_p_t_tne_dates_w_v_p__cnts__t_tne |>
      # t and tne
      filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
      group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
      mutate(t_and_tne_weeks_amnt =
               n_distinct(WEEK_OF_YEAR, YEAR)) |>
      ungroup()
  )

### check numbers ----
dim(v_p_t_tne_dates_w_v_p__cnts__t_tne)
# [1] 825393     45

# 
v_p_t_tne_dates_w_v_p__cnts__t_tne |>
  select(contains("vessel_id")) |>
  # select(contains("vessel")) |> # [1] 8322    5
  distinct() |>
  dim()
# 7059 3

v_p_t_tne_dates_w_v_p |>
  # select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  select(contains("vessel_id")) |>
  distinct() |>
  dim()
# [1] 7059    3

v_p_t_tne_dates_w_v_p__cnts__t_tne |>
  # select(VESSEL_VESSEL_ID) |>  # [1] 5603    1
  # select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  select(contains("vessel_id")) |>
  distinct() |>
  dim()
# [1] 7059    3

v_p_t_tne_dates |>
  # select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  select(contains("vessel_id")) |>
  distinct() |>
  dim()
# 7060 3

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |> 
  # select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  select(contains("vessel_id")) |>
  distinct() |>
  dim()
# 3181 3

t__tne__dates_w_cnt_t_tne_short |>
  select(contains("vessel_id")) |>
  distinct() |>
  dim()
# 3880

# TODO: join by another fields
# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 + t__tne__dates_w_cnt_t_tne_short
# 3181 +3880
# 7061
