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
  dplyr::mutate(my_end_date =
           dplyr::case_when((END_DATE < EFFECTIVE_DATE) &
                       (EXPIRATION_DATE > EFFECTIVE_DATE)
                     ~ EXPIRATION_DATE,
                     .default =
                       max(END_DATE,                                     EXPIRATION_DATE,
                           na.rm = T)
           )) %>%
  # dplyr::select(-c(END_DATE,
            # EXPIRATION_DATE)) %>%
  dplyr::ungroup() |> 
  dplyr::distinct()
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
  dplyr::mutate(all_ids = list(
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
  dplyr::mutate(unique_all_vessel_ids = list(na.omit(unique(all_ids)))) |>
  dplyr::ungroup()
toc()
# uid: 1.63 sec elapsed

dim(vessels_permits_2022_r_end_date_uid)
# [1] 20231    55

### fewer fields ----
vessels_permits_2022_r_end_date_uid_short <-
  vessels_permits_2022_r_end_date_uid |>
  dplyr::select(
    EFFECTIVE_DATE,
    END_DATE,
    EXPIRATION_DATE,
    permit_sa_gom,
    my_end_date,
    unique_all_vessel_ids
  ) |> 
  dplyr::distinct()

dim(vessels_permits_2022_r_end_date_uid_short)
# [1] 9442    6

## get the earliest and the latest permit dates ----
# print_df_names(vessels_permits_2022_r_end_date_uid_short)
vessels_permits_2022_r_end_date_uid_short_mm <-
  vessels_permits_2022_r_end_date_uid_short |>
  dplyr::group_by(unique_all_vessel_ids, permit_sa_gom) |>
  dplyr::mutate(
    min_permit_eff_date = min(EFFECTIVE_DATE),
    max_permit_end_date = max(my_end_date)
  ) |>
  dplyr::ungroup()

dim(vessels_permits_2022_r_end_date_uid_short_mm)
# [1] 9442   8

# vessels_permits_2022_r_end_date_uid_short_mm |>
#   dplyr::filter(grepl('FL8701TB', unique_all_vessel_ids)) |> View()
# 2023 is here, ok

# vessels_permits_2022_r_end_date_uid_short_mm |>
#   dplyr::filter(grepl('FL9004NX', unique_all_vessel_ids)) |>
#   View()
# diff sa / gom


## add weeks and months ----

vessels_permits_2022_r_end_date_uid_short_mm_w_y <-
  vessels_permits_2022_r_end_date_uid_short_mm |>
    dplyr::mutate(
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
  dplyr::mutate(
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
# dplyr::glimpse(vessels_permits_2022_r_end_date_uid_short_mm_w_y)
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y %>%
  dplyr::group_by(permit_sa_gom, unique_all_vessel_ids) |>
  dplyr::mutate(eff_int =
           lubridate::interval(min_permit_eff_date,
                               max_permit_end_date)) |> 
  dplyr::ungroup()
toc()
# get permit periods: 46.29 sec elapsed
# get permit periods: 48.8 sec elapsed

# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |>
#   dplyr::filter(grepl('FL9004NX', unique_all_vessel_ids)) |>
#   View()

## mark dual ----
# Dual only if GOM and SA for the same period
# dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv)

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |>
  # for each vessel and permit in effect interval
  dplyr::group_by(unique_all_vessel_ids, eff_int) |>
  # create a list of all permit regions
  dplyr::mutate(all_permit_sa_gom = list(na.omit(unique(permit_sa_gom)))) |>
  # get the length of each list of permits
  dplyr::mutate(all_permit_sa_gom_size = lengths(all_permit_sa_gom)) |>
  # if there are both sa and gom mark as dual,
  # otherwise keep the original permit region
  dplyr::mutate(permit_sa_gom_dual =
           dplyr::case_when(all_permit_sa_gom_size > 1 ~                                              "dual",
                     .default = permit_sa_gom)) |>
  # remove temporary columns
  dplyr::select(-c(all_permit_sa_gom, all_permit_sa_gom_size)) |>
  dplyr::ungroup()

### check ----
dim(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual)
# [1] 9442   16

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  dplyr::filter(grepl("FL8701TB|FL3610NF|FL9004NX", unique_all_vessel_ids)) |>
  dplyr::select(unique_all_vessel_ids,
         permit_sa_gom_dual) |>
  dplyr::distinct() |>
  dplyr::glimpse()
# $ unique_all_vessel_ids <list> <"FL3610NF", "328460">, <"FL8701TB", …
# $ permit_sa_gom_dual    <chr> "dual", "sa_only", "gom_only", "sa_only"
# FL9004NX in both, but not dual, bc intervals do not overlap

new_dual_ids <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |> 
  dplyr::filter(permit_sa_gom_dual == "dual") |> 
  dplyr::select(unique_all_vessel_ids) |> 
  dplyr::distinct()

dim(new_dual_ids)
# [1] 275   1

# old_dual_v_ids <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual |> 
#   dplyr::filter(permit_sa_gom == "dual") |> 
#   dplyr::select(unique_all_vessel_ids) |> 
#   dplyr::distinct()

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
# dplyr::glimpse(in_new_dual_only)
# 10

# in_new_dual_only1 <-
#   setdiff(new_dual_ids$unique_all_vessel_ids,
#           old_dual_v_ids$unique_all_vessel_ids)
# length(in_new_dual_only1)
# 0
# in_old_only <-
#   setdiff(old_dual_v_ids$unique_all_vessel_ids,
#         new_dual_ids$unique_all_vessel_ids)
# dplyr::glimpse(in_old_only)
# 82

#### why not in new? ----

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |> 
  dplyr::filter(grepl("FL2995SR", unique_all_vessel_ids)) |>
  dplyr::select(eff_int, permit_sa_gom_dual) |> 
  head()
  # eff_int                            permit_sa_gom_dual
#   <Interval>                                       <chr>             
# 1 2022-03-02 23:00:00 EST--2023-01-30 23:00:00 EST gom_only          
# 2 2022-10-19 00:00:00 EDT--2024-01-30 23:00:00 EST sa_only           
# gom and sa periods overlap

#### why not in old? ----
# dplyr::glimpse(in_new_dual_only)
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_dual |> 
#   dplyr::filter(grepl("FL9004NX", unique_all_vessel_ids)) |>
#   dplyr::glimpse()
# $ permit_sa_gom           <chr> "sa_only", "gom_only"

# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual |> 
#   dplyr::filter(grepl("FL9004NX", unique_all_vessel_ids)) |>
#   dplyr::glimpse()
# $ permit_sa_gom               <chr> "gom_only", "sa_only"

# vessels_permits_2022_r |> 
#   # dplyr::filter(PERMIT_VESSEL_ID == "FL9004NX") |>
#   dplyr::filter(PERMIT_VESSEL_ID == "TX6550AU") |>
#   dplyr::distinct() |> 
#   dplyr::glimpse()

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
#   dplyr::filter(PERMIT_VESSEL_ID %in% vessels_dual_maybe) |> 
#   View()

# vessels_permits_2022_r_10_dual_maybe <-
#   vessels_permits_2022_r |>
#   dplyr::filter(PERMIT_VESSEL_ID %in% vessels_dual_maybe) |>
#   dplyr::select(PERMIT_VESSEL_ID, EFFECTIVE_DATE,
#          END_DATE,
#          PERMIT_STATUS,
#          permit_sa_gom) |>
#   dplyr::distinct()

# write_csv(vessels_permits_2022_r_10_dual_maybe,
#           "vessels_permits_2022_dual_maybe.csv")

## split by permit to get dual in another way ----
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y %>%
#   split(as.factor(vessels_permits_2022_r_end_date_uid_short_mm_w_y$permit_sa_gom))
# 
# purrr::map_df(vessels_permits_2022_r_end_date_uid_short_mm_w_y_l, dim)
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
#   dplyr::select(unique_all_vessel_ids) %>%
#   dplyr::distinct() %>%
#   dim()
# # [1] 5461    1
# 
# vessels_permits_2022 %>%
#   # dplyr::select(QCSJ_C000000000300000
#   # [1] 5461    1
#   # dplyr::select(QCSJ_C000000000300001
#   # [1] 5461    1
#   dplyr::select(QCSJ_C000000000300000,
#          QCSJ_C000000000300001,
#          VESSEL_ALT_NUM) %>%
#   dplyr::distinct() %>%
#   dim()
# # [1] 5462    3 (+NA)
# 
 ### add "dual" to intervals ----
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join %>%
#   dplyr::mutate(permit_sa_gom =
#            dplyr::case_when(
#              !is.na(permit_sa_gom.sa) &
#                !is.na(permit_sa_gom.gom) ~ "dual",
#              .default =
#                dplyr::coalesce(permit_sa_gom.sa,
#                                permit_sa_gom.gom)
# 
#            )) |> 
#   dplyr::ungroup()
# 
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual %>%
#   dplyr::select(permit_sa_gom) %>%
#   dplyr::distinct()
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
# # dplyr::filter(!is.na(permit_sa_gom.sa))
# 
# tic("vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22")
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22 <-
#   vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual %>%
#   dplyr::mutate(
#     eff_int_gom =
#       lubridate::interval(EFFECTIVE_DATE.gom,
#                           my_end_date.gom),
#     eff_int_sa =
#       lubridate::interval(EFFECTIVE_DATE.sa,
#                           my_end_date.sa)
#   ) %>% 
#   dplyr::filter(int_overlaps(eff_int_gom,
#                       interval_2022) |
#            int_overlaps(eff_int_sa,
#                       interval_2022)
#          )
# toc()
# # vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22: 0.24 sec elapsed
# 
#### check ----
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22 %>%
#   dplyr::select(permit_sa_gom) %>%
#   dplyr::distinct()
# # all 3
# 
# vessels_permits_2022_r_end_date_uid_short_mm_w_y_l_overlap_join_w_dual_22 %>%
#   dplyr::filter(permit_sa_gom == "dual") %>%
#   dplyr::select(unique_all_vessel_ids) %>%
#   dplyr::distinct() %>%
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
  purrr::map(function(reg_name) {
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
  dplyr::select(-starts_with("EXPIRATION_DATE"),
    -starts_with("END_DATE"),
    -permit_sa_gom
  ) |>
  dplyr::distinct() |> dim()
  # dplyr::filter(grepl("FL3610NF", unique_all_vessel_ids)) |> 
  # View()
# [1] 472  13


# vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual |> 
#   dplyr::filter(grepl("FL3610NF", unique_all_vessel_ids)) |> 
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
## add trip interval ----

trips_info_2022_int <-
  trips_info_2022 %>%
  dplyr::mutate(trip_int =
           lubridate::interval(
             lubridate::floor_date(TRIP_START_DATE,
                                   unit = "day"),
             lubridate::floor_date(TRIP_END_DATE,
                                   unit = "day")
           ))

### check trips_info_2022_int ----
trips_info_2022_int %>%
  dplyr::select(TRIP_START_DATE, TRIP_END_DATE, trip_int) %>%
  dim()
# [1] 98528     3

## trip types A and H trips ----
trips_info_2022_int_ah <-
  trips_info_2022_int %>%
  dplyr::filter(TRIP_TYPE %in% c("A", "H"))

# Trip notifications (= declarations) ----
## trip types A and H trip_notif ----
trip_notifications_2022 %>%
   dplyr::select(TRIP_TYPE) %>% dplyr::distinct()
#     TRIP_TYPE
# 1           H
# 3           A
# 383         R
# 697         C

trip_notifications_2022_ah <-
  trip_notifications_2022 %>%
  dplyr::filter(TRIP_TYPE %in% c("A", "H"))

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
  dplyr::mutate(
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
  dplyr::mutate(
    TRIP_START_week_num =
      as.double(TRIP_START_week_num),
    TRIP_END_week_num =
      as.double(TRIP_END_week_num)
  )

# str(trips_info_2022_int_ah_w_y)

## to trip notifications ----
trip_notifications_2022_ah_w_y <-
  trip_notifications_2022_ah %>%
  dplyr::mutate(
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
  dplyr::mutate(
    TRIP_START_week_num =
      as.double(TRIP_START_week_num),
    TRIP_END_week_num =
      as.double(TRIP_END_week_num)
  )

## to negative trips ----
# print_df_names(trip_neg_2022)
trip_neg_2022_w_y <-
  trip_neg_2022 %>%
  dplyr::mutate(
    TRIP_week_num =
      strftime(TRIP_DATE, format = "%U"),
    TRIP_DATE_y =
      year(TRIP_DATE),
    TRIP_DATE_m =
      zoo::as.yearmon(TRIP_DATE)
  ) %>%
  dplyr::mutate(TRIP_week_num =
           as.double(TRIP_week_num))

# results:
# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list
# trips_info_2022_int_ah_w_y
# trip_neg_2022_w_y
# trip_notifications_2022_ah_w_y
# end of data preparations ----

# Count distinct weeks per vessel ----

## neg trip weeks ----
# TODO ?do we need a join?

# print_df_names(trip_neg_2022_w_y)
trip_neg_2022_w_y_cnt_u <-
  trip_neg_2022_w_y |>
  dplyr::group_by(VESSEL_ID) %>%
  summarise(distinct_weeks_ne = n_distinct(TRIP_week_num))

dim(trip_neg_2022_w_y_cnt_u)
# [1] 1709    5
# [1] 3414    2

## trip_notif weeks count per vessel ----
trip_notifications_2022_ah_w_y_cnt_u <-
  trip_notifications_2022_ah_w_y |>
  dplyr::group_by(VESSEL_ID) |>
  summarise(
    distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
    distinct_end_weeks_tn = n_distinct(TRIP_END_week_num)
  )

dim(trip_notifications_2022_ah_w_y_cnt_u)
# [1] 914   3

trip_notifications_2022_ah_w_y_cnt_u %>%
   dplyr::filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
   dim()
# [1] 0 6
# ok
# [1] 57  3
# TODO: why?

## trips weeks count per vessel ----
# View(trips_info_2022_int_ah_w_y)
trips_info_2022_int_ah_w_y_weeks_cnt_u <-
  trips_info_2022_int_ah_w_y %>%
    # browser()
    dplyr::group_by(VESSEL_ID) %>%
      summarise(
        distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
        distinct_end_weeks_t = n_distinct(TRIP_END_week_num)
      ) %>%
      dplyr::mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))

dim(trips_info_2022_int_ah_w_y_weeks_cnt_u)
# [1] 1110    7
# [1] 1934    4

trips_info_2022_int_ah_w_y_weeks_cnt_u %>%
   dplyr::filter(!distinct_start_weeks_t == distinct_end_weeks_t) %>%
   dim()
# 27
# 63

# Keep only sero permitted ----
trips_info_2022_int_ah_w_y_sero <-
  trips_info_2022_int_ah_w_y |>
  dplyr::filter(!is.na(SERO_VESSEL_PERMIT)) |>
  dplyr::distinct()

# no:
# trip_notifications_2022_ah_w_y_dates |>  dplyr::filter(!is.na(SERO_VESSEL_PERMIT)) |> dim()
# trip_neg_2022_w_y_dates |>  dplyr::filter(!is.na(SERO_VESSEL_PERMIT)) |> dim()

# Join dates_2022 and everything ----
# get_v_ids and dates only

View(dates_2022)
## p_v ----

### split all permit intervals by day ? ----
# View(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list)

# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual |> 
#   dplyr::group_by(unique_all_vessel_ids) |> 
#   dplyr::mutate(union_int_sa = )

### end split all permit intervals by day ----

map_df(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1  3281     3673   13749
# 2   116      116     116
# 1   635     1879    6308
# 2    33       33      33
# 1   653     1940    6356
# 2    37       37      37
# 2    26       26      26
# 1   917     2066    6459
# 2    16       16      16

l_names <- 
  names(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list)
# [1] "dual"     "gom_only" "sa_only"

# str_split("sa_only", "_")
# str_split("saonly", "_")
my_f <- function(curr_permit_region) {
  # browser()
  curr_df <-
    vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list[[curr_permit_region]]

  curr_permit_s_g_all <- curr_df |>
    dplyr::select(permit_sa_gom_dual) |>
    dplyr::distinct()
  
  # treat dual as gom for 2022
  if (curr_permit_s_g_all == "dual") {
    curr_permit_s_g <- "gom"
  } else {
    curr_permit_s_g_all0 <- str_split(curr_permit_s_g_all, "_")
    curr_permit_s_g <- curr_permit_s_g_all0[[1]][[1]]
  }
  
  curr_f_name <- paste0("EFFECTIVE_DATE.", curr_permit_s_g)
  
  res <-
    left_join(dates_2022,
              curr_df,
              # TODO:  change so not to loose data!
              join_by(COMPLETE_DATE == !!sym(curr_f_name)))
  
  return(res)
}

# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates <-
  # purrr::map(l_names, my_f)

# names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates) <- l_names

### check v_ dates join ---- 
# purrr::map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   648     1271    3615
# 2    36       36      36
# 1   657     1267    3616
# 2    40       40      40
# 2    29       29      29

# data_overview(dates_2022)
# COMPLETE_DATE 427

data_overview(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$dual)
# PERMIT_VESSEL_ID            353, 357
# VESSEL_VESSEL_ID            same
# EFFECTIVE_DATE.gom          232, 234
# EFFECTIVE_DATE.sa           230, 232
# unique_all_vessel_ids       357
# ---
# unique_all_vessel_ids   275
# EFFECTIVE_DATE          201

# data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates$dual)
# COMPLETE_DATE               427
# PERMIT_VESSEL_ID            293
# VESSEL_VESSEL_ID            293
# SERO_OFFICIAL_NUMBER.gom    293

## t ----
# print_df_names(trips_info_2022_int_ah_w_y)

# TODO: or end_date? check the difference for output week counts
t_by <- join_by(COMPLETE_DATE == TRIP_START_DATE)

trips_info_2022_int_ah_w_y_dates <-
  left_join(dates_2022,
            trips_info_2022_int_ah_w_y_sero,
            t_by)

### check numbers ----
# data_overview(trips_info_2022_int_ah_w_y_dates)
# COMPLETE_DATE                 427
# VESSEL_ID                    1935
# data_overview(trips_info_2022_int_ah_w_y)
# VESSEL_ID                    1934

trips_info_2022_int_ah_w_y_dates_ids <-
  trips_info_2022_int_ah_w_y_dates |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct()

trips_info_2022_int_ah_w_y_ids <-
  trips_info_2022_int_ah_w_y_sero |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct()

setdiff(trips_info_2022_int_ah_w_y_ids, trips_info_2022_int_ah_w_y_dates_ids)
# 0

## tne ----
# print_df_names(trip_neg_2022_w_y)
tne_by <- join_by(COMPLETE_DATE == TRIP_DATE)
trip_neg_2022_w_y_dates <-
  left_join(dates_2022,
            trip_neg_2022_w_y,
            tne_by)

count_uniq_by_column(trip_neg_2022_w_y)
# VESSEL_ID       3414

count_uniq_by_column(trip_neg_2022_w_y_dates)
# COMPLETE_DATE    427
# VESSEL_ID       3415

# why more vessel_ids? +NA

## tn ----

tn_by <- join_by(COMPLETE_DATE == TRIP_START_DATE)
trip_notifications_2022_ah_w_y_dates <-
  left_join(dates_2022,
            trip_notifications_2022_ah_w_y,
            tn_by)

count_uniq_by_column(trip_notifications_2022_ah_w_y)
# VESSEL_ID                      914
# TRIP_START_DATE                378

count_uniq_by_column(trip_notifications_2022_ah_w_y_dates)
# COMPLETE_DATE                  427
# VESSEL_ID                      915
# TRIP_START_DATE                378
# VESSEL_ID                      914

# dates and everethyng results:
# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates
# trip_notifications_2022_ah_w_y_dates
# trip_neg_2022_w_y_dates
# trips_info_2022_int_ah_w_y_dates

# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance

# ## keep only sero permitted
# 
# trip_notifications_2022_ah_w_y_dates_s <-
#   trip_notifications_2022_ah_w_y_dates |> 
#   dplyr::filter(!is.na())
# data_overview(trips_info_2022_int_ah_w_y_dates)
# 
# trip_neg_2022_w_y_dates
# data_overview(trips_info_2022_int_ah_w_y_dates)

## add 2022 SA period weeks amnt to v permit ----
# vessels_permits_2022 |> View()
#   dplyr::filter(PERMIT_VESSEL_ID == 'FL8701TB') |> View()
# vessels_permits_2022 |>
#   dplyr::filter(SERO_OFFICIAL_NUMBER == 'FL8701TB') |> View()
# TODO: fix end in 2023 disappeared

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates$sa_only |>
  # remove gom, keep sa only
  dplyr::select(-ends_with("gom")) |> 
  dplyr::mutate(permit_2022 =
           lubridate::intersect(eff_int_sa,
                                interval_2022)) |>
  dplyr::mutate(weeks_perm_2022_amnt =
           (permit_2022 / lubridate::dweeks(1)) |>
           round()) |>
  dplyr::distinct()

# View(vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list)

sa_v_p <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual__list$sa_only

# print_df_names(sa_v_p)
# EFFECTIVE_DATE, END_DATE, EXPIRATION_DATE, permit_sa_gom, my_end_date, unique_all_vessel_ids, min_permit_eff_date, max_permit_end_date, EFFECTIVE_DATE_week_num, my_end_week_num, EFFECTIVE_DATE_y, my_end_y, EFFECTIVE_DATE_m, my_end_m, eff_int, permit_sa_gom_dual

sa_v_p_short <-
  sa_v_p |> 
  dplyr::select(unique_all_vessel_ids,
         eff_int)

sa_v_p_short_22 <-
  sa_v_p_short |> 
  dplyr::mutate(eff_int_22 =
           lubridate::intersect(eff_int,
                                   interval_2022)) |> 
  dplyr::distinct()
# str(sa_v_p_short_22)
# [1] 6459    3
# [1] 3956    3 distinct


sa_v_p_short_22_w_amnt <-
  sa_v_p_short_22 |>
  dplyr::group_by(unique_all_vessel_ids) |>
  dplyr::mutate(week_amnt =
           time_length(eff_int_22, "week")) |> 
  dplyr::ungroup()

dim(sa_v_p_short_22_w_amnt)
# [1] 3956    4

min(sa_v_p_short_22_w_amnt$week_amnt, na.rm = T)
# [1] 0.1190476
max(sa_v_p_short_22_w_amnt$week_amnt, na.rm = T)
# 52
# 0-109 (without interval_2022)
# 0-52

data_overview(sa_v_p_short_22_w_amnt)
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

## combine trips and trip_negative week cnts (logbooks + DNFs) ----
# trips_info_2022_int_ah_w_y_weeks_cnt_u
# trip_notifications_2022_ah_w_y_cnt_u
# trip_neg_2022_w_y_cnt_u
# intersect(
# names(trips_info_2022_int_ah_w_y_weeks_cnt_u),
# names(trip_neg_2022_w_y_cnt_u))
# VESSEL_ID

t__tne__dates <-
  full_join(
    trips_info_2022_int_ah_w_y_dates,
    trip_neg_2022_w_y_dates,
    join_by(YEAR,
            MONTH_OF_YEAR,
            WEEK_OF_YEAR,
            COMPLETE_DATE,
            VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".t", ".tne")
  )

t__tne__dates_w_cnt_t <-
  left_join(t__tne__dates,
            trips_info_2022_int_ah_w_y_weeks_cnt_u,
            dplyr::join_by(VESSEL_ID))

t__tne__dates_w_cnt_t_tne <-
  left_join(t__tne__dates_w_cnt_t,
            trip_neg_2022_w_y_cnt_u,
            dplyr::join_by(VESSEL_ID))
  
dim(t__tne__dates_w_cnt_t_tne)
# [1] 838720     99
# [1] 823051     99 (sero t only)

t__tne__dates_w_cnt_t_tne |> 
  dplyr::filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
  dim()
# [1] 5971   99
# [1] 5575   99

## fewer columns t__tne__dates_w_cnt_t_tne ----
# print_df_names(t__tne__dates_w_cnt_t_tne)

t__tne__dates_w_cnt_t_tne_short <-
  t__tne__dates_w_cnt_t_tne |>
  dplyr::select(
    YEAR,
    MONTH_OF_YEAR,
    WEEK_OF_YEAR,
    COMPLETE_DATE,
    TRIP_ID.t,
    VESSEL_ID,
    TRIP_END_DATE,
    SERO_VESSEL_PERMIT,
    trip_int,
    TRIP_START_week_num,
    TRIP_END_week_num,
    TRIP_START_y,
    TRIP_END_y,
    TRIP_START_m,
    TRIP_END_m,
    TRIP_ID.tne,
    TRIP_week_num,
    TRIP_DATE_y,
    TRIP_DATE_m,
    distinct_start_weeks_t,
    distinct_end_weeks_t,
    max_weeks_cnt_t,
    distinct_weeks_ne
  ) |>
  dplyr::distinct()

dim(t__tne__dates)
# [1] 838720     95
# [1] 823051     95

dim(t__tne__dates_w_cnt_t_tne)
# [1] 838720     99
# [1] 823051     99

dim(t__tne__dates_w_cnt_t_tne_short)
# [1] 838720     23
# [1] 823051     23

View(t__tne__dates_w_cnt_t_tne_short)

## Compare week counts t/tne ----

# both t & tne
t__tne__dates_w_cnt_t_tne_short |>
  dplyr::filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |> 
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct() |> 
  dim()
# [1] 5971   23
# [1] 515   1 dplyr::select(VESSEL_ID) |>
# 481 sero only

# t only
t__tne__dates_w_cnt_t_tne_short |>
  dplyr::filter(!is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct() |>
  dim()
# [1] 91099    23
# [1] 1834    1 dplyr::select(VESSEL_ID) |>
# 1612    sero only

# tne only
# View(t__tne__dates_w_cnt_t_tne_short)
t__tne__dates_w_cnt_t_tne_short |>
  dplyr::filter(is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |> 
  dplyr::select(VESSEL_ID) |> 
  dplyr::distinct() |> 
  dim()
# [1] 741588     23
# [1] 3412    1   dplyr::select(VESSEL_ID) |> 

# View(t__tne__dates_w_cnt_t_tne_short)

t__tne__dates_w_cnt_t_tne_short |>
  dplyr::filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |> 
  dplyr::select(VESSEL_ID) |> 
  dplyr::distinct()
# 1 NA

# 1834 + 515 + 3412
# [1] 5761? it's more than total vessels
# TODO: why?
# 1834 + 3412 = 5246


# split ids to easier merge ----
# data %>% unnest_wider(ListCol)
sa_v_p_short_22_w_amnt_all_ids_long <-
  sa_v_p_short_22_w_amnt |>
  unnest_auto(unique_all_vessel_ids)
# Using `unnest_longer(unique_all_vessel_ids, indices_include = FALSE)`; no element has names

sa_v_p_short_22_w_amnt_all_ids_wide <-
  sa_v_p_short_22_w_amnt |>
  unnest_wider(unique_all_vessel_ids, names_sep = "_")

dim(sa_v_p_short_22_w_amnt_all_ids_long)
# [1] 8067    4

dim(sa_v_p_short_22_w_amnt_all_ids_wide)
# [1] 3956    6

## combine dates with v_p, t, tne ----
# to see the weeks with no reports

print_df_names(sa_v_p_short_22_w_amnt_all_ids_wide)
# [1] "unique_all_vessel_ids_1, unique_all_vessel_ids_2, unique_all_vessel_ids_3, eff_int, eff_int_22, week_amnt"

dim(sa_v_p_short_22_w_amnt)
# [1] 3615   22
# [1] 3616   24
# [1] 3956    4

# v_p_t_tne_dates_by = join_by(YEAR,
#             MONTH_OF_YEAR,
#             WEEK_OF_YEAR,
#             COMPLETE_DATE,
#             PERMIT_VESSEL_ID == VESSEL_ID)

print_df_names(t__tne__dates_w_cnt_t_tne_short)
# YEAR, MONTH_OF_YEAR, WEEK_OF_YEAR, COMPLETE_DATE, TRIP_ID.t, VESSEL_ID, TRIP_END_DATE, SERO_VESSEL_PERMIT, trip_int, TRIP_START_week_num, TRIP_END_week_num, TRIP_START_y, TRIP_END_y, TRIP_START_m, TRIP_END_m, TRIP_ID.tne, TRIP_week_num, TRIP_DATE_y, TRIP_DATE_m, distinct_start_weeks_t, distinct_end_weeks_t, max_weeks_cnt_t, distinct_weeks_ne


# df %>% dplyr::distinct(var1, var2, .keep_all = TRUE)

t__tne__dates_w_cnt_t_tne_short %<>%
  dplyr::mutate(VESSEL_ID = as.character(VESSEL_ID))

#### which id ----
t_tne_ids <-
  t__tne__dates_w_cnt_t_tne_short$VESSEL_ID |> 
  unique()
length(t_tne_ids)
# [1] 3880

sa_v_p_short_22_w_amnt_all_ids_wide |>
  dplyr::select(starts_with("unique_all_vessel_ids")) |>
  purrr::map(function(ids) {
    intersect(ids,
              t__tne__dates_w_cnt_t_tne_short$VESSEL_ID) |>
      length() %>%
      return()
  })

# $unique_all_vessel_ids_1
# [1] 0
# 
# $unique_all_vessel_ids_2
# [1] 1672
# 
# $unique_all_vessel_ids_3
# [1] 69

sa_v_p_short_22_w_amnt_all_ids_wide |> 
  dplyr::filter(!unique_all_vessel_ids_2 %in% t_tne_ids) |> 
#   data_overview()
# unique_all_vessel_ids_1 2284
# unique_all_vessel_ids_2 2282
# unique_all_vessel_ids_3  156
  dplyr::filter(unique_all_vessel_ids_3 %in% t_tne_ids) |>
  dim()
#   data_overview()
# unique_all_vessel_ids_1 2197
# unique_all_vessel_ids_2 2196
# unique_all_vessel_ids_3   69


##### is the rest gom/dual? 
vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_ids <-
  vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual |>
  # print_df_names()
  unnest_longer(unique_all_vessel_ids) |>
  dplyr::select(unique_all_vessel_ids) |>
  dplyr::distinct()

intersect(t__tne__dates_w_cnt_t_tne_short$VESSEL_ID,
          vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv_dual_ids$unique_all_vessel_ids) |> 
  length()
# [1] 2428

# length(unique(t__tne__dates_w_cnt_t_tne_short$VESSEL_ID))
# [1] 3880



## join v_p and t_tne ----
sa_v_p_t_tne_by1 <-
  dplyr::join_by(unique_all_vessel_ids_2 == VESSEL_ID)

sa_v_p_t_tne1 <-
  full_join(
    sa_v_p_short_22_w_amnt_all_ids_wide,
    t__tne__dates_w_cnt_t_tne_short,
    sa_v_p_t_tne_by1,
    relationship = "many-to-many",
    suffix = c(".v_p", ".t_tne")
  )

# print_df_names(sa_v_p_t_tne1)
# [1] "unique_all_vessel_ids_1, unique_all_vessel_ids_2, unique_all_vessel_ids_3, eff_int, eff_int_22, week_amnt, YEAR, MONTH_OF_YEAR, WEEK_OF_YEAR, COMPLETE_DATE, TRIP_ID.t, TRIP_END_DATE, SERO_VESSEL_PERMIT, trip_int, TRIP_START_week_num, TRIP_END_week_num, TRIP_START_y, TRIP_END_y, TRIP_START_m, TRIP_END_m, TRIP_ID.tne, TRIP_week_num, TRIP_DATE_y, TRIP_DATE_m, distinct_start_weeks_t, distinct_end_weeks_t, max_weeks_cnt_t, distinct_weeks_ne"

# sa_v_p_t_tne_by2 <-
#   dplyr::join_by(unique_all_vessel_ids_3 == VESSEL_ID)
# 
# tic("sa_v_p_t_tne2")
# sa_v_p_t_tne2 <-
#   full_join(
#     sa_v_p_t_tne1,
#     t__tne__dates_w_cnt_t_tne_short,
#     sa_v_p_t_tne_by2,
#     relationship = "many-to-many",
#     suffix = c(".v_p", ".t_tne")
#   )
# toc()
# sa_v_p_t_tne2: 52.23 sec elapsed

dim(sa_v_p_t_tne2)
# [1] 825558     42
# [1] 825335     28
# [1] 51984236       50

## Count weeks ----
### permit, but no t/tne ----
v_p_t_tne_dates |> 
  dplyr::filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |> 
  dim()
# [1] 2567   42

v_p_t_tne_dates_cnts__no_t__no_tne <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  dplyr::filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID)) |>
  # no t or tne
  dplyr::filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
  dplyr::group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::mutate(no_t_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR))

dim(v_p_t_tne_dates_cnts__no_t__no_tne)
# [1] 2402   43

### permit, t xor tne ----

v_p_t_tne_dates_cnts__t_xor_tne <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  dplyr::filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID)) |>
  # t xor tne
  dplyr::filter((is.na(TRIP_ID.t) | is.na(TRIP_ID.tne)) &
           # but not both are absent
           !(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne))) |>
           dplyr::group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
           dplyr::mutate(t_xor_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR))
         
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
#   dplyr::filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |> dim()
# [1] 2402   43


### permit, t and tne ----

v_p_t_tne_dates_cnts__t_and_tne <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  dplyr::filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID)) |>
  # t and tne
  dplyr::filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
  dplyr::group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::mutate(t_and_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR))

View(v_p_t_tne_dates_cnts__t_and_tne)
# [1] 5575   43

# df2 <- 
  # df %>%
  # dplyr::mutate(x = if_else(B > 100, A, A),
  #        Xi = if_else(B > 100,  x*0.1 + A, A),
  #        Xii = if_else(B > 100,  x*0.5 + A, A),
  #        Xiii = if_else(B > 100,  x*0.9 + A, A))

# all 3 conditions together ----
  # left_join(dt1,
  #           dt1 %>% 
  #                dplyr::filter(x2==1) %>%
  #                dplyr::group_by(x) %>%
  #                summarise(a=mean(y)), by='x') %>%
  #                dplyr::mutate(z=y/a)%>%
  #                head()

v_p_t_tne_dates_w_v_p <-
  v_p_t_tne_dates |>
  # permit period for this dates exists
  dplyr::filter(!is.na(PERMIT_VESSEL_ID) | !is.na(VESSEL_VESSEL_ID))

# 1) # no t or tne
v_p_t_tne_dates_w_v_p__cnts__t_tne <-
  left_join(
    v_p_t_tne_dates_w_v_p,
    v_p_t_tne_dates_w_v_p |>
      # no t or tne
      dplyr::filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
      dplyr::group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
      dplyr::mutate(no_t_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR)) |>
      dplyr::ungroup()
  )
dim(v_p_t_tne_dates_w_v_p__cnts__t_tne)
# [1] 825393     43

# 2) t xor tne
v_p_t_tne_dates_w_v_p__cnts__t_tne <-
  left_join(
    v_p_t_tne_dates_w_v_p__cnts__t_tne,
    v_p_t_tne_dates_w_v_p__cnts__t_tne |>
      # t xor tne, # but not both are absent
      dplyr::filter(
        (is.na(TRIP_ID.t) | is.na(TRIP_ID.tne)) &
        !(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne))
        ) |>
      dplyr::group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
      dplyr::mutate(t_xor_tne_weeks_amnt = n_distinct(WEEK_OF_YEAR, YEAR)) |>
      dplyr::ungroup()
  )

dim(v_p_t_tne_dates_w_v_p__cnts__t_tne)
# [1] 825393     44

# 3) t and tne
v_p_t_tne_dates_w_v_p__cnts__t_tne <-
  left_join(
    v_p_t_tne_dates_w_v_p__cnts__t_tne,
    v_p_t_tne_dates_w_v_p__cnts__t_tne |>
      # t and tne
      dplyr::filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
      dplyr::group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
      dplyr::mutate(t_and_tne_weeks_amnt =
               n_distinct(WEEK_OF_YEAR, YEAR)) |>
      dplyr::ungroup()
  )

### check numbers ----
dim(v_p_t_tne_dates_w_v_p__cnts__t_tne)
# [1] 825393     45

# 
v_p_t_tne_dates_w_v_p__cnts__t_tne |>
  dplyr::select(contains("vessel_id")) |>
  # dplyr::select(contains("vessel")) |> # [1] 8322    5
  dplyr::distinct() |>
  dim()
# 7059 3

v_p_t_tne_dates_w_v_p |>
  # dplyr::select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::select(contains("vessel_id")) |>
  dplyr::distinct() |>
  dim()
# [1] 7059    3

v_p_t_tne_dates_w_v_p__cnts__t_tne |>
  # dplyr::select(VESSEL_VESSEL_ID) |>  # [1] 5603    1
  # dplyr::select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::select(contains("vessel_id")) |>
  dplyr::distinct() |>
  dim()
# [1] 7059    3

v_p_t_tne_dates |>
  # dplyr::select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::select(contains("vessel_id")) |>
  dplyr::distinct() |>
  dim()
# 7060 3

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |> 
  # dplyr::select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::select(contains("vessel_id")) |>
  dplyr::distinct() |>
  dim()
# 3181 3

t__tne__dates_w_cnt_t_tne_short |>
  dplyr::select(contains("vessel_id")) |>
  dplyr::distinct() |>
  dim()
# 3880

# TODO: join by another fields
# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 + t__tne__dates_w_cnt_t_tne_short
# 3181 +3880
# 7061
