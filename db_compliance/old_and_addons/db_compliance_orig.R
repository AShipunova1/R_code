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

# separate_permits_into_3_groups ----
#repeat for permit only

# print_df_names(permit_vessel_query_exp21_reg)

permit_info_r <-
  permit_info  %>%
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

# View(permit_info_r)
# 'data.frame':	181188 obs. of  23 variables:
# [1] 181207     23

permit_info_r %>%
  dplyr::select(VESSEL_ID) %>%
  dplyr::distinct() %>%
  str()
# 13929
# 13930

# check differently
# https://stackoverflow.com/questions/63402652/comparing-dates-in-different-columns-to-isolate-certain-within-group-entries-in

## add my_end_date ----
permit_info_r_short <-
  permit_info_r %>%
  dplyr::select(
    VESSEL_ID,
    EXPIRATION_DATE,
    TOP,
    PERMIT,
    EFFECTIVE_DATE,
    END_DATE,
    PERMIT_STATUS,
    VESSEL_ALT_NUM,
    permit_sa_gom
  ) %>%
  dplyr::mutate(my_end_date =
           case_when((END_DATE < EFFECTIVE_DATE) &
                       (EXPIRATION_DATE > EFFECTIVE_DATE)
                     ~ EXPIRATION_DATE,
                     .default =
                       dplyr::coalesce(END_DATE,                                     EXPIRATION_DATE)
           )) %>%
  dplyr::select(-c(END_DATE,
            EXPIRATION_DATE)) %>%
  dplyr::distinct()

# split by permit ----
permit_info_r_l <-
  permit_info_r_short %>%
  split(as.factor(permit_info_r_short$permit_sa_gom))

# From Help:
# It is common to have right-open ranges with bounds like `[)`, which would
# mean an end value of `415` would no longer overlap a start value of `415`.
# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
by <- join_by(VESSEL_ID,
              overlaps(x$EFFECTIVE_DATE,
                       x$my_end_date,
                       y$EFFECTIVE_DATE,
                       y$my_end_date,
                       bounds = "[)"))

tic("permit_info_r_l_overlap_join1")
permit_info_r_l_overlap_join1 <-
  full_join(
  permit_info_r_l$gom_only,
  permit_info_r_l$sa_only,
  by,
  suffix = c(".gom", ".sa")
)
toc()
# permit_info_r_l_overlap_join1: 0.66 sec elapsed


# dim(permit_info_r_l_overlap_join1)
# [1] 84570     5
# [1] 186210     15

# View(permit_info_r_l_overlap_join1)

permit_info_r_l_overlap_join1 %>%
  dplyr::select(VESSEL_ID) %>%
  dplyr::distinct() %>%
  dim()
# [1] 13930     1

permit_info_r %>%
  dplyr::select(VESSEL_ID) %>%
  dplyr::distinct() %>%
  dim()
# 13930
# 13942

# add "dual" to intervals ----
permit_info_r_l_overlap_join1_w_dual <-
  permit_info_r_l_overlap_join1 %>%
  dplyr::mutate(permit_sa_gom =
           case_when(
             !is.na(permit_sa_gom.sa) &
               !is.na(permit_sa_gom.gom) ~ "dual",
             .default =
               dplyr::coalesce(permit_sa_gom.sa,
                               permit_sa_gom.gom)

           ))

# 186,210
permit_info_r_l_overlap_join1_w_dual %>%
  dplyr::select(permit_sa_gom) %>%
  dplyr::distinct()
# all 3

# to get dual in the overlapping period:
# dplyr::filter(!is.na(permit_sa_gom.sa))

interval_2022 = lubridate::interval(as.Date('2022-01-01'),
                                    as.Date('2022-12-31'))

permit_info_r_l_overlap_join1_w_dual_22 <-
  permit_info_r_l_overlap_join1_w_dual %>%
  dplyr::mutate(
    eff_int_gom =
      lubridate::interval(EFFECTIVE_DATE.gom,
                          my_end_date.gom),
    eff_int_sa =
      lubridate::interval(EFFECTIVE_DATE.sa,
                          my_end_date.sa)
  ) %>%
  #   dplyr::mutate(int_overlapped = int_overlaps(eff_int_gom, eff_int_sa) )
  dplyr::filter(int_overlaps(eff_int_gom,
                      interval_2022) |
           int_overlaps(eff_int_sa,
                      interval_2022)
         )

### check ----
permit_info_r_l_overlap_join1_w_dual_22 %>%
  dplyr::select(permit_sa_gom) %>%
  dplyr::distinct()
# all 3

permit_info_r_l_overlap_join1_w_dual_22 %>%
  dplyr::filter(permit_sa_gom == "dual") %>%
  dplyr::select(VESSEL_ID) %>%
  dplyr::distinct() %>%
  dim()
# 379
# end here permits

permit_info_r_l_overlap_join1_w_dual_22 %>%
  dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  dplyr::filter(!(VESSEL_ID == VESSEL_ALT_NUM.sa)) %>%
  dim()
# 652

permit_info_r_l_overlap_join1_w_dual_22 %>%
  dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  dplyr::filter(!(VESSEL_ID == VESSEL_ALT_NUM.gom)) %>%
  dim()
# 356

# permit_info_r_l_overlap_join1_w_dual_22 %>%
#   dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
#   dplyr::filter(!(VESSEL_ALT_NUM.gom == VESSEL_ALT_NUM.sa)) %>%
#   dim()
# 0

# split permits by region again ----
permit_info_r_l_overlap_join1_w_dual_22__list <-
  permit_info_r_l_overlap_join1_w_dual_22 %>%
  split(as.factor(permit_info_r_l_overlap_join1_w_dual_22$permit_sa_gom))

# combine permit VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom ----

## check the diff ----
check_1 <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map_df(
    ~ .x %>%
      dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      dplyr::filter(!(VESSEL_ALT_NUM.sa == VESSEL_ALT_NUM.gom))
  )
str(check_1)
# 0
# VESSEL_ALT_NUM.sa == VESSEL_ALT_NUM.gom:
# 'data.frame':	3668 obs. of  3 variables


check_2 <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map(
    ~ .x %>%
      dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      dplyr::distinct() %>%
      dplyr::filter(!(VESSEL_ID == VESSEL_ALT_NUM.gom))
  )

map_df(check_2, dim)
#    dual gom_only sa_only
# 1   143      213       0
# dplyr::distinct()
# 1    14       70       0
# [1] 356   3

check_3 <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map(
    ~ .x %>%
      dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      dplyr::distinct() %>%
      dplyr::filter(!(VESSEL_ID == VESSEL_ALT_NUM.sa))
  )

map_df(check_3, dim)
   # dual gom_only sa_only
# 1    14        0     151


# [1] 657   3
# ? ----
# get all vessel_ids for permits ----
permit_info_r_l_overlap_join1_w_dual_22__list_ids <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map(
    ~ .x %>%
      dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      tidyr::pivot_longer(
        cols = c(VESSEL_ID,
                 VESSEL_ALT_NUM.sa,
                 VESSEL_ALT_NUM.gom),
        values_to = "permit_vessel_id"
      ) %>%
      dplyr::select(permit_vessel_id) %>%
      dplyr::distinct() %>%
      return()
  )

# get all vessels for 2022 ----
# join by different vessel ids, then bind together and unique
vessels_permit_vsl_id_coast_g <-
  permit_info_r_l_overlap_join1_w_dual_22__list_ids %>%
  map(~ .x %>%
        inner_join(vessels_all,
                   join_by(permit_vessel_id == COAST_GUARD_NBR)))

vessels_permit_vsl_id_state_n <-
  permit_info_r_l_overlap_join1_w_dual_22__list_ids %>%
  map(~ .x %>%
        inner_join(vessels_all,
                   join_by(permit_vessel_id == STATE_REG_NBR)))

vessels_permit_vsl_id__all_l_0 <-
  # map over 2 lists of dataframes and make a list
  map2(vessels_permit_vsl_id_coast_g,
           vessels_permit_vsl_id_state_n,
           dplyr::bind_rows)

# add permit_sa_gom back as a column
vessels_permit_vsl_id__all_l <-
  vessels_permit_vsl_id__all_l_0 |>
  map2(names(vessels_permit_vsl_id__all_l_0),
       ~ dplyr::mutate(.x, permit_sa_gom = .y))

# setdiff(names(vessels_permit_vsl_id__all_l$dual),
#         names(vessels_permit_vsl_id__all_l_0$dual)
# )
# [1] "permit_sa_gom"

## the same for checking as a df ----
vessels_permit_vsl_id__all <-
  # map over 2 lists of dataframes and make a df
  map2_dfr(vessels_permit_vsl_id_coast_g,
           vessels_permit_vsl_id_state_n,
           dplyr::bind_rows) %>%
  dplyr::distinct()

grep(
  "int",
  names(vessels_permit_vsl_id__all),
  ignore.case = T,
  value = T
)
# 0
### check joins ----

vessels_by_permit_vessel_num <-
  vessels_permit_vsl_id__all %>%
  dplyr::select(permit_vessel_id) %>%
  dplyr::distinct() %>%
  dim()
# [1] 5632    1

# setdiff(
#   permit_info_r_l_overlap_join1_w_dual_22__list_ids$sa_only$permit_vessel_id,
#   vessels_permit_vsl_id__all_l$sa_only$permit_vessel_id
# )
# [1] "1304296"  "NA"       "FL6437NY" "1176885"

# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>%
#     dplyr::filter(VESSEL_ID == '1304296') %>% View()
# # alt_num.sa DL5161AM
#
# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>%
#     dplyr::filter(VESSEL_ID == 'FL6437NY') %>% View()

# FL6437NY
# 2015-09-01 EDT--2022-06-02 EDT
# VESSEL SOLD

# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>%
#     dplyr::filter(VESSEL_ID == '1176885') %>% View()
# alt num FL0668PV 2021-05-01 EDT--2022-03-31 EDT

# vessels_permit_vsl_id__all_l$sa_only %>%
#   dplyr::filter(vessel_id == '1304296' |
#            VESSEL_ID == '1304296' |
#            COAST_GUARD_NBR == '1304296' |
#            STATE_REG_NBR == '1304296'
#            ) %>%
#   View()
# # 0

# vessels_permit_vsl_id__all_l$sa_only %>%
#   dplyr::filter(permit_vessel_id == 'DL5161AM') %>%
#   View()
# 1
# SERO_OFFICIAL_NUMBER is NULL
# difference is in 1 vessel in sa_only

# clean up vessels_permit_vsl_id__all ----
# vessels_permit_vsl_id__all %>%
#   dplyr::count(permit_vessel_id) %>%
#   dplyr::filter(n > 1) %>%
# head()
# # 29
 # 1 1023478       2
 # 2 1064839       2
 # 3 1090694       2
 # 4 1243727       2
 # 5 1320038       2
 # 6 16250027      2

# vessels_permit_vsl_id__all %>%
#   dplyr::filter(permit_vessel_id == '1023478') %>%
#   dim()
# [1]  2 30

#
# https://stackoverflow.com/questions/45515218/combine-rows-in-data-frame-containing-na-to-make-complete-row
# coalesce_by_column <- function(df) {
#   return(coalesce(df[1], df[2]))
# }
coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}

### test coalesce_by_column ----
vessels_permit_vsl_id__all_2 <-
  vessels_permit_vsl_id__all %>%
  dplyr::filter(permit_vessel_id == '1023478') %>%
  dplyr::group_by(permit_vessel_id) %>%
  dplyr::summarise_all(coalesce_by_column)

# View(vessels_permit_vsl_id__all_2)

vessels_permit_vsl_id__all_0 <-
  vessels_permit_vsl_id__all %>%
  dplyr::filter(permit_vessel_id == '1023478')

all.equal(vessels_permit_vsl_id__all_2,
          vessels_permit_vsl_id__all_0[1,])
# [1] "Component “COAST_GUARD_NBR”: 'is.NA' value mismatch: 1 in current 0 in target"

all.equal(vessels_permit_vsl_id__all_2,
          vessels_permit_vsl_id__all_0[2,])
# [1] "Component “STATE_REG_NBR”: 'is.NA' value mismatch: 1 in current 0 in target"

## all coalesce ----
# Has to do it twice, here and after binding, otherwise it takes to long to run intermediate steps

# vessels_by_permit_vessel__all_u <-
#   vessels_permit_vsl_id__all %>%
#   dplyr::group_by(permit_vessel_id) %>%
#   dplyr::summarise_all(coalesce_by_column)

# # View(vessels_permit_vsl_id__all_l)
# tic("vessels_by_permit_vessel__all_l_u")
# vessels_by_permit_vessel__all_l_u <-
#   vessels_permit_vsl_id__all_l %>%
#   purrr::map(~ .x %>%
#         dplyr::group_by(permit_vessel_id) %>%
#         dplyr::summarise_all(coalesce_by_column))
# toc()
# # vessels_by_permit_vessel__all_l_u: 98.53 sec elapsed
# # vessels_by_permit_vessel__all_l_u: 148.02 sec elapsed
# vessels_by_permit_vessel__all_l_u: 137.68 sec elapsed

# View(vessels_permit_vsl_id__all_l)

my_function_vessels_permit_vsl_id__all_l <-
  function(vessels_permit_vsl_id__all_l) {
    purrr::map(
      vessels_permit_vsl_id__all_l,
      ~ .x %>%
        dplyr::group_by(permit_vessel_id) %>%
        dplyr::summarise_all(coalesce_by_column)
    ) %>%
      return()
  }

vessels_by_permit_vessel__all_l_u_file_path <-
  file.path(
    my_paths$inputs,
    current_project_name,
    "intermediate_dfs",
    "vessels_by_permit_vessel__all_l_u.rds"
  )

# View(vessels_permit_vsl_id__all_l)
vessels_by_permit_vessel__all_l_u <-
  read_rds_or_run(
    vessels_by_permit_vessel__all_l_u_file_path,
    vessels_permit_vsl_id__all_l,
    my_function_vessels_permit_vsl_id__all_l
  )
# run the function: 117.58 sec elapsed

map_df(vessels_by_permit_vessel__all_l_u, dim)

length(vessels_by_permit_vessel__all_l_u$dual$permit_vessel_id) +
  length(vessels_by_permit_vessel__all_l_u$gom_only$permit_vessel_id) +
  length(vessels_by_permit_vessel__all_l_u$sa_only$permit_vessel_id)
# 5684

# all.equal(vessels_by_permit_vessel__all_l_u$dual$permit_vessel_id,
#           vessels_by_permit_vessel__all_l_u_rds$dual$permit_vessel_id)
# T

length(unique(vessels_by_permit_vessel__all_l_u$gom_only$permit_vessel_id))
# 392
length(vessels_by_permit_vessel__all_l_u$gom_only$permit_vessel_id)
# 392

### check vessels_permit_vsl_id__all_u ---

map_df(vessels_permit_vsl_id__all_l, dim)
#    dual gom_only sa_only
# 1   392   141154  143932

map_df(vessels_by_permit_vessel__all_l_u, dim)
#    dual gom_only sa_only
# 1   392     1272    4020
# 2    30       30      30

uniq_permit_vsl_ids <-
  map_df(
  vessels_by_permit_vessel__all_l_u,
  ~ .x %>%
    dplyr::select(permit_vessel_id) %>%
    dplyr::distinct() %>%
    dim()
)

# uniq_permit_vsl_ids
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   392     1272    4020
# 2     1        1       1
# View(uniq_permit_vsl_ids)

# uniq_permit_vsl_ids %>%
  # rowSums()
# 5684

# View(trip_notifications_2022)

# join vessels and permits for 2022 ----
# join by different vessel ids, then bind together and unique
# print_df_names(permit_info_r_l_overlap_join1_w_dual_22__list$sa_only)

vessels_permit_1 <-
  purrr::map2(permit_info_r_l_overlap_join1_w_dual_22__list,
              vessels_by_permit_vessel__all_l_u,
              function(x, y) {
                dplyr::inner_join(x,
                                  y,
                                  join_by(VESSEL_ID == permit_vessel_id),
                                  suffix = c(".p", ".v"))
              })

# View(vessels_permit_1)

vessels_permit_2 <-
  purrr::map2(permit_info_r_l_overlap_join1_w_dual_22__list,
              vessels_by_permit_vessel__all_l_u,
              function(x, y) {
                dplyr::inner_join(x,
                                  y,
                                  join_by(VESSEL_ALT_NUM.sa == permit_vessel_id),
                                  suffix = c(".p", ".v"))
              })

vessels_permit_3 <-
  purrr::map2(permit_info_r_l_overlap_join1_w_dual_22__list,
              vessels_by_permit_vessel__all_l_u,
              function(x, y) {
                dplyr::inner_join(x,
                                  y,
                                  join_by(VESSEL_ALT_NUM.gom == permit_vessel_id),
                                  suffix = c(".p", ".v"))
              })


## bind ----

vessels_permit_bind1 <-
  map2(vessels_permit_1,
       vessels_permit_2,
       dplyr::bind_rows)

vessels_permit_bind <-
  map2(vessels_permit_bind1,
       vessels_permit_3,
       dplyr::bind_rows)

map_df(vessels_permit_bind, dim)
# 1 11004    10914   41069

## uniq ----

# vessels_permit_bind_u_test_0 <-
#   vessels_permit_bind$gom_only %>%
#   dplyr::filter(SERO_OFFICIAL_NUMBER == 'FL4203RB')
#
# vessels_permit_bind_u_test <-
#   vessels_permit_bind$gom_only %>%
#   dplyr::filter(SERO_OFFICIAL_NUMBER == 'FL4203RB') %>%
#   dplyr::group_by(SERO_OFFICIAL_NUMBER) %>%
#   dplyr::summarise_all(coalesce_by_column)
#
# vessels_permit_bind_u_test2 <-
#   vessels_permit_bind$gom_only %>%
#   dplyr::filter(SERO_OFFICIAL_NUMBER == 'FL4203RB') %>%
#   dplyr::group_by(VESSEL_ID.v) %>%
#   dplyr::summarise_all(coalesce_by_column)
#
# View(vessels_permit_bind_u_test_0)
# View(vessels_permit_bind_u_test)

### vessels_permit_bind ----

file_path_vessels_permit_bind_u <- file.path(
  my_paths$inputs,
  current_project_name,
  r"(intermediate_dfs\vessels_permit_bind_u.rds)"
)

my_function_vessels_permit_bind_u_one_df <-
  function(vessels_permit_bind) {
    vessels_permit_bind %>%
      map(~ .x %>%
            dplyr::group_by(VESSEL_ID.v) %>%
            dplyr::summarise_all(coalesce_by_column)) %>%
      return()
  }

vessels_permit_bind_u_one_df <-
  read_rds_or_run(file_path_vessels_permit_bind_u,
           vessels_permit_bind,
           my_function_vessels_permit_bind_u_one_df)
# run the function: 578.76 sec elapsed

map_df(vessels_permit_bind_u_one_df, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   379     1204    3877
# 2    48       48      48

# 379 + 1204 + 3877
# 5460
# all.equal(vessels_permit_bind_u_one_df,
#           vessels_permit_bind_u_one_df1)
# T

# fix trip data ----
## add trip_int ----
trips_info_2022 %>%
  dplyr::select(TRIP_START_DATE, TRIP_START_TIME, TRIP_END_DATE, TRIP_END_TIME) %>%
  str()
# POSIXct
# str(trips_info_2022)

# is_effective_date =
#   lubridate::floor_date(is_effective_date,
#                         unit = "day"),

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
  View()

## trip types A and H trips ----
trips_info_2022_int_ah <-
  trips_info_2022_int %>%
  dplyr::filter(TRIP_TYPE %in% c("A", "H"))

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

# trips_info_2022_int_ah %>%
#   dplyr::select(TRIP_START_DATE) %>%
#   head()

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

# trips_info_2022_int_ah_w_y %>%
#   dplyr::select(starts_with("TRIP")) %>%
#   dplyr::arrange(TRIP_START_DATE) %>%
#   View()

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

# trip_notifications_2022_ah_w_y %>%
#   dplyr::select(starts_with("TRIP")) %>%
#   dplyr::arrange(TRIP_START_DATE) %>%
#   View()

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

trip_neg_2022_w_y %>%
  dplyr::select(starts_with("TRIP")) %>%
  dplyr::arrange(TRIP_DATE) %>%
  View()

# vessels and trip_notifications ----

## compare vessel_ids ----
trip_notifications_2022_ah_w_y_vsl_ids <-
  trip_notifications_2022_ah_w_y %>%
  dplyr::select(VESSEL_ID) %>%
  dplyr::distinct()

vessels_by_permit_vessel__all_l_u_vsl_ids <-
  vessels_by_permit_vessel__all_l_u %>%
  map_df( ~ .x %>%
            dplyr::select(VESSEL_ID) %>%
            dplyr::distinct())

vessels_by_permit_vessel__all_l_u_vsl_ids_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map( ~ .x %>%
            dplyr::select(VESSEL_ID) %>%
            dplyr::distinct())

dim(trip_notifications_2022_ah_w_y)
# [1] 126726     33
# [1] 67738    33

# dim(trip_notifications_2022_vsl_ids)
# [1] 914   1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids)
# [1] 5459      1
# vessels_by_permit_vessel__all_l_u_vsl_ids %>%
#   dplyr::distinct() %>%
#   dim()
# [1] 5409    1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids_l$gom_only)
# [1] 1204    1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids_l$dual)
# 378
# 1204+378
# 1582
dim(vessels_by_permit_vessel__all_l_u_vsl_ids_l$sa_only)
# [1] 3877    1

# # not in vessel_trip sa
# not_in_vessel_trip_sa <-
#   setdiff(
#     trip_notifications_2022_vsl_ids$VESSEL_ID,
#     dplyr::distinct(vessels_permit_vsl_id__all_l__sa_ids$VESSEL_ID)
#   ) %>%
#   dplyr::distinct()
# dplyr::glimpse(not_in_vessel_trip_sa)
# # 60
 # num [1:60] 327682 248806 326294 249111 246954 ...

not_in_vessel_trip_gom <-
  setdiff(
    trip_notifications_2022_ah_w_y_vsl_ids$VESSEL_ID,
    unique(vessels_by_permit_vessel__all_l_u_vsl_ids_l$gom_only$VESSEL_ID)
  ) %>%
  unique()

glimpse(not_in_vessel_trip_gom)
 # num [1:15] 326294 249111 280684 326421 326390 ...
 # num [1:305] 328214 328340 247128 247129 326387 ...
# A, H
 # num [1:296] 328214 328340 247128 247129 326387 ...
 # num [1:226] 79639 329030 326452 247045 325995 ...

## join vessels, trip notif  ----
# View(vessels_by_permit_vessel__all_l_u)

vessels__trip_notif_22_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map(
    ~ .x %>%
      dplyr::distinct() %>%
      inner_join(
        trip_notifications_2022_ah_w_y,
        join_by(VESSEL_ID),
        relationship = "many-to-many",
        suffix = c(".v", ".tn")
      )
  )

# vessels__trip_notif_22_l %>%
#   map_df(dim)
#    dual gom_only sa_only
# 1 38464    95585    1250
# 2    62       62      62
# 1 19885    52087     555
# 2    69       69      69

# vessels and trip negatives ----
# View(trip_neg_2022_w_y)

vessels__trip_neg_22_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map(
    ~ .x %>%
      dplyr::distinct() %>%
      inner_join(
        trip_neg_2022_w_y,
        join_by(VESSEL_ID),
        relationship = "many-to-many",
        suffix = c(".v", ".tneg")
      )
  )

vessels__trip_neg_22_l %>%
  map_df(dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1 41272    56506  795944
# 2    41       41      41
# 1 17947    21175  390659
# 2    45       45      45

# vessels and trips ----

# View(trips_info_2022)

vessels__trips_22_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map(
    ~ .x %>%
      dplyr::distinct() %>%
      inner_join(
        trips_info_2022_int_ah_w_y,
        join_by(VESSEL_ID),
        relationship = "many-to-many",
        suffix = c(".v", ".t")
      )
  )

# vessels__trips_22_l %>%
#   map_df(dim)
#    dual gom_only sa_only
# 1 31108    75776  107751
# 1 14905    41172   47421
# 2   101      101     101
# 1 14525    40210   47157
# 2   102      102     102
# 1 14525    40210   47233
# 2   109      109     109

# print_df_names(vessels__trips_22_l$sa_only)

# GOM + dual 2022 compliance ----
# There should be a declaration for every logbook (in other words, the number of fishing intended charter declarations would need to be equal to logbooks to be compliant).
# There should be a logbook for every declaration of a charter or headboat intending to fish.

# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance

dates_2022_short <-
  dates_2022 %>%
  dplyr::select(-c(COMPLETE_DATE, MONTH_OF_YEAR))
# MONTH_OF_YEAR: could be 2 for the same week,: 17, 4, 5

# interval_2022
# dates_2022

# print_df_names(trips_info_2022_int_ah)

#   > strftime(c("2022-05-27", "2022-05-28", "2022-05-29", "2022-05-30", "2022-05-31", "2022-06-01", "2022-06-04", "2022-06-05"), format = "%U")

# View(vessels__trips_22_l)
# vessels__trips_22_l$sa_only

# for each vessel find all trips, trip_notif and trip_neg for each week
# View(vessels__trips_22_l)
# vessels__trip_notif_22_l
# vessels__trip_neg_22_l

## neg trips per week per vessel ----
vessels__trip_neg_22_l_sa_short <-
  vessels__trip_neg_22_l$sa_only %>%
  dplyr::filter(!lubridate::floor_date(TRIP_DATE,
                                unit = "day") ==
           "2021-12-31") %>%
  dplyr::select(permit_vessel_id,
         SUPPLIER_VESSEL_ID,
         TRIP_week_num,
         TRIP_DATE_y) %>%
  dplyr::distinct()

  # dplyr::select(contains("vessel"), starts_with("TRIP")) %>%
  # dplyr::distinct()

data_overview(vessels__trip_neg_22_l_sa_short)
# 390,659 10
# permit_vessel_id   1709
# [1] 66631     4
# permit_vessel_id   1714

### remove neg trips on 2021-12-31 ----
# vessels__trip_neg_22_l_sa_short %>%
#   dplyr::filter(lubridate::floor_date(TRIP_DATE,
#                                unit = "day") ==
#            "2021-12-31") %>% dplyr::glimpse()
# Rows: 1,006

# dim(vessels__trip_neg_22_l_sa_short)
# [1] 66631     3

# View(vessels__trip_neg_22_l_sa_short)

# vessels__trip_neg_22_l_sa_short %>%
#   dplyr::select(permit_vessel_id, SUPPLIER_VESSEL_ID, TRIP_week_num) %>%
#   dplyr::distinct() %>%
#   add_count(permit_vessel_id, SUPPLIER_VESSEL_ID) %>%
#   View()

vessels__trip_neg_22_l_sa_short %>%
  dplyr::filter(permit_vessel_id == '03017306') %>%
  dplyr::select(permit_vessel_id, TRIP_week_num) %>%
  dplyr::distinct() %>%
  dplyr::group_by(permit_vessel_id) %>%
  summarise(n_distinct(TRIP_week_num)) %>%
  # dplyr::count(TRIP_week_num) %>%
  dplyr::glimpse()
#   $ permit_vessel_id            <chr> "03017306"
# $ `n_distinct(TRIP_week_num)` <int> 19

vessels__trip_neg_22_l_sa_short_weeks_per_vessel <-
  vessels__trip_neg_22_l_sa_short %>%
  dplyr::group_by(permit_vessel_id, SUPPLIER_VESSEL_ID) %>%
  summarise(tot_weeks = n_distinct(TRIP_week_num))

View(vessels__trip_neg_22_l_sa_short_weeks_per_vessel)
# 1709
# 1714

## trip notifications per vessel per week
# data_overview(vessels__trip_notif_22_l$sa_only)
# permit_vessel_id              17

vessels__trip_notif_22_l_sa_short <-
  vessels__trip_notif_22_l$sa_only %>%
  dplyr::filter(TRIP_START_y %in% c('2021', '2022')) %>%
  dplyr::filter(TRIP_END_y %in% c('2022', '2023')) %>%
  dplyr::select(permit_vessel_id,
         TRIP_START_week_num,
         TRIP_END_week_num,
         TRIP_START_y,
         TRIP_END_y) %>%
  dplyr::distinct()

# str(vessels__trip_notif_22_l_sa_short)
vessels__trip_notif_22_l_sa_vessels_trips <-
  vessels__trip_notif_22_l_sa_short %>%
  dplyr::group_by(permit_vessel_id) %>%
  summarise(
    tot_start_weeks =
      n_distinct(TRIP_START_week_num),
    tot_end_weeks =
      n_distinct(TRIP_END_week_num)
  )

dim(vessels__trip_notif_22_l_sa_vessels_trips)
# [1] 17  3

## vessels and trips and weeks
# data_overview(vessels__trips_22_l$sa_only)
# TRIP_START_y                    2
# TRIP_END_y                      3
# permit_vessel_id             1110
# VESSEL_ID                    1069
# SUPPLIER_VESSEL_ID           1069
# --
# permit_vessel_id             1112
# VESSEL_ID                    1071

vessels__trips_22_l_sa_short <-
  vessels__trips_22_l$sa_only %>%
  dplyr::filter(TRIP_START_y %in% c('2021', '2022')) %>%
  dplyr::filter(TRIP_END_y %in% c('2022', '2023')) %>%
  dplyr::select(permit_vessel_id,
         TRIP_START_week_num,
         TRIP_END_week_num,
         TRIP_START_y,
         TRIP_END_y) %>%
  dplyr::distinct()

vessels__trips_22_l_sa_weeks_per_vessel <-
  vessels__trips_22_l_sa_short %>%
  dplyr::group_by(permit_vessel_id) %>%
  summarise(
    tot_start_weeks =
      n_distinct(TRIP_START_week_num),
    tot_end_weeks =
      n_distinct(TRIP_END_week_num)
  )

dim(vessels__trips_22_l_sa_weeks_per_vessel)
# 1112

## combine weeks, vessels and trips (all) info ----
# by = join_by(permit_vessel_id, TRIP_START_week_num, TRIP_END_week_num)
by_vessels_trips_and_notif_by_week =
  join_by(permit_vessel_id, TRIP_START_week_num, TRIP_START_y)

vessels_trips_and_notif_by_week <-
  vessels__trips_22_l_sa_short %>%
  full_join(vessels__trip_notif_22_l_sa_short,
            by_vessels_trips_and_notif_by_week)
# Joining with `by = join_by(permit_vessel_id, TRIP_START_week_num,
# TRIP_END_week_num, TRIP_START_y, TRIP_END_y)`

# all.equal(vessels_trips_and_notif_by_week,
#           vessels_trips_and_notif_by_week1)
# [1] "Names: 2 string mismatches"
# [2] "Length mismatch: comparison on first 5 components"
# [3] "Component 3: 'is.NA' value mismatch: 76 in current 0 in target"
# [4] "Component 5: 'is.NA' value mismatch: 76 in current 0 in target"

dim(vessels_trips_and_notif_by_week)
# [1] 19598     3
# with year
# [1] 19600     7
# [1] 19616     5

# same by notif weeks
# vessels_trips_and_notif_by_week %>%
#   dplyr::filter(is.na(TRIP_END_week_num)) %>%
#   dim()
# 0

## join with all weeks ----
by_start = join_by(TRIP_START_y == YEAR,
                   TRIP_START_week_num == WEEK_OF_YEAR)

### with trips ----
# print_df_names(vessels__trips_22_l_sa_short)

vessels__trips_22_l_sa_short_all_dates_t_start <-
  vessels__trips_22_l_sa_short %>%
  dplyr::right_join(dates_2022_short,
            by_start,
            relationship = "many-to-many") %>%
# [1] 136391    6
  dplyr::distinct()
# [1] 23362     6
# [1] 19524     5 (no day and month)

# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 166 of `y` matches multiple rows in `x`.

dim(vessels__trips_22_l_sa_short_all_dates_t_start)
# [1] 19433     5
# [1] 19445     5 (with 2021 and 2023)
# [1] 19461     5

vessels__trips_22_l_sa_short_all_dates_t_start %>%
  dplyr::filter(is.na(permit_vessel_id))
# A tibble: 10 × 5
# ...
# 4 NA 51 NA 2021
# 5 NA  1 NA 2023

View(vessels__trips_22_l_sa_short_all_dates_t_start)

#### keeping all columns ----
vessels__trips_22_l_sa_short_all_dates_t_start_all_cols <-
  sqldf("SELECT
  *
FROM
       vessels__trips_22_l_sa_short
  RIGHT JOIN dates_2022_short
  ON ( trip_start_y = year )
WHERE
  trip_start_week_num = week_of_year
"
  ) %>%
  dplyr::distinct()

dim(vessels__trips_22_l_sa_short_all_dates_t_start_all_cols)
# [1] 19435     7
# [1] 19451     7

vessels__trips_22_l_sa_short_all_dates_t_start_all_cols %>%
  dplyr::filter(is.na(permit_vessel_id))
# 0
# data_overview(vessels__trips_22_l_sa_short_all_dates_all_cols)

# View(vessels__trips_22_l_sa_short_all_dates_t_start_all_cols)

#### join by the end of week ? ----

## join trip notif and days ----

vessels__trip_notif_22_l_sa_short_all_dates_t_start <-
  vessels__trip_notif_22_l_sa_short %>%
  dplyr::right_join(dates_2022_short,
                    by_start,
                    relationship = "many-to-many") %>%
  dplyr::distinct()
dim(vessels__trip_notif_22_l_sa_short_all_dates_t_start)
# [1] 232   5

vessels__trip_notif_22_l_sa_short_all_dates_t_start %>%
  dplyr::filter(is.na(permit_vessel_id))
# 11
# ...
# 5 NA 52 NA 2021
# 6 NA  1 NA 2023

# vessels__trip_notif_22_l_sa_short_all_dates_t_start %>%
#   dplyr::filter(TRIP_START_week_num == 47)

## join trip neg and dates ----
by_start_t_neg = join_by(TRIP_DATE_y == YEAR,
                         TRIP_week_num == WEEK_OF_YEAR)

vessels__trip_neg_22_l_sa_short_all_dates_t_start <-
  vessels__trip_neg_22_l_sa_short %>%
  dplyr::right_join(dates_2022_short,
                    by_start_t_neg,
                    relationship = "many-to-many") %>%
  dplyr::distinct()

dim(vessels__trip_neg_22_l_sa_short_all_dates_t_start)
# [1] 65644     4
# [1] 65818     4

vessels__trip_neg_22_l_sa_short_all_dates_t_start %>%
  dplyr::filter(is.na(permit_vessel_id))
# 11
 # 5 NA NA 52 2021
 # 6 NA NA  1 2023


#### keeping all columns tne ----
vessels__trip_neg_22_l_sa_short__dates <-
  sqldf("SELECT
  *
FROM
       vessels__trip_neg_22_l_sa_short
  RIGHT JOIN dates_2022_short
  ON ( TRIP_DATE_y = year )
WHERE
  TRIP_week_num = week_of_year
"
  ) %>%
  dplyr::distinct()


## find periods (weeks) when each vessel was permitted ----

## for each vessel count neg rep and notif and compare with permitted weeks

# View(vessels_by_permit_vessel__all_l_u)
# dim(vessels_permit_bind_u1)
# vessels_permit_bind
vessels_permit_bind_u_one_df %>%
  map_df(dim)
#    dual gom_only sa_only
# 1   378     1204    3877
# 2    48       48      48
# from csv
# 1   379     1204    3877

# vessels_permit_bind_u1$sa_only %>%
#   head() %>% dplyr::glimpse()

## how many weeks the permit was in effect ----
# Restore eff_int_sa (might be frome the csv)
  # dplyr::mutate(
  #   eff_int_gom =
  #     lubridate::interval(EFFECTIVE_DATE.gom,
  #                         my_end_date.gom),
  #   eff_int_sa =
  #     lubridate::interval(EFFECTIVE_DATE.sa,
  #                         my_end_date.sa)
  # ) %>%


str(vessels_permit_bind_u_one_df)

vessels_permit_bind_u1_sa_w_p <-
  vessels_permit_bind_u_one_df$sa_only %>%
  dplyr::mutate(weeks_perm = eff_int_sa / lubridate::dweeks(1))

dim(vessels_permit_bind_u1_sa_w_p)
# [1] 3877   50

vessels_permit_bind_u1_sa_w_p_short <-
  vessels_permit_bind_u1_sa_w_p %>%
  # we need an sa only
  dplyr::select(-ends_with(".gom"))

dim(vessels_permit_bind_u1_sa_w_p_short)
# [1] 3877   43

### an err ----
# vessels_permit_bind_u1_sa_w_p_short %>%
#    dplyr::filter(!VESSEL_ID == VESSEL_ID.p) %>% View()
# [1]  1 42
#   VESSEL_ID.v VESSEL_ID TOP.sa PERMIT.sa EFFECTIVE_DATE.sa
#         <dbl> <chr>     <chr>  <chr>     <dttm>
# 1      100346 696709    CDW    CDW       2021-10-16 00:00:00
# SERO_OFF 696709
# VESSEL_ID.p 1243529
# HATCS337I485

# FHIER:
  # 696709
  # HATCS337I485

  # 1243529
  # YFY363121213

# Oracle vessels
# HATCS337I485	
# Coast g 696709
# state reg nbr 'FL'
# HAPPY DAY TODAY

#
# View(vessels__trip_neg_22_l)
# vessels_permit_bind_u1_sa_w_p_short
# check for 2022

# count distinct weeks per vessel, compare with permit weeks in year ----

# vessels__trip_neg_22_l_sa_weeks_cnt <-
#   vessels__trip_neg_22_l$sa_only %>%
#   dplyr::group_by(VESSEL_ID, permit_vessel_id) %>%
#   dplyr::mutate(distinct_weeks = n_distinct(TRIP_week_num))
#
# vessels__trip_neg_22_l_sa_weeks_cnt %>%
#   View()
# print_df_names(vessels__trip_neg_22_l$sa_only)

## neg trip weeks ----

vessels__trip_neg_22_l_sa_weeks_cnt_u <-
  vessels__trip_neg_22_l$sa_only %>%
  dplyr::group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
  summarise(distinct_weeks_ne = n_distinct(TRIP_week_num))

dim(vessels__trip_neg_22_l_sa_weeks_cnt_u)
# [1] 1714    5

### check ids ----
vessels__trip_neg_22_l_sa_weeks_cnt_u %>%
  dplyr::filter(!permit_vessel_id == SUPPLIER_VESSEL_ID) %>%
  dim()
# [1] 58  5

vessels__trip_neg_22_l_sa_weeks_cnt_u %>%
  dplyr::filter(!permit_vessel_id == SERO_OFFICIAL_NUMBER) %>%
  dim()
# [1] 58  5

vessels__trip_neg_22_l_sa_weeks_cnt_u %>%
  dplyr::filter(!SUPPLIER_VESSEL_ID == SERO_OFFICIAL_NUMBER)
#   VESSEL_ID permit_vessel_id SUPPLIER_VESSEL_ID SERO_OFFICIAL_NUMBER
# 1    383419 NC0676EK         1292480            NC0676EK
# 2    390281 FL0416RM         FL0416RM           504660

# %>%
  # dim()
# [1] 2 5
#

## trip_notif weeks count per vessel ----
vessels__trip_notif_22_l_sa_weeks_cnt_u <-
  vessels__trip_notif_22_l$sa_only %>%
  dplyr::group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
  summarise(distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
            distinct_end_weeks_tn = n_distinct(TRIP_END_week_num))

# View(vessels__trip_notif_22_l_sa_weeks_cnt_u)
vessels__trip_notif_22_l_sa_weeks_cnt_u %>%
   dplyr::filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
   dim()
# [1] 0 6
# ok

## trips weeks count per vessel ----

vessels__trips_22_l_sa_weeks_cnt_u <-
  vessels__trips_22_l$sa_only %>%
  dplyr::group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
  summarise(distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
            distinct_end_weeks_t = n_distinct(TRIP_END_week_num)) %>%
  dplyr::mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))

# View(vessels__trips_22_l_sa_weeks_cnt_u)
vessels__trips_22_l_sa_weeks_cnt_u %>%
   dplyr::filter(!distinct_start_weeks_t == distinct_end_weeks_t) %>%
   dim()
# 27

## join trips and trip_negative (logbooks + DNFs) ----

by_t__tne = join_by(
  VESSEL_ID,
  permit_vessel_id,
  SUPPLIER_VESSEL_ID,
  SERO_OFFICIAL_NUMBER,
  TRIP_week_num == TRIP_START_week_num,
  TRIP_DATE_y == TRIP_START_y
)

tic("join vessels__t_tne_sa")
vessels__t_tne_sa <-
  full_join(
    vessels__trip_neg_22_l$sa_only,
    vessels__trips_22_l$sa_only,
    by_t__tne,
    suffix = c(".tne", ".t"),
    relationship = "many-to-many"
  )
toc()
# join vessels__t_tne_sa: 1.55 sec elapsed

dim(vessels__t_tne_sa)
# [1] 457647    146
# [1] 458922    148

### check uniq vsls ----
vessels__t_tne_sa %>% dplyr::select(VESSEL_ID, permit_vessel_id) %>%
  dplyr::distinct() %>%
  dim()
# [1] 1751    2
# [1] 1756    2

vessels__trip_neg_22_l_sa_vsls <-
  vessels__trip_neg_22_l$sa_only %>%
  dplyr::select(VESSEL_ID, permit_vessel_id) %>%
  dplyr::distinct()

dim(vessels__trip_neg_22_l_sa_vsls)
# 1709
# [1] 1714    2

vessels__trips_22_l_sa_vsls <-
  vessels__trips_22_l$sa_only %>%
  dplyr::select(VESSEL_ID, permit_vessel_id) %>%
  dplyr::distinct()

dim(vessels__trips_22_l_sa_vsls)
# [1] 1110    2
# [1] 1112    2

full_join(
  vessels__trip_neg_22_l_sa_vsls,
  vessels__trips_22_l_sa_vsls,
  join_by(VESSEL_ID, permit_vessel_id)
) %>%
  dplyr::distinct() %>%
  dim()
# [1] 1751    2
# [1] 1756    2
# ok, as in join

### check, there should be no doubles? ----
vessels__t_tne_sa_tne_in_t_short <-
  vessels__t_tne_sa %>%
  dplyr::filter(dplyr::between(TRIP_DATE, TRIP_START_DATE, TRIP_END_DATE)) %>%
  dplyr::select(TRIP_DATE,
         TRIP_START_DATE,
         TRIP_END_DATE,
         trip_int,
         VESSEL_ID,
         permit_vessel_id)

dim(vessels__t_tne_sa_tne_in_t_short)
# [1] 5523    6

distinct(vessels__t_tne_sa_tne_in_t_short) %>%  dim()
# [1] 5219    6
# [1] 5229    6


# inner_join(d2, d1, by = join_by(x, between(pos, start, end)))
  # # between(x, left, right)
  # dplyr::between(TRIP_DATE, TRIP_START_DATE, TRIP_END_DATE)

# grep("TRIP", names(vessels__trip_neg_22_l$sa_only), ignore.case = T, value = T)
# [1] "TRIP_DATE"     "TRIP_ID"       "TRIP_week_num" "TRIP_DATE_y"
# [5] "TRIP_DATE_m"

# grep("TRIP", names(vessels__trips_22_l$sa_only), ignore.case = T, value = T)
#  [1] "TRIP_ID"             "TRIP_TYPE"           "SUPPLIER_TRIP_ID"
#  [4] "TRIP_NBR"            "SPLIT_TRIP"          "TRIP_START_DATE"
#  [7] "TRIP_END_DATE"       "TRIP_END_TIME"       "TRIP_START_TIME"
# [10] "SUB_TRIP_TYPE"       "TRIP_FEE"            "TRIP_TIME_ZONE"
# [13] "trip_int"            "TRIP_START_week_num" "TRIP_END_week_num"
# [16] "TRIP_START_y"        "TRIP_END_y"          "TRIP_START_m"
# [19] "TRIP_END_m"

# trip_int, TRIP_DATE
### mark if max_weeks_cnt_t, distinct_weeks_ne for the same week ---
# count ones
vessels__t_tne_sa_tne_in_t_cnt <-
  vessels__t_tne_sa %>%
  dplyr::mutate(
    neg_in_t =
      case_when(
        dplyr::between(TRIP_DATE,
                       TRIP_START_DATE,
                       TRIP_END_DATE) ~ "both_t__tne",
        is.na(TRIP_START_TIME) ~ "tne",
        is.na(TRIP_DATE) ~ "t",
        .default = "unknown"
      )
  )

tic("vessels__t_tne_sa_tne_in_t_cnt distinct")
vessels__t_tne_sa_tne_in_t_cnt_temp <-
  vessels__t_tne_sa_tne_in_t_cnt %>%
  dplyr::select(neg_in_t, starts_with("TRIP")) %>%
  dplyr::distinct()
toc()
# vessels__t_tne_sa_tne_in_t_cnt distinct: 0.53 sec elapsed
# vessels__t_tne_sa_tne_in_t_cnt unique: 322.19 sec elapsed

vessels__t_tne_sa_tne_in_t_cnt_temp %>%
  # dplyr::filter(neg_in_t == "both_t__tne") %>%
  dplyr::filter(neg_in_t == "unknown") %>%
  # Rows: 5,095
  # tail() %>%
  View()

View(vessels__t_tne_sa_tne_in_t_cnt)

# temp ----

# # vessels__t_tne_sa_tne_in_t_cnt distinct: 0.53 sec elapsed
# # vessels__t_tne_sa_tne_in_t_cnt unique: 322.19 sec elapsed
# vessels__t_tne_sa_tne_in_t_cnt_temp %>%
#       dplyr::filter(neg_in_t == "both_t__tne") %>%
#     dplyr::select(TRIP_week_num, TRIP_END_week_num) %>%
#     dplyr::distinct() %>%
#   # Rows: 5,095
#   # tail() %>%
#   dplyr::filter(!TRIP_week_num == TRIP_END_week_num) %>% dim()
# # 45
#
# # count total report number for trips + trip_neg ----
# print_df_names(vessels__t_tne_sa)
# # dplyr::between(TRIP_DATE,
# #                TRIP_START_DATE,
# #                TRIP_END_DATE) ~ "both_t__tne",
# # is.na(TRIP_START_TIME) ~ "tne",
# # is.na(TRIP_DATE) ~ "t",
#
# vessels__t_tne_sa %>%
#   dplyr::group_by(VESSEL_ID,
#                   permit_vessel_id,
#                   SUPPLIER_VESSEL_ID,
#                   SERO_OFFICIAL_NUMBER) %>%
#   dplyr::summarise(
#     distinct_weeks_ne =
#       dplyr::n_distinct(TRIP_week_num[neg_in_t == 'tne']),
#     distinct_start_weeks_t =
#       dplyr::n_distinct(TRIP_START_week_num[!neg_in_t == 'tne']),
#     distinct_end_weeks_t =
#       dplyr::n_distinct(TRIP_END_week_num[!neg_in_t == 'tne'])
#   ) %>%
#   dplyr::ungroup()
#
#   summarize(
#
#     # last_date = max(last_date_temp, na.rm = TRUE),
#
#     distinct_date = n_distinct(date[type != "Online"]),
#   ) %>%
#   dplyr::distinct()
#
#           # distinct_date = n_distinct(date[type != "Online"])
#
#   dplyr::summarise(
#     distinct_weeks_ne = dplyr::n_distinct(TRIP_week_num),
#     distinct_start_weeks_t = dplyr::n_distinct(TRIP_START_week_num),
#     distinct_end_weeks_t = dplyr::n_distinct(TRIP_END_week_num)
#   ) %>%
#   dplyr::mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t)) %>%
#   dplyr::ungroup()

#  end of temp ----

# 1) Get all weeks for permit per vessel
# 2) all trips (logbook) - weeks
# 3) all neg reports (DNF) - weeks
# 4) compare if all permit weeks have at least one or another

# 1) Get all weeks for permit per vessel
print_df_names(vessels_permit_bind_u1_sa_w_p_short)
# [1] "VESSEL_ID.v, VESSEL_ID, TOP.sa, PERMIT.sa, EFFECTIVE_DATE.sa, PERMIT_STATUS.sa, VESSEL_ALT_NUM.sa, permit_sa_gom.sa, my_end_date.sa, permit_sa_gom.p, eff_int_gom, eff_int_sa, COUNTY_CODE, STATE_CODE, ENTRY_DATE, SUPPLIER_VESSEL_ID, PORT_CODE, HULL_ID_NBR, STATE_REG_NBR, REGISTERING_STATE, VESSEL_NAME, PASSENGER_CAPACITY, VESSEL_TYPE, YEAR_BUILT, UPDATE_DATE, PRIMARY_GEAR, OWNER_ID, EVENT_ID, DE, UE, DC, UC, STATUS, SER_ID, UPDATED_FLAG, SERO_HOME_PORT_CITY, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, SERO_OFFICIAL_NUMBER, COAST_GUARD_NBR, permit_sa_gom.v, VESSEL_ID.p, weeks_perm"


# 2) all trips (logbook) - weeks
print_df_names(vessels__trips_22_l_sa_weeks_cnt_u)
# [1] "VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER, distinct_start_weeks_t, distinct_end_weeks_t, max_weeks_cnt_t"

setdiff(
  unique(vessels__trips_22_l_sa_weeks_cnt_u$permit_vessel_id),
  unique(vessels_permit_bind_u1_sa_w_p_short$VESSEL_ID.p)
)
# 44

setdiff(
  unique(vessels__trips_22_l_sa_weeks_cnt_u$SERO_OFFICIAL_NUMBER),
  unique(vessels_permit_bind_u1_sa_w_p_short$SERO_OFFICIAL_NUMBER)
)
# 0 (the same)

# 3) all neg reports (DNF) - weeks

print_df_names(vessels__trip_neg_22_l_sa_weeks_cnt_u)
# [1] "VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER, distinct_weeks_ne"

setdiff(
  unique(vessels__trip_neg_22_l_sa_weeks_cnt_u$SERO_OFFICIAL_NUMBER),
  unique(vessels_permit_bind_u1_sa_w_p_short$SERO_OFFICIAL_NUMBER)
)
# 0 (the same)


# setdiff(
#   unique(vessels__trips_22_l_sa_weeks_cnt_u$SERO_OFFICIAL_NUMBER),
#     unique(vessels__trip_neg_22_l_sa_weeks_cnt_u$SERO_OFFICIAL_NUMBER)
#
# )

# 4) see if all permit weeks have at least one or another
# SERO_OFFICIAL_NUMBER

vessels_permit_bind_u1_sa_w_p_short_weeks <-
  vessels_permit_bind_u1_sa_w_p_short |>
  dplyr::ungroup() |>
  dplyr::select(SERO_OFFICIAL_NUMBER, weeks_perm) |>
  # [1] 3877    2
  dplyr::distinct()
  # [1] 3873    2

# dim(vessels_permit_bind_u1_sa_w_p_short_weeks)

vessels__trips_22_l_sa_weeks_cnt_u_weeks <-
  vessels__trips_22_l_sa_weeks_cnt_u |>
  dplyr::ungroup() |>
  dplyr::select(SERO_OFFICIAL_NUMBER, max_weeks_cnt_t) |>
# [1] 1112    2
    dplyr::distinct()
# [1] 1071    2

vessels__trip_neg_22_l_sa_weeks_cnt_u_weeks <-
  vessels__trip_neg_22_l_sa_weeks_cnt_u |>
  dplyr::ungroup() |>
  dplyr::select(SERO_OFFICIAL_NUMBER, distinct_weeks_ne) |>
  dplyr::distinct()

# dim(vessels__trip_neg_22_l_sa_weeks_cnt_u_weeks)
# [1] 1714    2
# [1] 1656    2

# dim(vessels_permit_bind_u1_sa_w_p_short_weeks)
# 3873
# dim(v_p)
# 3869

# dim(vessels__trips_22_l_sa_weeks_cnt_u_weeks)
# 1071
# dim(t)
# 1069

v_p <-
  vessels_permit_bind_u1_sa_w_p_short_weeks |>
  dplyr::filter(!if_any(everything(), is.na))

t <- vessels__trips_22_l_sa_weeks_cnt_u_weeks |>
  dplyr::filter(!if_any(everything(), is.na))

tne <- vessels__trip_neg_22_l_sa_weeks_cnt_u_weeks |>
  dplyr::filter(!if_any(everything(), is.na))

dim(vessels__trip_neg_22_l_sa_weeks_cnt_u_weeks)
# 1656
dim(tne)
# 1655

join_v_p__t <-
  full_join(v_p,
            t)
# Joining with `by = join_by(SERO_OFFICIAL_NUMBER)`

# vessels_permit_bind_u1_sa_w_p_short_weeks[975,]
#   SERO_OFFICIAL_NUMBER weeks_perm
#   <chr>                     <dbl>
# 1 NA                         50.4
# vessels__trips_22_l_sa_weeks_cnt_u_weeks[490 ,]
#   SERO_OFFICIAL_NUMBER max_weeks_cnt_t
#   <chr>                          <int>
# 1 NA                                 1

join_v_p__t_tne <-
  full_join(join_v_p__t,
            tne)
# Joining with `by = join_by(SERO_OFFICIAL_NUMBER)`

View(join_v_p__t_tne)

# TODO: if SERO_OFFICIAL_NUMBER is NA, use an alternative id

# TODO: check that t and tne not in the same week! Before joining

# TODO: weeks per permit count separately for 2022
# vsl 586759
# p weeks 82.57143
# 39
# 22

# TODO: create all weeks for 2022, fill permit weeks, t and tne for each vsl

# interval_2022 = lubridate::interval(as.Date('2022-01-01'),
#                                     as.Date('2022-12-31'))

# dates_2022

# for each vessel full_join(dates_2022, permit_info) ----

data_overview(vessels_permit_bind_u_one_df$sa_only)
# EFFECTIVE_DATE.sa,
# my_end_date.sa,
# eff_int_sa,
# STATE_REG_NBR, SERO_OFFICIAL_NUMBER, COAST_GUARD_NBR,
# VESSEL_ID.v, VESSEL_ID, VESSEL_ALT_NUM.sa,
# VESSEL_ID.p

# check ids ----
# SERO_OFFICIAL_NUMBER  3870
# COAST_GUARD_NBR        159
# HULL_ID_NBR           3193
# STATE_REG_NBR          158
# SUPPLIER_VESSEL_ID    3877
# VESSEL_ID.v           3877
# VESSEL_ID.p           3871
# VESSEL_ALT_NUM.sa     3871
# VESSEL_ID             3871

data_overview(trips_info_2022_int_ah_w_y)
# SERO_VESSEL_PERMIT           1732
# TRIP_START_DATE              1026
# TRIP_END_DATE                 380
# VESSEL_ID                    1955
# trip_int                     1044
# TRIP_START_week_num            53
# TRIP_END_week_num              53
# TRIP_START_y                    2
# TRIP_END_y                      6
# TRIP_START_m                   17
# TRIP_END_m                     21

data_overview(trip_notifications_2022_ah_w_y)
# TRIP_START_week_num             53
# TRIP_END_week_num               54
# TRIP_START_y                     5
# TRIP_END_y                      15
# TRIP_START_m                    22
# TRIP_END_m                      35
# VESSEL_ID                      914
# TRIP_START_DATE                378
# TRIP_END_DATE                  393

data_overview(trip_neg_2022_w_y)
# VESSEL_ID       3414
# TRIP_week_num     53
# TRIP_DATE_y        2
# TRIP_DATE_m       12
# TRIP_DATE        365

# check vessels the same for t and tne ----
trip_neg_2022_w_y__v_ids <-
  trip_neg_2022_w_y |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct()

# dim(trip_neg_2022_w_y__v_ids)
# 3414

trips_info_2022_int_ah_w_y__v_ids <-
  trips_info_2022_int_ah_w_y |>
  dplyr::filter(!is.na(SERO_VESSEL_PERMIT)) |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct()

dim(trips_info_2022_int_ah_w_y__v_ids)
# 1711

trips_info_2022_int_ah_w_y__v_ids_all <-
  trips_info_2022_int_ah_w_y |>
  # dplyr::filter(!is.na(SERO_VESSEL_PERMIT)) |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct()

intersect(trips_info_2022_int_ah_w_y__v_ids$VESSEL_ID,
          trip_neg_2022_w_y__v_ids$VESSEL_ID) |>
  length()
# 1246

head(trips_info_2022_int_ah_w_y__v_ids$VESSEL_ID)
head(trip_neg_2022_w_y__v_ids$VESSEL_ID)

vessel_ids_t__tne <-
  intersect(
    trips_info_2022_int_ah_w_y__v_ids_all$VESSEL_ID,
    trip_neg_2022_w_y__v_ids$VESSEL_ID
  )

length(vessel_ids_t__tne)
# [1] 1290

head(vessel_ids_t__tne)

# check vessels the same for p_v and t and tne ----
vessels_permit_bind_u_one_df__v_ids <-
  vessels_permit_bind_u_one_df$sa_only |>
  dplyr::select(VESSEL_ID) |>
  dplyr::distinct()

# STATE_REG_NBR, SERO_OFFICIAL_NUMBER, COAST_GUARD_NBR,
# VESSEL_ID.v, VESSEL_ID, VESSEL_ALT_NUM.sa,
# VESSEL_ID.p

dim(vessels_permit_bind_u_one_df__v_ids)
# 3871

intersect(as.character(vessel_ids_t__tne),
          as.character(vessels_permit_bind_u_one_df__v_ids$VESSEL_ID)) |>
  length()
# 0

id_names <-
  c(
    "COAST_GUARD_NBR",
    "SERO_OFFICIAL_NUMBER",
    "STATE_REG_NBR",
    "VESSEL_ALT_NUM.sa",
    "VESSEL_ID",
    "VESSEL_ID.p",
    "VESSEL_ID.v"
  )

map(id_names,
    function(x) {
      print(x)
      curr_v_ids <-
        vessels_permit_bind_u_one_df$sa_only[[x]] |>
        as.character()

      intersect(
        as.character(vessel_ids_t__tne),
        curr_v_ids
      ) |>
        length()
    })
# str(vessels_permit_bind_u_one_df__v_ids$VESSEL_ID)
# VESSEL_ID.v = 1031

# TODO: the same for vessel and permit
print_df_names(permit_info)
# VESSEL_ID, VESSEL_ALT_NUM
print_df_names(permit_info_r_l_overlap_join1_w_dual_22__list$sa_only)
# VESSEL_ID, VESSEL_ALT_NUM.sa

print_df_names(vessels_all)

vessels_all__v_id_names <-
  c(
    "SERO_OFFICIAL_NUMBER",
    "COAST_GUARD_NBR",
    "STATE_REG_NBR",
    "VESSEL_ID",
    "SUPPLIER_VESSEL_ID"
  )

map(vessels_all__v_id_names,
    function(x) {
      curr_v_ids <-
        vessels_all[[x]] |>
        unique() |>
        as.character()

      print(x)
      print("__VESSEL_ID__")
      intersect(
        as.character(
          permit_info_r_l_overlap_join1_w_dual_22__list$sa_only$VESSEL_ID
        ),
        curr_v_ids
      ) |>
        length() |>
        print()

      print("__VESSEL_ALT_NUM.sa__")
      intersect(
        as.character(
          permit_info_r_l_overlap_join1_w_dual_22__list$sa_only$VESSEL_ALT_NUM.sa
        ),
        curr_v_ids
      ) |>
        length() |>
        print()
    })
# "SERO_OFFICIAL_NUMBER"
# "__VESSEL_ID__"
# 3868
# "__VESSEL_ALT_NUM.sa__"
# 3720

# "SUPPLIER_VESSEL_ID"
# "__VESSEL_ID__"
# 3870
# "__VESSEL_ALT_NUM.sa__"
# 3736

# "COAST_GUARD_NBR"
# "__VESSEL_ID__"
# 2520
# "__VESSEL_ALT_NUM.sa__"
# 2373

# "STATE_REG_NBR"
# "__VESSEL_ID__"
# 1361
# "__VESSEL_ALT_NUM.sa__"
# 1509

# "VESSEL_ID"
# "__VESSEL_ID__"
# 14
# "__VESSEL_ALT_NUM.sa__"
# 13

# compare vessel ids in vessel
vessels_all__v_id_names <-
  c(
    "SERO_OFFICIAL_NUMBER",
    "COAST_GUARD_NBR",
    "STATE_REG_NBR",
    "VESSEL_ID",
    "SUPPLIER_VESSEL_ID"
  )

vessels_all_ids <-
  vessels_all |>
  dplyr::select(all_of(vessels_all__v_id_names)) |>
  dplyr::distinct()

str(vessels_all_ids)
# tibble [140,405 × 5] (S3: tbl_df/tbl/data.frame)
#  $ SERO_OFFICIAL_NUMBER: chr [1:140405] "658020" NA "638529" NA ...
#  $ COAST_GUARD_NBR     : chr [1:140405] "658020" NA "638529" "557344" ...
#  $ STATE_REG_NBR       : chr [1:140405] NA "NY4726GN" NA NA ...
#  $ VESSEL_ID           : num [1:140405] 80803 80805 80807 80809 80811 ...
#  $ SUPPLIER_VESSEL_ID  : chr [1:140405] "658020" "NY4726GN" "638529" "557344" ...

vessels_all__v_id_names_l <-
  vessels_all__v_id_names |>
  map(
      function(id_name) {
        vessels_all_ids[id_name] |>
          dplyr::distinct()
      })

vessels_all__v_id_names_l_named <- set_names(vessels_all__v_id_names_l1, vessels_all__v_id_names)

# View(vessels_all__v_id_names_l_named)

library(gtools)
vessels_all__v_id_names__pairs <-
  combinations(length(vessels_all__v_id_names),
               2,
               vessels_all__v_id_names,
               repeats.allowed = F) |>
  as.data.frame()

# head(vessels_all__v_id_names__pairs)
# 6 SERO_OFFICIAL_NUMBER   SUPPLIER_VESSEL_ID

vessels_all__v_id_names__pairs_length <-
  vessels_all__v_id_names__pairs |>
  rowwise() %>%
  dplyr::mutate(
    inter =
      intersect(
        vessels_all__v_id_names_l_named[[V1]][[V1]],
        vessels_all__v_id_names_l_named[[V2]][[V2]]
      ) |>
      length()
  )

write_csv(vessels_all__v_id_names__pairs_length, "vessels_all__v_id_names__pairs_length.csv")

# for each vessel full_join(dates_2022, permit_info) ----
# View(vessels__trips_22_l)
# full_join(dates_2022, vessels_permit_bind_u1_sa_w_p_short_weeks)

vessels__trip_neg_22_l_sa_short_all_dates_t_start |>
  dplyr::select(TRIP_week_num, TRIP_DATE_y) |>
  dplyr::distinct() |>
  dim()
# 63

vessels__trip_neg_22_l_sa_short__dates |>
  dplyr::select(TRIP_week_num, TRIP_DATE_y) |>
  dplyr::distinct() |>
  dim()
# 52, 2

View(vessels__trips_22_l_sa_short_all_dates_t_start_all_cols)
vessels__trips_22_l_sa_short_all_dates_t_start_all_cols |>
  dplyr::select(WEEK_OF_YEAR, YEAR) |>
  dplyr::distinct() |>
  dim()
# 53, 2

v_p_t_tne_dates_join <-
  full_join(
    vessels__trips_22_l_sa_short_all_dates_t_start_all_cols,
    vessels__trip_neg_22_l_sa_short__dates
  )
# Joining with `by = join_by(permit_vessel_id, YEAR, WEEK_OF_YEAR)`
View(v_p_t_tne_dates_join)

v_p_t_tne_dates_join |> dplyr::filter(permit_vessel_id == "1020057") |>
  dplyr::filter(is.na(TRIP_START_week_num) & is.na(TRIP_week_num))
# 0

v_p_t_tne_dates_join |> dplyr::filter(permit_vessel_id == "1020057") |>
       dplyr::filter(!is.na(TRIP_START_week_num) & !is.na(TRIP_week_num)) |>
  View()

# check weeks and reports or dnfs (= t and tne)

v_p_t_tne_dates_join |> dplyr::filter(permit_vessel_id == "1020057") |>
       dplyr::filter(is.na(TRIP_START_week_num)) |>
  dplyr::select(WEEK_OF_YEAR, YEAR) |>
  dplyr::distinct() |>
  View()

# dplyr::mutate(a = mean(y[x2 == 1]), z = y / a)

v_p_t_tne_dates_join_week_cnts <-
  v_p_t_tne_dates_join |>
  dplyr::group_by(permit_vessel_id, SUPPLIER_VESSEL_ID) |>
  dplyr::mutate(
    total_WEEK_OF_YEAR = n_distinct(WEEK_OF_YEAR),
    tne_only =
      sum(!is.na(TRIP_week_num)),
    no_tne =
      sum(is.na(TRIP_week_num)),
    t_only =
      sum(!is.na(TRIP_START_week_num)),
    no_t =
      sum(is.na(TRIP_START_week_num)),
    both_t_tne = sum(!is.na(TRIP_START_week_num) &
                       !is.na(TRIP_week_num)),
    no_t_tne = sum(is.na(TRIP_START_week_num) &
                     is.na(TRIP_week_num))
  ) |> 
  dplyr::ungroup()

# View(x1)
       # ,
         # tne_only = sum(TRIP_week_num, na.rm = T),
         # )

# add weeks to permits ----
# dplyr::mutate(weeks_perm = eff_int_sa / lubridate::dweeks(1))
# vessels_permit_bind_u1_sa_w_p_short

permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p <-
  permit_info_r_l_overlap_join1_w_dual_22__list$sa_only |>
  dplyr::select(-ends_with("gom")) |>
  dplyr::mutate(weeks_perm =
           (eff_int_sa / lubridate::dweeks(1)) |>
           round()
         ) |>
  dplyr::distinct()

dim(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p)
# [1] 13693    10

print_df_names(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p)
print_df_names(v_p_t_tne_dates_join)

# permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p_short <-
#   permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p |> 
#   dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, weeks_perm) |> 
#   # dplyr::filter()
#   dplyr::distinct()
# 
# data_overview(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p_short)
# # Max.   :886.00  
# # 
# # Count unique values in each column:                     .
# # VESSEL_ID         3875
# # VESSEL_ALT_NUM.sa 3872
# # weeks_perm         192
# 
# permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p_short |> 
#   dplyr::filter(weeks_perm > 53) |> 
#   dplyr::distinct() |> 
#   View()

# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only |> 
#     dplyr::filter(VESSEL_ID == 'FL7957JL') |> View()
# dplyr::select j.*, weeks_perm
# tic("v_p_t_tne_dates_join__p sql")
# v_p_t_tne_dates_join__p_sql <-
#   sqldf("
#         dplyr::select j.*, weeks_perm
#         FROM
#         permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p_short p
#         FULL join
#         v_p_t_tne_dates_join j
#         on(VESSEL_ID = permit_vessel_id)
#         ")
# toc()
# too slow
# v_p_t_tne_dates_join__p sql: 25.11 sec elapsed

# View(v_p_t_tne_dates_join__p_sql)

# tic("v_p_t_tne_dates_join__p")
# v_p_t_tne_dates_join__p <-
# dplyr::full_join(
#   v_p_t_tne_dates_join,
#   permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p_short,
#   dplyr::join_by(permit_vessel_id == VESSEL_ID),
#   relationship = "many-to-many"
# )
# toc()
# v_p_t_tne_dates_join__p: 0.03 sec elapsed

# tic("v_p_t_tne_dates_join_week_cnts__p")
# v_p_t_tne_dates_join_week_cnts__p <-
# dplyr::full_join(
#   v_p_t_tne_dates_join_week_cnts,
#   permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p_short,
#   dplyr::join_by(permit_vessel_id == VESSEL_ID),
#   relationship = "many-to-many"
# )
# toc()
# v_p_t_tne_dates_join_week_cnts__p: 0.47 sec elapsed

# View(v_p_t_tne_dates_join_week_cnts__p)

# add 2022 period weeks to permit ----

# df %>%
#   dplyr::mutate(overlap =
#            map2(start, end,
#                 ~ sum(
#                   seq(.x, .y, by = '1 day') %in%
#                     seq(my.start, my.end, by = '1 day')
#                 )))
# I think it is 0 5 5 12 0 –
# > intersect(interval(ymd("2017-11-23"),ymd("2017-11-30")),interval(ymd("2017-11-20"),ymd("2018-11-16")))

permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22 <-
  permit_info_r_l_overlap_join1_w_dual_22__list$sa_only |>
  dplyr::select(-ends_with("gom")) |>
  dplyr::mutate(permit_2022 = lubridate::intersect(eff_int_sa,
                      interval_2022)) |>
  dplyr::mutate(weeks_perm_2022_amnt =
           (permit_2022 / lubridate::dweeks(1)) |>
           round()
         ) |>
  dplyr::distinct()

min(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22$weeks_perm_2022_amnt)
# 0-52  

data_overview(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53

permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22_short <-
  permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22 |>
  dplyr::select(VESSEL_ID, VESSEL_ALT_NUM.sa, weeks_perm_2022_amnt) |>
  dplyr::distinct()

tic("v_p_t_tne_dates_join_week_cnts__p22")
v_p_t_tne_dates_join_week_cnts__p22 <-
dplyr::full_join(
  v_p_t_tne_dates_join_week_cnts,
  permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22_short,
  dplyr::join_by(permit_vessel_id == VESSEL_ID),
  relationship = "many-to-many"
)
toc()
# v_p_t_tne_dates_join_week_cnts__p22: 0.44 sec elapsed
View(v_p_t_tne_dates_join_week_cnts__p22)

# v_p_t_tne_dates_join_week_cnts__p22: 0.44 sec elapsed

v_p_t_tne_dates_join_week_cnts__p22 |> 
   dplyr::filter(weeks_perm_2022_amnt > total_WEEK_OF_YEAR) |> 
   View()

v_p_t_tne_dates_join_week_cnts__p22 |> 
    dplyr::filter(permit_vessel_id == '1020057') |> dim()
# 104


v_p_t_tne_dates_join_week_cnts__p22_short <-
  v_p_t_tne_dates_join_week_cnts__p22 |>
  dplyr::select(
    -c(
      TRIP_START_week_num,
      TRIP_END_week_num,
      TRIP_START_y,
      TRIP_END_y,
      YEAR,
      WEEK_OF_YEAR,
      TRIP_week_num,
      TRIP_DATE_y,
    )
  ) |>
  dplyr::distinct()

View(v_p_t_tne_dates_join_week_cnts__p22_short)
