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

## Fewer fields for vessels_permits_2022_r ----
vessels_permits_2022_r_short <-
  vessels_permits_2022_r |> 
  select(
    PERMIT_VESSEL_ID,
    VESSEL_VESSEL_ID,
    COAST_GUARD_NBR,
    SERO_OFFICIAL_NUMBER,
    STATE_REG_NBR,
    SUPPLIER_VESSEL_ID,
    VESSEL_ALT_NUM,
    EFFECTIVE_DATE,
    END_DATE,
    EXPIRATION_DATE,
    permit_sa_gom
  ) |>
  distinct()

# dim(vessels_permits_2022_r)
# [1] 40474    52
# dim(vessels_permits_2022_r_short)
# [1] 9442   11

## add my_end_date ----
# print_df_names(vessels_permits_2022_r)
vessels_permits_2022_r_end_date <-
  vessels_permits_2022_r_short |> 
  mutate(my_end_date =
           case_when((END_DATE < EFFECTIVE_DATE) &
                       (EXPIRATION_DATE > EFFECTIVE_DATE)
                     ~ EXPIRATION_DATE,
                     .default =
                       dplyr::coalesce(END_DATE,                                     EXPIRATION_DATE)
           )) %>%
  select(-c(END_DATE,
            EXPIRATION_DATE)) %>%
  distinct()

# View(vessels_permits_2022_r_end_date)
# [1] 20231    51
# short
# [1] 9433   10

## add weeks and months ----
# print_df_names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22)

vessels_permits_2022_r_end_date_w_y <-
vessels_permits_2022_r_end_date |> 
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

# dim(vessels_permits_2022_r_end_date_w_y)
# [1] 9433   16

## split by permit ----
vessels_permits_2022_r_end_date_l <-
  vessels_permits_2022_r_end_date_w_y %>%
  split(as.factor(vessels_permits_2022_r_end_date_w_y$permit_sa_gom))

map_df(vessels_permits_2022_r_end_date_l, dim)
#   gom_only sa_only
# 1     2525    6908
# 2       16      16

## join by overlap of gom and sa (= dual) ----
# From Help:
# It is common to have right-open ranges with bounds like `[)`, which would
# mean an end value of `415` would no longer overlap a start value of `415`.
# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
# View(vessels_permits_2022)
# View(vessels_permits_2022_r_end_date_l$gom_only)
by <- join_by(PERMIT_VESSEL_ID,
              VESSEL_VESSEL_ID,
              overlaps(x$EFFECTIVE_DATE,
                       x$my_end_date,
                       y$EFFECTIVE_DATE,
                       y$my_end_date,
                       bounds = "[)"))

tic("vessels_permits_2022_r_end_date_overlap_join")
vessels_permits_2022_r_end_date_l_overlap_join <-
  full_join(
  vessels_permits_2022_r_end_date_l$gom_only,
  vessels_permits_2022_r_end_date_l$sa_only,
  by,
  suffix = c(".gom", ".sa")
)
toc()
# permit_info_r_l_overlap_join1: 0.66 sec elapsed

dim(vessels_permits_2022_r_end_date_l_overlap_join)
# [1] 20918   112
# short
# [1] 8939   30

vessels_permits_2022_r_end_date_l_overlap_join %>%
  select(VESSEL_VESSEL_ID) %>%
  distinct() %>%
  dim()
# select(PERMIT_VESSEL_ID) %>%
# [1] 5461    1

vessels_permits_2022 %>%
  # select(QCSJ_C000000000300000
  # [1] 5461    1
  # select(QCSJ_C000000000300001
  # [1] 5461    1
  
  select(QCSJ_C000000000300000,
         QCSJ_C000000000300001,
         VESSEL_ALT_NUM) %>%
  distinct() %>%
  dim()
# [1] 5462    1

### add "dual" to intervals ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual <-
  vessels_permits_2022_r_end_date_l_overlap_join %>%
  mutate(permit_sa_gom =
           case_when(
             !is.na(permit_sa_gom.sa) &
               !is.na(permit_sa_gom.gom) ~ "dual",
             .default =
               dplyr::coalesce(permit_sa_gom.sa,
                               permit_sa_gom.gom)

           ))

vessels_permits_2022_r_end_date_l_overlap_join_w_dual %>%
  select(permit_sa_gom) %>%
  distinct()
# all 3

dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual)
# [1] 20918   114
# [1] 8939   31

# View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual)

# to get dual in the overlapping period:
# filter(!is.na(permit_sa_gom.sa))

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual %>%
  mutate(
    eff_int_gom =
      lubridate::interval(EFFECTIVE_DATE.gom,
                          my_end_date.gom),
    eff_int_sa =
      lubridate::interval(EFFECTIVE_DATE.sa,
                          my_end_date.sa)
  ) %>%
  #   mutate(int_overlapped = int_overlaps(eff_int_gom, eff_int_sa) )
  filter(int_overlaps(eff_int_gom,
                      interval_2022) |
           int_overlaps(eff_int_sa,
                      interval_2022)
         )

#### check ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  select(permit_sa_gom) %>%
  distinct()
# all 3

# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
#   filter(!VESSEL_VESSEL_ID == PERMIT_VESSEL_ID) |> 
#   dim()
# [1] 8822   33


vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  filter(permit_sa_gom == "dual") %>%
  # select(VESSEL_VESSEL_ID) %>%
  select(PERMIT_VESSEL_ID) %>%
  distinct() %>%
  dim()
# 379
# 353

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  select(PERMIT_VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  filter(!(PERMIT_VESSEL_ID == VESSEL_ALT_NUM.sa)) %>%
dim()
# 660
# 636
# [1] 274   3

## split permits by region again ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  split(as.factor(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22$permit_sa_gom))

map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list, dim)
#    dual gom_only sa_only
# 1   635     1879    6308
# 2    33       33      33

# TODO: compare vessel_permits from db and v_permits by overlapping with interval 2022
# adjust the query

# end vessel_permits preparations ----

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
trip_notifications_2022 %>%
   select(TRIP_TYPE) %>% distinct()
#     TRIP_TYPE
# 1           H
# 3           A
# 383         R
# 697         C

trip_notifications_2022_ah <-
  trip_notifications_2022 %>%
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

## to trip notifications ----
trip_notifications_2022_ah_w_y <-
  trip_notifications_2022_ah %>%
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
# print_df_names(trip_neg_2022)
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

# trip_neg_2022_w_y_cnt_u <-
# vessels__trip_neg_22_l_sa_weeks_cnt_u <-
#   vessels__trip_neg_22_l$sa_only %>%
#   group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
#   summarise(distinct_weeks_ne = n_distinct(TRIP_week_num))

# dim(vessels__trip_neg_22_l_sa_weeks_cnt_u)
# [1] 1709    5

## trip_notif weeks count per vessel ----
# vessels__trip_notif_22_l_sa_weeks_cnt_u <-
#   vessels__trip_notif_22_l$sa_only %>%
#   group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
#   summarise(distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
#             distinct_end_weeks_tn = n_distinct(TRIP_END_week_num))
# 
# dim(vessels__trip_notif_22_l_sa_weeks_cnt_u)
# # 17
# vessels__trip_notif_22_l_sa_weeks_cnt_u %>%
#    filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
#    dim()
# [1] 0 6
# ok

## trips weeks count per vessel ----
# View(trips_info_2022_int_ah_w_y)
trips_info_2022_int_ah_w_y_weeks_cnt_u <-
  trips_info_2022_int_ah_w_y %>%
    # browser()
    group_by(VESSEL_ID) %>%
      summarise(
        distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
        distinct_end_weeks_t = n_distinct(TRIP_END_week_num)
      ) %>%
      mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))
  
dim(trips_info_2022_int_ah_w_y_weeks_cnt_u)
# [1] 1110    7
# [1] 1934    4

trips_info_2022_int_ah_w_y_weeks_cnt_u %>%
   filter(!distinct_start_weeks_t == distinct_end_weeks_t) %>%
   dim()
# 27
# 63

# Join everything to dates_2022 ----
# View(dates_2022)
## p_v ----
View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list)
     # , dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1  3281     3673   13749
# 2   116      116     116

# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual$EFFECTIVE_DATE.gom
# effective_date, COMPLETE_DATE

my_f <- function(curr_permit_region) {
  # browser()
  curr_permit_region_o <- paste0(curr_permit_region, "_only")
  curr_df <-
    vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list[[curr_permit_region_o]]
  curr_f_name <- paste0("EFFECTIVE_DATE.", curr_permit_region)
  res <-
    left_join(dates_2022,
              curr_df,
              join_by(COMPLETE_DATE == !!sym(curr_f_name)))
  
  return(res)
}

nl <- list("gom", "sa" )
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates <-
  map(nl, my_f)

View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates)

map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates, dim)

## t ----
## tne ----
## tn ----


# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance

# add 2022 SA period weeks cnt to permit ----

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list__sa_w_p22 <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$sa_only |>
  # remove gom, keep sa only
  select(-ends_with("gom")) |>
  mutate(permit_2022 = lubridate::intersect(eff_int_sa,
                      interval_2022)) |>
  mutate(weeks_perm_2022_amnt =
           (permit_2022 / lubridate::dweeks(1)) |>
           round()
         ) |>
  distinct()

min(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list__sa_w_p22$weeks_perm_2022_amnt)
max(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list__sa_w_p22$weeks_perm_2022_amnt)
# 0-52  

# data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list__sa_w_p22)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53
# VESSEL_ID                3888


## join trips and trip_negative (logbooks + DNFs) ----
by_t__tne = join_by(
  VESSEL_ID,
  permit_vessel_id,
  SUPPLIER_VESSEL_ID,
  SERO_OFFICIAL_NUMBER,
  TRIP_week_num == TRIP_START_week_num,
  TRIP_DATE_y == TRIP_START_y
)

tic("join trips_info_2022_int_ah_w_y and tne")
vessels__t_tne_sa <-
  full_join(
    trips_info_2022_int_ah_w_y,
    trip_neg_2022_w_y,
    by_t__tne,
    suffix = c(".tne", ".t"),
    relationship = "many-to-many"
  )
toc()
# join vessels__t_tne_sa: 1.55 sec elapsed

dim(vessels__t_tne_sa)
# [1] 457647    146
# [1] 458922    148



vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list__sa_w_p22

