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

# vessels_permits_2022_r_short |> 
#   filter(SERO_OFFICIAL_NUMBER == 'FL8701TB') |> View()
# 23 is here

## add my_end_date ----
# print_df_names(vessels_permits_2022_r)
vessels_permits_2022_r_end_date <-
  vessels_permits_2022_r_short |>
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
  distinct()

# vessels_permits_2022_r_end_date |> 
#   filter(SERO_OFFICIAL_NUMBER == 'FL8701TB') |> View()
# correct now

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

# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual)
# [1] 8949   35

tic("vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22")
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
toc()
# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22: 225.5 sec elapsed

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
# 357

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  select(PERMIT_VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  filter(!(PERMIT_VESSEL_ID == VESSEL_ALT_NUM.sa)) %>%
dim()
# 660
# 636
# [1] 274   3
# 282

## fewer v ids ----

id_names <- c(
  "PERMIT_VESSEL_ID",
  "VESSEL_VESSEL_ID",
  "COAST_GUARD_NBR.gom",
  "SERO_OFFICIAL_NUMBER.gom",
  "STATE_REG_NBR.gom",
  "SUPPLIER_VESSEL_ID.gom",
  "VESSEL_ALT_NUM.gom",
  "COAST_GUARD_NBR.sa",
  "SERO_OFFICIAL_NUMBER.sa",
  "STATE_REG_NBR.sa",
  "SUPPLIER_VESSEL_ID.sa",
  "VESSEL_ALT_NUM.sa")

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 |>
  rowwise() |>
  mutate(all_ids = list(
    c(
      PERMIT_VESSEL_ID,
      VESSEL_VESSEL_ID,
      COAST_GUARD_NBR.gom,
      SERO_OFFICIAL_NUMBER.gom,
      STATE_REG_NBR.gom,
      SUPPLIER_VESSEL_ID.gom,
      VESSEL_ALT_NUM.gom,
      COAST_GUARD_NBR.sa,
      SERO_OFFICIAL_NUMBER.sa,
      STATE_REG_NBR.sa,
      SUPPLIER_VESSEL_ID.sa,
      VESSEL_ALT_NUM.sa
    )
  )) |>
  mutate(unique_ids = list(na.omit(unique(all_ids)))) |>
  ungroup() |>
  select(-any_of(id_names), -all_ids)

# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22)
# 8949 37
# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid)
# [1] 8949   39
# [1] 8949   26

## fewer fields
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid %>%
  select(eff_int_gom, eff_int_sa, unique_ids, permit_sa_gom)

# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short)
# [1] 8949    4

## split permits by region again ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short %>%
  split(as.factor(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid$permit_sa_gom))

map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list, dim)
#    dual gom_only sa_only
# 1   635     1879    6308
# 2    33       33      33
# 1   653     1940    6356
# 2    37       37      37
# 2    26       26      26
# 2     4        4       4

# TODO: compare vessel_permits from db and v_permits by overlapping with interval 2022
# adjust the query

## union intervals ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual |>
  mutate(union_int_sa_gom =
           lubridate::union(eff_int_gom, eff_int_sa))

View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list)

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

# print_df_names(trip_neg_2022_w_y)
trip_neg_2022_w_y_cnt_u <-
  trip_neg_2022_w_y |>
  group_by(VESSEL_ID) %>%
  summarise(distinct_weeks_ne = n_distinct(TRIP_week_num))

dim(trip_neg_2022_w_y_cnt_u)
# [1] 1709    5
# [1] 3414    2

## trip_notif weeks count per vessel ----
trip_notifications_2022_ah_w_y_cnt_u <-
  trip_notifications_2022_ah_w_y |>
  group_by(VESSEL_ID) |>
  summarise(
    distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
    distinct_end_weeks_tn = n_distinct(TRIP_END_week_num)
  )

dim(trip_notifications_2022_ah_w_y_cnt_u)
# [1] 914   3

trip_notifications_2022_ah_w_y_cnt_u %>%
   filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
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

# Keep only sero permitted ----
trips_info_2022_int_ah_w_y_sero <-
  trips_info_2022_int_ah_w_y |>
  filter(!is.na(SERO_VESSEL_PERMIT)) |>
  distinct()

# no:
# trip_notifications_2022_ah_w_y_dates |>  filter(!is.na(SERO_VESSEL_PERMIT)) |> dim()
# trip_neg_2022_w_y_dates |>  filter(!is.na(SERO_VESSEL_PERMIT)) |> dim()


# Join dates_2022 and everything ----
# get_v_ids and dates only

# View(dates_2022)
## p_v ----

### split all permit intervals by day ----
# View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list)

# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual |> 
#   group_by(unique_ids) |> 
#   mutate(union_int_sa = )

### end split all permit intervals by day ----
map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1  3281     3673   13749
# 2   116      116     116
# 1   635     1879    6308
# 2    33       33      33
# 1   653     1940    6356
# 2    37       37      37
# 2    26       26      26

l_names <- 
  names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list)
# [1] "dual"     "gom_only" "sa_only"

# str_split("sa_only", "_")
# str_split("saonly", "_")
my_f <- function(curr_permit_region) {

  # browser()
  curr_df <-
    vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list[[curr_permit_region]]

  curr_permit_s_g_all <- curr_df |>
    select(permit_sa_gom) |>
    distinct()
  
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

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates <-
  map(l_names, my_f)

names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates) <- l_names

### check v_ dates join ---- 
map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   648     1271    3615
# 2    36       36      36
# 1   657     1267    3616
# 2    40       40      40

# data_overview(dates_2022)
# COMPLETE_DATE 427

# data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual)
# PERMIT_VESSEL_ID            353, 357
# VESSEL_VESSEL_ID            same
# EFFECTIVE_DATE.gom          232, 234
# EFFECTIVE_DATE.sa           230, 232

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
  select(VESSEL_ID) |>
  distinct()

trips_info_2022_int_ah_w_y_ids <-
  trips_info_2022_int_ah_w_y_sero |>
  select(VESSEL_ID) |>
  distinct()

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

# why more vessel_ids? NA

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
#   filter(!is.na())
# data_overview(trips_info_2022_int_ah_w_y_dates)
# 
# trip_neg_2022_w_y_dates
# data_overview(trips_info_2022_int_ah_w_y_dates)

## add 2022 SA period weeks amnt to v permit ----
# vessels_permits_2022 |> View()
#   filter(PERMIT_VESSEL_ID == 'FL8701TB') |> View()
# vessels_permits_2022 |>
#   filter(SERO_OFFICIAL_NUMBER == 'FL8701TB') |> View()
# TODO: fix end in 2023 disappeared

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates$sa_only |>
  # remove gom, keep sa only
  select(-ends_with("gom")) |> 
  mutate(permit_2022 =
           lubridate::intersect(eff_int_sa,
                                interval_2022)) |>
  mutate(weeks_perm_2022_amnt =
           (permit_2022 / lubridate::dweeks(1)) |>
           round()) |>
  distinct()

# View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
min(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22$weeks_perm_2022_amnt, na.rm = T)
max(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22$weeks_perm_2022_amnt, na.rm = T)
# 0-52
# 0-109 (without interval_2022)
# 0-52

# View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
# data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53
# VESSEL_ID                3888
# today() [1] "2023-07-17"
# VESSEL_VESSEL_ID           3181
# weeks_perm_2022_amnt         54

## compare vessel_ids ----

### v_p ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids <-
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |> 
  select(contains("vessel")) |> 
  distinct()

dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids)
# [1] 3182    4
# 3181

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids |> 
  filter(!PERMIT_VESSEL_ID == SUPPLIER_VESSEL_ID.sa) |> glimpse()
# $ PERMIT_VESSEL_ID      <chr> "NC0676EK"
# $ VESSEL_VESSEL_ID      <dbl> 383419
# $ SUPPLIER_VESSEL_ID.sa <chr> "1292480"
# $ VESSEL_ALT_NUM.sa     <chr> "NC0676EK"
# FHIER correct:
# 1292480:NC0676EK........ SOUTHERN RUN - BENJAMIN AUGUSTUS MORRIS            (828) 4298076

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids |> 
  filter(!VESSEL_ALT_NUM.sa == SUPPLIER_VESSEL_ID.sa) |> 
  dim()
# 130

### v_p vs. t ----
trips_info_2022_int_ah_w_y_dates_ids <-
  trips_info_2022_int_ah_w_y_dates |>
  filter(!is.na(SERO_VESSEL_PERMIT)) |> 
  select(contains("vessel")) |>
  distinct()

dim(trips_info_2022_int_ah_w_y_dates_ids)
# [1] 2244    3
# [1] 1729    3 filter(!is.na(SERO_VESSEL_PERMIT)) |> 
# setdiff(
#   trips_info_2022_int_ah_w_y_dates_ids$VESSEL_ID,
#   vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID
# ) |> glimpse()
  # length()
  # 774
# 775

# trips_info_2022_int_ah_w_y_dates_ids |> 
#   filter(VESSEL_ID == '327558') |> 
#   glimpse()

setdiff(
  trips_info_2022_int_ah_w_y_dates_ids$SERO_VESSEL_PERMIT,
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$PERMIT_VESSEL_ID
) |>
  length()
# 1713
# SUPPLIER_VESSEL_ID.sa, VESSEL_ALT_NUM.sa same

setdiff(
  trips_info_2022_int_ah_w_y_dates_ids$SERO_VESSEL_PERMIT,
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID
) |>
  length()
# [1] 1710

### v_p vs. tne ----

# View(trip_neg_2022_w_y_dates)
trip_neg_2022_w_y_dates_ids <-
  trip_neg_2022_w_y_dates |>
  select(contains("vessel")) |>
  distinct()

# tail(trip_neg_2022_w_y_dates_ids)
# [1] 3415    1
# VESSEL_ID 
setdiff(
  trip_neg_2022_w_y_dates_ids$VESSEL_ID,
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID
) |>
  length()
# 1984

setdiff(
  trip_neg_2022_w_y_dates_ids$VESSEL_ID,
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$PERMIT_VESSEL_ID
) |>
  length()
# 3414

### reverse (in permit, but not in t or tne: ----
setdiff(
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID,
  trips_info_2022_int_ah_w_y_dates_ids$VESSEL_ID
) |>
  length()
# 2245

setdiff(
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID,
  trip_neg_2022_w_y_dates_ids$VESSEL_ID
) |>
  length()
# 1751, 1750

# use vessel and vessel_id

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
  filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
  dim()
# [1] 5971   99
# [1] 5575   99

## fewer columns t__tne__dates_w_cnt_t_tne ----
# print_df_names(t__tne__dates_w_cnt_t_tne)

t__tne__dates_w_cnt_t_tne_short <-
  t__tne__dates_w_cnt_t_tne |>
  select(
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
  distinct()

dim(t__tne__dates)
# [1] 838720     95
# [1] 823051     95

dim(t__tne__dates_w_cnt_t_tne)
# [1] 838720     99
# 823051
dim(t__tne__dates_w_cnt_t_tne_short)
# [1] 838720     23
# 823051

# View(t__tne__dates_w_cnt_t_tne_short)

## Total vessels SA 2022 ----
# 3181

## Compare week counts t/tne ----

# both t & tne
t__tne__dates_w_cnt_t_tne_short |>
  filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |> 
  select(VESSEL_ID) |>
  distinct() |> 
  dim()
# [1] 5971   23
# [1] 515   1 select(VESSEL_ID) |>
# 481 sero only

# t only
t__tne__dates_w_cnt_t_tne_short |>
  filter(!is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
  select(VESSEL_ID) |>
  distinct() |>
  dim()
# [1] 91099    23
# [1] 1834    1 select(VESSEL_ID) |>
# 1612    sero only

# tne only
# View(t__tne__dates_w_cnt_t_tne_short)
t__tne__dates_w_cnt_t_tne_short |>
  filter(is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |> 
  select(VESSEL_ID) |> 
  distinct() |> 
  dim()
# [1] 741588     23
# [1] 3412    1   select(VESSEL_ID) |> 

# View(t__tne__dates_w_cnt_t_tne_short)

t__tne__dates_w_cnt_t_tne_short |>
  filter(is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |> 
  select(VESSEL_ID) |> 
  distinct()
# 1 NA

# 1834 + 515 + 3412
# [1] 5761? it's more than total vessels
# 1834 + 3412 = 5246

## combine dates with v_p, t, tne ----
# to see the weeks with no reports

# print_df_names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)

# vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_short <-
#   vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |>
#   select(contains("VESSEL"), weeks_perm_2022_amnt) |>
#   distinct()

# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
# [1] 3615   22
# [1] 3616   24

# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_short)
# [1] 3464    5
# [1] 3452    5

# print_df_names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)

v_p_t_tne_dates_by = join_by(YEAR,
            MONTH_OF_YEAR,
            WEEK_OF_YEAR,
            COMPLETE_DATE,
            PERMIT_VESSEL_ID == VESSEL_ID)

# df %>% distinct(var1, var2, .keep_all = TRUE)
v_p_ids <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |>
  distinct(
    PERMIT_VESSEL_ID,
    SERO_OFFICIAL_NUMBER.sa,
    SUPPLIER_VESSEL_ID.sa,
    VESSEL_ALT_NUM.sa,
    VESSEL_VESSEL_ID,
    .keep_all = TRUE
  ) 

dim(v_p_ids)
# [1] 3181   24
# dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
# [1] 3616   24
# View(v_p_ids)

v_p_ids_only <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |>
  distinct(
    PERMIT_VESSEL_ID,
    SERO_OFFICIAL_NUMBER.sa,
    SUPPLIER_VESSEL_ID.sa,
    VESSEL_ALT_NUM.sa,
    VESSEL_VESSEL_ID
  ) 
dim(v_p_ids_only)
# [1] 3181    5

v_p_ids_only |> 
  filter(!SERO_OFFICIAL_NUMBER.sa == SUPPLIER_VESSEL_ID.sa) |> 
  glimpse()
# 1
# $ PERMIT_VESSEL_ID        <chr> "NC0676EK"
# $ SERO_OFFICIAL_NUMBER.sa <chr> "NC0676EK"
# $ SUPPLIER_VESSEL_ID.sa   <chr> "1292480"
# $ VESSEL_ALT_NUM.sa       <chr> "NC0676EK"
# $ VESSEL_VESSEL_ID        <dbl> 383419

# v_p_ids <-
#   vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |>
#   mutate(distinct_id = 
#            select(    PERMIT_VESSEL_ID,
#     SERO_OFFICIAL_NUMBER.sa,
#     SUPPLIER_VESSEL_ID.sa,
#     VESSEL_ALT_NUM.sa,
#     VESSEL_VESSEL_ID
# ) |> 
#   distinct(
#   ) |> as.list()
#   )  |> str()

# t__tne__dates_w_cnt_t_tne_short ids
t__tne__dates_w_cnt_t_tne_short_v_ids <-
  distinct(t__tne__dates_w_cnt_t_tne_short, VESSEL_ID)
dim(t__tne__dates_w_cnt_t_tne_short_v_ids)
# 3880

#### check id intersections ----
intersect(v_p_ids_only$VESSEL_VESSEL_ID,
          t__tne__dates_w_cnt_t_tne_short_v_ids$VESSEL_ID) |>
  length()
# [1] 1457

t__tne__dates_w_cnt_t_tne_short_v_ids_v_ids <-
  as.character(t__tne__dates_w_cnt_t_tne_short_v_ids$VESSEL_ID)

# str(t__tne__dates_w_cnt_t_tne_short_v_ids_v_ids)

ii <- function(id_name) {
  # browser()
  intersect(v_p_ids_only[[id_name]],
            t__tne__dates_w_cnt_t_tne_short_v_ids_v_ids)
}

# ii("PERMIT_VESSEL_ID")

all_id_inters <-
  names(v_p_ids_only) |>
  map(ii)

names(all_id_inters) <- names(v_p_ids_only)
# glimpse(all_id_inters)
map_df(all_id_inters, length) |> 
  glimpse()
# $ PERMIT_VESSEL_ID        <int> 1
# $ SERO_OFFICIAL_NUMBER.sa <int> 1
# $ SUPPLIER_VESSEL_ID.sa   <int> 1
# $ VESSEL_ALT_NUM.sa       <int> 1
# $ VESSEL_VESSEL_ID        <int> 1457

#### which ids are in trips, but not in v_p? ----
ids_in_t_tne <-
  setdiff(t__tne__dates_w_cnt_t_tne_short_v_ids_v_ids,
          v_p_ids_only$VESSEL_VESSEL_ID)

length(ids_in_t_tne)
# 2423

str(ids_in_t_tne)
# [1] "328420" "326154" "327521" "327779" "328640" "250451"

print_df_names(vessels_permits_2022)
vessels_permits_2022 |> 
  filter(QCSJ_C000000000300001 %in% ids_in_t_tne) |> 
  distinct() |> 
  dim()
# [1] 4015   51

# glimpse(vessels_permits_2022_r)
vessels_permits_2022_r |> 
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |> 
# 4015
  filter(permit_sa_gom == 'sa_only') |> 
# [1] 1524   52
  distinct() |> 
  dim()

vessels_permits_2022_r_short |>
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
# 1838   
  filter(permit_sa_gom == 'sa_only') |> 
  distinct() |>
  dim()
# [1] 564  11

vessels_permits_2022_r_end_date |> 
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
# 1838   
  filter(permit_sa_gom == 'sa_only') |> 
# [1] 564  11
  distinct() |>
  dim()

vessels_permits_2022_r_end_date_w_y |>
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
# 1838   
  filter(permit_sa_gom == 'sa_only') |>
# 564
  distinct() |>
  dim()

# vessels_permits_2022_r_end_date_l |> 
#   map(~    filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
#   distinct() |>
#   dim()
# )

View(vessels_permits_2022_r_end_date_l_overlap_join)
vessels_permits_2022_r_end_date_l_overlap_join |>
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
# 1574   
  filter(permit_sa_gom.sa == 'sa_only') |>
  # [1] 624  34
  distinct() |>
  dim()

vessels_permits_2022_r_end_date_l_overlap_join_w_dual |>
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
# 1574   
  # filter(permit_sa_gom.sa == 'sa_only') |>
# 624
  filter(permit_sa_gom == 'sa_only') |>
  # 257  
  distinct() |>
  dim()

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 |>
  filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
# 1574   
  # filter(permit_sa_gom.sa == 'sa_only') |>
# 624
  filter(permit_sa_gom == 'sa_only') |>
  # 257  
  distinct() |>
  dim()

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list |>
  map(function(curr_df) {
    curr_df |>
      filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
      distinct()
  }) |>
  map_df(dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   367      950     257


dd <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates |>
  # 536
  map(function(curr_df) {
    curr_df |>
      filter(VESSEL_VESSEL_ID %in% ids_in_t_tne) |>
      distinct()
  })

# map_df(dd, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   203      570       0

## join v_p and t_tne ----
v_p_t_tne_dates <-
  full_join(
    vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22,
    t__tne__dates_w_cnt_t_tne_short,
    v_p_t_tne_dates_by,
    relationship = "many-to-many",
    suffix = c(".v_p", ".t_tne")
  )

dim(v_p_t_tne_dates)
# [1] 825558     42

## Count weeks ----
### permit, but no t/tne ----
v_p_t_tne_dates |> 
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
