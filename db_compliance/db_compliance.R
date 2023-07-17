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

# Join dates_2022 and everything  ----
# View(dates_2022)
## p_v ----
map_df(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list, dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1  3281     3673   13749
# 2   116      116     116
# 1   635     1879    6308
# 2    33       33      33

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

# data_overview(dates_2022)
# COMPLETE_DATE 427

data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list$dual)
# PERMIT_VESSEL_ID            353
# VESSEL_VESSEL_ID            353
# EFFECTIVE_DATE.gom          232
# EFFECTIVE_DATE.sa           230

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
            trips_info_2022_int_ah_w_y,
            t_by)

### check numbers ----
data_overview(trips_info_2022_int_ah_w_y_dates)
# COMPLETE_DATE                 427
# VESSEL_ID                    1935
# data_overview(trips_info_2022_int_ah_w_y)
# VESSEL_ID                    1934

trips_info_2022_int_ah_w_y_dates_ids <- trips_info_2022_int_ah_w_y_dates |> 
  select(VESSEL_ID) |> 
  distinct()

trips_info_2022_int_ah_w_y_ids <-
trips_info_2022_int_ah_w_y |> 
  select(VESSEL_ID) |> 
  distinct()

# setdiff(trips_info_2022_int_ah_w_y_ids, trips_info_2022_int_ah_w_y_dates_ids)
# 0
## tne ----
print_df_names(trip_neg_2022_w_y)
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
# TODO: fix end in 2023 dissappeared

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates$sa_only |>
  # remove gom, keep sa only
  select(-ends_with("gom")) |> View()
  mutate(permit_2022 =
           lubridate::intersect(eff_int_sa,
                                interval_2022)) |>
  mutate(weeks_perm_2022_amnt =
           (permit_2022 / lubridate::dweeks(1)) |>
           round()) |>
  distinct()

View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
min(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22$weeks_perm_2022_amnt, na.rm = T)
max(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22$weeks_perm_2022_amnt, na.rm = T)
# 0-52
# 0-109 (without interval_2022)
# 0-52

data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53
# VESSEL_ID                3888
# today() [1] "2023-07-17"
# VESSEL_VESSEL_ID           3182
# weeks_perm_2022_amnt         54

## compare vessel_ids ----

### v_p ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids <-
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |> 
  select(contains("vessel")) |> 
  distinct()

dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids)
# [1] 3182    4
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
setdiff(
  trips_info_2022_int_ah_w_y_dates_ids$VESSEL_ID,
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID
) |> glimpse()
  # length()
  # 774

trips_info_2022_int_ah_w_y_dates_ids |> 
  filter(VESSEL_ID == '327558') |> 
  glimpse()

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
# 1751

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

t__tne__dates_w_cnt_t_tne |> 
  filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |>
  dim()
# [1] 5971   99

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

# dim(t__tne__dates)
# [1] 838720     95
# dim(t__tne__dates_w_cnt_t_tne)
# [1] 838720     99
# dim(t__tne__dates_w_cnt_t_tne_short)
# [1] 838720     23

View(t__tne__dates_w_cnt_t_tne_short)

## Compare week counts t/tne ----

# both
t__tne__dates_w_cnt_t_tne_short |>
  filter(!is.na(TRIP_ID.t) & !is.na(TRIP_ID.tne)) |> 
  select(VESSEL_ID) |>
  distinct() |> 
  dim()
# [1] 5971   23
# [1] 515   1 select(VESSEL_ID) |>

# t only
t__tne__dates_w_cnt_t_tne_short |>
  filter(!is.na(TRIP_ID.t) & is.na(TRIP_ID.tne)) |>
  select(VESSEL_ID) |>
  distinct() |>
  dim()
# [1] 91099    23
# [1] 1834    1 select(VESSEL_ID) |>

# tne only
View(t__tne__dates_w_cnt_t_tne_short)
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
# [1] 5761


## combine v_p, t, tne ----

print_df_names(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_short <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22 |>
  select(contains("VESSEL"), weeks_perm_2022_amnt) |>
  distinct()

dim(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
# [1] 3615   22

View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_short)
# [1] 3464    5


v_p_t_dates <-
  left_join(
    vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22,
    trips_info_2022_int_ah_w_y_dates,
    join_by(VESSEL_VESSEL_ID == VESSEL_ID)
  )

View(trips_info_2022_int_ah_w_y_dates)
