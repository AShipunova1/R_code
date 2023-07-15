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

# Permits ----
## separate_permits_into_3_groups ----

permit_info_r <-
  permit_info  %>%
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

# dim(permit_info_r)
# 'data.frame':	181188 obs. of  23 variables:
# [1] 181207     23

permit_info_r %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  str()
# 13929
# 13949 

# check differently
# https://stackoverflow.com/questions/63402652/comparing-dates-in-different-columns-to-isolate-certain-within-group-entries-in

## add my_end_date ----
permit_info_r_short <-
  permit_info_r %>%
  select(
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

## split by permit ----
permit_info_r_l <-
  permit_info_r_short %>%
  split(as.factor(permit_info_r_short$permit_sa_gom))

## join by overlap of gom and sa (= dual) ----
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
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# [1] 13949 

permit_info_r %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# 13930
# 13942

### add "dual" to intervals ----
permit_info_r_l_overlap_join1_w_dual <-
  permit_info_r_l_overlap_join1 %>%
  mutate(permit_sa_gom =
           case_when(
             !is.na(permit_sa_gom.sa) &
               !is.na(permit_sa_gom.gom) ~ "dual",
             .default =
               dplyr::coalesce(permit_sa_gom.sa,
                               permit_sa_gom.gom)

           ))

# 186,210
permit_info_r_l_overlap_join1_w_dual %>%
  select(permit_sa_gom) %>%
  distinct()
# all 3

# to get dual in the overlapping period:
# filter(!is.na(permit_sa_gom.sa))

interval_2022 = lubridate::interval(as.Date('2022-01-01'),
                                    as.Date('2022-12-31'))

permit_info_r_l_overlap_join1_w_dual_22 <-
  permit_info_r_l_overlap_join1_w_dual %>%
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
permit_info_r_l_overlap_join1_w_dual_22 %>%
  select(permit_sa_gom) %>%
  distinct()
# all 3

permit_info_r_l_overlap_join1_w_dual_22 %>%
  filter(permit_sa_gom == "dual") %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# 379

# permit_info_r_l_overlap_join1_w_dual_22 %>%
#   select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
#   filter(!(VESSEL_ID == VESSEL_ALT_NUM.sa)) %>%
  # dim()
# 660

## split permits by region again ----
permit_info_r_l_overlap_join1_w_dual_22__list <-
  permit_info_r_l_overlap_join1_w_dual_22 %>%
  split(as.factor(permit_info_r_l_overlap_join1_w_dual_22$permit_sa_gom))

# add 2022 period weeks cnt to permit ----

permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22 <-
  permit_info_r_l_overlap_join1_w_dual_22__list$sa_only |>
  # remove gom, keep sa only
  select(-ends_with("gom")) |>
  mutate(permit_2022 = lubridate::intersect(eff_int_sa,
                      interval_2022)) |>
  mutate(weeks_perm_2022_amnt =
           (permit_2022 / lubridate::dweeks(1)) |>
           round()
         ) |>
  distinct()

min(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22$weeks_perm_2022_amnt)
max(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22$weeks_perm_2022_amnt)
# 0-52  

data_overview(permit_info_r_l_overlap_join1_w_dual_22__list__sa_w_p22)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53


# end here permits ----

# vessels_permits_2022 ----

## fist weird header
vessels_permits_2022 %<>%
  rename("VESSEL_ID" = "QCSJ_C000000000300000")

## region permit groups 
vessels_permits_2022_r <-
  vessels_permits_2022  %>%
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

## add my_end_date ----
vessels_permits_2022_r_end_date <-
  vessels_permits_2022_r %>%
  # select(
  #   VESSEL_ID,
  #   EXPIRATION_DATE,
  #   TOP,
  #   PERMIT,
  #   EFFECTIVE_DATE,
  #   END_DATE,
  #   PERMIT_STATUS,
  #   VESSEL_ALT_NUM,
  #   permit_sa_gom
  # ) %>%
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

dim(vessels_permits_2022_r_end_date)
# [1] 20231    51

## split by permit ----
vessels_permits_2022_r_end_date_l <-
  vessels_permits_2022_r_end_date %>%
  split(as.factor(vessels_permits_2022_l_end_date$permit_sa_gom))

## join by overlap of gom and sa (= dual) ----
# From Help:
# It is common to have right-open ranges with bounds like `[)`, which would
# mean an end value of `415` would no longer overlap a start value of `415`.
# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
# View(vessels_permits_2022)
# View(vessels_permits_2022_r_end_date_l$gom_only)
by <- join_by(VESSEL_ID,
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
# [1] 20918   101

vessels_permits_2022_r_end_date_l_overlap_join %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# [1] 5461    1

vessels_permits_2022 %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# [1] 5461    1

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
# [1] 20918   102

View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual)

# to get dual in the overlapping period:
# filter(!is.na(permit_sa_gom.sa))

# 2022 year interval ----
interval_2022 = lubridate::interval(as.Date('2022-01-01'),
                                    as.Date('2022-12-31'))

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

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  filter(permit_sa_gom == "dual") %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# 379
# 353

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  filter(!(VESSEL_ID == VESSEL_ALT_NUM.sa)) %>%
dim()
# 660
# 636

## split permits by region again ----
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 %>%
  split(as.factor(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22$permit_sa_gom))

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

data_overview(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list__sa_w_p22)
# VESSEL_ID            3875
# weeks_perm_2022_amnt   53
# VESSEL_ID                3888

# end here vessel_permits ----

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

# end of data fixing ----

# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance
