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
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
con <- connect_to_secpr()

# get data ----
## permit ----
file_name_permits <- r"(my_outputs\from_db\mv_sero_fh_permits_his.csv)"

mv_sero_fh_permits_his <-
  read_csv(file_name_permits)
# Rows: 181032 Columns: 22
# ── Column specification ─────────────────────────────────────
# Delimiter: ","
# chr (14): VESSEL_ID, EXPIRATION_DATE, TOP, PERMIT, EFFECT...

# View(mv_sero_fh_permits_his)

### the same from db ----

mv_sero_fh_permits_his_query <-
  "select * from
srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
"

permit_info <- dbGetQuery(con,
                          mv_sero_fh_permits_his_query)

# View(permit_info)

### the same from another db table (to compare) ----

udp_v_sero_oth_prm_period_his_query <-
  "select * from
  udp.v_sero_oth_prm_period_his@secpr_dblk
"

permit_info_udp <- dbGetQuery(con,
                          udp_v_sero_oth_prm_period_his_query)


### permit + vessel ----
permit_vessel_query_exp21_query <-
"select * from srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
join safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
ON (v.sero_official_number = p.vessel_id)
where expiration_date > TO_DATE('01-JAN-21')
"
# and top in ('CDW', 'CHS', 'SC')

permit_vessel_query_exp21 <- dbGetQuery(con,
                          permit_vessel_query_exp21_query)

View(permit_vessel_query_exp21)

# add sa gom field

names(permit_vessel_query_exp21) <-
  make.unique(names(permit_vessel_query_exp21), sep = "_")

# print_df_names(permit_vessel_query_exp21)

permit_vessel_query_exp21 %>%
  filter(!(VESSEL_ID == SERO_OFFICIAL_NUMBER)) %>%
  dim()
# 0

# permit_vessel_query_exp21 %>%
#   filter(VESSEL_ID == "910032") %>%
#   View()

## separate_permits_into_3_groups ----
#repeat for permit only

# print_df_names(permit_vessel_query_exp21_reg)

permit_info_r <-
  permit_info  %>%
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

# dim(permit_info_r)
# 'data.frame':	181188 obs. of  23 variables:
# [1] 181207     23

permit_info_r %>%
  select(VESSEL_ID) %>%
  unique() %>%
  str()
# 13929
# 13930

# check differently
# https://stackoverflow.com/questions/63402652/comparing-dates-in-different-columns-to-isolate-certain-within-group-entries-in

## split by permit ----
permit_info_r_l <-
  permit_info_r %>%
  split(as.factor(permit_info_r$permit_sa_gom))

## add my_end_date ----

# View(permit_info_r_l)
permit_info_r_l_short <-
  permit_info_r_l %>%
  map(
    ~ .x %>%
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
      mutate(
        my_end_date =
          case_when((END_DATE < EFFECTIVE_DATE) &
                      (EXPIRATION_DATE > EFFECTIVE_DATE)
                    ~ EXPIRATION_DATE,
                    .default =
                      dplyr::coalesce(END_DATE,                                     EXPIRATION_DATE)
          )
      ) %>%
      select(-c(END_DATE,
                EXPIRATION_DATE)) %>%
      unique()
  )

# From Help:
# It is common to have right-open ranges with bounds like `[)`, which would
# mean an end value of `415` would no longer overlap a start value of `415`.
# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
# by <- join_by(VESSEL_ID,
#               overlaps(x$EFFECTIVE_DATE,
#                        x$my_end_date,
#                        y$EFFECTIVE_DATE,
#                        y$my_end_date,
#                        bounds = "[)"))
# 
# overlap_join1 <-
#   full_join(
#   permit_info_r_l_short$gom_only,
#   permit_info_r_l_short$sa_only,
#   by,
#   suffix = c(".gom", ".sa")
# )
# 
# dim(overlap_join1)
# [1] 84570     5

# View(overlap_join1)

# to get dual in the overlapping period:
# filter(!is.na(permit_sa_gom.sa))

# overlap_join1 %>%
  # filter(VESSEL_ID == '669631') %>%
  # mutate(
  #   eff_int_gom =
  #     lubridate::interval(EFFECTIVE_DATE.gom,
  #                         my_end_date.gom),
  #   eff_int_sa =
  #     lubridate::interval(EFFECTIVE_DATE.sa,
  #                         my_end_date.sa)
  # ) %>%
  # View()

# overlap_join1 %>%
#   filter(!is.na(EFFECTIVE_DATE.sa) |
#            !is.na(EFFECTIVE_DATE.gom)
#          ) %>%
#   select(VESSEL_ID) %>%
#   unique() %>%
#   dim()
# [1] 13929     1
 # dual permits with dates overlapping between SA and GOM

# add intervals ----
glimpse(permit_info_r_l_short$gom_only)
# [1] 48441     8
# dim(permit_info_r_l_short$sa_only)
# [1] 132600    8

permit_info_r_l_short_int <-
  permit_info_r_l_short %>%
  map( ~ .x %>%
         mutate(eff_int =
                  lubridate::interval(EFFECTIVE_DATE,
                                      my_end_date)))

# View(permit_info_r_l_short_int)

## get all permit info for 2022 ----

permit_info_r_l_short_22 <-
  permit_info_r_l_short_int %>%
  map( ~ .x %>%
         filter(year(EFFECTIVE_DATE) == '2022'))

# dim(permit_info_r_l_short_22$gom_only)
# [1] 1342    3
# 2595    9
# dim(permit_info_r_l_short_22$sa_only)
# [1] 3649    3
# [1] 8148    9

# TODO: split after all map .x

#
# overlapped_gom_sa_int <-
#   overlapped_gom_sa %>%
#     mutate(
#     eff_int_gom =
#       lubridate::interval(EFFECTIVE_DATE.gom,
#                           my_end_date.gom),
#     eff_int_sa =
#       lubridate::interval(EFFECTIVE_DATE.sa,
#                           my_end_date.sa)
#   ) %>%
#   mutate(int_overlapped = int_overlaps(eff_int_gom, eff_int_sa) )

# get overlapping periods
# https://stackoverflow.com/questions/37486572/date-roll-up-in-r/37487673#37487673
  # mutate(gr = cumsum(FromDate-lag(ToDate, default=1) != 1)) %>%
# ---
# https://stackoverflow.com/questions/76076208/calculating-number-of-overlapping-days-between-two-date-ranges
# dat <-
#   data.frame(enr_dte = sample(seq(
#     as.Date('2022-01-01'),
#     as.Date('2023-06-30'),
#     by = "day"
#   ), 10))
#
# dat %>%
#   mutate(
#     # Create interval between enrollment and enrollment + 180 days:
#     enr_end_int = lubridate::interval( enr_dte, enr_dte + days(180) )
#     # Create winter interval:
#     , winter_int = lubridate::interval( as.Date('2022-10-01'), as.Date('2023-05-31') )
#     # Get the intersection between enrollment interval and winter interval:
#     , enr_winter_intersection = lubridate::intersect( enr_end_int, winter_int )
#     # Get the length of the intersection (int_length can only return length in seconds):
#     , enr_winter_intersection_length_sec = lubridate::int_length( enr_winter_intersection )
#     # Convert seconds to days:
#     , enr_winter_intersection_length_days = enr_winter_intersection_length_sec/60/60/24
#   ) %>%
#   View()

# lubridate::
# int_overlaps
# int_diff() returns the intervals that occur between the elements of a vector of date-times. int_diff() is similar to the POSIXt and Date methods of diff(), but returns an Interval object instead of a difftime object.
# interval(start = NULL, end = NULL, tzone = tz(start))
# int_length(int)
# period(months = 1, days = 15)
# [1] "1m 15d 0H 0M 0S"

# int_standardize(int)
# int_shift(int, duration(days = 11))

# print_df_names(overlap_join1)
# int_gom = EFFECTIVE_DATE.gom, my_end_date.gom
# in_sa = EFFECTIVE_DATE.sa, my_end_date.sa
# overlapps?
# int_o = int_overlaps(int_gom, in_sa)
# if overlaps get overlapping period dates:
# int_start(int_o) =
#   ymd(min(EFFECTIVE_DATE.gom, EFFECTIVE_DATE.sa))
# int_end(int_o) =
#   ymd(max(EFFECTIVE_DATE.gom, EFFECTIVE_DATE.sa))

# Permits: by day solutuion ----

# library(sqldf)
# sqldf("SELECT ID, Date, COUNT(*) as PurchaseCount
#        FROM df
#        GROUP BY Date, ID")

# permit_vessel_query_exp21_reg_0_list_by_perm_r$gom_only

## get all days in 2022 ----

# print_df_names(permit_info)

# dim(permit_info_r_l_short_22$gom_only)
# 'data.frame':	85586 obs. of  3 variables:
# 'data.frame':	85592 obs. of  5 variables:
# 'data.frame':	85319 obs. of  3 variables:

my_compl_function <- function(my_row) {
  # browser()
  my_row %>%
    tidyr::complete(
      EFFECTIVE_DATE =
        seq(EFFECTIVE_DATE, my_end_date, "1 day"),
      VESSEL_ID = VESSEL_ID,
      TOP = TOP,
      PERMIT = PERMIT,
      PERMIT_STATUS = PERMIT_STATUS,
      VESSEL_ALT_NUM = VESSEL_ALT_NUM,
      permit_sa_gom = permit_sa_gom,
      my_end_date = my_end_date,
      eff_int = eff_int
    ) %>%
    return()
}

# print_df_names(permit_info_r_l_short_22$gom_only)

tic("permit_info_r_l_short_22 by day")
permit_info_22_days <-
  permit_info_r_l_short_22 %>%
  map(~.x %>%
        group_by(VESSEL_ID) %>%
        purrr::pmap(
          # .l = ex1,
          .f = function(VESSEL_ID, TOP, PERMIT, EFFECTIVE_DATE, PERMIT_STATUS, VESSEL_ALT_NUM, permit_sa_gom, my_end_date, eff_int) {
            # browser()
            my_df <- data.frame(VESSEL_ID, TOP, PERMIT, EFFECTIVE_DATE, PERMIT_STATUS, VESSEL_ALT_NUM, permit_sa_gom, my_end_date, eff_int)
            if (EFFECTIVE_DATE > my_end_date) {
              print(paste(VESSEL_ID, 
                          EFFECTIVE_DATE, 
                          my_end_date))
              res = my_df
            } else {
              res <- my_compl_function(my_df)
            }
            return(res)
          }
        ) %>%
        list_rbind()
  )
toc()
# permit_info_r_l_short_22 by day: 196 sec elapsed

# permit_info_r_l_short_22 by day: 395.39 sec elapsed
# with all fields
# permit_info_22 by day: 33.33 sec elapsed
# permit_info_r_l_short_22 by day: 16.42 sec elapsed
# print_df_names(permit_info_22_days_rename$gom_only)
# permit_info_r_l_short_22 by day: 23.05 sec elapsed

# TODO: expired before eff
# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "1115726 2022-01-31 23:00:00 2022-01-30 23:00:00"

# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "FL0173JY 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "1115726 2022-01-31 23:00:00 2022-01-30 23:00:00"
# [1] "1115726 2022-01-31 23:00:00 2022-01-30 23:00:00"
# permit_info_r_l_short_22 by day: 38.83 sec elapsed

permit_info_22_days <-
  permit_info_22_days %>%
  map(~ .x %>%
        rename(is_effective_date = EFFECTIVE_DATE))

## get day only ----
# permit_info_22_days <-
#   permit_info_22_days_rename %>%
#   map(
#     ~ .x %>%
#       mutate(
#         is_effective_date =
#           lubridate::floor_date(is_effective_date,
#                                 unit = "day"),
#         my_end_date =
#           lubridate::floor_date(my_end_date,
#                                 unit = "day")
#       )
#   )

# View(permit_info_22_days$gom_only)

# dim(permit_info_22_days$gom_only)
# [1] 444989      3
# 860296      9
# dim(permit_info_22_days$sa_only)
# [1] 1313487       3
# 444989 + 1313487
# 1758476
# permit_info_22_days
# [1] 3497829       3
 # 3497829 /1758476
# [1] 1.989125

# whole year df ----
days_22 <-
  seq(ISOdate(2022,1,1), ISOdate(2023,1,1), "days") %>%
  as.data.frame()
# str(days_22)
# 'data.frame':	366 obs. of  1 variable:
#  $ .: POSIXct, format: "2022-01-01 12:00:00" ...

names(days_22) <- "day_in_2022"
# print_df_names(permit_info_22_days$gom_only)

days_22$day_in_2022 <-
  floor_date(days_22$day_in_2022, unit = "day")

# View(days_22)

# get vessels which are in both ----

permit_info_22_days_vessels_only <-
  permit_info_22_days %>%
  map(
    ~ .x %>%
        select(VESSEL_ID) %>% 
        unique()
  )

vessels_in_both <-
  inner_join(
    permit_info_22_days_vessels_only$gom_only,
    permit_info_22_days_vessels_only$sa_only
  ) %>% 
  unique()

# dim(vessels_in_both)
# 277   

## use only vesel_ids in both ----
tic("permit_info_22_days_vsls_in_both")
permit_info_22_days_vsls_in_both <-
  permit_info_22_days %>%
  map(~ .x %>%
        filter(VESSEL_ID %in% vessels_in_both$VESSEL_ID) %>%
        unique())
toc()
# permit_info_22_days_vsls_in_both: 221.2 sec elapsed

# permit_info_22_days_vsls_in_both$gom_only %>% 
#   select(is_effective_date) %>% 
#   unique() %>% 
#   dim()
# [1] 849   1

# join with days_22 ----
## convert to days only, no hours ----
permit_info_22_days_vsls_in_both_d <-
  permit_info_22_days_vsls_in_both %>%
  map(~ .x %>%
        mutate(is_effective_date =
                 floor_date(is_effective_date,
                            unit = "day")))

# permit_info_22_days_vsls_in_both$gom_only$is_effective_date <-
#   permit_info_22_days_vsls_in_both$gom_only$is_effective_date %>%
#   floor_date(unit = "day")

# str(permit_info_22_days_vsls_in_both$gom_only)
 # $ is_effective_date: POSIXct[1:100862], format: "2022-05-16" ...

# str(days_22)

# join each with days_22 ----

tic("permit_info_22_days_vsls_in_both_d full_join")
permit_info_22_days_vsls_in_both_d_j <-
  permit_info_22_days_vsls_in_both_d %>%
  map(~ .x %>%
        full_join(days_22,
                  join_by(is_effective_date == day_in_2022)) %>%
        unique())
toc()
# permit_info_22_days_vsls_in_both_d full_join: 210.74 sec elapsed

# str(permit_info_22_days_vsls_in_both_d_j)
# tibble [101,228 × 3] (S3: tbl_df/tbl/data.frame)
# $ gom_only: tibble [192,768 × 9] (S3: tbl_df/tbl/data.frame)

# TODO: get only few field to join on, add other info later?


# join gom and sa with all days and vessels ----
# print_df_names(permit_info_22_days_vsls_in_both_d$gom_only)
# is_effective_date, VESSEL_ID, TOP, PERMIT, PERMIT_STATUS, VESSEL_ALT_NUM, permit_sa_gom, my_end_date, eff_int

tic("days_22_permits_g_s full join")
days_22_permits_g_s <-
  full_join(
    permit_info_22_days_vsls_in_both_d$gom_only,
    permit_info_22_days_vsls_in_both_d$sa_only,
    join_by(is_effective_date, 
            VESSEL_ID,
            VESSEL_ALT_NUM
            ),
    suffix = c(".gom", ".sa"),
    relationship = "many-to-many"
  )
toc()
# days_22_permits_g_s full join: 1.16 sec elapsed

View(days_22_permits_g_s)

## get overlapping intervals ----
tic("days_22_permits_g_s_overl")
days_22_permits_g_s_overl <-
  days_22_permits_g_s %>%
  filter(int_overlaps(eff_int.gom, eff_int.sa)) %>% 
  unique()
toc()
# View(days_22_permits_g_s_overl)

days_22_permits_g_s %>% 
  # group_by(VESSEL_ID) %>% 
  select(is_effective_date, VESSEL_ID) %>% 
  count(is_effective_date)
tic("days_22_permits_g_s_cnts u")
days_22_permits_g_s_cnts <-
  days_22_permits_g_s %>%
  mutate(is_effective_date =
           lubridate::floor_date(is_effective_date,
                                 unit = "day")) %>%
  select(is_effective_date, VESSEL_ID) %>%
  unique() %>%
  add_count(is_effective_date)
toc()
# days_22_permits_g_s_cnts u: 3.34 sec elapsed
# dim(days_22_permits_g_s_cnts)
# [1] 106736      3

View(days_22_permits_g_s_cnts)

# %>%
  # filter(is_effective_date == '2022-01-01 00:00:00')
  # C%>%
  # View()

# days_22_permits_g_s %>%
#   filter(VESSEL_ID == '558306') %>% 
#   unique() %>% 
#   dim()
# [1] 698  15

# add dual ----
View(days_22_permits_g_s_overl)
