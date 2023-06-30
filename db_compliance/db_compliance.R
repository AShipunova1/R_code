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

# View(permit_vessel_query_exp21)

# add sa gom field

names(permit_vessel_query_exp21) <-
  make.unique(names(permit_vessel_query_exp21), sep = "_")

print_df_names(permit_vessel_query_exp21)
grep(".1", names(permit_vessel_query_exp21), value = T)
# [1] "VESSEL_ID.1"


permit_vessel_query_exp21 %>%
  filter(!(VESSEL_ID.1 == VESSEL_ID)) %>%
  dim()
# 37424: all

permit_vessel_query_exp21 %>%
  filter(!(VESSEL_ID == SERO_OFFICIAL_NUMBER)) %>%
  dim()
# 0

# permit_vessel_query_exp21 %>%
#   filter(VESSEL_ID == "910032") %>%
#   View()

permit_vessel_query_exp21_1 <-
  permit_vessel_query_exp21 %>%
  dplyr::rename(VESSEL_ID_permit = VESSEL_ID,
                VESSEL_ID_vessel = VESSEL_ID.1)

## separate_permits_into_3_groups ----
#repeat for permit only

# print_df_names(permit_vessel_query_exp21_reg)

permit_info_r <-
  permit_info  %>%
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

# str(permit_info_r)
# 'data.frame':	181188 obs. of  23 variables:

permit_info_r %>% 
  select(VESSEL_ID) %>%
  unique() %>%
  str()
# 13929

# check differently
# https://stackoverflow.com/questions/63402652/comparing-dates-in-different-columns-to-isolate-certain-within-group-entries-in

## get all permit info for 2022 ---- 

permit_info_r_1 <-
  permit_info_r_l %>%
  map(~.x %>% 
  select(VESSEL_ID,
         EFFECTIVE_DATE,
         END_DATE,
         EXPIRATION_DATE) %>%
  mutate(my_end_date =
           case_when((END_DATE < EFFECTIVE_DATE) &
                       (EXPIRATION_DATE > EFFECTIVE_DATE)
                     ~ EXPIRATION_DATE,
                     .default =
                       dplyr::coalesce(END_DATE,                                     EXPIRATION_DATE)
           )) %>%
  select(-c(END_DATE,
            EXPIRATION_DATE)) %>%
  unique())

View(permit_info_r_1)

permit_info_r_l <-
  permit_info_r %>%
  split(as.factor(permit_info_r$permit_sa_gom))

glimpse(permit_info_r_l)

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

overlap_join1 <-
  full_join(
  permit_info_r_l$gom_only,
  permit_info_r_l$sa_only,
  by,
  suffix = c(".gom", ".sa")
)

dim(overlap_join1)
# 16639

View(overlap_join1)

# to get dual in the overlapping period:
# filter(!is.na(permit_sa_gom.sa)) 

overlap_join1 %>% 
  filter(!is.na(permit_sa_gom.sa)) %>% 
  # View()
# 12,868 
  # select(SERO_OFFICIAL_NUMBER) %>% 
  # unique() %>% 
  # dim()
# 4982
  filter(SERO_OFFICIAL_NUMBER == '669631')

# an overlap example
#   SERO_OFFICIAL_NUMBER permit_sa_gom.x EFFECTIVE_DATE.x
# 1               669631        gom_only       2020-06-27
# 2               669631        gom_only       2021-06-04
#         my_end_date.x permit_sa_gom.sa EFFECTIVE_DATE.sa       my_end_date.sa
# 1 2021-05-31 00:00:00         sa_only       2021-05-14 2021-12-06 23:00:00
# 2 2021-12-05 23:00:00         sa_only       2021-05-14 2021-12-06 23:00:00

overlap_join1 %>%
  filter(!is.na(permit_sa_gom.sa)) %>%
  filter(SERO_OFFICIAL_NUMBER == '669631') %>%
  mutate(
    eff_int_gom =
      lubridate::interval(EFFECTIVE_DATE.gom,
                          my_end_date.gom),
    eff_int_sa =
      lubridate::interval(EFFECTIVE_DATE.sa,
                          my_end_date.sa)
  ) %>%
  View()

overlap_join1 %>% 
  filter(!is.na(permit_sa_gom.sa)) %>% 
  select(SERO_OFFICIAL_NUMBER) %>% 
  unique() %>% 
  dim()
# 4982 dual permits with dates overlapping between SA and GOM

overlapped_gom_sa <-
  overlap_join1 %>% 
  filter(!is.na(permit_sa_gom.sa)) %>% 
  unique()

dim(overlapped_gom_sa)
# 12868     

overlapped_gom_sa_int <-
  overlapped_gom_sa %>% 
    mutate(
    eff_int_gom =
      lubridate::interval(EFFECTIVE_DATE.gom,
                          my_end_date.gom),
    eff_int_sa =
      lubridate::interval(EFFECTIVE_DATE.sa,
                          my_end_date.sa)
  ) %>%
  mutate(int_overlapped = int_overlaps(eff_int_gom, eff_int_sa) )

print_df_names(overlapped_gom_sa_int)

overlapped_gom_sa_int_22 <-
  overlapped_gom_sa_int %>%
  filter(
    int_overlapped == TRUE &
      !(eff_int_gom == eff_int_sa) &
      year(EFFECTIVE_DATE.sa) == '2022'
  ) 
# %>%
  # mutate(eff_year_sa = year(EFFECTIVE_DATE.sa)) %>%
  # View()
# 359

df <- read.table(
  text = "
                 id          start           end
    1            2      2018-10-01    2018-12-01
    2            3      2018-01-01    2018-04-01
",
header = TRUE
)

do.call(rbind, 
        with(df, lapply(1:nrow(df), function(i)
  data.frame(
    id = id[i],
    date = seq(as.Date(start[i]), as.Date(end[i]), by = "month")
  )))) %>% 
  View()

lst1 <-
  Map(seq,
      MoreArgs = list(by = 'month'),
      as.Date(df$start),
      as.Date(df$end))

View(lst1)

data.frame(id = rep(df$id, lengths(lst1)),
           date = do.call(c, lst1))

    # select(SERO_OFFICIAL_NUMBER) %>% 
  # unique() %>% dim()
# 221   

# df1 %>% 
#   group_by(id) %>% 
#   mutate(new_var = case_when(value == max(value) ~ "higher",
#       TRUE ~ "lower")) %>% 
#   ungroup

# Warning in View :
#   Values from `eff_int_gom` are not uniquely identified;
# output will contain list-cols.
# • Use `values_fn = list` to suppress this warning.
# • Use `values_fn = {summary_fun}` to summarise duplicates.
# • Use the following dplyr code to identify duplicates.
#   {data} %>%
#   dplyr::group_by(SERO_OFFICIAL_NUMBER) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)

overlapped_gom_sa_int_22 %>%
  filter(SERO_OFFICIAL_NUMBER == '901070') %>%
  dplyr::group_by(SERO_OFFICIAL_NUMBER) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)


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
glimpse(permit_vessel_query_exp21_reg_0_list_by_perm_r$gom_only)

## get all days in 2022 ----
days_22 <- seq(ISOdate(2022,1,1), ISOdate(2023,1,1), "days")
# str(days)
# POSIXct[1:366],

print_df_names(permit_info)

## get all permit info for 2022 ---- 

permit_info_1 <-
  permit_info %>%
  select(VESSEL_ID,
         EFFECTIVE_DATE,
         END_DATE,
         EXPIRATION_DATE) %>%
  mutate(my_end_date =
           case_when((END_DATE < EFFECTIVE_DATE) &
                       (EXPIRATION_DATE > EFFECTIVE_DATE)
                     ~ EXPIRATION_DATE,
                     .default =
                       dplyr::coalesce(END_DATE,                                     EXPIRATION_DATE)
           )) %>%
  select(-c(END_DATE,
            EXPIRATION_DATE)) %>%
  unique()

str(permit_info_1)
# 'data.frame':	85586 obs. of  3 variables:
# 'data.frame':	85592 obs. of  5 variables:
# 'data.frame':	85319 obs. of  3 variables:

year_int <-
  lubridate::interval(ISOdate(2022, 1, 1),
                      ISOdate(2023, 1, 1))
permit_info_22 <-
  permit_info_1 %>%
  mutate(permit_eff =
           lubridate::interval(EFFECTIVE_DATE,
                               my_end_date)) %>%
  filter(lubridate::int_overlaps(permit_eff, year_int))

dim(permit_info_22)
# 9074
# 9072
# 9070

my_compl_function <- function(my_row) {
  # browser()
  my_row %>%
    tidyr::complete(
      EFFECTIVE_DATE =
        seq(EFFECTIVE_DATE, my_end_date, "1 day"),
      VESSEL_ID = VESSEL_ID,
      my_end_date = my_end_date
    ) %>%
    return()
}


tic("permit_info_22 by day")
permit_info_22_days <-
  permit_info_22 %>%
  group_by(VESSEL_ID) %>%
  purrr::pmap(
    # .l = ex1,
    .f = function(VESSEL_ID,
                  EFFECTIVE_DATE,
                  my_end_date,
                  ...) {
      # browser()
      my_df <- data.frame(VESSEL_ID, EFFECTIVE_DATE, my_end_date)
      if (EFFECTIVE_DATE > my_end_date) {
        res = my_df
      } else {
        res <- my_compl_function(my_df)
      }
      return(res)
    }
  ) %>%
  list_rbind()
toc()
# permit_info_22 by day: 33.33 sec elapsed

dim(permit_info_22_days)
# [1] 3497829       3
