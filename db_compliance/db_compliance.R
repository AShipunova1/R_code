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
  select(VESSEL_ID) %>%
  distinct() %>%
  str()
# 13929
# 13930

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
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# [1] 13930     1

permit_info_r %>%
  select(VESSEL_ID) %>%
  distinct() %>%
  dim()
# 13930
# 13942     

# add "dual" to intervals ----
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

### check ----
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
# end here permits

permit_info_r_l_overlap_join1_w_dual_22 %>%
  select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  filter(!(VESSEL_ID == VESSEL_ALT_NUM.sa)) %>%
  dim()
# 652

permit_info_r_l_overlap_join1_w_dual_22 %>%
  select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
  filter(!(VESSEL_ID == VESSEL_ALT_NUM.gom)) %>%
  dim()
# 356

# permit_info_r_l_overlap_join1_w_dual_22 %>%
#   select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
#   filter(!(VESSEL_ALT_NUM.gom == VESSEL_ALT_NUM.sa)) %>%
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
      select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      filter(!(VESSEL_ALT_NUM.sa == VESSEL_ALT_NUM.gom))
  )
str(check_1)
# 0
# VESSEL_ALT_NUM.sa == VESSEL_ALT_NUM.gom:
# 'data.frame':	3668 obs. of  3 variables


check_2 <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map(
    ~ .x %>%
      select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      distinct() %>%
      filter(!(VESSEL_ID == VESSEL_ALT_NUM.gom))
  )

map_df(check_2, dim)
#    dual gom_only sa_only
# 1   143      213       0
# distinct()
# 1    14       70       0
# [1] 356   3

check_3 <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map(
    ~ .x %>%
      select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      distinct() %>%
      filter(!(VESSEL_ID == VESSEL_ALT_NUM.sa))
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
      select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      pivot_longer(
        cols = c(VESSEL_ID,
                 VESSEL_ALT_NUM.sa,
                 VESSEL_ALT_NUM.gom),
        values_to = "permit_vessel_id"
      ) %>%
      select(permit_vessel_id) %>%
      distinct() %>%
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
       ~ mutate(.x, permit_sa_gom = .y))

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
  distinct()

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
  select(permit_vessel_id) %>%
  distinct() %>%
  dim()
# [1] 5632    1

# setdiff(
#   permit_info_r_l_overlap_join1_w_dual_22__list_ids$sa_only$permit_vessel_id,
#   vessels_permit_vsl_id__all_l$sa_only$permit_vessel_id
# )
# [1] "1304296"  "NA"       "FL6437NY" "1176885"

# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>%
#     filter(VESSEL_ID == '1304296') %>% View()
# # alt_num.sa DL5161AM
#
# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>%
#     filter(VESSEL_ID == 'FL6437NY') %>% View()

# FL6437NY
# 2015-09-01 EDT--2022-06-02 EDT
# VESSEL SOLD

# permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>%
#     filter(VESSEL_ID == '1176885') %>% View()
# alt num FL0668PV 2021-05-01 EDT--2022-03-31 EDT

# vessels_permit_vsl_id__all_l$sa_only %>%
#   filter(vessel_id == '1304296' |
#            VESSEL_ID == '1304296' |
#            COAST_GUARD_NBR == '1304296' |
#            STATE_REG_NBR == '1304296'
#            ) %>%
#   View()
# # 0

# vessels_permit_vsl_id__all_l$sa_only %>%
#   filter(permit_vessel_id == 'DL5161AM') %>%
#   View()
# 1
# SERO_OFFICIAL_NUMBER is NULL
# difference is in 1 vessel in sa_only

# clean up vessels_permit_vsl_id__all ----
# vessels_permit_vsl_id__all %>%
#   count(permit_vessel_id) %>%
#   filter(n > 1) %>%
# head()
# # 29
 # 1 1023478       2
 # 2 1064839       2
 # 3 1090694       2
 # 4 1243727       2
 # 5 1320038       2
 # 6 16250027      2

# vessels_permit_vsl_id__all %>%
#   filter(permit_vessel_id == '1023478') %>%
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
  filter(permit_vessel_id == '1023478') %>%
  group_by(permit_vessel_id) %>%
  dplyr::summarise_all(coalesce_by_column)

# View(vessels_permit_vsl_id__all_2)

vessels_permit_vsl_id__all_0 <-
  vessels_permit_vsl_id__all %>%
  filter(permit_vessel_id == '1023478')

all.equal(vessels_permit_vsl_id__all_2,
          vessels_permit_vsl_id__all_0[1,])
# [1] "Component “COAST_GUARD_NBR”: 'is.NA' value mismatch: 1 in current 0 in target"

all.equal(vessels_permit_vsl_id__all_2,
          vessels_permit_vsl_id__all_0[2,])
# [1] "Component “STATE_REG_NBR”: 'is.NA' value mismatch: 1 in current 0 in target"

## all coalesce ----
# vessels_by_permit_vessel__all_u <-
#   vessels_permit_vsl_id__all %>%
#   group_by(permit_vessel_id) %>%
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
    "vessels_by_permit_vessel__all_l_u.csv"
  )

# vessels_by_permit_vessel__all_l_u_col_types <-
#   cols(VESSEL_ID.v = "c")
# names(vessels_by_permit_vessel__all_l_u)

# View(vessels_permit_vsl_id__all_l)
vessels_by_permit_vessel__all_l_u <-
  read_csv_or_run(
    vessels_by_permit_vessel__all_l_u_file_path,
    vessels_permit_vsl_id__all_l,
    my_function_vessels_permit_vsl_id__all_l
  )

length(vessels_by_permit_vessel__all_l_u$dual$permit_vessel_id) +
  length(vessels_by_permit_vessel__all_l_u$gom_only$permit_vessel_id) +
  length(vessels_by_permit_vessel__all_l_u$sa_only$permit_vessel_id)
# 5684

all.equal(vessels_by_permit_vessel__all_l_u$dual$permit_vessel_id,
          vessels_by_permit_vessel__all_l_u1$permit_vessel_id)
# [1] "Lengths (392, 5632) differ (string compare on first 392)"
# length(unique(vessels_by_permit_vessel__all_l_u$dual$permit_vessel_id))
# 392
# length(vessels_by_permit_vessel__all_l_u$dual$permit_vessel_id)
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
    select(permit_vessel_id) %>%
    distinct() %>%
    dim()
)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   392     1272    4020
# 2     1        1       1
# View(uniq_permit_vsl_ids)

uniq_permit_vsl_ids %>%
  rowSums()
# 5684

# vessels_by_permit_vessel__all %>%
#   select(permit_vessel_id) %>%
#   distinct() %>%
#   dim()
# 5632

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

View(vessels_permit_1)

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

# View(vessels_permit_bind)

## uniq ----

# vessels_permit_bind_u_test_0 <-
#   vessels_permit_bind$gom_only %>%
#   filter(SERO_OFFICIAL_NUMBER == 'FL4203RB')
# 
# vessels_permit_bind_u_test <-
#   vessels_permit_bind$gom_only %>%
#   filter(SERO_OFFICIAL_NUMBER == 'FL4203RB') %>%
#   group_by(SERO_OFFICIAL_NUMBER) %>%
#   dplyr::summarise_all(coalesce_by_column)
# 
# vessels_permit_bind_u_test2 <-
#   vessels_permit_bind$gom_only %>%
#   filter(SERO_OFFICIAL_NUMBER == 'FL4203RB') %>%
#   group_by(VESSEL_ID.v) %>%
#   dplyr::summarise_all(coalesce_by_column)
# 
# View(vessels_permit_bind_u_test_0)
# View(vessels_permit_bind_u_test)

### vessels_permit_bind_ from csv ----

file_path_vessels_permit_bind_u.csv <- file.path(
  my_paths$inputs,
  current_project_name,
  r"(intermediate_dfs\vessels_permit_bind_u.csv)"
)
# grep("date",
#      names(vessels_permit_bind_u1),
#      ignore.case = T,
#      value = T)

# sn1 <- setNames(c(list(col_character()),
           # rep(list(col_integer(
           # )), 5)),
           # grep("date", names(vessels_permit_bind_u1), ignore.case = T,
                # value = T))
         # c("name", paste0("id_", 1:5)))

# View(sn1)


if (file.exists(file_path_vessels_permit_bind_u.csv)) {
  vessels_permit_bind_u_one_df <-
    readr::read_csv(file_path_vessels_permit_bind_u.csv,
                    col_types = cols(
                      VESSEL_ID.v = "c"
                    # col_types = cols(.default = "c", 
                    #                  EFFECTIVE_DATE.gom = "D",
                    #                  my_end_date.gom = "D",
                    #                  EFFECTIVE_DATE.sa = "D",
                    #                  my_end_date.sa = "D",
                                     )) %>% 
    # need distinct because the first line is written twices, see below
    distinct()
  # Rows: 5460 Columns: 48             
  
  vessels_permit_bind_u1 <-
    vessels_permit_bind_u_one_df %>% 
    split(as.factor(vessels_permit_bind_u_one_df$permit_sa_gom))
  
} else {
  tic("vessels_permit_bind_u1")
  vessels_permit_bind_u1 <-
    vessels_permit_bind %>%
    map( ~ .x %>%
           group_by(VESSEL_ID.v) %>%
           dplyr::summarise_all(coalesce_by_column))
  toc()
  # vessels_permit_bind_u1: 698.83 sec elapsed

  # write headers
  vessels_permit_bind_u1[[1]][1,] %>%
    write_csv(file_path_vessels_permit_bind_u.csv)
  
  # write all
  vessels_permit_bind_u1 %>%
    walk(~ .x %>%
           write_csv(file_path_vessels_permit_bind_u.csv,
                     append = TRUE))
}


# all.equal(vessels_permit_bind_u_sero$gom_only,
#           vessels_permit_bind_u1$gom_only)
#  [2] "Attributes: < Component “row.names”: Numeric: lengths (1202, 1204) differ >"
all.equal(vessels_permit_bind_u_sero$sa_only,
          vessels_permit_bind_u1$sa_only)
 # [2] "Attributes: < Component “row.names”: Numeric: lengths (3870, 3877) differ >"

all.equal(vessels_permit_bind_u_sero$dual,
          vessels_permit_bind_u1$dual)
 # [1] "Names: 28 string mismatches"
all.equal(vessels_permit_bind_u_sero,
          vessels_permit_bind_u1)
  # [4] "Component “dual”: Component “VESSEL_ID”: 378 string mismatches"

# fix trip data ----
## add trip_int ----
trips_info_2022 %>%
  select(TRIP_START_DATE, TRIP_START_TIME, TRIP_END_DATE, TRIP_END_TIME) %>%
  str()
# POSIXct
# str(trips_info_2022)

# is_effective_date =
#   lubridate::floor_date(is_effective_date,
#                         unit = "day"),

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
  View()

## trip types A and H trips ----
trips_info_2022_int_ah <-
  trips_info_2022_int %>%
  filter(TRIP_TYPE %in% c("A", "H"))

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

# trips_info_2022_int_ah %>%
#   select(TRIP_START_DATE) %>%
#   head()

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

# trips_info_2022_int_ah_w_y %>%
#   select(starts_with("TRIP")) %>%
#   arrange(TRIP_START_DATE) %>%
#   View()

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

# trip_notifications_2022_ah_w_y %>%
#   select(starts_with("TRIP")) %>%
#   arrange(TRIP_START_DATE) %>%
#   View()

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

trip_neg_2022_w_y %>%
  select(starts_with("TRIP")) %>%
  arrange(TRIP_DATE) %>%
  View()

# vessels and trip_notifications ----

## compare vessel_ids ----
trip_notifications_2022_ah_w_y_vsl_ids <-
  trip_notifications_2022_ah_w_y %>%
  select(VESSEL_ID) %>%
  distinct()

vessels_by_permit_vessel__all_l_u_vsl_ids <-
  vessels_by_permit_vessel__all_l_u %>%
  map_df( ~ .x %>%
            select(VESSEL_ID) %>%
            distinct())

vessels_by_permit_vessel__all_l_u_vsl_ids_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map( ~ .x %>%
            select(VESSEL_ID) %>%
            distinct())

dim(trip_notifications_2022_ah_w_y)
# [1] 126726     33
# [1] 67738    33

# dim(trip_notifications_2022_vsl_ids)
# [1] 914   1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids)
# [1] 5459      1
# vessels_by_permit_vessel__all_l_u_vsl_ids %>%
#   distinct() %>%
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
#     distinct(vessels_permit_vsl_id__all_l__sa_ids$VESSEL_ID)
#   ) %>%
#   distinct()
# glimpse(not_in_vessel_trip_sa)
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
      distinct() %>%
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

# vessels and trip negatives ----
# View(trip_neg_2022_w_y)

vessels__trip_neg_22_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map(
    ~ .x %>%
      distinct() %>%
      inner_join(
        trip_neg_2022_w_y,
        join_by(VESSEL_ID),
        relationship = "many-to-many",
        suffix = c(".v", ".tneg")
      )
  )

# vessels__trip_neg_22_l %>%
#   map_df(dim)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1 41272    56506  795944
# 2    41       41      41
# 1 17947    21175  390659

# vessels and trips ----

# View(trips_info_2022)

vessels__trips_22_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map(
    ~ .x %>%
      distinct() %>%
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

# print_df_names(vessels__trips_22_l$sa_only)

# GOM + dual 2022 compliance ----
# There should be a declaration for every logbook (in other words, the number of fishing intended charter declarations would need to be equal to logbooks to be compliant).
# There should be a logbook for every declaration of a charter or headboat intending to fish.

# SA 2022 compliance ----
# There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# DNFs should not be submitted more than 30 days in advance

dates_2022_short <-
  dates_2022 %>%
  select(-c(COMPLETE_DATE, MONTH_OF_YEAR))
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
  filter(!lubridate::floor_date(TRIP_DATE,
                                unit = "day") ==
           "2021-12-31") %>%
  select(permit_vessel_id,
         SUPPLIER_VESSEL_ID,
         TRIP_week_num,
         TRIP_DATE_y) %>%
  distinct()

  # select(contains("vessel"), starts_with("TRIP")) %>%
  # distinct()

# data_overview(vessels__trip_neg_22_l_sa_short)
# 390,659 10
# permit_vessel_id   1709
# [1] 66631     4


### remove neg trips on 2021-12-31 ----
# vessels__trip_neg_22_l_sa_short %>%
#   filter(lubridate::floor_date(TRIP_DATE,
#                                unit = "day") ==
#            "2021-12-31") %>% glimpse()
# Rows: 1,006

# dim(vessels__trip_neg_22_l_sa_short)
# [1] 66631     3

# View(vessels__trip_neg_22_l_sa_short)

# vessels__trip_neg_22_l_sa_short %>%
#   select(permit_vessel_id, SUPPLIER_VESSEL_ID, TRIP_week_num) %>%
#   distinct() %>%
#   add_count(permit_vessel_id, SUPPLIER_VESSEL_ID) %>%
#   View()

vessels__trip_neg_22_l_sa_short %>%
  filter(permit_vessel_id == '03017306') %>%
  select(permit_vessel_id, TRIP_week_num) %>%
  distinct() %>%
  group_by(permit_vessel_id) %>%
  summarise(n_distinct(TRIP_week_num)) %>%
  # count(TRIP_week_num) %>%
  glimpse()
#   $ permit_vessel_id            <chr> "03017306"
# $ `n_distinct(TRIP_week_num)` <int> 19

vessels__trip_neg_22_l_sa_short_weeks_per_vessel <-
  vessels__trip_neg_22_l_sa_short %>%
  group_by(permit_vessel_id, SUPPLIER_VESSEL_ID) %>%
  summarise(tot_weeks = n_distinct(TRIP_week_num))

# View(vessels__trip_neg_22_l_sa_short_weeks_per_vessel)
# 1709

## trip notifications per vessel per week
# data_overview(vessels__trip_notif_22_l$sa_only)
# permit_vessel_id              17

vessels__trip_notif_22_l_sa_short <-
  vessels__trip_notif_22_l$sa_only %>%
  filter(TRIP_START_y %in% c('2021', '2022')) %>%
  filter(TRIP_END_y %in% c('2022', '2023')) %>%
  select(permit_vessel_id,
         TRIP_START_week_num,
         TRIP_END_week_num,
         TRIP_START_y,
         TRIP_END_y) %>%
  distinct()

# str(vessels__trip_notif_22_l_sa_short)
vessels__trip_notif_22_l_sa_vessels_trips <-
  vessels__trip_notif_22_l_sa_short %>%
  group_by(permit_vessel_id) %>%
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

vessels__trips_22_l_sa_short <-
  vessels__trips_22_l$sa_only %>%
  filter(TRIP_START_y %in% c('2021', '2022')) %>%
  filter(TRIP_END_y %in% c('2022', '2023')) %>%
  select(permit_vessel_id,
         TRIP_START_week_num,
         TRIP_END_week_num,
         TRIP_START_y,
         TRIP_END_y) %>%
  distinct()

vessels__trips_22_l_sa_weeks_per_vessel <-
  vessels__trips_22_l_sa_short %>%
  group_by(permit_vessel_id) %>%
  summarise(
    tot_start_weeks =
      n_distinct(TRIP_START_week_num),
    tot_end_weeks =
      n_distinct(TRIP_END_week_num)
  )

dim(vessels__trips_22_l_sa_weeks_per_vessel)
# 1110

## combine weeks, vessels and trips (all) info ----
# by = join_by(permit_vessel_id, TRIP_START_week_num, TRIP_END_week_num)
by = join_by(permit_vessel_id, TRIP_START_week_num, TRIP_START_y)

vessels_trips_and_notif_by_week <-
  vessels__trips_22_l_sa_short %>%
  full_join(vessels__trip_notif_22_l_sa_short,
            by)

dim(vessels_trips_and_notif_by_week)
# [1] 19598     3
# with year
# [1] 19600     7

# same by notif weeks
# vessels_trips_and_notif_by_week %>%
#   filter(is.na(TRIP_END_week_num)) %>%
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
  distinct()
# [1] 23362     6
# [1] 19524     5 (no day and month)

# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 166 of `y` matches multiple rows in `x`.

dim(vessels__trips_22_l_sa_short_all_dates_t_start)
# [1] 19433     5
# [1] 19445     5 (with 2021 and 2023)

vessels__trips_22_l_sa_short_all_dates_t_start %>%
  filter(is.na(permit_vessel_id))
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
  distinct()

dim(vessels__trips_22_l_sa_short_all_dates_t_start_all_cols)
# [1] 19435     7

vessels__trips_22_l_sa_short_all_dates_t_start_all_cols %>%
  filter(is.na(permit_vessel_id))
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
  distinct()
dim(vessels__trip_notif_22_l_sa_short_all_dates_t_start)
# [1] 232   5

vessels__trip_notif_22_l_sa_short_all_dates_t_start %>%
  filter(is.na(permit_vessel_id))
# 11
# ...
# 5 NA 52 NA 2021
# 6 NA  1 NA 2023

# vessels__trip_notif_22_l_sa_short_all_dates_t_start %>%
#   filter(TRIP_START_week_num == 47)

## join trip neg and dates ----
by_start_t_neg = join_by(TRIP_DATE_y == YEAR,
                         TRIP_week_num == WEEK_OF_YEAR)

vessels__trip_neg_22_l_sa_short_all_dates_t_start <-
  vessels__trip_neg_22_l_sa_short %>%
  dplyr::right_join(dates_2022_short,
                    by_start_t_neg,
                    relationship = "many-to-many") %>%
  distinct()

dim(vessels__trip_neg_22_l_sa_short_all_dates_t_start)
# [1] 65644     4

vessels__trip_neg_22_l_sa_short_all_dates_t_start %>%
  filter(is.na(permit_vessel_id))
# 11
 # 5 NA NA 52 2021
 # 6 NA NA  1 2023

## find periods (weeks) when each vessel was permitted ----

## for each vessel count neg rep and notif and compare with permitted weeks

# View(vessels_by_permit_vessel__all_l_u)
# dim(vessels_permit_bind_u1)
vessels_permit_bind_u1 %>%
  map_df(dim)
#    dual gom_only sa_only
# 1   378     1204    3877
# 2    48       48      48
# from csv
# 1   379     1204    3877

vessels_permit_bind_u1$sa_only %>%
  head() %>% glimpse()

## how many weeks the permit was in effect ----
# eff_int_sa

vessels_permit_bind_u1_sa_w_p <-
  vessels_permit_bind_u1$sa_only %>%
  mutate(weeks_perm = eff_int_sa / lubridate::dweeks(1))
dim(vessels_permit_bind_u1_sa_w_p)
# [1] 3877   49

vessels_permit_bind_u1_sa_w_p_short <-
  vessels_permit_bind_u1_sa_w_p %>%
  # we need an sa only
  select(-ends_with(".gom"))
dim(vessels_permit_bind_u1_sa_w_p_short)
# [1] 3877   42

### an err ----
# vessels_permit_bind_u1_sa_w_p_short %>%
#    filter(!VESSEL_ID == VESSEL_ID.p) %>% View()
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
View(vessels__trip_neg_22_l)
# vessels_permit_bind_u1_sa_w_p_short
# check for 2022
# count distinct weeks per vessel, compare with permit weeks in year ----

# vessels__trip_neg_22_l_sa_weeks_cnt <-
#   vessels__trip_neg_22_l$sa_only %>%
#   group_by(VESSEL_ID, permit_vessel_id) %>%
#   mutate(distinct_weeks = n_distinct(TRIP_week_num))
#
# vessels__trip_neg_22_l_sa_weeks_cnt %>%
#   View()
# print_df_names(vessels__trip_neg_22_l$sa_only)

## neg trip weeks ----

vessels__trip_neg_22_l_sa_weeks_cnt_u <-
  vessels__trip_neg_22_l$sa_only %>%
  group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
  summarise(distinct_weeks_ne = n_distinct(TRIP_week_num))

# View(vessels__trip_neg_22_l_sa_weeks_cnt_u)
### check ids ----
vessels__trip_neg_22_l_sa_weeks_cnt_u %>%
  filter(!permit_vessel_id == SUPPLIER_VESSEL_ID) %>%
  dim()
# [1] 58  5

vessels__trip_neg_22_l_sa_weeks_cnt_u %>%
  filter(!permit_vessel_id == SERO_OFFICIAL_NUMBER) %>%
  dim()
# [1] 58  5

vessels__trip_neg_22_l_sa_weeks_cnt_u %>%
  filter(!SUPPLIER_VESSEL_ID == SERO_OFFICIAL_NUMBER)
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
  group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
  summarise(distinct_start_weeks_tn = n_distinct(TRIP_START_week_num),
            distinct_end_weeks_tn = n_distinct(TRIP_END_week_num))

# View(vessels__trip_notif_22_l_sa_weeks_cnt_u)
vessels__trip_notif_22_l_sa_weeks_cnt_u %>%
   filter(!distinct_start_weeks_tn == distinct_end_weeks_tn) %>%
   dim()
# [1] 0 6
# ok

## trips weeks count per vessel ----

vessels__trips_22_l_sa_weeks_cnt_u <-
  vessels__trips_22_l$sa_only %>%
  group_by(VESSEL_ID, permit_vessel_id, SUPPLIER_VESSEL_ID, SERO_OFFICIAL_NUMBER) %>%
  summarise(distinct_start_weeks_t = n_distinct(TRIP_START_week_num),
            distinct_end_weeks_t = n_distinct(TRIP_END_week_num)) %>%
  mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t))

# View(vessels__trips_22_l_sa_weeks_cnt_u)
vessels__trips_22_l_sa_weeks_cnt_u %>%
   filter(!distinct_start_weeks_t == distinct_end_weeks_t) %>%
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

### check uniq vsls ----
vessels__t_tne_sa %>% select(VESSEL_ID, permit_vessel_id) %>%
  distinct() %>%
  dim()
# [1] 1751    2

vessels__trip_neg_22_l_sa_vsls <-
  vessels__trip_neg_22_l$sa_only %>%
  select(VESSEL_ID, permit_vessel_id) %>%
  distinct()

# dim(vessels__trip_neg_22_l_sa_vsls)
# 1709

vessels__trips_22_l_sa_vsls <-
  vessels__trips_22_l$sa_only %>%
  select(VESSEL_ID, permit_vessel_id) %>%
  distinct()
# [1] 1110    2

full_join(
  vessels__trip_neg_22_l_sa_vsls,
  vessels__trips_22_l_sa_vsls,
  join_by(VESSEL_ID, permit_vessel_id)
) %>%
  distinct() %>%
  dim()
# [1] 1751    2
# ok, as in join

### check, there should not be doubles? ----
vessels__t_tne_sa_tne_in_t_short <-
  vessels__t_tne_sa %>%
  filter(dplyr::between(TRIP_DATE, TRIP_START_DATE, TRIP_END_DATE)) %>%
  select(TRIP_DATE,
         TRIP_START_DATE,
         TRIP_END_DATE,
         trip_int,
         VESSEL_ID,
         permit_vessel_id)

  # distinct() same

distinct(vessels__t_tne_sa_tne_in_t_short) %>%  dim()
# [1] 5219    6

# View(vessels__t_tne_sa_tne_in_t_short)
# [1] 5513  146

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
  mutate(
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
  select(neg_in_t, starts_with("TRIP")) %>%
  distinct()
toc()
# vessels__t_tne_sa_tne_in_t_cnt distinct: 0.53 sec elapsed
# vessels__t_tne_sa_tne_in_t_cnt unique: 322.19 sec elapsed

vessels__t_tne_sa_tne_in_t_cnt_temp %>%
  # filter(neg_in_t == "both_t__tne") %>%
  filter(neg_in_t == "unknown") %>%
  # Rows: 5,095
  # tail() %>%
  View()
# stopped here ----
# vessels__t_tne_sa_tne_in_t_cnt distinct: 0.53 sec elapsed
# vessels__t_tne_sa_tne_in_t_cnt unique: 322.19 sec elapsed
vessels__t_tne_sa_tne_in_t_cnt_temp %>%
      filter(neg_in_t == "both_t__tne") %>%
    select(TRIP_week_num, TRIP_END_week_num) %>%
    distinct() %>%
  # Rows: 5,095
  # tail() %>%
  filter(!TRIP_week_num == TRIP_END_week_num) %>% dim()
# 45

# count total report number for trips + trip_neg ----
print_df_names(vessels__t_tne_sa)
# dplyr::between(TRIP_DATE,
#                TRIP_START_DATE,
#                TRIP_END_DATE) ~ "both_t__tne",
# is.na(TRIP_START_TIME) ~ "tne",
# is.na(TRIP_DATE) ~ "t",

vessels__t_tne_sa %>%
  dplyr::group_by(VESSEL_ID,
                  permit_vessel_id,
                  SUPPLIER_VESSEL_ID,
                  SERO_OFFICIAL_NUMBER) %>%
  dplyr::summarise(
    distinct_weeks_ne =
      dplyr::n_distinct(TRIP_week_num[neg_in_t == 'tne']),
    distinct_start_weeks_t =
      dplyr::n_distinct(TRIP_START_week_num[!neg_in_t == 'tne']),
    distinct_end_weeks_t =
      dplyr::n_distinct(TRIP_END_week_num[!neg_in_t == 'tne'])
  ) %>%
  ungroup()

  summarize(

    # last_date = max(last_date_temp, na.rm = TRUE),

    distinct_date = n_distinct(date[type != "Online"]),
  ) %>%
  distinct()

          # distinct_date = n_distinct(date[type != "Online"])

  dplyr::summarise(
    distinct_weeks_ne = dplyr::n_distinct(TRIP_week_num),
    distinct_start_weeks_t = dplyr::n_distinct(TRIP_START_week_num),
    distinct_end_weeks_t = dplyr::n_distinct(TRIP_END_week_num)
  ) %>%
  dplyr::mutate(max_weeks_cnt_t = max(distinct_start_weeks_t, distinct_end_weeks_t)) %>%
  dplyr::ungroup()

