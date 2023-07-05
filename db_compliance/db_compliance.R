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
  unique() %>%
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
  unique()

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
  unique() %>% 
  dim()
# [1] 13930     1

permit_info_r %>% 
  select(VESSEL_ID) %>% 
  unique() %>% 
  dim()
# 13930     

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
  unique()
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
  unique()
# all 3

permit_info_r_l_overlap_join1_w_dual_22 %>% 
  filter(permit_sa_gom == "dual") %>% 
  select(VESSEL_ID) %>% 
  unique() %>% 
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
      unique() %>%
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

vessels_permit_vsl_id__all_l <-
  # map over 2 lists of dataframes and make a list
  map2(vessels_permit_vsl_id_coast_g,
           vessels_permit_vsl_id_state_n,
           dplyr::bind_rows)

## the same for checking as a df ----
vessels_permit_vsl_id__all <-
  # map over 2 lists of dataframes and make a df
  map2_dfr(vessels_permit_vsl_id_coast_g,
           vessels_permit_vsl_id_state_n,
           dplyr::bind_rows) %>%
  unique()

### check joins ----

vessels_by_permit_vessel_num <-
  vessels_permit_vsl_id__all %>%
  select(permit_vessel_id) %>%
  unique() %>%
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

View(vessels_permit_vsl_id__all_2)

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

# View(vessels_permit_vsl_id__all_l)
tic("vessels_by_permit_vessel__all_l_u")
vessels_by_permit_vessel__all_l_u <-
  vessels_permit_vsl_id__all_l %>%
  map(~ .x %>%
        group_by(permit_vessel_id) %>%
        dplyr::summarise_all(coalesce_by_column))
toc()
# vessels_by_permit_vessel__all_l_u: 98.53 sec elapsed

### check vessels_permit_vsl_id__all_u ---

map_df(vessels_permit_vsl_id__all_l,
       ~.x %>% dim())
#    dual gom_only sa_only
# 1   392   141154  143932

map_df(vessels_by_permit_vessel__all_l_u,
       ~.x %>% dim())
#    dual gom_only sa_only
# 1   392     1272    4020
# 2    30       30      30

uniq_permit_vsl_ids <-
  map_df(
  vessels_by_permit_vessel__all_l_u,
  ~ .x %>%
    select(permit_vessel_id) %>%
    unique() %>%
    dim()
)
#    dual gom_only sa_only
#   <int>    <int>   <int>
# 1   392     1272    4020
# 2     1        1       1
View(uniq_permit_vsl_ids)
uniq_permit_vsl_ids %>% 
  rowSums()
# 5684    

# vessels_by_permit_vessel__all %>%
#   select(permit_vessel_id) %>%
#   unique() %>%
#   dim()
# 5632    

# GOM 2022 compliance ----
# There should be a declaration for every logbook (in other words, the number of fishing intended charter declarations would need to be equal to logbooks to be compliant).
# There should be a logbook for every declaration of a charter or headboat intending to fish.

# View(trip_notifications_2022)

### compare vessel_ids ----
trip_notifications_2022_vsl_ids <-
  trip_notifications_2022 %>%
  select(VESSEL_ID) %>%
  unique()

vessels_by_permit_vessel__all_l_u_vsl_ids <-
  vessels_by_permit_vessel__all_l_u %>%
  map_df( ~ .x %>%
            select(VESSEL_ID) %>%
            unique())

vessels_by_permit_vessel__all_l_u_vsl_ids_l <-
  vessels_by_permit_vessel__all_l_u %>%
  map( ~ .x %>%
            select(VESSEL_ID) %>%
            unique())

dim(trip_notifications_2022)
# [1] 129746     33
dim(trip_notifications_2022_vsl_ids)
# [1] 1095    1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids)
# [1] 5459      1
# vessels_by_permit_vessel__all_l_u_vsl_ids %>%
#   unique() %>%
#   dim()
# [1] 5409    1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids_l$gom_only)
# [1] 1204    1
dim(vessels_by_permit_vessel__all_l_u_vsl_ids_l$dual)
# 378   
# 1204+378
# 1582

# # not in vessel_trip sa
# not_in_vessel_trip_sa <-
#   setdiff(
#     trip_notifications_2022_vsl_ids$VESSEL_ID,
#     unique(vessels_permit_vsl_id__all_l__sa_ids$VESSEL_ID)
#   ) %>%
#   unique()
# glimpse(not_in_vessel_trip_sa)
# # 60
 # num [1:60] 327682 248806 326294 249111 246954 ...

View()
not_in_vessel_trip_gom <-
  setdiff(
    trip_notifications_2022_vsl_ids$VESSEL_ID,
    unique(vessels_by_permit_vessel__all_l_u_vsl_ids_l$gom_only$VESSEL_ID)
  ) %>%
  unique()

glimpse(not_in_vessel_trip_gom)
 # num [1:15] 326294 249111 280684 326421 326390 ...
 # num [1:305] 328214 328340 247128 247129 326387 ...

## join gom vessels, trip, trip notif  ----
View(vessels_by_permit_vessel__all_l_u)

vessels__trip_notif_22_gom <-
  inner_join(
    trip_notifications_2022,
    unique(vessels_by_permit_vessel__all_l_u$gom_only),
    join_by(VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".tn", ".v")
  )

View(vessels__trip_notif_22_gom)

### trip types A and H gom trip_notif ----

vessels__trip_notif_22_gom %>% 
   select(TRIP_TYPE) %>% unique()
#     TRIP_TYPE
# 1           H
# 3           A
# 383         R
# 697         C

vessels__trip_notif_22_gom_AH <-
  vessels__trip_notif_22_gom %>% 
  filter(TRIP_TYPE %in% c("A", "H"))

