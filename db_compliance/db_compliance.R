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

# split by permit ----

permit_info_r_l_overlap_join1_w_dual_22__list <-
  permit_info_r_l_overlap_join1_w_dual_22 %>%
  split(as.factor(permit_info_r_l_overlap_join1_w_dual_22$permit_sa_gom))

permit_info_r_l_overlap_join1_w_dual_22__list_ids <-
  permit_info_r_l_overlap_join1_w_dual_22__list %>%
  map(
    ~ .x %>%
      select(VESSEL_ID, VESSEL_ALT_NUM.sa, VESSEL_ALT_NUM.gom) %>%
      pivot_longer(
        cols = c(VESSEL_ID,
                 VESSEL_ALT_NUM.sa,
                 VESSEL_ALT_NUM.gom),
        values_to = "vessel_id"
      ) %>%
      select(vessel_id) %>%
      unique() %>%
      return()
  )

# View(permit_info_r_l_overlap_join1_w_dual_22__list_ids)
print_df_names(vessels_all)
# get all vessels for 2022 ----
vessels_by_sero_of_num_coast_g <-
  permit_info_r_l_overlap_join1_w_dual_22__list_ids %>%
  map(~ .x %>%
        inner_join(vessels_all,
                   join_by(vessel_id == COAST_GUARD_NBR)))

vessels_by_sero_of_num_state_n <-
  permit_info_r_l_overlap_join1_w_dual_22__list_ids %>%
  map(~ .x %>%
        inner_join(vessels_all,
                   join_by(vessel_id == STATE_REG_NBR)))



permit_info_r_l_overlap_join1_w_dual_22__list_id_num <-
  permit_info_r_l_overlap_join1_w_dual_22__list_ids %>%
  map(~ .x %>% dim())
      

vessels_by_sero_of_num_state_n_num <-
  vessels_by_sero_of_num_state_n %>% 
  map(~ .x %>% 
        select(vessel_id) %>% 
        unique() %>% 
      dim())

str(permit_info_r_l_overlap_join1_w_dual_22__list_id_num)
str(vessels_by_sero_of_num_coast_g)
str(vessels_by_sero_of_num_state_n)

vessels_by_sero_of_num__all <-
  # map over 2 lists of dataframes
  map2_dfr(vessels_by_sero_of_num_coast_g,
           vessels_by_sero_of_num_state_n,
           dplyr::bind_rows) %>% 
  unique()

vessels_by_sero_of_num__all_l <-
  # map over 2 lists of dataframes
  map2(vessels_by_sero_of_num_coast_g,
           vessels_by_sero_of_num_state_n,
           dplyr::bind_rows)


View(vessels_by_sero_of_num__all_l)

vessels_by_sero_of_num__all_l_num <-
  vessels_by_sero_of_num__all_l %>% 
  map(~ .x %>% 
        select(vessel_id) %>% 
        unique() %>% 
      dim()
      )

str(permit_info_r_l_overlap_join1_w_dual_22__list_id_num)
str(vessels_by_sero_of_num__all_l_num)
# > str(permit_info_r_l_overlap_join1_w_dual_22__list_id_num)
# List of 3
#  $ dual    : int [1:2] 393 1
#  $ gom_only: int [1:2] 1272 1
#  $ sa_only : int [1:2] 4024 1
# > str(vessels_by_sero_of_num__all_l_num)
# List of 3
#  $ dual    : int [1:2] 393 1
#  $ gom_only: int [1:2] 1272 1
#  $ sa_only : int [1:2] 4020 1

all.equal(
  permit_info_r_l_overlap_join1_w_dual_22__list_id_num,
  vessels_by_sero_of_num__all_l_num
)
# [1] "Component “sa_only”: Mean relative difference: 0.0009940358"

setdiff(
  permit_info_r_l_overlap_join1_w_dual_22__list_ids$sa_only$vessel_id,
  vessels_by_sero_of_num__all_l$sa_only$vessel_id
)
# [1] "1304296"  "NA"       "FL6437NY" "1176885" 

setdiff(
  vessels_by_sero_of_num__all_l$sa_only$vessel_id,
  permit_info_r_l_overlap_join1_w_dual_22__list_ids$sa_only$vessel_id
)
0

permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>% 
    filter(VESSEL_ID == '1304296') %>% View()
# alt_num.sa DL5161AM

permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>% 
    filter(VESSEL_ID == 'FL6437NY') %>% View()

# FL6437NY
# 2015-09-01 EDT--2022-06-02 EDT
# VESSEL SOLD


permit_info_r_l_overlap_join1_w_dual_22__list$sa_only %>% 
    filter(VESSEL_ID == '1176885') %>% View()
# alt num FL0668PV 2021-05-01 EDT--2022-03-31 EDT


vessels_by_sero_of_num__all_l$sa_only %>% 
  filter(vessel_id == '1304296' |
           VESSEL_ID == '1304296' |
           COAST_GUARD_NBR == '1304296' |
           STATE_REG_NBR == '1304296'
           ) %>% 
  View()
# 0


vessels_by_sero_of_num__all_l$sa_only %>% 
  filter(vessel_id == 'DL5161AM') %>% 
  View()
# 1
# SERO_OFFICIAL_NUMBER is NULL