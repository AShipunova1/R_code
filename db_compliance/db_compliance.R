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


### permit + vessel from db ----
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

# View(permit_info_r_l_overlap_join1_w_dual)
# 186,210 
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
                      interval_2022))

permit_info_r_l_overlap_join1_w_dual_22
  filter(permit_sa_gom == "dual") %>% 
  select(VESSEL_ID) %>% 
  unique() %>% 
  dim()
# 361
# end here permits

