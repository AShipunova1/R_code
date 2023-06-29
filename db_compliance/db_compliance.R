# db_compliance
# It would be really interesting to see an after the fact analysis done for compliance with another point of view, you can query the source of the activity from the tables we download from ACCSP and the permit materialized view mv_sero_fh_permits_his and then apply the latest compliance rules (all the fields needed for compliance are in these tables), it is very different to write an analysis report after the fact than build something for day-to-day activity that it has to be assigned to multiple users step by step.
# CATCHES
# EFFORTS
# TRIPS
# TRIPS_NEG
# TRIP_NOTIFICATIONS
# VESSELS

# setup ----

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

# print_df_names(permit_vessel_query_exp21_reg)

permit_vessel_query_exp21_reg <-
  permit_vessel_query_exp21_1 %>%
  separate_permits_into_3_groups(permit_group_field_name = "TOP")

permit_vessel_query_exp21_reg %>%
  select(PERMIT_GROUP,
         permit_sa_gom) %>%
  unique() %>%
  arrange(PERMIT_GROUP) %>%
  head(7)
#   PERMIT_GROUP permit_sa_gom
# 1            2       sa_only
# 2            4       sa_only
# 3            5       sa_only
# 4            6       sa_only
# 5            7      gom_only
# 6            7       sa_only
# 7            8       sa_only


permit_vessel_query_exp21_reg %>%
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom) %>%
  unique() %>%
# 7573
  count(SERO_OFFICIAL_NUMBER) %>%
  filter(n > 1) %>%
  View()
# 483

### both permit group should be active at the same time to be "dual" ----

print_df_names(permit_vessel_query_exp21_reg)

permit_vessel_query_exp21_reg_0 <-
  permit_vessel_query_exp21_reg %>%
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         EFFECTIVE_DATE,
         END_DATE,
         EXPIRATION_DATE) %>%
  mutate(my_end_date = dplyr::coalesce(END_DATE,
                                       EXPIRATION_DATE)) %>%
  select(-c(END_DATE,
            EXPIRATION_DATE)) %>%
  unique()
# dim()
# 17588
# 17583
# View()

permit_vessel_query_exp21_reg_0 %>% 
  dplyr::group_by(SERO_OFFICIAL_NUMBER) %>% 
  dplyr::summarise(distinct_regs = n_distinct(permit_sa_gom)) %>% 
  filter(distinct_regs > 1) %>%
  ungroup() %>% 
  View()
# 483

permit_vessel_query_exp21_reg_1 <- 
  permit_vessel_query_exp21_reg_0 %>% 
  dplyr::group_by(SERO_OFFICIAL_NUMBER) %>% 
  dplyr::mutate(distinct_regs = n_distinct(permit_sa_gom)) %>% 
  ungroup()

# View(permit_vessel_query_exp21_reg_1)

permit_vessel_query_exp21_reg_1 %>%
  filter(distinct_regs > 1 &
           (all(abs(diff(my_end_date)) > 0) |
           all(abs(diff(EFFECTIVE_DATE)) > 0))) %>%
  View()
# 0
  # filter(n() >= 2, all(abs(diff(dat)) <= 1)) %>%
  

permit_vessel_query_exp21_reg_1 %>%
  group_by(SERO_OFFICIAL_NUMBER) %>% 
  filter(distinct_regs > 1 &
           (all(abs(diff(my_end_date)) < 1) &
           all(abs(diff(EFFECTIVE_DATE)) < 1))) %>%
  ungroup() %>% 
  View()
# 102
  # filter(n() >= 2, all(abs(diff(dat)) <= 1)) %>%
  # ungroup

permit_vessel_query_exp21_reg_2 <-
  permit_vessel_query_exp21_reg_1 %>%
  group_by(SERO_OFFICIAL_NUMBER) %>%
  mutate(dual_perm_same_dates =
           case_when(distinct_regs > 1 &
                       (all(abs(
                         diff(my_end_date)
                       ) < 1) &
                         all(abs(
                           diff(EFFECTIVE_DATE)
                         ) < 1)) ~ "dual",
                     .default = permit_sa_gom)) %>% 
  ungroup()

permit_vessel_query_exp21_reg_2 %>%
  filter(dual_perm_same_dates == "dual") %>% 
  dim()
# 102

permit_vessel_query_exp21_1 %>% 
  filter(VESSEL_ID_permit == '676256') %>% 
  view()

# print_df_names(permit_vessel_query_exp21_reg_1)

permit_vessel_query_exp21_reg_1 %>% 
  filter(SERO_OFFICIAL_NUMBER == '676256') %>% 
  str()

permit_vessel_query_exp21_reg_1 %>%
  # filter(SERO_OFFICIAL_NUMBER == '676256') %>% 
  group_by(SERO_OFFICIAL_NUMBER) %>%
  mutate(a = abs(diff(my_end_date))) %>% 
  ungroup() %>% 
  str()

# ℹ In argument: `a = abs(diff(my_end_date))`.
# ℹ In group 1: `SERO_OFFICIAL_NUMBER = "1000042"`.
# Caused by error:
# ! `a` must be size 4 or 1, not 3.

# check differently
# https://stackoverflow.com/questions/63402652/comparing-dates-in-different-columns-to-isolate-certain-within-group-entries-in