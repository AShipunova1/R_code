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

print_df_names(permit_vessel_query_exp21_reg)

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
  View()
# 7573