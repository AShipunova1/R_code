# check diff between logbook csv and db
View(fhier_logbooks_content)
fhier_logbooks_content %>%
  filter(trip_de >= "2022-01-01 00:00:00") %>%
  select(trip_id) %>% str()
# 346005
max(fhier_logbooks_content$trip_de)
# [1] "2023-02-26 23:51:34"

trip_id_vessel_from_logbooks <-
fhier_logbooks_content %>%
  filter(trip_de >= "2022-01-01 00:00:00",
         !(tolower(vendor_app_name) == "vms")) %>%
  select(trip_id, vessel_official_number) %>% unique()
# not unique 277526

dim(trip_id_vessel_from_logbooks)
# 81594

unique(trip_id_vessel_from_logbooks) %>% dim()

trip_id_vessel_from_logbooks %>% select(trip_id) %>% unique() %>% dim()
# 81594

# write_csv(trip_id_vessel, "trip_id_vessel.csv")

trip_id_vessel_from_db %>% select(trip_id) %>% unique() %>% dim()
# 87145
# 89632     with sero_permit is null
str(trip_id_vessel_from_db)

unique(trip_id_vessel_from_db) %>% dim()
# 87145     

# trip_id_vessel_from_logbooks
# 81594
str(trip_id_vessel_from_logbooks)
trips_logbooks <- as.numeric(trip_id_vessel_from_logbooks$trip_id) %>%
  unique()
trips_db_trips <- as.numeric(trip_id_vessel_from_db$trip_id) %>%
  unique()
# str(trips_db_trips)

in_logbooks_only <-
  setdiff(trips_logbooks, trips_db_trips)
length(in_logbooks_only)
# 60

in_db_only <-
  setdiff(trips_db_trips, trips_logbooks)
length(in_db_only)
# 8098

# out_dir <- file.path(my_paths$inputs, "fhier_vs_db")
# write_csv(as.data.frame(in_logbooks_only),
#           file.path(out_dir, "trips_in_logbooks_only.csv"))
# write_csv(as.data.frame(in_db_only),
#           file.path(out_dir, "trips_in_db_only.csv"))

# ----
trip_id_vessel_from_logbooks %>%
  filter(trip_id == "61479063")

trip_id_vessel_from_db %>%
  filter(trip_id == "61479063")

# ---
str(in_db_only)

trip_id_vessel_from_logbooks %>%
  filter(trip_id == "59403264")

trip_id_vessel_from_db %>%
  filter(trip_id == "59403264")

trip_id_vessel_from_logbooks %>%
  filter(vessel_official_number == "556499")

#    trip_id state_reg_nbr coast_guard_nbr
# 1 59403264          <NA>          556499
# in FHIER trips are all in 2021
# names(trip_id_vessel_st_from_db)
str(trip_id_vessel_st_from_db)
# [1] "TRIP_ID"         "STATE_REG_NBR"   "COAST_GUARD_NBR"
# [4] "DE"              "VENDOR_APP_NAME" "STATE_NAME"     

str(in_db_only)
# trip_id_vessel_st_from_db %<>%
#   mutate(TRIP_ID = as.numeric(TRIP_ID))
trip_id_vessel_st_from_db %>%
  filter(TRIP_ID %in% in_db_only) %>%
  select(STATE_NAME) %>% unique()

fhier_logbooks_content %>%
  # select(all_of(ends_with("state"))) %>%
  # select(start_port_state, end_port_state) %>%
  select(start_port_state) %>%
  arrange(start_port_state) %>%
  unique()
# 17
# NJ              
# 12 NY              
# 13 RI

# states are not the cause for the difference
head(in_db_only)

# vessel_nbr ----
vessel_nbr_trips_indb_only <-
  trip_id_vessel_st_from_db %>%
  filter(TRIP_ID %in% in_db_only) %>%
  mutate(vessel_off_num = coalesce(STATE_REG_NBR, COAST_GUARD_NBR)) %>% 
  # select(STATE_REG_NBR, COAST_GUARD_NBR) %>% 
  select(vessel_off_num) %>%
    unique() 
# %>% 
#   dim()
# 691   

str(vessel_nbr_trips_indb_only)
fhier_logbooks_content %>%
  filter(vessel_official_nbr %in% vessel_nbr_trips_indb_only$vessel_off_num) %>% View()
