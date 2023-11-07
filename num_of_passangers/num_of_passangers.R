# From db num of passengers > capacity ----
# View(passengers_from_db)

data_overview(passengers_from_db)
# TRIP_ID               493
# VESSEL_NAME            62


# check diff between logbook csv and db ----
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

# Run once
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

# Run once
# out_dir <- file.path(my_paths$inputs, "fhier_vs_db")
# write_csv(as.data.frame(in_logbooks_only),
#           file.path(out_dir, "trips_in_logbooks_only.csv"))
# write_csv(as.data.frame(in_db_only),
#           file.path(out_dir, "trips_in_db_only.csv"))

# compare one trip ----
trip_id_vessel_from_logbooks %>%
  filter(trip_id == "61479063")

trip_id_vessel_from_db %>%
  filter(trip_id == "61479063")

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
# [1] "TRIP_ID"          "SUPPLIER_TRIP_ID" "STATE_REG_NBR"   
# [4] "COAST_GUARD_NBR"  "DE"               "VENDOR_APP_NAME" 
# [7] "STATE_NAME"      
str(trip_id_vessel_st_from_db)

# is the diff by port? no
str(in_db_only)
# trip_id_vessel_st_from_db %<>%
#   dplyr::mutate(TRIP_ID = as.numeric(TRIP_ID))
trip_id_vessel_st_from_db %>%
  filter(TRIP_ID %in% in_db_only) %>%
  select(STATE_NAME) %>% unique()

fhier_logbooks_content %>%
  select(start_port_state) %>%
  dplyr::arrange(start_port_state) %>%
  unique()
# 17
# NJ              
# 12 NY              
# 13 RI

# states are do not cause the difference
head(in_db_only)

# vessel_nbr ----
vessel_nbr_trips_indb_only <-
  trip_id_vessel_st_from_db %>%
  filter(trip_id %in% in_db_only) %>%
  dplyr::mutate(vessel_off_num = coalesce(state_reg_nbr, coast_guard_nbr)) %>% 
  # select(STATE_REG_NBR, COAST_GUARD_NBR) %>% 
  select(vessel_off_num) %>%
    unique() 
# %>%
  # dim()
# 691   
# 0

str(vessel_nbr_trips_indb_only)
fhier_logbooks_content %>%
  filter(vessel_official_nbr %in% vessel_nbr_trips_indb_only$vessel_off_num) %>% View()

# === compare downloaded from Fhier and from db again ----
logbooks_downloaded_from_fhier_trip_id_only <-
  logbooks_downloaded_from_fhier_trip_vsl %>%
  select(trip_id) %>% unique() 
# %>% 
  # dim()
  
trip_id_vessel_st_from_db_trip_id_only <-
  trip_id_vessel_st_from_db %>%
  select(trip_id) %>% unique()
# %>% dim()

# in FHIER only
setdiff(logbooks_downloaded_from_fhier_trip_id_only$trip_id, 
        trip_id_vessel_st_from_db_trip_id_only$trip_id) %>% length()
# 2

# In db only
setdiff(trip_id_vessel_st_from_db_trip_id_only$trip_id,
        logbooks_downloaded_from_fhier_trip_id_only$trip_id) %>%
  length()
# 5348

names(trip_id_vessel_st_from_db)
# [1] "trip_id"          "supplier_trip_id" "state_reg_nbr"   
# [4] "coast_guard_nbr"  "de"               "vendor_app_name" 
# [7] "state_name" 

names(logbooks_downloaded_from_fhier)
#  [1] "view"                      "supplier_trip_id"         
#  [3] "trip_type"                 "vessel_official_number"   
#  [5] "vesselname"                "activitystart_date"       
#  [7] "activitystart_time"        "activityend_date"         
#  [9] "activityend_time"          "portname"                 
# [11] "portcounty"                "portstate"                
# [13] "de"                        "ue"                       
# [15] "submissionlag_time__mins_"

# by supplier_trip_id ----
# FHIER
logbooks_downloaded_from_fhier_sup_trip_id_only <-
  logbooks_downloaded_from_fhier %>%
  select(supplier_trip_id) %>% unique() 

# names(logbooks_downloaded_from_fhier_trip_vsl)
  
# DB
trip_id_vessel_st_from_db_sup_trip_id_only <-
  trip_id_vessel_st_from_db %>%
  select(supplier_trip_id) %>% unique()

# str(logbooks_downloaded_from_fhier_sup_trip_id_only)

in_db_only <-
setdiff(trip_id_vessel_st_from_db_sup_trip_id_only$supplier_trip_id,
        logbooks_downloaded_from_fhier_sup_trip_id_only$supplier_trip_id) 
# str(logbooks_downloaded_from_fhier_trip_id_only)

length(in_db_only)
# 7057
# with VMS 8958
# with VMS and correct dates 9148
# 5259

in_fhier_only <-
  setdiff(
    logbooks_downloaded_from_fhier_sup_trip_id_only$supplier_trip_id,
    trip_id_vessel_st_from_db_sup_trip_id_only$supplier_trip_id
  )

length(in_fhier_only)
# 17586
# with VMS 2380
# 2

# head(in_fhier_only)

# what is in logbooks only: ----
# VMS
logbooks_downloaded_from_fhier %>%
  filter(supplier_trip_id %in% in_fhier_only) %>%
  dplyr::glimpse()
# $ activitystart_date        <chr> "06/23/2022", "06/03/2022"
# $ activityend_date          <chr> "06/24/2022", "06/03/2021"
# $ ue                        <chr> "VMS", "VMS"
# $ submissionlag_time__mins_ <dbl> -525174, 469

# what is in db only: ----
in_db_only

# not downloaded

vessel_num_in_db_only <-
  trip_id_vessel_st_from_db %>%
  filter(supplier_trip_id %in% in_db_only) %>%
  # dplyr::glimpse()
  dplyr::mutate(vessel_num = coalesce(state_reg_nbr, coast_guard_nbr)) %>%
  select(vessel_num)

str(vessel_num_in_db_only)
# vessel_num_in_db_only$vessel_num %>% head(100) %>% 
  # paste0(collapse = ", ")

# filter supplier_trip_id in DB only----

supplier_trip_id_in_db_only <-
  trip_id_vessel_st_from_db %>%
  filter(supplier_trip_id %in% in_db_only) %>%
  select(supplier_trip_id)
  
str(supplier_trip_id_in_db_only)

# A filter fo FHIER
# supplier_trip_id_in_db_only$supplier_trip_id %>%
#   head(2000) %>%
#   tail(1000) %>%
#   paste0(collapse = ", ")
# 

# names(new_logb)
in_db_only <-
setdiff(new_logb$suppliertripid,
        logbooks_downloaded_from_fhier_sup_trip_id_only$supplier_trip_id) 
# str(logbooks_downloaded_from_fhier_trip_id_only)

length(in_db_only)
# 24473
# 27575

# --- compare with FHIER downloads ----
# names(new_logb)
new_logb_supplier_ids <-
  new_logb %>%
  filter(suppliertripid %in% in_db_only) %>%
  select(suppliertripid) %>% unique()

new_logb_supplier_ids %>%
  head()

# 42023030444943, 31423124614460, 22723072211510, 22823061120428, 31823115219825, 42523113314349
# not in fhier
