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

