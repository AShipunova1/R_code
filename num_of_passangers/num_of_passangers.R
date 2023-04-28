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

# from db ----
query1 <- "select distinct trip_id, COALESCE(state_reg_nbr, coast_guard_nbr)  from
           safis.trips@secapxdv_dblk.sfsc.noaa.gov t
                 JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
      USING ( vessel_id )
    WHERE
      t.sero_vessel_permit IS NOT NULL
      AND t.vendor_app_name <> 'VMS'
    and t.de <= '26-FEB-23'
      AND t.de >= '01-JAN-22'"

trip_id_vessel_from_db <- dbGetQuery(con, query1)
trip_id_vessel_from_db %>% select(TRIP_ID) %>% unique() %>% dim()
# 87145
str(trip_id_vessel_from_db)

### rename columns to be the same ----
paste0(names(trip_id_vessel_from_db), collapse = ", ")
# "TRIP_ID, COALESCE(STATE_REG_NBR,COAST_GUARD_NBR)"
names(trip_id_vessel_from_db) <- c("trip_id", "vessel_official_number")

unique(trip_id_vessel_from_db) %>% dim()

setdiff()