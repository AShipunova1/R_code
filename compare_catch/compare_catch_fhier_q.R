##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)

# include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# spp. is 0
# region is not SA or GOM
glimpse(logbooks_content)

## spp. is 0 ----
logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>% glimpse()
# Rows: 89

# grep("common", names(logbooks_content), value = T)
# common_name

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  select(common_name) %>% unique()
# NA

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  select(trip_start_date) %>% unique()
# 70

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  write.csv(file = "logbooks_content_sp0.csv", row.names = F)

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  # View()
  select(vessel_official_nbr) %>% unique()
  
## region is not SA or GOM ----
glimpse(fhier_logbooks_content_waves__sa_gom)

fhier_logbooks_content_waves__sa_gom %>%
  filter(!(end_port_sa_gom %in% c("sa", "gom"))) %>% glimpse()
# Rows: 188

not_specified_region <-
  fhier_logbooks_content_waves__sa_gom %>%
  filter(!(end_port_sa_gom %in% c("sa", "gom")))

not_specified_region_states <-
  not_specified_region %>%
  select(
    vessel_official_nbr,
    trip_id,
    in_state,
    latitude,
    longitude,
    area_code,
    sub_area_code,
    distance_code,
    distance_code_name,
    local_area_code,
    state,
    state_name,
    start_port,
    start_port_name,
    start_port_county,
    start_port_state,
    end_port,
    end_port_name,
    end_port_county,
    end_port_state
  )

View(not_specified_region_states)

not_specified_region_states %>%
  filter(end_port_county == "NOT-SPECIFIED" &
           start_port_county != "MONROE") %>%
  # select(vessel_official_nbr) %>% unique()
  # 1244719
  # glimpse()
  # select(state_name) %>% unique()
  # FLORIDA
  # select(end_port) %>% unique()
  # 100999
  select(start_port_name) %>% unique()
# FLORIDA(STATE)

fhier_logbooks_content_waves_fl_county %>%
  filter(state_name == 'FLORIDA') %>%
  filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>%
  select(-c(`1`)) %>%
  # filter(!all(is.na(.))) %>%
  #   Rows: 112
  # Columns: 159
  # filter(complete.cases(.)) %>%
  # Rows: 0
  # unique() %>% glimpse()
  # write.csv(file = "fhier_logbooks_no_fl_county.csv", row.names = F)
  
  ## === logbooks_content wrong dates ====

grep("date", names(fhier_logbooks_content), value = T)

fhier_dates <-
  fhier_logbooks_content %>%
  select(grep("date", names(fhier_logbooks_content), value = T))

max(fhier_dates$trip_start_date_time)
# [1] "2023-06-13 08:00:00 EDT"
min(fhier_dates$trip_start_date_time)

max(fhier_dates$trip_end_date_time)
# [1] "2023-06-13 16:00:00 EDT"
min(fhier_dates$trip_end_date_time)
# [1] "1969-08-17 12:30:00 EDT"

fhier_dates %>%
  filter(trip_start_date_time < "2022-01-01" |
           trip_start_date_time > "2023-04-01")
# 1

fhier_dates %>%
  filter(trip_end_date_time < "2022-01-01" |
           trip_end_date_time > "2023-04-01")
# 34

# ## to csv
fhier_logbooks_content %>%
  filter(
    trip_start_date_time < "2022-01-01" |
      trip_start_date_time > "2023-04-01" |
      trip_end_date_time < "2022-01-01" |
      trip_end_date_time > "2023-04-01"
  ) %>%
  write.csv(file = "fhier_logbooks_wrong_dates.csv", row.names = F)

## questions 2 ----
# (1) check the transmission year. Anything sent in 2021 we ignore, since it was a testing year for approving apps.
# (2) check the vendor. If it’s VMS, there’s not a lot we can do to ask the vendor to resolve but we can of course ask the auditing team to call the user for corrections. If it’s eTrips, we need to see if the vessel has our permits. Since a user can select any vessel in eTrips, it means we sometimes were getting reports from vessels that did not have our permit and so we’re no getting our questions. These we just need to filter out of the data. As of about 4 months ago, Yanet should be filtering all data to exclude vessels that did not have our permit(s).

### === transmission ====
grep("transm", names(logbooks_content), value = T)

logbooks_content_transmission_date_ct <-
  logbooks_content %>%
  mutate(transmission_date_ct = as.POSIXct(transmission_date,
                                           format = "%Y-%m-%d %H:%M:%S"))
# %>%
# select(transmission_date_ct, transmission_date) %>% unique() %>% str()
# d <- "2022-08-09 00:55:00"
# as.POSIXct(d, format = "%Y-%m-%d %H:%M:%S")

grep("start",
     names(logbooks_content_transmission_date_ct),
     value = T)

logbooks_content_dates_ct <-
  logbooks_content_transmission_date_ct %>%
  mutate(
    trip_start_date_ct = as.POSIXct(trip_start_date, format = "%Y-%m-%d %H:%M:%S"),
    trip_end_date_ct = as.POSIXct(trip_end_date, format = "%Y-%m-%d %H:%M:%S")
  )
logbooks_content_dates_ct %>%
  filter(
    trip_start_date < "2022-01-01" |
      trip_start_date > "2023-04-01" |
      trip_end_date < "2022-01-01" |
      trip_end_date > "2023-04-01"
  ) %>%
  filter(transmission_date_ct > "2022-01-01") %>%
  select(-`1`)
# %>%
# select(trip_start_date, trip_end_date, transmission_date) %>% unique()
# glimpse()
# Rows: 20
# write.csv(file = "fhier_logbooks_wrong_dates1.csv", row.names = F)

### === vendor ====
# (2) check the vendor. If it’s VMS, there’s not a lot we can do to ask the vendor to resolve but we can of course ask the auditing team to call the user for corrections. If it’s eTrips, we need to see if the vessel has our permits. Since a user can select any vessel in eTrips, it means we sometimes were getting reports from vessels that did not have our permit and so we’re no getting our questions. These we just need to filter out of the data. As of about 4 months ago, Yanet should be filtering all data to exclude vessels that did not have our permit(s).

fhier_logbooks_content_waves_fl_county %>%
  select(starts_with("notif"), user_app) %>%
  unique() %>%
  # Rows: 47,242
  # Columns: 33
  #   filter(complete.cases(.)) %>%
  # 0
  glimpse()

# user_app, system
grep("sta", names(not_specified_region), value = T)

grep("accsp", names(not_specified_region), value = T)

not_specified_region %>%
  filter(state_name == "FLORIDA") %>%
  select(starts_with("notif"), user_app, accsp_permit_license_nbr) %>%
  unique() %>%
  View()
