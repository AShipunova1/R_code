##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)

## include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false
## get data ----

source("~/R_code_github/compare_catch/compare_catch_data_preparation.R")

# Use fhier_logbooks_content_waves__sa_gom

### Transmission year ====
# (1) check the transmission year (trip_de fireld). Anything sent in 2021 we ignore, since it was a testing year for approving apps.
# ```{r Transmission year}

# fhier_logbooks_content_waves__sa_gom %>%
#   select(trip_de, transmission_date) %>% unique()

# grep("transm", names(fhier_logbooks_content_waves__sa_gom), value = T)

logbooks_content_transmission_trip_de_ct <-
  fhier_logbooks_content_waves__sa_gom %>%
  # convert to a date format
  mutate(trip_de_ct =
           as.POSIXct(trip_de,
                      format = "%Y-%m-%d %H:%M:%S"))

logbooks_content_transmission_date_ok <-
  logbooks_content_transmission_trip_de_ct %>%
  # keep only correct transmission dates
  filter(trip_de_ct > "2022-01-01" |
           # or NAs
           is.na(trip_de_ct)) %>%
  # rm an extra column
  select(-`1`)

dim(logbooks_content_transmission_trip_de_ct)
# [1] 320024    161

dim(logbooks_content_transmission_date_ok)
# [1] 320021    160

# names(logbooks_content_transmission_date_ok)

# glimpse(logbooks_content_transmission_date_ok)

### Check the vendor ----
# (2) check the vendor. If it’s VMS, there’s not a lot we can do to ask the vendor to resolve but we can of course ask the auditing team to call the user for corrections. If it’s eTrips, we need to see if the vessel has our permits. Since a user can select any vessel in eTrips, it means we sometimes were getting reports from vessels that did not have our permit and so we’re no getting our questions. These we just need to filter out of the data. As of about 4 months ago, Yanet should be filtering all data to exclude vessels that did not have our permit(s).

# find the field
# grep("vendor", names(logbooks_content), value = T)
# [1] "vendor_app_name" "vendor_platform"

logbooks_content %>%
  select(starts_with("vendor")) %>%
  filter(!grepl("vms", tolower(vendor_app_name))) %>%
  unique() %>% glimpse()
# 9

logbooks_content_transmission_date_ok %>%
  select(vendor_app_name) %>% unique() %>% glimpse()
# 6

transm_before_after <-
  logbooks_content_transmission_date_ok %>%
  mutate(
    transmission_date_group =
      case_when(
        trip_de_ct < "2022-01-01" ~ "before Jan 2022",
        trip_de_ct > "2022-01-01" ~ "after Jan 2022"
      )
  )

transm_before_after %>%
  # select(vendor_app_name, trip_de_ct, transmission_date_group) %>%
  # glimpse()
  select(vendor_app_name, transmission_date_group, sero_vessel_permit, accsp_permit_license_nbr) %>%
  # unique()
  dim()
# [1] 320021      4

#### count how many in each group ----
transm_before_after %>%
  # having SERO permit
  filter(!is.na(sero_vessel_permit)) %>%
  select(
    vendor_app_name,
    transmission_date_group
  ) %>%
  group_by(
    vendor_app_name,
    transmission_date_group
  ) %>%
  summarise(n = n())
# transmission_date is NA for all vendors except "VMS"
# using trip_de
# without filter(!is.na(sero_vessel_permit))
# vendor_app_name        transmission_date_group      n
# <chr>                  <chr>                    <int>
#   1 BLUEFIN DATA ACCSP SDK after Jan 2022            5847
# 2 ETRIPS ONLINE          after Jan 2022            1189
# 3 ETRIPS/MOBILE 2        after Jan 2022           68716
# 4 VESL                   after Jan 2022          182091
# 5 VMS                    after Jan 2022           59878
# 6 NA                     after Jan 2022            2300

# with sero_vessel_permit
#   vendor_app_name        transmission_date_group      n
#   <chr>                  <chr>                    <int>
# 1 BLUEFIN DATA ACCSP SDK after Jan 2022            5395
# 2 ETRIPS ONLINE          after Jan 2022            1189
# 3 ETRIPS/MOBILE 2        after Jan 2022           68051
# 4 VESL                   after Jan 2022          179309
# 5 NA                     after Jan 2022            2230

logbooks_content_transmission_date_not_vms_ok <-
  logbooks_content_transmission_date_ok %>%
  # not VMS
  filter(!tolower(vendor_app_name) == "vms") %>%
  # has SERO permit
  filter(!is.na(sero_vessel_permit))

dim(logbooks_content_transmission_date_not_vms_ok)
# [1] 257843    162
# with sero_vessel_permit
# [1] 253944    160

# names(logbooks_content_transmission_date_not_vms_ok)

## Wrong dates ----
fhier_dates <-
  logbooks_content_transmission_date_not_vms_ok %>%
  select(grep(
    "date",
    names(logbooks_content_transmission_date_not_vms_ok),
    value = T
  ), trip_de, trip_de_ct)
# View(fhier_dates)

max(fhier_dates$trip_start_date_time)
# [1] "2023-06-13 08:00:00 EDT" VMS
min(fhier_dates$trip_start_date_time)

max(fhier_dates$trip_end_date_time)
# [1] "2023-06-13 16:00:00 EDT" VMS
min(fhier_dates$trip_end_date_time)
# [1] "1969-08-17 12:30:00 EDT" VMS

fhier_dates %>%
  filter(trip_start_date_time < "2022-01-01" |
           trip_start_date_time > "2023-04-01") %>%
  head(1)
# 1 VMS

fhier_dates %>%
  filter(trip_end_date_time < "2022-01-01" |
           trip_end_date_time > "2023-04-01")  %>%
  head()
# 34 VMS
# 0

### wrong dates to csv ----

# logbooks_content_transmission_date_not_vms_ok %>%
#   filter(
#     trip_start_date_time < "2022-01-01" |
#       trip_start_date_time > "2023-04-01" |
#       trip_end_date_time < "2022-01-01" |
#       trip_end_date_time > "2023-04-01"
#   ) %>% head()
# %>%
#   write.csv(file = "fhier_logbooks_wrong_dates.csv", row.names = F)

## region is not SA or GOM ----

logbooks_content_transmission_date_not_vms_ok %>%
  filter(!(end_port_sa_gom %in% c("sa", "gom"))) %>%
  dim()
# Rows: 112
# Rows: 188 with VMS

### not_specified_region_states ----

not_specified_region <-
  logbooks_content_transmission_date_not_vms_ok %>%
  filter(!(end_port_sa_gom %in% c("sa", "gom")))

not_specified_region_states <-
  not_specified_region %>%
  select(
    vessel_official_nbr,
    trip_id,
    trip_de,
    trip_de_ct,
    sero_vessel_permit,
    accsp_permit_license_nbr,
    vendor_app_name,
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

dim(not_specified_region_states)
# [1] 112  25

### Specifics ----
not_specified_region_states_not_monroe <-
  not_specified_region_states %>%
  filter(end_port_county == "NOT-SPECIFIED" &
           start_port_county != "MONROE")

not_specified_region_states_not_monroe %>%
  filter(trip_de_ct >= "2022-01-01") %>%
  select(
    vessel_official_nbr,
    sero_vessel_permit,
    accsp_permit_license_nbr,
    vendor_app_name,
    state_name,
    end_port,
    start_port_name
  ) %>%
  unique() %>% glimpse()
# vessel_official_nbr     : "FL2949RP"
# sero_vessel_permit      : "277056"
# accsp_permit_license_nbr: "608982"
# vendor_app_name         : "ETRIPS/MOBILE 2"
# state_name              : "FLORIDA"
# end_port                : "100999"
# start_port_name         : "FLORIDA(STATE)"

### CSV: region is not SA or GOM ----

logbooks_content_transmission_date_not_vms_ok %>%
  filter(
    state_name == 'FLORIDA' &
      !(end_port_sa_gom %in% c("sa", "gom")) &
      # transpmission date
      (trip_de_ct >= "2022-01-01") &
      # has a SERO permit
      !is.na(sero_vessel_permit)
  ) %>%
  # filter(!all(is.na(.))) %>%
  #   Rows: 112
  # Columns: 159
  # filter(complete.cases(.)) %>%
  # Rows: 0
  unique() %>%
#   dim()
# [1] 112 160
write_csv(file = "~\\fhier_logbooks_no_fl_county_not_VMS.csv")

## spp. is 0 ----
logbooks_content_transmission_date_not_vms_ok %>%
  filter(!!sym(itis_field_name) == "0") %>%
  dim()
# Rows: 0
# Rows: 89 with VMS

# grep("common", names(logbooks_content), value = T)
# common_name

logbooks_content_transmission_date_not_vms_ok %>%
  filter(!!sym(itis_field_name) == "0") %>%
  select(common_name) %>% unique()
# NA VMS

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  select(trip_start_date) %>% unique() %>% 
  dim()
# 70

logbooks_content_transmission_date_not_vms_ok %>%
  filter(!!sym(itis_field_name) == "0") %>%
  dim()
# 0
  # glimpse()
# A tibble: 89 × 151 with VMS
# write.csv(file = "logbooks_content_sp0.csv", row.names = F)

# logbooks_content %>%
#   filter(!!sym(itis_field_name) == "0") %>%
#   # head()
#   select(vessel_official_nbr) %>% unique() %>%
#   dim()
# 11