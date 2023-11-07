
##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)

# include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()



##| echo: false
# get data
source("~/R_code_github/compare_catch/get_data.R")

## FHIER data

### Transmission year
# (1) check the transmission year. Anything sent in 2021 we ignore, since it was a testing year for approving apps.
# ```{r Transmission year}

grep("transm", names(logbooks_content), value = T)

logbooks_content_transmission_date_ct <-
  logbooks_content %>%
  mutate(transmission_date_ct = as.POSIXct(transmission_date,
                                           format = "%Y-%m-%d %H:%M:%S"))

#### "start" names ----
grep("start",
     names(logbooks_content_transmission_date_ct),
     value = T)

logbooks_content_dates_ct <-
  logbooks_content_transmission_date_ct %>%
  mutate(
    trip_start_date_ct = as.POSIXct(trip_start_date, format = "%Y-%m-%d %H:%M:%S"),
    trip_end_date_ct = as.POSIXct(trip_end_date, format = "%Y-%m-%d %H:%M:%S")
  )

transmission_date_ok <-
  logbooks_content_dates_ct %>%
  dplyr::filter(
    trip_start_date < "2022-01-01" |
      trip_start_date > "2023-04-01" |
      trip_end_date < "2022-01-01" |
      trip_end_date > "2023-04-01"
  ) %>%
  dplyr::filter(transmission_date_ct > "2022-01-01") %>%
  dplyr::select(-`1`)

# transmission_date_ok %>%
# dplyr::select(trip_start_date, trip_end_date, transmission_date) %>% 
#   unique() %>%
#   head()
# glimpse()
# Rows: 20
# write.csv(file = "fhier_logbooks_wrong_dates1.csv", row.names = F)

### Get field names into variables
# There are different formats in different available files.
# Find a column name with "itis" in it

logbooks_content_transmission_date_ok <- transmission_date_ok
itis_field_name <- grep("itis", names(logbooks_content_transmission_date_ok), value = T)
# catch_species_itis

# Same for "vessel.*official"
vessel_id_field_name <-
  grep("vessel.*official", names(logbooks_content_transmission_date_ok), value = T)
# vessel_official_nbr


### Clean logbooks_content

fhier_logbooks_content <-
  logbooks_content_transmission_date_ok  %>%
  # create a new column
  mutate(trip_start_date_time =
    # trip start: combine a date without time, a space and a time
    paste(substr(trip_start_date, 1, 10),
    trip_start_time)) %>%
  # Same for the trip end
  mutate(trip_end_date_time = paste(substr(trip_end_date, 1, 10), trip_end_time)) %>%
  # change the new column types to a date
  change_to_dates("trip_start_date_time", "%Y-%m-%d %H%M") %>%
  change_to_dates("trip_end_date_time", "%Y-%m-%d %H%M") %>%
  # change the column type to a number
  mutate(reported_quantity = as.integer(reported_quantity))

# head
fhier_logbooks_content %>% dplyr::select(starts_with("trip")) %>% str()

#### Fix typos

fhier_logbooks_content_date_fixed_tmp <-
  fhier_logbooks_content %>%
  # if a "trip_end_date" is before 2020 - use "notif_trip_end_date" column instead
  mutate(trip_end_date1 = ifelse(
    trip_end_date < "2020-01-01",
    notif_trip_end_date,
    trip_end_date
  ))

fhier_logbooks_content_date_fixed <-
  fhier_logbooks_content_date_fixed_tmp %>%
  # manually change the wrong value
  mutate(trip_end_date2 = ifelse(
    # find it
    grepl("1992", fhier_logbooks_content_date_fixed_tmp$trip_end_date1),
    # change it
    "2022-10-16 01:00:00",
    # don't change anything else
    trip_end_date1
  ))

#### Use only 2022 data

fhier_logbooks_content_date_fixed %<>%
  dplyr::filter(year(trip_end_date) == "2022")

### Add waves

fhier_logbooks_content_waves <-
  fhier_logbooks_content_date_fixed %>% glimpse()
  # add a new column with a trip end Month
  mutate(end_month = as.yearmon(trip_end_date2)) %>%
  # add a new column with a trip end Year
  mutate(end_year =
           year(trip_end_date2)) %>%
  # add a new column with a number for each trip end Month
  mutate(end_month_num = month(trip_end_date2)) %>%
  # add a new column with a Wave
  mutate(end_wave  = floor((end_month_num + 1) / 2))

### FL county to region

# Florida counties by region (from the Internet)
fl_counties <- list(
  "SA" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "GOM" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  )
)

fhier_logbooks_content_waves_fl_county <-
  fhier_logbooks_content_waves %>%
  # create a new column "end_port_fl_reg" with SA, GOM or whatever else left
  mutate(
    end_port_fl_reg = case_when(
      # check in the list
      fix_names(end_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(end_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      # if not on the list - keep it
      .default = end_port_county
    )
  )

#### Not GOM or SA fhier_logbooks_content_waves_fl_county

fhier_logbooks_content_waves_fl_county %>%
  # get FL only
  dplyr::filter(end_port_state == "FL") %>%
  # sort by county
  arrange(end_port_county) %>%
  distinct() %>%
  # 37 counties
  dplyr::select(end_port_fl_reg) %>%
  # what else is in the new column beside sa and gom
  dplyr::filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>% unique()

# NOT-SPECIFIED

### States to GOM or SA

# list of states in the South Atlantic region (from the Internet)
states_sa <- data.frame(
  state_name = c(
    "Delaware",
    "District of Columbia",
    # "Florida", # exclude, we have it separated by county
    "Georgia",
    "Maryland",
    "North Carolina",
    "South Carolina",
    "Virginia",
    "West Virginia"
  )
)

#### Get state abbreviations
sa_state_abb <-
  # a default R table
  state_tbl %>%
  # get only these in our list
  dplyr::filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  dplyr::select(state_abb)
#### Add sa/gom to states

fhier_logbooks_content_waves__sa_gom <-
  fhier_logbooks_content_waves_fl_county %>%
  # add a new column "end_port_sa_gom" with sa or gom for each state
  # use fix_name aux function to unify state names (lower case, no spaces etc.)
  mutate(end_port_sa_gom = case_when(
    # if a name is in our SA list - "sa", otherwise - "gom"
    fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
    .default = "gom"
  )) %>%
  # go through the new column again
  # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
  # otherwise don't change
  mutate(end_port_sa_gom = ifelse(
    tolower(end_port_state) == "fl",
    end_port_fl_reg,
    end_port_sa_gom
  )) %>%
  # remove this column, we don't need it anymore
  dplyr::select(-end_port_fl_reg)



glimpse(logbooks_content)




fhier_dates <-
  fhier_logbooks_content %>%
  dplyr::select(grep("date", names(fhier_logbooks_content), value = T))

max(fhier_dates$trip_start_date_time)
# [1] "2023-06-13 08:00:00 EDT"
min(fhier_dates$trip_start_date_time)

max(fhier_dates$trip_end_date_time)
# [1] "2023-06-13 16:00:00 EDT"
min(fhier_dates$trip_end_date_time)
# [1] "1969-08-17 12:30:00 EDT"

fhier_dates %>%
  dplyr::filter(trip_start_date_time < "2022-01-01" |
           trip_start_date_time > "2023-04-01") %>%
  head(1)
# 1

fhier_dates %>%
  dplyr::filter(trip_end_date_time < "2022-01-01" |
           trip_end_date_time > "2023-04-01")  %>%
  head()
# 34




fhier_logbooks_content %>%
  dplyr::filter(
    trip_start_date_time < "2022-01-01" |
      trip_start_date_time > "2023-04-01" |
      trip_end_date_time < "2022-01-01" |
      trip_end_date_time > "2023-04-01"
  ) %>% head()
# %>%
#   write.csv(file = "fhier_logbooks_wrong_dates.csv", row.names = F)




# glimpse(fhier_logbooks_content_waves__sa_gom)

fhier_logbooks_content_waves__sa_gom %>%
  dplyr::filter(!(end_port_sa_gom %in% c("sa", "gom"))) %>%
  glimpse()
# Rows: 188



not_specified_region <-
  fhier_logbooks_content_waves__sa_gom %>%
  dplyr::filter(!(end_port_sa_gom %in% c("sa", "gom")))

not_specified_region_states <-
  not_specified_region %>%
  dplyr::select(
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

head(not_specified_region_states)





not_specified_region_states_not_monroe <-
  not_specified_region_states %>%
  dplyr::filter(end_port_county == "NOT-SPECIFIED" &
           start_port_county != "MONROE")

not_specified_region_states_not_monroe %>%
  dplyr::select(vessel_official_nbr) %>% unique()
  # 1244719
  # glimpse()
not_specified_region_states_not_monroe %>%
  dplyr::select(state_name) %>% unique()
  # FLORIDA

not_specified_region_states_not_monroe %>%
  dplyr::select(end_port) %>% unique()
  # 100999

not_specified_region_states_not_monroe %>%
  dplyr::select(start_port_name) %>% unique()
# FLORIDA(STATE)





fhier_logbooks_content_waves_fl_county %>%
  dplyr::filter(state_name == 'FLORIDA') %>%
  dplyr::filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>%
  dplyr::select(-c(`1`)) %>%
  # dplyr::filter(!all(is.na(.))) %>%
  #   Rows: 112
  # Columns: 159
  # dplyr::filter(complete.cases(.)) %>%
  # Rows: 0
  unique() %>%
  glimpse()
  # write.csv(file = "fhier_logbooks_no_fl_county.csv", row.names = F)
  



## spp. is 0 ----
logbooks_content %>%
  dplyr::filter(!!sym(itis_field_name) == "0") %>%
  glimpse()
# Rows: 89

# grep("common", names(logbooks_content), value = T)
# common_name

logbooks_content %>%
  dplyr::filter(!!sym(itis_field_name) == "0") %>%
  dplyr::select(common_name) %>% unique()
# NA

logbooks_content %>%
  dplyr::filter(!!sym(itis_field_name) == "0") %>%
  dplyr::select(trip_start_date) %>% unique()
# 70

logbooks_content %>%
  dplyr::filter(!!sym(itis_field_name) == "0") %>%
  glimpse()
# A tibble: 89 × 151
# write.csv(file = "logbooks_content_sp0.csv", row.names = F)

logbooks_content %>%
  dplyr::filter(!!sym(itis_field_name) == "0") %>%
  # head()
  dplyr::select(vessel_official_nbr) %>% unique()



fhier_logbooks_content_waves_fl_county %>%
  dplyr::select(starts_with("notif"), user_app) %>%
  unique() %>%
  # Rows: 47,242
  # Columns: 33
  #   dplyr::filter(complete.cases(.)) %>%
  # 0
  glimpse()

# user_app, system
grep("sta", names(not_specified_region), value = T)

grep("accsp", names(not_specified_region), value = T)

not_specified_region %>%
  dplyr::filter(state_name == "FLORIDA") %>%
  dplyr::select(starts_with("notif"),
         user_app,
         accsp_permit_license_nbr) %>%
  unique() %>%
  head()

