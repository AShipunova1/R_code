# spp. is 0
# region is not SA or GOM
glimpse(logbooks_content)

# spp. is 0 ----
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

## region is not SA or GOM
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