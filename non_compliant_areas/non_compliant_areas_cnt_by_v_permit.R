# count vessels by home_port and compliance ----

# Adding a count column named cnt_vsl_by_port_coord_n_compl based on the variables lat, long, and is_compliant_in_22 using the dplyr::add_count function.

vessels_permits_home_port_22_compliance_list_cnt <-
  vessels_permits_home_port_22_compliance_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::add_count(lat,
                       long,
                       non_compl_year,
                       name = "cnt_vsl_by_port_coord_n_compl")
  })

## check cnts ----
test_head_0 <-   
  vessels_permits_home_port_22_compliance_list_cnt$sa_only |> 
  select(vessel_official_nbr,
         lat,
         long,
         non_compl_year,
         cnt_vsl_by_port_coord_n_compl) |> 
  distinct() |> 
  arrange(vessel_official_nbr) |> 
  head()

test_head_1 <- 
  vessels_permits_home_port_22_compliance_list$sa_only |> 
  select(vessel_official_nbr,
         lat,
         long,
         non_compl_year) |> 
  distinct() |> 
  add_count(lat,
         long,
         non_compl_year,
         name = "cnt_vsl_by_port_coord_n_compl") |> 
  arrange(vessel_official_nbr) |> 
  head()

all.equal(test_head_0, test_head_1)
# T

test_3 <- 
  vessels_permits_home_port_22_compliance_list$sa_only |>
  filter(vessel_official_nbr == 1020822) |>
  mutate(round_lat = round(lat, 4),
         round_long = round(long, 4))

vessels_permits_home_port_22_compliance_list$sa_only |>
  filter(round(lat, 4) == test_3$round_lat[[1]] &
           round(long, 4) == test_3$round_long[[1]]) |>
  glimpse()

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  filter(
    cnt_vsl_by_permit_n_port_coord == 22 &
      round(lat, 4) == test_3$round_lat[[1]] &
      round(long, 4) == test_3$round_long[[1]]
  ) |>
  glimpse()

# cnt_vsl_by_permit_n_port_coord == 22 is correct for permit info from vessel_permits, 
# but wrong for compliant permit information (7 sa_only vessel_official_nbr)

vessels_permits_home_port_22_compliance_list |>
  map(\(curr_df) {
    curr_df |>
      filter(round(lat, 4) == test_3$round_lat[[1]] &
               round(long, 4) == test_3$round_long[[1]]) |> 
      dim()
  })
# $sa_only
# [1] 7 8
# 7 vessels, correct

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
        filter(round(lat, 4) == test_3$round_lat[[1]] &
               round(long, 4) == test_3$round_long[[1]]) |> 
glimpse()

vessels_permits_home_port_22_compliance_list_cnt$sa_only |> 
      filter(round(lat, 4) == test_3$round_lat[[1]] &
               round(long, 4) == test_3$round_long[[1]]) |> 
  select(non_compl_year, cnt_vsl_by_port_coord_n_compl) |> 
  distinct()
# 1 TRUE                                          5
# 2 FALSE                                         2

# add total vessel num per place by permit region in compl data ----
vessels_permits_home_port_22_compliance_list_cnt_tot <-
  vessels_permits_home_port_22_compliance_list_cnt |>
  map(\(curr_df) {
    curr_df |>
      select(-vessel_official_nbr) |>
      distinct() |>
      group_by(lat, long) |> 
      dplyr::add_count(wt = cnt_vsl_by_port_coord_n_compl,
                       name = "total_vsl_per_place_perm") |> 
      ungroup()
  })

# check
vessels_permits_home_port_22_compliance_list_cnt_tot$sa_only |>
  filter(state_fixed == "GA") |>
  select(-c(permit_sa_gom, city_fixed, state_fixed)) |> 
  distinct() |> 
  glimpse()
# Rows: 20

vessels_permits_home_port_22_compliance_list_cnt_tot$sa_only |>
  filter(state_fixed == "GA") |>
  select(-c(permit_sa_gom, city_fixed, state_fixed,
            lat,
            long,
            cnt_vsl_by_permit_n_port_coord)) |>
  distinct() |>
  # glimpse()
  group_by(non_compl_year) |>
    count(wt = cnt_vsl_by_port_coord_n_compl)
# Rows: 16
#   non_compl_year     n
#   <lgl>          <int>
# 1 FALSE             20
# 2 TRUE              14

# check
vessels_permits_home_port_22_compliance_list_cnt_tot$sa_only |> 
      filter(round(lat, 4) == test_3$round_lat[[1]] &
               round(long, 4) == test_3$round_long[[1]]) |> 
  glimpse()
# $ non_compl_year                 <lgl> TRUE, FALSE
# $ lat                            <dbl> 32.07901, 32.07901
# $ long                           <dbl> -81.09213, -81.09213
# $ cnt_vsl_by_port_coord_n_compl  <int> 5, 2
# $ total_vsl_per_place_perm       <int> 7, 7

# Percent of (non)compliant by state ----

## Count using coordinates ----
### convert to an sf ----
vessels_permits_home_port_22_compliance_list_cnt_tot_sf <-
  vessels_permits_home_port_22_compliance_list_cnt_tot |>
  map(\(curr_df) {
    curr_df |> 
      filter(!is.na(long) &
               !is.na(lat)) |> 
      sf::st_as_sf(coords = c("long", "lat"), crs = tigris_crs)
  })

map(vessels_permits_home_port_22_compliance_list_cnt_tot_sf, dim)
# $dual
# [1] 162   8
# 
# $gom_only
# [1] 188   8
# 
# $sa_only
# [1] 387   8

### Join with the state map ----
vessels_permits_home_port_22_compliance_list_cnt_tot_sf_join_states <-
  vessels_permits_home_port_22_compliance_list_cnt_tot_sf |>
  map(\(curr_df) {
    curr_df |>
      sf::st_join(south_east_coast_states_shp, left = FALSE) %>%
      
      # extract the longitude and latitude coordinates from the joined spatial object.
      dplyr::mutate(
        longitude = sf::st_coordinates(.)[, 1],
        latitude = sf::st_coordinates(.)[, 2]
      )
    
  })

# print_df_names(vessels_permits_home_port_22_compliance_list_cnt_tot_sf_join_states$sa_only)

### check where port by coords != port from vessel info ----
vessels_permits_home_port_22_compliance_list_cnt_tot_sf_join_states |>
  map(\(curr_df) {
    curr_df |>
      filter(!STUSPS == state_fixed) |>
      select(city_fixed, state_fixed, NAME)
  })

# sa_only
#   city_fixed state_fixed NAME                       geometry
#   <chr>      <chr>       <chr>                   <POINT [°]>
# 1 OCEAN CITY DE          North Carolina (-77.49136 34.45656)
# $ cnt_vsl_by_permit_n_port_coord <int> 1

