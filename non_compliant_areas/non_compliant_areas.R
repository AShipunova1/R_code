# Non compliant SA only permitted vessels (2022) by home port
# identifying any particular areas of high non-compliance to help focus future outreach efforts. 
# do this as a map

# home port from the permit as an area

# source the usual setup 
# get data
# remove not in metrics
# separate by permit region
# remove not in Jeannette's SA list
# remove not in Jeannette's GOM list
# add home port

library(maps)
library(mapdata)

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)
library(leafpop)
library(leaflet)

if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")
  library(tidygeocoder)
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_path <- this.path::this.dir()

current_project_dir_name <- basename(current_project_dir_path)

# prepare data ----
get_data_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_get_data.R")

source(get_data_file_path)

# vessels_permits_home_port_lat_longs_city_state |> dim()
# [1] 4729    6

## prepare permit data ---- 
### check for duplicate vessels ----
vessels_permits_home_port_lat_longs_city_state |>
  distinct() |>
  group_by(SERO_OFFICIAL_NUMBER) %>%
  filter(n() > 1) |>
  dim()
# 0

### check how many coords have more than one vessel ----
vessels_permits_home_port_lat_longs_city_state |>
  distinct() |>
#   group_by(permit_sa_gom, lat, long) %>%
# [1] 4393    6
  group_by(lat, long) %>%
  filter(n() > 1) |>
  dim()
# [1] num of SERO_OFFICIAL_NUMBER    6
# [1] 4505    6

## add counts to vessel_permit ----
# permit group info is different with that in compliance info!
# Adding a count column with num of SERO_OFFICIAL_NUMBER based on permit type, latitude, and longitude to the data frame.

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port <-
  vessels_permits_home_port_lat_longs_city_state |>
  dplyr::add_count(permit_sa_gom,
                   lat,
                   long,
                   name = "cnt_vsl_by_permit_n_port_coord")

### check counts ----
vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         lat,
         long) |>
  count(permit_sa_gom,
        lat,
        long) |>
  filter(permit_sa_gom == "sa_only") |>
  arrange(desc(n)) |>
  head()

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  filter(permit_sa_gom == "sa_only") |>
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  arrange(desc(cnt_vsl_by_permit_n_port_coord)) |>
  head()

## Compliance info, if a vessel is non compliant even once - it is non compliant the whole year, keep only unique vessel ids ----
compl_err_db_data_metrics_permit_reg_list_short_year_nc <- 
  compl_err_db_data_metrics_permit_reg_list_short |> 
  map(\(curr_df) {
    curr_df |>
      group_by(vessel_official_nbr) |> 
      mutate(non_compl_year = 0 %in% is_comp) |> 
      ungroup()
    })

## check compl_year ----
compl_err_db_data_metrics_permit_reg_list_short_year_nc$sa_only |>
  # filter(vessel_official_nbr == 1020822) |>
  arrange(vessel_official_nbr) |> 
  head(4)
#   vessel_official_nbr is_comp non_compl_year
#   <chr>                 <int> <lgl>         
# 1 1000164                   0 TRUE          
# 2 1020057                   1 FALSE         
# 3 1020822                   1 TRUE          
# 4 1020822                   0 TRUE          

## keep unique vessel ids only ----
compl_err_db_data_metrics_permit_reg_list_short_uniq <- 
  compl_err_db_data_metrics_permit_reg_list_short_year_nc |> 
  map(\(curr_df){
    curr_df |> 
      select(-is_comp) |> 
      distinct()
  })

# check
compl_err_db_data_metrics_permit_reg_list_short_uniq$sa_only |>
  filter(vessel_official_nbr == 1020822)
#   vessel_official_nbr non_compl_year
# 3 1020822             TRUE          

# Join home port and compliance info by vessel ----

# In summary, this code applies a left join operation to each data frame in the 'compl_err_db_data_metrics_permit_reg_list_short' list with another data frame, and the result is stored in a new list named 'vessels_permits_home_port_22_compliance_list'. The join is based on the equality of the columns 'vessel_official_nbr' and 'SERO_OFFICIAL_NUMBER'. The map function is used to apply this left join operation to each element of the list.

vessels_permits_home_port_22_compliance_list <-
  compl_err_db_data_metrics_permit_reg_list_short_uniq |>
  map(\(curr_df) {
    dplyr::left_join(
      curr_df,
      vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port,
      join_by(vessel_official_nbr == SERO_OFFICIAL_NUMBER)
    )
  })

map(vessels_permits_home_port_22_compliance_list, count_uniq_by_column)
# $dual
# vessel_official_nbr            374
# is_comp                          2

# $gom_only
# vessel_official_nbr            939
# is_comp                          2

# $sa_only
# vessel_official_nbr            2135
# is_comp                           2

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

## Count by state name ----
### total vsls per state 
vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd <-
  vessels_permits_home_port_22_compliance_list_cnt_tot |>
  map(\(curr_df) {
    curr_df |>
      group_by(state_fixed) |>
      add_count(name = "total_vsls_per_state_fxd",
                wt = cnt_vsl_by_port_coord_n_compl) |>
      ungroup() |> 
      group_by(state_fixed, non_compl_year) |>
      add_count(name = "tot_compl_per_state_fxd",
                wt = cnt_vsl_by_port_coord_n_compl) |>
      mutate(compl_percent_per_st = 
               tot_compl_per_state_fxd * 100 /
               total_vsls_per_state_fxd) |> 
      ungroup()
  })

# check
vessels_permits_home_port_22_compliance_list_cnt_tot$sa_only |>
  select(state_fixed,
         cnt_vsl_by_port_coord_n_compl) |>
  filter(state_fixed %in% c("GA")) |>
  mutate(sum(cnt_vsl_by_port_coord_n_compl)) |> 
  select(-cnt_vsl_by_port_coord_n_compl) |> 
  distinct()
# state_fixed `sum(total_vsl_per_place_perm)`
# < chr >                                 < int >
#  GA                                       38

vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd$sa_only |>
  select(non_compl_year,
         state_fixed, 
         total_vsls_per_state_fxd,
         compl_percent_per_st) |> 
# filter(state_fixed %in% c("GA")) |> 
  arrange(state_fixed, non_compl_year) |> 
  distinct()
#   state_fixed total_places_per_state_fxd
#   <chr>                            <int>
# 1 FL                                 158
# 2 NC                                  86
# 3 SC                                  34
# 4 GA                                  20

 #   non_compl_year state_fixed total_vsls_per_state_fxd compl_percent_per_st
 #   <lgl>          <chr>                          <int>                <dbl>
 # 1 TRUE           AK                                 2                100  
 # 2 FALSE          AL                                 1                100  
 # 3 TRUE           CT                                 1                100  
 # 4 FALSE          DE                                36                 30.6
 # 5 TRUE           DE                                36                 69.4
 # 6 FALSE          FL                              1701                 40.4
 # 7 TRUE           FL                              1701                 59.6
 # 8 FALSE          GA                                38                 57.9
 # 9 TRUE           GA                                38                 42.1

vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd$sa_only |>   
  filter(state_fixed %in% c("GA")) |>
  View()

# [1] "non_compl_year, permit_sa_gom, city_fixed, state_fixed, lat, long, cnt_vsl_by_permit_n_port_coord, cnt_vsl_by_port_coord_n_compl, total_vsl_per_place_perm, total_vsls_per_state_fxd, tot_compl_per_state_fxd, compl_percent_per_st"

### remove extra columns ----
vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd_short <- 
  vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd |>
  map(\(curr_df) {
    curr_df |>
      select(non_compl_year,
             state_fixed,
             tot_compl_per_state_fxd,
             compl_percent_per_st) |> 
      distinct()
  })

vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd_short$sa_only |> 
  glimpse()

# TRUE
# FL
# 6338
# 63.380000

# FALSE
# FL
# 3662
# 36.620000

# If use state names, there are typos. Either fix, or use coordinates

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

### count total_places_per_state ----
vessels_permits_home_port_22_compliance_list_cnt_tot_sf_join_states_places <-
  vessels_permits_home_port_22_compliance_list_cnt_tot_sf_join_states |>
  map(\(curr_df) {
    curr_df |>
      group_by(STUSPS) |>
      mutate(total_places_per_state = n()) |>
      ungroup()
  })

#### compare with to by state fixed ----
vessels_permits_home_port_22_compliance_list_cnt_tot_sf_join_states_places$sa_only |>
  select(STUSPS, total_places_per_state) |>
  sf::st_drop_geometry() |> 
  filter(STUSPS %in% c("FL", "NC", "SC", "GA")) |>
  distinct()
#   STUSPS total_places_per_state
#   <chr>                   <int>
# 1 FL                        153
# 2 NC                         81
# 3 SC                         31
# 4 GA                         20

#   state_fixed total_places_per_state_fxd
#   <chr>                            <int>
# 1 FL                                 158
# 2 NC                                  86
# 3 SC                                  34
# 4 GA                                  20

# combine fixed states with percentage ----
vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd_short |> 
  map(\(curr_df) {
    curr_df |>
      south_east_coast_states_shp
  })

print_df_names(vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd_short$sa_only)

shp_file_with_cnts_sa <-
  south_east_coast_states_shp |>
  left_join(
    vessels_permits_home_port_22_compliance_list_cnt_tot_cnt_tot_vsl_per_state_fxd_short$sa_only,
    join_by(STUSPS ==
              state_fixed)
  )

# [1] "non_compl_year, state_fixed, tot_compl_per_state_fxd, compl_percent_per_st"

shp_file_with_cnts_sa |> 
  filter(non_compl_year == TRUE) |>
  mutate(nc_round_perc = round(compl_percent_per_st)) |> 
  mapview(zcol = "nc_round_perc")
# View(shp_file_with_cnts)

# ggplot(crimes_long, aes(map_id = state)) +
#   geom_map(aes(fill = value), map = states_map) +
#   coord_sf(
#     crs = 5070,
#     default_crs = 4326,
#     xlim = c(-125, -70),
#     ylim = c(25, 52)
#   )
# shp_file_with_cnts_sa |> 
#   filter(non_compl_year == TRUE) |>
#   mutate(nc_round_perc = round(compl_percent_per_st)) |> 
#   mapview(zcol = "nc_round_perc")

# empty_map <-
#   ggplot() + geom_sf(
#     data = south_east_coast_states_shp,
#     # aes(x = long, y = lat, group = group),
#     colour = "black",
#     fill = NA
#   )


usa_map <- map_data("usa")
# data_overview(usa_map)
usa_map_img <-
  ggplot() +
  geom_polygon(
    data = usa_map,
    aes(x = long,
        y = lat,
        group = group),
    colour = "grey",
    fill = NA
  ) +
  coord_fixed(1.3)

usa_map_img

# cnts_map <- 
  shp_file_with_cnts_sa |> 
  filter(non_compl_year == TRUE) |>
  mutate(nc_round_perc = round(compl_percent_per_st)) |> 
  # ggplot() +
  ggplot(aes(fill = nc_round_perc)) +
  geom_sf()
  
  
  geom_map(
    data = usa_map,
    # aes(x = long,
        # y = lat,
        # group = group),
    colour = "grey",
    fill = NA
  ) +
  coord_fixed(1.3)

cnts_map 

# ex
# get south states only
states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# add X and Y
states_sf <- cbind(states_sf, st_coordinates(st_centroid(states)))

print_df_names(shp_file_with_cnts_sa)

shp_file_with_cnts_sa |>
  filter(non_compl_year == TRUE) |>
  mutate(
    nc_round_perc = round(compl_percent_per_st),
    my_label =
      stringr::str_glue("{STUSPS}:
                             {nc_round_perc}% of {tot_compl_per_state_fxd}")
  ) |>
ggplot() +
  geom_sf(aes(fill = nc_round_perc)) +
  geom_sf_label(
    aes(label = my_label),
    size = 3
  ) +
  # geom_sf_text(
  #   aes(label = my_label),
  #   colour = "purple",
  #   size = 3
  # ) +
  geom_sf(data = states_sf, fill = NA) +
  # geom_text(data = states_sf, aes(X, Y, label = ID), size = 3) +
  coord_sf(xlim = c(-107, -75),
           ylim = c(24, 37),
           expand = FALSE)

# south_east_coast_states_shp_bb


# old ----
# Percent of (non)compliant by port ----
# Adding new columns to the data frame:
# 
# non_comp_perc: Non-compliance percentage calculated as the ratio of non-compliant vessels to total vessels.
# is_comp_perc_round: Rounded percentage of non-compliance. No digits after the decimal point.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt |>
  dplyr::group_by(is_compliant_in_22) |>
  dplyr::mutate(
    non_comp_perc =
      cnt_vsl_by_port_coord_n_compl * 100 /
      cnt_vsl_by_permit_n_port_coord,
    is_comp_perc_round =
      round(non_comp_perc)
  ) |>
  dplyr::ungroup()

glimpse(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc)
# [1] 3388   11

# spot test counts ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |> 
  filter(city_fixed == "SEBASTIAN") |> 
  data_overview()
# SERO_OFFICIAL_NUMBER 27
# is_comp               2

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |> 
  filter(city_fixed == "SEBASTIAN") |> 
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  glimpse()
# $ is_compliant_in_22               <chr> "NO", "YES"
# $ cnt_vsl_by_port_coord_n_compl <int> 15, 12
# $ is_comp_perc_round               <dbl> 55.6, 44.4

## check multiple names for the same cooordinates ---- 
# mult_names <- "AMELIA"
# BAYOU LA BATRE
mult_names <- "BATRE"
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  filter(grepl(mult_names, city_fixed)) |>
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |>
  glimpse()
# $ SERO_OFFICIAL_NUMBER             <chr> "938369", "FL3307AE"
# $ city_fixed                       <chr> "AMELIA IS", "AMELIA ISLAND"
# $ cnt_vsl_by_permit_n_port_coord   <int> 2, 2


# $ city_fixed                       <chr> "BAYOU LA BATRE", "BAYOU  LA BATRE"…
# $ state_fixed                      <chr> "AL", "AL", "LA"
# $ cnt_vsl_by_permit_n_port_coord   <int> 103, 103, 103


# Count a proportion of non compliant vessels per total vessels, home ports and states ----
# color different states accordingly
