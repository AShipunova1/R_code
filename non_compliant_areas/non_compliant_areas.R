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
# [1] 4505    6
  # count_uniq_by_column()
# SERO_OFFICIAL_NUMBER 4505
# city_fixed            376
# state_fixed            17
# lat                   323

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
#   permit_sa_gom city_fixed      state_fixed   lat  long cnt_vsl_by_permit_n_port_coord
#   <chr>         <chr>           <chr>       <dbl> <dbl>                          <int>
# 1 sa_only       KEY WEST        FL           24.6 -81.8                            139

## Compliance info, if a vessel is non compliant even once - it is non compliant the whole year, keep only unique vessel ids ----

# compl_err_db_data_metrics_permit_reg_list_short is sourced from non_compliant_areas_get_data.R
compl_err_db_data_metrics_permit_reg_list_short_year_nc <- 
  compl_err_db_data_metrics_permit_reg_list_short |> 
  map(\(curr_df) {
    # For each data frame curr_df in the list (one df for a permit_region):
    curr_df |>
      group_by(vessel_official_nbr) |> 

      mutate(non_compl_year = 0 %in% is_comp) |> 
      # Add a new column non_compl_year, which is TRUE if 0 ("is not compliant") is in the is_comp column for this vessel.
      
      ungroup()
      # Ungroup the data frame, removing the grouping structure.
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

# 1020822 is non compliant for the whole year 

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
# 1 1020822             TRUE          

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
# non_compl_year                   2

# $gom_only
# vessel_official_nbr            939
# non_compl_year                   2

# $sa_only
# vessel_official_nbr            2135
# non_compl_year                    2

# Count vessels by state name ----
## total vsls per state ----

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt <-
  vessels_permits_home_port_22_compliance_list |>
  map(\(curr_df) {
    curr_df |>
      group_by(state_fixed) |>
      add_count(state_fixed,
                name = "total_vsl_by_state_cnt") |> 
      ungroup()
  })

# View(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt)

### cnt by state prove of concept ----

# vessels_permits_home_port_22_compliance_list$sa_only |> 
#   glimpse()

vessel_by_state_cnt <-
  vessels_permits_home_port_22_compliance_list |>
  map(\(curr_df) {
    curr_df |>
      select(vessel_official_nbr, state_fixed) |>
      distinct() |>
      # group_by(state_fixed) |>
      add_count(state_fixed)
  })

# View(vessel_by_state_cnt)

vessel_by_state_cnt1 <-
  vessels_permits_home_port_22_compliance_list |>
  map(\(curr_df) {
    curr_df |>
      group_by(state_fixed) |>
      add_count(state_fixed) |> 
      select(vessel_official_nbr, state_fixed, n) |>
      distinct()
  })

diffdf::diffdf(vessel_by_state_cnt$sa_only, vessel_by_state_cnt1$sa_only)
# T

head(vessel_by_state_cnt$sa_only)
head(vessel_by_state_cnt1$sa_only)

# cnt vessel by state and compliance ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt <- 
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt |>
  map(\(curr_df) {
    curr_df |>
      group_by(state_fixed, non_compl_year) |>
      add_count(state_fixed, non_compl_year,
                name = "compliance_by_state_cnt") |> 
      ungroup()
  })

## check non_compl counts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$sa_only |> 
  select(vessel_official_nbr, state_fixed, non_compl_year, total_vsl_by_state_cnt, compliance_by_state_cnt) |>
  distinct() |>
  select(-vessel_official_nbr) |> 
  distinct() |> 
  filter(state_fixed %in% c("FL", "GA"))
# state_fixed non_compl_year total_vsl_by_state_cnt compliance_by_state_cnt
#  <chr>  <lgl>  <int>  <int>
# 1 FL  TRUE   896  500
# 2 FL  FALSE  896  396
# 3 GA  TRUE    38   16
# 4 GA  FALSE   38   22

## test on one df ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$sa_only |>
  glimpse()

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$sa_only |>
  select(vessel_official_nbr, state_fixed, non_compl_year, total_vsl_by_state_cnt) |>
  distinct() |>
  add_count(state_fixed, non_compl_year) |> 
  select(-vessel_official_nbr) |> 
  distinct() |> 
  filter(state_fixed %in% c("FL", "GA"))

# percent of non compliant by state ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc <- 
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt |>
  map(\(curr_df) {
    curr_df |>
      group_by(state_fixed, non_compl_year) |>
      mutate(compl_percent_per_st =
               compliance_by_state_cnt * 100 /
               total_vsl_by_state_cnt) |>
      ungroup()
  })

## check perc cnts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc$sa_only |> 
  select(state_fixed,
         non_compl_year,
         total_vsl_by_state_cnt,
         compliance_by_state_cnt,
         compl_percent_per_st) |>
  distinct() |> 
  filter(state_fixed %in% c("FL", "GA")) |>
  glimpse()

## test on one df ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$sa_only |>
  glimpse()

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$sa_only |>
  select(state_fixed,
         non_compl_year,
         total_vsl_by_state_cnt,
         compliance_by_state_cnt) |>
  distinct() |>
  group_by(state_fixed, non_compl_year) |>
  mutate(compl_percent_per_st =
           compliance_by_state_cnt * 100 /
           total_vsl_by_state_cnt) |>
  ungroup() |> 
  filter(state_fixed %in% c("FL", "GA")) |> 
  glimpse()

# map percentage ----

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc |>
  map(\(curr_df) {
    curr_df |>
      filter(non_compl_year == TRUE) |>
      select(
        state_fixed,
        total_vsl_by_state_cnt,
        compliance_by_state_cnt,
        compl_percent_per_st
      ) |>
      distinct() |>
      mutate(
        nc_round_perc = round(compl_percent_per_st),
        my_label =
          stringr::str_glue(
            "{state_fixed}:
                             {nc_round_perc}% of {total_vsl_by_state_cnt}"
          )
      )
  })

### check the labels ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short$sa_only |> 
  glimpse()

## add to the shape file by state name ----
shp_file_with_cnts_sa <-
  south_east_coast_states_shp |>
  left_join(
    vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short$sa_only,
    join_by(STUSPS ==
              state_fixed)
  )

# print_df_names(shp_file_with_cnts_sa)
# [1] "STATEFP, STATENS, AFFGEOID, GEOID, STUSPS, NAME, LSAD, ALAND, AWATER, total_vsl_by_state_cnt, compliance_by_state_cnt, compl_percent_per_st, nc_round_perc, my_label, geometry"

# shp_file_with_cnts_sa |> 
#   mapview(zcol = "nc_round_perc")
# View(shp_file_with_cnts)

## get usa map ----
usa_map <- map_data("usa")

# data_overview(usa_map)
# usa_map_img <-
#   ggplot() +
#   geom_polygon(
#     data = usa_map,
#     aes(x = long,
#         y = lat,
#         group = group),
#     colour = "grey",
#     fill = NA
#   ) +
#   coord_fixed(1.3)

# usa_map_img

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
