# Non compliant vessels (2022) by home port
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

# Load the 'maps' and 'mapdata' libraries, which provide functionality for working with maps in R.
library(maps)
library(mapdata)

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)
# Load the 'leafpop' and 'leaflet' libraries, which are used for creating interactive maps in R.
library(leafpop)
library(leaflet)

# The tidygeocoder package makes getting data from geocoder services easy.
# Check if the 'tidygeocoder' package is already installed; if not, install it and load the library.
if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")  # Install 'tidygeocoder' package if not already installed.
  library(tidygeocoder)  # Load the 'tidygeocoder' library after installation.
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# set up common functions and get data ----
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
  dplyr::distinct() |>
  dplyr::group_by(SERO_OFFICIAL_NUMBER) %>%
  dplyr::filter(dplyr::n() > 1) |>
  dim()
# 0

### check how many coords have more than one vessel ----
vessels_permits_home_port_lat_longs_city_state |>
  dplyr::distinct() |>
#   group_by(permit_sa_gom, lat, long) %>%
# [1] 4393    6
  # Group the data by latitude and longitude, then filter for rows with more than one occurrence.
  dplyr::group_by(lat, long) %>%
  dplyr::filter(dplyr::n() > 1) |>
  # Return the dimensions (number of rows and columns) of the resulting data frame.
  dim()
# [1] 4505    6
  # count_uniq_by_column()
# SERO_OFFICIAL_NUMBER 4505
# city_fixed            376
# state_fixed            17
# lat                   323

## Compliance info combine dual and GOM ----

# compl_err_db_data_metrics_permit_reg_list_short is sourced from non_compliant_areas_get_data.R

# Combine two data frames, 'gom_only' and 'dual', from the specified list and assign it to 'gom_dual_compl'.
gom_dual_compl <-
  rbind(
    compl_err_db_data_metrics_permit_reg_list_short$gom_only,
    compl_err_db_data_metrics_permit_reg_list_short$dual
  )

# count_uniq_by_column(gom_dual_compl)
# vessel_official_nbr 1313
# is_comp                2
# dim(gom_dual_compl)
# [1] 1588    2

### check if vessels are in gom and dual ----
dplyr::intersect(compl_err_db_data_metrics_permit_reg_list_short$gom_only$vessel_official_nbr,
    compl_err_db_data_metrics_permit_reg_list_short$dual$vessel_official_nbr)
# 0 - no  

## add gom_dual_compl back to the list of dfs ----
compl_err_db_data_metrics_permit_reg_list_short$gom_dual <-
  gom_dual_compl

# View(compl_err_db_data_metrics_permit_reg_list_short) 

# Use the 'map' function from the 'purrr' package to apply the 'dim' function to each element
# of the list 'compl_err_db_data_metrics_permit_reg_list_short'.
purrr::map(compl_err_db_data_metrics_permit_reg_list_short, dim)
# $dual
# [1] 474   2
# 
# $gom_only
# [1] 1114    2
# 
# $sa_only
# [1] 2855    2
# 
# $gom_dual
# [1] 1588    2

# apply count_uniq_by_column() function to each df in the list
purrr::map(compl_err_db_data_metrics_permit_reg_list_short,
           count_uniq_by_column)
# $dual
# vessel_official_nbr 374
# is_comp               2
# 
# $gom_only
# vessel_official_nbr 939
# is_comp               2
# 
# $sa_only
# vessel_official_nbr 2135
# is_comp                2
# 
# $gom_dual
# vessel_official_nbr 1313
# is_comp                2

## Compliance info, if a vessel is non compliant even once - it is non compliant the whole year, keep only unique vessel ids ----

compl_err_db_data_metrics_permit_reg_list_short_year_nc <- 
  compl_err_db_data_metrics_permit_reg_list_short |> 
  purrr::map(\(curr_df) {
    # For each data frame curr_df in the list (one df for a permit_region):
    curr_df |>
      dplyr::group_by(vessel_official_number) |> 

      dplyr::mutate(non_compl_year = 0 %in% is_comp) |> 
      # Add a new column non_compl_year, which is TRUE if 0 ("is not compliant") is in the is_comp column for this vessel.
      
      dplyr::ungroup()
      # Ungroup the data frame, removing the grouping structure.
  })

## check compl_year ----
compl_err_db_data_metrics_permit_reg_list_short_year_nc$sa_only |>
  # filter(vessel_official_number == 1020822) |>
  dplyr::arrange(vessel_official_number) |> 
  head(4)
#   vessel_official_number is_comp non_compl_year
#   <chr>                 <int> <lgl>         
# 1 1000164                   0 TRUE          
# 2 1020057                   1 FALSE         
# 3 1020822                   1 TRUE          
# 4 1020822                   0 TRUE          

# 1020822 is non compliant for the whole year 

## keep unique vessel ids only ----
# Create a new list 'compl_err_db_data_metrics_permit_reg_list_short_uniq' by applying a series of
# operations to each element of the 'compl_err_db_data_metrics_permit_reg_list_short_year_nc' list.
compl_err_db_data_metrics_permit_reg_list_short_uniq <- 
  compl_err_db_data_metrics_permit_reg_list_short_year_nc |> 
  purrr::map(\(curr_df){
    # Select all columns except 'is_comp' and retain only distinct rows.
    curr_df |> 
      dplyr::select(-is_comp) |> 
      dplyr::distinct()
  })

# check
compl_err_db_data_metrics_permit_reg_list_short_uniq$sa_only |>
  dplyr::filter(vessel_official_number == 1020822)
#   vessel_official_number non_compl_year
# 1 1020822             TRUE          

# Join home port and compliance info by vessel ----

# In summary, this code applies a left join operation to each data frame in the 'compl_err_db_data_metrics_permit_reg_list_short' list with another data frame, and the result is stored in a new list named 'vessels_permits_home_port_22_compliance_list'. The join is based on the equality of the columns 'vessel_official_number' and 'SERO_OFFICIAL_NUMBER'. The map function is used to apply this left join operation to each element of the list.

vessels_permits_home_port_22_compliance_list <-
  compl_err_db_data_metrics_permit_reg_list_short_uniq |>
  purrr::map(\(curr_df) {
    dplyr::left_join(
      curr_df,
      vessels_permits_home_port_lat_longs_city_state,
      dplyr::join_by(vessel_official_number == SERO_OFFICIAL_NUMBER)
    )
  })

purrr::map(vessels_permits_home_port_22_compliance_list,
           count_uniq_by_column)
# $dual
# vessel_official_number            374
# non_compl_year                   2

# $gom_only
# vessel_official_number            939
# non_compl_year                   2

# $sa_only
# vessel_official_number            2135
# non_compl_year                    2

# $gom_dual
# vessel_official_number            1313
# non_compl_year                    2


# Count vessels by state name ----
## total vsls per state ----

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt <-
  vessels_permits_home_port_22_compliance_list |>
  purrr::map(\(curr_df) {
    # Group the data by 'state_fixed', add a count column, and ungroup the data.
    curr_df |>
      dplyr::group_by(state_fixed) |>
      dplyr::add_count(state_fixed,
                name = "total_vsl_by_state_cnt") |> 
      dplyr::ungroup()
  })

# View(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt)

### cnt by state prove of concept ----

# vessels_permits_home_port_22_compliance_list$sa_only |> 
#   glimpse()

#### select first, then count ----
# Create a new list 'vessel_by_state_cnt' by applying a series of operations to each element
# of the 'vessels_permits_home_port_22_compliance_list' by using map().
vessel_by_state_cnt <-
  vessels_permits_home_port_22_compliance_list |>
  purrr::map(\(curr_df) {
    # Select only 'vessel_official_number' and 'state_fixed' columns, retain distinct rows,
    # and add a count column for each state.
    curr_df |>
      dplyr::select(vessel_official_number, state_fixed) |>
      dplyr::distinct() |>
      dplyr::add_count(state_fixed)
  })

# View(vessel_by_state_cnt)

#### group_by, then count ----
# check if the result is the same with group_by()
# Group the data by 'state_fixed', add a count column, select specific columns,
# retain distinct rows based on those columns.

vessel_by_state_cnt1 <-
  vessels_permits_home_port_22_compliance_list |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(state_fixed) |>
      dplyr::add_count(state_fixed) |> 
      dplyr::select(vessel_official_number, state_fixed, n) |>
      dplyr::distinct()
  })

diffdf::diffdf(vessel_by_state_cnt$sa_only,
               vessel_by_state_cnt1$sa_only)
# T
# no difference!

head(vessel_by_state_cnt$sa_only)
head(vessel_by_state_cnt1$sa_only)

# cnt vessel by state and compliance ----

# Use map() to create a new list by applying a series of operations to each element of the
# 'vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt'.

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt |>
  purrr::map(\(curr_df) {
    # Group the data by 'state_fixed' and 'non_compl_year',
    # add a count column (counting vessel number per state and compliance), and ungroup the data.
    curr_df |>
      dplyr::group_by(state_fixed, non_compl_year) |>
      dplyr::add_count(state_fixed, non_compl_year, 
                       name = "compliance_by_state_cnt") |>
      dplyr::ungroup()
  })

## spot check if compl and non compl vessel number is equal total counts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$sa_only |> 
  dplyr::select(vessel_official_number, state_fixed, non_compl_year, total_vsl_by_state_cnt, compliance_by_state_cnt) |>
  dplyr::distinct() |>
  dplyr::select(-vessel_official_number) |> 
  dplyr::distinct() |> 
  dplyr::filter(state_fixed %in% c("FL", "GA")) |> 
  dplyr::glimpse()
# $ state_fixed             <chr> "FL", "FL", "GA", "GA"
# $ non_compl_year          <lgl> TRUE, FALSE, TRUE, FALSE
# $ total_vsl_by_state_cnt  <int> 896, 896, 38, 38
# $ compliance_by_state_cnt <int> 500, 396, 16, 22

## test counts on one input df ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$sa_only |>
  dplyr::glimpse()

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$sa_only |>
  dplyr::select(vessel_official_number,
                state_fixed,
                non_compl_year,
                total_vsl_by_state_cnt) |>
  dplyr::distinct() |>
  dplyr::add_count(state_fixed, non_compl_year) |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  dplyr::filter(state_fixed %in% c("FL", "GA")) |> 
  dplyr::glimpse()
# the result is the same as above

# percent of non compliant by state ----

# use map() to apply a series of operations to each df (permit_region) of the list
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc <- 
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt |>
  purrr::map(\(curr_df) {
    # Group the data by 'state_fixed' and 'non_compl_year',
    # calculate compliance percentage per state, and ungroup the data.
    
    curr_df |>
      dplyr::group_by(state_fixed, non_compl_year) |>
      dplyr::mutate(compl_percent_per_st =
                      compliance_by_state_cnt * 100 /
                      total_vsl_by_state_cnt) |>
      dplyr::ungroup()
  })

## check perc cnts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc$sa_only |>
  dplyr::select(
    state_fixed,
    non_compl_year,
    total_vsl_by_state_cnt,
    compliance_by_state_cnt,
    compl_percent_per_st
  ) |>
  dplyr::distinct() |>
  dplyr::filter(state_fixed %in% c("FL", "GA")) |>
  dplyr::glimpse()
# $ state_fixed             <chr> "FL", "FL", "GA", "GA"
# $ non_compl_year          <lgl> TRUE, FALSE, TRUE, FALSE
# $ total_vsl_by_state_cnt  <int> 896, 896, 38, 38
# $ compliance_by_state_cnt <int> 500, 396, 16, 22
# $ compl_percent_per_st    <dbl> 55.80357, 44.19643, 42.10526, 57.89474

## test on one df ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$sa_only |>
  dplyr::glimpse()

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$sa_only |>
  dplyr::select(state_fixed,
         non_compl_year,
         total_vsl_by_state_cnt,
         compliance_by_state_cnt) |>
  dplyr::distinct() |>
  dplyr::group_by(state_fixed, non_compl_year) |>
  dplyr::mutate(compl_percent_per_st =
           compliance_by_state_cnt * 100 /
           total_vsl_by_state_cnt) |>
  dplyr::ungroup() |> 
  dplyr::filter(state_fixed %in% c("FL", "GA")) |> 
  dplyr::glimpse()
# the result is the same

# map percentage ----

## shorten and add labels ----
# Use map() to apply the following operations to each permit_region df in the list:
# Filter for rows where a vessel was not compliant at least once in 2022 ('non_compl_year' is TRUE), 
# select specific columns,
# retain distinct rows, 
# calculate rounded compliance percentage, 
# and create a label.
  
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::filter(non_compl_year == TRUE) |>
      dplyr::select(
        state_fixed,
        total_vsl_by_state_cnt,
        compliance_by_state_cnt,
        compl_percent_per_st
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        nc_round_perc = round(compl_percent_per_st),
        my_label =
          stringr::str_glue("{state_fixed}:
                             {nc_round_perc}% of {total_vsl_by_state_cnt}")
      )
  })

### check the labels ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short$sa_only |> 
  dplyr::glimpse()

## add to the shape file by state name ----

# Left join the 'south_east_coast_states_shp' shape file data frame with the each permit region data frame from the list,
# using 'STUSPS' from the shapefile and 'state_fixed' from the current data frame.

shp_file_with_cnts_list <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short |>
  purrr::map(\(curr_df) {
    south_east_coast_states_shp |>
      dplyr::left_join(curr_df,
                       dplyr::join_by(STUSPS ==
                                        state_fixed))
  })

# shp_file_with_cnts_list$sa_only |> str()
# if join to a df:
# tibble [8 × 15] (S3: tbl_df/tbl/data.frame)
# if join to an sf:
# Classes ‘sf’ and 'data.frame':	8 obs. of  15 variables

### check on one region ----
shp_file_with_cnts_sa <-
  south_east_coast_states_shp |>
  dplyr::left_join(
    vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short$sa_only,
    dplyr::join_by(STUSPS ==
                     state_fixed)
  )

# print_df_names(shp_file_with_cnts_sa)
# [1] "STATEFP, STATENS, AFFGEOID, GEOID, STUSPS, NAME, LSAD, ALAND, AWATER, total_vsl_by_state_cnt, compliance_by_state_cnt, compl_percent_per_st, nc_round_perc, my_label, geometry"

# shp_file_with_cnts_sa |> 
#   mapview(zcol = "nc_round_perc")
# View(shp_file_with_cnts)

# shp_file_with_cnts_list$sa_only |> 
#   mapview(zcol = "nc_round_perc")

# get south states map ----
# to add empty states to the map
states_sf <-
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# add X and Y
states_sf <-
  cbind(states_sf,
        sf::st_coordinates(sf::st_centroid(states_sf)))

# mapview(states_sf)

# combine static map ----
# get boundaries from south_east_coast_states_shp_bb

label_text_size <- 3

# The code creates a plot using the ggplot2 library to visualize spatial data.
shp_file_with_cnts_list_maps <- 
  shp_file_with_cnts_list |>
  purrr::map(\(curr_sf) {
    curr_sf |>
      filter(!is.na(total_vsl_by_state_cnt)) |> 
      
      ggplot2::ggplot() +
      # Start building the ggplot object for plotting.
      
      ggplot2::geom_sf(aes(fill = nc_round_perc)) +
      # Add a layer for plotting spatial features, using nc_round_perc for fill color.
      
      ggplot2::geom_sf_label(aes(label = my_label),
                    size = label_text_size) +
      # Add a layer for labeling spatial features using the my_label column, with a specified size.
      
      ggplot2::geom_sf(data = states_sf, fill = NA) +
      # Add a layer for plotting state boundaries, with no fill color (NA).

      # Set the coordinate limits for the plot, based on the bounding box of southeast coast states,
      
      ggplot2::coord_sf(
        xlim =
          c(
            floor(south_east_coast_states_shp_bb$xmin),
            ceiling(south_east_coast_states_shp_bb$xmax)
          ),
        ylim =
          c(
            floor(south_east_coast_states_shp_bb$ymin),
            ceiling(south_east_coast_states_shp_bb$ymax)
          ),
        # with expand = FALSE to prevent expansion beyond the specified limits.
        expand = FALSE
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_fill_continuous(name = "Percent non-compliant",
                            type = "viridis",
                            direction = -1) +
      ggplot2::theme(legend.position = "bottom")

  })

# individual plots ----

## make map titles ----
permit_regions <-
  c("SA only",
    "GOM and Dual",
    "GOM only")

# Generate plot titles using 'str_glue' to include the 'permit_region'.
perc_plot_titles <-
  permit_regions |>
  purrr::map(\(permit_region) {
    stringr::str_glue("Percent of non-compliant Vessels {permit_region} permitted in 2022 by Home Port State")
  })

# Set the names of the 'perc_plot_titles' list to be the same as the 'permit_regions'.
names(perc_plot_titles) <- permit_regions

## save plot to file function ----
write_png_to_file <- function(output_file_name,
                              map_plot) {
  
  png_width  <- 30
  png_height <- 20
  
  ggplot2::ggsave(
      file = output_file_name,
      plot = map_plot,
      device = "png",
      path = file.path(my_paths$outputs,
                       current_project_dir_name),
      width = png_width,
      height = png_height,
      units = "cm"
    )
}

## GOM ----
permit_region <- "GOM only"

gom_map <-
  shp_file_with_cnts_list_maps$gom_only +
  ggplot2::ggtitle(perc_plot_titles[[permit_region]])

output_file_name <- "gom_perc_by_state.png"

write_png_to_file(output_file_name,
                  gom_map)

## GOM and dual ----
permit_region <- "GOM and Dual"

gom_dual_map <-
  shp_file_with_cnts_list_maps$gom_dual +
  ggplot2::ggtitle(perc_plot_titles[[permit_region]])

output_file_name <- "gom_dual_perc_by_state.png"

write_png_to_file(output_file_name,
                  gom_dual_map)

## SA only ----
permit_region <- "SA only"

sa_only_map <-
  shp_file_with_cnts_list_maps$sa_only +
  ggplot2::ggtitle(perc_plot_titles[[permit_region]])

output_file_name <- "sa_only_perc_by_state.png"

write_png_to_file(output_file_name,
                  sa_only_map)
