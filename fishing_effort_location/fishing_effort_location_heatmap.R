# Use the 'source' function to execute R code from a file located at the given path.

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# data are from "by_permit"

# Construct the file path using elements from the 'my_paths' list.
by_permit_path <-
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )

source(by_permit_path)

# rstudioapi::getSourceEditorContext()$path
# setup for fishing_effort_location_heatmap ----
# library(ggplot2) # a visualization package
# library(ggmap) # extends 'ggplot2' for creating maps and working with spatial data.
library(viridis) # additional color palettes

# Heatmap ----

# Construct the file path using elements from the 'my_paths' list.
heatmap_func_path <-
  file.path(my_paths$git_r,
            r"(fishing_effort_location\prepare_heatmap_func.R)")

source(heatmap_func_path)

## heatmap data ----

# for_heatmap_lat_lon_trips_only <-
#   coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$gom_and_dual |>
#   dplyr::select(trip_id, latitude, longitude) |>
#   dplyr::distinct()

# glimpse(for_heatmap_lat_lon_trips_only)
# Rows: 41,455

# Split the data frame into multiple sub-data frames based on the 'permit_region' column.

coord_data_2022_short_good_sf_crop_big_df_in_metricks_list <-
  split(
    # Data frame to be split
    coord_data_2022_short_good_sf_crop_big_df_in_metricks,

    # Split based on the 'permit_region' column
    as.factor(
      coord_data_2022_short_good_sf_crop_big_df_in_metricks$permit_region
    )
  )

# Use the 'map' function to apply the 'dim' function to each element in the list.
purrr::map(
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list,
  dim
)

# gom
# Create a new data frame 'for_heatmap_lat_lon_trips_vessels_gom_only' by applying a series of data manipulation operations.

for_heatmap_lat_lon_trips_vessels_gom_only <-
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$gom_and_dual |>

  # dplyr::select specific columns.
  dplyr::select(trip_id, vessel_official_nbr, latitude, longitude) |>

  # Remove duplicate rows using 'distinct'.
  dplyr::distinct()

dim(for_heatmap_lat_lon_trips_vessels_gom_only)
# Rows: 41,455
# [1] 46763     4 mv

# sa
for_heatmap_lat_lon_trips_vessels_sa_only <-
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  # dplyr::select specific columns.
  dplyr::select(trip_id, vessel_official_nbr, latitude, longitude) |>
  # Remove duplicate rows using 'distinct'.
  dplyr::distinct()

dim(for_heatmap_lat_lon_trips_vessels_sa_only)
# [1] 68122     4
# [1] 44060     4

### remove vessels not in Jeannette's SA list ----

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_to_remove_from_ours' vector.
for_heatmap_lat_lon_trips_vessels_sa_only_rm <-
  for_heatmap_lat_lon_trips_vessels_sa_only |>
  filter(!vessel_official_nbr %in% vessels_to_remove_from_ours)

dim(for_heatmap_lat_lon_trips_vessels_sa_only_rm)
# [1] 67983     4

## add the grid ----
# assuming data is dataframe with variables LATITUDE, LONGITUDE, and trips

# tic("effort")
# effort <- df_join_grid(for_heatmap_lat_lon_trips_only)
# toc()
# effort: 0.75 sec elapsed

tic("effort_vsl_gom")
# Create a new object 'my_crs' by extracting the coordinate reference system (CRS)
# from a spatial object 'GOMsf'.
my_crs <- sf::st_crs(GOMsf)
# Create a new data frame 'effort_vsl_gom' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_gom_only' and 'grid_gom5' based on a common
# spatial reference system defined by 'my_crs'.
effort_vsl_gom <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_gom_only,
               grid_gom5,
               my_crs)
toc()
# effort_vsl: 0.62 sec elapsed

# class(effort_vsl)

tic("effort_vsl_sa")
# Create a new object 'my_crs_sa' by extracting the coordinate reference system (CRS)
# from a spatial object 'sa_shp'.
my_crs_sa <- sf::st_crs(sa_shp)
# sf::st_geometry(grid_sa5)
# Geodetic CRS:  NAD83
# sf::st_geometry(sa_shp)
# Geodetic CRS:  NAD83

# Create a new data frame 'effort_vsl_sa' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_sa_only_rm' and 'grid_sa5' based on a
# spatial reference system defined by 'my_crs_sa'.
effort_vsl_sa <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_sa_only_rm,
               grid_sa5,
               my_crs = my_crs_sa)
toc()
# effort_vsl_sa: 1.22 sec elapsed

## crop by the shape ----
tic("effort_vsl_cropped_gom")
effort_vsl_cropped_gom <- crop_by_shape(effort_vsl_gom)
toc()
# effort_cropped2: 0.44 sec elapsed

dim(effort_vsl_cropped_gom)
# [1] 35822     7
# [1] 40604     7 mv

tic("effort_vsl_cropped_sa")
effort_vsl_cropped_sa <- crop_by_shape(effort_vsl_sa, sa_shp)
toc()
# effort_vsl_cropped_sa: 0.54 sec elapsed

dim(effort_vsl_cropped_sa)
# [1] 21461     8
# [1] 20147     8 mv

## count trip ids and vessels by grid cell ----

# Create a list 'effort_vsl_cropped_cnt_l' by applying 'add_vsl_and_trip_cnts' function to data frames.

effort_vsl_cropped_cnt_l <-
  list(effort_vsl_cropped_gom, effort_vsl_cropped_sa) |>

  # Use the 'map' function to apply a function to each element in the list.
  purrr::map(function(effort_vsl_cropped) {

    # Apply the 'add_vsl_and_trip_cnts' function to each 'effort_vsl_cropped' data frame.
    add_vsl_and_trip_cnts(effort_vsl_cropped)
  })

map(effort_vsl_cropped_cnt_l, dim)
# [[1]]
# [1] 35822     9
#
# [[2]]
# [1] 21461    10
# mv data:
# [[1]]
# [1] 40604     9
#
# [[2]]
# [1] 20147    10

# check
# effort_vsl_cropped_cnt_l[[1]] |>
#   sf::st_drop_geometry() |>
#   filter(cell_id == 1864) |>
#   dplyr::select(vsl_cnt, trip_id_cnt) |>
#   dplyr::distinct() |>
#   glimpse()
# vsl_cnt     <int> 11
# trip_id_cnt <int> 236

# class(effort_vsl_cropped_cnt2)

### no rule3 ----
# Create a list 'effort_cropped_short_cnt2_short_l' by applying a set of operations to data frames.

effort_cropped_short_cnt2_short_l <-
  effort_vsl_cropped_cnt_l |>

  # Use the 'map' function to apply a function to each element in the list.
  purrr::map(function(effort_vsl_cropped_cnt) {

    # Use the 'select' function to remove specific columns,
    # 'latitude', 'longitude', 'trip_id', and 'VESSEL_OFFICIAL_NBR', from each data frame.
    effort_vsl_cropped_cnt |>
      dplyr::select(-c(latitude, longitude, trip_id, vessel_official_nbr))
  })

# map(effort_cropped_short_cnt2_short_l, dim)

### not used, with rule 3 ----
# effort_cropped_short_cnt_rule3_short <-
#   effort_cropped_short_cnt_rule3 |>
#   dplyr::select(-c(latitude, longitude, trip_id, vessel_official_nbr))
#
# dim(effort_cropped_short_cnt_rule3_short)
# # [1] 31981     5
#
## join with min grid ----
### not used, rule 3 ----
# heat.plt_rule3 <-
#   effort_cropped_short_cnt_rule3_short |>
#   # have to use data.frame, to avoid
#   # Error: y should not have class sf; for spatial joins, use st_join
#   inner_join(data.frame(grid))
# # Joining with `by = join_by(cell_id)`
#
# # mapview(grid)
# dim(heat.plt_rule3)
# # [1] 35828     6
# # [1] 33883     6 (rule3)
# # [1] 31981     6
#
### no rule 3 ----
heat.plt_gom <-
  # Extract the first element from the list.
  # Use the pipe operator to pass it to the next operation.
  effort_cropped_short_cnt2_short_l[[1]] |>
  # Perform an inner join with the data frame 'grid_gom5'
  # using a common column specified by 'join_by(cell_id)'.
  # Store the result in the variable 'heat.plt_gom'.
  # have to use data.frame, to avoid:
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_gom5))

# the same for SA
heat.plt_sa <-
  effort_cropped_short_cnt2_short_l[[2]] |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_sa5))
# Joining with `by = join_by(cell_id)`

## make a plot ----

max_num3_gom <- max(heat.plt_gom$trip_id_cnt)
# 1209
# 1317 mv

max_num3_sa <- max(heat.plt_sa$trip_id_cnt)
# 590
# 561 mv

# map_trips_rule_3 <-
#   make_map_trips(heat.plt,
#            st_union_GOMsf,
#            "total trips",
#            max_num = max_num3,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution). 2022. GoM permitted vessels. Only squares with more than 3 reporting vessels are shown. ")

### GOM & dual 2022 ====
# map_trips_no_rule_3
# print_df_names(heat.plt_gom)
map_trips_no_rule_3_gom <-
  make_map_trips(heat.plt_gom,
           st_union_GOMsf,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 1.2)

map_trips_no_rule_3_gom

### SA 2022 ====
map_trips_no_rule_3_sa <-
  make_map_trips(heat.plt_sa,
           sa_shp,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 0.9,
           legend_text_text_size = 7.5)

map_trips_no_rule_3_sa +
# Add the spatial features from 'sa_s_shp' to the plot using 'geom_sf'.
geom_sf(data = sa_s_shp) +

# Annotate the plot with text labels from 'sa_s_shp' using 'geom_sf_text'.
geom_sf_text(data = sa_s_shp,
               label = sa_s_shp$NAME,  # Use the 'NAME' column as labels.
               size = 3)  # Set the size of the text labels to 3.


# make_map_trips <-
#   function(map_trip_base_data,
#            shape_data,

#            total_trips_title,
#            trip_cnt_name,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
#            unit_num = 1,
#            print_stat_zone = NULL
#            ) {

# To get end port numbers by state ----
permit_end_port_path <-
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit_and_end_port.R)"
  )

source(permit_end_port_path)

