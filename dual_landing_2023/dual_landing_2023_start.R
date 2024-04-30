# dual_landing_2023.R

# setup ----
# 2022, 2023
# dual + SA
# library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
# library(zoo)   # Load the 'zoo' library, which deals with time series data.
# library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
# library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.

# Read R Code from a File
source("~/R_code_github/useful_functions_module.r")

my_year1 <- "2022"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2023"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions' definition.
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

dir.create(curr_proj_output_path)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

dir.create(curr_proj_input_path)

project_name <- current_project_basename
# "dual_landing_2023"

# get data ----
dual_landing_2023_get_data_path <- 
  file.path(current_project_dir_name,
            "dual_landing_2023_get_data.R")

source(dual_landing_2023_get_data_path)

# processed_logbooks
# vessels_no_logbooks

dim(processed_logbooks)
# [1] 164159    153
# grep("lon", names(processed_logbooks), value = T)

processed_logbooks_dual_short <-
  processed_logbooks |>
  filter(permit_sa_gom_dual == "dual") |>
  select(vessel_official_number,
         trip_id,
         trip_start_date,
         trip_end_date,
         latitude,
         longitude) |>
  distinct()

dim(processed_logbooks_dual_short)
# 4958    6

# n_distinct(processed_logbooks_dual_short$vessel_official_number)
# 143

processed_logbooks_dual_short_no_na <-
  processed_logbooks_dual_short |>
  dplyr::filter(stats::complete.cases(latitude) &
                  stats::complete.cases(longitude))

dim(processed_logbooks_dual_short_no_na)
# [1] 4816    6
my_crs <-
  st_crs(sa_states_shp) # lat/long coordinate reference system

# convert crss ----

tic("st_transform1")
GOM_s_fl_state_waters_only_my_shp <- 
  st_transform(GOM_s_fl_state_waters_only, crs = my_crs)
toc()

tic("st_transform2")
GOMsf_my_shp <- 
  st_transform(GOMsf, crs = my_crs)
toc()
# st_transform2: 60.79 sec elapsed

tic("st_transform sa_fl_state_w_counties_shp")
sa_fl_state_w_counties_shp_my_shp <- 
  st_transform(sa_fl_state_w_counties_shp, crs = my_crs)
toc()



processed_logbooks_dual_short_sf <- sf::st_as_sf(
  processed_logbooks_dual_short_no_na,
  coords = c("longitude", "latitude"),
  crs = my_crs  
)

# subset point by a sa shp ----
tic("sa st_intersection")
processed_logbooks_dual_short_sf_sa <-
  st_intersection(processed_logbooks_dual_short_sf,
                  sa_states_shp)
toc()
# 0.89 sec elapsed

mapview(sa_states_shp) + 
mapview(processed_logbooks_dual_short_sf_sa, 
        col.regions = "pink")

# by big box ----
big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

# big_bounding_box_sf <- bbox2sf(big_bounding_box)

# sf_use_s2(TRUE)

class(big_bounding_box) <- "bbox"

box_end <- big_bounding_box |>
  st_as_sfc() |>
  st_as_sf(crs = my_crs)

mapview(box_end)

# subset point by the big box ----

processed_logbooks_dual_short_sf_bb <-
  st_intersection(processed_logbooks_dual_short_sf,
                  box_end)
# 0.89 sec elapsed

# mapview(
#   sa_states_shp,
#   col.regions = "yellow",
#   layer.name = "SA states"
# ) +
mapview(GOMsf,
        layer.name = c("GOM waters")) +
  mapview(GOM_s_fl_state_waters_only_my_shp,
          layer.name = c("GOM S. Florida waters")) +
  mapview(
    processed_logbooks_dual_short_sf_sa_bb,
    col.regions = "lightgreen",
    cex = 5,
    layer.name = c("Dual permitted vessels' trips in 2023")
  )

# subset by sa FL waters ----

processed_logbooks_dual_short_sf_sa__sa_fl <-
  st_intersection(processed_logbooks_dual_short_sf_sa_bb,
                  sa_fl_state_w_counties_shp)

# mapview(sa_fl_state_w_counties_shp)+
# mapview(processed_logbooks_dual_short_sf_sa__sa_fl)
# 
# n_distinct(processed_logbooks_dual_short_sf_sa__sa_fl$trip_id)
# # 883
# 
# n_distinct(processed_logbooks_dual_short_sf_sa$trip_id)
# # 3891

tic("gom w fl w intersect")
processed_logbooks_dual_short_sf_bb__gom_fl <-
  st_intersection(processed_logbooks_dual_short_sf_bb,
                  GOM_s_fl_state_waters_only_my_shp)
toc()
# gom w st intersect: 1.42 sec elapsed

n_distinct(processed_logbooks_dual_short_sf_bb__gom_fl$trip_id)
# 337

tic("gom w st intersect")
processed_logbooks_dual_short_sf_bb__gomsf <-
  st_intersection(processed_logbooks_dual_short_sf_bb,
                  GOMsf_my_shp)
toc()
# gom w st intersect: 3.14 sec elapsed

# Get points not in gom waters ----
# A <- st_as_sf(meuse) %>% select(geometry) #155 obs
# B <- filter(A, row_number()<= 100)%>% select(geometry) #100 obs
# 
# diff_meuse <- st_difference(st_combine(A), st_combine(B)) %>% st_cast('POINT')

A <-
  st_as_sf(sa_fl_state_w_counties_shp_my_shp) %>% 
  select(geometry)

B <-
  st_as_sf(GOM_s_fl_state_waters_only_my_shp) |>
  select(geometry)

diff_meuse <-
  st_difference(st_combine(A), st_combine(B)) %>% st_cast('POINT')

# mapview(diff_meuse)


# mapview(sa_fl_state_w_counties_shp,
#         col.regions = "green") +
#   mapview(GOM_s_fl_state_waters_only)

# get counts ----
total_trips <-
  n_distinct(processed_logbooks_dual_short$trip_id)
# 4957

total_with_coords <-
  n_distinct(processed_logbooks_dual_short_no_na$trip_id)
# 4815 (total not NA)

processed_logbooks_dual_short_sf_sa_df <-
  sf::st_drop_geometry(processed_logbooks_dual_short_sf_sa)

n_distinct(processed_logbooks_dual_short_sf_sa_df$trip_id)
# 739 (in sa)

739/4815*100
# [1] 15.34787

## 1) count all in big_box ----
processed_logbooks_dual_short_sf_sa_bb_df <-
  st_drop_geometry(processed_logbooks_dual_short_sf_sa_bb)

total_points_in_bb <-
  n_distinct(processed_logbooks_dual_short_sf_sa_bb_df$trip_id)
# [1] 3891

3891/4815*100
# 80.80997
# 81% in the big box

# From points in SA state and federal waters
# subset GOM_s_fl_state_waters_only
# 2) points in SA state
sa_fl_state_w_counties_shp
sa_fl_state_w_counties_shp_df <-
  st_drop_geometry(sa_fl_state_w_counties_shp)

total_points_in_bb <-
  n_distinct(sa_fl_state_w_counties_shp_df$trip_id)




# in GOMsg
gomsf_trips <-
  n_distinct(processed_logbooks_dual_short_sf_bb__gomsf$trip_id)
# 2117

# in gom_st_fl
gom_st_fl_trips <- 
  n_distinct(processed_logbooks_dual_short_sf_bb__gom_fl)

total_in_gom <- 
  gom_st_fl_trips + gomsf_trips
# 2454

# total in SA ----
total_in_sa <- 
  total_points_in_bb - total_in_gom
# 1437

mapviewOptions(basemaps = # no "CartoDB.DarkMatter"
    c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery",
      "OpenTopoMap"))

mapviewOptions(basemaps.color.shuffle = FALSE)

mapview(GOM_s_fl_state_waters_only, 
        map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE")) +
  mapview(sa_fl_state_w_counties_shp,
          col.regions = "green") +
  mapview(processed_logbooks_dual_short_sf_sa__sa_fl)
