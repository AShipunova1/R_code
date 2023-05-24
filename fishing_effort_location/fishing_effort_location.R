# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(tictoc) #benchmarking

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

my_to_sf <- function(my_df) {
  my_df %>%
    sf::st_as_sf(
      coords = c("LONGITUDE",
                 "LATITUDE"),
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 

# fields to get 
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth

# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# combine with additional area data ----
db_data_w_area <- full_join(db_area_data, db_data)
# Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
# LOCAL_AREA_CODE)`

all_points <- dim(db_data_w_area)[1]
# 254689

# View(db_data_w_area)

# make a flat file ----
dir_to_comb <- file.path(my_paths$git_r, current_project_name)

files_to_combine <-
  c(
    file.path(my_paths$git_r, "useful_functions_module.r"),
    file.path(dir_to_comb, "read.me.R"),
    file.path(dir_to_comb, "fishing_effort_locations_get_data.R"),
    file.path(dir_to_comb, "fishing_effort_location.R"),
    file.path(dir_to_comb, "fishing_effort_location_viz.R")
  )

flat_file_name = file.path(dir_to_comb, "fishing_effort_location_flat.R")

# run as needed
make_a_flat_file(flat_file_name,
                 files_to_combine_list)

# correct lat/long ----
db_data_w_area_report <-
  db_data_w_area %>%
  # all LONG should be negative
  mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  select(
    TRIP_START_DATE,
    TRIP_END_DATE,
    START_PORT,
    START_PORT_NAME,
    START_PORT_COUNTY,
    START_PORT_STATE,
    END_PORT,
    LATITUDE,
    LONGITUDE,
    # MINIMUM_BOTTOM_DEPTH,
    # MAXIMUM_BOTTOM_DEPTH,
    FISHING_GEAR_DEPTH
  )

dim(db_data_w_area_report)
# 254689     

db_data_w_area_report_short <-
  db_data_w_area_report %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))

dim(db_data_w_area_report_short)
# data_overview(db_data_w_area_report)
# 253142     

db_data_w_area_report_sf <- my_to_sf(db_data_w_area_report_short)

dim(db_data_w_area_report_sf)
# 253142      11

# SA EEZ only ----
## sa eez st_intersection ----
tic("sf::st_intersection(db_data_w_area_report_sf, sa_shp)")
db_data_w_area_report_sa_eez_sf <-
  sf::st_intersection(db_data_w_area_report_sf, sa_shp)
# 2min
toc()
# 657.37 / 60 ~ 11m
# clean session: ~62 sec

# or read it
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

# all.equal(db_data_w_area_report_sa_eez_sf,
#           db_data_w_area_report_sa_eez_sf1)
dim(db_data_w_area_report_sa_eez_sf)
# 54950 13
# db_data_w_area_report_sa_eez_sf <- db_data_w_area_report_sa_eez

#### save sa_eez_data ----
# sf::st_write(db_data_w_area_report_sa_eez, file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf.shp"))
# err

write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

# cc <- sf::st_coordinates(db_data_w_area_report_sa_eez)
# 
# str(db_data_w_area_report_sa_eez)

# m_db_data_w_area_report_sa_eez <- mapview(db_data_w_area_report_sa_eez_sf,
        # layer.name = 'SA EEZ')

# db_data_w_area_report_sa_eez %>%
#   select(-c(Id, AreaName)) %>%
#   write_csv(
#   file.path(
#     my_paths$outputs,
#     current_project_name,
#     "db_data_w_area_report_sa_eez.csv"
#   ))
# 
#   

# to_sf

# str(sa_shp)
# s1 <- filter(sa_shp,
#              AreaName == "Off FL")
# 


mapview(db_data_w_area_report_sa_eez_sf1)

# south of 28N - all SA ----
db_data_w_area_report_sf_28_s <-
  db_data_w_area_report_short %>%
  filter(between(LATITUDE, 23, 28) &
           between(LONGITUDE, -83, -71)) %>%
  my_to_sf()

dim(db_data_w_area_report_sf_28_s)
# 92949    

## state waters sa ----
fl_counties_sa <- c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "Saint Johns",
    "Saint Lucie",
    "Volusia", 
    "Monroe") #has GOM too, remove separately

# mapview(fl_state_w_counties)
# fl_state_w_counties$gnis_name %>% paste0(collapse = ", ")

fl_state_w_counties_names <- fl_state_w_counties$gnis_name
length(fl_state_w_counties_names)
# 67

# grep("Monroe", fl_state_w_counties_names, value = T)

length(fl_counties_sa)
# 12 + Monroe

fl_state_w_counties_names_df <- as.data.frame(fl_state_w_counties_names)
# str(fl_state_w_counties_names_df)
# fl_state_w_counties_names) %>%

# View(as.data.frame(fl_counties_sa))

sa_fl_state_w_counties_names <-
  as.data.frame(fl_counties_sa)[[1]] %>%
  map_df(function(fl_county) {
    # browser()
    sa_county <-
      fl_state_w_counties_names_df %>%
      filter(grepl(
        fl_county,
        fl_state_w_counties_names_df$fl_state_w_counties_names
      ))
    
    return(sa_county)
  })

fl_state_w_counties_sa <- filter(fl_state_w_counties,
             gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names)

# mapview(fl_state_w_counties_sa)

# names(fl_state_w_counties_sa)
# View(fl_state_w_counties_sa)

tic("sf::st_intersection(db_data_w_area_report_sf_28_s,
                      fl_state_w_counties_sa)")
db_data_w_area_report_28_s_sa_counties_sf <-
  sf::st_intersection(db_data_w_area_report_sf_28_s,
                      fl_state_w_counties_sa)
# 3 m
toc()
# 5.03 sec 1 county
# 10.39 sec

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30

# names(db_data_w_area_report_28_s_sa_counties_sf)
write_csv(db_data_w_area_report_28_s_sa_counties_sf, file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf.csv"))

# mapview(db_data_w_area_report_28_s_sa_counties_sf,
#           col.regions = "green",
#   layer.name = 'State and inner waters'
# ) + sa_shp

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482

sf::sf_use_s2(FALSE)
tic("sf::st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)")
db_data_w_area_report_28_s_sa_counties_no_gom_sf <- sf::st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
toc()
# 15 m
# 673.98 = 11 m
# 7.16 sec clean session, no plots, 1 county
# 541.61 / 60 = 9 m, all sa counties, 1 plot

# or read csv 
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  sf::st_as_sf(
    coords = c("LONGITUDE",
               "LATITUDE"),
    crs = sf::st_crs(sa_shp),
    # keep LAT/LONG, to save in a file
    remove = FALSE
  )

# all.equal(db_data_w_area_report_28_s_sa_counties_no_gom_sf, db_data_w_area_report_28_s_sa_counties_no_gom_sf1)

# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
write_csv(db_data_w_area_report_28_s_sa_counties_no_gom_sf,
          sa_counties_no_gom_sf_filename)

### map ----
tic("mapview(
  db_data_w_area_report_sa_counties_no_gom")
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  mapview(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  col.regions = "green",
  layer.name = 'State and inner waters'
)
toc()

tic("Show m_db_data_w_area_report_28_s_sa_counties_no_gom_sf + eez")
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf +
  db_data_w_area_report_sa_eez_sf1
toc()

## below 28 grey ----
# db_data_w_area_lat_lon_reg <-  
#   db_data_w_area_lat_lon %>% 
#   filter(!grepl("GULF OF MEXICO", AREA_NAME)) %>% 
#   filter(!grepl("TAMPA", AREA_NAME)) %>% 
#   filter(!grepl("FORT MYERS", AREA_NAME)) %>% 
#   filter(!grepl("GULF OF MEXICO", REGION))
# 
# dim(db_data_w_area_lat_lon_reg)
# 88796

# View(db_data_w_area_lat_lon_reg)

# to_report <-
#   db_data_w_area_lat_lon_reg %>%
#   select(
#     TRIP_START_DATE,
#     TRIP_END_DATE,
#     START_PORT,
#     START_PORT_NAME,
#     START_PORT_COUNTY,
#     START_PORT_STATE,
#     END_PORT,
#     LATITUDE,
#     LONGITUDE,
#     MINIMUM_BOTTOM_DEPTH,
#     MAXIMUM_BOTTOM_DEPTH,
#     FISHING_GEAR_DEPTH,
#     DEPTH
#   ) %>%
#   unique() #?

# dim(to_report)
# 27077

# add counts to unique?
#   db_data_w_area_lat_lon_reg %>%
#   select(
#     TRIP_START_DATE,
#     TRIP_END_DATE,
#     START_PORT,
#     START_PORT_NAME,
#     START_PORT_COUNTY,
#     START_PORT_STATE,
#     END_PORT,
#     LATITUDE,
#     LONGITUDE,
#     MINIMUM_BOTTOM_DEPTH,
#     MAXIMUM_BOTTOM_DEPTH,
#     FISHING_GEAR_DEPTH,
#     DEPTH
#   ) %>%
# data_overview()

## Below 28: remove GOM at sea points ----
# db_data_w_area_report_28_s_no_gom_reef <-
#   sf::st_difference(db_data_w_area_report_sf_28_s, gom_reef_shp)

## Below 28: keep only SA counties ----
# db_data_w_area_report_28_s_no_gom_reef_state_w_sf <-
#   sf::st_intersection(db_data_w_area_report_28_s_no_gom_reef,
#                       fl_state_w_counties_sa)
# 
# 
# m_db_data_w_area_report_28_s_no_gom_reef_state_w_sf <-
#   mapview(
#   db_data_w_area_report_28_s_no_gom_reef_state_w_sf,
#   col.regions = "green",
#   layer.name = 'State and inner waters south of 28N'
# )
# 
# m_db_data_w_area_report_sa_eez + m_db_data_w_area_report_28_s_no_gom_reef_state_w_sf

# mapview(db_data_w_area_report_28_s_no_gom_reef) + m_s

### grey points in SA outside of EEZ ----

# - sa_eez
names(sa_shp)
sa_shp$AreaName

sa_shp_fl <-
  filter(sa_shp,
         AreaName == "Off FL")

geom_sa_shp_fl <- st_geometry(sa_shp_fl)
# Geometry type: POLYGON
# Bounding box:  xmin: -83 ymin: 23.81794 xmax: -76.5011 ymax: 30.71267

# all but ymax are from sa_shp_fl
new_box <- c(
  xmin = -83,
  ymin = 23.81794,
  xmax = -76.5011,
  ymax = 28 # 28N
)

sa_shp_fl_s_28 <- sf::st_crop(sa_shp, new_box)
# st_geometry(sa_shp_fl_s_28)

# all.equal(sa_shp_fl_s_28, sa_shp_fl_s_281)

geom_db_data_w_area_report_sf_28_s <- st_geometry(db_data_w_area_report_sf_28_s) 
# Geometry set for 92949 features 
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -83 ymin: 23.29354 xmax: -78 ymax: 28

# st_disjoint
db_data_w_area_report_sf_28_s_minus_eez <-
  sf::st_difference(db_data_w_area_report_sf_28_s,
                    sa_shp_fl_s_28)

mapview(db_data_w_area_report_28_s_no_gom_reef_minus_sa_eez)

# - state_w
dim(db_data_w_area_report_sf_28_s)
dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
db_data_w_area_report_sf_28_s_minus_state <-
  sf::st_difference(db_data_w_area_report_sf_28_s,                    db_data_w_area_report_28_s_sa_counties_no_gom_sf)
