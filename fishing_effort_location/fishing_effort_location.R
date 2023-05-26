# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)

# setup ----

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

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)
  
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  toc()
  return(res)
}

with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)
  
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  toc()
  return(res)
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

all_points <- dim(db_data_w_area)[1]
# 254689

# View(db_data_w_area)

# source(
#   file.path(
#     my_paths$git_r,
#     current_project_name,
#     "fishing_effort_location_by_table.R"
#   )
# )

# Filter out maximum by data
db_data_w_area_no_mex <-
  db_data_w_area %>%
  # [1] 254689     32
  dplyr::filter(!(grepl("MEX", AREA_NAME))) %>% 
    # 254503     
  dplyr::filter(!(grepl("GOM", AREA_NAME))) %>%
  dplyr::filter(!REGION %in% c("GULF OF MEXICO")) %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>% 
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) %>% 
  # [1] 253003     32
  # south and north by SA shp
  dplyr::filter(between(LATITUDE, 23.81794, 36.55028)) %>%
    # [1] 241183     32
  # 133889     
  dplyr::filter(between(LONGITUDE, -83, -71.37133))
  # [1] 140177     32

# st_geometry(sa_shp)
# Bounding box:  xmin: -83 ymin: 23.81794 xmax: -71.37133 ymax: 36.55028

db_data_w_area_no_mex_uniq <-
  db_data_w_area_no_mex %>% 
# [1] 140177     32
# [1] 48579    32
  unique() 
# dim(db_data_w_area_no_mex_uniq)
# n of 28
# [1] 17725    32
# all
# 45322
# filter(is.na(LONGITUDE) | is.na(LATITUDE)) %>% dim()
# 1500
# 0
# 45315    
  
# keep fewer columns ----
db_data_w_area_report_short <-
  db_data_w_area_no_mex_uniq %>%
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
    FISHING_GEAR_DEPTH
  )

dim(db_data_w_area_report_short)
# 254503     10     
# [1] 140177     10
# uniq above 28
# [1] 17725    10
# 45315        

# keep fewer rows by removing duplicates ----
db_data_w_area_report <-
  db_data_w_area_report_short %>% unique()
# dim(db_data_w_area_report_short)
# [1] 45261    10
dim(db_data_w_area_report)
# above 28n
# [1] 17714    10
# all
# 45261

# SA EEZ for all ----
## sa eez st_intersection ----

db_data_w_area_report_sf <-
  db_data_w_area_report_short %>%
  # north of 28n
  # dplyr::filter(between(LATITUDE, 28, 36.55028)) %>%
  my_to_sf()

dim(db_data_w_area_report_sf)
# 253142      11
# [1] 140177     11
# [1] 45261    11
# 17725
# [1] 45315    11

### with st_intersection ----

db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
# 594.35 sec
# 63.44 sec (uniq)
# 65.1 sec 
# after restart
# 174.95 sec

# or read it
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

#### save sa_eez_data ----

write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

# mapview(db_data_w_area_report_sa_eez_sf)

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report %>%
  filter(between(LATITUDE, 23, 28)) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sf)
# [1] 92882    11
# 27986    uniq

## state waters sa ----
state_waters_sa_sf <- function() {
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
    "Monroe"
  ) #has GOM too, remove separately
  
  # mapview(fl_state_w_counties)
  # fl_state_w_counties$gnis_name %>% paste0(collapse = ", ")
  
  fl_state_w_counties_names <- fl_state_w_counties_shp$gnis_name
  
  # length(fl_state_w_counties_names)
  # 67
  
  # grep("Monroe", fl_state_w_counties_names, value = T)
  
  # length(fl_counties_sa)
  # 12 + Monroe
  
  fl_state_w_counties_names_df <-
    as.data.frame(fl_state_w_counties_names)
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
  
  fl_state_w_counties_sa <- filter(
    fl_state_w_counties_shp,
    gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names
  )
  
  return(fl_state_w_counties_sa)
}

fl_state_w_counties_sa_sf <- state_waters_sa_sf()

# mapview(db_data_w_area_report_28_s_sf)

db_data_w_area_report_28_s_sa_counties_sf <-
  with_st_intersection(db_data_w_area_report_28_s_sf,
                      fl_state_w_counties_sa_sf)
# 0.37 sec 
# 3.56 sec

# mapview(db_data_w_area_report_28_s_sa_counties_sf)
# [1] 10761    30

# or read csv
db_data_w_area_report_28_s_sa_counties_file_name <- 
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf_u.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  my_to_sf()

write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30
# 10761    

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

# using sf - slow
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
# 188.69 sec

# or read csv 
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
# 8903

write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

# mapview(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

# Report csv ----

my_sf_to_csv <- function(my_sf, file_name) {
  my_df <-
    my_sf %>%
    sf::st_drop_geometry() %>%
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
      FISHING_GEAR_DEPTH
    ) %>%
    unique()
  
  write_csv(
    my_df,
    file.path(my_paths$outputs,
              current_project_name,
              "report",
              paste0(file_name, ".csv"))
  )
}

my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "north_of_28n")
my_sf_to_csv(db_data_w_area_report_28_s_sa_counties_no_gom_sf, "south_of_28na") 
# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903   

# map ----
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  mapview(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  col.regions = "green",
  layer.name = 'State and inner waters'
)

# grey points in SA outside of EEZ ----

# (1) combine all shp
# sa_shp$AreaName
sa_shp_fl <-
  sa_shp %>% 
  filter(AreaName == "Off FL")

all_shp1 <-
  sf::st_union(sa_shp_fl, gom_reef_shp)

tic("sf::st_union(all_shp1, fl_state_w_counties_shp)")
all_shp2 <-
  sf::st_union(all_shp1, fl_state_w_counties_shp)
toc()
# 83.17 
# plot(all_shp2)

## all points below 28 minus all shapes ----
tic("with_st_difference(db_data_w_area_report_sf_28_s, all_shp2)")
db_data_w_area_report_sf_28_s_minus_all_shp <-
  with_st_difference(db_data_w_area_report_sf_28_s, all_shp2)
toc()

# 1) all points below 28 minus "good points"
# db_data_w_area_report_28_s_sa_counties_no_gom_sf
# 2) (1) - gom shape
# 3) (2) - Florida not sa counties

# 1) all points below 28 minus "good points" ----
# mapview(db_data_w_area_report_sf_28_s)
# mapview(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

db_data_w_area_report_sf_28_s_char <- mutate(db_data_w_area_report_sf_28_s,
         across(everything(), as.character))
db_data_w_area_report_28_s_sa_counties_no_gom_sf_char <- mutate(db_data_w_area_report_28_s_sa_counties_no_gom_sf,
         across(everything(), as.character))

# anti_join(x, y, by = NULL, copy = FALSE, ...)

all_s_28_minus_good_p_anti <-
  anti_join(
    as.data.frame(db_data_w_area_report_sf_28_s_char),
    as.data.frame(db_data_w_area_report_28_s_sa_counties_no_gom_sf_char),
    by = join_by(
      TRIP_START_DATE,
      TRIP_END_DATE,
      START_PORT,
      START_PORT_NAME,
      START_PORT_COUNTY,
      START_PORT_STATE,
      END_PORT,
      LATITUDE,
      LONGITUDE,
      FISHING_GEAR_DEPTH
    )
  )

dim(db_data_w_area_report_sf_28_s)
# [1] 92949    11

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# [1] 23519    32

# dim(all_s_28_minus_good_p)
# [1] 69430    11

a <- my_to_sf(all_s_28_minus_good_p_joins)
mapview(a)
# [1] 116468     33
# names(db_data_w_area_report_sf_28_s_char)
# names(db_data_w_area_report_28_s_sa_counties_no_gom_sf_char)
# names(all_s_28_minus_good_p_joins)
all_s_28_minus_good_p <-
  all_s_28_minus_good_p_joins %>%
  filter(!is.na(gnis_name)) %>%
  my_to_sf()

dim(all_s_28_minus_good_p)
# [1] 23519    34

# all_s_28_minus_good_p

mapview(all_s_28_minus_good_p) + sa_shp

# 2) (1) - gom shape ----
all_s_28_minus_good_p_minus_not_gom <-
  with_st_difference(all_s_28_minus_good_p, gom_reef_shp)
# 1227.47 / 60 ~ 20 min

### or read csv ----
all_s_28_minus_good_p_minus_not_gom_file_name <-
  file.path(my_paths$outputs, current_project_name, "all_s_28_minus_good_p_minus_not_gom_file_name.csv")
  
write_csv(all_s_28_minus_good_p_minus_not_gom, all_s_28_minus_good_p_minus_not_gom_file_name)

all_s_28_minus_good_p_minus_not_gom <-
  read_sf(all_s_28_minus_good_p_minus_not_gom_file_name) %>%
  my_to_sf()

mapview(all_s_28_minus_good_p_minus_not_gom) + m_s
# [1] 36509    13

# 3) (2) - Florida not sa counties ----
str(fl_state_w_counties)

all_s_28_minus_good_p_minus_not_gom_not_state <-
  with_st_difference(all_s_28_minus_good_p_minus_not_gom,
                     fl_state_w_counties)
# 571.08 sec
dim(all_s_28_minus_good_p_minus_not_gom_not_state)

# =======
# - sa_eez
get_florida_st_box <- function() {
  # names(sa_shp)
  # sa_shp$AreaName
  
  sa_shp_fl <-
    filter(sa_shp,
           AreaName == "Off FL")
  
  # geom_sa_shp_fl <- st_geometry(sa_shp_fl)
  # Geometry type: POLYGON
  # Bounding box:  xmin: -83 ymin: 23.81794 xmax: -76.5011 ymax: 30.71267
  
  # all but ymax are from sa_shp_fl
  new_box <- c(
    xmin = -83,
    ymin = 23.81794,
    xmax = -76.5011,
    ymax = 28 # 28N
  )
  return(new_box)
}

new_box <- get_florida_st_box()

sa_shp_fl_s_28 <- sf::st_crop(sa_shp, new_box)
# st_geometry(sa_shp_fl_s_28)

# geom_db_data_w_area_report_sf_28_s <- st_geometry(db_data_w_area_report_sf_28_s) 
# Geometry set for 92949 features 
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -83 ymin: 23.29354 xmax: -78 ymax: 28

tic("st_filter(db_data_w_area_report_sf_28_s,
                       sa_shp_fl_s_28,
                       .pred = st_disjoint)")
db_data_w_area_report_sf_28_s_minus_eez <-
  st_filter(db_data_w_area_report_sf_28_s,
                       sa_shp_fl_s_28,
                       .predicate = st_disjoint)
toc()
# 3.29  sec

dim(db_data_w_area_report_sf_28_s)
# [1] 92949    11

dim(sa_shp_fl_s_28)
# 1

dim(db_data_w_area_report_sf_28_s_minus_eez)
# [1] 62537    11

# mapview(db_data_w_area_report_sf_28_s_minus_eez)

# -state_w
dim(db_data_w_area_report_sf_28_s)
# [1] 92949    11

tic("  filter(
    db_data_w_area_report_sf_28_s_minus_eez,
    !geometry %in% db_data_w_area_report_28_s_sa_counties_no_gom_sf$geometry
"
)

db_data_w_area_report_sf_28_s_minus_eez_minus_gom <-
  filter(
    db_data_w_area_report_sf_28_s_minus_eez,
    !geometry %in% db_data_w_area_report_28_s_sa_counties_no_gom_sf$geometry
  )
toc()
# 0.91 s

### or read it ----

db_data_w_area_report_sf_28_s_minus_eez_minus_gom_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sf_28_s_minus_eez_minus_gom_file_name.csv")
  
write_csv(db_data_w_area_report_sf_28_s_minus_eez_minus_gom, db_data_w_area_report_sf_28_s_minus_eez_minus_gom_file_name)

db_data_w_area_report_sf_28_s_minus_eez_minus_gom <-
  read_sf(db_data_w_area_report_sf_28_s_minus_eez_minus_gom_file_name) %>%
  my_to_sf()

dim(db_data_w_area_report_sf_28_s_minus_eez)
# [1] 62537    11

str(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# [1] 23519    32

# [1] 39200    11

# 23519 + 39200 - 62537
# 182?
# exclude gom reef again ----
# gom_reef_shp
# sf::sf_use_s2(FALSE)

tic("sf::st_difference(db_data_w_area_report_sf_28_s_minus_eez_minus_gom,
                    gom_reef_shp)")
db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok <-
  sf::st_difference(db_data_w_area_report_sf_28_s_minus_eez_minus_gom,
                    gom_reef_shp)
toc()
# 730.45 / 60 = 12 min

dim(db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok)
# 6279

### or read it ----

db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok.csv")
  
write_csv(db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok, db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok_file_name)

db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok <-
  read_sf(db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok_file_name) %>%
  my_to_sf()

dim(db_data_w_area_report_sf_28_s_minus_eez_minus_gom_ok)
# [1] 6279   13

# all maps together ----
m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  layer.name = "South Altlantic",
  legend = FALSE
)

m_g_r <- mapview(
  gom_reef_shp,
  col.regions = "lightblue",
  alpha.regions = 0.2,
  layer.name = "GOM Reef Fish EFH",
  legend = FALSE
)

m_sa_eez <-
  mapview(
    db_data_w_area_report_sa_eez_sf,
    # col.regions = "green",
    layer.name = 'SA EEZ'
  )

# m_grey_outside <-
#   mapview(
#     db_data_w_area_report_sf_28_s_minus_eez_minus_gom,
#     col.regions = "darkgrey",
#     layer.name = 'State and inner waters'
#   )

tic("show all maps")
all_maps <-
 # m_grey_outside +
  m_sa_eez +
  m_db_data_w_area_report_28_s_sa_counties_no_gom_sf +
  m_s +
  m_g_r
toc()
# 4s

# m_db_data_w_area_report_sf_28_s_minus_eez_minus_gom

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
# make_a_flat_file(flat_file_name,
                 # files_to_combine_list)
