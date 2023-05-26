# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# fields to get 
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

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

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)
  
  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)
  
  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

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

# Filter out maximum by data ----
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
  # west and east by SA shp
  dplyr::filter(between(LONGITUDE, -83, -71.37133))
  # [1] 140177     32

# st_geometry(sa_shp)
# Bounding box:  xmin: -83 ymin: 23.81794 xmax: -71.37133 ymax: 36.55028

db_data_w_area_no_mex_uniq <-
  db_data_w_area_no_mex %>% 
  unique() 

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
# 45315 10

# keep fewer rows by removing duplicates ----
db_data_w_area_report <-
  db_data_w_area_report_short %>% unique()
# [1] 45261    10

# SA EEZ for all ----
## get only the points inside the SA EEZ ----
db_data_w_area_report_sf <-
  db_data_w_area_report_short %>%
  unique() %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

# dim(db_data_w_area_report_sf)
# [1] 45261    11

### with st_intersection ----
# get only the points inside the SA EEZ by intersection
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
# 594.35 sec
# 63.44 sec (uniq)
# 65.1 sec 
# 174.95 sec

### or read it ----
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

#### save sa_eez_data ----
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

dim(db_data_w_area_report_sa_eez_sf)
# 18989 13

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report %>%
  filter(between(LATITUDE, 23, 28)) %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

dim(db_data_w_area_report_28_s_sf)
# 92882   11
# 27979   11   unique

## state waters sa ----
get_state_waters_sa_sf <- function() {
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

fl_state_w_counties_sa_sf <- get_state_waters_sa_sf()

# mapview(db_data_w_area_report_28_s_sf)

### get only state and inner waters by intersection ----
db_data_w_area_report_28_s_sa_counties_sf <-
  with_st_intersection(db_data_w_area_report_28_s_sf,
                      fl_state_w_counties_sa_sf)
# 0.37 sec 
# 3.56 sec

# dim(db_data_w_area_report_28_s_sa_counties_sf)
# [1] 10761    30

# mapview(db_data_w_area_report_28_s_sa_counties_sf)

### or read csv ----
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
# 10761    30

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

### using sf::st_difference ----
# slow 
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
# 188.69 sec

# or read csv 
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
# 8903

write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

# mapview(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

# Report csv ----
my_sf_to_df <- function(my_sf) {
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
  
  return(my_df)
}

my_sf_to_csv <- function(my_sf, file_name) {
  my_df <- my_sf_to_df(my_sf)
  
  write_csv(
    my_df,
    file.path(my_paths$outputs,
              current_project_name,
              "report",
              paste0(file_name, ".csv"))
  )
}

db_data_w_area_more_fields_u <-
  db_data_w_area %>%
  unique()
# %>%
#   mutate(across(.fns = as.character))

str(db_data_w_area_more_fields_u)
# 75536    

db_data_w_area_report_sa_eez_df <- my_sf_to_df(db_data_w_area_report_sa_eez_sf) 
# %>% 
    # mutate(across(.fns = as.character))


dim(db_data_w_area_report_sa_eez_df)
# 18967    

db_data_w_area_report_sa_eez_df_more_fields <-
  inner_join(
    db_data_w_area_more_fields_u,
    db_data_w_area_report_sa_eez_df
    # ,
    # join_by(
    #   TRIP_START_DATE,
    #   TRIP_END_DATE,
    #   START_PORT,
    #   END_PORT,
    #   LATITUDE,
    #   LONGITUDE,
    #   FISHING_GEAR_DEPTH
    # )
  )
# Joining with `by = join_by(TRIP_START_DATE, TRIP_END_DATE, START_PORT,
# START_PORT_NAME, START_PORT_COUNTY, START_PORT_STATE, END_PORT, LATITUDE,
# LONGITUDE, FISHING_GEAR_DEPTH)`

dim(db_data_w_area_report_sa_eez_df_more_fields)
# [1] 15482    30

# south
db_data_w_area_report_28_s_sa_counties_no_gom_df <- my_sf_to_df(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# %>%
#   mutate(SPECIESITIS = as.character(SPECIESITIS))

  # mutate(across(.fns = as.character))
# ! Using `across()` without supplying `.cols` was deprecated in dplyr
#   1.1.0.
# ℹ Please supply `.cols` instead.

dim(db_data_w_area_report_28_s_sa_counties_no_gom_df)
# 8903   

db_data_w_area_more_fields_u1 <-
  db_data_w_area_more_fields_u %>%
  mutate(across(.cols = c(TRIP_END_DATE,
                          TRIP_START_DATE,
                          LATITUDE,
                          LONGITUDE,
                          FISHING_GEAR_DEPTH
                          ),
                .fns = as.character)) 
# %>% 
    # mutate(across(.cols = c(TRIP_END_DATE,
    #                       TRIP_START_DATE
    #                       ),
    #             .fns = as.double()))

db_data_w_area_report_28_s_sa_counties_no_gom_df_more_fields <-
  inner_join(db_data_w_area_more_fields_u1,
             db_data_w_area_report_28_s_sa_counties_no_gom_df)

# ℹ `x$TRIP_START_DATE` is a <datetime<local>>.
# ℹ `y$TRIP_START_DATE` is a <character>.


dim(db_data_w_area_report_28_s_sa_counties_no_gom_df_more_fields)
my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "sa_eez_all")
# dim(db_data_w_area_report_sa_eez_sf)
# 18989    

my_sf_to_csv(db_data_w_area_report_28_s_sa_counties_no_gom_sf, "south_of_28_state_w") 
# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903   

# all maps together ----
## south of 28 map ----
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  mapview(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  col.regions = "green",
  layer.name = 'State and inner waters'
)

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
    layer.name = 'SA EEZ'
  )

all_maps <-
  m_s +
  m_g_r +
  m_sa_eez +
  m_db_data_w_area_report_28_s_sa_counties_no_gom_sf


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

# The relative would be looking by depth, area, and seasonally. ----
## by area: ----
# db_data_w_area_report_sa_eez_sf
# db_data_w_area_report_28_s_sa_counties_no_gom_sf

## by depth, state ----
db_data_w_area_report_sa_eez_sf %>%
  my_sf_to_df() %>%
  count(FISHING_GEAR_DEPTH, START_PORT_STATE) %>%
  View()

db_data_w_area_report_28_s_sa_counties_no_gom_sf %>% 
  my_sf_to_df() %>%
  count(FISHING_GEAR_DEPTH, START_PORT_STATE) %>%
  View()

# by end_port, depth, month ---
db_data_w_area_report %>% 
    dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  count(FISHING_GEAR_DEPTH, END_PORT, TRIP_START_M) %>% glimpse()
  # View()

## seasonally ----

