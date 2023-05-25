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
library(sfheaders) #working with sf

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

# to a table report ----
db_data_w_area_short <-
  db_data_w_area %>%
  select(
    TRIP_START_DATE,
    TRIP_END_DATE,
    START_PORT,
    START_PORT_NAME,
    START_PORT_COUNTY,
    START_PORT_STATE,
    END_PORT,
    END_PORT_NAME,
    END_PORT_COUNTY,
    END_PORT_STATE,
    LATITUDE,
    LONGITUDE,
    FISHING_GEAR_DEPTH,
    AREA_NAME,
    AREA_CODE,
    SUB_AREA_NAME,
    SUB_AREA_CODE,
    AREA_DISPLAY_NAME,
    DISTANCE_CODE_NAME,
    REGION
  )

dim(db_data_w_area_short)
# [1] 254689     15

db_data_w_area_report_minus_gom <-
  db_data_w_area_short %>%
  dplyr::filter(!(grepl("MEX", AREA_NAME))) %>%
  dplyr::filter(!(grepl("GOM", AREA_NAME))) %>%
  dplyr::filter(!REGION %in% c("GULF OF MEXICO")) %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  dplyr::filter(between(LONGITUDE, -83, -71))

dim(db_data_w_area_report_minus_gom)
# [1] 145694     15

# data_overview(db_data_w_area_report_minus_gom)
# LATITUDE      
# Min.   : 8.00 !
# Max.   :85.00  

    # filter(between(LATITUDE, 23, 28)

# db_data_w_area %>%
#   filter(is.na(AREA_CODE)) %>% dim()
# 504


## find codes ----
# names(db_data_w_area_report_minus_gom)
db_data_w_area_report_minus_gom %>%
  # filter(AREA_CODE == "001") %>% 
  select(# AREA_NAME,
    # SUB_AREA_CODE,
    AREA_CODE,
    # DISTANCE_CODE_NAME,
    # REGION
    ) %>% 
  # filter(is.na(AREA_CODE)) %>%
  # 0
  unique() %>%
  # arrange(SUB_AREA_CODE)
    # arrange(AREA_CODE)
# %>%
  dim()
  # View()
# 1 0000         
# 2 0001         
# 3 0008         
# 4 0009 
# REGION
# 1 NA            
# 2 SOUTH ATLANTIC
# 3 UNKNOWN  
# Bahamas 186

## sa area codes and unknowns ----
### FL sub-areas ----
sa_sub_area_codes <- list(
  "001" = c("0001", "0009"), # for 001
  "002" = c("0002", "0009"), # for 002
  "748" = c("0000", "0009") # for 748
)

# names(sa_sub_area_codes)

db_data_w_area_report_minus_gom_sub1 <-
  names(sa_sub_area_codes) %>%
  purrr::map_df(function(current_area_code) {
  # purrr::map(function(current_area_code) {
    # browser()
    filter(
      db_data_w_area_report_minus_gom,
      (toupper(db_data_w_area_report_minus_gom$START_PORT_STATE) == "FL") &
      db_data_w_area_report_minus_gom$AREA_CODE == current_area_code &
        db_data_w_area_report_minus_gom$SUB_AREA_CODE %in%
        sa_sub_area_codes[current_area_code][[1]]
    ) %>%
      return()
  })

dim(db_data_w_area_report_minus_gom_sub1)
# [1] 10806    19

### area_codes_to_keep (all other SA areas) ----
area_codes_to_keep <- c(
  "000", 631:747, 749
)

glimpse(area_codes_to_keep)
# 119

db_data_w_area_report_minus_gom_sub2 <-
  db_data_w_area_report_minus_gom %>%
  dplyr::filter(AREA_CODE %in%
                  area_codes_to_keep)

dim(db_data_w_area_report_minus_gom_sub2)
# [1] 26004    19

### unknown areas ----
states_sa <- data.frame(
  state_name = c(
    # "Florida", separate
    "Georgia",
    "North Carolina",
    "South Carolina")
)

# Reformat the R state df (create a DF of state abbreviations and state names as cols; 2x50)
state_tbl <- data.frame(state.abb, tolower(state.name))
names(state_tbl) = c("state_abb", "state_name")

# get SA states only from state_tbl
sa_state_abb <-
  # a table from above
  state_tbl %>%
  # get only these in our list
  filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  select(state_abb) %>%
  as.data.frame()

# str(sa_state_abb)

# db_data_w_area_report_minus_gom %>%
#   filter(AREA_CODE == "000") %>%
#   select(END_PORT_STATE) %>% count(END_PORT_STATE)
#   END_PORT_STATE    n
# 1             DE    4
# 2             FL 6101
# 3             GA  334
# 4             MD   33
# 5             NC  637
# 6             NJ   49

  # View()

### filter_sa_states ----
# create a filter
filter_sa_states <- quo(AREA_CODE == "000" &
                          tolower(END_PORT_STATE) %in% tolower(sa_state_abb$state_abb))

# use the filter
# names(db_data_w_area_report_minus_gom)
db_data_w_area_report_minus_gom_sub3 <-
  db_data_w_area_report_minus_gom %>%
  filter(!!filter_sa_states)
#   count(END_PORT_STATE)
#     END_PORT_STATE     n
#   <chr>          <int>
# 1 GA               334
# 2 NC               637

### filter_fl_counties ----
# create a filter
filter_fl_counties  <- quo(
  AREA_CODE == "000" &
    tolower(END_PORT_STATE) == "fl" &
    tolower(END_PORT_COUNTY) %in% tolower(fl_counties_sa)
)

# use the filter
db_data_w_area_report_minus_gom_sub4 <-
  db_data_w_area_report_minus_gom %>%
  filter(!!filter_fl_counties)

db_data_w_area_report_minus_gom_sub4 %>% 
  # View()
  count(END_PORT_COUNTY)
# END_PORT_COUNTY     n
#   <chr>           <int>
# 1 BREVARD           720
# 2 DUVAL              26
# 3 MARTIN             18
# 4 MIAMI-DADE          2
# 5 MONROE           3120
# 6 NASSAU             12
# 7 PALM BEACH        107
# 8 VOLUSIA           507
  

## combine SA areas ----
db_data_w_area_report_minus_gom <-
  full_join(db_data_w_area_report_minus_gom_sub1,
            db_data_w_area_report_minus_gom_sub2)

dim(db_data_w_area_report_minus_gom)
# 26004 + 10806 = 36810

View(db_data_w_area_report_minus_gom)
# end port
# 2 maps, 2 tables

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
# 253142     

db_data_w_area_report_sf <- my_to_sf(db_data_w_area_report_short)

dim(db_data_w_area_report_sf)
# 253142      11

# SA EEZ only ----
## sa eez st_intersection ----
### with st_intersection ----

db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)

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
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

# mapview(db_data_w_area_report_sa_eez_sf1)

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

fl_state_w_counties_names <- fl_state_w_counties_shp$gnis_name

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

fl_state_w_counties_sa <- filter(fl_state_w_counties_shp,
             gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names)

db_data_w_area_report_28_s_sa_counties_sf <-
  with_st_intersection(db_data_w_area_report_sf_28_s,
                      fl_state_w_counties_sa)

# or read csv
db_data_w_area_report_28_s_sa_counties_file_name <- 
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  my_to_sf()

write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)


db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)

# or read csv 
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32

write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

### map ----
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

m_grey_outside <-
  mapview(
    db_data_w_area_report_sf_28_s_minus_eez_minus_gom,
    col.regions = "darkgrey",
    layer.name = 'State and inner waters'
  )

tic("show all maps")
m_db_data_w_area_report_sf_28_s_minus_eez_minus_gom <-
 m_grey_outside +
  m_db_data_w_area_report_28_s_sa_counties_no_gom_sf +
  db_data_w_area_report_sa_eez_sf +
  m_s +
  m_g_r
toc()
# 4s

m_db_data_w_area_report_sf_28_s_minus_eez_minus_gom

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
