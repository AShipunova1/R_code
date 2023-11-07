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

# to a table report ----
db_data_w_area_short <-
  db_data_w_area %>%
  dplyr::select(
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

## filtering by region/area/coords ----
db_data_w_area_report_minus_gom <-
  db_data_w_area_short %>%
  dplyr::filter(!(grepl("MEX", AREA_NAME))) %>%
  dplyr::filter(!(grepl("GOM", AREA_NAME))) %>%
  dplyr::filter(!REGION %in% c("GULF OF MEXICO")) %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  dplyr::filter(between(LONGITUDE, -83, -71.37133)) %>%
  dplyr::filter(between(LATITUDE, 23.81794, 36.55028))
# st_geometry(sa_shp)
# Bounding box:  xmin: -83 ymin: 23.81794 xmax: -71.37133 ymax: 36.55028

# dim(db_data_w_area_report_minus_gom)
# [1] 145694     15

data_overview(db_data_w_area_report_minus_gom)
# no lat/long NAs

# db_data_w_area %>%
  # filter(is.na(AREA_CODE)) %>% dim()
# 504
# db_data_w_area_report_minus_gom %>% 
   # filter(is.na(AREA_CODE)) %>% dim()
# 0

## add a rowid ----
db_data_w_area_report_minus_gom %<>%
  dplyr::mutate(row_id = row_number())

# View(db_data_w_area_report_minus_gom)

## find codes ----
# names(db_data_w_area_report_minus_gom)
# db_data_w_area_report_minus_gom %>%
  # filter(AREA_CODE == "001") %>% 
  # dplyr::select(# AREA_NAME,
    # SUB_AREA_CODE,
    # AREA_CODE,
    # DISTANCE_CODE_NAME,
    # REGION
    # ) %>% 
  # filter(is.na(AREA_CODE)) %>%
  # 0
  # unique() %>%
  # dplyr::arrange(SUB_AREA_CODE)
    # dplyr::arrange(AREA_CODE)
# %>%
  # dim()
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

### FL sub-areas SA ----
sa_sub_area_codes <- list(
  "001" = c("0001", "0009"), # for 001
  "002" = c("0002", "0009"), # for 002
  "748" = c("0000", "0009") # for 748
)

sub1_fl_sub_areas <-
  names(sa_sub_area_codes) %>%
  purrr::map_df(function(current_area_code) {
    dplyr::filter(
      db_data_w_area_report_minus_gom,
      # current sub-area
      db_data_w_area_report_minus_gom$AREA_CODE == current_area_code &
        db_data_w_area_report_minus_gom$SUB_AREA_CODE %in%
        sa_sub_area_codes[current_area_code][[1]]
    ) %>%
      return()
  })

View(sub1_fl_sub_areas)
# [1] 10824    21

sub2_not_fl_sub_areas <-
  dplyr::filter(db_data_w_area_report_minus_gom,
                !(AREA_CODE %in% names(sa_sub_area_codes)))

# dim(sub1_fl_sub_areas)
# [1] 10824    21

# dim(sub2_not_fl_sub_areas)
# [1] 125389     21

# should be
# 10824 + 125389 = 136213

# (AREA_CODE %in% names(sa_sub_area_codes)
# [1] 14788    21

# dim(db_data_w_area_report_minus_gom)
# [1] 140177     21

db_data_w_area_report_minus_gom_fl_sa <-
  rbind(sub1_fl_sub_areas, sub2_not_fl_sub_areas)

dim(db_data_w_area_report_minus_gom_fl_sa)
# [1] 136213     21

to_mapview <- function(my_df) {
  my_df %>%
    dplyr::select(LATITUDE, LONGITUDE, row_id) %>%
    my_to_sf() %>% mapview() + sa_shp
}
# db_data_w_area_report_minus_gom_fl_sa %>% 
#   dplyr::select(LATITUDE, LONGITUDE, row_id) %>%
#   my_to_sf() %>% mapview() + sa_shp

### area_codes_to_keep (all other SA areas) ----
area_codes_to_remove <- paste0("00", c(3:9))
area_codes_to_remove_part2 <- paste0("0", c(10:19))

# db_data_w_area_report_minus_gom_fl_sa %>% 
  # filter(AREA_CODE %in% c("015", "052")) %>% View()

# db_data_w_area_report_minus_gom_fl_sa %>% dplyr::select(AREA_CODE) %>% unique %>% dplyr::arrange(AREA_CODE) %>% View()
# length(area_codes_to_keep)
# 122

db_data_w_area_report_minus_gom_stat_areas <-
  db_data_w_area_report_minus_gom_fl_sa %>%
  dplyr::filter(
    !(AREA_CODE %in%
        area_codes_to_remove) &
      !(AREA_CODE %in%
          area_codes_to_remove_part2)
  )

# db_data_w_area_report_minus_gom_stat_areas %>%
#   dplyr::select(AREA_CODE) %>% unique() %>%
#   dplyr::arrange(AREA_CODE) %>% 
#   View()
# 052?
# dim(db_data_w_area_report_minus_gom_stat_areas)
# [1] 130232     21
# [1] 127553     21 (codes 10:)

# 36366    
# [1] 26004    19
# [1] 25542    20
# no 000
# [1] 18478    20

# filter(db_data_w_area_report_minus_gom_stat_areas,
#        row_id == 139475) %>% View()

## if an area code is 000 or "OFF" use the end port ----
db_data_w_area_report_minus_gom_stat_not0 <-
  filter(db_data_w_area_report_minus_gom_stat_areas,
         !(AREA_CODE %in% c("000", "OFF", "OFU")))
  
db_data_w_area_report_minus_gom_stat_no_area <-
  filter(db_data_w_area_report_minus_gom_stat_areas,
         AREA_CODE %in% c("000", "OFF", "OFU"))

### filter_fl_counties ----
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

# create a filter
filter_fl_counties <- quo(
    tolower(END_PORT_STATE) == "fl" &
    tolower(END_PORT_COUNTY) %in% tolower(fl_counties_sa)
)

# length(fl_counties_sa)
# 13

# use the filter
sub1_fl_counties <-
  db_data_w_area_report_minus_gom_stat_no_area %>%
  filter(!!filter_fl_counties)
# dim(sub1_fl_counties)
# [1] 42949    21

# save what is not in florida
db_data_w_area_report_minus_gom_stat_no_area_not_fl <-
  filter(db_data_w_area_report_minus_gom_stat_no_area,
       !(tolower(END_PORT_STATE) == tolower("fl")))

# dim(db_data_w_area_report_minus_gom_stat_no_area_not_fl)
# [1] 23440    21
# 23440 + 42949 = 66389

# dim(db_data_w_area_report_minus_gom_stat_no_area)

sub1_fl_counties %>% 
  # View()
  count(END_PORT_COUNTY)
# 11
# ...
# 8 MONROE          27476

# 000
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
  # all
#    END_PORT_COUNTY     n
#    <chr>           <int>
#  1 BREVARD          1766
#  2 BROWARD           579
#  3 DUVAL            1596
#  4 INDIAN RIVER       43
#  5 MARTIN            278
#  6 MIAMI-DADE       2278
#  7 MONROE          18581
#  8 NASSAU            206
#  9 PALM BEACH       1556
# 10 VOLUSIA          1056

# Monroe county "good coords"
good_coords_monroe <-
  sub1_fl_counties %>%
  filter(END_PORT_COUNTY == "MONROE" &
           !is.na(LATITUDE) &
           !is.na(LONGITUDE))
dim(good_coords_monroe)
# [1] 27476    21

good_coords_monroe_sf <-
  good_coords_monroe %>%
  dplyr::select(LATITUDE, LONGITUDE, row_id) %>%
  my_to_sf()
# dim(good_coords_monroe_sf)

tic("with_st_difference(good_coords_monroe_sf, gom_reef_shp)")
good_coords_monroe_sf_minus_gom <-
  with_st_difference(good_coords_monroe_sf, gom_reef_shp)
toc()
# 55.92 sec
# 59.19 sec after 1 mapview
# 496.29 sec ~ 8 min

good_coords_monroe_sf_minus_gom_file_path <-
  file.path(my_paths$outputs,
            current_project_name,
            "good_coords_monroe_sf_minus_gom.csv")

good_coords_monroe_sf_minus_gom <-
  read_sf(good_coords_monroe_sf_minus_gom_file_path) %>%
  my_to_sf()

write_csv(
  good_coords_monroe_sf_minus_gom,
  good_coords_monroe_sf_minus_gom_file_path
)

good_coords_monroe_sf_minus_gom_df <-
  # remove the geometry field
  sf::st_drop_geometry(good_coords_monroe_sf_minus_gom) %>%
  # remove the rest of sf columns
  dplyr::select(-c("Area_SqKm", "Perim_M")) 

# dim(good_coords_monroe_sf_minus_gom_df)
# [1] 22558     3

# to_mapview(good_coords_monroe_sf_minus_gom_df)

sub1_fl_counties_no_monroe <-
  filter(sub1_fl_counties,
         !(END_PORT_COUNTY == "MONROE"))
# dim(sub1_fl_counties_no_monroe)
# [1] 15473    21

sub2_fl_counties_monroe_good <-
  filter(
    sub1_fl_counties,
    (END_PORT_COUNTY == "MONROE") &
      row_id %in% good_coords_monroe_sf_minus_gom_df$row_id
  )

# dim(sub2_fl_counties_monroe_good)
# [1] 22558    21

### combine ----

db_data_w_area_report_table_cleaned <-
  rbind(
    db_data_w_area_report_minus_gom_stat_not0,
    db_data_w_area_report_minus_gom_stat_no_area_not_fl,
    sub1_fl_counties_no_monroe,
    sub2_fl_counties_monroe_good
  )

to_mapview(db_data_w_area_report_table_cleaned)
# filter(db_data_w_area_report_table_cleaned,
#        row_id == 31873) %>% View()
# 43114

write_csv(db_data_w_area_report_table_cleaned,
          file.path(my_paths$outputs,
            current_project_name,
            "db_data_w_area_report_table_cleaned.csv")
)

# TODO: check below
## combine SA areas ----
db_data_w_area_report_minus_gom <-
  db_data_w_area_report_minus_gom_sub1 %>% 
  rbind(db_data_w_area_report_minus_gom_sub2) %>%
  rbind(db_data_w_area_report_minus_gom_sub3) %>%
  rbind(db_data_w_area_report_minus_gom_sub4)

# dim(db_data_w_area_report_minus_gom_sub1)
# dim(db_data_w_area_report_minus_gom_sub2)
# dim(db_data_w_area_report_minus_gom_sub3)
# dim(db_data_w_area_report_minus_gom_sub4)

dim(db_data_w_area_report_minus_gom)
# [1] 42293    20
# after using sa_shp box
# [1] 40269    20

dim(db_data_w_area_report_minus_gom_sub1)[1] +
dim(db_data_w_area_report_minus_gom_sub2)[1] +
dim(db_data_w_area_report_minus_gom_sub3)[1] +
dim(db_data_w_area_report_minus_gom_sub4)[1]
# 40269 - correct

# View(db_data_w_area_report_minus_gom)
# end port
# 2 maps, 2 tables

# db_data_w_area_report_minus_gom %>%
#   filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) %>% dim()
# same

db_data_w_area_report_minus_gom_sf <-
  my_to_sf(db_data_w_area_report_minus_gom)

mapview(db_data_w_area_report_minus_gom_sf) + sa_shp +
  gom_reef_shp + fl_state_w_counties_shp

# Correct lat/long ----
db_data_w_area_report <-
  db_data_w_area %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  dplyr::select(
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

tic("with_st_intersection(db_data_w_area_report_sf, sa_shp)")
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
toc()

# or read it
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

# all.equal(db_data_w_area_report_sa_eez_sf,
#           db_data_w_area_report_sa_eez_sf1)
# mapview(db_data_w_area_report_sa_eez_sf)
# 54950 13
# db_data_w_area_report_sa_eez_sf <- db_data_w_area_report_sa_eez

#### save sa_eez_data ----
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

mapview(db_data_w_area_report_sa_eez_sf)

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

db_data_w_area_report_sf_28_s_char <- dplyr::mutate(db_data_w_area_report_sf_28_s,
         across(everything(), as.character))
db_data_w_area_report_28_s_sa_counties_no_gom_sf_char <- dplyr::mutate(db_data_w_area_report_28_s_sa_counties_no_gom_sf,
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
