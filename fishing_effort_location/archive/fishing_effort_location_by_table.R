# Clean data using mostly area codes
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
  ) %>%
  unique()

dim(db_data_w_area_short)
# [1] 254689     15
# [1] 75541    20

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

dim(db_data_w_area_report_minus_gom)
# [1] 145694     15
# [1]  45269    20

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
  # select(# AREA_NAME,
    # SUB_AREA_CODE,
    # AREA_CODE,
    # DISTANCE_CODE_NAME,
    # REGION
    # ) %>%
  # filter(is.na(AREA_CODE)) %>%
  # 0
  # unique() %>%
  # arrange(SUB_AREA_CODE)
    # arrange(AREA_CODE)
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

dim(sub1_fl_sub_areas)
# [1] 10824    21
# 3866

sub2_not_fl_sub_areas <-
  dplyr::filter(db_data_w_area_report_minus_gom,
                !(AREA_CODE %in% names(sa_sub_area_codes)))

dim(sub1_fl_sub_areas)
# [1] 10824    21
# 3866

dim(sub2_not_fl_sub_areas)
# [1] 125389     21
# 40177

# 10824 + 125389 = 136213
# 40177 + 3866 = 44043
# dim(db_data_w_area_report_minus_gom) - in gom sub areas
# 45269
# (AREA_CODE %in% names(sa_sub_area_codes)
# [1] 14788    21

dim(db_data_w_area_report_minus_gom)
# [1] 140177     21
# 45269

db_data_w_area_report_minus_gom_fl_sa <-
  rbind(sub1_fl_sub_areas, sub2_not_fl_sub_areas)

dim(db_data_w_area_report_minus_gom_fl_sa)
# [1] 136213     21
# 44043

to_mapview <- function(my_df) {
  my_df %>%
    select(LATITUDE, LONGITUDE, row_id) %>%
    my_to_sf() %>% mapview() + sa_shp
}
# db_data_w_area_report_minus_gom_fl_sa %>%
#   select(LATITUDE, LONGITUDE, row_id) %>%
#   my_to_sf() %>% mapview() + sa_shp

### area_codes_to_keep (all other SA areas) ----
area_codes_to_remove <- paste0("00", c(3:9))
area_codes_to_remove_part2 <- paste0("0", c(10:19))

# db_data_w_area_report_minus_gom_fl_sa %>%
  # filter(AREA_CODE %in% c("015", "052")) %>% View()

# db_data_w_area_report_minus_gom_fl_sa %>% select(AREA_CODE) %>% unique %>% arrange(AREA_CODE) %>% View()
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
#   select(AREA_CODE) %>% unique() %>%
#   arrange(AREA_CODE) %>%
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
dim(sub1_fl_counties)
# [1] 42949    21
# 14291

# save what is not in florida
db_data_w_area_report_minus_gom_stat_no_area_not_fl <-
  filter(db_data_w_area_report_minus_gom_stat_no_area,
       !(tolower(END_PORT_STATE) == tolower("fl")))

dim(db_data_w_area_report_minus_gom_stat_no_area_not_fl)
# [1] 23440    21
# 23440 + 42949 = 66389
# 8565

# dim(db_data_w_area_report_minus_gom_stat_no_area)

sub1_fl_counties %>%
  # View()
  count(END_PORT_COUNTY)
# 11
# ...
# 8 MONROE          27476
 # 8 MONROE           9394

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
# [1] 9394   21

good_coords_monroe_sf <-
  good_coords_monroe %>%
  select(LATITUDE, LONGITUDE, row_id) %>%
  my_to_sf()
# dim(good_coords_monroe_sf)

good_coords_monroe_sf_minus_gom <-
  with_st_difference(good_coords_monroe_sf, gom_reef_shp)
# 55.92 sec
# 59.19 sec after 1 mapview
# 496.29 sec ~ 8 min
# 172.65 s

good_coords_monroe_sf_minus_gom_file_path <-
  file.path(my_paths$outputs,
            current_project_name,
            "good_coords_monroe_sf_minus_gom_u.csv")

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
  select(-c("Area_SqKm", "Perim_M"))

dim(good_coords_monroe_sf_minus_gom_df)
# [1] 22558     3
# [1] 8046    3

# to_mapview(good_coords_monroe_sf_minus_gom_df)

sub1_fl_counties_no_monroe <-
  filter(sub1_fl_counties,
         !(END_PORT_COUNTY == "MONROE"))
# dim(sub1_fl_counties_no_monroe)
# [1] 15473    21
# [1] 4897   21

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

# dim(db_data_w_area_report_table_cleaned)
# [1] 33297    21

db_data_w_area_report_table_cleaned %>%
  select(LATITUDE, LONGITUDE, row_id) %>%
  my_to_sf() %>% mapview(layer.name = 'points cleared by area and end port') + m_s

# filter(db_data_w_area_report_table_cleaned,
#        row_id == 31873) %>% View()
# 43114

write_csv(db_data_w_area_report_table_cleaned,
          file.path(my_paths$outputs,
            current_project_name, "report",
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

