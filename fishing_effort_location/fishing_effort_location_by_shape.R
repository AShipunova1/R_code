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
library(leaflet)
library(tictoc) #benchmarking
# library(htmlwidgets) # add js script to leaflets
library(stringi) # add characters
library(htmltools)
library(htmlwidgets)

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

## From db ----
all_points <- dim(db_data_w_area)[1]
# 254689
#  75536 u

# View(db_data_w_area)

# source(
#   file.path(
#     my_paths$git_r,
#     current_project_name,
#     "fishing_effort_location_by_table.R"
#   )
# )
fields_list <-
  c("TRIP_START_DATE",
"TRIP_END_DATE",
"START_PORT",
"START_PORT_NAME",
"START_PORT_COUNTY",
"START_PORT_STATE",
"END_PORT",
"END_PORT_NAME",
"END_PORT_COUNTY",
"END_PORT_STATE",
"ACTIVITY_TYPE_NAME",
"TRIP_TYPE_NAME",
"AREA_CODE",
"SUB_AREA_CODE",
"DISTANCE_CODE_NAME",
"LATITUDE",
"LONGITUDE",
"FISHING_GEAR_DEPTH")

dim(db_data_w_area)
# [1] 75536    30

db_data_w_area_short <-
  db_data_w_area |>
  select(all_of(fields_list)) |>
  unique()

dim(db_data_w_area_short)
# [1] 75536    18

# print_df_names(db_data_w_area_short)

db_data_w_area_short_good_coord <-
  db_data_w_area_short %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(db_data_w_area_short_good_coord)
# [1] 74520    18
# [1] 74504    18 -abs(LONGITUDE)

### convert to sf ----
db_data_w_area_short_good_coord_sf <-
  my_to_sf(db_data_w_area_short_good_coord)

# mapview(db_data_w_area_short_good_coord_sf,
#         legend = F)
# [1] 74520    19

## From FHIER ----
# View(safis_efforts_extended_2022_short)

safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17

### convert to sf from FHIER ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

# tic("mapview safis_efforts_extended_2022_short_good_sf")
# mapview(safis_efforts_extended_2022_short_good_sf,
#         legend = F)
# toc()
# mapview safis_efforts_extended_2022_short_good_sf: 7.76 sec elapsed


# shapes ----
fl_counties_map <-
  mapview(fl_state_w_counties_shp,
        color = "lightgreen")

# gom_fed_waters <-
#   mapview(gom_fed)

gom_state_waters_only_sf <-
  st_difference(gom_reef_shp, gom_fed)
  # st_intersection
# mapview(gom_state_waters_only)

all_gom_sf <-
  st_union(gom_state_waters_only_sf,
           gom_fed)

all_gom <-
  mapview(gom_state_waters_only_sf,
          color = "lightblue") +
  mapview(gom_fed)
  # fl_counties_map +

# show all boundaries ----

tic("all_waters_sf")
all_waters_sf <-
  st_union(gom_state_waters_only_sf,
           gom_fed,
           fl_state_w_counties_shp,
           sa_shp)
toc()
# all_waters_sf: 0.78 sec elapsed


# fl_counties_map

# tic("all_waters")
# all_waters <-
#   mapview(gom_state_waters_only_sf,
#           # color = "lightblue",
#           legend = F) +
#   mapview(gom_fed,
#           legend = F) +
#   mapview(fl_state_w_counties_shp,
#           color = "lightgreen",
#           legend = F) +
#   mapview(sa_shp,
#           # color = "red",
#           legend = F)
# toc()
# all_waters: 42.79 sec elapsed

# mapview(db_data_w_area_short_good_coord_sf)
# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\fishing_trips_GOM_2022\all_fishing_spots.png"

# db_data_w_area_short_good_coord_sf__sea <-
#   with_st_intersection(db_data_w_area_short_good_coord_sf, all_gom_sf)
# too long, get a subset first

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
safis_efforts_extended_2022_short_good_sf_crop_big <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          big_bounding_box)
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18

# tic("mapview(safis_efforts_extended_2022_short_good_sf_crop_big)")
# mapview(safis_efforts_extended_2022_short_good_sf_crop_big)
# toc()

# convert back to df ----
safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metricks only vessels ----
source(r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids
# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_df)

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17

# GOM area ----
## get gom boundaries ----
all_gom_sf_bbox <-
  st_bbox(all_gom_sf)

## subset data by all_gom_sf_bounding_box ----
dim(safis_efforts_extended_2022_short_good_sf)
# [1] 97547    18

tic("st_crop(safis_efforts_extended_2022_short_good_sf")
gom_safis_efforts_extended_2022_short_good_sf_crop <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          all_gom_sf_bbox)
toc()
# st_crop(safis_efforts_extended_2022_short_good_sf: 0.5 sec elapsed

# mapview(gom_safis_efforts_extended_2022_short_good_sf)

dim(gom_safis_efforts_extended_2022_short_good_sf_crop)
# [1] 68190    18

## get only points on GOM maps ----

tic("gom_safis_efforts_extended_2022_short_good_sf_crop_inters")
gom_safis_efforts_extended_2022_short_good_sf_crop_inters <-
  with_st_intersection(safis_efforts_extended_2022_short_good_sf,
          all_gom_sf)
toc()
# gom_safis_efforts_extended_2022_short_good_sf_crop_inters: 722.62 sec elapsed
# 722.62/60 = 722.62 m

# str(safis_efforts_extended_2022_short_good_sf)
# sfc_POINT of length 97547
# str(all_gom_sf)
#   ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"

st_intersection_faster <- function(x, y, ...) {
  #faster replacement for st_intersection(x, y,...)

  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {
      y[., ]
    }

  st_intersection(x, y_subset, ...)
}

tic("gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1")
gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1 <-
  st_intersection_faster(safis_efforts_extended_2022_short_good_sf,
          all_gom_sf)
toc()
# gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1: 695.34 sec elapsed
# 695.34/60 = 11.589m

dim(gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1)
# [1] 42866    26

tic("gom_points_n_shape")
gom_points_n_shape <-
  mapview(
    all_gom_sf,
    col.regions = "#F4E3FF",
    alpha.regions = 0.2,
    layer.name = "Gulf of Mexico",
    legend = FALSE
  ) +
  mapview(gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1,
          legend = F,
          clusterOptions = markerClusterOptions())
toc()
# gom_points_n_shape: 13.22 sec elapsed

## image with clusters -----
lat_lon_data <- gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1

image_with_clusters_base <- function(lat_lon_data) {
  tic("gom_clusters_shape")
  gom_clusters_shape_base <-
    leaflet(data = lat_lon_data) |>
    addTiles() |>
    addPolygons(data = all_gom_sf,
                weight = 5,
                col = "#F4E3FF") |>
    flyToBounds(-97.8, 23.8, -80.4, 31.1) |>
    setView(-89, 29, zoom = 5)
  toc()
  return(gom_clusters_shape_base)
}

map_base <- image_with_clusters_base(lat_lon_data)

marker_js <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

map_base |>
    addCircleMarkers(clusterOptions =
                         markerClusterOptions(
          iconCreateFunction = marker_js
                       ))

# add ten minute grid ----
map_base |>
 addGraticule(interval = 1 / 60 * 10,
              style = list(color = "grey", weight = 1))
              # style = list(color = "#FF0000", weight = 1))


# baselayerchange
map_base |>
  htmlwidgets::onRender("
    function(e, x) {
      var myMap = this;

			this._shownPolygon = new L.Polygon(e.layer.getConvexHull(), this.options.polygonOptions);
			myMap.addLayer(this._shownPolygon);


    }")

js_cluster_polygons <-
  function(cluster) {
    # JS(
    "
# function(cluster) {
        cluster._showCoverage({ layer: cluster })
        //var coverages = new L.LayerGroup();
        // coverages.clearLayers();
        //coverages.addLayer(L.polygon(cluster.getConvexHull()));

          // getConvexHull())
          //	var latLngBounds = cluster.getBounds();
        	var latLngConvex = cluster.getConvexHull();
          // L.addLayer(L.polygon(cluster.getConvexHull()));


          var sum = 55;
          var children = cluster.getAllChildMarkers();
         return new L.DivIcon({
              html: '<div style=\"background-color: rgb(111,198,204); opacity : 0.7\"><span>' + JSON.stringify(coverages) + '</div><span>',
              className: 'marker-cluster'
                                               });
                                           # }
    "
    # )
  }

image_with_clusters()
# JSON.stringify(coverages)


l <- leaflet() %>% setView(0,0,3)

esri <- grep("^Esri", providers, value = TRUE)

for (provider in esri) {
  l <- l %>% addProviderTiles(provider, group = provider)
}

l %>%
  addLayersControl(baseGroups = names(esri),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
    position = "bottomleft") %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")

lat_lon_data_short <-
  lat_lon_data |>
  sf::st_drop_geometry() |>
  select(LATITUDE,
         LONGITUDE,
         TRIP_ID)
# |>
  # distinct()

lat_lon_data_coord <-
  lat_lon_data |>
  sf::st_drop_geometry() |>
  # na.omit() |>
  select(LATITUDE,
         LONGITUDE)
# |>
  # distinct()

# dim(lat_lon_data_uniq_coord)
# [1] 35762     2

dim(lat_lon_data_short)
# [1] 42865     3
# LATITUDE  31996
# LONGITUDE 32220

lat_lon_data_uniq_coord_ten_min <-
  lat_lon_data_coord |>
  # head(10) |>
  get_ten_min_coords()

lat_lon_data_uniq_coord_ten_min_short <-
  lat_lon_data_uniq_coord_ten_min |>
  select(ten_min_lat, ten_min_lon)
# |>
  # distinct() |>
  # dim()
# [1] 1295    2

lat_lon_data_uniq_coord_ten_min_short <-
  lat_lon_data_uniq_coord_ten_min_short |>
  rename(LATITUDE = ten_min_lat,
         LONGITUDE = ten_min_lon)

map_base <-
  image_with_clusters_base(lat_lon_data_uniq_coord_ten_min_short)

map_base |>
  addCircleMarkers(clusterOptions =
                     markerClusterOptions(iconCreateFunction =                                            marker_js)) |>
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "#FF0000", weight = 1))


# SA ----
### with st_intersection ----
# get only the points inside the SA EEZ by intersection
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
# 594.35 sec
# 63.44 sec (uniq)
# 65.1 sec
# 174.95 sec
# 193.08 /60 ~3
# 196.25 sec

### or read it ----
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf_more_fields.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

#### save sa_eez_data ----
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

dim(db_data_w_area_report_sa_eez_sf)
# 18989 13
# [1] 18967    13
# [1] 18970    21

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report %>%
  filter(between(LATITUDE, 23, 28)) %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

dim(db_data_w_area_report_28_s_sf)
# 92882   11
# 27979   19   unique

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

dim(db_data_w_area_report_28_s_sa_counties_sf)
# [1] 10761    38

# mapview(db_data_w_area_report_28_s_sa_counties_sf)

### or read csv ----
db_data_w_area_report_28_s_sa_counties_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf_u_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  my_to_sf()

write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30
# 10761    38

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

### using sf::st_difference ----
# slow
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
# 188.69 sec
# 195.96 sec ~3 m


# or read csv
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
# 8903 40

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
    select(all_of(fields_list)
           # )
    # select(
    #   TRIP_START_DATE,
    #   TRIP_END_DATE,
    #   START_PORT,
    #   START_PORT_NAME,
    #   START_PORT_COUNTY,
    #   START_PORT_STATE,
    #   END_PORT,
    #   LATITUDE,
    #   LONGITUDE,
    #   FISHING_GEAR_DEPTH
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

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903   40

my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "sa_eez_all")
# names(db_data_w_area_report_sa_eez_sf)
# 18989
# [1] 18970    21


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

files_to_combine_list <-
  c(
    file.path(my_paths$git_r, "useful_functions_module.r"),
    file.path(dir_to_comb, "read.me.R"),
    file.path(dir_to_comb, "fishing_effort_locations_get_data.R"),
    file.path(dir_to_comb, "fishing_effort_location.R")
    # ,
    # file.path(dir_to_comb, "fishing_effort_location_viz.R")
  )

flat_file_name = file.path(dir_to_comb, "fishing_effort_location_flat_05_30.R")

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

# SA only ----

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
  select(all_of(fields_list))

dim(db_data_w_area_report_short)
# 45315 10
# [1] 45264    18

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

dim(db_data_w_area_report_sf)
# [1] 45264    19

### with st_intersection ----
# get only the points inside the SA EEZ by intersection
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
# 594.35 sec
# 63.44 sec (uniq)
# 65.1 sec
# 174.95 sec
# 193.08 /60 ~3
# 196.25 sec

### or read it ----
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf_more_fields.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

#### save sa_eez_data ----
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

dim(db_data_w_area_report_sa_eez_sf)
# 18989 13
# [1] 18967    13
# [1] 18970    21

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report %>%
  filter(between(LATITUDE, 23, 28)) %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

dim(db_data_w_area_report_28_s_sf)
# 92882   11
# 27979   19   unique

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

dim(db_data_w_area_report_28_s_sa_counties_sf)
# [1] 10761    38

# mapview(db_data_w_area_report_28_s_sa_counties_sf)

### or read csv ----
db_data_w_area_report_28_s_sa_counties_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf_u_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  my_to_sf()

write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30
# 10761    38

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

### using sf::st_difference ----
# slow
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
# 188.69 sec
# 195.96 sec ~3 m


# or read csv
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
# 8903 40

write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

# mapview(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

# Report csv ----
# convert sf to a df
my_sf_to_df <- function(my_sf) {
  my_df <-
    my_sf %>%
    sf::st_drop_geometry() %>%
    select(all_of(fields_list)
           # )
    # select(
    #   TRIP_START_DATE,
    #   TRIP_END_DATE,
    #   START_PORT,
    #   START_PORT_NAME,
    #   START_PORT_COUNTY,
    #   START_PORT_STATE,
    #   END_PORT,
    #   LATITUDE,
    #   LONGITUDE,
    #   FISHING_GEAR_DEPTH
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

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903   40

my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "sa_eez_all")
# names(db_data_w_area_report_sa_eez_sf)
# 18989
# [1] 18970    21


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

files_to_combine_list <-
  c(
    file.path(my_paths$git_r, "useful_functions_module.r"),
    file.path(dir_to_comb, "read.me.R"),
    file.path(dir_to_comb, "fishing_effort_locations_get_data.R"),
    file.path(dir_to_comb, "fishing_effort_location.R")
    # ,
    # file.path(dir_to_comb, "fishing_effort_location_viz.R")
  )

flat_file_name = file.path(dir_to_comb, "fishing_effort_location_flat_05_30.R")

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

