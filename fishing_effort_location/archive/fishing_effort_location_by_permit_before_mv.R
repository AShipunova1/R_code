# by permit before mv data ----
# print_df_names(coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "trip_id, vessel_official_nbr, latitude, longitude, permit_sa_gom"
map_by_permit_before_mv_data <- function() {

# convert to ten_min ----
coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(coord_data_2022_short_good_sf_crop_big_df_in_metricks)

View(coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7
# [1] 90838    75 from mv

# split by permit ----
## add permit_name_col ----
coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  dplyr::mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
# Use the 'map' function to perform a series of operations on a list of data frames.

coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      # Perform operations on each 'permit_df' data frame within the list.
      permit_df |>
      dplyr::select(-c(latitude, longitude)) |>
      # dplyr::select columns except 'latitude' and 'longitude'
      count(ten_min_lat, ten_min_lon) |>
      # Count occurrences of 'ten_min_lat' and 'ten_min_lon' combinations
      dplyr::arrange(desc(n)) |>
      # Arrange rows in descending order of the count 'n'
      head(2)
      # dplyr::select the top 2 rows
    }
  )

# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      dplyr::select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      dplyr::group_by(ten_min_lat, ten_min_lon) |>
      dplyr::mutate(location_cnts_u = (n_distinct(latitude, longitude))) |>
      dplyr::ungroup()
  })

# View(coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, vessel_official_nbr, ten_min_lat, ten_min_lon, location_cnts"

coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      dplyr::select(-c(TRIP_ID, vessel_official_nbr,
                latitude, longitude)) |>
      dplyr::distinct()
  })

coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  dplyr::mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  dplyr::mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  dplyr::group_by(ten_min_lat, ten_min_lon) |>
  dplyr::mutate(location_cnts_u = (n_distinct(latitude, longitude))) |>
  dplyr::ungroup()

short_example_3_cnts |>
  dplyr::select(latitude, longitude) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # dplyr::distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  dplyr::select(-c(vessel_official_nbr,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  dplyr::distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  dplyr::select(-c(latitude, longitude)) |>
  dplyr::distinct() |>
  dplyr::arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  dplyr::mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ latitude,
    lng = ~ longitude,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text
}
  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])


