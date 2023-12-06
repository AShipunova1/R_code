
# convert to sf ----
## how many don't have coords ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  filter(is.na(long) &
           is.na(lat)) |>
  dim()
# 11

## Preparing spatial data for map visualization ----
# Filtering rows with non-missing longitude and latitude values using the filter function.
# 
# Converting the data frame to a spatial object using the sf::st_as_sf function.
# 
# Specifying the field names ("long", "lat") to be used as coordinates.
# 
# Setting the Coordinate Reference System (CRS) for the spatial object to match the US states map using the crs parameter with the tigris_crs value.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  dplyr::filter(!is.na(long) &
           !is.na(lat)) |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = tigris_crs)

all_home_ports <-
  mapview(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf)

## crop all home ports by us state south map ----
# Creating a subset of data for the southern and eastern coast states
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf |>
  sf::st_crop(south_east_coast_states_shp_bb)  # Bounding box used for cropping
# although coordinates are longitude/latitude, st_intersection assumes that they
# are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

dim(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south)
# [1] 3106   10

# Prepare for mapping: non compl only, add labels ----

# The following code modifies the data frame for map visualization by filtering, selecting distinct rows, and creating new columns. Here's the breakdown of the comments:
# 
# 1. Modifying the data frame for map visualization.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south` to the subsequent functions.
# 
# 3. Filtering rows where the compliance status in 2022 is "NO" using the `filter` function.
# 
# 4. Keeping only distinct rows in the data frame using the `distinct` function.
# 
# 5. Adding a new column named 'my_label' using string interpolation with the `str_glue` function. This column combines the city, state, and non-compliance percentage for labeling on the map.
# 
# 6. Adding a new column named 'perc_nc_bin' using the `cut_number` function. This column represents the non-compliance percentage binned into 5 levels for color representation on the map.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south |>
  dplyr::filter(is_compliant_in_22 == "NO") |>
  dplyr::distinct() |> 
  dplyr::mutate(my_label =
           stringr::str_glue("{city_fixed} {state_fixed}; {is_comp_perc_round}% non-compl")) |> 
  dplyr::mutate(perc_nc_bin = 
                  ggplot2::cut_number(is_comp_perc_round, n = 5))

# View(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)

dim(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)
# [1] 832  12

## Calculating the number of unique colors based on a specific column ----
uniq_color_num <-
  function(zcol_name) {
    length(
      unique(
        vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab[[zcol_name]]
      )
    ) %>%
      return()
  }
# uniq_color_num("is_comp_perc_round")
# 43

## pop-up marker table ----
# The following code creates pop-up marker information for the map by renaming columns and generating a pop-up table. Here's the breakdown of the comments:
# 
# 1. Creating pop-up marker information for the map using the `leafpop::popupTable` function.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab` to the subsequent functions.
# 
# 3. Renaming columns in the data frame for clarity in pop-up information. The column `cnt_vsl_by_permit_n_port_coord` is renamed to `total_vsls`, and `cnt_sa_vsl_by_port_coord_n_compl` is renamed to `non_compliant`.
# 
# 4. Generating a pop-up table using the specified columns (`total_vsls`, `non_compliant`, and `perc_nc_bin`) with the `leafpop::popupTable` function.
# 
# 5. Disabling the display of feature ID in the pop-up table by setting `feature.id` to FALSE.
# 
# 6. Disabling the display of row numbers in the pop-up table by setting `row.numbers` to FALSE.
# 
# 7. Specifying the columns to be displayed in the pop-up table using the `zcol` parameter.
markers_info <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  dplyr::rename(total_vsls = cnt_vsl_by_permit_n_port_coord,
         non_compliant = cnt_sa_vsl_by_port_coord_n_compl,) |>
  leafpop::popupTable(
    feature.id = FALSE,
    row.numbers = FALSE,
    zcol = c(
      "total_vsls",
      "non_compliant",
      "perc_nc_bin"
    )
  )

## mapview ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  mapview::mapview(
    label = "my_label",
    # Specifying the column to be used for coloring the map
    zcol = "perc_nc_bin",
    # Scaling factor for the size of the markers
    cex = "cnt_vsl_by_permit_n_port_coord",
    # Setting the transparency level of the markers
    alpha = 0.3,
    # Specifying the color palette for the map
    # The direction parameter is set to -1 to reverse the order of colors.
    col.regions =
      viridisLite::mako(uniq_color_num("perc_nc_bin"), direction = -1),
    # legend = FALSE,
    layer.name = '% non compliant SA permitted vessels (2022) by home port coordinates',
    popup = markers_info
    # Enabling burst mode for each bin to be a layer
    # burst = TRUE 
  # )
) +
  south_east_coast_states_shp

# with leaflet clusters ----
# [1] "SERO_OFFICIAL_NUMBER, permit_sa_gom, city_fixed, state_fixed, cnt_vsl_by_permit_n_port_coord, is_compliant_in_22, cnt_sa_vsl_by_port_coord_n_compl, non_comp_perc, is_comp_perc_round, geometry, my_label, perc_nc_bin"
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  glimpse()

legend_pal <- hcl.colors(10, palette = 'Spectral', rev = T)

iconCreateFunction_js <- 
  paste0(
  "function (cluster) {
  var markers = cluster.getAllChildMarkers();
  var sum = 0;
  for (i = 0; i < markers.length; i++) {
    sum += Number(markers[i].options.nc_perc);
  }
  var count = markers.length;
  var avg = Math.round(sum/count);
  return L.divIcon({
    html: '<div><span>' + sum + '</span></div>',
    iconSize: new L.Point(40, 40) 
  });
}"

)

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  # select(perc_nc_bin) |>
  leaflet::leaflet() |>
  # options =
  # leafletOptions(crs = leafletCRS(tigris_crs))) |>
  leaflet::addTiles() |>
  # addMarkers(
  addCircleMarkers(
    options = markerOptions(nc_perc = ~ is_comp_perc_round),
    data = vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab,
    label = ~ as.character(my_label),
    popup = ~ as.character(perc_nc_bin),
    radius = ~ is_comp_perc_round,
    
    # color = cnt_vsl_by_permit_n_port_coord
    # color = ~ perc_nc_bin,
    # group = ~ perc_nc_bin,
    clusterOptions =
      leaflet::markerClusterOptions(iconCreateFunction = 
                                      JS(iconCreateFunction_js)),
    labelOptions = labelOptions(noHide = T,
                                direction = "auto")
  )
#  radius = 10,

#   leaflet::addMarkers(
#     # clusterOptions = 
#     #                     leaflet::markerClusterOptions())
#     clusterOptions = 
#       leaflet::markerClusterOptions(iconCreateFunction = JS("function (cluster) {    
#     var theseMarkers = cluster.getAllChildMarkers();
#     let myCustomCount = 0;
#     for (const childMarker of childMarkers) {
#       myCustomCount += childMarker.perc_nc_bin; 
#     }
# 
#     return L.divIcon({ html: '<b>' + myCustomCount + '</b>' });
#     
#   }"))
# )
# 

#           "function (cluster) {
# 				var markers = cluster.getAllChildMarkers();
# 				var nc_perc_n = 0;
# 				for (var i = 0; i < markers.length; i++) {
# 					nc_perc_n += markers[i].options.nc_perc
# 				}
# 
# 								return L.divIcon({ 
#                             html: '<div style=\"background-color:'+c+'\"><span>'+avg+'</span></div>', 
#                             className: 'marker-cluster', 
#                             iconSize: new L.Point(40, 40) });
# 
#     return L.divIcon({ html: '<div><span>'+
# nc_perc_n
# +'</span></div>', 
# iconSize: new L.Point(40, 40) });
# 			}
# "
