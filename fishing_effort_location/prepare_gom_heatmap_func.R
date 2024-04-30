# plot text sizes ----
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 13,
  axis_text_y_size = 13,
  plot_caption_text_size = 13,
  legend_title_text_size = 10,
  legend_text_text_size = 10,
  ### common axes for Months ----
  y_left_fontsize = 10
)


# read in GOM trip ticket grid
GOMsf <-
  sf::read_sf(r"(GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>%
  summarise()
# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create GOM 5x5 minute grid ----
min_grid <-
  function(minute_num = 1) {
    grid <-
      sf::st_make_grid(x = sf::st_bbox(GOMsf),
                       cellsize = 1 / 60 * minute_num) %>%
      sf::st_as_sf() %>%
      mutate(cell_id = 1:nrow(.))

    return(grid)
  }

grid <- min_grid(5)

sf::st_agr(GOMsf) = sf::st_agr(grid) = "constant"

### remove internal boundaries from the shape file ----

tic("st_union(GOMsf)")
st_union_GOMsf <- sf::st_union(GOMsf)
toc()
# st_union(GOMsf): 21.59 sec elapsed

# str(GOMsf)
# sf [21 × 2] (S3: sf/tbl_df/tbl/data.frame)
#  $ StatZone: num [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#  $ geometry:sfc_GEOMETRY of length 21; first list element: List of 6

# str(st_union_GOMsf)
# sfc_MULTIPOLYGON of length 1; first list element: List of 15
#  $ :List of 21234

## by n min grid ----
df_join_grid <-
  function(my_df) {
    my_df |>
      # join n minute grid
      sf::st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = sf::st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        sf::st_join(grid, join = sf::st_nearest_feature) %>%
          return()
        }

crop_by_shape <-
  function(my_sf) {
    my_sf |>
      sf::st_join(GOMsf, left = FALSE) %>%
      dplyr::mutate(LONGITUDE = sf::st_coordinates(.)[, 1],
             LATITUDE = sf::st_coordinates(.)[, 2]) %>%
      return()
  }

## count trip ids and vessels by grid cell ----
add_vsl_and_trip_cnts <-
  function(my_df, vessel_id_name = "VESSEL_OFFICIAL_NBR") {
    my_df |>
      group_by(cell_id) |>
      mutate(vsl_cnt = n_distinct(!!sym(vessel_id_name)),
             trip_id_cnt = n_distinct(TRIP_ID)) |>
      ungroup() %>%
      return()
  }

## count trip ids and vessels by grid cell ----
add_vsl_and_trip_cnts <-
  function(my_df, vessel_id_name = "VESSEL_OFFICIAL_NBR") {
    my_df |>
      group_by(cell_id) |>
      mutate(vsl_cnt = n_distinct(!!sym(vessel_id_name)),
             trip_id_cnt = n_distinct(TRIP_ID)) |>
      ungroup() %>%
      return()
  }

## make a plot ----
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
           unit_num = 1,
           print_stat_zone = NULL
           ) {
    # browser()
    max_num <- max(map_trip_base_data[[trip_cnt_name]])
    map_trips <-
      ggplot() +
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      geom_sf(data = shape_data, fill = NA)

    # test for an optional argument
    if (!missing(print_stat_zone)) {
      map_trips <-
        map_trips +
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5)
    }

    map_trips <-
        map_trips +
      labs(
        x = "",
        y = "",
        fill = "",
        caption = caption_text
      ) +
      theme_bw() +
      scale_fill_gradient2(
        name = total_trips_title,
        labels = scales::comma,
        low = "red",
        mid = "purple",
        high = "blue",
        # trans = "log2",
        trans = "log1p",
        limits = c(1, max_num)
        # ,
        # oob = scales::oob_keep
      ) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.key.width = unit(unit_num, "npc"),
        legend.title = element_text(size =
                                      text_sizes[["legend_title_text_size"]]),
        legend.text = element_text(size =
                                     text_sizes[["legend_text_text_size"]]), # for charter heatmap use 7
        plot.caption = element_text(hjust = 0,
                                    size = text_sizes[["plot_caption_text_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      guides(fill = guide_colourbar(title.position = "top"))

    return(map_trips)
  }
